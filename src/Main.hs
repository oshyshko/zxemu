module Main where


import           Control.Monad
import           Data.Bits
import qualified Data.ByteString              as BS
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT

box :: IO ()
box = renderPrimitive Quads $ mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z)[ (0::GLint, 0, 0)
                                             , (0, 1, 0)
                                             , (1, 1, 0)
                                             , (1, 0, 0) ]

-- See http://www.animatez.co.uk/computers/zx-spectrum/screen-memory-layout/
--     https://en.wikipedia.org/wiki/ZX_Spectrum_graphic_modes
--
colorForPixelAt :: BS.ByteString -> Int -> Int -> Color3 GLubyte
colorForPixelAt screen x y =
  let bitIndex  = 7 - rem x 8
      byteIndex = shiftL (0xC0 .&. y) 5  -- 0b11000000
              .|. shiftL (0x07 .&. y) 8  -- 0b00000111
              .|. shiftL (0x38 .&. y) 2  -- 0b00111000
              .|. shiftR (0xF8 .&. x) 3  -- 0b11111000
      -- alternative implementation with div/rem
      -- byteIndez = div x 8                                 -- x offset
      --           + (div (rem y 64) 8 * 32 + rem y 8 * 256) -- start of line within a segment
      --           + (div y 64 * 2048)                       -- 1/2/3 segment
      isSet     = testBit (BS.index screen byteIndex) bitIndex
      attrIndex = 6144 + div y 8 * 32 + div x 8
      attr      = BS.index screen attrIndex
      bright    = testBit attr 6 -- flash     = testBit attr 7
      compAt n  = if testBit attr n then (if bright then 0xFF else 0xCD) else 0x00
      fgColor   = Color3 (compAt 1) (compAt 2) (compAt 0)
      bgColor   = Color3 (compAt 4) (compAt 5) (compAt 3)
   in if isSet then fgColor else bgColor

-- TODO perf: render to a texture, then draw with one quad (or buffer copy)
display :: BS.ByteString -> DisplayCallback
display screen = do
  clear [ColorBuffer]
  preservingMatrix $ do
    translate $ Vector3 (-1) 1 (0::GLfloat)
    scale (2/256) (-2/192) (1::GLfloat)
    forM_ [0..191] $ \y ->
      forM_ [0..255] $ \x ->
        preservingMatrix $ do
          translate $ Vector3 (fromIntegral x) (fromIntegral y)  (0::GLfloat)
          color $ colorForPixelAt screen x y
          box
  flush


reshape :: ReshapeCallback
reshape size = do
  -- viewport $= (Position (-256) (-192), size)
  viewport $= (Position 0 0, size)
  postRedisplay Nothing

keyboardMouse :: KeyboardMouseCallback
keyboardMouse _key _state _modifiers _position = return ()

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "zxemu"

  -- src <- BS.readFile "data/R-Type.scr"
  src <- BS.readFile "data/DanDare-PilotOfTheFuture.scr"

  clearColor $= Color4 0 0 0 0

  windowSize $= Size (256 * 2) (192 * 2)
  displayCallback $= display src
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just keyboardMouse
  mainLoop
