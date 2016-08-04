{-# LANGUAGE GADTs, StandaloneDeriving, TemplateHaskell #-}

module Main where

-- import           Cpu
import qualified Screen as Screen

import           Control.Concurrent
-- import           Control.Exception (assert) -- assert False undefined
import           Control.Lens
import qualified Data.ByteString    as BS
import           Data.Word

data Reg bits where
  A, B, C, D, E, F, H, L     :: Reg Word8
  BC, DE, HL, SP, PC, IX, IY :: Reg Word16
deriving instance Show (Reg bits)

type Addr = Word16

data Src bits where
  SrcConst :: bits     -> Src bits
  SrcReg   :: Reg bits -> Src bits
  SrcAddr  :: Addr     -> Src bits

data Dst bits where
  DstReg   :: Reg bits -> Dst bits
  DstAddr  :: Addr     -> Dst bits

-- https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/basic-lensing
-- view a      $ State 1 2 3  => 1
-- set  a 5    $ State 1 2 3  => State {_a = 5, _b = 2, _c = 3}
-- over a (+5) $ State 1 2 3  => State {_a = 6, _b = 2, _c = 3}
data Comp = Comp { _a, _b, _c, _d, _e, _h, _l :: Word8,
                   _bc, _de, _hl              :: Word16,
                   _memory                    :: BS.ByteString } deriving Show
makeLenses ''Comp

-- reg2field :: Reg bits -> Lens' Comp bits
reg2field reg = case reg of
    -- A  -> a
    -- B  -> b
    -- C  -> c
    _ -> undefined


getSrc :: Src bits -> Comp -> bits
getSrc src comp = case src of
  SrcConst value -> value
  SrcReg   reg   -> (view $ reg2field reg) comp
  SrcAddr  addr  -> error "TODO"
                    -- case bits of
                    --   Word8  -> BS.index (view memory comp) $ fromIntegral addr
                    --   _ -> error "Not implemented"

setDst :: Dst bits -> bits -> Comp -> Comp
setDst dst value comp = case dst of
  DstReg   reg   -> set (reg2field reg) value comp
  DstAddr  addr  -> error "TODO"

-- targetAddr8 addr value comp = over memory (BS.update (fromIntegral addr) value) comp
-- TODO lookup "update" in documentation. Consider switching to Data.Vector

-- class (Show i) => Instruction i where
--   eval :: i -> Comp -> Comp
--   cost :: i -> Int



data Op where
  Nop   ::                               Op
  Ld    :: Dst bits -> Src bits       -> Op
  Call  :: Addr                       -> Op
  Ret   ::                               Op
  Inc   :: Reg b                      -> Op
  Rla   ::                               Op
  Rra   ::                               Op
  And   :: Reg b                      -> Op
  Or    :: Reg b                      -> Op
  Add   :: Reg b                      -> Op
  Add2  :: Reg b -> Reg b             -> Op
  Djnz  :: Addr                       -> Op
-- deriving instance Show (Op)

data Instruction = Instruction { _code     :: [Word8]
                               , _op       :: Op
                               , _mCost    :: Int
                               , _mnemonic :: String }

-- ins = Instruction

-- instructions = [ ins 0x00 Nop 1 "nop"
--                , ins 0x26 Ld  2 "ld r,n"]



{--
  26 48       ld h, 'H'
  06 0C       ld b, 12
  0E 0D       ld c, 13
  CD 09 5D    call $095d

  2665 060C 0E0E CD095D
  266C 060C 0E0F CD095D
  266C 060C 0E10 CD095D
  266F 060C 0E11 CD095D
  2621 060C 0E12 CD095D
  C9      ret

    $095d:
  7C          ld a,h
  17 17 17    x3 rla
  E6F8        and %11111000
  5F          ld e,a
  7C
  17 17 17 17 x4 rla
  E6 07       and %00000111

  57          ld d,a
  21 003C     ld hl, $3c00
  19          add hl, de

  78          ld a,b
  E6 18       and %00011000
  F6 40       or  %01000000
  57          ld d,a

  78          ld a,b
  1F 1F 1F 1F x4 rra
  E6 E0       and %11100000
  5F          ld e,a
  79          ld a,c
  E6 1F       and %00011111
  B3          or e
  5F          ld e,a

  06 08       ld b,8

    loop:
  7E          ld a,(hl)
  12          ld (de),a
  23          inc hl
  14          inc d
  10 FA       djnz loop
  C9          ret
--}

-- f x = g x
-- g x = error (show x)
-- main = try (evaluate (f ())) :: IO (Either SomeException ())

main :: IO ()
main = do
  -- src <- BS.readFile "data/R-Type.scr"
  bytes <- BS.readFile "data/DanDare-PilotOfTheFuture.scr"
  screen <- newEmptyMVar

  -- let ram = BS.replicate 65536 0

  putMVar screen bytes

  Screen.streamScreen screen
