#!/bin/sh

set -xue

stack build; stack exec zxemu-exe
