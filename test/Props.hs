-- |
-- Module:     Main
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Test program for the fastirc package.

module Main where

import Props.Raw
import Test.Framework


main :: IO ()
main =
    defaultMain $
        rawTests :
        []
