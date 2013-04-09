-- |
-- Module:     Main
-- Copyright:  (c) 2013 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
--
-- Test program for the fastirc package.

module Main where

--import Test.Framework
--import Test.Framework.Providers.HUnit
--import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
--import Test.QuickCheck


main :: IO ()
main = $(defaultMainGenerator)
