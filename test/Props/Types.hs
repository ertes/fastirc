module Props.Types
    ( module Props.Types,
      module Test.Framework.Providers.QuickCheck2,
      module Test.Framework.TH,
      module Test.QuickCheck
    )
    where

import qualified Data.ByteString as B
import Control.Applicative
import Data.ByteString (ByteString)
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Test.QuickCheck


newtype ArgToken =
    ArgToken { getArgToken :: ByteString }
    deriving (Show)

instance Arbitrary ArgToken where
    arbitrary =
        fmap (ArgToken . B.pack) $
             (:)
             <$> choose (1, 255) `suchThat` notChar [10, 13, 32, 58]
             <*> listOf (choose (1, 255) `suchThat` notChar [10, 13, 32])


newtype LastArgToken =
    LastArgToken { getLastArgToken :: ByteString }
    deriving (Show)

instance Arbitrary LastArgToken where
    arbitrary =
        fmap (LastArgToken . B.pack) . listOf $
             choose (1, 255) `suchThat` notChar [10, 13]


newtype NonToken =
    NonToken { getNonToken :: ByteString }
    deriving (Show)

instance Arbitrary NonToken where
    arbitrary =
        fmap (NonToken . B.pack) . listOf . elements $ [0, 10, 13, 32]


notChar :: (Eq a) => [a] -> a -> Bool
notChar = flip notElem
