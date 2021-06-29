{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Encoding
    (
    -- * Encoding
      encodingToLazyByteString
    , inlineList
    , list
    , int
    ) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as B
import Data.ByteString.Builder (Builder, char7, toLazyByteString)

-- | An encoding of a JSON value.
--
-- @tag@ represents which kind of JSON the Encoding is encoding to,
-- we reuse 'Text' and 'Value' as tags here.
newtype Encoding' tag = Encoding {
      fromEncoding :: Builder
      -- ^ Acquire the underlying bytestring builder.
    }

-- | Often used synonym for 'Encoding''.
type Encoding = Encoding' ()

encodingToLazyByteString :: Encoding' a -> BSL.ByteString
encodingToLazyByteString = toLazyByteString . fromEncoding
{-# INLINE encodingToLazyByteString #-}


infixr 6 ><
(><) :: Encoding' a -> Encoding' a -> Encoding' a
Encoding a >< Encoding b = Encoding (a <> b)
{-# INLINE (><) #-}

list, inlineList :: (a -> Encoding) -> [a] -> Encoding
inlineList _ []     = openBracket >< closeBracket
inlineList to' (x:xs) = openBracket >< to' x >< commas xs >< closeBracket
  where
    commas = foldr (\v vs -> comma >< to' v >< vs) empty
{-# INLINE inlineList #-}

list = inlineList
{-# NOINLINE list #-}


empty :: Encoding' a
empty = Encoding mempty

-------------------------------------------------------------------------------
-- chars
-------------------------------------------------------------------------------

comma        = Encoding $ char7 ','
openBracket  = Encoding $ char7 '['
closeBracket = Encoding $ char7 ']'

-------------------------------------------------------------------------------
-- Decimal numbers
-------------------------------------------------------------------------------


int :: Int -> Encoding
int = Encoding . B.intDec

