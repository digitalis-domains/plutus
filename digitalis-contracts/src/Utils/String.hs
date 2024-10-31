module Utils.String
  ( integerAsBuiltinByteString,
    integerToBuiltinByteStringMapping,
    baseQ,
    readMaybeInteger,
    convertBuiltinByteStringToInteger,
  )
where

import Data.Text (pack)
import Data.Text.Read (decimal)
import qualified PlutusLedgerApi.V3 as PlutusV3
import qualified PlutusTx.Builtins.Internal as Internal
import PlutusTx.Prelude
import qualified Prelude as Haskell

-------------------------------------------------------------------------------

-- | The Mapping for converting an integer into a stringed version.

-------------------------------------------------------------------------------
{-# INLINEABLE integerToBuiltinByteStringMapping #-}
integerToBuiltinByteStringMapping :: Integer -> PlutusV3.BuiltinByteString
integerToBuiltinByteStringMapping ch
  | ch == 0 = "0"
  | ch == 1 = "1"
  | ch == 2 = "2"
  | ch == 3 = "3"
  | ch == 4 = "4"
  | ch == 5 = "5"
  | ch == 6 = "6"
  | ch == 7 = "7"
  | ch == 8 = "8"
  | ch == 9 = "9"
  | otherwise = ""

-------------------------------------------------------------------------------

-- | Convert an integer into a string.

-------------------------------------------------------------------------------
{-# INLINEABLE integerAsBuiltinByteString #-}
integerAsBuiltinByteString :: Integer -> PlutusV3.BuiltinByteString
integerAsBuiltinByteString num = if num == 0 then "0" else convertToString base10 ""
  where
    base10 :: [Integer]
    base10 = baseQ num 10

    convertToString :: [Integer] -> BuiltinByteString -> BuiltinByteString
    convertToString [] str = str
    convertToString (x : xs) str = convertToString xs (str <> integerToBuiltinByteStringMapping x)

-------------------------------------------------------------------------------

-- | Write an integer in base Q and return a list of integers.

-------------------------------------------------------------------------------
{-# INLINEABLE baseQ #-}
baseQ :: Integer -> Integer -> [Integer]
baseQ number base = baseQ' number base []
  where
    baseQ' :: Integer -> Integer -> [Integer] -> [Integer]
    baseQ' number' base' list = do
      if number' == 0
        then list
        else baseQ' (Internal.divideInteger number' base') base' (Internal.modInteger number' base' : list)

-------------------------------------------------------------------------------

-- | Utility function to convert Text to Maybe Integer

-------------------------------------------------------------------------------
readMaybeInteger :: Haskell.String -> Maybe Integer
readMaybeInteger str
  | all (`Haskell.elem` ['0' .. '9']) str = case decimal (pack str) of
    Right (num, _) -> Just num
    _ -> Nothing
  | otherwise = Nothing

-------------------------------------------------------------------------

-- | Take in a byteString and converts it to a number via a product. Similar
-- to creating an integer list but instead takes the product of all the values.
-- The product is limited to numbers less than 2^64 - 1.
--
-- Testing: Test.Groups.String

-------------------------------------------------------------------------
{-# INLINEABLE convertBuiltinByteStringToInteger #-}
convertBuiltinByteStringToInteger :: BuiltinByteString -> Integer
convertBuiltinByteStringToInteger hexString
  | hexString == emptyByteString = 0
  | lengthOfByteString hexString >= fixedLength + 2 = hexStringToInteger hexString fixedLength 1 -- force length
  | otherwise = 0
  where
    -- this will restrict to numbers less than 2^64 - 1
    fixedLength :: Integer
    fixedLength = 6

    -- add 1 to each number to avoid multiplying by zero
    hexStringToInteger :: BuiltinByteString -> Integer -> Integer -> Integer
    hexStringToInteger hex_string counter value'
      | counter > 0 = hexStringToInteger hex_string (counter - 1) (value' * (indexByteString hex_string counter + 1))
      | otherwise = value' * (indexByteString hex_string 0 + 1)
