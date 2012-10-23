module Data.Text.Punycode.Decode {-# DEPRECATED "Use Data.Encoding.BootString from the 'encoding' package instead" #-} (decode) where

import qualified Data.ByteString as BS
import           Data.Char
import           Data.Serialize hiding (decode)
import           Data.Word

import           Data.Text.Punycode.Shared

{-# DEPRECATED decode "Use Data.Encoding.BootString from the 'encoding' package instead" #-}

-- | Decode a string into its unicode form
decode :: BS.ByteString -> String
decode input = let Right out = runGet (inner2 initial_n 0 initial_bias before) after in out
  where (before, after)
          | BS.any f input = (init $ map (chr . fromIntegral) $ BS.unpack b1, a1)
          | otherwise = ("", input)
        f = (== (fromIntegral $ ord '-'))
        (b1, a1) = BS.breakEnd f input

inner2 :: Int -> Int -> Int -> String -> Get String
inner2 n oldi bias output = do
  b <- isEmpty
  helper b
  where helper False = do
          i <- inner base 1 oldi bias
          helper' i
          where helper' i = inner2 n' (i' + 1) bias' output'
                  where bias' = adapt (i - oldi) (length output + 1) (oldi == 0)
                        n' = n + i `div` (length output + 1)
                        i' = i `mod` (length output + 1)
                        output' = insertInto output n' i'
        helper True = return output

inner :: Int -> Int -> Int -> Int -> Get Int
inner k w i bias = do
  word8 <- getWord8
  helper $ word8ToDigit word8
  where helper digit
          | digit < t = return i'
          | otherwise = inner (k + base) w' i' bias
          where w' = w * (base - t)
                i' = i + digit * w
                t
                  | k <= bias + tmin = tmin
                  | k >= bias + tmax = tmax
                  | otherwise = k - bias

insertInto :: String -> Int -> Int -> String
insertInto input n i = take i input ++ [chr n] ++ drop i input

word8ToDigit :: Word8 -> Int
word8ToDigit = helper . fromIntegral
  where helper word8
          | word8 >= ord 'a' && word8 <= ord 'z' = word8 - (ord 'a')
          | otherwise = 26 + word8 - (ord '0')
