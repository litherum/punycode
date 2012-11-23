module Data.Text.Punycode.Decode (decode) where

import qualified Data.ByteString as BS
import           Data.Char
import           Data.Serialize hiding (decode)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Word

import           Data.Text.Punycode.Shared

-- | Decode a string into its unicode form
decode :: BS.ByteString -> T.Text
decode input = let Right out = runGet (inner2 initial_n 0 initial_bias before) after in out
  where (before, after)
          | BS.any f input = (TE.decodeUtf8 $ BS.init b1, a1)
          | otherwise = (T.empty, input)
        f = (== (fromIntegral $ ord '-'))
        (b1, a1) = BS.breakEnd f input

inner2 :: Int -> Int -> Int -> T.Text -> Get T.Text
inner2 n oldi bias output = do
  b <- isEmpty
  helper b
  where helper False = do
          i <- inner base 1 oldi bias
          helper' i
          where helper' i = inner2 n' (i' + 1) bias' output'
                  where bias' = adapt (i - oldi) (T.length output + 1) (oldi == 0)
                        n' = n + i `div` (T.length output + 1)
                        i' = i `mod` (T.length output + 1)
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

insertInto :: T.Text -> Int -> Int -> T.Text
insertInto input n i = T.concat [T.take i input, T.singleton $ chr n, T.drop i input]

word8ToDigit :: Word8 -> Int
word8ToDigit = helper . fromIntegral
  where helper word8
          | word8 >= ord 'a' && word8 <= ord 'z' = word8 - (ord 'a')
          | otherwise = 26 + word8 - (ord '0')
