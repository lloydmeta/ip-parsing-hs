module Data.IpAddress.Internal where

import           Control.Applicative
import           Control.Monad       (join)
import           Data.Char           (toLower)
import           Data.List           (intersperse, isInfixOf, isPrefixOf,
                                      isSuffixOf)
import qualified Data.List           as L
import           Data.List.Utils     (replace)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe)
import           Data.Word
import           Text.Trifecta

data IPAddress = IPAddress Word32
  deriving (Eq, Ord)

instance Num IPAddress where
  (IPAddress w1) + (IPAddress w2) = IPAddress $ w1 + w2
  (IPAddress w1) * (IPAddress w2) = IPAddress $ w1 * w2
  abs (IPAddress w1) = IPAddress $ abs w1
  signum (IPAddress w1) = IPAddress $ signum w1
  fromInteger i = IPAddress $ fromInteger i
  negate (IPAddress w1) = IPAddress $ negate w1

instance Real IPAddress where
  toRational (IPAddress w) = toRational w

instance Enum IPAddress where
  toEnum i = IPAddress (toEnum i)
  fromEnum (IPAddress w) = fromEnum w

instance Integral IPAddress where
  quotRem (IPAddress n) (IPAddress d) = (IPAddress $ fromIntegral q, IPAddress $ fromIntegral r)
    where
      (q, r) = quotRem (toInteger n) (toInteger d)
  toInteger (IPAddress w) = toInteger w

instance Show IPAddress where
  show ip = join $ intersperse "." asStrings
    where
      IPV4DotFields repr = ipAddressToIPV4DotFields ip
      asStrings = fmap show repr

data IPV4DotFields = IPV4DotFields [Integer]
  deriving (Eq, Ord, Show)

parseIPV4DotFields :: Parser IPV4DotFields
parseIPV4DotFields = do
  first3 <- count 3 (decimal <* char '.')
  i0 <- decimal
  return $ IPV4DotFields (first3 ++ [i0])

parseIPAddress :: Parser IPAddress
parseIPAddress = do
  ipV4fields <- parseIPV4DotFields
  let ipAddy = iPV4DotFieldsToIpAddress ipV4fields
  return ipAddy

iPV4DotFieldsToIpAddress :: IPV4DotFields -> IPAddress
iPV4DotFieldsToIpAddress (IPV4DotFields f) = IPAddress $ fromIntegral fAsInteger
  where
    fAsInteger = foldr (\l acc -> l + 256 * acc) 0 (reverse f)

ipV6FullSegments :: Int
ipV6FullSegments = 8

newtype IPV6Normed = IPV6Normed String
  deriving (Eq, Ord, Show)

newtype IPV6Str = IPV6Str String
  deriving (Eq, Ord, Show)

validHexChars :: String
validHexChars = "0123456789abcdefABCDEF"

validHexCharsLowerOnly :: String
validHexCharsLowerOnly = "0123456789abcdef"

hexCharToValue :: Map Char Int
hexCharToValue = M.fromList $ zip validHexCharsLowerOnly [0 ..]

parseIPV6Section :: Parser String
parseIPV6Section = do
  mL <- optional (try $ string "::" <|> string ":")
  seq <- some (oneOf validHexChars)
  mR <- optional (try $ string "::" <|> string ":")
  let lowered = map toLower seq
      l = fromMaybe "" mL
      r = fromMaybe "" mR
  return $ l ++ lowered ++ r

parseIPV6Str :: Parser IPV6Str
parseIPV6Str = do
  s <- (try $ (fmap (: []) (string "::" <* eof))) <|> manyTill parseIPV6Section
                                                        eof
  if length s < 1
    then fail "Did not find valid sections"
    else return $ IPV6Str $ join s

parseIPV6Normed :: Parser IPV6Normed
parseIPV6Normed = do
  str <- parseIPV6Str
  let IPV6Str (s) = str
      full = mkIPV6Normed s
  case full of
    Left err      -> fail err
    Right fullstr -> return fullstr

mkIPV6Normed :: String -> Either String IPV6Normed
mkIPV6Normed origS = result
  where
    expand s
      | s == "::" = IPV6Normed $ buildExpanded0s ipV6FullSegments
      | isPrefixOf "::" s =
          let expandCnt = ipV6FullSegments - (length $ split ':' s) + 2
              filler = buildExpanded0s expandCnt ++ ":"
              replaced = replace "::" filler s
          in IPV6Normed replaced
      | isSuffixOf "::" s =
          let expandCnt = ipV6FullSegments - (length $ split ':' s) + 1
              filler = ':' : buildExpanded0s expandCnt
              replaced = replace "::" filler s
          in IPV6Normed replaced
      | isInfixOf "::" s =
          let expandCnt = ipV6FullSegments - (length $ split ':' s) + 1
              filler = ':' : buildExpanded0s expandCnt ++ ":"
              replaced = replace "::" filler s
          in IPV6Normed replaced
      | otherwise = IPV6Normed s
    expanded = expand origS
    IPV6Normed expandedStr = expanded
    result = if length (split ':' expandedStr) == ipV6FullSegments
               then Right expanded
               else Left "invalid sections"

buildExpanded0s :: Int -> String
buildExpanded0s i = intersperse ':' (take i (repeat '0'))

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y)
  where
    (x, y) = L.span (/= d) s

data IPAddress6 = IPAddress6 Word64 Word64
  deriving (Eq, Ord)

ipAddress6toInteger :: IPAddress6 -> Integer
ipAddress6toInteger (IPAddress6 q r) = toInteger q * word64Max + toInteger r

integerToIPAddress6 :: Integer -> IPAddress6
integerToIPAddress6 i = IPAddress6 qWord rWord
  where
    (q, r) = quotRem i word64Max
    qWord = fromIntegral q
    rWord = fromIntegral r

instance Num IPAddress6 where
  i1 + i2 = integerToIPAddress6
              (ipAddress6toInteger i1 + ipAddress6toInteger i2)
  i1 * i2 = integerToIPAddress6
              (ipAddress6toInteger i1 * ipAddress6toInteger i2)
  abs i1 = integerToIPAddress6 . abs $ ipAddress6toInteger i1
  signum i = integerToIPAddress6 $ signum $ ipAddress6toInteger i
  fromInteger = integerToIPAddress6
  negate i = integerToIPAddress6 $ negate $ ipAddress6toInteger i

instance Real IPAddress6 where
  toRational i = toRational $ ipAddress6toInteger i

instance Enum IPAddress6 where
  toEnum = integerToIPAddress6 . fromIntegral
  fromEnum = fromIntegral . ipAddress6toInteger

instance Integral IPAddress6 where
  toInteger = ipAddress6toInteger
  quotRem nIp dIp = (integerToIPAddress6 q, integerToIPAddress6 r)
    where
      nInteger = toInteger nIp
      dInteger = toInteger dIp
      (q, r) = quotRem nInteger dInteger

instance Show IPAddress6 where
  show ip = normed
    where
      IPV6Normed normed = iPAddress6ToIPV6Normed ip

parseIPAddress6 :: Parser IPAddress6
parseIPAddress6 = do
  normed <- parseIPV6Normed
  return $ ipv6NormedToIPAddress6 normed

twoRaised16Exp :: [Integer]
twoRaised16Exp = fmap ((2 ^ 16) ^) [0,1 ..]

ipv6NormedToIPAddress6 :: IPV6Normed -> IPAddress6
ipv6NormedToIPAddress6 (IPV6Normed str) = IPAddress6 quotient remainder
  where
    asSegs = split ':' str
    zippedWithExp = zip (reverse asSegs) twoRaised16Exp
    asInteger = foldr (\(s, exp) acc -> hexToDec s * exp + acc) 0 zippedWithExp
    (q, r) = quotRem asInteger word64Max
    quotient = fromIntegral q
    remainder = fromIntegral r

word64Max :: Integer
word64Max = toInteger (maxBound :: Word64)

hexToDec :: String -> Integer
hexToDec s = toInteger asInt
  where
    asInt = foldr
              (\c acc ->
                 fromMaybe 0 (M.lookup (toLower c) hexCharToValue) + 16 * acc)
              0
              (reverse s)

ipAddressToIPV4DotFields :: IPAddress -> IPV4DotFields
ipAddressToIPV4DotFields (IPAddress word) = IPV4DotFields repr
  where
    asInteger = toInteger word
    repr = integralToBaseM asInteger 0 [0 .. 255]

iPAddress6ToIPV6Normed :: IPAddress6 -> IPV6Normed
iPAddress6ToIPV6Normed ip = IPV6Normed s
  where
    asInteger = ipAddress6toInteger ip
    chopped = integerToChoppedUp asInteger
    ss = fmap integerToHexString chopped
    fillCnt = ipV6FullSegments - length ss
    filled = (take fillCnt (repeat "0")) ++ ss
    s = join $ intersperse ":" filled

integerToChoppedUp :: Integer -> [Integer]
integerToChoppedUp i = go i []
  where
    go 0 [] = [0]
    go 0 acc = acc
    go curr acc =
      let (q, r) = quotRem curr (2 ^ 16)
      in go q (r : acc)

-- Turns an integral into an list representation in another base
-- :: Integral -> zero -> [digits] -> [representation]
integralToBaseM :: Integral a => a -> b -> [b] -> [b]
integralToBaseM i zero digits = if base == 0
                                  then []
                                  else go i []
  where
    base = fromIntegral $ length digits
    go 0 [] = [zero]
    go 0 acc = acc
    go curr acc =
      let (q, r) = quotRem curr base
      in go q ((digits !! fromIntegral r) : acc)

integerToHexString :: Integer -> String
integerToHexString i = integralToBaseM i '0' validHexCharsLowerOnly

ipV4ToIpV6Normed :: IPAddress -> IPV6Normed
ipV4ToIpV6Normed (IPAddress word) = normed
  where
    asInteger = toInteger word
    chopped = integerToChoppedUp asInteger
    ss = fmap integerToHexString chopped
    fillCnt = ipV6FullSegments - length ss - 1
    -- - ffff signifies an ip4 to ip6 conversion
    -- (http://www.tcpipguide.com/free/t_IPv6IPv4AddressEmbedding-2.htm)
    filled = (take fillCnt $ repeat "0") ++ ["ffff"] ++ ss
    s = join $ intersperse ":" filled
    normed = IPV6Normed s

ipV4ToIpV6 :: IPAddress -> IPAddress6
ipV4ToIpV6 ip = ipv6
  where
    normed = ipV4ToIpV6Normed ip
    ipv6 = ipv6NormedToIPAddress6 normed
