module IpAddressSpec where

import           Data.IpAddress
import           Test.Hspec
import           Text.Trifecta

parseIP :: String -> Maybe IPAddress
parseIP s = o
  where
    r = parseString parseIPAddress mempty s
    o =
      case r of
        (Success o) -> Just o
        _           -> Nothing

parseIP6 :: String -> Maybe IPAddress6
parseIP6 s = o
  where
    r = parseString parseIPAddress6 mempty s
    o =
      case r of
        (Success o) -> Just o
        _           -> Nothing

ipAddressSpecs = hspec $ do
  describe "IpAddress tests" $ do
    describe "parseIPAddress" $
      it "should work" $ do
        parseIP "152.163.254.3" `shouldBe` (Just $ fromIntegral 2560884227)
        parseIP "204.20.50.135" `shouldBe` (Just $ fromIntegral 3423875719)
        parseIP "224.165.197.142" `shouldBe` (Just $ fromIntegral 3768960398)
    describe "parseIPAddress6" $
      it "should work" $ do
        parseIP6 "ff39:0:0:0:2f2:b3ff:f23d:8d5" `shouldBe` (Just $ fromIntegral
                                                                     339249099846090032765509468706555365589)
        parseIP6 "ff39::2f2:b3ff:f23d:8d5" `shouldBe` (Just $ fromIntegral
                                                                339249099846090032765509468706555365589)
        parseIP6 "0:0:0:0:0:ffff:abc:fed9" `shouldBe` (Just $ fromIntegral
                                                                281470861901529)
        parseIP6 "::ffff:abc:fed9" `shouldBe` (Just $ fromIntegral
                                                        281470861901529)
        parseIP6 "ffff:cc78:f:0:0:0:0:0" `shouldBe` (Just $ fromIntegral
                                                              340281321743036709175527292837595971584)
        parseIP6 "ffff:cc78:f::" `shouldBe` (Just $ fromIntegral
                                                      340281321743036709175527292837595971584)
        parseIP6 "9ff3:EA8::8A:30:2F0C:1F7A" `shouldBe` (Just $ fromIntegral
                                                                  212609276730491340425271727923210755962)
    describe "ipV4ToIpV6" $
      it "should work" $ do
        (show . ipV4ToIpV6 <$> parseIP "124.155.107.12") `shouldBe` Just
                                                                      "0:0:0:0:0:ffff:7c9b:6b0c"
        (show . ipV4ToIpV6 <$> parseIP "192.168.0.1") `shouldBe` Just
                                                                   "0:0:0:0:0:ffff:c0a8:1"
    describe "show" $ do
      it "should show IPAddress6 properly" $ do
        (show <$> parseIP6 "ff39:0:0:0:2f2:b3ff:f23d:8d5") `shouldBe` Just
                                                                        "ff39:0:0:0:2f2:b3ff:f23d:8d5"
        (show <$> parseIP6 "9ff3:EA8::8A:30:2F0C:1F7A") `shouldBe` Just
                                                                     "9ff3:ea8:0:0:8a:30:2f0c:1f7a"
        (show <$> parseIP6 "::ffff:abc:fed9") `shouldBe` Just "0:0:0:0:0:ffff:abc:fed9"
      it "should show IPAddress properly" $ do
        (show <$> parseIP "152.163.254.3") `shouldBe` Just "152.163.254.3"
        (show <$> parseIP "224.165.197.142") `shouldBe` Just "224.165.197.142"
        (show <$> parseIP "124.155.107.12") `shouldBe` Just "124.155.107.12"
