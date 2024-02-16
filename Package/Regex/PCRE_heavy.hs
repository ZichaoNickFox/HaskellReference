{-# LANGUAGE QuasiQuotes #-}

import           Package.Regex.CustomQQ
import           Safe
import           Test.Hspec
import           Text.Regex.PCRE.Heavy
import           Text.Regex.PCRE.Light
import           Util

-- https://hackage.haskell.org/package/pcre-heavy-1.0.0.3
officialSpec :: SpecWith ()
officialSpec = do
  it "checking" $ "https://val.packett.cool" =~ [re|^http.*|] `shouldBe` True
  it "checking" $ "https://val.packett.cool" ≈ [re|^http.*|] `shouldBe` True
  it "matching (searching)" $ scan [re|\s*entry (\d+) (\w+)\s*&?|] " entry 1 hello  &entry 2 hi" `shouldBe`
    [
      (" entry 1 hello  &", ["1", "hello"]),
      ("entry 2 hi", ["2", "hi"])
    ]
  it "matching (searching) lazy" $ headMay (scan [re|\s*entry (\d+) (\w+)\s*&?|] " entry 1 hello  &entry 2 hi") `shouldBe`
    Just (" entry 1 hello  &", ["1", "hello"])
  it "replacement gsub" $ gsub [re|\d+|] "!!!NUMBER!!!" "Copyright (c) 2015 The 000 Group" `shouldBe`
    "Copyright (c) !!!NUMBER!!! The !!!NUMBER!!! Group"
  it "replacement gsub group" $ gsub [re|%(\d+)(\w+)|] (\(d : w : _) -> "{" ++ d ++ " of " ++ w ++ "}") "Hello, %20thing" `shouldBe`
    "Hello, {20 of thing}"
  it "replacement gsub matching" $ gsub [re|-\w+|] (\x -> "+" ++ (reverse $ drop 1 x)) "hello -world" `shouldBe`
    "hello +dlrow"
  -- it "replacement gsub group & matching"
  it "spliting" $ split [re|%(begin|next|end)%|] "%begin%hello%next%world%end%" `shouldBe`
    ["", "hello", "world", ""]
  it "options scanO" $
    scanO [customQQRe|\s*entry (\d+) (\w+)\s*&?|] [exec_no_utf8_check] " entry 1 hello  &entry 2 hi"
      `shouldBe`
      [
        (" entry 1 h", ["1", "h"]),
        ("entry 2 h", ["2", "h"])
      ]
  it "options gsubO" $
    gsubO [customQQRe|\d+|] [exec_notempty] "!!!NUMBER!!!" "Copyright (c) 2015 The 000 Group"
      `shouldBe` "Copyright (c) !!!NUMBER!!!!!!NUMBER!!!!!!NUMBER!!!!!!NUMBER!!! The !!!NUMBER!!!!!!NUMBER!!!!!!NUMBER!!! Group"

-- https://edu.anarcho-copy.org/Programming%20Languages/Haskell/Practical%20Web%20Development%20with%20Haskell.pdf
-- p38
-- regex1 = [re|^(hell.), (.+)!$]

spec::SpecWith ()
spec = do
  describe "officialSpec" officialSpec

main :: IO ()
main = hspec spec
