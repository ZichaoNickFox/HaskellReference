module Package.Regex.CustomQQ where

import           Text.Regex.PCRE.Heavy
import           Text.Regex.PCRE.Light

customQQRe = mkRegexQQ [multiline, utf8, ungreedy]
