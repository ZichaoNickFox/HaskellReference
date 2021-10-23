{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MegaparsecSpec (specs) where

import qualified Control.Applicative
import Control.Monad
import Data.Text (pack)
import Data.Text.Internal ( Text )
import Data.Void ( Void )
import Text.Megaparsec hiding (optional)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.Debug
import Test.Hspec

shouldParseAs :: (VisualStream s, TraversableStream s, ShowErrorComponent e, Show a, Show b) =>
  Either (ParseErrorBundle s e) a -> b -> Expectation
shouldParseAs result expected =
  case result of
    Left e -> unless (show (errorBundlePretty e) == show expected) . expectationFailure $ "failed"
    Right a -> unless (show a == show expected) . expectationFailure $ "failed"

shouldParseAsWhat :: (VisualStream s, TraversableStream s, ShowErrorComponent e, Show a) =>
  Either (ParseErrorBundle s e) a -> () -> IO()
shouldParseAsWhat result () =
  case result of
    Left e -> print $ errorBundlePretty e
    Right a -> print a

----------------------------------------------------------------------------------------------------

specParserFromSatisfy :: SpecWith()
specParserFromSatisfy = do
  it "Parser from satisfy" $ do
    parse (satisfy (=='a')::Parsec Void Text Char) "" "a" `shouldParseAs` 'a'
    parse (satisfy (=='b')::Parsec Void Text Char) "" "a" `shouldParseAs` unlines [
      "1:1:",
      "  |",
      "1 | a",
      "  | ^",
      "unexpected 'a'"]
    parse (satisfy (=='b')::Parsec Void Text Char) "" "" `shouldParseAs` unlines [
      "1:1:",
      "  |",
      "1 | <empty line>",
      "  | ^",
      "unexpected end of input"]
    parse (satisfy (> 'c')::Parsec Void Text Char) "" "d" `shouldParseAs` 'd'
    parse ((satisfy::(Char->Bool)->Parsec Void Text Char) (> 'c')) "" "d" `shouldParseAs` 'd'

----------------------------------------------------------------------------------------------------

specParserFromString :: SpecWith()
specParserFromString = do
  it "Parser from string, strings'" $ do
    parse (string "hello"::Parsec Void Text Text) "" "hello" `shouldParseAs` "hello"
    parse (string "hello"::Parsec Void Text Text) "" "world" `shouldParseAs` unlines [
      "1:1:",
      "  |",
      "1 | world",
      "  | ^^^^^",
      "unexpected \"world\"",
      "expecting \"hello\""]
    parse (string "hEllO"::Parsec Void Text Text) "" "hello" `shouldParseAs` unlines [
      "1:1:",
      "  |",
      "1 | hello",
      "  | ^^^^^",
      "unexpected \"hello\"",
      "expecting \"hEllO\""]
    parse (string' "hEllO"::Parsec Void Text Text) "" "hello" `shouldParseAs` "hello"

----------------------------------------------------------------------------------------------------
    
specParserFromChar :: SpecWith()
specParserFromChar = do
  it "Parser from char" $ do
    parse (mySequence1 :: Parsec Void Text (Char, Char, Char)) "" "abc" `shouldParseAs` ('a', 'b', 'c')
    parse (mySequence1' :: Parsec Void Text (Char, Char, Char)) "" "abc" `shouldParseAs` ('a', 'b', 'c')
    parse (mySequence2 :: Parsec Void Text (Char, Char, Char)) "" "abc" `shouldParseAs` ('a', 'b', 'c')
    parse (mySequence2' :: Parsec Void Text (Char, Char, Char)) "" "abc" `shouldParseAs` ('a', 'b', 'c')
    parse (mySequence2'' :: Parsec Void Text (Char, Char, Char)) "" "abc" `shouldParseAs` ('a', 'b', 'c')
    parse (mySequence1 :: Parsec Void Text (Char, Char, Char)) "" "bcd" `shouldParseAs` unlines [
      "1:1:",
      "  |",
      "1 | bcd",
      "  | ^",
      "unexpected 'b'",
      "expecting 'a'"]
    parse (mySequence1 :: Parsec Void Text (Char, Char, Char)) "" "adc" `shouldParseAs` unlines [
      "1:2:",
      "  |",
      "1 | adc",
      "  |  ^",
      "unexpected 'd'",
      "expecting 'b'"]

mySequence1 :: Parsec Void Text (Char, Char, Char)
mySequence1 = do
  a <- char 'a'
  b <- char 'b'
  c <- char 'c'
  return (a, b, c)

mySequence1' :: Parsec Void Text (Char, Char, Char)
mySequence1' =
  char 'a' >>= \a ->
  char 'b' >>= \b ->
  char 'c' >>= \c ->
  return (a, b, c)

mySequence2 :: Parsec Void Text (Char, Char, Char)
mySequence2 = do
  (,,) <$> char 'a'
       <*> char 'b'
       <*> char 'c'

mySequence2' :: Parsec Void Text (Char, Char, Char)
mySequence2' = do
  (((((,,)::Char->Char->Char->(Char,Char,Char))
       <$> (char::Char->Parsec Void Text Char) 'a' :: Parsec Void Text (Char->Char->(Char,Char,Char)))
       <*> (char::Char->Parsec Void Text Char) 'b' :: Parsec Void Text (Char->(Char,Char,Char)))
       <*> (char::Char->Parsec Void Text Char) 'c' :: Parsec Void Text (Char, Char, Char))

mySequence2'' :: Parsec Void Text (Char, Char, Char)
mySequence2'' =
  (,,) <$> char 'a'
       <*> char 'b'
       <*> char 'c'

----------------------------------------------------------------------------------------------------

specParserFromMany :: SpecWith ()
specParserFromMany = do
  it "Parser from many" $ do
    parse (many (char 'a') :: Parsec Void Text [Char]) "" "aaaa" `shouldParseAs` "aaaa"
    parse (many (char 'a') :: Parsec Void Text [Char]) "" "aabb" `shouldParseAs` "aa"
    parse (many (char 'a') <* eof :: Parsec Void Text [Char]) "" "aaaa" `shouldParseAs` "aaaa"
    parse (many (char 'a') <* eof :: Parsec Void Text [Char]) "" "aabb" `shouldParseAs` unlines [
      "1:3:",
      "  |",
      "1 | aabb",
      "  |   ^",
      "unexpected 'b'",
      "expecting 'a' or end of input"]

----------------------------------------------------------------------------------------------------

specParserFromChoice :: SpecWith ()
specParserFromChoice = do
  it "Parser from <|>, choice, <$" $ do
    parse parserFromChoice "" "file" `shouldParseAs` "file"
    parse parserFromChoice "" "irc" `shouldParseAs` "irc"
    parse parserFromChoice "" "http" `shouldParseAs` "http"
    parse parserFromChoice "" "https" `shouldParseAs` "https"
    parse parserFromChoice' "" "file" `shouldParseAs` "file"
    parse parserFromChoice' "" "irc" `shouldParseAs` "irc"
    parse parserFromChoice "" "" `shouldParseAs` unlines [
      "1:1:",
      "  |",
      "1 | <empty line>",
      "  | ^",
      "unexpected end of input",
      "expecting \"data\", \"file\", \"fpt\", \"http\", \"https\", \"irc\", or \"mailto\""]
    parse parserFromChoice "" "" `shouldParseAs` unlines [
      "1:1:",
      "  |",
      "1 | <empty line>",
      "  | ^",
      "unexpected end of input",
      "expecting \"data\", \"file\", \"fpt\", \"http\", \"https\", \"irc\", or \"mailto\""]
    parse parserFromChoice "" "dat" `shouldParseAs` unlines [
      "1:1:",
      "  |",
      "1 | dat",
      "  | ^^^",
      "unexpected \"dat\"",
      "expecting \"data\", \"file\", \"fpt\", \"http\", \"https\", \"irc\", or \"mailto\""]

parserFromChoice :: Parsec Void Text Text
parserFromChoice = string "data"
  <|> string "file"
  <|> string "fpt"
  <|> string "https"
  <|> string "http"
  <|> string "irc"
  <|> string "mailto"

parserFromChoice' :: Parsec Void Text Text
parserFromChoice' = (choice::[Parsec Void Text Text]->Parsec Void Text Text) [
  (string::Text->Parsec Void Text Text) "data",
  string "file",
  string "fpt",
  string "https",
  string "http",
  string "irc",
  string "mailto" ]
  
----------------------------------------------------------------------------------------------------

alternatives :: Parsec Void Text (Char, Char)
alternatives = try foo <|> bar
  where
    foo = (,) <$> char 'a' <*> char 'b'
    bar = (,) <$> char 'a' <*> char 'c'

specParserFromAlternatives :: SpecWith ()
specParserFromAlternatives = do
  it "Parser from alternatives" $ do
    parse alternatives "" "ab" `shouldParseAs` ('a', 'b')
    parse alternatives "" "ac" `shouldParseAs` ('a', 'c')
  
----------------------------------------------------------------------------------------------------

specParserFromUri :: SpecWith ()
specParserFromUri = doSpecParserFromUri

data Scheme =
  SchemeData |
  SchemeFile |
  SchemeFtp |
  SchemeHttps |
  SchemeHttp |
  SchemeIrc |
  SchemeMailto deriving (Eq, Show)

parserFromScheme :: Parsec Void Text Scheme
parserFromScheme = choice [
  SchemeData <$ string "data",
  SchemeFile <$ string "file",
  SchemeFtp <$ string "ftp",
  SchemeHttps <$ string "https",
  SchemeHttp <$ string "http",
  SchemeIrc <$ string "irc",
  SchemeMailto <$ string "mailto" ]

data Uri = Uri {
  uriScheme :: Scheme,
  uriAuthority :: Maybe Authority
  } deriving (Eq, Show)

data Authority = Authority {
  authUser :: Maybe (Text, Text), -- (user, password)
  authHost :: Text,
  authPort :: Maybe Int
} deriving (Eq, Show)

optional :: Control.Applicative.Alternative f => f a -> f (Maybe a)
optional p = (Just <$> p) <|> pure Nothing

parserFromUri :: Parsec Void Text Uri
parserFromUri = do
  uriScheme <- parserFromScheme
  char ':'
  uriAuthority <- optional . try $ do
    string "//"
    authUser <- optional . try $ do
      user <- pack <$> some alphaNumChar
      char ':'
      password <- pack <$> some alphaNumChar
      char '@'
      return (user, password)
    authHost <- pack <$> some (alphaNumChar <|> char '.')
    authPort <- optional (char ':' *> decimal)
    return Authority {..}
  return Uri {..}

doSpecParserFromUri :: SpecWith ()
doSpecParserFromUri = do
  it "Parser from Uri" $ do
    parse (parserFromUri <* eof) "" "https://mark:secret@example.com" `shouldParseAs`
      Uri {
        uriScheme = SchemeHttps,
        uriAuthority = Just ( Authority {
          authUser = Just ("mark", "secret"),
          authHost = "example.com",
          authPort = Nothing } ) }
    parse (parserFromUri <* eof) "" "https://mark:secret@example.com:123" `shouldParseAs`
      Uri {
        uriScheme = SchemeHttps,
        uriAuthority = Just ( Authority {
          authUser = Just ("mark", "secret"),
          authHost = "example.com",
          authPort = Just 123 } ) }
    
    parse (parserFromUri <* eof) "" "https://example.com:123" `shouldParseAs`
      Uri {
        uriScheme = SchemeHttps,
        uriAuthority = Just ( Authority {
          authUser = Nothing,
          authHost = "example.com",
          authPort = Just 123 } ) }
    
    parse (parserFromUri <* eof) "" "https://mark@example.com" `shouldParseAs` unlines [
      "1:13:",
      "  |",
      "1 | https://mark@example.com",
      "  |             ^",
      "unexpected '@'",
      "expecting '.', ':', alphanumeric character, or end of input"]

----------------------------------------------------------------------------------------------------

parserFromUriHint :: Parsec Void Text Uri
parserFromUriHint = do
  uriScheme <- parserFromScheme <?> "valid scheme"
  char ':'
  uriAuthority <- optional . try $ do
    string "//"
    authUser <- optional . try $ do
      user <- pack <$> some alphaNumChar <?> "username"
      char ':'
      password <- pack <$> some alphaNumChar <?> "password"
      char '@'
      return (user, password)
    authHost <- pack <$> some (alphaNumChar <|> char '.') <?> "hostname"
    authPort <- optional (char ':' *> label "port number" decimal)
    return Authority {..}
  return Uri {..}

-- TODO: Not same with tutorial
specParserFromUriHint :: SpecWith ()
specParserFromUriHint = do
  it "Parser from Uri Without Hint" $ do
    parse (parserFromUriHint <* eof) "" "https://mark:@example.com" `shouldParseAsWhat` () --unlines [
      -- "1:13:",
      -- "  |",
      -- "1 | https://mark:@example.com",
      -- "  |             ^",
      -- "unexpected '@'",
      -- "expecting password" ]
  -- it "Parser from Uri Hint" $ do
  --   parse (parserFromUriHint <* eof) "" "https://mark:@example.com" `shouldParseAsWhat` () --unlines [
      -- "1:13:",
      -- "  |",
      -- "1 | https://mark:@example.com",
      -- "  |             ^",
      -- "unexpected '@'",
      -- "expecting password" ]
  
----------------------------------------------------------------------------------------------------

specs::SpecWith()
specs = do
  describe "MegaparsecSpec" $ do
    specParserFromSatisfy
    specParserFromString      
    specParserFromChar
    specParserFromMany
    specParserFromChoice 
    specParserFromAlternatives
    
    specParserFromUri

    -- specParserFromUriHint