{-# LANGUAGE OverloadedStrings #-}

module Parser.Parser (Command(..), parseFile, readScriptH, readScript) where

import           Matrix.D3Point
import           Data.Attoparsec.ByteString.Lazy (skipWhile, takeWhile)
import           Data.Attoparsec.ByteString.Char8 hiding (skipWhile, takeWhile)
import           Data.ByteString (ByteString, hGetContents)
import           Data.ByteString.Char8 (unpack)
import           System.IO (Handle, withFile, IOMode(..))
import           Control.Monad
import           Control.Applicative
import           Prelude hiding (takeWhile)

data Command = Line D3Coord D3Coord D3Coord D3Coord D3Coord D3Coord
             | Circle D3Coord D3Coord D3Coord
             | Hermite D3Coord D3Coord D3Coord D3Coord D3Coord D3Coord D3Coord D3Coord
             | Bezier D3Coord D3Coord D3Coord D3Coord D3Coord D3Coord D3Coord D3Coord
             | Box D3Coord D3Coord D3Coord D3Coord D3Coord D3Coord
             | Identity
             | Scale D3Coord D3Coord D3Coord
             | Translate D3Coord D3Coord D3Coord
             | RotateX D3Coord
             | RotateY D3Coord
             | RotateZ D3Coord
             | Apply
             | Display
             | Save FilePath
  deriving Show

uncurryList1 :: (a -> b) -> [a] -> b
uncurryList1 f (x:_) = f x

uncurryList3 :: (a -> a -> a -> b) -> [a] -> b
uncurryList3 f (x1:x2:x3:_) = f x1 x2 x3

uncurryList6 :: (a -> a -> a -> a -> a -> a -> b) -> [a] -> b
uncurryList6 f (x1:x2:x3:x4:x5:x6:_) = f x1 x2 x3 x4 x5 x6

uncurryList8 :: (a -> a -> a -> a -> a -> a -> a -> a -> b) -> [a] -> b
uncurryList8 f (x1:x2:x3:x4:x5:x6:x7:x8:_) =
  f x1 x2 x3 x4 x5 x6 x7 x8

restOfLine :: Parser ByteString
restOfLine = takeWhile (not . isEndOfLine)

parseComment :: Parser ByteString
parseComment = do
  skipSpace
  comment <- "#" *> restOfLine
  endOfLine
  return comment

blankLine :: Parser ()
blankLine = skipWhile isHorizontalSpace >> endOfLine

skipCommentsAndSpace :: Parser ()
skipCommentsAndSpace = skipMany (void parseComment <|> blankLine)

matchString :: Parser ByteString -> Parser ()
matchString s = skipCommentsAndSpace <* s *> skipCommentsAndSpace

makeCmdArgParser :: Parser ByteString -> Int -> ([Double] -> Command) -> Parser Command
makeCmdArgParser s n f = do
  matchString s
  xs <- count n (skipSpace >> double)
  return $ f xs

makeCmdParser :: Parser ByteString -> Command -> Parser Command
makeCmdParser s cmd = matchString s *> return cmd

parseLine :: Parser Command
parseLine = makeCmdArgParser "line" 6 $ uncurryList6 Line

parseCircle :: Parser Command
parseCircle = makeCmdArgParser "circle" 3 $ uncurryList3 Circle

parseHermite :: Parser Command
parseHermite = makeCmdArgParser "hermite" 8 $ uncurryList8 Hermite

parseBezier :: Parser Command
parseBezier = makeCmdArgParser "bezier" 8 $ uncurryList8 Bezier

parseBox :: Parser Command
parseBox = makeCmdArgParser "box" 6 $ uncurryList6 Box

parseIdentity :: Parser Command
parseIdentity = makeCmdParser "ident" Identity

parseScale :: Parser Command
parseScale = makeCmdArgParser "scale" 3 $ uncurryList3 Scale

parseTranslate :: Parser Command
parseTranslate = makeCmdArgParser "translate" 3 $ uncurryList3 Translate

parseRotateX :: Parser Command
parseRotateX = makeCmdArgParser "xrotate" 1 $ uncurryList1 RotateX

parseRotateY :: Parser Command
parseRotateY = makeCmdArgParser "yrotate" 1 $ uncurryList1 RotateY

parseRotateZ :: Parser Command
parseRotateZ = makeCmdArgParser "zrotate" 1 $ uncurryList1 RotateZ

parseApply :: Parser Command
parseApply = makeCmdParser "apply" Apply

parseDisplay :: Parser Command
parseDisplay = makeCmdParser "display" Display

parseSave :: Parser Command
parseSave = do
  matchString "save"
  filename <- takeWhile1 (not . isSpace)
  return $ Save (unpack filename)

parseFile :: Parser [Command]
parseFile = many $
  parseLine      <|>
  parseCircle    <|>
  parseHermite   <|>
  parseBezier    <|>
  parseBox       <|>
  parseIdentity  <|>
  parseScale     <|>
  parseTranslate <|>
  parseRotateX   <|>
  parseRotateY   <|>
  parseRotateZ   <|>
  parseApply     <|>
  parseDisplay   <|>
  parseSave

readScriptH :: Handle -> IO (Either String [Command])
readScriptH h = parseOnly parseFile <$> hGetContents h

readScript :: FilePath -> IO (Either String [Command])
readScript file = withFile file ReadMode readScriptH
