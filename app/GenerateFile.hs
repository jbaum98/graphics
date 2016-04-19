module GenerateFile (generateFile) where

import D2Point
import Parser

generateFile :: FilePath -> D2Point -> IO ()
generateFile path maxPoint = do
  parse <- readScript path
  case parse of
    Left err -> putStrLn $ "Error on parsing: " ++ err
    Right cmds -> execute maxPoint cmds
