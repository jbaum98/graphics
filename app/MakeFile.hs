module MakeFile (makeFile) where

makeFile :: FilePath -> IO ()
makeFile path = putStrLn $ "This is a dumby main.\nThe output file would have been " ++ path
