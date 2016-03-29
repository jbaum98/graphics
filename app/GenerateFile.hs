module GenerateFile (generateFile) where

import D2Point

generateFile :: String -> D2Point -> IO ()
generateFile path maxPoint = putStrLn $ "This is a dummy main. It would have created a picture of size " ++ size ++ " at " ++ path
  where size = x ++ "x" ++ y
        Pair x y = show <$> maxPoint
