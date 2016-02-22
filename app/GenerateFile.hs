module GenerateFile (generateFile) where

import Point

generateFile :: String -> Point -> IO ()
generateFile path maxPoint = putStrLn $ "This is a dummy main. It would have created a picture of size " ++ size ++ " at " ++ path
  where size = x ++ "x" ++ y
        Pair x y = show <$> maxPoint
