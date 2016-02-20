import Pbm
import Picture
import System.Environment

main :: IO ()
main = do args <- getArgs
          respond args

respond :: [String] -> IO ()
respond [] = putStrLn "You didn't supply any args\nUsage: ./main <output file>"
respond (path:_) = makeFile path

makeFile :: String -> IO ()
makeFile path = writePbm path pic
  where f1 (Pair x y) = round $ exp (fromIntegral x * fromIntegral y)
        f2 (Pair x y) = x*y
        f3 (Pair x y) = round $ (sini x + sini y + 1) * 75
          where sini = sin . fromIntegral
        pic = mathPic (Triple f1 f2 f3) (Pair 500 500)
