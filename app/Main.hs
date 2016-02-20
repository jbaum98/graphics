import Pbm
import System.Environment

main :: IO ()
main = do args <- getArgs
          respond args

respond :: [String] -> IO ()
respond [] = putStrLn "You didn't supply any args\nUsage: ./main <output file>"
respond (path:_) = putStrLn $ "This is a dumby main.\nThe output file would have been " ++ path
