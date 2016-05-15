module Language.MDL.Interp (
  execute
  ) where

import Data.Maybe
import Data.Monoid

import Language.MDL.Expr
import Language.MDL.Interp.Eval
import Language.MDL.Interp.Interp

execute :: Foldable f => f Expr -> IO ()
execute exprs = do
  flip evalInterp state . mapM_ eval $ exprs
  putStrLn $ "nFrames: " ++ show nFrames
  putStrLn $ "basename: " ++ show bname
  where
    state   = initState { nframes = nFrames, basename = bname }
    nFrames = fromMaybe 1 $ getNumFrames exprs
    bname   = getBasename exprs

getNumFrames :: Foldable f => f Expr -> Maybe Int
getNumFrames = fmap (round . getFrames) . findLast isFrames
  where
    isFrames (Frames _) = True
    isFrames _          = False
    getFrames (Frames n) = n
    -- should never happen
    getFrames _          = error "getFrames called on some non-Frame Expr"

getBasename :: Foldable f => f Expr -> Maybe FilePath
getBasename = fmap getName . findLast isBasename
  where
    isBasename (Basename _) = True
    isBasename _            = False
    getName (Basename name) = name
    -- should never happen
    getName _          = error "getName called on some non-Basename Expr"

findLast :: Foldable t => (a -> Bool) -> t a -> Maybe a
findLast p = getLast . foldMap f
  where f x = if p x then Last (Just x) else Last Nothing
