module Language.MDL.Interp (
  execute
  ) where

import Data.Foldable
import Data.Maybe
import Data.Monoid
import Text.Printf
import System.IO.Temp
import System.Process
import System.FilePath
import Control.Concurrent
import Control.Monad
import Prelude hiding (filter)

import Language.MDL.Expr
import Language.MDL.Interp.Eval
import Language.MDL.Interp.Interp
import Language.MDL.SymTab hiding (filter, foldl)
import Forking

execute :: Foldable f => f Expr -> IO ()
execute expr = withSystemTempDirectory "graphics" $ flip execute' expr

execute' :: Foldable f => FilePath -> f Expr -> IO ()
execute' tmpdir exprs = do
  mapM_ (\(st, i) -> forkChild $ evalInterp (interp >> saveFrame i) st) (zip states [1..])
  waitForChildren
  callCommand $ "convert -delay 10 " ++ combos "simple" ++ " " ++ "simple" <.> "gif"
  where
    interp = mapM_ eval exprs
    states = (\st -> baseState { symtab = st}) <$> genVarySymTabs nFrames exprs
    baseState = initState { nframes = nFrames, basename = mBname }
    nFrames = fromMaybe 1 $ getNumFrames exprs
    mBname   = getBasename exprs
    saveFrame i = case mBname of
                    Just bname -> eval $ Save $ tmpdir </> mkName bname i
                    Nothing    -> return ()
    combos s = unwords [mkName (tmpdir </> s) i | i <- [1..nFrames]]

mkName :: FilePath -> Int -> FilePath
mkName = printf "%s%03d.gif"

genVarySymTabs :: Foldable f => Int -> f Expr -> [SymTab]
genVarySymTabs nFrames exprs = genSymTab exprs <$> [0..nFrames - 1]

genSymTab :: Foldable f => f Expr -> Int -> SymTab
genSymTab exprs i = foldl (flip $ addVaryVal i) empty exprs

addVaryVal :: Int -> Expr -> SymTab -> SymTab
addVaryVal i (Vary knob startFrame endFrame startVal endVal) = insert knob $ DoubleVal val
  where val = lerp (round $ endFrame - startFrame) startVal endVal i
addVaryVal _ _ = id

lerp :: Fractional a => Int -> a -> a -> Int -> a
lerp n start end i = ((end - start) / n' * i' ) + start
  where n' = fromIntegral n
        i' = fromIntegral i
{-# SPECIALIZE lerp :: Int -> Double -> Double -> Int -> Double #-}

getNumFrames :: Foldable f => f Expr -> Maybe Int
getNumFrames = fmap (round . getFrames) . findLast isFrames
  where
    isFrames Frames {} = True
    isFrames _          = False
    getFrames (Frames n) = n
    -- should never happen
    getFrames _          = error "getFrames called on some non-Frame Expr"

getBasename :: Foldable f => f Expr -> Maybe FilePath
getBasename = fmap getName . findLast isBasename
  where
    isBasename Basename {} = True
    isBasename _            = False
    getName (Basename name) = name
    -- should never happen
    getName _          = error "getName called on some non-Basename Expr"

findLast :: Foldable t => (a -> Bool) -> t a -> Maybe a
findLast p = getLast . foldMap f
  where f x = if p x then Last (Just x) else Last Nothing
