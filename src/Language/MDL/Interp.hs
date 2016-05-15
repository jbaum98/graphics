module Language.MDL.Interp (
  execute
  ) where

import Data.Foldable
import Data.Maybe
import Data.Monoid
import Prelude hiding (filter)

import Language.MDL.Expr
import Language.MDL.Interp.Eval
import Language.MDL.Interp.Interp
import Language.MDL.SymTab hiding (filter, foldl)

execute :: Foldable f => f Expr -> IO ()
execute exprs = mapM_ (evalInterp interp) states
  where
    interp = mapM_ eval exprs
    states = (\st -> baseState { symtab = st}) <$> genVarySymTabs nFrames exprs
    baseState = initState { nframes = nFrames, basename = bname }
    nFrames = fromMaybe 1 $ getNumFrames exprs
    bname   = getBasename exprs

genVarySymTabs :: Foldable f => Int -> f Expr -> [SymTab]
genVarySymTabs nFrames exprs = genSymTab exprs <$> [1..nFrames]

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
