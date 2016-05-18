module Language.MDL.Interp (
  execute
  ) where

import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Time.Clock
import Data.ByteString.Lazy.Char8 hiding (any, zip, unwords, foldl, empty)
import Text.Printf
import System.IO.Temp
import System.Process
import System.FilePath
import System.Directory
import Control.Concurrent
import Control.Monad
import Prelude hiding (filter)

import Language.MDL.Expr
import Language.MDL.Interp.Eval
import Language.MDL.Interp.Interp
import Language.MDL.SymTab hiding (filter, foldl)
import Forking

execute :: Foldable f => f Expr -> IO ()
execute exprs = if any animCmds exprs
                 then withSystemTempDirectory "graphics" $
                      executeAnimation bname nFrames exprs
                 else executeSinglePic exprs
  where
    Just bname   = getBasename exprs
    Just nFrames = getNumFrames exprs
    animCmds Frames {}   = True
    animCmds Vary {}     = True
    animCmds Basename {} = True
    animCmds _           = False

executeSinglePic :: Foldable f => f Expr -> IO ()
executeSinglePic exprs = evalInterp interp initState
  where interp = mapM_ eval exprs

executeAnimation :: Foldable f => ByteString -> Int -> f Expr -> FilePath -> IO ()
executeAnimation bname nFrames exprs tmpdir = do
  mapM_ (\(st, i) -> evalInterp (interp >> saveFrame i) st) (zip states [1..])
  callCommand $ "convert -delay 10 " ++ combos (unpack bname) ++ " " ++ unpack bname <.> "gif"
  where
    interp = mapM_ eval exprs
    states = (\st -> initState { symtab = st }) <$> genVarySymTabs nFrames exprs
    saveFrame i = eval $ Save $ pack $ tmpdir </> mkName (unpack bname) i
    combos s = unwords [mkName (tmpdir </> s) i | i <- [1..nFrames]]

mkName :: FilePath -> Int -> FilePath
mkName fp i = fp <> show i <.> "png"

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

getBasename :: Foldable f => f Expr -> Maybe ByteString
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
