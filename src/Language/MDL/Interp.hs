module Language.MDL.Interp (
  execute
  ) where

import Control.Monad
import Data.ByteString.Lazy.Char8 hiding (any, zip, foldl, empty)
import Data.Monoid
import System.FilePath
import System.IO.Temp
import System.Posix.Process

import Language.MDL.Expr
import Language.MDL.Interp.Eval
import Language.MDL.Interp.Interp
import Language.MDL.SymTab hiding (foldl)

-- | Exexecute the actions represented by the 'Expr's
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

-- | Execute the actoins represented by the 'Expr's to produce a single picture
executeSinglePic :: Foldable f => f Expr -> IO ()
executeSinglePic exprs = evalInterp interp initState
  where interp = mapM_ eval exprs

-- | Execute the actions represented by the 'Expr's to produce an animation
executeAnimation :: Foldable f
                 => ByteString -- ^ The basename of the output file
                 -> Int        -- ^ The number of frames
                 -> f Expr
                 -> FilePath   -- ^ The path to the system temporary directory
                 -> IO ()
executeAnimation bname nFrames exprs tmpdir = do
  res <- mapM (\(st, i) -> evalInterp (interp >> saveFrame i) st) (zip states [1..])
  return $ seq res ()
  pid <- forkProcess $
    let args = "-delay" : "10" : combos (unpack bname) ++ [unpack bname <.> "gif"]
    in executeFile "convert" True args Nothing
  void $ getProcessStatus True False pid
  where
    interp = mapM_ eval exprs
    states = (\st -> initState { symtab = st }) <$> genVarySymTabs nFrames exprs
    saveFrame i = eval $ Save $ pack $ tmpdir </> mkName (unpack bname) i
    combos s = [mkName (tmpdir </> s) i | i <- [1..nFrames]]

-- | Create a name from a number and a base name, like "pic-1.png"
mkName :: FilePath -> Int -> FilePath
mkName fp i = fp <> show i <.> "png"

-- | Generate a list of 'SymTab's, one for each frame of an animation containing
-- the correct values for each knob
genVarySymTabs :: Foldable f
               => Int    -- ^ The number of frames
               -> f Expr -> [SymTab]
genVarySymTabs nFrames exprs = genSymTab exprs <$> [0..nFrames - 1]

-- | Generate the correct 'SymTab' for a single frame of an animation
genSymTab :: Foldable f => f Expr
          -> Int -- ^ The number of the current frame
          -> SymTab
genSymTab exprs i = foldl (flip $ addVaryVal i) empty exprs

-- | Add the correct value for a varying knob to a 'SymTab' for a single frame
addVaryVal :: Int -- ^ The number of the current frame
           -> Expr -> SymTab -> SymTab
addVaryVal i (Vary _     startFrame endFrame _       _)
  | i < round startFrame || i > round endFrame = id
addVaryVal i (Vary knob startFrame endFrame startVal endVal) =
  insert knob $ DoubleVal val
  where val = lerp (round $ endFrame - startFrame) startVal endVal i
addVaryVal _ _ = id

-- | Linearly interpolate between two values in a given number of steps
lerp :: Fractional a
     => Int -- ^ The total number of steps
     -> a   -- ^ The starting value
     -> a   -- ^ The ending value
     -> Int -- ^ The number of the step
     -> a   -- ^ The value at that step
lerp n start end i = ((end - start) / n' * i' ) + start
  where n' = fromIntegral n
        i' = fromIntegral i
{-# SPECIALIZE lerp :: Int -> Double -> Double -> Int -> Double #-}

-- | Find the number of frames set by the last "frames" statement, if there are
-- any
getNumFrames :: Foldable f => f Expr -> Maybe Int
getNumFrames = fmap (round . getFrames) . findLast isFrames
  where
    isFrames Frames {} = True
    isFrames _          = False
    getFrames (Frames n) = n
    -- should never happen
    getFrames _          = error "getFrames called on some non-Frame Expr"

-- | Find the basename set by the last "basename" statement, if there are any
getBasename :: Foldable f => f Expr -> Maybe ByteString
getBasename = fmap getName . findLast isBasename
  where
    isBasename Basename {} = True
    isBasename _            = False
    getName (Basename name) = name
    -- should never happen
    getName _          = error "getName called on some non-Basename Expr"

-- | Find the last element satisfying a predicate
findLast :: Foldable t => (a -> Bool) -> t a -> Maybe a
findLast p = getLast . foldMap f
  where f x = if p x then Last (Just x) else Last Nothing
