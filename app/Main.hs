{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import qualified Data.ByteString as B
import Data.Char (isAlphaNum)
import Data.Foldable (fold)
import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import System.Directory (listDirectory)
import Text.Printf (printf)
import Prelude hiding (Word)

newtype Word' = Word' T.Text deriving (Show, Read, Eq, Ord)

mkWord :: T.Text -> Word'
mkWord = Word' . T.toUpper

wordToText :: Word' -> T.Text
wordToText (Word' w) = w

newtype Bow = Bow
  { -- | Bag of words to `Data.Map`
    bowToMap :: M.Map Word' Int
  }
  deriving (Show, Read)

wordToBow :: Word' -> Bow
wordToBow w = Bow $ M.fromList [(w, 1)]

textToBow :: T.Text -> Bow
textToBow = foldMap wordToBow . normalizeTextToWords

emptyBow :: Bow
emptyBow = Bow M.empty

instance Semigroup Bow where
  Bow b1 <> Bow b2 = Bow $ M.unionWith (+) b1 b2

instance Monoid Bow where
  mempty = emptyBow

data SpamModel = SpamModel
  { spamBow :: Bow,
    hamBow :: Bow
  }

spamModel :: IO SpamModel
spamModel = do
  spamBow <- bowFromFolder "./data/train/spam/"
  hamBow <- bowFromFolder "./data/train/ham/"
  return $ SpamModel spamBow hamBow

normalizeTextToWords :: T.Text -> [Word']
normalizeTextToWords = map mkWord . T.words . T.map (\x -> if isAlphaNum x then x else ' ')

wordsCount :: Bow -> Int
wordsCount (Bow bow) = sum $ map snd $ M.toList bow

wordProbablity :: Word' -> Bow -> Double
wordProbablity w bow = fromIntegral n / fromIntegral (wordsCount bow)
  where
    n = fromMaybe 0 $ M.lookup w $ bowToMap bow

readFileIfPossible :: FilePath -> IO (Maybe T.Text)
readFileIfPossible filePath = do
  bytes <- B.readFile filePath
  case T.decodeUtf8' bytes of
    Left _ -> return Nothing
    Right txt -> return (Just txt)

bowFromFile :: FilePath -> IO (Maybe Bow)
bowFromFile filePath = do
  contents <- readFileIfPossible filePath
  return (textToBow <$> contents)

bowFromFolder :: FilePath -> IO Bow
bowFromFolder folderPath = do
  fileNames <- listDirectory folderPath
  bows <- mapM (bowFromFile . (folderPath <>)) fileNames
  return $ fold $ catMaybes bows

wordProbablitySpam :: SpamModel -> Word' -> Maybe Double
wordProbablitySpam sm@(SpamModel spamBow hamBow) w
  | seenWord w sm =
    let pws = wordProbablity w spamBow
        phs = wordProbablity w hamBow
        ps = pws + phs
     in Just (pws / (pws + phs))
  | otherwise = Nothing

wordProbablityHam :: SpamModel -> Word' -> Maybe Double
wordProbablityHam sm@(SpamModel spamBow hamBow) w
  | seenWord w sm =
    let pws = wordProbablity w spamBow
        phs = wordProbablity w hamBow
        ps = pws + phs
     in Just (phs / (pws + phs))
  | otherwise = Nothing

textProbablitySpam :: SpamModel -> T.Text -> Double
textProbablitySpam sm text = pp / (pp + product ips)
  where
    ws = normalizeTextToWords text
    ps = mapMaybe (wordProbablitySpam sm) ws
    ips = map (1.0 -) ps
    pp = product ps

textProbablityHam :: SpamModel -> T.Text -> Double
textProbablityHam sm text = pp / (pp + product ips)
  where
    ws = normalizeTextToWords text
    ps = mapMaybe (wordProbablityHam sm) ws
    ips = map (1.0 -) ps
    pp = product ps

classifyText :: SpamModel -> T.Text -> (Double, Double)
classifyText sm t = (textProbablitySpam sm t, textProbablityHam sm t)

classifyFile :: SpamModel -> FilePath -> IO (Double, Double)
classifyFile sm fp = classifyText sm <$> T.readFile fp

classifyFolder :: SpamModel -> FilePath -> IO ()
classifyFolder sm folderPath = do
  fileNames <- listDirectory folderPath
  forM_
    fileNames
    ( \fileName -> do
        let filePath = folderPath <> "/" <> fileName
        stats <- classifyFile sm filePath
        printf "%s -> %s\n" filePath (show stats)
    )

summaryBow :: Bow -> IO ()
summaryBow (Bow bow) =
  forM_ (sortBy (compare `on` snd) $ M.toList bow) (\(w, f) -> printf "%s -> %d" (wordToText w) f)

seenWord :: Word' -> SpamModel -> Bool
seenWord w (SpamModel (Bow spamBow) (Bow hamBow)) = isJust sm || isJust hm
  where
    sm = M.lookup w spamBow
    hm = M.lookup w hamBow

main :: IO ()
main = print "Hi"
