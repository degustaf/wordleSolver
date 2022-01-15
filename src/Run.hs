module Run (run) where

import Import
import Util
import qualified RIO.ByteString as B
import qualified RIO.List as L
import qualified RIO.Map as Map
import qualified RIO.Text as T

run :: RIO App ()
run = do
  input <- B.readFile "/usr/share/dict/words"
  data_ <- case T.decodeUtf8' input of
    Left err -> throwIO err
    Right data_' -> return data_'
  wrds <- return $ parse data_
  logInfo $ "We found " <> (display $ length wrds) <> " words."
  logInfo $ "We found " <> (display $ mostCommonLetters 10 wrds) <> " words."
  let wrds' = f wrds
  logInfo $ "We found " <> (display $ length wrds') <> " words."
  logInfo $ "We found " <> (display $ mostCommonLetters 10 wrds') <> " words."
  logInfo $ "We found " <> (display $ T.intercalate ", " $ f' wrds') <> " words."


mostCommonLetters :: Int -> [Text] -> Text
mostCommonLetters n = tshow . take n . mostCommonLetters'

mostCommonLetters' :: [Text] -> [(Char, Int)]
mostCommonLetters' = L.sortBy (\(_,x) (_,y) -> compare y x) . Map.toList . L.foldl g Map.empty 
  where g = T.foldl (\m c -> Map.insertWith (+) c 1 m)


f' :: [Text] -> [Text]
f' = puzzleState -- $ (mustHave 'A') :
                   []


f :: [Text] -> [Text]
f = puzzleState $ -- (mustHave 'A') :
                  noMatches ""

matched :: Int -> Char -> Text -> Bool
matched n c txt = T.index txt n == c

mustHave :: Char -> Text -> Bool
mustHave c = T.any (==c)

notMatched :: Int -> Char -> Text -> Bool
notMatched n c = not . (matched n c)

noMatch :: Char -> Text -> Bool
noMatch c = not . T.any (==c)

noMatches :: Text -> [Text->Bool]
noMatches = map noMatch . T.unpack

puzzleState :: [Text->Bool] -> [Text] -> [Text]
puzzleState fs wrds = foldr filter wrds fs
