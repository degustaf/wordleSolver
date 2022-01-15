module Util ( 
  parse
  ) where

import Import
import qualified RIO.List as L
import qualified RIO.List.Partial as LP
import qualified RIO.Text as T

-- Although the input is alphabetical and has unique entries, it is case sensitive
-- so we've (probably) added duplicates with toUpper. This is corrected with nub.
parse :: Text -> [Text]
parse = nub . filter f . T.lines . T.toUpper
  where f :: Text -> Bool
        f x
          | (T.length x) /= 5 = False
          | otherwise = all (\c -> elem c ['A'..'Z']) $ T.unpack x

-- By adding the Ord constraint, we can improve performance from O(n^2) to O(n log n).
nub :: Ord a => [a] -> [a]
nub = L.map LP.head . L.group . L.sort
