module Main

where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)

import           Text.Parsec
import           Text.Parsec.String  (Parser)

type Region     = String
type Bug        = String
type Quantity   = String
type RegionData = M.Map Bug Quantity
type BugDB      = M.Map Region RegionData

type QtyRegions   = (Quantity, [Region])
type BugInputData = (Bug, [QtyRegions])

addBugRecord :: Bug -> Quantity -> Region -> BugDB -> BugDB
addBugRecord r b q = M.alter (Just . M.insert b q . fromMaybe M.empty) r

addBugInfo :: BugInputData -> BugDB -> BugDB
addBugInfo (b, xs) db = let ab   = addBugRecord b
                            abq  = map (\ (x,y) -> (ab x, y) ) xs
                            abqr = concat . map (\ (x,y) -> map x y ) $ abq
                        in foldr ($) db abqr

fillBugDB :: BugDB -> [BugInputData] -> BugDB
fillBugDB db = foldr addBugInfo db 

------------ Parsing ------------

lineParser :: Parser String
lineParser = manyTill anyChar (try newline)

bugParser :: Parser Bug
bugParser = lineParser

qtyRegionsParser :: Parser QtyRegions
qtyRegionsParser = do { l <- lineParser
                      ; let (q, rs) = span (/= ':') l
                      ; let rs' = getRegions (tail rs)
                      ; return (q, rs')
                      }

getRegions :: String -> [Region]
getRegions = gR []
             where gR rs [] = rs
                   gR rs xs = let (r,xs') = span (/= ',') xs
                              in gR (tail r : rs) xs' -- Regions in reverse order but that's ok


{-
parseBugsData :: String -> Parser BugInputData
readBugsData = do { b <- lineParser <* spaces 
                  ; 
-}
main :: IO ()
main = do { return ();
          }
	  