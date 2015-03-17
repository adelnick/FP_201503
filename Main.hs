module Main

where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>), (<*>), (<*), (*>))
import System.IO 
import Data.List.Split (splitOn)

import           Text.Parsec
import           Text.Parsec.String  (Parser)

type Region     = String
type Bug        = String
type Quantity   = String
type RegionData = M.Map Bug Quantity
type AllBugs    = S.Set Bug
type BRQData    = M.Map Region RegionData
type BugDB      = (AllBugs, BRQData)

type QtyRegions   = (Quantity, [Region])
type BugInputData = (Bug, [QtyRegions])

------------ Maintaining and querying DB      ------------

addBugRecord :: Bug -> Quantity -> Region -> BugDB -> BugDB
addBugRecord b q r (a, rbq) = (S.insert b a, M.alter (Just . M.insert b q . fromMaybe M.empty) r rbq)

addBugInfo :: BugDB -> BugInputData -> BugDB
addBugInfo db (b, xs) = let ab   = addBugRecord b
                            abq  = map (\ (x,y) -> (ab x, y) ) xs
                            abqr = concat . map (\ (x,y) -> map x y ) $ abq
                        in foldr ($) (clearBugInfo db b) abqr

clearBugInfo :: BugDB -> Bug -> BugDB
clearBugInfo (a,db) b 
  | b `S.member` a = (S.delete b a, M.map (M.delete b) db)
  | otherwise    = (a,db)

fillBugDB :: BugDB -> [BugInputData] -> BugDB
fillBugDB db = foldl addBugInfo db

getNrOfBugs :: RegionData -> Int
getNrOfBugs = M.size

getNrOfBugsStat :: BugDB -> (Region, Int)
getNrOfBugsStat = map (\ (x,y) -> (x, getNrOfBugs y)). toList . snd

------------ Exporting to CSV    ------------

regionDataToCSV :: AllBugs -> RegionData -> String
regionDataToCSV a rd = S.foldl (rdToCSV rd) "" a
                       where rdToCSV rd s b = case (M.lookup b rd) of Just q   -> q ++ (';' : s)
                                                                      Nothing  -> (';' : s)

exportToCSV :: BugDB -> String
exportToCSV (a, rbq) = unlines . map (\ (x, y) -> x ++ (';' : regionDataToCSV a y)) . M.toList $ rbq

------------ Reading and Parsing ------------



lineParser :: Parser String
lineParser = manyTill anyChar (try newline)

bugParser :: Parser Bug
bugParser = lineParser

qtyRegionsParser :: Parser QtyRegions
qtyRegionsParser = (,) <$> manyTill anyChar (string ": ") 
                       <*> (getRegions <$> lineParser) -- it's easier to parse out a line and split it later, as the region name is not strictly defined

getRegions :: String -> [Region]
getRegions = splitOn ", "

bugInfoParser :: Parser BugInputData
bugInfoParser = (,) <$> bugParser <* spaces 
                    <*> many (qtyRegionsParser <* spaces)

parseBugInfo :: String -> Either ParseError BugInputData
parseBugInfo = runParser bugInfoParser () ""

readBugsData :: String -> IO (Maybe BugInputData)
readBugsData f = do { handle <- openFile f ReadMode
                    ; hSetEncoding handle utf8_bom
                    ; fc <- hGetContents handle
                    ; let ebi = parseBugInfo fc
                    ; return (case ebi of (Left err) -> Nothing
                                          (Right bi) -> Just bi)
                    }

---------------------------------------------

main :: IO ()
main = do { return ();
          }
	  