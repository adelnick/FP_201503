{-# LANGUAGE OverloadedStrings #-}

module Main

where

import System.IO
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Data.List (intercalate)
import Data.List.Split (splitOn)
import qualified Data.Text    as T
import qualified Data.Text.IO as T


type Region     = String
type Bug        = String
type Quantity   = String
type RegionData = M.Map Bug Quantity
type AllBugs    = S.Set Bug
type BRQData    = M.Map Region RegionData
type BugDB      = (AllBugs, BRQData)

type QtyRegions   = (Quantity, [Region])
type BugInputData = (Bug, [QtyRegions])

type States      = M.Map Region Double
type Frequencies = M.Map Quantity Double

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

getNrOfBugsStat :: BugDB -> [(Region, Int)]
getNrOfBugsStat = map (\ (x,y) -> (x, getNrOfBugs y)) . M.toList . snd

------------ Exporting to CSV    ------------
allBugsToCSV :: AllBugs -> String
allBugsToCSV = (';' : ) . intercalate ";" . S.toList

regionDataToCSV :: AllBugs -> RegionData -> String
regionDataToCSV a rd = S.foldl (rdToCSV rd) "" a
                       where rdToCSV rd s b = case (M.lookup b rd) of Just q   -> q ++ (';' : s)
                                                                      Nothing  -> (';' : s)

exportToCSV :: BugDB -> String
exportToCSV (a, rbq) = unlines . (allBugsToCSV a :) . map (\ (x, y) -> x ++ (';' : regionDataToCSV a y)) . M.toList $ rbq

printUTFString :: String -> IO ()
printUTFString = T.putStr . T.pack

------------ Reading and Parsing ------------

qtyRegionsParser :: String -> QtyRegions
qtyRegionsParser s = let (q,rs) = span (/= ':') s
                     in (q, splitOn ", " (tail rs))

parseBugInfo :: String -> BugInputData                               -- No error handling, it's tedious
parseBugInfo xs = let (b : _ : ys) = lines xs
                  in (b, map qtyRegionsParser ys)

readBugsData :: String -> IO (BugInputData)
readBugsData f = do { t <- T.readFile f
                    ; return $ parseBugInfo . T.unpack $ t
                    }
{-
readBugsData f = do { handle <- openFile f ReadMode
                    ; hSetEncoding handle utf8_bom
                    ; fc <- hGetContents handle
                    ; return $ parseBugInfo fc
                    }
-}

parseStates :: String -> States
parseStates s = foldl insState M.empty (map convert (lines s))
                where convert = (\(x,y) -> (tail y, read x :: Double)) . span (/= ' ')
                      insState m (s,v) = M.insert s v m

readStates :: String -> IO (States)
readStates f = do { t <- T.readFile f
                  ; return $ parseStates . T.unpack $ t
                  }

--parseFrequencies :: String -> Frequencies
--parseFrequencies = map (insert . flip . span (== ' ')) . lines

---------------------------------------------

main :: IO ()
main = do { c <- readBugsData "D:/Git/FP_201503/data/Aurata.dat"
          ; let db = addBugInfo (S.empty, M.empty) c
          ; let csv = exportToCSV db
          ; print csv
          ; printUTFString csv
          ; s <- readStates "D:/Git/FP_201503/data/States.txt"
          ; printUTFString $ show s
          }
	  