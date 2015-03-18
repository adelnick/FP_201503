{-# LANGUAGE TupleSections #-}

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

type BugExtinctionRisk = (Bug,Double)

type Dataset = (BugDB, States, Frequencies)

trimLeft :: String -> String
trimLeft = dropWhile (== ' ')
------------ Maintaining and querying Dataset      ------------

createDS = ((S.empty, M.empty), M.empty, M.empty)

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

calcExtinctionRiskPerRegion :: Double -> Bug -> Frequencies -> RegionData -> Double
calcExtinctionRiskPerRegion rCoef b f rd = let mbQ = M.lookup b rd
                                           in case mbQ of (Just q) -> (f M.! q) * rCoef


calcBugExtinctionRisk :: BRQData -> States -> Frequencies -> Bug -> BugExtinctionRisk
calcBugExtinctionRisk brq s f b = (b, M.foldWithKey getRisk 0.0 brq)
                                 where getRisk r rd extCoef = extCoef + (s M.! r) * (fCoef rd)
                                       fCoef rd = let mbQ = M.lookup b rd
                                                  in case mbQ of (Just q) -> (f M.! q)
                                                                 Nothing  -> 0.0
 

calcExtinctionRisk :: Dataset -> [BugExtinctionRisk]
calcExtinctionRisk ((a,brq),s,f) = map (calcBugExtinctionRisk brq s f) $ S.toList a

------------ Export             ------------
allBugsToCSV :: AllBugs -> String
allBugsToCSV = (';' : ) . intercalate ";" . S.toList

regionDataToCSV :: AllBugs -> RegionData -> String
regionDataToCSV a rd = S.foldr (rdToCSV rd) "" a
                       where rdToCSV rd b s = case (M.lookup b rd) of Just q   -> q ++ (';' : s)
                                                                      Nothing  -> (';' : s)

exportToCSV :: BugDB -> String
exportToCSV (a, rbq) = unlines . (allBugsToCSV a :) . map (\ (x, y) -> x ++ (';' : regionDataToCSV a y)) . M.toList $ rbq

printUTFString :: String -> IO ()
printUTFString = T.putStr . T.pack

showNrOfBugsStat :: [(Region, Int)] -> String
showNrOfBugsStat = unlines . map (\ (x,y) -> x ++ (':' : ' ' : show y))

showExtinctionRisk :: BugExtinctionRisk -> String
showExtinctionRisk (b,x) = b ++ (';' : ' ' : show x)

------------ Reading and Parsing ------------

qtyRegionsParser :: String -> QtyRegions
qtyRegionsParser s = let (q,rs) = span (/= ':') s
                     in (q, splitOn ", " (tail .tail $ rs))

parseBugInfo :: String -> BugInputData                               -- No error handling, it's tedious
parseBugInfo xs = let (b : _ : ys) = lines xs
                  in (b, map qtyRegionsParser ys)

readBugData :: String -> IO (BugInputData)
readBugData f = do { t <- T.readFile f
                    ; return $ parseBugInfo . T.unpack $ t
                    }
parseStates :: String -> States
parseStates s = foldl insState M.empty (map convert (filter (not . null) (lines s)))
                where convert = (\(x,y) -> (trimLeft y, read x :: Double)) . span (/= ' ')
                      insState m (s,v) = M.insert s v m

readStates :: String -> IO (States)
readStates f = do { t <- T.readFile f
                  ; return $ parseStates . T.unpack $ t
                  }

parseFrequencies :: String -> Frequencies
parseFrequencies s = foldl insFrequency M.empty (map convert (filter (not . null) (lines s)))
                where convert = (\(x,y) -> (trimLeft y, read x :: Double)) . span (/= ' ')
                      insFrequency m (s,v) = M.insert s v m

readFrequencies :: String -> IO (Frequencies)
readFrequencies f = do { t <- T.readFile f
                  ; return $ parseFrequencies . T.unpack $ t
                  }

---------------------------------------------

menuLoad :: BugDB -> IO (BugDB)
menuLoad db = do { print "Input file name"
                 ; f <- getLine
                 ; b <- readBugData f
                 ; return $ addBugInfo db b
                 }

menuExport :: BugDB -> IO ()
menuExport db = do { print "Input file name"
                   ; f <- getLine
                   ; T.writeFile f (T.pack . exportToCSV $ db)
--                   ; printUTFString $ exportToCSV db
                   }

menuNrOfBugs :: BugDB -> IO ()
menuNrOfBugs = printUTFString . showNrOfBugsStat . getNrOfBugsStat

menuLoadStatesAndFrequencies :: IO ((States,Frequencies))
menuLoadStatesAndFrequencies = do { print "Input states file name"
                                  ; sf <- getLine
                                  ; s <- readStates sf
                                  ; print "Input frequencies file name"
                                  ; ff <- getLine
                                  ; f <- readFrequencies ff
                                  ; return (s,f)
                                  }

menuExtinctionRisk :: Dataset -> IO ()
menuExtinctionRisk = printUTFString . unlines . map showExtinctionRisk . calcExtinctionRisk

userMenu :: Dataset -> IO ()
userMenu ds@(db,s,f) = do { print "1 - Load bug info from file"
                          ; print "2 - Export to CSV"
                          ; print "3 - Display number of bugs per region"
                          ; print "4 - Load state and frequency info"
                          ; print "5 - Display extinction risk"
                          ; print "0 - Exit"
                          ; c <- getLine
                          ; case c of "1" -> (,s,f) <$> menuLoad db >>= userMenu
                                      "2" -> menuExport db >> userMenu ds
                                      "3" -> menuNrOfBugs db >> userMenu ds
                                      "4" -> uncurry (db,,) <$> menuLoadStatesAndFrequencies >>= userMenu
                                      "5" -> menuExtinctionRisk ds >> userMenu ds
                                      "0" -> return ()
                 }


main :: IO ()
main = do { userMenu createDS
          ; f <- readStates "S.txt"
          ; printUTFString . show $ f
          }
	  