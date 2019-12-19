module Main where
import Text.PhoneticCode.Soundex (soundexNARA, soundexSimple)
import Text.PhoneticCode.Phonix (phonix)
import Data.List.Split (splitOn)
import System.Environment

countWords :: Eq a => [a] -> [(a,Int)]
countWords [] = []
countWords xs =  (head xs, count (head xs) xs) : countWords refilter
     where refilter =  filter (/=(head xs)) xs
           count x =  length . filter (==x) 


main :: IO ()
main = do
  print "Input a txt file:"
  args <- getLine
  input <- readFile args
  let ls = lines input
  let ws =  concat $ map words ls 
  let phx = map soundexNARA ws
  let cw = countWords phx
  print $ "The text file is "
    ++ show (length phx) 
    ++ " words long with  " 
    ++ show (length cw) 
    ++ " unique sounds"
  