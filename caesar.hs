import Data.Maybe
import Data.List
import Data.Char

cipher = "MHILY LZA ZBHL XBPZXBL MVYABUHL HWWPBZ JSHBKPBZ JHLBJZ KPJABT HYJHUBT LZA ULBAYVU"

alph :: [(Char, Int)]
alph = zip ['a'..'z'] [0..]

caesar :: Int -> [(Char, Int)]
caesar i = zip ['a'..'z'] $ map (\i' -> mod i' 26 ) [i..]

getNewChar :: Char -> Int -> Char
getNewChar c shift = do
     let i  = fromJust $ lookup c alph 
     let (_, i') = (caesar shift) !! i
     fst $ alph !! i'

getNewString :: String -> Int -> String
getNewString s shift = concat $ intersperse " " $ map (map (\c -> getNewChar (toLower c) shift )) (words s)
