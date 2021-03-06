{-# LANGUAGE TemplateHaskell #-}

import Control.Monad.State.Lazy
import Data.List
import Data.Char
import Control.Lens

data Env = Env { _dict :: [(Char, Char)] , _text :: String, _chars :: [Char]}
   deriving Show
makeLenses ''Env
 
type Translation = StateT Env IO 

emptyTranslation      :: String -> [Char] -> Env
emptyTranslation s cs = Env { _dict = zip cs cs , _text = s, _chars = cs}

start :: String -> IO Env
start s = execStateT trans (emptyTranslation s (allChars s))

allChars   :: String -> [Char]
allChars s = map head $ group $ sort $ s

trans :: Translation ()
trans = do
    t <- gets _text
    d <- gets _dict
    liftIO $ putStrLn $ "Current text"
    liftIO $ putStrLn $ translate d t
    liftIO $ putStrLn "What letter do you want to change?"
    l <- liftIO $ getLine
    case l of
       "quit" -> return ()
       "stat" -> liftIO (putStrLn $ show $ getStatistic $ t) >> liftIO getLine >> trans
       "both" -> liftIO (putStrLn $ show $ zip (words t) (words (translate d t))) >> liftIO getLine >> trans
       "dict" -> liftIO (putStrLn $ show $ d) >> liftIO getLine >> trans
       "restart" -> let cs = allChars t in dict .= zip cs cs >> trans
       "solved"  -> liftIO (putStrLn $ show $ map head $ group $ sort $ filter (\s -> and (map isLower s)) (words (translate d t))) >> liftIO getLine >> trans
       "words"   -> let ws = group $ sort $ words (translate d t) in liftIO ( putStrLn $ show $ sort $ zip (map length ws) (map head ws)) >> liftIO getLine >> trans
       "find"    -> liftIO Main.find >> liftIO getLine >> trans
       _      -> do
             liftIO $ putStrLn "Change into what?"
             l' <- liftIO $ getLine
             updateDict l l'

             d' <- gets _dict
             liftIO $ putStrLn $ "The following words changed"
             liftIO $ putStrLn $ show $ sort $ filter (\(a,b) -> a /= b) (zip (words (translate d t)) (words (translate d' t)))
             l'' <- liftIO getLine 
             case l'' of
                "rev" -> dict .= d >> trans
                _     -> trans
      where
        updateDict :: String -> String -> Translation ()
        updateDict [] _ = return ()
        updateDict _ [] = return ()
        updateDict (c:cs) (c':cs') = do
                     dict %= map (\(a,b) -> if a == c then (a,c') else (a,b))
                     updateDict cs cs'
               

translate   :: [(Char, Char)] -> String -> String
translate d = map (\c -> case lookup c d of
      (Just c') -> c'
      Nothing   -> c) 

getStatistic :: String -> [(Float, Char)]
getStatistic s = reverse $ sort $ map (\l -> ((fromIntegral 100) *(fromIntegral (length l)) / (fromIntegral (length s)), l !! 0)) sorted
    where
        sorted = group $ sort $ s

find = do
  putStrLn "Which lang? english, french, greek, italian, latin or spanish?"
  lang <- getLine
  cnt <- readFile $ "wordlists/" ++ lang ++ ".txt"
  putStrLn "Put word to find"
  s <- getLine
  mapM_ putStrLn $ findWord s (lines cnt)
  return ()

findWord :: String -> [String] -> [String]
findWord s ss = do getMatch ss (getSame s) 
     where 
       getMatch :: [String] -> ([[Int]], [[(Char,Int)]]) -> [String]
       getMatch [] is = []
       getMatch (s:ss) ci@(is, ciss) = if (length s == (length (concat is) + length (concat ciss))) && (and $ map (\cs -> length cs == 1) $ map (nub.map (s!!)) is) && (and $ concat $ (map (map (\(c,i) -> s!!i == c)) ciss)) then s:getMatch ss ci else getMatch ss ci
       getSame :: String -> ([[Int]], [[(Char,Int)]])
       getSame s = do
         let ciss = getSame' s
         let cs  = filter (\cis -> isAsciiLower(s!!(snd $ cis!!0))) ciss
         let is  = map (snd.unzip) (ciss \\ cs)
         (is, cs)
         
       getSame'   :: String -> [[(Char, Int)]]
       getSame' s =  groupBy (\(c,i) (c',i') -> c == c') (sort (zip s ([0..] :: [Int])))
           
text1 :: String
text1 = "BT JPX RMLX PCUV AMLX ICVJP IBTWXVR CI M LMT'R PMTN, MTN YVCJX CDXV MWMBTRJ JPX AMTNGXRJBAH UQCT JPX QGMRJXV CI JPX YMGG CI JPX HBTW'R QMGMAX; MTN JPX HBTW RMY JPX QMVJ CI JPX PMTN JPMJ YVCJX. JPXT JPX HETW'R ACUTJXTMTAX YMR APMTWXN, MTN PBR JPCUWPJR JVCUFGXN PBL, RC JPMJ JPX SCBTJR CI PBR GCBTR YXVX GCCRXN, MTN PBR HTX XR RLCJX CTX MWMBTRJ MTCJPXV. JPX HBTW AVBXN MGCUN JC FVBTW BT JPX MRJVCGCWXVR, JPX APMGNXMTR, MTN JPX RCCJPRMEXVR. MTN JPX HBTW RQMHX, MTN RMBN JC JPX YBRX LXT CI FMFEGCT, YPCR CXDXV RPMGG VXMN JPBR YVBJBTW, MTN RPCY LX JPX BTJXVQVXJMJBCT JPXVXCI, RPMGG FX AGCJPXN YBJP RAMVGXJ, MTN PMDX M APMBT CI WCGN MFCUJ PBR TXAH, MTN RPMGG FX JPX JPBVN VUGXV BT JPX HBTWNCL. JPXT AMLX BT MGG JPX HBTW'R YBRX LXT; FUJ JPXE ACUGN TCJ VXMN JPX YVBJBTW, TCV LMHX HTCYT JC JPX HBTW JPX BTJXVQVXJMJBCT JPXVXCI. JPXT YMR HBTW FXGRPMOOMV WVXMJGE JVCUFGXN, MTN PBR ACUTJXTMTAX YMR APMTWXN BT PBL, MTN PBR GCVNR YXVX MRJCTBRPXN. TCY JPX KUXXT, FE VXMRCT CI JPX YCVNR CI JPX HBTW MTN PBR GCVNR, AMLX BTJC JPX FMTKUXJ  PCURX; MTN JPX KUXXT RQMHX MTN RMBN, C HBTW, GBDX ICVXDXV; GXJ TCJ JPE JPCUWPJR JVCUFGX JPXX, TCV GXJ JPE ACUTJXTMTAX FX APMTWXN; JPXVX BR M LMT BT JPE HBTWNCL, BT YPCL BR JPX RQBVBJ CI JPX PCGE WCNR; MTN BT JPX NMER CI JPE IMJPXV GBWPJ MTN UTNXVRJMTNBTW MTN YBRNCL, GBHX JPX YBRNCL CI JPX WCNR, YMR ICUTN BT PBL; YPCL JPX HBTW TXFUAPMNTXOOMV JPE IMJPXV, JPX HBTW, B RME, JPE IMJPXV, LMNX LMRJXV CI JPX LMWBABMTR, MRJVCGCWXVR, APMGNXMTR, MTN RCCJPRMEXVR; ICVMRLUAP MR MT XZAXGGXTJ RQBVBJ, MTN HTCYGXNWX, MTN UTNXVRJMTNBTW, BTJXVQVXJBTW CI NVXMLR, MTN RPCYBTW CI PMVN RXTJXTAXR, MTN NBRRCGDBTW CI NCUFJR, YXVX ICUTN BT JPX RMLX NMTBXG, YPCL JPX HBTW TMLXN FXGJXRPMOOMV; TCY GXJ NMTBXG FX AMGGXN, MTN PX YBGG RPCY JPX BTJXVQVXJMJBCT. JPX IBVRJ ACNXYCVN BR CJPXGGC."


{- ANSWER
Env {_dict = [('A','c'),('B','i'),('C','o'),('D','v'),('E','y'),('F','b'),('G','l'),('H','k'),('I','f'),('J','t'),('K','K'),('L','m'),('M','a'),('N','d'),('O','z'),('P','h'),('Q','p'),('R','s'),('S','S'),('T','n'),('U','u'),('V','r'),('W','g'),('X','e'),('Y','w'),('Z','Z')]}
-}

text2 :: String
text2 = "IXDVMUFXLFEEFXSOQXYQVXSQTUIXWF*FMXYQVFJ*FXEFQUQXJFPTUFXMX*ISSFLQTUQXMXRPQEUMXUMTUIXYFSSFI*MXKFJF*FMXLQXTIEUVFXEQTEFXSOQXLQ*XVFWMTQTUQXTITXKIJ*FMUQXTQJMVX*QEYQVFQTHMXLFVQUVIXM*XEI*XLQ*XWITLIXEQTHGXJQTUQXSITEFLQVGUQX*GXKIEUVGXEQWQTHGXDGUFXTITXDIEUQXGXKFKQVXSIWQXAVPUFXWGXYQVXEQJPFVXKFVUPUQXQXSGTIESQTHGX*FXWFQFXSIWYGJTFXDQSFIXEFXGJPUFXSITXRPQEUGXIVGHFITXYFSSFI*CXC*XSCWWFTIXSOQXCXYQTCXYIESFCX*FXCKVQFXVFUQTPUFXQXKI*UCXTIEUVCXYIYYCXTQ*XWCUUFTIXLQFXVQWFXDCSQWWIXC*FXC*XDI**QXKI*IXEQWYVQXCSRPFEUCTLIXLC*X*CUIXWCTSFTIXUPUUQX*QXEUQ**QXJFCXLQX*C*UVIXYI*IXKQLQCX*CXTIUUQXQX*XTIEUVIXUCTUIXACEEIXSOQXTITXEPVJQCXDPIVXLQ*XWCVFTXEPI*IXSFTRPQXKI*UQXVCSSQEIXQXUCTUIXSCEEIX*IX*PWQXQVZXLFXEIUUIXLZX*ZX*PTZXYIFXSOQXTUVZUFXQVZKZWXTQX*Z*UIXYZEEIRPZTLIXTZYYZVKQXPTZXWITUZJTZXAVPTZXYQVX*ZXLFEUZTHZXQXYZVKQWFXZ*UZXUZTUIXRPZTUIXKQLPUZXTITXZKQZXZ*SPTZXTIFXSFXZ**QJVNWWIXQXUIEUIXUIVTIXFTXYFNTUIXSOQXLQX*NXTIKNXUQVVNXPTXUPVAIXTNSRPQXQXYQVSIEEQXLQ*X*QJTIXF*XYVFWIXSNTUIXUVQXKI*UQXF*XDQXJFVBVXSITXUPUUQX*BSRPQXBX*BXRPBVUBX*QKBVX*BXYIYYBXFTXEPEIXQX*BXYVIVBXFVQXFTXJFPXSIWB*UVPFXYFBSRPQFTDFTXSOQX*XWBVXDPXEIYVBXTIFXVFSOFPEIXX*BXYBVI*BXFTXSILFSQXQXQRPBUIV"


{-
[('*','*'),('A','A'),('B','B'),('C','C'),('D','D'),('E','E'),('F','F'),('G','G'),('H','H'),('I','o'),('J','J'),('K','K'),('L','L'),('M','M'),('N','N'),('O','h'),('P','P'),('Q','e'),('R','R'),('S','c'),('T','n'),('U','t'),('V','V'),('W','W'),('X',' '),('Y','Y'),('Z','Z')]


[('*','r'),('A','A'),('B','B'),('C','C'),('D','D'),('E','l'),('F','a'),('G','G'),('H','H'),('I','o'),('J','J'),('K','K'),('L','m'),('M','M'),('N','N'),('O','h'),('P','a'),('Q','e'),('R','R'),('S','t'),('T','n'),('U','t'),('V','r'),('W','W'),('X',' '),('Y','p'),('Z','Z')]-}
