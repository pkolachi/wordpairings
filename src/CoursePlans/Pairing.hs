module Main where

import Prelude hiding ( Word )
import Data.Map( Map, (!) )
import qualified Data.Map as M
import Data.Set( Set )
import qualified Data.Set as S
import Data.List hiding ( nub )
import Data.Maybe
import Data.Char
import System.Process( system )
import System.IO( hFlush, getChar, stdout, hSetBuffering, stdin, BufferMode(..) )

--------------------------------------------------------------------------------
-- main

{-
parts :: [String] -> [[String]]
parts (".":xs) = parts xs
parts (w:".":xs) =
  case parts xs of
    []   -> [b]
    a:as | w `elem` abbrs -> (b++a):as
         | otherwise      -> b:a:as
 where
  b = w : [ "." | w `elem` abbrs ]
parts (w:xs) =
  case parts xs of
    []   -> [[w]]
    a:as -> (w:a):as
parts []       = []

abbrs = ["bzw", "no", "nr", "d", "h", "z"]
     
     putStr $ unlines $
       [ show (length p) ++ "+" ++ show (length q) ++ " --> "
      ++ show (map length ps) ++ "+" ++ show (map length qs)
      ++ (if length ps /= length qs then " !" else "") ++ "\n"
      ++ show [ v | (v,".") <- p `zip` tail p ] ++ "+"
      ++ show [ v | (v,".") <- q `zip` tail q ] ++ "+"
       | (p,q) <- pqs0
       , let ps = parts p
             qs = parts q
       , length ps > 1
       , length qs > 1
       , length ps /= length qs
       ]
     
     writeFile "problematic.txt" $ unlines $ concat $
       [ [ "-- ENG --" ]
      ++ map (("* "++).unwords) ps
      ++ [ "-- GER --" ]
      ++ map (("* "++).unwords) qs
      ++ [""]
       | (p,q) <- pqs0
       , let ps = parts p
             qs = parts q
       , length ps > 1
       , length qs > 1
       , length ps /= length qs
       ]
     

     --let preDot = nub [ v | (p,q) <- pqs0, r <- [p,q], (v,".") <- r `zip` tail r ]
     --putStr $ unlines $ preDot
-}

main :: IO ()
main =
  do (pqs0,pos1,pos2) <- readCorpus "mpi6.txt"
     putStrLn ("-- read " ++ show (length pqs0) ++ " sentence pairs")

     s <- readFile "known-pairs"
     let vws0 = nub [ (v,w) | [v,"~",w] <- map words (lines s) ]
     putStrLn ("-- got " ++ show (length vws0) ++ " known word pairs")

     let pqs = onCorpus2 ( --makeWordGroups
                          removeDuplicateWords
                         )             
             . removeEmptyPhrases
             -- . removeAllBut pos1 pos2 ["N"]
             -- . splitSentences
             $ pqs0
     putStrLn ("-- after processing, " ++ show (length pqs) ++ " sentence pairs")

     print [ (p,q) | (p,q@[_]) <- pqs ]

     --vws <- findPairingsInteractive pqs
     
     vws <- findPairingsCplex (True,True) vws0 pqs pos1 pos2

     {-
     vws1 <- findPairingsCplex (True,False) vws0 pqs pos1 pos2
     vws2 <- findPairingsCplex (False,True) vws0 pqs pos1 pos2
     let vws = S.toList (S.fromList vws1 `S.union` S.fromList vws2)
     -}
     putStrLn ("-- final list has " ++ show (length vws) ++ " word pairs")
     
     loose <- findLoosePairs pqs pos1 pos2 vws
     putStrLn ("-- found " ++ show (length loose) ++ " loose pairs")
     writePairings "pairings" pqs pos1 pos2 loose vws

abbrvs = ["bzw", "no", "nr", "d", "h", "z"]


--------------------------------------------------------------------------------
-- types

type Word     = String
type Phrase   = [Word]
type Corpus   = [Phrase]
type Corpus2  = [(Phrase,Phrase)]
type POS      = Map Word [String]
type WPairing = [(Word,Word)]

--------------------------------------------------------------------------------
-- reading a parallel corpus from a file

readCorpus :: FilePath -> IO (Corpus2, POS, POS)
readCorpus file =
  do s <- readFile file
     let pqs = [ (ps, qs)
               | l <- lines s
               , let ws = words' l
                     ps = takeWhile (/="|||") ws
                     qs = drop 1 $ dropWhile (/="|||") ws
               ]
     return ( nub [ (map word (filter focus p), map word (filter focus q)) | (p,q) <- pqs ]
            , M.fromListWith union [ (word w, [pos w]) | (p,_) <- pqs, w <- p ]
            , M.fromListWith union [ (word w, [pos w]) | (_,q) <- pqs, w <- q ]
            )
 where
  words' = filter (not . null) . map (map toLower . filter isOK) . words
   where
    isOK c = isAlpha c || c `elem` "|"
 
  word = id -- reverse . drop 1 . dropWhile (/='_') . reverse
  pos  = reverse . takeWhile (/='_') . reverse
  
  focus w = True -- last w `elem` ts
   where
    ts = "NAV"
  
--------------------------------------------------------------------------------
-- functions on single corpora

removeDuplicateWords :: Corpus -> Corpus
removeDuplicateWords ps = [ nub p | p <- ps ]

makeWordGroups :: Corpus -> Corpus
makeWordGroups ps =
  [ concatMap replace p
  | p <- ps
  ]
 where
  word2pat  = M.fromListWith (++)
              [ (w,[(i,p)])
              | (p,i) <- ps `zip` [1..] 
              , w <- p
              ]

  pat2words = M.fromListWith (++)
              [ (usort (map fst ips),[(w, snd (head ips))])
              | (w,ips) <- M.toList word2pat
              ]

  word2grp  = M.fromList
              [ (w, if w == v then [cw] else [])
              | (is,wps@((_,p):_:_)) <- M.toList pat2words
              , let vs@(v:_) = [ v | v <- p, v `elem` map fst wps ]
                    cw       = "\"" ++ intercalate " " vs ++ "\""
                               ++ (if k > 1 then show k else "")
                    k        = length is
              , (w,_) <- wps
              ]

  replace w = case M.lookup w word2grp of
                Nothing  -> [w]
                Just ws' -> ws'

--------------------------------------------------------------------------------
-- functions on parallel corpora

swap :: [(a,b)] -> [(b,a)]
swap pqs = [ (q,p) | (p,q) <- pqs ]

-- lifting a function on single corpora to a parallel corpus
onCorpus2 :: (Corpus -> Corpus) -> Corpus2 -> Corpus2
onCorpus2 f pqs = f ps `zip` f qs where (ps,qs) = unzip pqs

removeSmallWords :: Corpus2 -> Corpus2
removeSmallWords pqs = [ ( filter isOK p
                         , filter isOK q
                         )
                       | (p,q) <- pqs
                       ]
 where
  isOK w = any isAlpha w

removeAllBut :: POS -> POS -> [String] -> Corpus2 -> Corpus2
removeAllBut pos1 pos2 kinds pqs =
  [ (filter (can pos1 kinds) p, filter (can pos2 kinds) q)
  | (p,q) <- pqs
  ]
 where
  can pos ks w =
    case M.lookup w pos of
      Nothing  -> True
      Just ks' -> any (`elem` ks') ks

splitSentences :: Corpus2 -> Corpus2
splitSentences pqs =
  concat
  [ split p q
  | (p,q) <- pqs
  ]
 where
  split p q
    | length ps == length qs = ps `zip` qs
    | otherwise              = [(p,q)]
   where
    ps = parts p
    qs = parts q
  
  parts :: [String] -> [[String]]
  parts = groups
        . dropFirstPeriod
        . dropLastPeriod
        . eatPeriods
        . loosenPeriods

  dropFirstPeriod :: [String] -> [String]
  dropFirstPeriod (".":ws) = dropFirstPeriod ws
  dropFirstPeriod ws       = ws

  dropLastPeriod = reverse . dropFirstPeriod . reverse

  groups :: [String] -> [[String]]
  groups [] = []
  groups ws = takeWhile (/=".") ws : groups (drop 1 (dropWhile (/=".") ws))
  
  eatPeriods :: [String] -> [String]
  eatPeriods (w:".":ws) | w `elem` abbrvs = w : eatPeriods ws
  eatPeriods (w:ws)                       = w : eatPeriods ws
  eatPeriods []                           = []
  
  loosenPeriods :: [String] -> [String]
  loosenPeriods (w:ws)
    | last w == '.' = init w : "." : loosenPeriods ws
    | otherwise     = w : loosenPeriods ws
  loosenPeriods []  = []

removeEmptyPhrases :: Corpus2 -> Corpus2
removeEmptyPhrases pqs = [ pq | pq@(p,q) <- pqs, not (null p || null q) ]

writeCplex :: FilePath -> (Bool,Bool) -> [(Word,Word)] -> Corpus2 -> POS -> POS
           -> IO (Map (Word,Word) String)
writeCplex file (ltr,rtl) vws0 pqs pos1 pos2 =
  do putStrLn ("-- creating " ++ show (M.size tab) ++ " possible pairs")
     putStrLn ("-- writing " ++ file ++ " ...")
     writeFile file $ unlines $
       [ "Minimize"
       , "obj: " ++ intercalate " + "
         [ (if sim then "" else "1.001 ") ++ x
         | ((v,w),x) <- M.toList tab
         , let sim = case (M.lookup v pos1, M.lookup w pos2) of
                       (Just ps, Just qs) -> not (null (ps `intersect` qs))
                       _                  -> True
         ]
       , "Subject To"
       ] ++
       [ x ++ " = 1"
       | (v,w) <- vws0
       , Just x <- [M.lookup (v,w) tab]
       ] ++
       [ "c" ++ show i ++ ": " ++ intercalate " + " c ++ " >= 1"
       | (c,i) <- cs `zip` [1..]
       ] ++
       [ "Bounds" ] ++
       [ "0 <= " ++ v ++ " <= 1"
       | v <- vs
       ] ++
       [ "Generals" ] ++
       vs ++
       [ "End" ]
     return tab
 where
  tab = M.fromList (vws `zip` vs)
 
  cs = concat
       [ [ [ x | w <- q, Just x <- [M.lookup (v,w) tab] ] | ltr, v <- p ]
      ++ [ [ x | v <- p, Just x <- [M.lookup (v,w) tab] ] | rtl, w <- q ]
       | (p,q) <- pqs
       ]
 
  vs = take (length vws) [ "p" ++ show i | i <- [1..] ]

  vws = nub
        [ (v,w)
        | (p,q) <- pqs
        , let n   = length p
              m   = length q
              lim = win * n * m `div` min n m
        , (v,i) <- p `zip` [0..]
        , (w,j) <- q `zip` [0..]
        -- , abs (i*m - j*n) <= lim
        ]
  
  win = 99999

{-
  vws = nub
        [ (v,w)
        | (p,q) <- pqs
        , v <- p
        --, let ksv = kinds pos1 v
        , w <- q
        --, let ksw = kinds pos2 w
        --, any (`elem` ksv) ksw
        ]
-}

findPairingsCplex :: (Bool,Bool) -> [(Word,Word)] -> Corpus2 -> POS -> POS -> IO [(Word,Word)]
findPairingsCplex both vws0 pqs pos1 pos2 =
  do tab <- writeCplex "lang.lp" both vws0 pqs pos1 pos2
     putStrLn "-- solving ..."
     system "./cplex-solve-ssh lang.lp > cplex.out"
     s <- readFile "cplex.out"
     let xs   = words s
         tab' = M.fromList [ (x,vw) | (vw,x) <- M.toList tab ]
         vws  = [ vw | x <- xs, Just vw <- [M.lookup x tab'] ]
     putStrLn ("-- solution found! " ++ show (length vws) ++ " pairs")
     return vws
     
writePairings :: FilePath -> Corpus2 -> POS -> POS -> [((Word,Word),[(Word,Word)])] -> [(Word,Word)] -> IO ()
writePairings file pqs pos1 pos2 loose vws0 =
  do putStrLn ("-- writing pairings ...")
     writeFile file $ unlines $
       [ v ++ " ~ " ++ w ++ " (" ++ show r ++ ", " ++ pos pos1 v ++ " ~ " ++ pos pos2 w ++ ")"
       | (r,(v,w)) <- vws
       ]
     putStrLn ("-- writing pairings.html ...")
     writeFile (file ++ ".html") $ unlines $
       [ "<HTML>" ] ++
       [ "<HR><B>DICTIONARY</B><P>"
       , "<UL>"
       ] ++
       [ "<LI>" ++ intercalate "<BR>"
         [ "<B>" ++ sh v ++ "</B> = "
           ++ intercalate " + " [ sh' w | w <- ws ]
           ++ " {" ++ show k ++ "}"
         | (k,ws) <- freq [ usort [ (w,i+1) | w <- q, Just i <- [index (v,w) (map snd vws)] ]
                          | (p,q) <- pqs
                          , v `elem` p
                          ]
         , not (null ws)
         , let sh' (w,i) = "<a href=#wpr" ++ show i ++ ">" ++ sh w ++ "</a>"
         ]
       | let vs = nub [ v | (v,w) <- vws0 \\ map fst loose ]
       , v <- vs
       ] ++
       [ "</UL>"
       , "<HR><B>WORD PAIRINGS</B><P>"
       , "<UL>"
       ] ++
       concat
       [ [ "<LI><A NAME=wpr" ++ show j ++ ">" ++ sh v ++ " ~ " ++ sh w ++ "</A> (" ++ proc r ++ ", " ++ pos pos1 v ++ " ~ " ++ pos pos2 w ++ ")"
         , "<UL>"
         ] ++
         [ "<LI><A HREF=#phr" ++ show i ++ ">" ++ sh (unwords p)
                              ++ "<BR>" ++ sh (unwords q) ++ "</A>"
         | ((p,q),i) <- pqs `zip` [1..]
         , v `elem` p
         , w `elem` q
         ] ++
         [ "</UL>"
         ]
       | ((r,(v,w)),j) <- vws `zip` [1..]
       ] ++
       [ "<HR><B>PHRASES</B><P>"
       , "<UL>"
       ] ++
       concat
       [ [ "<LI><A NAME=phr" ++ show i ++ ">" ++ sh (unwords p)
                             ++ "<BR>" ++ sh (unwords q) ++ "</A>"
         , "<UL>"
         ] ++
         [ "<LI><A HREF=#wpr" ++ show j ++ ">" ++ sh v ++ " ~ " ++ sh w ++ "</A>"
         | ((_,(v,w)),j) <- vws `zip` [1..]
         , v `elem` p
         , w `elem` q
         ] ++
         [ "</UL>"
         ]
       | ((p,q),i) <- pqs `zip` [1..]
       ] ++
       [ "</UL>"
       , "</HTML>"
       ]
 where
  pos mp v = case M.lookup v mp of
               Just ps -> show ps
               Nothing -> "?"
 
  vws = reverse $ sort [ (good vw, vw) | vw <- vws0 \\ map fst loose ]
 
  good (v,w) = (fromIntegral avw * fromIntegral avw) / (fromIntegral av * fromIntegral aw)
   where
    av  = length [ p | (p,q) <- pqs, v `elem` p ]
    avw = length [ p | (p,q) <- pqs, v `elem` p, w `elem` q ]
    aw  = length [ q | (p,q) <- pqs, w `elem` q ]

  sh ('å':s) = "&aring;" ++ sh s
  sh ('ä':s) = "&auml;"  ++ sh s
  sh ('ö':s) = "&ouml;"  ++ sh s
  sh ('ü':s) = "&uuml;"  ++ sh s
  sh ('Ü':s) = "&Uuml;"  ++ sh s
  sh ('à':s) = "&agrave;"  ++ sh s
  sh ('á':s) = "&aacute;"  ++ sh s
  sh ('Å':s) = "&Aring;" ++ sh s
  sh ('Ä':s) = "&Auml;"  ++ sh s
  sh ('Ö':s) = "&Ouml;"  ++ sh s
  sh ('ß':s) = "&szlig;"  ++ sh s
  sh ('—':s) = "-"  ++ sh s
  sh ('"':s@(_:_)) | all isDigit s = "\"<sup>" ++ s ++ "</sup>"
  sh (c:s)   = c : sh s
  sh ""      = "" 

  proc r = (if length s == 1 then "0" else init s) ++ "." ++ [last s] ++ "%"
   where
    s = show (floor (r*1000))

findLoosePairs :: Corpus2 -> POS -> POS -> [(Word,Word)] -> IO [((Word,Word),[(Word,Word)])]
findLoosePairs pqs pos1 pos2 vws =
  return loose
 where
  loose =
    [ ((v,w),[(v,w) | w <- ws'])
    | (v,w) <- vws
    , let pqs' = [ (p,q) | (p,q) <- pqs, v `elem` p, w `elem` q ]
    , and [ or [ (v',w) `elem` vws | v' <- p, v' /= v ]
          | (p,q) <- pqs'
          ]
    , let ws' = foldr1 intersect [ q | (_,q) <- pqs' ] \\ [w]
    , not (null ws')
    ] ++
    [ ((v,w),[(v,w) | v <- vs'])
    | (v,w) <- vws
    , let pqs' = [ (p,q) | (p,q) <- pqs, v `elem` p, w `elem` q ]
    , and [ or [ (v,w') `elem` vws | w' <- q, w' /= w ]
          | (p,q) <- pqs'
          ]
    , let vs' = foldr1 intersect [ p | (p,_) <- pqs' ] \\ [v]
    , not (null vs')
    ]

findPairingsInteractive :: Corpus2 -> IO [(Word,Word)]
findPairingsInteractive pqs =
  do writeFile "interactive" ""
     hSetBuffering stdin NoBuffering
     loop [] cs vws
 where
  vws = nub
        [ (v,w)
        | (p,q) <- pqs
        , v <- p
        , w <- q
        ]

  cs = concat
       [ [ [ (v,w) | w <- q ] | v <- p ] ++
         [ [ (v,w) | v <- p ] | w <- q ]
       | (p,q) <- pqs
       ]

  loop vws1 cs [] =
    do putStrLn "Done (no pairs left)."
       return vws1
   
  loop vws1 cs vws =
    do appendFile "interactive" $ unlines
         [ v ++ " ~ " ++ w ++ " : DERIVED"
         | (v,w) <- vws0
         ]
       sequence_
         [ putStrLn ("YES: " ++ v ++ " ~ " ++ w)
         | (v,w) <- vws0
         ]
       if null cs' then
         do putStrLn "Done (no constraints left)."
            return vws1
        else
         do b <- askYesNo ("ASK: (" ++ show (length vws) ++ ") " ++ v ++ " ~ " ++ w)
            if b then
              do 
                 loop ((v,w):vws1) [ c | c <- cs', (v,w) `notElem` c ] (vws \\ [(v,w)])
             else
              do appendFile "interactive" $ (v ++ " ~ " ++ w ++ " : NO\n")
                 loop vws1 [ c \\ [(v,w)] | c <- cs' ] (vws \\ [(v,w)])
   where
    vws0    = [ (v,w) | [(v,w)] <- cs ]
    cs'     = [ c | c@(_:_:_) <- cs ]
    (v,w):_ = freqNub [ (v,w) | c <- cs', (v,w) <- c ]

--------------------------------------------------------------------------------
-- auxiliary functions

nub :: Ord a => [a] -> [a]
nub xs = go S.empty xs
 where
  go seen [] = []
  go seen (x:xs)
    | x `S.member` seen = go seen xs
    | otherwise         = x : go (S.insert x seen) xs

freq :: Ord a => [a] -> [(Int,a)]
freq xs = reverse
        . sort
        . map (\(x,k) -> (k,x))
        . M.toList
        . M.fromListWith (+)
        $ [ (x,1) | x <- xs ]

freqNub :: Ord a => [a] -> [a]
freqNub = map snd . freq

usort :: Ord a => [a] -> [a]
usort = S.toList . S.fromList

index :: Eq a => a -> [a] -> Maybe Int
index x []                 = Nothing
index x (y:ys) | x == y    = Just 0
               | otherwise = fmap (+1) (index x ys)

askYesNo :: String -> IO Bool
askYesNo s =
  do putStr (s ++ " ? ")
     hFlush stdout
     c <- getChar
     if c `elem` "yYnN" then
       do putStrLn ""
          return (c `elem` "yY")
      else
       do putStr "\n(Y or N) "
          askYesNo s

--------------------------------------------------------------------------------


