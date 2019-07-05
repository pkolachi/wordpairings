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

--------------------------------------------------------------------------------
-- main

main :: IO ()
main =
  do (pqs0,pos1,pos2) <- readCorpus "corpus2.txt"
     let pqs    = onCorpus2 ( makeWordGroups
                            . removeDuplicateWords
                            )
                . removeEmptyPhrases
                $ pqs0
         
         pairs0 = [] -- findTrivialPairings pqs

     sequence_
       [ print (show p, show q)
       | (p,q) <- pqs
       , null p || null q
       ]

     vws <- findPairingsCplex pqs pairs0
     --vws' <- minimizeCrossings pqs vws
     writePairings "pairings" pqs vws

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
               , let ws = words l
                     ps = takeWhile (/="|||") ws
                     qs = drop 1 $ dropWhile (/="|||") ws
               ]
     return ( [ (map word p, map word q) | (p,q) <- pqs ]
            , M.fromListWith union [ (word w, [pos w]) | (p,_) <- pqs, w <- p ]
            , M.fromListWith union [ (word w, [pos w]) | (p,_) <- pqs, w <- p ]
            )
 where
  word = reverse . drop 1 . dropWhile (/='_') . reverse
  pos  = reverse . takeWhile (/='_') . reverse
  
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
                    cw       = "{" ++ intercalate "," vs ++ "}"
                               ++ (if k > 1 then show k else "")
                    k        = length is
              , (w,_) <- wps
              ]

  replace w = case M.lookup w word2grp of
                Nothing  -> [w]
                Just ws' -> ws'

--------------------------------------------------------------------------------
-- functions on parallel corpora

-- lifting a function on single corpora to a parallel corpus
onCorpus2 :: (Corpus -> Corpus) -> Corpus2 -> Corpus2
onCorpus2 f pqs = f ps `zip` f qs where (ps,qs) = unzip pqs

removeEmptyPhrases :: Corpus2 -> Corpus2
removeEmptyPhrases pqs = [ pq | pq@(p,q) <- pqs, not (null p || null q) ]

findTrivialPairings :: Corpus2 -> WPairing
findTrivialPairings pqs =
  [ (v,w)
  | (vs,ws) <- M.elems $ M.intersectionWith (,)
               (pat2words (word2pat ps))
               (pat2words (word2pat qs))
  , v <- vs
  , w <- ws
  ]
 where
  (ps,qs) = unzip pqs
  
  word2pat rs  = M.map usort $ M.fromListWith (++)
                 [ (w,[i])
                 | (r,i) <- rs `zip` [1..] 
                 , w <- r
                 ]

  pat2words mp = M.fromListWith (++)
                 [ (is,[w])
                 | (w,is) <- M.toList mp
                 ]

{-
findPairingsGreedy :: Corpus2 -> WPairing -> WPairing
findPairingsGreedy pqs pairs0 = pairs0 ++ greedy vws (constraints pqs)
 where
  vws = nub
        [ (v,w)
        | (p,q) <- pqs
        , v <- p
        , w <- q
        , (v,w) `notElem` pairs0
        ]
  
  constraints pqs =
    concat
    [ [ [ (v,w) | w <- q ] | v <- p ] ++
      [ [ (v,w) | v <- p ] | w <- q ]
    | (p,q) <- pqs
    ]
-}

writeCplex :: FilePath -> Corpus2 -> WPairing -> IO (Map (Word,Word) String)
writeCplex file pqs pairs0 =
  do putStrLn ("-- have " ++ show (length pairs0) ++ " trivial pairs")
     putStrLn ("-- creating " ++ show (M.size tab - length pairs0) ++ " new possible pairs")
     putStrLn ("-- writing " ++ file ++ " ...")
     writeFile file $ unlines $
       [ "Minimize"
       , "obj: " ++ intercalate " + " (vs)
          -- ++ [ "0.1 " ++ x | (xs,_) <- crs, x <- xs, all (not . isUpper) x ])
       , "Subject To"
       ] ++
       [ "c" ++ show i ++ ": " ++ intercalate " + " c ++ " >= 1"
       | (c,i) <- cs `zip` [1..]
       ] ++ {-
       [ "x" ++ show i ++ ": " ++ c
       | (c,i) <- concat [ cs | (_,cs) <- crs ] `zip` [1..]
       ] ++ -}
       [ "Bounds" ] ++
       [ (if v `S.member` ps0 then "1" else "0") ++ " <= " ++ v ++ " <= 1"
       | v <- vs
       ] ++ {-
       [ "0 <= " ++ x ++ " <= 1"
       | (xs,_) <- crs
       , x <- xs
       ] ++ -}
       [ "Generals" ] ++
       vs ++
       [ "End" ]
     return tab
 where
  tab = M.fromList (vws `zip` vs)
  ps0 = S.fromList [ v | p <- pairs0, Just v <- [M.lookup p tab] ]
 
  cs = concat
       [ [ [ tab!(v,w) | w <- q ] | v <- p ]
      ++ [ [ tab!(v,w) | v <- p ] | w <- q ]
       | (p,q) <- pqs
       ]
 
  vs = take (length vws) [ "p" ++ show i | i <- [1..] ]
  
  crs = [ crosses i (p,q) | ((p,q),i) <- pqs `zip` [1..] ]
  
  crosses i (p,q) =
    ( concat
      [ [ "x" ++ show i ++ "_" ++ show j
        , "xLR" ++ show i ++ "_" ++ show j
        , "xRL" ++ show i ++ "_" ++ show j
        ]
      | j <- [1..k]
      ]
    , concat
      [ [ "xLR" ++ show i ++ "_" ++ show j ++ " - " ++ tab!(v,w) ++ " >= 0"
        | v <- take j p
        , w <- drop j q
        ]
     ++ [ "xRL" ++ show i ++ "_" ++ show j ++ " - " ++ tab!(v,w) ++ " >= 0"
        | v <- drop j p
        , w <- take j q
        ]
     ++ [ "x" ++ show i ++ "_" ++ show j
       ++ " - xLR" ++ show i ++ "_" ++ show j
       ++ " - xRL" ++ show i ++ "_" ++ show j
       ++ " >= -1"
        ]
      | j <- [1..k]
      ]
    )
   where
    k = (length p `min` length q)-1
  
  vws = nub
        [ (v,w)
        | (p,q) <- pqs
        , v <- p
        , w <- q
        ]

findPairingsCplex :: Corpus2 -> WPairing -> IO [(Word,Word)]
findPairingsCplex pqs pairs0 =
  do tab <- writeCplex "lang.lp" pqs pairs0
     putStrLn "-- solving ..."
     system "./cplex-solve-ssh lang.lp > cplex.out"
     s <- readFile "cplex.out"
     let xs   = words s
         tab' = M.fromList [ (x,vw) | (vw,x) <- M.toList tab ]
         vws  = [ vw | x <- xs, Just vw <- [M.lookup x tab'] ]
     putStrLn ("-- solution found! " ++ show (length vws) ++ " pairs")
     return vws
     
writePairings :: FilePath -> Corpus2 -> [(Word,Word)] -> IO ()
writePairings file pqs vws0 =
  do putStrLn ("-- writing pairings ...")
     writeFile file $ unlines $
       [ v ++ " ~ " ++ w ++ " (" ++ show r ++ ")"
       | (r,(v,w)) <- vws
       ]
     putStrLn ("-- writing pairings.html ...")
     writeFile (file ++ ".html") $ unlines $
       [ "<HTML>"
       , "<HR><B>WORD PAIRINGS</B><P>"
       , "<UL>"
       ] ++
       concat
       [ [ "<LI><A NAME=wpr" ++ show j ++ ">" ++ sh v ++ " ~ " ++ sh w ++ "</A> (" ++ proc r ++ ")"
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
  vws = reverse $ sort [ (good vw, vw) | vw <- vws0 ]
 
  good (v,w) = (fromIntegral avw * fromIntegral avw) / (fromIntegral av * fromIntegral aw)
   where
    av  = length [ p | (p,q) <- pqs, v `elem` p ]
    avw = length [ p | (p,q) <- pqs, v `elem` p, w `elem` q ]
    aw  = length [ q | (p,q) <- pqs, w `elem` q ]

  sh ('å':s) = "&aring;" ++ sh s
  sh ('ä':s) = "&auml;"  ++ sh s
  sh ('ö':s) = "&ouml;"  ++ sh s
  sh ('à':s) = "&agrave;"  ++ sh s
  sh ('á':s) = "&aacute;"  ++ sh s
  sh ('Å':s) = "&Aring;" ++ sh s
  sh ('Ä':s) = "&Auml;"  ++ sh s
  sh ('Ö':s) = "&Ouml;"  ++ sh s
  sh ('—':s) = "-"  ++ sh s
  sh (c:s)   = c : sh s
  sh ""      = "" 

  proc r = (if s=="0" then "0" else init s) ++ "." ++ [last s] ++ "%"
   where
    s = show (floor (r*1000))

--------------------------------------------------------------------------------

minimizeCrossings :: Corpus2 -> [(Word,Word)] -> IO [(Word,Word)]
minimizeCrossings pqs vws =
  let crs = crossings vws in opt vws (length crs) (freqNub crs)
 where
  crossings vws =
    [ ((v1,w1),(v2,w2))
    | (p,q) <- pqs
    , let vws' = [ (v,w) | (v,w) <- vws, v `elem` p, w `elem` q ]
    , (v1,w1) <- vws'
    , (v2,w2) <- vws'
    , v1 /= v2
    , w1 /= w2
    , index v1 p < index v2 p
    , index w1 q > index w2 q 
    ]
  
  index x (y:ys) | x == y    = 0
                 | otherwise = 1 + index x ys

  opt vws n [] =
    do return vws
  
  opt vws n (((v1,w1),(v2,w2)):crs)
    | ok' && n' < n =
      do putStrLn ("/ "  ++ v1 ++ " ~ " ++ w2)
         putStrLn ("\\ " ++ v2 ++ " ~ " ++ w1)
         putStrLn (show n' ++ " crossings")
         opt vws' n' (freqNub crs')
    
    | otherwise =
      do opt vws n crs
   where
    vws' = [(v1,w2),(v2,w1)] ++ (vws \\ [(v1,w1),(v2,w2)])
    ok'  = and [ and [ or [ (v,w) `elem` vws' | w <- q ] | v <- p ]
              && and [ or [ (v,w) `elem` vws' | v <- p ] | w <- q ]
               | (p,q) <- pqs
               , v1 `elem` p || v2 `elem` p
               , w1 `elem` q || w2 `elem` q
               ]
    crs' = crossings vws'
    n'   = length crs'

--------------------------------------------------------------------------------
-- auxiliary functions

nub :: Ord a => [a] -> [a]
nub xs = go S.empty xs
 where
  go seen [] = []
  go seen (x:xs)
    | x `S.member` seen = go seen xs
    | otherwise         = x : go (S.insert x seen) xs

freqNub :: Ord a => [a] -> [a]
freqNub xs = map snd
           . reverse
           . sort
           . map (\(x,k) -> (k,x))
           . M.toList
           . M.fromListWith (+)
           $ [ (x,1) | x <- xs ]

usort :: Ord a => [a] -> [a]
usort = S.toList . S.fromList

--------------------------------------------------------------------------------


