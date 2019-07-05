
import Data.Map( Map )
import qualified Data.Map as M
import Data.Set( Set )
import qualified Data.Set as S
import Data.List hiding ( nub )
import Data.Char
import MiniSat
import MinSat
import Nat

main = main2

--------------------------------------------------------------------------------

swords :: IO ()
swords =
  do s <- readFile "swe-eng-texts.tsv"
     let ws = nub (concatMap (words . takeWhile (/='|')) (lines s))
     sequence_
       [ putStrLn w
       | w <- ws
       , length w <= 5
       ]

inflections :: IO ()
inflections =
  do s <- readFile "swe-eng-texts.tsv"
     let ws = nub (concatMap (words . dropWhile (/='|')) (lines s))
         mp = M.fromListWith (++) [ (stem_en w, [w]) | w <- ws ]
         gs = [ ws | (_, ws@(_:_:_)) <- M.toList mp ]
     sequence_
       [ do sequence_ [ putStrLn w | w <- g ]
            putStrLn ""
       | g <- gs
       ]

stem_se :: String -> String
stem_se s = snd $ minimum $ [ (length t, t)
                            | end <- endings_se
                            , end `isSuffixOf` s
                            , let t = take (length s - length end) s
                            ] ++ [ (length s, s) ]

endings_se :: [String]
endings_se = [ "er", "ar", "arna", "or", "orna", "a", "r", "s", "ds", "ts", "d", "t", "en", "ens", "et", "ets" ]

stem_en :: String -> String
stem_en "are" = "be"
stem_en "is"  = "be"
stem_en s = snd $ minimum $ [ (length t, t)
                            | end <- endings_en
                            , end `isSuffixOf` s
                            , let t = take (length s - length end) s
                            ] ++ [ (length s, s) ]

endings_en :: [String]
endings_en = [ "s", "ed", "es" ]

--------------------------------------------------------------------------------

norm :: String -> (String -> String)
norm tab = \w -> case M.lookup w mp of
                   Just w' -> w'
                   Nothing -> w
 where
  mp = M.fromList [ (s,w++"~") | g <- gs, let w = snd (minimum [ (length w, w) | w <- g ]), s <- g ]

  gs = takeWhile (not . null)
     . map (takeWhile (not . null))
     . iterate (drop 1 . dropWhile (not . null))
     . lines
     $ tab

main2 :: IO ()
main2 =
  do infl_se <- readFile "inflections_se"
     infl_en <- readFile "inflections_en"

     let norm_se = norm infl_se
         norm_en = norm infl_en

     s <- readFile "swe-eng-texts.tsv"
     let pqs = --take 500
               [ (ps,qs)
               | l <- lines s
               , let ok ('[':_) = False
                     ok w       = not (null w)

                     isLetter c = isAlphaNum c || c == '\'' || c == '[' || c == '-' || c == '/'

                     ps = map norm_se $ filter ok $ map (filter isLetter) $ words $ takeWhile (/= '|') l
                     qs = map norm_en $ filter ok $ map (filter isLetter) $ words $ dropWhile (/= '|') l
               , not (null ps)
               ]
     print (length pqs)

     findCoverings pqs

     {-
     abs <- readPhrases "judge-these.txt"
     let jabs = map (judge vws) abs

         foundBad    = [ ab | Just ab <- jabs ]
         notFoundBad = [ ab | (Nothing,ab) <- jabs `zip` abs ]
         foundGood   = [ pq | Just pq <- map (judge vws) pqs ]

     putStrLn "=== Flagged Bad Phrases ==="
     sequence_
       [ putStrLn (unwords a ++ "\n" ++ unwords b ++ "\n")
       | (a,b) <- foundBad
       ]

     if not (null notFoundBad) then
       do putStrLn "*** Not-Flagged Bad Phrases (false positives) ***"
          sequence_
            [ putStrLn (unwords a ++ "\n" ++ unwords b ++ "\n")
            | (a,b) <- notFoundBad
            ]
      else
       do return ()

     if not (null foundGood) then
       do putStrLn "*** Flagged Good Phrases (false negatives) ***"
          sequence_
            [ putStrLn (unwords a ++ "\n" ++ unwords b ++ "\n")
            | (a,b) <- foundGood
            ]
      else
       do return ()
     -}

--------------------------------------------------------------------------------

type Word = String
type Phrase = [Word]

readPhrases :: FilePath -> IO [(Phrase,Phrase)]
readPhrases file =
  do s <- readFile file
     let ls = lines s
     return (parse ls)
 where
  parse (en:nl:ls) =
    (words en, words nl) :
      case ls of
        "":ls' -> parse ls'
        _      -> error ("no empty line: " ++ show (take 1 ls))

  parse ls | all null ls =
    []

  parse ls =
    error ("loose phrase: " ++ show ls)

--------------------------------------------------------------------------------

findCoverings :: [(Phrase,Phrase)] -> IO ()
findCoverings pqs =
  do putStrLn "=== Finding Trivial Coverings ==="
     printPairs vs pairs0
     findCoverings1 [ (rem vs p, rem ws q) | (p,q) <- pqs ]
 where
  pairs = nub [ (v,w)
              | (p,q) <- pqs
              , v <- nub p
              , w <- nub q
              ]

  pairs0 = [ (v,w)
           | (v,w) <- pairs
           , all (\(p,q) -> (v `elem` p) == (w `elem` q)) pqs
           ]

  vs = nub (map fst pairs0)
  ws = nub (map snd pairs0)

  rem xs ws
    | null ws'  = ["*"]
    | otherwise = ws'
   where
    ws' = filter (`notElem` xs) ws

  printPairs [] _ =
    do return ()

  printPairs (v:vs) vws =
    do putStrLn ( "{" ++ concat (intersperse "," as)
               ++ "} <=> {"
               ++ concat (intersperse "," bs) ++ "}")
       printPairs (vs \\ as) vws
   where
    (as,bs) = follow [v] [] [] vws

    follow []     as bs vws = (as,bs)
    follow (v:vs) as bs vws
      | v `elem` as = follow vs as bs vws
      | otherwise   = follow (vs ++ [ v | (v,w') <- vws, w' `elem` ws]) (v:as) (bs`union`ws) vws
      where
       ws = [ w | (v',w) <- vws, v' == v ]

findCoverings1 :: [(Phrase,Phrase)] -> IO ()
findCoverings1 pqs =
  do putStrLn "=== Finding Gready Covering ==="
     sequence_ [ putStrLn (v ++ " <=> " ++ w) | (v,w) <- vws ]
     putStrLn (show (length vws) ++ " pairings")
 where
  cls = concat
        [ [ [ (v,w) | w <- ws ] | v <- vs ] ++
          [ [ (v,w) | v <- vs ] | w <- ws ]
        | (p,q) <- pqs
        , let vs = nub p
              ws = nub q
        ]

  vws = mini cls

findCoverings2 :: [(Phrase,Phrase)] -> IO ()
findCoverings2 pqs =
  do putStrLn "=== Finding Non-Trivial Coverings ==="
     sat <- newSolver

     putStrLn "creating pairs..."
     tab <- newLits sat [ (v,w)
                        | (p,q) <- pqs
                        , v <- nub p
                        , w <- nub q
                        ]
     putStrLn ("found " ++ show (M.size tab) ++ " pairs.")

     putStrLn ("adding constraints...")
     sequence_
       [ addClause sat
           [ l
           | w <- nub q
           , Just l <- [M.lookup (v,w) tab]
           ]
       | (p,q) <- pqs
       , v <- nub p
       ]
     sequence_
       [ addClause sat
           [ l
           | v <- nub p
           , Just l <- [M.lookup (v,w) tab]
           ]
       | (p,q) <- pqs
       , w <- nub q
       ]

     let h = do vwss <- sequence
                  [ do b <- modelValue sat l
                       if b == Just True
                         then return [(v,w)]
                         else return []
                  | ((v,w),l) <- M.toList tab
                  ]
                sequence_
                  [ putStrLn (v ++ " <=> " ++ w)
                  | (v,w) <- concat vwss
                  ]

     putStrLn ("estimate minimizing coverings...")
     b <- solveMinimizeLocal sat [] h (M.elems tab)

     putStrLn ("actual minimizing coverings...")
     num <- count sat (M.elems tab)
     b <- solveMinimize sat [] h num

     deleteSolver sat
     return ()

solveMinimizeLocal sat as h xs =
  do putStrLn "-> computing literals from 0..."
     let opti0 xs =
           do putStrLn (show (length xs) ++ " literals...")
              b <- solve sat (as ++ map neg xs)
              if b then return xs
                   else do c <- conflict sat
                           opti0 [ x | x <- xs, x `notElem` c ]

     a <- newLit sat
     --xs' <- opti0 xs
     let xs' = xs
     putStrLn ("setting " ++ show (length xs - length xs') ++ " literals to 0.")
     sequence_ [ addClause sat [neg a, neg x] | x <- xs, x `notElem` xs' ]
     putStrLn (show (length xs') ++ " literals left.")

     putStrLn "-> computing literals from 1..."
     let opti1 xs =
           do putStrLn (show (length xs) ++ " literals...")
              addClause sat (neg a : [ neg x | x <- xs ])
              b <- solve sat (a:as)
              if b then do bs <- sequence [ modelValue sat x | x <- xs ]
                           sequence_ [ addClause sat [neg a, neg x]
                                     | (b,x) <- bs `zip` xs
                                     , b /= Just True
                                     ]
                           opti1 [ x | (Just True,x) <- bs `zip` xs ]
                   else return xs

     xs'' <- opti1 xs'
     putStrLn (show (length xs'') ++ " literals left.")


newLits :: Ord a => Solver -> [a] -> IO (Map a Lit)
newLits sat [] =
  do return M.empty

newLits sat (x:xs) =
  do mp <- newLits sat xs
     case M.lookup x mp of
       Just _  -> do return mp
       Nothing -> do l <- newLit sat
                     return (M.insert x l mp)

--------------------------------------------------------------------------------

judge :: Set (Word,Word) -> (Phrase,Phrase) -> Maybe (Phrase,Phrase)
judge vws (a,b)
  | null vs && null ws = Nothing
  | otherwise          = Just (mark vs a, mark ws b)
 where
  vs = [ v | v <- a, all (\w -> not ((v,w) `S.member` vws)) b ]
  ws = [ w | w <- b, all (\v -> not ((v,w) `S.member` vws)) a ]

  mark vs a = map mrk a
   where
    mrk v | v `elem` vs = "\ESC[7m" ++ v ++ "\ESC[m"
          | otherwise   = v

--------------------------------------------------------------------------------

nub xs = map head . group . sort $ xs

