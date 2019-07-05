
import Prelude hiding ( Word )
import Data.Map( Map )
import qualified Data.Map as M
import Data.Set( Set )
import qualified Data.Set as S
import Data.List hiding ( nub )
import Data.Char
import MiniSat
import MinSat
import Nat
import Data.IORef

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

main :: IO ()
main =
  do infl_se <- readFile "inflections_se"
     infl_en <- readFile "inflections_en"

     let norm_se = norm infl_se
         norm_en = norm infl_en

     comm_se <- readFile "common_se"
     comm_en <- readFile "common_en"

     let filt_se = (`notElem` words comm_se)
         filt_en = (`notElem` words comm_en)

     s <- readFile "swe-eng-texts.tsv"
     let pqs = --take 500
               [ (ps,qs)
               | l <- lines s
               , let ok ('[':_) = False
                     ok w       = not (null w)

                     isLetter c = isAlphaNum c || c == '\'' || c == '[' || c == '-' || c == '/'

                     ps = filter filt_se $ map norm_se $ filter ok $ map (filter isLetter) $ words $ takeWhile (/= '|') l
                     qs = filter filt_en $ map norm_en $ filter ok $ map (filter isLetter) $ words $ dropWhile (/= '|') l
               , not (null ps)
               , not (null qs)
               ]

     let pqs' = groupWords pqs
     let ws = nub [ w | (p,_) <- pqs', w <- p ]
     putStrLn (show (length pqs') ++ " sentences")
     putStrLn (show (length ws) ++ " swedish word-groups")

     writeFile "prasanth" $ unlines $
       [ unwords p ++ " | " ++ unwords q
       | (p,q) <- pqs'
       ]

     let pair [v,"-",w] = (v,w)
     gold <- (map (pair . words) . lines) `fmap` readFile "gold"

     let high ws v | v `elem` ws = "[" ++ v ++ "]"
                   | otherwise   = v

     sequence_
       [ do putStrLn ""
            putStrLn (unwords (map (high vs) p))
            putStrLn (unwords (map (high ws) q))
       | (p,q) <- pqs'
       , let vs = [ v | v <- p, all (\w -> (v,w) `notElem` gold) q ]
             ws = [ w | w <- q, all (\v -> (v,w) `notElem` gold) p ]
       , not (null (vs ++ ws))
       ]

     --findCoverings pqs'

--------------------------------------------------------------------------------

groupWords :: [(Phrase,Phrase)] -> [(Phrase,Phrase)]
groupWords pqs = [ (grp gps p, grp gqs q) | (p,q) <- pqs ]
 where
  gps = groups (map fst pqs)
  gqs = groups (map snd pqs)

  grp []           p = p
  grp (g@(w:_):gs) p
    | w `elem` p     = concat (intersperse "+" g) : grp gs (p \\ g)
    | otherwise      = grp gs p

groups :: [Phrase] -> [[Word]]
groups ps = map collapse (grps abs)
 where
  abs0 = nub [ (a,b) | p <- ps, let s = nub p, (a,b) <- pairs s ]
  abs  = [ ab
         | ab@(a,b) <- abs0
         , all (\p -> (a `elem` p) == (b `elem` p)) ps
         ]

  pairs (x:xs) = [ (x,y) | y <- xs ] ++ pairs xs
  pairs []     = []

  grps :: [(Word,Word)] -> [[(Word,Word)]]
  grps []          = []
  grps ((a,b):abs) = add (a,b) (grps abs)
   where
    add (a,b) []     = [[(a,b)]]
    add (a,b) (g:gs)
      | a `inn` g || b `inn` g = ((a,b):g):gs
      | otherwise              = g : add (a,b) gs

  x `inn` g = x `elem` (map fst g ++ map snd g)

  collapse :: [(Word,Word)] -> [Word]
  collapse xys = trail xs xys
   where
    xs = nub [ z | (x,y) <- xys, z <- [x,y] ]

    trail [] _   = []
    trail xs xys =
      case [ x | x <- xs, all ((/= x) . snd) xys ] of
        x:_ -> pick x
        []  -> pick (head xs)
     where
      pick x = x : trail (xs \\ [x]) [ (a,b) | (a,b) <- xys, a /= x, b /= x ]

--------------------------------------------------------------------------------

type Word = String
type Phrase = [Word]

findCoverings :: [(Phrase,Phrase)] -> IO ()
findCoverings pqs =
  do putStrLn "=== Finding Trivial Coverings ==="
     sequence_ [ putStrLn (v ++ " <=> " ++ w) | (v,w) <- pairs0 ]
     findCoverings2 pairs0 pqs
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

  rem vs p
    | null p'   = ["*"]
    | otherwise = p'
   where
    p' = p \\ vs

findCoverings1 :: [(Phrase,Phrase)] -> IO ()
findCoverings1 pqs =
  do putStrLn "=== Finding Gready Covering ==="
     sequence_ [ putStrLn (v ++ " <=> " ++ w) | (v,w) <- vws ]
     putStrLn (show (length vws) ++ " pairings")
     --findCoverings2 pqs
 where
  cls = concat
        [ [ [ (v,w) | w <- ws ] | v <- vs ] ++
          [ [ (v,w) | v <- vs ] | w <- ws ]
        | (p,q) <- pqs
        , let vs = nub p
              ws = nub q
        ]

  vws = mini cls

findCoverings2 :: [(Word,Word)] -> [(Phrase,Phrase)] -> IO ()
findCoverings2 pairs0 pqs =
  do putStrLn "=== Finding Structure ==="
     findStructure pairs0 pqs

     putStrLn "=== Finding Non-Trivial Coverings ==="
     sat <- newSolver

     putStrLn "creating pairs..."
     tab1 <- newLits sat [ (v,w)
                         | (p,q) <- pqs
                         , v <- nub p
                         , w <- nub q
                         , (v,w) `notElem` pairs0
                         ]
     putStrLn ("found " ++ show (M.size tab1) ++ " new pairs.")
     true <- newLit sat
     addClause sat [true]
     let tab0 = M.fromList [ (vw,true) | vw <- pairs0 ]
         tab  = tab0 `M.union` tab1

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

     ref <- newIORef 10000000
     let h =
           do vws <- concat `fmap` sequence
                [ do b <- modelValue sat l
                     if b == Just True
                       then return [(v,w)]
                       else return []
                | ((v,w),l) <- M.toList tab
                ]
              n <- readIORef ref
              let n' = length vws
              if n' < n then
                do writeIORef ref n'
                   putStrLn (show n' ++ " pairings")
                   writeFile "pairings-raw" $ unlines $
                     [ v ++ " - " ++ w
                     | (v,w) <- vws
                     ]
                   writeFile "pairings" $ unlines $
                     [ "# " ++ show n' ++ " pairings" ] ++
                     [ ""
                     , "### one-to-one pairings ###" ] ++
                     [ v ++ " <=> " ++ w
                     | (v,w) <- sort (oneToOne vws)
                     ] ++
                     [ ""
                     , "### one-to-many pairings ###" ] ++
                     [ v ++ " => " ++ concat (intersperse ", " ws)
                     | let (a,_) `first` (b,_) = a == b
                     , vvws <- groupBy first $ sort (manyToMany vws)
                     , let v  = fst $ head $ vvws
                           ws = map snd vvws
                     ] ++
                     [ ""
                     , "### many-to-one pairings ###" ] ++
                     [ w ++ " <= " ++ concat (intersperse ", " vs)
                     | let (_,a) `second` (_,b) = a == b
                     , vwws <- groupBy second $ sortBy (\x y -> snd x `compare` snd y) (manyToMany vws)
                     , let w  = snd $ head $ vwws
                           vs = map fst vwws
                     ]
                   writeFile "pairings.gv" $ unlines $
                     [ "digraph G {"
                     , "  #overlap = false;"
                     ] ++
                     let vws' = manyToMany vws
                         plus = concatMap (\c -> if c == '+' then " " else [c])
                      in
                     [ "  \"" ++ plus v ++ "\" -> \"" ++ plus w ++ "\";"
                     | (v,w) <- vws'
                     ] ++
                     [ "}" ]
               else
                do return ()

     putStrLn ("estimate minimizing coverings...")
     solveMinimizeLocal sat [] h (M.elems tab)

     --putStrLn ("actual minimizing coverings...")
     --num <- count sat (M.elems tab)
     --b <- solveMinimize sat [] h num

     deleteSolver sat
     return ()

oneToOne :: Ord a => [(a,a)] -> [(a,a)]
oneToOne xys = [ (x,y)
               | (x,y) <- xys
               , x `notElem` (xs \\ [x])
               , y `notElem` (ys \\ [y])
               ]
 where
  xs = [ x | (x,_) <- xys ]
  ys = [ y | (_,y) <- xys ]

manyToMany :: Ord a => [(a,a)] -> [(a,a)]
manyToMany xys = [ (x,y)
                 | (x,y) <- xys
                 , x `elem` (xs \\ [x])
                   || y `elem` (ys \\ [y])
                 ]
 where
  xs = [ x | (x,_) <- xys ]
  ys = [ y | (_,y) <- xys ]

findStructure :: [(Word,Word)] -> [(Phrase,Phrase)] -> IO ()
findStructure pairs0 pqs =
  writeFile "structure.gv" $ unlines $
    [ "digraph G {"
    , "  #overlap = false;"
    ] ++
    [ "  \"" ++ plus v ++ "\" -> \"" ++ plus w ++ "\";"
    | ((v,_),(w,_)) <- graph
    ] ++
    [ "}" ]
 where
  ps = map fst pqs
  qs = map snd pqs

  plus = concatMap (\c -> if c == '+' then " " else [c])

  graph = [ ((v,1),(w,1)) | (v,w) <- struct ps ]
      -- ++ [ ((v,2),(w,2)) | (v,w) <- struct qs ]
      -- ++ [ ((v,1),(w,2)) | (v,w) <- crosss pqs, (v,w) `notElem` pairs0 ]
      -- ++ [ ((v,2),(w,1)) | (v,w) <- crosss (map swap pqs), (w,v) `notElem` pairs0 ]

  swap (x,y) = (y,x)

  struct ps =
    [ (v,w)
    | (v,w) <- vws
    , all (\p -> v `notElem` p || w `elem` p) ps
    ]
   where
    vws = nub [ (v,w) | p <- ps, let vs = nub p, v <- vs, w <- vs, v /= w ]

  crosss pqs =
    [ (v,w)
    | (v,w) <- vws
    , all (\(p,q) -> v `notElem` p || w `elem` q) pqs
    ]
   where
    vws = nub [ (v,w) | (p,q) <- pqs, v <- nub p, w <- nub q ]

solveMinimizeLocal sat as h xs =
  do let local a xs =
           do addClause sat (neg a : [ neg x | x <- xs ])
              b <- solve sat (a:as)
              if b then do h
                           bs <- sequence [ modelValue sat x | x <- xs ]
                           sequence_ [ addClause sat [neg a, neg x]
                                     | (b,x) <- bs `zip` xs
                                     , b /= Just True
                                     ]
                           local a [ x | (Just True,x) <- bs `zip` xs ]
                   else do return xs

         global xs =
           do putStrLn "-- global round --"
              a <- newLit sat
              ys <- local a xs
              putStrLn (show (length ys))
              addClause sat [ neg y | y <- ys ]
              pullup [ x | x <- xs, x `notElem` ys ]
              global xs

         pullup xs =
           do b <- solve sat xs
              if b then return ()
                   else do c <- conflict sat
                           pullup [ x | x <- xs, neg x `notElem` c ]

     global xs

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

nub xs = go S.empty xs
 where
  go seen [] = []
  go seen (x:xs)
    | x `S.member` seen = go seen xs
    | otherwise         = x : go (S.insert x seen) xs

