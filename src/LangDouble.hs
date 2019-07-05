
import Data.Map( Map )
import qualified Data.Map as M
import Data.List

main :: IO ()
main =
  do pqs <- readPhrases "phrases.txt"
     let prd = valuePairs pqs
     putStrLn "=== example pairs ==="
     putStr $ unlines $ map pair $ take 100 $ reverse $ sort $
       [ (d,vw)
       | (vw,d) <- M.toList prd
       ]

     abs <- readPhrases "judge-these.txt"
     putStrLn "=== judged bad phrases ==="
     putStr $ unlines $ map jdg $ sort $
       [ (judge prd ab,ab)
       | ab <- abs
       ]

     putStrLn "=== some judged good phrases ==="
     putStr $ unlines $ map jdg $ take 10 $ sort $
       [ (judge prd ab,ab)
       | ab <- pqs
       ]
 where
  pair (d,(v,w)) = showA 3 d ++ ": " ++ v ++ " <=> " ++ w
  
  showA k d = replicate (k-n) ' ' ++ s
   where
    s = show (round (100*d))
    n = length s

  jdg ((d,eww),(a,b)) =
    showA 3 d ++ ": " ++ phrsL a ++ "\n" ++
    "     " ++ phrsR b ++ "\n"
   where
    phrsL a = case eww of
                Left w -> mark w a
                _      -> unwords a
    phrsR a = case eww of
                Right w -> mark w a
                _       -> unwords a
    
    mark w a = unwords [ if v == w then "\ESC[7m" ++ v ++ "\ESC[m" else v | v <- a ] 

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

countWords :: [Phrase] -> Map Word Int
countWords ps =
  M.fromListWith (+)
  [ (w,1)
  | p <- ps
  , w <- nub p
  ]

countPairs :: [(Phrase,Phrase)] -> Map (Word,Word) Int
countPairs pqs =
  M.fromListWith (+)
  [ ((v,w),1)
  | (p,q) <- pqs
  , v <- nub p
  , w <- nub q
  ]

valuePairs :: [(Phrase,Phrase)] -> Map (Word,Word) Double
valuePairs pqs =
  M.fromList
  [ -- ((v,w),(pq*pq)//(p*q))
    ((v,w),pq//(p `max` q))
  | ((v,w),pq) <- M.toList prc
  , let p = occurs pc v
        q = occurs qc w
  ]
 where
  pc  = countWords (map fst pqs)
  qc  = countWords (map snd pqs)
  prc = countPairs pqs

  occurs mp x = M.findWithDefault 0 x mp
  n // m = fromIntegral n / fromIntegral m
 
judge :: Map (Word,Word) Double -> (Phrase,Phrase) -> (Double,Either Word Word)
judge pqd (a,b) =
  minimum $
  [ (maximum vals, eww)
  | eww <- [ Left v | v <- a ] ++ [ Right w | w <- b ]
  , let vals = case eww of
                 Left v  -> [ value (v,w) | w <- b ]
                 Right w -> [ value (v,w) | v <- a ]
  ]
 where
  value vw =
    case M.lookup vw pqd of
      Just d  -> d
      Nothing -> 1.0
  
  
