
import Data.List hiding ( nub )
import qualified Data.Set as S

(%%) :: Int -> Int -> String
a %% t = show (p `div` 10) ++ "." ++ show (p `mod` 10) ++ "%"
 where
  p = (a * 1000) `div` t

(<%%) :: Eq a => [a] -> [a] -> String
xs <%% ys = length [ x | x <- xs, x `elem` ys ] %% length xs

main =
  do s1 <- readFile "pairings-raw"
     s2 <- readFile "swe-eng.ibmmodel.pairings"
     s3 <- readFile "gold"
     
     let vws1 = [ (v,w)
                | l <- lines s1
                , [v,"-",w] <- [words l]
                ]

         n    = length vws1
         
         vws2 = [ (v,w,p)
                | l <- lines s2
                , [v,w,sp] <- [words l]
                , let p = read sp :: Double
                ]
     
         vws2_ = [ (v,w)
                 | (v,w,_) <- vws2
                 ]
     
         vws2' = nub $
                 [ (v,w)
                 | (v,w,p) <- vws2
                 , p >= r
                 ] {- ++
                 [ (v,w)
                 | v <- vs
                 , let w:_ = map snd $ reverse $ sort $ [ (p,w) | (v',w,p) <- vws2, v == v' ]
                 ] -}
     
         r  = -0.7 -- head $ drop (length vws1 * 2 - 1) [ p | (_,_,p) <- vws2 ]
     
         vs = sort $ nub ([ v | (v,_) <- vws1 ] ++ [ v | (v,_,_) <- vws2 ])
     
         vws3 = [ (v,w)
                | l <- lines s3
                , [v,"-",w] <- [words l]
                ]
     
     putStrLn (show (length vs) ++ " Swedish words")

     putStrLn ""
     putStrLn "-- Koen"
     putStrLn ("precision: " ++ (vws1 <%% vws3))
     putStrLn ("recall:    " ++ (vws3 <%% vws1))
     
     putStrLn ""
     putStrLn ("-- Prasanth (all)")
     putStrLn ("precision: " ++ (vws2_ <%% vws3))
     putStrLn ("recall:    " ++ (vws3 <%% vws2_))
     
     putStrLn ""
     putStrLn ("-- Prasanth (cut-off " ++ show r ++ ")")
     putStrLn ("precision: " ++ (vws2' <%% vws3))
     putStrLn ("recall:    " ++ (vws3 <%% vws2'))
                
     
{-
     putStrLn "-- pairings that both have --"
     sequence_ $
       [ sequence_
           [ putStrLn ("§" ++ v ++ " - " ++ w)
           | w <- ws1
           , w `elem` ws2'
           ]
       | v <- vs
       , let ws1  = [ w     | (q,w)   <- vws1, q == v ]
             ws2  = [ (w,p) | (q,w,p) <- vws2, q == v ]
             ws2' = nub $ map fst $ take 1 ws2 ++ takeWhile ((>= r) . snd) ws2
       ]
     
     putStrLn ""
     putStrLn "-- pairings that Koen has but not Prasanth --"
     sequence_ $
       [ sequence_
           [ putStrLn (v ++ " - " ++ w)
           | w <- ws1
           , w `notElem` ws2'
           ]
       | v <- vs
       , let ws1  = [ w     | (q,w)   <- vws1, q == v ]
             ws2  = [ (w,p) | (q,w,p) <- vws2, q == v ]
             ws2' = nub $ map fst $ take 1 ws2 ++ takeWhile ((>= r) . snd) ws2
       ]

     putStrLn ""
     putStrLn "-- pairings that Prasanth has but not Koen --"
     sequence_ $
       [ sequence_
           [ putStrLn (v ++ " - " ++ w ++ "  (" ++ show p ++ ")")
           | (w,p) <- ws2'
           , w `notElem` ws1
           ]
       | v <- vs
       , let ws1  = [ w     | (q,w)   <- vws1, q == v ]
             ws2  = [ (w,p) | (q,w,p) <- vws2, q == v ]
             ws2' = nub $ take 1 ws2 ++ takeWhile ((>= r) . snd) ws2
       ]
   -}
     
nub :: Ord a => [a] -> [a]
nub xs = go S.empty xs
 where
  go seen []            = []
  go seen (x:xs)
    | x `S.member` seen = go seen xs
    | otherwise         = x : go (S.insert x seen) xs

