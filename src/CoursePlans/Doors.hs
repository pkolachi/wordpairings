import SAT
import SAT.Val

main =
  withNewSolver $ \s ->
    let steps 0 girl =
          do sequence_ [ addClause s [neg x] | x <- girl ]
             return []
        
        steps n girl =
          do boy   <- newVal s [ i | (i,_) <- [1..] `zip` girl ]
             girl' <- sequence
                      [ do x <- newLit s
                           addClause s [x, neg gleft,  boy .= (i-1)]
                           addClause s [x, neg gright, boy .= (i+1)]
                           return x
                      | (i,(gright,gleft)) <- [1..] `zip` ((tail girl ++ [false]) `zip` (false:girl))
                      ]
             strat <- steps (n-1) girl'
             return (boy:strat)

     in do strat <- steps 14 (replicate n true)
           b <- solve s []
           if b then
             do putStrLn "YES!"
                putStrLn (concatMap show [1..n])
                sequence_ [ do i <- SAT.Val.modelValue s boy
                               putStrLn (replicate (i-1) ' ' ++ show i ++ replicate (n-i) ' ')
                          | boy <- strat
                          ]
            else
             do putStrLn "NO!"

n = 9

