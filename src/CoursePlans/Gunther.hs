import Data.List
import Data.Set( Set )
import qualified Data.Set as S

data Group = G Int Int Int deriving (Eq, Ord)

instance Show Group where
  show (G b s k) =
    "{" ++ (concat (intersperse "," $
            [ show b ++ "B" | b > 0 ] ++
            [ show s ++ "S" | s > 0 ] ++
            [ show k ++ "K" | k > 0 ]))
        ++ "}"

ok :: Group -> Bool
ok (G b s k) = s >= 0 && k >= 0 && b >= 0 && (b >= (s+k) || b == 0 || s == 0)

type State = (Group,Bool,Group)

start :: State
start = (G 5 2 2, True, G 0 0 0)

final :: State -> Bool
final (G b s k, _, _) = b == 0 && s == 0 && k == 0

(?+), (?-) :: Group -> Group -> Group
G b1 s1 k1 ?+ G b2 s2 k2 = G (b1+b2) (s1+s2) (k1+k2)
G b1 s1 k1 ?- G b2 s2 k2 = G (b1-b2) (s1-s2) (k1-k2)

steps :: State -> [State]
steps (g1, True, g2) =
  [ (g1', False, g2')
  | bt <- boats
  , let g1' = g1 ?- bt
  , ok g1'
  , let g2' = g2 ?+ bt
  , ok g2'
  ]
 where
  boats = [G 2 0 0, G 0 2 0, G 1 0 0, G 0 1 0, G 1 1 0, G 0 1 1, G 1 0 1]

steps (g1, False, g2) =
  [ (g1', True, g2')
  | (g2', _, g1') <- steps (g2, True, g1)
  ]

search :: Set State -> State -> [[State]]
search seen now
  | final now = [[now]]
  | otherwise = [ now:path | next <- nexts, path <- search (S.insert now seen) next ]
 where
  nexts = filter (not . (`S.member` seen)) (steps now)

main :: IO ()
main =
  case search S.empty start of
    path:_ ->
      do print path

    [] ->
      do putStrLn "NO"

