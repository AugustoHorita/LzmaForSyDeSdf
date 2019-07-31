module LZsdf (
  Tree, searchT, codeT, insertPrefixT, insertStringT, lzA
  ) where

import ForSyDe.Shallow

-- Dictionary Structure
data Tree a = Node a [Tree a] deriving (Eq, Show)
type LZtok = [Tree (Char, Int)]

instance (Ord a) => Ord (Tree a) where
  Node a1 l1 <= Node a2 l2
    | a1 < a2 = True
    | a1 > a2 = False
    | a1 == a2 = or $ zipWith (<=) l1 l2


searchT :: Char -> LZtok -> Maybe Int
searchT _ [] = Nothing
searchT c (Node (a,_) _:ls)
  | c /= a = searchT c ls >>= (\x -> return (x+1))
  | otherwise = Just 0


codeT :: String -> LZtok -> Maybe Int
codeT "" _ = Nothing
codeT _ [] = Nothing
codeT [s] (Node (a,n) _:ts)
  | s == a = Just n
  | otherwise = codeT [s] ts
codeT (s:ss) (Node (a,_) l:ts)
  | s /= a = codeT (s:ss) ts
  | otherwise = codeT ss l


insertPrefixT :: String -> LZtok -> Int -> (LZtok, String)
insertPrefixT "" tree _ = (tree, "")
insertPrefixT (c:cs) [] n = ([Node (c,n) []], cs)
insertPrefixT (c:cs) (Node a l:ts) n
  | c == fst a = (Node a (fst pTree) : ts, snd pTree)
  | otherwise = (Node a l : fst dTree, snd dTree)
  where pTree = insertPrefixT cs l n
        dTree = insertPrefixT (c:cs) ts n


insertStringT :: String -> LZtok -> Int -> LZtok
insertStringT "" t _ = t
insertStringT s t n = insertStringT ss tt (n+1)
  where (tt, ss) = insertPrefixT s t n


lzF :: [Char] -> [(LZtok, String, Int)] -> ([Maybe (Maybe Int, Char)], [(LZtok, String, Int)])
lzF [c] [(dic, pref, n)]
  | codeT (c:pref) dic == Nothing = ([Just (codePref, c)], [(insertStringT (c:pref) dic n, "", n+1)])
  | otherwise = ([Nothing], [(dic, c:pref, n)])
  where codePref = codeT pref dic

-- LZ actor model
lzA :: Signal Char -> Signal (Maybe (Maybe Int, Char))
lzA Sis = Sfc
  where (Sfc, fb) = actor22SDF (1,1) (1,1) lzF Sis fb'
        fb' = delaySDF [([],"",0)] fb
