{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MagnusTest
  (
    U2,
    U,
    Zipper
  ) where

import Control.Comonad
import System.IO

class Zipper f where
  left :: f a -> f a
  right :: f a -> f a  
  write :: a -> f a -> f a

-- magnus sin kladd
-- U for universe
data U a = U [a] a [a]
data U2 a = U2 (U (U a))

-- note:
-- i imagine U to be like this
--
--     l     m     r
--     |     |     |
--     v     v     v
-- [.. 2, 1] 0 [1, 2 ..]
--
-- while it really is like this
--
--     l     m     r
--     |     |     |
--     v     v     v
-- [1, 2 ..] 0 [1, 2 ..]
--
-- if left seems weird


instance Zipper U where
  right (U l m (r:rs)) = (U (m:l) r rs)
  left (U (l:ls) m r) = (U ls l (m:r))
  write x (U l _ r) = U l x r

instance Functor U where
  fmap f (U l m r) = U (map f l) (f m) (map f r)

instance Comonad U where
  duplicate x = U (tail $ iterate left x) x (tail $ iterate right x)
  extract (U _ m _) = m

instance Zipper U2 where
  right (U2 u) = U2 (fmap right u)
  left (U2 u) = U2 (fmap left u)
  write x (U2 u) = U2 (fmap (write x) u)

-- maybe this belongs in the zipper typeclass
up :: U2 a -> U2 a
up (U2 u) = U2 (left u)

down :: U2 a -> U2 a
down (U2 u) = U2 (right u)

move :: (f a -> f a) -> (f a -> f a) -> f a -> U (f a)
move g h x = U (tail $ iterate g x) x (tail $ iterate h x)

shift :: Int -> U a -> U a
shift i u = (iterate (if i < 0 then left else right) u) !! abs i

toList i j u = take (j - i) $ half $ shift i u where
  half (U _ m r) = [m] ++ r

-- test
-- sierpinski!
rule :: U Bool -> Bool
rule (U (l:_) _ (r:_)) = l /= r

test = let u = U (repeat False) True (repeat False)
       in putStr $
          unlines $
          take 20 $
          map (map (\x -> if x then '#' else ' ') . toList (-20) 20) $
          iterate (=>> rule) u

