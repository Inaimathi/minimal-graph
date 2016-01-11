module Data.MinimalGraph ( fromPairs, fromTriples
                    , vertices, neighbors, query, Query(..)) where

import Data.Function (on)
import Data.List (nub, sortBy)

data Edge a b = Edge { label :: b, from :: a, to :: a } deriving (Eq, Ord, Show)

data Graph a b = Sparse [Edge a b] deriving (Eq, Ord, Show)

toEdge :: (a, b, a) -> Edge a b
toEdge (f, l, t) = Edge { label = l, from = f, to = t }

fromPairs :: [(a, a)] -> Graph a ()
fromPairs = Sparse . map (\(f, t) -> toEdge (f, (), t))

fromTriples :: [(a, b, a)] -> Graph a b
fromTriples = Sparse . map toEdge

verticesIn :: Eq a => [Edge a b] -> [a]
verticesIn = nub . concatMap (\e -> [from e, to e])

vertices :: Eq a => Graph a b -> [a]
vertices (Sparse es) = verticesIn es

edgesFrom :: Eq a => Graph a b -> a -> [Edge a b]
edgesFrom (Sparse es) v = filter ((==v) . from) es

edgesTo :: Eq a => Graph a b -> a -> [Edge a b]
edgesTo (Sparse es) v = filter ((==v) . to) es

neighbors :: Eq a => Graph a b -> a -> [a]
neighbors g v = concat [map to $ edgesFrom g v, map from $ edgesTo g v]

data Query b = Out (b -> Bool)
             | In (b -> Bool)
             | InC (b -> b -> Ordering)
             | OutC (b -> b -> Ordering)
             | AllOut
             | AllIn

query :: Eq a => [Query b] -> Graph a b -> a -> [a]
query q g v = recur q [v]
    where recur [] acc = acc
          recur (clause:rest) acc =
              recur rest $ concatMap step acc
                  where step = case clause of
                                 AllOut -> map to . edgesFrom g
                                 AllIn -> map from . edgesTo g
                                 InC f -> map from .take 1 . sortBy (f `on` label) . edgesTo g
                                 OutC f -> map to . take 1 . sortBy (f `on` label) . edgesTo g
                                 Out f -> map to . filter (f . label). edgesFrom g
                                 In f -> map from . filter (f . label) . edgesTo g
