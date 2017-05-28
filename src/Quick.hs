{-# LANGUAGE RankNTypes #-}
import Data.STRef
import Data.Vector (fromList, toList, freeze, thaw)
import Control.Monad
import Data.Vector.Mutable (MVector, STVector, read, write, swap)
import qualified Data.Vector as V (Vector, length)
import Data.List (sortOn)
import Prelude hiding (read)
import GHC.ST

-- type annotations added to code from http://vaibhavsagar.com/drafts/imperative-haskell.html

type Partition = forall a s . Ord a => MVector s a -> Int -> Int -> ST s Int

partitionFirst :: Partition 
partitionFirst array l r = do
      p <- read array l
      i <- newSTRef (l+1)
      forM_ [l+1..(r-1)] $ \j -> do
        arrayJ <- read array j
        i'     <- readSTRef i
        when (arrayJ < p) $ do
              swap array i' j
              modifySTRef' i (+1)
      i' <- readSTRef i
      swap array (i'-1) l
      return (i'-1)

partitionLast :: Partition 
partitionLast array l r = do
    swap array (r-1) l
    partitionFirst array l r

partitionMedian :: Partition 
partitionMedian array l r = do
    p <- chooseMedian array l r
    swap array p l
    partitionFirst array l r

chooseMedian :: Partition 
chooseMedian array l r = do
    h <- read array l
    t <- read array (r-1)
    let len = r-l
    let mid = if (len `mod` 2) == 0
        then l + (len `div` 2) - 1
        else l + (len `div` 2)
    m <- read array mid
    let options = sortOn snd [(l, h), (mid, m), (r-1, t)]
    return (fst (options !! 1))

quicksort' :: Ord a => STVector s a -> Int -> Int -> Partition -> STRef s Int -> ST s ()
quicksort' array start end partition comparisons = when (start < end) $ do
    i <- partition array start end
    modifySTRef' comparisons (+ (end-start-1))
    quicksort' array start i   partition comparisons
    quicksort' array (i+1) end partition comparisons


quicksort ::  Ord a => V.Vector a -> Partition ->  (V.Vector a, Int)
quicksort vector partition = runST $ do
    array  <- (thaw vector)
    comps  <- newSTRef 0
    quicksort' array 0 (V.length vector) partition comps
    res <- freeze array
    cnt <- readSTRef comps
    return (res,cnt)

qs :: (Ord a)  => [a] -> [a]
qs as = toList $ fst $ quicksort (fromList as) partitionFirst

