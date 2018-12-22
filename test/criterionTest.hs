import Criterion.Main
import qualified MinHeapTree as T
import qualified MinHeapArray as A
import qualified FileBinomiale as F
import qualified Data.Heap as H
import Data.Word
import System.Random (randomRIO)

randomList :: Int -> IO([Word64])
randomList 0 = return []
randomList n = do
  r  <- randomRIO (1,18446744073709551614) :: IO Word64
  rs <- randomList (n-1)
  return (r:rs)

splitHalf list = splitAt (((length list) + 1) `div` 2) list

createLists = do
  [a,b,c,d,e,f,g] <- mapM randomList [100,500,1000,5000,10000,20000,50000]
  return [a,b,c,d,e,f,g]

createArrays = do
	lists <- mapM randomList [50,50,250,250,500,500,2500,2500,5000,5000,10000,10000,25000,25000]
	arrays <- mapM A.consIter lists
	return arrays

createTrees = do
	lists <- mapM randomList [50,50,250,250,500,500,2500,2500,5000,5000,10000,10000,25000,25000]
	let trees = map T.consIter lists
	return trees


createQueues = do
	lists <- mapM randomList [50,50,250,250,500,500,2500,2500,5000,5000,10000,10000,25000,25000]
	let queues = map F.consIter lists
	return queues

unionFB (a, b) = F.union a b
unionT [a, b] = T.union a b
addFB (a, b) = F.ajoutElem a b
addT (a, b) = T.add a b


-- Our benchmark harness.
main = do
	a@[a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1] <- createArrays
	b@[a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2] <- createTrees
	c@[a3,b3,c3,d3,e3,f3,g3,h3,i3,j3,k3,l3,m3,n3] <- createQueues
	defaultMain [
	    env createLists $ \ ~[a,b,c,d,e,f,g] -> bgroup "consIter" [
	  			bgroup "tree" [
	  			     bench "100"  $ whnf T.consIter a
	               , bench "500"  $ whnf T.consIter b
	               , bench "1000"  $ whnf T.consIter c
	               , bench "5000" $ whnf T.consIter d
	               , bench "10000" $ whnf T.consIter e
	               , bench "20000" $ whnf T.consIter f
	               , bench "50000" $ whnf T.consIter g
	               ]
	            ,bgroup "fb" [
	  			     bench "100"  $ whnf F.consIter a
	               , bench "500"  $ whnf F.consIter b
	               , bench "1000"  $ whnf F.consIter c
	               , bench "5000" $ whnf F.consIter d
	               , bench "10000" $ whnf F.consIter e
	               , bench "20000" $ whnf F.consIter f
	               , bench "50000" $ whnf F.consIter g
	            ]
	            ,bgroup "array" [
	  			     bench "100"  $ whnfIO $ A.consIter a
	               , bench "500"  $ whnfIO $ A.consIter b
	               , bench "1000"  $ whnfIO $ A.consIter c
	               , bench "5000" $ whnfIO $ A.consIter d
	               , bench "10000" $ whnfIO $ A.consIter e
	               , bench "20000" $ whnfIO $ A.consIter f
	               , bench "50000" $ whnfIO $ A.consIter g
	            ]
	    	]
	    , bgroup "union" [
	  			bgroup "tree" [
	  			     bench "100"  $ whnf  unionT [a2, b2]
	               , bench "500"  $ whnf  unionT [c2, d2]
	               , bench "1000"  $ whnf  unionT [e2, f2]
	               , bench "5000" $ whnf  unionT [g2, h2]
	               , bench "10000" $ whnf  unionT [i2, j2]
	               , bench "20000" $ whnf  unionT [k2, l2]
	               , bench "50000" $ whnf  unionT [m2, n2]
	               ]
	            ,bgroup "fb" [
	  			     bench "100"  $ whnf unionFB (a3, b3) -- no evaluado
	               , bench "500"  $ whnf unionFB (c3, d3)
	               , bench "1000"  $ whnf unionFB (e3, f3)
	               , bench "5000" $ whnf unionFB (g3, h3)
	               , bench "10000" $ whnf unionFB (i3, j3)
	               , bench "20000" $ whnf unionFB (k3, k3)
	               , bench "50000" $ whnf unionFB (m3, n3)
	            ]
	            ,bgroup "array" [
	  			     bench "100"  $ whnfIO $ A.union a1 b1
	               , bench "500"  $ whnfIO $  A.union c1 d1
	               , bench "1000"  $ whnfIO $ A.union e1 f1
	               , bench "5000" $ whnfIO $ A.union g1 h1
	               , bench "10000" $ whnfIO $ A.union i1 j1
	               , bench "20000" $ whnfIO $ A.union k1 l1
	               , bench "50000" $ whnfIO $ A.union m1 n1
	            ]
	    	]
	    ,bgroup "add" [
	  			bgroup "tree" [
	  			     bench "100"  $ whnf addT (a2)
	               , bench "500"  $ whnf
	               , bench "1000"  $ whnf
	               , bench "5000" $ whnf
	               , bench "10000" $ whnf
	               , bench "20000" $ whnf
	               , bench "50000" $ whnf
	               ]
	            ,bgroup "fb" [
	  			     bench "100"  $ whnf unionFB (a3, b3) -- no evaluado
	               , bench "500"  $ whnf unionFB (c3, d3)
	               , bench "1000"  $ whnf unionFB (e3, f3)
	               , bench "5000" $ whnf unionFB (g3, h3)
	               , bench "10000" $ whnf unionFB (i3, j3)
	               , bench "20000" $ whnf unionFB (k3, k3)
	               , bench "50000" $ whnf unionFB (m3, n3)
	            ]
	            ,bgroup "array" [
	  			     bench "100"  $ whnfIO $ A.union a1 b1
	               , bench "500"  $ whnfIO $  A.union c1 d1
	               , bench "1000"  $ whnfIO $ A.union e1 f1
	               , bench "5000" $ whnfIO $ A.union g1 h1
	               , bench "10000" $ whnfIO $ A.union i1 j1
	               , bench "20000" $ whnfIO $ A.union k1 l1
	               , bench "50000" $ whnfIO $ A.union m1 n1
	            ]
	    	]
	    -- ,bgroup "deleteMin" [
	  		-- 	bgroup "tree" [
	  		-- 	     bench "100"  $ whnf  unionT (a2, b2)
	    --            , bench "500"  $ whnf  unionT (c2, d2)
	    --            , bench "1000"  $ whnf  unionT (e2, f2)
	    --            , bench "5000" $ whnf  unionT (g2, h2)
	    --            , bench "10000" $ whnf  unionT (i2, j2)
	    --            , bench "20000" $ whnf  unionT (k2, l2)
	    --            , bench "50000" $ whnf  unionT (m2, n2)
	    --            ]
	    --         ,bgroup "fb" [
	  		-- 	     bench "100"  $ whnf unionFB (a3, b3) -- no evaluado
	    --            , bench "500"  $ whnf unionFB (c3, d3)
	    --            , bench "1000"  $ whnf unionFB (e3, f3)
	    --            , bench "5000" $ whnf unionFB (g3, h3)
	    --            , bench "10000" $ whnf unionFB (i3, j3)
	    --            , bench "20000" $ whnf unionFB (k3, k3)
	    --            , bench "50000" $ whnf unionFB (m3, n3)
	    --         ]
	    --         ,bgroup "array" [
	  		-- 	     bench "100"  $ whnfIO $ A.union a1 b1
	    --            , bench "500"  $ whnfIO $  A.union c1 d1
	    --            , bench "1000"  $ whnfIO $ A.union e1 f1
	    --            , bench "5000" $ whnfIO $ A.union g1 h1
	    --            , bench "10000" $ whnfIO $ A.union i1 j1
	    --            , bench "20000" $ whnfIO $ A.union k1 l1
	    --            , bench "50000" $ whnfIO $ A.union m1 n1
	    --         ]
	    -- 	]
		]
