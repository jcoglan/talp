import Prelude hiding (and, or, not, fst, snd)
import Lambda


section :: Char -> IO ()
section c = putStrLn $ replicate 72 c


main = do
  let t1 = (lam "x" $ var 'x') <~ var 'y'
  print t1
  print $ betaReduce t1
  section '-'

  let t2 = (lam "x" $ var 'x' <~ (lam "x" $ var 'x')) <~ (var 'u' <~ var 'r')
  print t2
  print $ betaReduce t2


  let id = lam "x" (var 'x')
  let t = id <~ id <~ lam "z" (id <~ var 'z')

  section '='
  putStrLn "[Full beta reduction]"
  eval print fullBeta t

  section '='
  putStrLn "[Normal order]"
  eval print normalOrder t

  section '='
  putStrLn "[Call by name]"
  eval print callByName t

  section '='
  putStrLn "[Call by value]"
  eval print callByValue t

  section '='
  putStrLn "[Test]"

  let testTru = test <~ tru <~ var 'v' <~ var 'w'
  byValue testTru

  let testFls = test <~ fls <~ var 'v' <~ var 'w'
  byValue testFls

  section '='
  putStrLn "[And]"
  byValue $ and <~ tru <~ tru
  byValue $ and <~ fls <~ tru
  byValue $ and <~ tru <~ fls
  byValue $ and <~ fls <~ fls

  section '='
  putStrLn "[Or]"
  byValue $ or <~ tru <~ tru
  byValue $ or <~ fls <~ tru
  byValue $ or <~ tru <~ fls
  byValue $ or <~ fls <~ fls

  section '='
  putStrLn "[Not]"
  byValue $ not <~ tru
  byValue $ not <~ fls

  section '='
  putStrLn "[Pairs]"
  let p = pair <~ var 'x' <~ var 'y'
  byValue p
  section '-'
  byValue $ fst <~ p
  section '-'
  byValue $ snd <~ p

  section '='
  putStrLn "[Numerals]"
  byValue $ c 0
  byValue $ c 1
  byValue $ c 2
  byValue $ c 3
  section '-'
  byValue $ scc <~ c 3
  section '-'
  normal $ scc <~ c 3
  section '-'
  byValue $ scc' <~ c 3
  section '-'
  normal $ scc' <~ c 3

  section '='
  putStrLn "[Plus]"
  byValue $ plus <~ c 2 <~ c 3
  section '-'
  normal $ plus <~ c 2 <~ c 3

  section '='
  putStrLn "[Times]"
  normal $ times <~ c 2 <~ c 2

  section '='
  putStrLn "[Power]"
  normal $ power <~ c 3 <~ c 2

