module Nilsson where 

import Control.DeepSeq 

type Capacity = Int 
type Length   = Int 
type Weight   = Int 
type Value    = Int 
type Path     = [Int] 

-- ********************* MCSP Main Functions ******************************

-- ~~~~~~~~~~ Lazy, non path tracking

nch :: [(Capacity, Length)] -> [(Capacity, Length)] -> [(Capacity, Length)] 
nch [] cls2 = cls2 
nch cls1 [] = cls1 
nch clcls1@((c1, l1) : cls1) clcls2@((c2, l2) : cls2) 
    | c1 == c2  = (c1, min l1 l2) : chAux (min l1 l2) cls1   cls2 
    | c1 >  c2  = (c1,     l1   ) : chAux      l1     cls1   clcls2 
    | otherwise = (c2,        l2) : chAux         l2  clcls1 cls2 
    where 
        chAux _ []                []                = [] 
        chAux l []                ((c2, l2) : cls2) = chAux' l c2 l2 [] cls2 
        chAux l ((c1, l1) : cls1) []                = chAux' l c1 l1 cls1 [] 
        chAux l clcls1@((c1, l1) : cls1) clcls2@((c2, l2) : cls2) 
            | c1 == c2  = chAux' l c1 (min l1 l2) cls1   cls2 
            | c1 >  c2  = chAux' l c1      l1     cls1   clcls2 
            | otherwise = chAux' l c2         l2  clcls1 cls2 

        chAux' l c' l' cls1 cls2 
            | l > l'    = (c', l') : chAux l' cls1 cls2 
            | otherwise =            chAux l  cls1 cls2 

njn :: [(Capacity, Length)] -> [(Capacity, Length)] -> [(Capacity, Length)] 
njn [] _  = [] 
njn _  [] = [] 
njn ((c1, l1) : cls1) ((c2, l2) : cls2) 
    | c1 <= c2  =  jnAux c1 l1 l2 cls1 cls2 
    | otherwise =  jnAux c2 l2 l1 cls2 cls1 
    where 
        jnAux c l l' cls1 [] = (c, l + l') : [ (c1, l1 + l') | (c1, l1) <- cls1 ] 
        jnAux c l l' cls1 clcls2@((c2, l2) : cls2) 
            | c <= c2   =  jnAux c l l2 cls1 cls2 
            | otherwise =  (c, l + l') : 
              case cls1 of 
               ((c1, l1) : cls1) | c1 > c2 ->  jnAux c1 l1 l' cls1 clcls2 
               _                           ->  jnAux c2 l2 l  cls2 cls1 

-- ~~~~~~~~~~ Strict, non path tracking

nchS :: [(Capacity, Length)] -> [(Capacity, Length)] -> [(Capacity, Length)] 
nchS [] cls2 = cls2 
nchS cls1 [] = cls1 
nchS clcls1@((c1, l1) : cls1) clcls2@((c2, l2) : cls2) 
    | c1 == c2  = mkStrictPair c1 (min l1 l2) : chAux (min l1 l2)  cls1   cls2 
    | c1 >  c2  =                    (c1, l1) : chAux      l1      cls1   clcls2 
    | otherwise =                    (c2, l2) : chAux         l2   clcls1 cls2 
    where 
        chAux _ []                []                = [] 
        chAux l []                ((c2, l2) : cls2) = chAux' l c2 l2 [] cls2 
        chAux l ((c1, l1) : cls1) []                = chAux' l c1 l1 cls1 [] 
        chAux l clcls1@((c1, l1) : cls1) clcls2@((c2, l2) : cls2) 
            | c1 == c2  = chAux' l c1 (min l1 l2) cls1   cls2 
            | c1 >  c2  = chAux' l c1      l1     cls1   clcls2 
            | otherwise = chAux' l c2         l2  clcls1 cls2 

        chAux' l c' l' cls1 cls2 
            | l > l'    = (c', l') : chAux l' cls1 cls2 
            | otherwise =            chAux l  cls1 cls2 

njnS :: [(Capacity, Length)] -> [(Capacity, Length)] -> [(Capacity, Length)] 
njnS [] _  = [] 
njnS _  [] = [] 
njnS ((c1, l1) : cls1) ((c2, l2) : cls2) 
    | c1 <= c2  = jnAux c1 l1 l2 cls1 cls2 
    | otherwise = jnAux c2 l2 l1 cls2 cls1 
    where 
        jnAux c l l' cls1 [] = mkStrictPair c (l+l') :
                               [ mkStrictPair c1 (l1 + l') | (c1, l1) <- cls1 ] 
        jnAux c l l' cls1 clcls2@((c2, l2) : cls2) 
            | c <= c2   = jnAux c l l2 cls1 cls2 
            | otherwise = mkStrictPair c (l + l') : 
              case cls1 of 
               ((c1, l1) : cls1) | c1 > c2 -> jnAux c1 l1 l' cls1 clcls2 
               _                           -> jnAux c2 l2 l  cls2 cls1 

-- ~~~~~~~~~~ Lazy with path tracking

nchP :: [(Capacity, Length, Path)] -> [(Capacity, Length, Path)] -> [(Capacity, Length, Path)] 
nchP [] cls2 = cls2 
nchP cls1 [] = cls1 
nchP clcls1@((c1, l1, p1) : cls1) clcls2@((c2, l2, p2) : cls2) 
    | c1 == c2  = if min l1 l2 == l2 then (c1, min l1 l2, p2) : chAux (min l1 l2) cls1 cls2 
                                     else (c1, min l1 l2, p1) : chAux (min l1 l2) cls1 cls2 
    | c1 >  c2  = (c1, l1, p1)  : chAux l1 cls1 clcls2 
    | otherwise = (c2, l2, p2)  : chAux l2 clcls1 cls2 
    where 
        chAux _ [] []                     = []
        chAux l [] ((c2, l2, p2) : cls2)  = chAux' l c2 l2 p2 [] cls2 
        chAux l ((c1, l1, p1) : cls1) []  = chAux' l c1 l1 p1 cls1 [] 
        chAux l clcls1@((c1, l1, p1) : cls1) clcls2@((c2, l2, p2) : cls2) 
            | c1 == c2  = if min l1 l2 == l2 then chAux' l c1 (min l1 l2) p2 cls1 cls2 
                                             else chAux' l c1 (min l1 l2) p1 cls1 cls2 
            | c1 >  c2  = chAux' l c1 l1 p1 cls1 clcls2 
            | otherwise = chAux' l c2 l2 p2 clcls1 cls2 

        chAux' l c' l' p cls1 cls2 
            | l > l'    = (c', l', p) : chAux l' cls1 cls2 
            | otherwise =               chAux l  cls1 cls2 

njnP :: [(Capacity, Length, Path)] -> [(Capacity, Length, Path)] -> [(Capacity, Length, Path)] 
njnP [] _  = [] 
njnP _  [] = [] 
njnP ((c1, l1, p1) : cls1) ((c2, l2, p2) : cls2) 
    | c1 <= c2  = jnAux c1 l1 l2 p1 p2 cls1 cls2 
    | otherwise = jnAux c2 l2 l1 p2 p1 cls2 cls1 
    where 
        jnAux c l l' p p' cls1 [] = (c, l + l', jnPath p p') :
                                    [ (c1, l1 + l', jnPath p' p1) | (c1, l1, p1) <- cls1 ] 
        jnAux c l l' p p' cls1 clcls2@((c2, l2, p2) : cls2) 
            | c <= c2   = jnAux c l l2 p p2 cls1 cls2 
            | otherwise = (c, l + l', jnPath p p') : 
              case cls1 of 
               ((c1, l1, p1) : cls1) | c1 > c2 -> jnAux c1 l1 l' p1 p' cls1 clcls2 
               _                               -> jnAux c2 l2 l  p2 p  cls2 cls1 

-- ~~~~~~~~~~ Strict with path tracking

nchPS :: [(Capacity, Length, Path)] -> [(Capacity, Length, Path)] -> [(Capacity, Length, Path)] 
nchPS [] cls2 = cls2 
nchPS cls1 [] = cls1 
nchPS clcls1@((c1, l1, p1) : cls1) clcls2@((c2, l2, p2) : cls2) 
    | c1 == c2  = if min l1 l2 == l2 then mkStrictTriple c1 (min l1 l2) p2 : chAux (min l1 l2) cls1 cls2 
                                     else mkStrictTriple c1 (min l1 l2) p1 : chAux (min l1 l2) cls1 cls2 
    | c1 >  c2  = (c1, l1, p1)  : chAux l1 cls1 clcls2 
    | otherwise = (c2, l2, p2)  : chAux l2 clcls1 cls2 
    where
        chAux _ [] []                     = []
        chAux l [] ((c2, l2, p2) : cls2)  = chAux' l c2 l2 p2 [] cls2 
        chAux l ((c1, l1, p1) : cls1) []  = chAux' l c1 l1 p1 cls1 [] 
        chAux l clcls1@((c1, l1, p1) : cls1) clcls2@((c2, l2, p2) : cls2)
            | c1 == c2  = if min l1 l2 == l2 then chAux' l c1 (min l1 l2) p2 cls1 cls2 
                                             else chAux' l c1 (min l1 l2) p1 cls1 cls2 
            | c1 >  c2  = chAux' l c1 l1 p1 cls1 clcls2 
            | otherwise = chAux' l c2 l2 p2 clcls1 cls2 

        chAux' l c' l' p cls1 cls2
            | l > l'    = (c', l', p) : chAux l' cls1 cls2 
            | otherwise =               chAux l  cls1 cls2 

njnPS :: [(Capacity, Length, Path)] -> [(Capacity, Length, Path)] -> [(Capacity, Length, Path)] 
njnPS [] _  = [] 
njnPS _  [] = [] 
njnPS ((c1, l1, p1) : cls1) ((c2, l2, p2) : cls2) 
    | c1 <= c2  = jnAux c1 l1 l2 p1 p2 cls1 cls2 
    | otherwise = jnAux c2 l2 l1 p2 p1 cls2 cls1 
    where 
        jnAux c l l' p p' cls1 [] = (mkStrictTriple c  (l + l') (jnPath p p')) : 
                                    [ (mkStrictTriple c1 (l1 + l')  (jnPath p' p1)) | (c1, l1, p1) <- cls1 ] 
        jnAux c l l' p p' cls1 clcls2@((c2, l2, p2) : cls2) 
            | c <= c2   = jnAux c l l2 p p2 cls1 cls2 
            | otherwise = (mkStrictTriple c (l + l')  (jnPath p p')) : 
              case cls1 of 
               ((c1, l1, p1) : cls1) | c1 > c2 -> jnAux c1 l1 l' p1 p' cls1 clcls2 
               _                               -> jnAux c2 l2 l  p2 p  cls2 cls1 


-- *****************************  Knapsack Functions *******************************************

-- ~~~~~~~~~~ Lazy, non path tracking

njnk :: Weight -> (Value,Weight) -> [(Value,Weight)] -> [(Value,Weight)] 
njnk wc (v,w) []   = if   w > wc 
                     then [] 
                     else [(v,w)] 
njnk wc (v,w) vwss = if   w > wc 
                     then vwss 
                     else njnAk wc (v,w) vwss 

njnAk :: Weight -> (Value,Weight) -> [(Value,Weight)] -> [(Value,Weight)]  
njnAk wc (v,w) []       = [(v,w)]
njnAk wc (v,w) (vw:vws) 
  | w + (snd vw) > wc = njnAk wc (v,w) vws 
  | otherwise         = (v+fst vw, w+snd vw) : njnAk wc (v,w) vws 


nchk :: [(Value,Weight)] -> [(Value,Weight)] -> [(Value,Weight)] 
nchk xs ys = nch xs ys 

-- ~~~~~~~~~~ Strict, non path tracking

njnkS :: Weight -> (Value,Weight) -> [(Value,Weight)] -> [(Value,Weight)] 
njnkS wc (v,w) []   = if   w > wc 
                      then [] 
                      else [(v,w)] 
njnkS wc (v,w) vwss = if   w > wc 
                     then vwss 
                     else njnAkS wc (v,w) vwss 

njnAkS :: Weight -> (Value,Weight) -> [(Value,Weight)] -> [(Value,Weight)]  
njnAkS wc (v,w) []       = [(v,w)]
njnAkS wc (v,w) (vw:vws) 
  | w + (snd vw) > wc = njnAkS wc (v,w) vws 
  | otherwise         = mkStrictPair (v+fst vw) (w+snd vw) : njnAkS wc (v,w) vws 


nchkS :: [(Value,Weight)] -> [(Value,Weight)] -> [(Value,Weight)] 
nchkS xs ys = nchS xs ys 

-- ~~~~~~~~~~ Lazy with path tracking

njnkP :: Weight -> (Value,Weight,Path) -> [(Value,Weight,Path)] -> [(Value,Weight,Path)] 
njnkP wc (v,w,p) []   = if   w > wc 
                        then [] 
                        else [(v,w,p)] 
njnkP wc (v,w,p) vwss = if   w > wc 
                        then vwss 
                        else njnAkP wc (v,w,p) vwss 

njnAkP :: Weight -> (Value,Weight,Path) -> [(Value,Weight,Path)] -> [(Value,Weight,Path)]  
njnAkP wc (v,w,p) []       = [(v,w,p)]
njnAkP wc (v,w,p) (vw:vws) 
  | w + (snd3 vw) > wc = njnAkP wc (v,w,p) vws 
  | otherwise          = (v+fst3 vw, w+snd3 vw, (trd3 vw)++p) : njnAkP wc (v,w,p) vws 


nchkP :: [(Value,Weight,Path)] -> [(Value,Weight,Path)] -> [(Value,Weight,Path)] 
nchkP xs ys = nchP xs ys 

-- ~~~~~~~~~~ Strict with path tracking

njnkPS :: Weight -> (Value,Weight,Path) -> [(Value,Weight,Path)] -> [(Value,Weight,Path)] 
njnkPS wc (v,w,p) []   = if   w > wc 
                         then [] 
                         else [(v,w,p)] 
njnkPS wc (v,w,p) vwss = if   w > wc 
                         then vwss 
                         else njnAkPS wc (v,w,p) vwss 

njnAkPS :: Weight -> (Value,Weight,Path) -> [(Value,Weight,Path)] -> [(Value,Weight,Path)]  
njnAkPS wc (v,w,p) []       = [(v,w,p)]
njnAkPS wc (v,w,p) (vw:vws) 
  | w + (snd3 vw) > wc = njnAkPS wc (v,w,p) vws 
  | otherwise          = mkStrictTriple (v+fst3 vw) (w+snd3 vw) ((trd3 vw)++p) :
                                        njnAkPS wc (v,w,p) vws 

nchkPS :: [(Value,Weight,Path)] -> [(Value,Weight,Path)] -> [(Value,Weight,Path)] 
nchkPS xs ys = nchPS xs ys 


-- ********************* Auxiliar Functions ******************************

fst3 (x,_,_) = x
snd3 (_,y,_) = y
trd3 (_,_,z) = z 

mkStrictPair x y = 
   let xy = (x, y) 
   in  deepseq xy xy 

mkStrictTriple x y z = 
   let xyz = (x, y, z)
   in  deepseq xyz xyz 

jnPath :: Path -> Path -> Path 
jnPath [] ys = ys 
jnPath xs [] = xs
jnPath x'@(x:xs) y'@(y:ys) =
        if last x' == y then x'++ys 
        else y'++xs 

mixListsPairs          :: (Num a, Num b, Num c) => [[a]] -> [(b,c)] -> [[(a,b,c)]] 
mixListsPairs   []   _ =  [] 
mixListsPairs (x:xs) y =  mixLP x y : mixListsPairs xs (drop (length x) y)
 where 
    mixLP   []      _   = [] 
    mixLP   [x]     _   = [(x,2,1)]
--    mixLP [x] (y:ys)    = (x, fst y, snd y) : []
    mixLP (x:xs) (y:ys) = (x, fst y, snd y) : mixLP xs ys 

-- ~~~~~~~~ Unit and Zero elements for choose and join operations ~~~~~
-- ~~~~~~~~ 1000 represents in this case the infinity value avoiding
-- ~~~~~~~~ to handle a more elaborated data type

zeroNonPath :: [(Int,Int)] 
zeroNonPath =  [(0,1000)] 

oneNonPath :: [(Int,Int)] 
oneNonPath  = [(1000,0)] 

zeroPath :: [(Int,Int,[Int])] 
zeroPath =  [(0,1000,[])] 

onePath :: [(Int,Int,[Int])] 
onePath =  [(1000,0,[])] 

zeroK = [(0,0)] :: [(Value,Weight)] 
oneK  = [(0,0)] :: [(Value,Weight)] 
