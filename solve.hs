module Main where

import IO
import Parse
import Data.List



main = do str <- readFile "input.txt"
          let tests = readTests$lines str
          let s = map (parse.scan) tests -- список списков утв
          let ccfs = map makeCCF s
          --mapM_ (putStrLn.show) ccfs
          --putStrLn ""
          --putStrLn ""
          let sccfs = map (\fs -> mfst$makeSCCF fs []) ccfs
          
          writeXml "output.xml" sccfs
          --mapM_ (putStrLn.ppShow) sccfs
          
          
readTests :: [String] -> [String]
readTests [] = []
readTests (s:ss) = (unlines$take n ss):(readTests$drop n ss)
    where n = read s
    

makeCCF :: [Stmt] -> [(Int,[Int],String)]
makeCCF sts = f nsts
    where nsts = zip [1..(length sts)] sts
          f [] = []
          f ((n,Stmt out iN stmt):ss) = (n, dep,stmt):(f ss)
                where pred = take (n-1) nsts
                      r1 = filter (\(i,Stmt pout pin ps) -> elem pout iN) pred
                      r2 = filter (\(i,Stmt pout pin ps) -> elem out pin) pred
                      r3 = filter (\(i,Stmt pout pin ps) -> out==pout ) pred
                      dep = map fst (union (union r1 r2) r3)

subset :: (Eq a) => [a] -> [a] -> Bool
subset [] b     = True
subset (a:aS) b = (elem a b) && (subset aS b)

takeStmt (_,_,s) =s 
takeN (n,_,_) = n  

mfst (x,_,_)=x
msnd (_,x,_)=x
mtrd (_,_,x)=x

unSeq (Seq a) = a
--unSeq (Branch a) = [Branch a]                     

makeSCCF :: [(Int,[Int],String)] -> [Int]->(SCCF (String),[Int],[(Int,[Int],String)])
makeSCCF ccf cover | (length$fst part) == 0 = (Seq [],cover,(snd part))
                   | (length$fst part) == 1 = makeSeq (head$fst part) ---(Seq ((Node (takeStmt$head$fst part)) : (unSeq$fst seqch)),seqcover)
                   | otherwise = (Seq (bch: (unSeq$mfst bseq)),msnd bseq,mtrd bseq)
    where part = partition (\(_,dep,_)->subset dep cover) ccf 
          --seqch = makeSCCF (snd part) seqcover
          --seqcover = union cover [takeN$head$fst part]
          bchs = map (\x -> makeSeq x) (fst part)
          bch = Branch (map mfst bchs)
          bcover = foldl union cover (map msnd bchs)
          unplaced = (foldl intersect (snd part) (map mtrd bchs))
          bseq = makeSCCF (unplaced) bcover
          --bcovers = map (\(n,dep,s) -> union cover [n]) (fst part)
         -- bch = map (\cov -> makeSCCF (snd part) cov) bcovers
          makeSeq (n,dep,s) = (Seq  ((Node s): (unSeq$mfst sch)), msnd sch,mtrd sch)
            where scover = union cover [n]
                  sch = makeSCCF (snd part) scover
                      
data SCCF a = Node a | Seq [SCCF a] | Branch [SCCF a] deriving (Eq,Show)         


sscfToXml :: Int -> SCCF String -> [String]
sscfToXml l (Node s) = [(replicate (l*2) ' ')++"<statement>"++s++"</statement>"]
sscfToXml l (Seq ss) = (foldl (++) [(replicate (l*2) ' ')++ "<sequence>"] (map (sscfToXml (l+1)) ss))++ [(replicate (l*2) ' ')++ "</sequence>"]       
sscfToXml l (Branch sb) = (foldl (++) [(replicate (l*2) ' ')++"<branches>"] 
                                      (map (\b-> [(replicate ((l+1)*2) ' ')++"<branch>"]++
                                                 (sscfToXml (l+2) b)++
                                                 [(replicate ((l+1)*2) ' ')++"</branch>"]  ) sb))++  [(replicate (l*2) ' ')++"</branches>"]                    
    

writeXml  :: String -> [SCCF String] ->  IO()
writeXml path sccfs = writeFile path(unlines$concat (intersperse [""] (map (sscfToXml 0) sccfs)))
                        