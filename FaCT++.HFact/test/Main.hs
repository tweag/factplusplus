module Main where

import HFact

main :: IO ()
main = do
  kernel <- newReasoningKernel
  loadLispFacts kernel "./test-data/input.tbox"
  whenM (loadLispFacts kernel "this-file-does-not-exist") $
    error "HFact: test/Main.hs: loadLispFacts loaded ghost file"
  sourceConcept <- mkConcept kernel "http://www.example.com/ontology/basic#ClassA"
  strSubConcepts <- getElements1D =<< getSubConcepts kernel sourceConcept All
  mapM_ putStrLn strSubConcepts
  where whenM pred act = pred >>= \b -> if b then act else return ()
