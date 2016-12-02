module Main where

import HFact

main :: IO ()
main = do
  kernel <- newReasoningKernel
  sourceConcept <- mkConcept kernel "http://www.example.com/ontology/basic#ClassA"
  strSubConcepts <- getElements1D =<< getSubConcepts kernel sourceConcept All
  mapM_ putStrLn strSubConcepts
