{-# LANGUAGE OverloadedStrings #-}
module Cv.Internal where

import Hakyll

data TupleEntry = TupleEntry
    { firstTupleEntry :: String
    , secondTupleEntry :: String
    }

teCtx :: Context TupleEntry
teCtx = field "firstTupleEntry" (\(Item _ (TupleEntry t _)) -> return t)
    <> field "secondTupleEntry" (\(Item _ (TupleEntry _ t)) -> return t)

data ListTupleEntry = ListTupleEntry
    { labelListTupleEntry :: String
    , listListTupleEntry :: [String]
    }

ltCtx :: Context ListTupleEntry
ltCtx = field "labelListTupleEntry" (\(Item _ (ListTupleEntry t _)) -> return t)
    <> listFieldWith "listListTupleEntry" listContext (\(Item _ (ListTupleEntry _ t)) -> return . map (Item "") $ t)

listContext :: Context String
listContext = field "string" itemToCompiler

data Competition = Competition
    { compYear :: Int
    , compName :: String
    , compProj :: String
    , compPlacement :: [String]
    }

competitionCtx :: Context Competition
competitionCtx = field "year" yearFromCompetition
    <> field "competition" competitionNameFromCompetition
    <> field "project" projectFromCompetition
    <> listFieldWith "placements" placementCtx placementsFromCompetition

yearFromCompetition :: Item Competition -> Compiler String
yearFromCompetition (Item _id (Competition year _ _ _)) = return . show $ year

competitionNameFromCompetition :: Item Competition -> Compiler String
competitionNameFromCompetition (Item _id (Competition _ name _ _)) = return name

projectFromCompetition :: Item Competition -> Compiler String
projectFromCompetition (Item _id (Competition _ _ proj _)) = return proj

placementCtx :: Context String
placementCtx = field "placement" itemToCompiler

itemToCompiler :: Item a -> Compiler a
itemToCompiler (Item _id x) = return x

placementsFromCompetition :: Item Competition -> Compiler [Item String]
placementsFromCompetition (Item _id (Competition _ _ _ placements)) = return $ map (Item "") placements