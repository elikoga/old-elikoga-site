{-# LANGUAGE OverloadedStrings #-}
module Cv.Competitions where

import Hakyll

data Competition = Competition
    { compYear :: Int
    , compName :: String
    , compProj :: String
    , compPlacement :: [String]
    }

deCompetitions :: [Competition]
deCompetitions = 
    [ Competition 
        2015
        "Jugend Forscht"
        "Linienfolgen mit Farbsensoren"
        ["Regionalwettbewerb 2. Preis", "ProMINT Sonderperis von dSpace"]
    , Competition 
        2015
        "World Robot Olympiad"
        "Team Epunkt e. RC E"
        ["Regionalwettbewerb 2. Platz", "Deutschlandfinale 10. Platz"]
    , Competition 
        2016
        "First Lego League"
        "Team Jawas"
        [ "Regionalwettbewerb 2. Platz"
        , "Semifinale 2. Platz"
        , "Zentraleuropafinale 17. Platz"
        ]
    , Competition
        2017
        "Jugend Forscht"
        "Wildunfallprävention"
        ["Regionalwettbewerb 3. Preis", "Sonderpreis Umwelt"]
    , Competition
        2017
        "First Lego League"
        "Team Jawas - A new hope"
        [ "Regionalwettbewerb 1. Platz"
        , "Semifinale 3. Platz"
        , "Zentraleuropafinale 8. Platz"
        , "Teilnahme an der Open European Championship 2017 Aarhus"
        ]
    , Competition
        2017
        "World Robot Olympiad"
        "Team NielsEli"
        [ "Regionalwettbewerb 1. Platz"
        , "Deutschlandfinale 3. Platz"
        , "Weltfinale 71. Platz"
        ]
    , Competition
        2018
        "World Robot Olympiad"
        "Team P.I.D."
        [ "Regionalwettbewerb 3. Platz"]
    , Competition
        2018
        "First Lego League"
        "Team Jawas"
        [ "Regionalwettbewerb 1. Platz"
        , "Semifinale 1. Platz"
        , "Deutschlandfinale 11. Platz"
        , "Teilnahme an der Open European Championship 2018 Debrecen"
        ]
    , Competition
        2019
        "Jugend Forscht"
        "Kann man Energie aus der Bewegungsenergie von Regen gewinnen?"
        [ "Regionalwettbewerb 2. Preis"
        , "Preis der Jugendjury Schüler experimentieren"
        , "ProMINT Sonderpreis von dSPACE"
        ]
    , Competition
        2019
        "World Robot Olympiad"
        "Team Jawas"
        [ "Regionalwettbewerb 1. Platz"
        , "Deutschlandfinale 2. Platz"
        , "Weltfinale 15. Platz"
        ]
    , Competition
        2019
        "First Lego League"
        "Team Jawas"
        ["Regionalwettbewerb 2. Platz"]
    , Competition
        2020
        "First Lego League"
        "Team Jawas"
        [ "Regionalwettbewerb 2. Platz"
        , "Semifinale 10. Platz"
        ]
    ]

enCompetitions :: [Competition]
enCompetitions = 
    [ Competition 
        2015
        "Jugend Forscht (German contest for young scientists)"
        "Linienfolgen mit Farbsensoren (Linefollowing with color sensors)"
        ["Regional competition 2. price", "ProMINT special price from dSPACE"]
    , Competition 
        2015
        "World Robot Olympiad"
        "Team Epunkt e. RC E"
        ["Regional competition 2. place", "German final 10. Place"]
    , Competition 
        2016
        "First Lego League"
        "Team Jawas"
        [ "Regional competition 2. place"
        , "Semifinal 2. Place"
        , "Central European final 17. place"
        ]
    , Competition
        2017
        "Jugend Forscht (German contest for young scientists)"
        "Wildunfallprävention (Wildlife Accident Prevention)"
        ["Regional competition 3. price", "Special price: Environment"]
    , Competition
        2017
        "First Lego League"
        "Team Jawas - A new hope"
        [ "Regional competition 1. place"
        , "Semifinal 3. place"
        , "Central European final 8. place"
        , "Participation at the Open European Championship 2017 Aarhus"
        ]
    , Competition
        2017
        "World Robot Olympiad"
        "Team NielsEli"
        [ "Regional competition 1. place"
        , "German final 3. place"
        , "World final 71. place"
        ]
    , Competition
        2018
        "World Robot Olympiad"
        "Team P.I.D."
        ["Regional competition 3. place"]
    , Competition
        2018
        "First Lego League"
        "Team Jawas"
        [ "Regional competition 1. place"
        , "Semifinal 1. place"
        , "Germany final 11. place"
        , "Participation at the Open European Championship 2018 Debrecen"
        ]
    , Competition
        2019
        "Jugend Forscht (German contest for young scientists)"
        "Kann man Energie aus der Bewegungsenergie von Regen gewinnen? (Is it possible to harness energy from the kinetic energy of rain?)"
        [ "Regional competition 2. price"
        , "Price of the Youth Jury \"Schüler experimentieren\" (School students experiment)"
        , "ProMINT special price from dSPACE"
        ]
    , Competition
        2019
        "World Robot Olympiad"
        "Team Jawas"
        [ "Regional competition 1. place"
        , "German final 2. place"
        , "World final 15. place"
        ]
    , Competition
        2019
        "First Lego League"
        "Team Jawas"
        ["Regional competition 2. place"]
    , Competition
        2020
        "First Lego League"
        "Team Jawas"
        [ "Regional competition 2. place"
        , "Semifinal 10. place"
        ]
    ]

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