{-# LANGUAGE OverloadedStrings #-}
module Cv.De
    ( deCvCtx
    , deOtherCtx
    ) where

import Hakyll
import Cv.Internal

import Data.List
import Data.Function

deCvCtx :: Context String
deCvCtx = constField "address" "Eli Kogan-Wang<br/>Im Lichenfelde 47<br/>33100 Paderborn<br/>Deutschland"
    <> constField "back" "Zurück zur Hauptseite"
    <> constField "resume" "Lebenslauf"
    <> constField "title" "Student"
    <> constField "educationSection" "Bildung"
    <> constField "year" "Jahr"
    <> constField "eduInstitution" "Bildungseinrichtung"
    <> listField "eduInstitutions" teCtx (return deEdus)
    <> constField "internshipSection" "Praktika"
    <> constField "internship" "Praktikum"
    <> listField "internships" teCtx (return deInternships)
    <> constField "languageSection" "Sprachkenntnisse"
    <> constField "language" "Sprache"
    <> constField "langKnowledge" "Erfahrung"
    <> listField "languages" teCtx (return deLanguages)
    <> constField "devTechnologySection" "Technische Kenntnisse"
    <> constField "technology" "Sprache/Technologie"
    <> constField "keywords" "Schlüsselbegriffe"
    <> listField "devTechnologies" ltCtx (return deDevTechnologies)
    <> constField "otherSection" "Andere Einträge"
    <> constField "otherDescr" "Andere potentiell relevante Einträge wie beispielsweise MINT-Wettbewerbsteilnahmen oder Fortbildungen sind bei <a href=\"/cv/de/other.html\">https://cv.eliko.ga/cv/de/other.html</a> zu finden."
    <> defaultContext

deEdus :: [Item TupleEntry]
deEdus = reverse $ map (Item "")
    [ TupleEntry "2010-2013" "Grundschule Bonifatius"
    , TupleEntry "2013-2021" "Gymnasium Schloß Neuhaus"
    , TupleEntry "2021-*" "Universität Paderborn"
    ]

deInternships :: [Item TupleEntry]
deInternships = reverse $ map (Item "") 
    [ TupleEntry "2018" "Berufsfelderkundungstag bei der WABSOLUTE® GmbH"
    , TupleEntry "2018" "Berufsfelderkundungstag bei Hesse Mechatronics"
    , TupleEntry "2018" "Berufsfelderkundungstag bei der WFG Paderborn mbH"
    , TupleEntry "2020" "Schülerpraktikum bei der Fastec GmbH<br/>C#/WPF Entwicklung"
    , TupleEntry "2020" "Schülerpraktikum bei der ORDIX AG<br/>PHP Entwicklung"
    , TupleEntry "2021" "Vergütetes Praktikum bei der Flying Circus Internet Operations GmbH<br/><a href=\"https://github.com/flyingcircusio/fc-nixos/commits?author=elikoga&until=2021-07-13\">NixOS Plattform Entwicklung</a>"
    , TupleEntry "2021" "Vergütetes Praktikum bei der Weidmüller Interface GmbH & Co. KG<br/>Frontend Entwicklung mit Svelte"
    ]

deLanguages :: [Item TupleEntry]
deLanguages = map (Item "")
    [ TupleEntry "Deutsch" "Muttersprache"
    , TupleEntry "Englisch" $ "Teilnahme an der Cambridge English: Advanced (C1) Prüfung"
            ++ "<br/>Ausfall der schriftlichen Prüfungen durch COVID-19 Pandemie"
    ]

deDevTechnologies :: [Item ListTupleEntry]
deDevTechnologies = map (Item "") . sortBy (compare `on` labelListTupleEntry) $
    [ ListTupleEntry "C/C++" ["ESP32", "ATmega Microcontroller", "CMake"]
    , ListTupleEntry "Java" ["Maven"]
    , ListTupleEntry "Python" ["NumPy/SciPy", "OpenCV", "matplotlib"]
    , ListTupleEntry "C#" ["WPF", "Autofac", "xUnit", "Unity3D"]
    , ListTupleEntry "Unity3D" ["VR/OpenXR"]
    , ListTupleEntry "JavaScript" ["Node.js", "express.js", "TypeScript", "React", "Svelte", "Rete.js"]
    , ListTupleEntry "PHP" ["Laravel Framework", "Composer"]
    , ListTupleEntry "HTML/CSS" ["Bootstrap", "jQuery"]
    , ListTupleEntry "SQL" ["MySQL/MariaDB", "PostgreSQL", "SQLite"]
    , ListTupleEntry "Kotlin" ["Micronaut"]
    , ListTupleEntry "Haskell" ["Stack", "Yesod", "Hakyll", "mtl"]
    , ListTupleEntry "Linux" ["Bash", "Debian", "Ubuntu", "NixOS", "Arch Linux"]
    , ListTupleEntry "Docker" ["Docker Compose"]
    ]

deOtherCtx :: Context String
deOtherCtx = constField "back" "Zurück zum Lebenslauf"
    <> constField "otherTitle" "Sonstige Leistungen"
    <> constField "otherDescr" ("Ich habe sowohl an mehreren MINT-Wettbewerben als auch an anderen relevanten Veranstaltungen teilgenommen."
                    ++ "Eine Auflistung dieser Teilnahmen finden Sie hier.")
    <> constField "otherLangText" "Sie können die englische Version dieser Auflistung <a href=\"/cv/en/other.html\">hier</a> besuchen."
    <> constField "otherPricesSection" "Sonstige Preise und Auszeichnungen"
    <> constField "year" "Jahr"
    <> constField "price" "Preis/Auszeichung"
    <> listField "prices" teCtx (return dePrices)
    <> constField "eventsSection" "Workshops / Fortbildungen / Veranstaltungen"
    <> constField "event" "Workshop / Fortbildung / Veranstaltung"
    <> listField "events" teCtx (return deEvents)
    <> constField "competitionSection" "MINT-Wettbewerbsteilnahmen"
    <> constField "competitionLabel" "Wettbewerb"
    <> constField "projectLabel" "Team/Projekt"
    <> constField "placementLabel" "Platzierung/Preis"
    <> listField "competitions" competitionCtx (return deCompetitions)
    <> defaultContext

dePrices :: [Item TupleEntry]
dePrices = map (Item "") 
    [ TupleEntry "2017" "Förderpreis der Wirtschaft in Anerkennung besonderer Leistungen in den naturwissenschaftlichen Fächern"
    , TupleEntry "2018" "Förderpreis der Wirtschaft in Anerkennung besonderer Leistungen in den naturwissenschaftlichen Fächern"
    , TupleEntry "2018" "Einstein-OWL Auszeichnung für ein Forschunsprojekt"
    ]

deEvents :: [Item TupleEntry]
deEvents = map (Item "") 
    [ TupleEntry "2017" "Schülerakademie Mathematik (SAM-OWL)"
    , TupleEntry "2018" "Workshop - Erstellung einer responsiven Website im Team"
    , TupleEntry "2018" "PIT-Hackathon by Paderborn ist Informatik e.V. - Unity3D+OpenXR"
    , TupleEntry "2018" "PIT-Hackathon by Paderborn ist Informatik e.V. - Unity3D"
    ]

deCompetitions :: [Item Competition]
deCompetitions = map (Item "")
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
