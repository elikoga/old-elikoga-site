{-# LANGUAGE OverloadedStrings #-}
module Cv.En
    ( enCvCtx
    , enOtherCtx
    ) where

import Hakyll
import Cv.Internal

enCvCtx :: Context String
enCvCtx = constField "address" "Eli Kogan-Wang<br/>Von-Stauffenberg-Str. 22<br/>33102 Paderborn"
    <> constField "back" "Go back to main Page"
    <> constField "resume" "Résumé"
    <> constField "title" "Prospective high school graduate"
    <> constField "educationSection" "Education"
    <> constField "year" "Year"
    <> constField "eduInstitution" "School"
    <> listField "eduInstitutions" teCtx (return enEdus)
    <> constField "internshipSection" "Internships"
    <> constField "internship" "Internship"
    <> listField "internships" teCtx (return enInternships)
    <> constField "languageSection" "Language proficiency"
    <> constField "language" "Language"
    <> constField "langKnowledge" "Proficiency"
    <> listField "languages" teCtx (return enLanguages)
    <> constField "devTechnologySection" "Technical proficiencies"
    <> constField "technology" "Language/Technology"
    <> constField "keywords" "Keywords"
    <> listField "devTechnologies" ltCtx (return enDevTechnologies)
    <> constField "otherSection" "Other entries"
    <> constField "otherDescr" "Other potentially relevant entries like STEM-Competition participations or further training are avialiable at <a href=\"/cv/en/other.html\">https://eliko.ga/cv/en/other.html</a>."
    <> defaultContext

enEdus :: [Item TupleEntry]
enEdus = map (Item "") 
    [ TupleEntry "2010-2013" "Primary school: Grundschule Bonifatius"
    , TupleEntry "2013-2021" "Secondary school: Gymnasium Schloß Neuhaus"
    ]

enInternships :: [Item TupleEntry]
enInternships = map (Item "") 
    [ TupleEntry "2018" "Career Exploration Day at WABSOLUTE GmbH"
    , TupleEntry "2018" "Career Exploration Day at Hesse Mechatronics"
    , TupleEntry "2018" "Career Exploration Day at the WFG Paderborn mbH"
    , TupleEntry "2020" "School student internship at the Fastec GmbH<br/>C#/WPF Development"
    , TupleEntry "2020" "School student internship at the ORDIX AG<br/>PHP Entwicklung"
    ]

enLanguages :: [Item TupleEntry]
enLanguages = map (Item "")
    [ TupleEntry "German" "Native language"
    , TupleEntry "English" $ "Participation in the Cambridge English: Advanced (C1) Exam"
            ++ "<br/>Cancellation of written Exams due to the COVID-19 Pandemic"
    ]

enDevTechnologies :: [Item ListTupleEntry]
enDevTechnologies = map (Item "")
    [ ListTupleEntry "C/C++" ["ESP32", "ATmega Microcontroller", "CMake"]
    , ListTupleEntry "Java" ["Maven"]
    , ListTupleEntry "Python" ["NumPy/SciPy", "OpenCV", "matplotlib"]
    , ListTupleEntry "C#" ["WPF", "Autofac", "xUnit", "Unity3D"]
    , ListTupleEntry "Unity3D" ["VR/OpenXR"]
    , ListTupleEntry "JavaScript" ["Node.js", "express.js"]
    , ListTupleEntry "PHP" ["Laravel Framework"]
    , ListTupleEntry "HTML/CSS" ["Bootstrap", "jQuery"]
    , ListTupleEntry "SQL" ["MySQL/MariaDB", "PostgreSQL"]
    , ListTupleEntry "Kotlin" []
    , ListTupleEntry "Haskell" ["Stack", "Yesod", "Hakyll", "mtl"]
    , ListTupleEntry "Linux" ["Bash", "Debian", "Ubuntu", "NixOS", "Arch Linux"]
    , ListTupleEntry "Docker" ["Docker Compose"]
    ]

enOtherCtx :: Context String
enOtherCtx = constField "back" "Go back to Résumé"
    <> constField "otherTitle" "Other achievements"
    <> constField "otherDescr" ("I have participated in several STEM-Competitions as well as having participated in other relevant events."
                    ++ "A listing of these participations is given here.")
    <> constField "otherLangText" "You can visit the German version of this listing <a href=\"/cv/de/other.html\">here</a>."
    <> constField "otherPricesSection" "Other prices and awards"
    <> constField "year" "Year"
    <> constField "price" "Price/Award"
    <> listField "prices" teCtx (return enPrices)
    <> constField "eventsSection" "Workshops / Further training / Events"
    <> constField "event" "Workshop / Further training / Event"
    <> listField "events" teCtx (return enEvents)
    <> constField "competitionSection" "STEM-Competitions"
    <> constField "competitionLabel" "Competition"
    <> constField "projectLabel" "Team/Project"
    <> constField "placementLabel" "Placement/Price"
    <> listField "competitions" competitionCtx (return enCompetitions)
    <> defaultContext

enPrices :: [Item TupleEntry]
enPrices = map (Item "") 
    [ TupleEntry "2017" "Förderpreis der Wirtschaft (Price given by local companies) in recognition of special school achievements in the natural science subjects"
    , TupleEntry "2018" "Förderpreis der Wirtschaft (Price given by local companies) in recognition of special school achievements in the natural science subjects"
    , TupleEntry "2018" "Einstein-OWL (research competition) Award for a research project"
    ]

enEvents :: [Item TupleEntry]
enEvents = map (Item "") 
    [ TupleEntry "2017" "Students Academy of Mathematics (Schülerakademie der Mathematik SAM-OWL)"
    , TupleEntry "2018" "Workshop - Creation of a responsive website in a team"
    ]


enCompetitions :: [Item Competition]
enCompetitions = map (Item "")
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

