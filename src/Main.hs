{-# LANGUAGE OverloadedStrings #-}
module Main where 

import Social
import Cv

import Hakyll
import Hakyll.Web.Sass
import GHC.IO.Encoding

main :: IO ()
main =  (setLocaleEncoding utf8 >>) $ hakyll $ do
    match "assets/images/**" $ do
        route idRoute
        compile copyFileCompiler

    match "assets/css/**.css" $ do
        route idRoute
        compile compressCssCompiler
    
    match "assets/js/**.js" $ do
        route idRoute
        compile copyFileCompiler

    scssDependency <- makePatternDependency "assets/css/**.scss"
    rulesExtraDependencies [scssDependency]
        $ match "assets/css/default.scss"
        $ do
            route $ setExtension "css"
            compile (fmap compressCss <$> sassCompiler)

    match "sites/index.html" $ do
        route $ gsubRoute "sites/" (const "")
        compile $ do
            let socialItems = map (Item "") socials
                indexCtx = listField "socials" socialCtx (return socialItems)

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    match "sites/cv/cv.html" $ version "de" $ do
        route $ gsubRoute "sites/cv/cv" (const "cv/de/index")
        compile $ getResourceBody
            >>= applyAsTemplate deCvCtx
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls
                
    match "sites/cv/cv.html" $ version "dePrint" $ do
        route $ gsubRoute "sites/cv/cv" (const "cv/de/print")
        compile $ getResourceBody
            >>= (\(Item ident s) 
                -> return . Item ident $("<script src=\"https://unpkg.com/pagedjs/dist/paged.polyfill.js\"></script>" ++ s))
            >>= applyAsTemplate deCvCtx
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "sites/cv/cv.html" $ version "en" $ do
        route $ gsubRoute "sites/cv/cv" (const "cv/en/index")
        compile $ getResourceBody
            >>= applyAsTemplate enCvCtx
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "sites/cv/cv.html" $ version "enPrint" $ do
        route $ gsubRoute "sites/cv/cv" (const "cv/en/print")
        compile $ getResourceBody
            >>= (\(Item ident s) 
                -> return . Item ident $("<script src=\"https://unpkg.com/pagedjs/dist/paged.polyfill.js\"></script>" ++ s))
            >>= applyAsTemplate enCvCtx
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "sites/cv/other.html" $ version "de" $ do
        route $ gsubRoute "sites/cv/" (const "cv/de/")
        compile $ getResourceBody
            >>= applyAsTemplate deOtherCtx
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "sites/cv/other.html" $ version "en" $ do
        route $ gsubRoute "sites/cv/" (const "cv/en/")
        compile $ getResourceBody
            >>= applyAsTemplate enOtherCtx
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "sites/**.md" $ do
        route $ gsubRoute "sites/" (const "") `composeRoutes` setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "sites/**.html" $ do
        route $ gsubRoute "sites/" (const "")
        compile $ getResourceBody
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    match "templates/**" $ compile templateBodyCompiler

