--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import Hakyll.Web.Sass


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "assets/images/*" $ do
        route idRoute
        compile copyFileCompiler

    match "assets/css/*.css" $ do
        route idRoute
        compile compressCssCompiler
    
    match "assets/js/*.js" $ do
        route idRoute
        compile copyFileCompiler

    scssDependency <- makePatternDependency "assets/css/**.scss"
    rulesExtraDependencies [scssDependency]
        $ match "assets/css/default.scss"
        $ do
            route $ setExtension "css"
            compile (fmap compressCss <$> sassCompiler)

    match "sites/*.md" $ do
        route $ gsubRoute "sites/" (const "") `composeRoutes` setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "sites/*.html" $ do
        route $ gsubRoute "sites/" (const "")
        compile $ getResourceBody
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
