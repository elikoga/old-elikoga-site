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

    match "sites/index.html" $ do
        route $ gsubRoute "sites/" (const "")
        compile $ do
            let socialItems = map (\s -> Item "" s) socials
                indexCtx = listField "socials" socialCtx (return socialItems)

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

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

data SocialConnection -- Link + Icon / CopyString + Title + Icon
    = LinkTo String String
    | CopyString String String String

socials :: [SocialConnection]
socials =
    [ LinkTo "https://github.com/elikoga" "github"
    , LinkTo "https://gitlab.com/elikoga" "gitlab"
    , LinkTo "https://twitter.com/elikoga/" "twitter"
    , LinkTo "https://www.instagram.com/elikoga/" "instagram"
    , LinkTo "https://old.reddit.com/user/elikoga" "reddit"
    , LinkTo "https://t.me/coafin" "telegram"
    , CopyString "'elikoga#1802'" "my Discord username" "discord"
    , CopyString "'elikowa' + '@' + 'gmail.com'" "my e-mail-address" "envelope"
    ]

socialCtx :: Context SocialConnection
socialCtx = field "aTags" tagsFromSocial
    <> field "icon" iconFromSocial

tagsFromSocial :: Item SocialConnection -> Compiler String
tagsFromSocial (Item _id (LinkTo link _icon)) = return $
    "href=\"" <> link <> "\""
tagsFromSocial (Item _id (CopyString copyString titleString _icon)) = return $
    "href=\"#\" onclick=\"copyString("
    <> copyString
    <> ")\" data-toggle=\"tooltip\" data-placement=\"top\" title=\"Copies "
    <> titleString
    <> " to your clipboard\""

iconFromSocial :: Item SocialConnection -> Compiler String
iconFromSocial (Item _id (LinkTo _link icon)) = return $
    "fa fa-" <> icon
iconFromSocial (Item _id (CopyString _copyString _titleString icon)) = return $
    "fa fa-" <> icon