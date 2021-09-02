module Social where

import Hakyll

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
    "target=\"_blank\" href=\"" <> link <> "\""
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
