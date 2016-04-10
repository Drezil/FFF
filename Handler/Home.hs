module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "Find Fellow Friends!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    defaultLayout $ do
        setTitle "Find Fellow Friends!"
        $(widgetFile "homepage")
