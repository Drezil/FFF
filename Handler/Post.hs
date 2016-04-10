module Handler.Post where

import Import
import Yesod.Form.Bootstrap3
import Text.Blaze.Internal (Markup)

data RawPost = RawPost
               { rPname :: Text
               , rPgender :: Text
               , rPlookingFor :: Text
               , rPlat :: Text
               , rPlng :: Text
               , rpdesc :: Text
               , user :: UserId
               }

getPostR :: Handler Html
getPostR = do
        uid <- requireAuthId
        (widget,enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm (postForm uid)
        defaultLayout $ [whamlet|
<form method="post" enctype=#{enctype}>
    ^{widget}
    <p>
  <button type="submit" class="btn btn-default" role="button">Post! »
|]

postPostR :: Handler Html
postPostR = do
        uid <- requireAuthId
        ((res,widget),enctype) <- runFormPost (validPostForm uid)
        case res of
          FormSuccess p -> do
                             runDB $ insert_ p
                             redirect ShowR
          _ ->
                        defaultLayout $ [whamlet|
<form method="post" enctype=#{enctype}>
    ^{widget}
    <p>
  <button type="submit" class="btn btn-default" role="button">Post! »
|]

validPostForm :: UserId -> Markup -> MForm Handler (FormResult Post, Widget)
validPostForm uid html = do
        (res, widget) <- renderBootstrap3 BootstrapBasicForm (postForm uid) html
        return $ case res of
                   FormSuccess r -> convert r
                      where
                        msg = "Invalid coordinates" :: Text
                        convert (RawPost n g l lat lng d u) = case (,) <$> convlat lat <*> convlng lng of
                                        Just (lat', lng') -> (FormSuccess (Post n g l lat' lng' d u), widget)
                                        Nothing -> (FormFailure [msg], [whamlet|
                                                <p .errors>#{msg}
                                                ^{widget}
                                                |])
                   _ -> (convWrong <$> res, widget)

convlat :: Text -> Maybe Double
convlat c = readMay c
            --TODO: add parser for 51ˇ21'22''N-Format


convlng :: Text -> Maybe Double
convlng c = readMay c
            --TODO: add parser for 51ˇ21'22''N-Format

convWrong :: RawPost -> Post
convWrong (RawPost n g l _ _ d u) = Post n g l 0 0 d u

postForm :: UserId -> AForm Handler RawPost
postForm uid = RawPost
        <$> areq textField (withAutofocus $ withPlaceholder "Bernd" $ bfs ("Nickname" :: Text)) Nothing
        <*> areq textField (withPlaceholder "unicorn" $ bfs ("Gender" :: Text)) Nothing
        <*> areq textField (withPlaceholder "anyone" $ bfs ("Looking for..." :: Text)) Nothing
        <*> areq textField (withPlaceholder "Latitude" $ bfs ("Latitude" :: Text)) Nothing
        <*> areq textField (withPlaceholder "Longitude" $ bfs ("Longitude" :: Text)) Nothing
        <*> (unTextarea <$> areq textareaField (withPlaceholder "I want some choclate... and candy.." $ bfs ("Description" :: Text)) Nothing)
        <*> pure uid

