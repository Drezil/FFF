module Handler.Show where

import Import

getShowR :: Handler Html
getShowR = do
        res <- runDB $ selectList [] []
        defaultLayout [whamlet|
$forall (Entity _ (Post n g l lat lng d)) <- res
  <div class="panel panel-default">
    <div class="panel-heading">
      <h3 class="panel-title">#{n}
    <div class="panel-body">
      <span .label .label-default>Gender: #{g}
      <span .label .label-default>Looks for: #{l}
      <span .label .label-default>Latitude: #{lat}
      <span .label .label-default>Longitude: #{lng}
      <p>#{d}
|]

postShowR :: Handler Html
postShowR = error "Not yet implemented: postShowR"
