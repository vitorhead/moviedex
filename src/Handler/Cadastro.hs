{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Cadastro where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import Utils
import Autenticacao 
import Data.Time
import Control.Monad.IO.Class


postCadastroR :: Handler TypedContent
postCadastroR = do
    cad <- requireJsonBody :: Handler Cadastros
    idCad <- runDB $ insert cad
    sendStatusJSON created201 $ Retorno 0 (toJSON $ fromSqlKey idCad)
    
    
deleteApagarCadR :: CadastrosId -> Handler TypedContent
deleteApagarCadR idCad = do
    _ <- runDB $ get404 idCad --verificando se ta na base; caso nao esteja volta 404
    runDB $ delete idCad
    sendStatusJSON noContent204 $ Retorno 0 "Deletado com sucesso!"
        
putAlterarCadR :: CadastrosId -> Handler TypedContent
putAlterarCadR idCad = do
    _ <- runDB $ get404 idCad
    newCad <- requireJsonBody :: Handler Cadastros
    runDB $ replace idCad newCad
    sendStatusJSON created201 $ Retorno 0 "Alterado com sucesso!"
    
getBuscaCadR :: CadastrosId -> Handler TypedContent
getBuscaCadR idCad = do
    cad <- runDB $ get404 idCad
    -- sendStatusJSON ok200 $ Retorno 0 (show cad)
    sendStatusJSON ok200 $ (toJSON cad)

getLoginUsuarioR :: Text -> Text -> Handler TypedContent
getLoginUsuarioR login senha = do
    buscaCad <- runDB $ getBy (UniqueEmail login)
    case buscaCad of
        Nothing ->  sendStatusJSON notFound404 $ Retorno 100 $ object ( ["resp" .= (show login ++" Não encontrado!"),
                                                                      "idcadastro" .= (show ""),
                                                                      "autenticacao" .= (show "")] )
        
        --liftIO : transformer // utilizado por causa do ActionT                                                              
        Just (Entity idcad _) -> do
                                    auth <- liftIO $ stringRandom 100 
                                    --getCurrentTime volta uma IO UTCTTime... liftIO pra trabalhar com o MonadT
                                    dtNow <- liftIO getCurrentTime
                                    idAuth <- runDB $ insert (Autenticacao (pack auth) idcad dtNow)
                                    sendStatusJSON ok200 $ Retorno 0 $ object ( ["resp" .= (show ""),
                                                                                 "idcadastro" .= (fromSqlKey idcad),
                                                                                 "autenticacao" .= (show auth)] )
                    


-- -- Define our data that will be used for creating the form.
-- data FileForm = FileForm
--     { fileInfo :: FileInfo
--     , fileDescription :: Text
--     }

-- -- This is a handler function for the GET request method on the HomeR
-- -- resource pattern. All of your resource patterns are defined in
-- -- config/routes
-- --
-- -- The majority of the code you will write in Yesod lives in these handler
-- -- functions. You can spread them across multiple files if you are so
-- -- inclined, or create a single monolithic file.
-- getHomeR :: Handler Html
-- getHomeR = do
--     (formWidget, formEnctype) <- generateFormPost sampleForm
--     let submission = Nothing :: Maybe FileForm
--         handlerName = "getHomeR" :: Text
--     defaultLayout $ do
--         let (commentFormId, commentTextareaId, commentListId) = commentIds
--         aDomId <- newIdent
--         setTitle "Welcome To Yesod!"
--         $(widgetFile "homepage")

-- postHomeR :: Handler Html
-- postHomeR = do
--     ((result, formWidget), formEnctype) <- runFormPost sampleForm
--     let handlerName = "postHomeR" :: Text
--         submission = case result of
--             FormSuccess res -> Just res
--             _ -> Nothing

--     defaultLayout $ do
--         let (commentFormId, commentTextareaId, commentListId) = commentIds
--         aDomId <- newIdent
--         setTitle "Welcome To Yesod!"
--         $(widgetFile "homepage")

-- sampleForm :: Form FileForm
-- sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
--     <$> fileAFormReq "Choose a file"
--     <*> areq textField textSettings Nothing
--     -- Add attributes like the placeholder and CSS classes.
--     where textSettings = FieldSettings
--             { fsLabel = "What's on the file?"
--             , fsTooltip = Nothing
--             , fsId = Nothing
--             , fsName = Nothing
--             , fsAttrs =
--                 [ ("class", "form-control")
--                 , ("placeholder", "File description")
--                 ]
--             }

-- commentIds :: (Text, Text, Text)
-- commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")
