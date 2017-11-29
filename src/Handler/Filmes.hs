{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Filmes where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import Utils


postFilmesR :: Handler TypedContent
postFilmesR = do
    film <- requireJsonBody :: Handler Filmes
    idFilm <- runDB $ insert film
    sendStatusJSON created201 $ Retorno 0 (toJSON $ fromSqlKey idFilm)
    
getConsultaFilmesR  :: Int -> Handler TypedContent
getConsultaFilmesR idApi  = do
    filme <- runDB $ selectList [FilmesIdapi ==. idApi] []   
    case filme of
        [] -> sendStatusJSON notFound404 $ object(["resp" .= (show "Não encontrado")])
        [x] -> sendStatusJSON created201 $ toJSON x
    