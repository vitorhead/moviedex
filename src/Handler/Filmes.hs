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
postFilmesR = undefined