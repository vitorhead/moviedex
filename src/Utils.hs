{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}
module Utils where

import Database.Persist.TH
import GHC.Generics
import Data.Aeson
import Data.Time.Calendar

data Sexo = M | F 
    deriving (Show, Read, Eq, Generic)
derivePersistField "Sexo"
instance ToJSON Sexo 
instance FromJSON Sexo


data Retorno = Retorno {codigo :: Int , mensagem :: Value} 
    deriving (Show, Read, Eq, Generic)
derivePersistField "Retorno"
instance ToJSON Retorno
instance FromJSON Retorno

juntaLista :: [a] -> [a] -> [a]
juntaLista [] ys = ys
juntaLista xs [] = xs
juntaLista (x:xs) (y:ys) = x : y : juntaLista xs ys


-- data Cadastro = Cadastro {nome :: String, dtNascimento :: Day, sexo  :: Sexo} deriving (Show, Read, Eq, Generic)
-- instance ToJSON Cadastro
-- instance FromJSON Cadastro