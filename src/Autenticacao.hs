module Autenticacao where

import System.Random
import Control.Applicative

--volta um listão hehe
chars :: String 
chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

--obrigatoriamente volta IO, doc System.Random
numRandom :: IO Int
numRandom = randomRIO (0, (length chars)-1)

--ix = numero qualquer. chars = listão dos caracteres
--pega um char qualquer naquela lista 
charRandom :: IO Char
charRandom = do
    ix <- numRandom
    return $ chars !! ix


--replicate 4 1 [1,1,1,1]
--replicate tam 1 = [1..tam] 
--replicate tam charRandom = n*charRandom
--sequence pela monad
stringRandom :: Int -> IO String
stringRandom tam = sequence $ replicate tam charRandom