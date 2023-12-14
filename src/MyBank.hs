module MyBank where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

import Control.Concurrent.STM

type Currency = String
type Amount = Integer
type Account = (Currency, Amount)
type User = (String, [Account])
type Bank = TVar [User]

registerUser :: Bank -> String -> STM ()
registerUser bank name = do
  users <- readTVar bank
  writeTVar bank $ (name, []) : users

deleteUser :: Bank -> String -> STM ()
deleteUser bank name = do
  users <- readTVar bank
  writeTVar bank $ filter (\(n, _) -> n /= name) users

openAccount :: Bank -> String -> Currency -> Amount -> STM ()
openAccount bank name currency amount = do
  users <- readTVar bank
  let updatedUsers = map (\(n, accounts) -> if n == name then (n, (currency, amount) : accounts) else (n, accounts)) users
  writeTVar bank updatedUsers

closeAccount :: Bank -> String -> Currency -> STM ()
closeAccount bank name currency = do
  users <- readTVar bank
  let updatedUsers = map (\(n, accounts) -> if n == name then (n, filter (\(c, _) -> c /= currency) accounts) else (n, accounts)) users
  writeTVar bank updatedUsers

deposit :: Bank -> String -> Currency -> Amount -> STM ()
deposit bank name currency amount = do
  users <- readTVar bank
  let updatedUsers = map (\(n, accounts) -> if n == name then (n, updateAccount accounts) else (n, accounts)) users
  writeTVar bank updatedUsers
  where
    updateAccount [] = [(currency, amount)]
    updateAccount ((c, a) : rest)
      | c == currency = (c, a + amount) : rest
      | otherwise = (c, a) : updateAccount rest

withdraw :: Bank -> String -> Currency -> Amount -> STM ()
withdraw bank name currency amount = do
  users <- readTVar bank
  let updatedUsers = map (\(n, accounts) -> if n == name then (n, updateAccount accounts) else (n, accounts)) users
  writeTVar bank updatedUsers
  where
    updateAccount [] = []
    updateAccount ((c, a) : rest)
      | c == currency && a >= amount = (c, a - amount) : rest
      | otherwise = (c, a) : updateAccount rest

transfer :: Bank -> String -> String -> Currency -> Amount -> STM ()
transfer bank fromUser toUser currency amount = do
  users <- readTVar bank
  let updatedUsers = map (\(n, accounts) -> if n == fromUser then (n, updateAccount accounts) else if n == toUser then (n, updateAccount' accounts) else (n, accounts)) users
  writeTVar bank updatedUsers
  where
    updateAccount [] = []
    updateAccount ((c, a) : rest)
      | c == currency && a >= amount = (c, a - amount) : rest
      | otherwise = (c, a) : updateAccount rest
    updateAccount' [] = [(currency, amount)]
    updateAccount' ((c, a) : rest)
      | c == currency = (c, a + amount) : rest
      | otherwise = (c, a) : updateAccount' rest

transferBetweenUsers :: Bank -> String -> String -> Currency -> Amount -> STM ()
transferBetweenUsers bank fromUser toUser currency amount = do
  users <- readTVar bank
  let updatedUsers = map (\(n, accounts) -> if n == fromUser then (n, updateAccount accounts) else if n == toUser then (n, updateAccount' accounts) else (n, accounts)) users
  writeTVar bank updatedUsers
  where
    updateAccount [] = []
    updateAccount ((c, a) : rest)
      | c == currency && a >= amount = (c, a - amount) : rest
      | otherwise = (c, a) : updateAccount rest
    updateAccount' [] = [(currency, amount)]
    updateAccount' ((c, a) : rest)
      | c == currency = (c, a + amount - commission) : rest
      | otherwise = (c, a) : updateAccount' rest
    commission = max 1 (amount `div` 100)
