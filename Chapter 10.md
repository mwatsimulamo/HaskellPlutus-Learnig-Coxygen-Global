module Main where

-- Définition du type
data PaymentMethod = Cash | Card | Crypto

-- Définition de la classe ShowSimple
class ShowSimple a where
  showSimple :: a -> String

-- Instance pour PaymentMethod
instance ShowSimple PaymentMethod where
  showSimple Cash = "Cash Payment"
  showSimple Card = "Card Payment"
  showSimple Crypto = "Crypto Payment"

-- Nouveau type
data Studentpay = FrancsCongolais | Dollars | MobileMoney

-- Classe ShowOky
class ShowOky a where
  showOky :: a -> Bool

-- Instance pour Studentpay
instance ShowOky Studentpay where
  showOky FrancsCongolais = True
  showOky Dollars = True
  showOky MobileMoney = True
-- Instance pour Int (exemple : on accepte seulement > 0)
instance ShowOky Int where
  showOky x = x > 0

-- Petit test
main :: IO ()
main = do
  putStrLn (showSimple Cash)
  putStrLn (showSimple Card)
  putStrLn (showSimple Crypto)
  print (showOky FrancsCongolais) -- True
  print (showOky Dollars) -- True
  print (showOky MobileMoney) -- True
