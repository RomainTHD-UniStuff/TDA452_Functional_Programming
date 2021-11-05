{- Lab 2
   Date: 04/11/2021
   Authors: Fanny Rouvel - Romain Theodet
   Lab group: 27
 -}

module BlackJack where
import Cards
import RunGame
import Test.QuickCheck

displaySuit :: Suit -> String
displaySuit Hearts   = "\9829"
displaySuit Spades   = "\9824"
displaySuit Diamonds = "\9830"
displaySuit Clubs    = "\9827"

displayCard :: Card -> String
displayCard (Card (Numeric n) s) = show n ++ (displaySuit s)
displayCard (Card Jack s)        = "J"    ++ (displaySuit s)
displayCard (Card Queen s)       = "Q"    ++ (displaySuit s)
displayCard (Card King s)        = "K"    ++ (displaySuit s)
displayCard (Card Ace s)         = "A"    ++ (displaySuit s)

display :: Hand -> String
display Empty           = "\n"
display (Add card hand) = displayCard card ++ " " ++ display hand
