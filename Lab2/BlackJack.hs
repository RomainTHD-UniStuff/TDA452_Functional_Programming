{- Lab 2
   Date: 04/11/2021
   Authors: Fanny Rouvel - Romain Theodet
   Lab group: 27
 -}

module BlackJack where
import Cards
import RunGame
import Test.QuickCheck

-- A1

-- This function displays a suit
displaySuit :: Suit -> String
displaySuit Hearts   = "\9829"
displaySuit Spades   = "\9824"
displaySuit Diamonds = "\9830"
displaySuit Clubs    = "\9827"

-- This function displays a rank
displayRank :: Rank -> String
displayRank (Numeric n) = show n
displayRank Jack        = "J"
displayRank Queen       = "Q"
displayRank King        = "K"
displayRank Ace         = "A"

-- This function displays a card
displayCard :: Card -> String
displayCard (Card r s) = (displayRank r) ++ (displaySuit s)

-- This function displays a hand
display :: Hand -> String
display Empty           = "\n"
display (Add card hand) = displayCard card ++ " " ++ display hand
