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
displayCard (Card rank suit) = (displayRank rank) ++ (displaySuit suit)

-- This function displays a hand
display :: Hand -> String
display Empty           = "\n"
display (Add card hand) = displayCard card ++ " " ++ display hand

-- A2

-- Card value, assuming aces having a value 11
cardValue :: Card -> Integer
cardValue (Card (Numeric n) _) = n
cardValue (Card Ace         _) = 11
cardValue (Card _           _) = 10

-- Initial "basic" value of a hand, assuming aces having a value 11
initialValue :: Hand -> Integer
initialValue Empty           = 0
initialValue (Add card hand) = cardValue card + initialValue hand

-- Number of aces in a hand
numberOfAces :: Hand -> Integer
numberOfAces Empty                   = 0
numberOfAces (Add (Card Ace _) hand) = numberOfAces hand + 1
numberOfAces (Add (Card _   _) hand) = numberOfAces hand

-- Calculates the "true" value of a hand, by replacing 11-aces by 1-aces if
--  required, removing 10 to the value for each overflowing ace
valueCalc :: Integer -> Integer -> Integer
valueCalc value 0                 = value
valueCalc value aces | value > 21 = valueCalc (value - 10) (aces - 1)
                     | otherwise  = value

-- Value of a hand
value :: Hand -> Integer
value hand = valueCalc (initialValue hand) (numberOfAces hand)

-- Tests

handUnder21 = Add (Card (Numeric 8) Hearts) (Add (Card Ace Spades) Empty)
handOver21  = Add (Card (Numeric 8) Hearts) (Add (Card Ace Spades) (Add (Card King Clubs) Empty))
