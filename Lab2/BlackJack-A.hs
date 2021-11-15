{- Lab 2
   Date: 04/11/2021
   Authors: Fanny Rouvel - Romain Theodet
   Lab group: 27
 -}

module BlackJack where
import Cards
import RunGame
import Test.QuickCheck

-- A0

sizeSteps :: [Integer]
sizeSteps = [ size hand2
            , size (Add (Card (Numeric 2) Hearts)
                        (Add (Card Jack Spades) Empty))
            , 1 + size (Add (Card Jack Spades) Empty)
            , 2 + size Empty
            ,2]
  where hand2 = Add (Card (Numeric 2) Hearts)
            (Add (Card Jack Spades) Empty)

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

-- Rank value, assuming aces having a value 11
valueRank :: Rank -> Integer
valueRank (Numeric n) = n
valueRank Ace         = 11
valueRank _           = 10

-- Initial "basic" value of a hand, assuming aces having a value 11
initialValue :: Hand -> Integer
initialValue Empty           = 0
initialValue (Add (Card rank _) hand) = valueRank rank + initialValue hand

-- Number of aces in a hand
numberOfAces :: Hand -> Integer
numberOfAces Empty                   = 0
numberOfAces (Add (Card Ace _) hand) = numberOfAces hand + 1
numberOfAces (Add (Card _   _) hand) = numberOfAces hand

-- Calculates the "true" value of a hand, by replacing 11-aces by 1-aces if the
--  current value is above 21, removing 10 to the value for each overflowing ace
valueCalc :: Integer -> Integer -> Integer
valueCalc value 0                 = value
valueCalc value aces | value > 21 = valueCalc (value - 10) (aces - 1)
                     | otherwise  = value

-- Value of a hand
value :: Hand -> Integer
value hand = valueCalc (initialValue hand) (numberOfAces hand)

-- A3

-- Player bust or not
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- A4

-- Gives the winner of a game
winner :: Hand -> Hand -> Player
winner handGuest handBank | gameOver handGuest = Bank
                          | gameOver handBank = Guest
                          | value handGuest > value handBank = Guest
                          | otherwise = Bank
