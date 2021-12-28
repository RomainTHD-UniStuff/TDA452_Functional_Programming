{- Lab 2
   Date: 15/11/2021
   Authors: Fanny Rouvel - Romain Theodet
   Lab group: 27
 -}

module BlackJack_B where
import Cards
import RunGame
import BlackJack_A
import Test.QuickCheck
import System.Random

-- B1

-- Concatenate two hands
(<+) :: Hand -> Hand -> Hand
(<+) Empty h2 = h2
(<+) (Add card h1) h2 = Add card (h1 <+ h2)

-- Property for quickcheck, concatenation associativity
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1 <+ (p2 <+ p3) == (p1 <+ p2) <+ p3

-- Property for quickcheck, top of
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf handA handB = size (handA <+ handB) == size handA + size handB

-- B2

-- Generate the numeric cards for a suit
numericSuit :: Integer -> Suit -> Hand
numericSuit 1 _ = Empty
numericSuit n s = Add (Card (Numeric n) s) (numericSuit (n - 1) s)

-- Generate a full suit
fullSuit :: Suit -> Hand
fullSuit s = Add (Card Ace s)
            (Add (Card King s)
            (Add (Card Queen s)
            (Add (Card Jack s)
            (numericSuit 10 s))))

-- Generate a deck
fullDeck :: Hand
fullDeck = fullSuit Hearts
        <+ fullSuit Spades
        <+ fullSuit Diamonds
        <+ fullSuit Clubs

-- B3

-- Draw a card
draw :: Hand -> Hand -> (Hand, Hand)
draw (Add card deck) hand = (deck, Add card hand)
draw Empty           _    = error "draw: The deck is empty."

-- B4

-- Play as the bank
playBank :: Hand -> Hand
playBank deck = playBankHelper deck Empty

-- Helper of playBank
playBankHelper :: Hand -> Hand -> Hand
playBankHelper deck hand | value biggerHand >= 16 = biggerHand
                         | otherwise = playBankHelper smallerDeck biggerHand
    where (smallerDeck, biggerHand) = draw deck hand

-- B5

-- Helper to remove a card
remove' :: Hand -> Integer -> Hand -> (Card, Hand)
remove' before 1 (Add card hand) = (card, before <+ hand)
remove' before n (Add card hand) = remove' (Add card before) (n - 1) hand

-- Remove a card from a hand
remove :: Integer -> Hand -> (Card, Hand)
remove = remove' Empty

-- Helper to shuffle a deck
generateShuffledDeck :: StdGen -> Hand -> Hand -> Hand
generateShuffledDeck g newDeck Empty   = newDeck
generateShuffledDeck g newDeck oldDeck =
    generateShuffledDeck g' (Add card newDeck) smallerDeck
    where (n, g') = randomR (1, size oldDeck) g
          (card, smallerDeck) = remove n oldDeck

-- Shuffle a deck
shuffleDeck :: StdGen -> Hand -> Hand
shuffleDeck g = generateShuffledDeck g Empty

-- Property for quickcheck, same card after shuffle
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffleDeck g h

-- Property for quickcheck, belongs to shuffled deck
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

-- Property for quickcheck, same deck size after shuffle
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g hand = size hand == size (shuffleDeck g hand)

-- B6

-- Implementation
implementation = Interface {
    iFullDeck = fullDeck,
    iValue    = value,
    iDisplay  = display,
    iGameOver = gameOver,
    iWinner   = winner,
    iDraw     = draw,
    iPlayBank = playBank,
    iShuffle  = shuffleDeck
}

-- Main function
main :: IO ()
main = runGame implementation
