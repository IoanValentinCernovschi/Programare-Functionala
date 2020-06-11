import System.Exit
import Control.Monad.Random
import Data.List

data CardRank = Normal Int | Jack | Queen | King | Ace deriving (Show, Eq, Ord)
data CardType = Spades | Hearts | Diamonds | Clubs deriving (Show, Eq)
type Card = (CardType, CardRank)

--caut pe hoogle o functie de tip [a] -> [b] -> [(a, b)]
cards = [(Normal 2), (Normal 3), (Normal 4), (Normal 5), (Normal 6), (Normal 7), (Normal 8), (Normal 9), (Normal 10), Jack, Queen, King, Ace]
cardTypes = [Spades, Hearts, Diamonds, Clubs]
deck = getDeck

main :: IO ()
main = 
    do
    putStrLn "What's your name?"
    name <- getLine
    putStrLn  ("Hi " ++ name ++ "! Are you ready to play War?")
    checkPlayerStatus

checkPlayerStatus :: IO ()
checkPlayerStatus = 
                  do
                  response <- getLine
                  if (response == "no" || response == "No" || response == "NO") then endSuccess 
                     else
                         if (response == "yes" || response == "Yes" || response == "YES" ) 
                          then 
                            play
                            else
                                endFailure

play :: IO ()
play = 
  do
    putStrLn $"Perfect. Let's go"
    let userDeck = getUserDeck giveRandomIndexes
    let computerDeck = getComputerDeck userDeck
    war (userDeck, computerDeck)


endSuccess :: IO ()
endSuccess =
    do
    putStrLn "Bye."
    exitSuccess

endFailure :: IO ()
endFailure =
    do
    putStrLn "Sorry, I couldn't catch that."
    checkPlayerStatus

isItWar :: Card -> Card -> Bool
isItWar (cardType1, cardRank1) (cardType2, cardRank2) | cardRank1 == cardRank2 = True
                            | otherwise = False

compareCards :: Card -> Card -> Card
compareCards (cardType1, cardRank1) (cardType2, cardRank2) | cardRank1 < cardRank2 = (cardType2, cardRank2)
                                 | otherwise = (cardType1, cardRank1)

getNthCard :: Int -> Card
getNthCard x = deck !! x

showDeck :: IO ()
showDeck = print deck

giveRandomIndexes :: [Int]
giveRandomIndexes = (take 26 (nub (randomRs (0, 51 :: Int) (mkStdGen 201))))
                          
getUserDeck :: [Int] -> [Card]
getUserDeck [] = []
getUserDeck (hd : tl) = [getNthCard hd] ++ (getUserDeck tl)

getComputerDeck :: [Card] -> [Card]
getComputerDeck userDeck = deck \\ userDeck

getDeck :: [Card]
getDeck = [(cardType, cardRank) | cardType <- cardTypes, cardRank <- cards]

battle :: ([Card], [Card]) -> ([Card], [Card])
battle ([], (computerHead : computerTail)) = ([], (computerHead : computerTail))
battle ((userHead : userTail), []) = ((userHead : userTail), []) 
battle ((userHead : userTail), (computerHead : computerTail)) | (isItWar userHead computerHead == True) = draw3 ((userHead : userTail), (computerHead : computerTail)) 0 (battle3 (userTail, computerTail) 0)  
                                                              | compareCards userHead computerHead == userHead = ((userTail ++ [userHead, computerHead]), computerTail)
                                                              | compareCards userHead computerHead == computerHead = (userTail, (computerTail ++ [computerHead, userHead]))

war :: ([Card], [Card]) -> IO ()
war ([], computerDeck) = putStrLn "You lost :("
war (userDeck, []) = putStrLn "You won :)" 
war ((userHead : userTail), (computerHead : computerTail)) = do
  putStrLn "Press Enter to continue..."
  n <- getChar
  printRound (userHead : userTail) (computerHead : computerTail)
  war (battle ((userHead : userTail), (computerHead : computerTail)))

battle3 :: ([Card], [Card]) -> Int -> Int 
battle3 ([], (computerHead : computerTail)) x = 2
battle3 ((userHead : userTail), []) x = 1
battle3 ((userHead : userTail), (computerHead : computerTail)) x | (x == 3) && (isItWar userHead computerHead) == True = battle3 (userTail, computerTail) 0
                                                                 | (x == 3) && ((compareCards userHead computerHead) == userHead) = 1
                                                                 | (x == 3) && ((compareCards userHead computerHead) == computerHead) = 2
                                                                 | otherwise = battle3 (userTail, computerTail) (x + 1)  

draw3 :: ([Card], [Card]) -> Int -> Int -> ([Card], [Card])
draw3 ([], (computerHead : computerTail)) x y = ([], (computerHead : computerTail))
draw3 ((userHead : userTail), []) x y = ((userHead : userTail), [])
draw3 ((userHead : userTail), (computerHead : computerTail)) x y | (x < 3 ) && (y == 1) = draw3 ((userTail ++ [userHead, computerHead]), computerTail) (x + 1) 1
                                                                 | (x < 3) && (y == 2) = draw3 (userTail, (computerTail ++ [computerHead, userHead])) (x + 1) 2
                                                                 | otherwise = ((userHead : userTail), (computerHead : computerTail)) 

showCard :: Card -> String
showCard (cardType, Normal x) = (show x) ++ " of " ++ (show cardType)
showCard (cardType, cardRank) = (show cardRank) ++ " of " ++ (show cardType)

printRound :: [Card] -> [Card] -> IO ()
printRound (userHead : userTail) (computerHead : computerTail) = 
  if (isItWar userHead computerHead == True ) then
    do
      putStrLn $ "You: " ++ (showCard userHead) 
      putStrLn $ "Bob: " ++ (showCard computerHead) 
      if (battle3 (userTail, computerTail) 0 == 1) then 
        putStrLn $ "Round Winner: " ++ " It's a war won by you! \n"
      else
        putStrLn $ "Round Winner: " ++ " It's a war won by Bob! \n"
  else
    if (compareCards userHead computerHead) == userHead then
      do
        putStrLn $ "You: " ++ (showCard userHead) 
        putStrLn $ "Bob: " ++ (showCard computerHead)
        putStrLn $ "Round Winner: " ++ "You \n"
    else 
      do
        putStrLn $ "You: " ++ (showCard userHead) 
        putStrLn $ "Bob: " ++ (showCard computerHead)
        putStrLn $ "Round Winner: " ++ "Bob \n"

