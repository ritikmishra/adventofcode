module Day21 where

import Control.Monad.State (State, execState, get, put, runState, evalState)
import GHC.Show (Show)
import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Control.Monad (when)

data Dice = Dice {lastRolled :: Int, numRolls :: Int} deriving (Show, Eq)

data Player = Player {pos :: Int, score :: Int} deriving (Show, Eq)

winner :: Player -> Bool
winner = (>= 1000) . score

type GameState = (Player, Player, Dice)

initState :: (Int, Int) -> GameState
initState (a, b) = (Player {pos = a, score = 0}, Player {pos = b, score = 0}, Dice {lastRolled = 0, numRolls = 0})

rollDice :: State GameState Int
rollDice = do
  (a, b, cur) <- get
  let new = if lastRolled cur == 100 then 1 else lastRolled cur + 1
  put (a, b, Dice {lastRolled = new, numRolls = 1 + numRolls cur})
  return new

advanceAroundBoard :: Int -> Int -> Int
advanceAroundBoard pos delta =
  (pos - 1 + delta) `mod` 10 + 1

playOneRound :: State GameState ()
playOneRound = do
  a1 <- rollDice
  a2 <- rollDice
  a3 <- rollDice
  (a, b, d) <- get
  let newApos = advanceAroundBoard (pos a) (a1 + a2 + a3)
  let newAscore = newApos + score a
  put (Player {pos = newApos, score = newAscore}, b, d)

  when (newAscore < 1000) $ do
    b1 <- rollDice
    b2 <- rollDice
    b3 <- rollDice
    (a', b', d') <- get
    let newBpos = advanceAroundBoard (pos b') (b1 + b2 + b3)
    put (a', Player {pos = newBpos, score = newBpos + score b'}, d')
    return ()

playUntilWinner :: State GameState Int
playUntilWinner = do
  playOneRound
  (a, b, d) <- get
  if winner a
    then return $ score b * numRolls d
    else
      if winner b
        then return $ score a * numRolls d
        else playUntilWinner

day21main :: IO ()
day21main = do
  putStrLn "Day 21 Part 1:"
  print $ evalState playUntilWinner (initState (8,7))