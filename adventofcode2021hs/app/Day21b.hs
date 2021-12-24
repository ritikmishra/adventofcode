module Day21b where

import Control.Monad.State (State, get, put, runState)
import qualified Data.Map as Map
import Day21 (advanceAroundBoard)
import Debug.Trace (trace)

data Player = Player
  { pos :: Int,
    score :: Int
  }
  deriving (Show, Eq, Ord)

type GameState = (Bool, Map.Map (Player, Player) Integer)

initState :: Int -> Int -> GameState
initState aPos bPos = (True, Map.fromList [((Player {pos = aPos, score = 0}, Player {pos = bPos, score = 0}), 1)])

advancePlayerDirac :: GameState -> GameState
advancePlayerDirac (curPlayerIsA, curMap) =
  let nextMap =
        Map.foldrWithKey
          ( \(a, b) val acc ->
              let (movingPlayer@Player {pos = pos, score = score}, staticPlayer) = if curPlayerIsA then (a, b) else (b, a)
               in let newMovingPlayers =
                        [ let newPos = advanceAroundBoard pos (d1 + d2 + d3)
                           in Player {pos = newPos, score = score + newPos}
                          | d1 <- [1 .. 3],
                            d2 <- [1 .. 3],
                            d3 <- [1 .. 3]
                        ]
                   in let staticPlayers = staticPlayer : staticPlayers
                       in let newPlayers = if curPlayerIsA then zip newMovingPlayers staticPlayers else zip staticPlayers newMovingPlayers
                    -- in let newPlayers = trace (show $ length newPlayers') newPlayers'
                           in foldl (\acc' p' -> Map.insertWith (+) p' val acc') acc newPlayers
          )
          Map.empty
          curMap
   in let nextPlayer = not curPlayerIsA
       in (nextPlayer, nextMap)

advanceUntilAllUniversesDone :: GameState -> (Integer, Integer)
advanceUntilAllUniversesDone gs@(_, unimap) =
  if null unimap
    then (0, 0)
    else
      let gs'@(turn, unimap) = advancePlayerDirac gs
       in let (unimap'', (a', b')) =
                Map.foldrWithKey
                  ( \p@(a, b) val (unimap', c@(aWinsCount, bWinsCount)) ->
                      case (score a >= 21, score b >= 21) of
                        (True, True) -> error "not allowed"
                        (True, False) -> (unimap', (aWinsCount + val, bWinsCount))
                        (False, True) -> (unimap', (aWinsCount, bWinsCount + val))
                        (False, False) -> (Map.insert p val unimap', c)
                  )
                  (Map.empty, (0, 0))
                  unimap
           in let (a'', b'') = advanceUntilAllUniversesDone (turn, unimap'')
               in (a' + a'', b' + b'')

day21bmain :: IO ()
day21bmain = do
  putStrLn "Day 21 part 2:"
  print $ advanceUntilAllUniversesDone (initState 8 7)
