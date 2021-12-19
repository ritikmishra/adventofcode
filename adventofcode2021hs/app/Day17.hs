module Day17 where

import System.IO (IOMode (ReadMode), hGetContents, openFile)

data Probe = Probe
  { pos :: (Int, Int),
    vel :: (Int, Int)
  } deriving (Show, Eq)

stepProbe :: Probe -> Probe
stepProbe Probe {pos = (x, y), vel = (dx, dy)} =
  Probe
    { pos = newPos,
      vel = newVel
    }
  where
    newPos = (x + dx, y + dy)
    newVel =
      ( if dx < 0 then dx + 1 else if dx > 0 then dx - 1 else 0,
        dy - 1
      )

launchProbe :: (Int, Int) -> Probe
launchProbe vel =
  Probe
    { pos = (0, 0),
      vel = vel
    }

type Range = ((Int, Int), (Int, Int))

data TargetStatus = Coming | Landed | Missed deriving (Show, Eq)

whereIsPosComparedToRange :: Range -> (Int, Int) -> TargetStatus
whereIsPosComparedToRange ((xMin, xMax), (yMin, yMax)) (x, y)
  | (xMin <= x && x <= xMax) && (yMin <= y && y <= yMax) = Landed
  | x < xMax && y > yMin = Coming
  | otherwise = Missed

-- highestYPosOnTrajectory :: ((Int, Int) -> TargetStatus) -> (Int, Int) -> Maybe Int
highestYPosOnTrajectory didWeHit startingVel  =
  let launchedProbe = launchProbe startingVel
   in let probePath =
            takeWhile ((/= Missed) . fst) $
              map
                (\el -> (didWeHit $ pos el, pos el))
                $ scanl (\acc _ -> stepProbe acc) launchedProbe [1 ..]
       in if any ((== Landed) . fst) probePath
            then Just $ maximum $ map (\(_, (_x, y)) -> y) probePath
            else Nothing

day17part1 :: ((Int, Int) -> TargetStatus) -> Int
day17part1 didWeHit = 
    let velsToTry = [(dx, dy) | dx <- [0..100], dy <- [-100..3000]]
      in maximum [maxY | Just maxY <- map (highestYPosOnTrajectory didWeHit) velsToTry]

day17part2 :: ((Int, Int) -> TargetStatus) -> Int
day17part2 didWeHit = 
    let velsToTry = [(dx, dy) | dx <- [0..100], dy <- [-200..3000]]
      in length [maxY | Just maxY <- map (highestYPosOnTrajectory didWeHit) velsToTry]


day17main :: IO ()
day17main = do
  let whereAreWe = whereIsPosComparedToRange ((48, 70), (-189, -148))
--   let whereAreWe = whereIsPosComparedToRange ((20, 30), (-10, -5))
  putStr "Day 17 part 1: "
  print $ day17part1 whereAreWe
  putStr "Day 17 part 2: "
  print $ day17part2 whereAreWe
