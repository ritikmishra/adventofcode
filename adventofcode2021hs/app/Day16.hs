module Day16 where
import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Control.Monad.State

import Utils (bitsToDecimal)

inputToBitStr :: String -> [Bool]
inputToBitStr = concatMap hexToBits
    where
        hexToBits :: Char -> [Bool]
        hexToBits '0' = [False, False, False, False]
        hexToBits '1' = [False, False, False, True]
        hexToBits '2' = [False, False, True, False]
        hexToBits '3' = [False, False, True, True]
        hexToBits '4' = [False, True, False, False]
        hexToBits '5' = [False, True, False, True]
        hexToBits '6' = [False, True, True, False]
        hexToBits '7' = [False, True, True, True]
        hexToBits '8' = [True, False, False, False]
        hexToBits '9' = [True, False, False, True]
        hexToBits 'A' = [True, False, True, False]
        hexToBits 'B' = [True, False, True, True]
        hexToBits 'C' = [True, True, False, False]
        hexToBits 'D' = [True, True, False, True]
        hexToBits 'E' = [True, True, True, False]
        hexToBits 'F' = [True, True, True, True]
        hexToBits other = error $ "Unsupported hex character: " ++ show other


data Packet = Packet {
    versionNumber :: Int,
    typeId :: Int,
    packetBody :: PacketBody
} deriving (Show, Eq)

data PacketBody = LiteralValue Int | OperatorPacket [Packet] deriving (Show, Eq)

grabNextBit :: State [Bool] Bool
grabNextBit = do
    cur <- get
    case cur of
        x:xs -> do
            put xs
            return x
        _ -> error "idk what to do"

grabNext :: Int -> State [Bool] [Bool]
grabNext n = do
    cur <- get
    let (a,b) = splitAt n cur
    put b
    return a

parsePacketHeader :: State [Bool] (Int, Int)
parsePacketHeader = do
    versionBits <- grabNext 3
    typeIdBits <- grabNext 3
    return (bitsToDecimal versionBits, bitsToDecimal typeIdBits)

grabLiteralPacketBytes :: State [Bool] [Bool]
grabLiteralPacketBytes = do
    nums <- grabNext 5
    case nums of
        True:rest -> do
            banana <- grabLiteralPacketBytes
            return $ rest ++ banana
        False:rest -> return rest
        [] -> return []

parseLiteralPacket :: State [Bool] Int
parseLiteralPacket = bitsToDecimal <$> grabLiteralPacketBytes

parseOperatorPacket :: State [Bool] [Packet]
parseOperatorPacket = do
    lengthId <- grabNextBit
    if lengthId then do
            numberSubpackets <- bitsToDecimal <$> grabNext 11
            parseNPackets numberSubpackets
        else do
            childPacketBitLength <- bitsToDecimal <$> grabNext 15
            parseNBitsOfPackets childPacketBitLength
    where
        parseNBitsOfPackets :: Int -> State [Bool] [Packet]
        parseNBitsOfPackets 0 = return []
        parseNBitsOfPackets n = do
            beforeBitLength <- gets length
            packet <- parsePacket
            afterBitLength <- gets length
            let delta = beforeBitLength - afterBitLength
            remainder <- parseNBitsOfPackets (n - delta)
            return $ packet:remainder
        parseNPackets :: Int -> State [Bool] [Packet]
        parseNPackets 0 = return []
        parseNPackets n = do
                first <- parsePacket
                rest <- parseNPackets (n - 1)
                return $ first:rest

parsePacket :: State [Bool] Packet
parsePacket = do
    (version, typeIdNum) <- parsePacketHeader
    case typeIdNum of
        4 -> do
            literalBody <- parseLiteralPacket
            let ret = Packet {
                versionNumber = version,
                typeId = typeIdNum,
                packetBody = LiteralValue literalBody
            }
            return ret
        _ -> do
            childPackets <- parseOperatorPacket
            return $ Packet {
                versionNumber = version,
                typeId = typeIdNum,
                packetBody = OperatorPacket childPackets
            }

day16part1 :: Packet -> Int
day16part1 Packet { versionNumber = v, packetBody = LiteralValue n } = v
day16part1 Packet { versionNumber = v, packetBody = OperatorPacket children } = v + sum (map day16part1 children)

day16part2 :: Packet -> Int
day16part2 Packet { packetBody = LiteralValue n } = n
day16part2 Packet { typeId = 0, packetBody = OperatorPacket ps } = sum $ map day16part2 ps
day16part2 Packet { typeId = 1, packetBody = OperatorPacket ps } = product $ map day16part2 ps
day16part2 Packet { typeId = 2, packetBody = OperatorPacket ps } = minimum $ map day16part2 ps
day16part2 Packet { typeId = 3, packetBody = OperatorPacket ps } = maximum $ map day16part2 ps
day16part2 Packet { typeId = 5, packetBody = OperatorPacket [lhs, rhs] } = if day16part2 lhs > day16part2 rhs then 1 else 0
day16part2 Packet { typeId = 6, packetBody = OperatorPacket [lhs, rhs] } = if day16part2 lhs < day16part2 rhs then 1 else 0
day16part2 Packet { typeId = 7, packetBody = OperatorPacket [lhs, rhs] } = if day16part2 lhs == day16part2 rhs then 1 else 0
day16part2 _ = error "malformed packet"

day16main :: IO ()
day16main = do
  handle <- openFile "../inputs2021/day16.txt" ReadMode
  contents <- hGetContents handle
  let bits = inputToBitStr contents
  let (parsed, state) = runState parsePacket bits
  if or state then do
      putStrLn "what? some unparsed truthy bits are left over"
      print state
  else do
      putStr "Day 16 part 1: "
      print $ day16part1 parsed
      putStr "Day 16 part 2: "
      print $ day16part2 parsed

