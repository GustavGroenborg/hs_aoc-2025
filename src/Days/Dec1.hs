module Days.Dec1 where

data Dial = Dial 
    { getSum      :: Int
    , finalZero   :: Int
    , runningZero :: Int
    } deriving (Show, Eq)

data Instruction = Instruction 
    { direction :: Char
    , rotations :: Int
    } deriving (Show)

turnRight :: Dial -> Dial
turnRight dial = Dial sum' (finalZero dial) ro'
    where sum' = if getSum dial + 1 == 100 then 0 else getSum dial + 1
          ro'  = if sum' == 0 then runningZero dial + 1 else runningZero dial

turnLeft :: Dial -> Dial
turnLeft dial = Dial sum' (finalZero dial) ro'
    where sum' = if getSum dial - 1 == -1 then 99 else getSum dial - 1
          ro'  = if sum' == 0 then runningZero dial + 1 else runningZero dial

parseInstruction :: String -> Instruction
parseInstruction [] = error "empty input"
parseInstruction (s:str) = Instruction s (read str)

doInstruction :: Instruction -> Dial -> Dial
doInstruction (Instruction _ 0) dial =
    if getSum dial == 0 then
        Dial 0 (finalZero dial + 1) (runningZero dial)
    else dial
doInstruction (Instruction dir num) dial =
    doInstruction (Instruction dir (num - 1)) dial'
    where dial' = if dir == 'L' then turnLeft dial else turnRight dial

doInstructions :: [Instruction] -> Dial -> Dial
doInstructions [] dial     = dial
doInstructions (i:is) dial = doInstructions is (doInstruction i dial)
