import Data.Maybe
import Data.Char
import Text.Read

data Register = A | B | C | D deriving (Read, Eq)
type State = (Int, (Register -> Int))

instructions :: String -> Int -> Maybe [String]
instructions input x =
    if x >= length program then Nothing
    else Just $ words $ map toUpper $ program !! x
        where program = lines input

update :: (Register -> Int) -> Register -> Int -> (Register -> Int)
update registers register value = (\x -> if x == register then value else registers x)

decodeJNZ :: [String] -> State -> State
decodeJNZ (operand:jump:[]) (line, registers) = (line+offset, registers)
    where value = case readMaybe operand of
            Just a -> a
            Nothing -> registers $ read operand
          offset = if value == 0 then 1 else read jump

decodeCPY :: [String] -> State -> State
decodeCPY (operand:register:[]) (line, registers) = (line+1, update registers (read register) value)
    where value = case readMaybe operand of
            Just a -> a
            Nothing -> registers $ read operand

decodeINC :: [String] -> State -> State
decodeINC (register:[]) (line, registers) = (line+1, update registers (read register) $ (registers $ read register)+1)

decodeDEC :: [String] -> State -> State
decodeDEC (register:[]) (line, registers) = (line+1, update registers (read register) $ (registers $ read register)-1)

decode :: [String] -> State -> State
decode (instruction:operands) state
    | instruction == "JNZ" = decodeJNZ operands state
    | instruction == "CPY" = decodeCPY operands state
    | instruction == "INC" = decodeINC operands state
    | instruction == "DEC" = decodeDEC operands state

runProgram :: (Int -> Maybe [String]) -> State -> (Register -> Int)
runProgram instructions (line, registers) =
    case instruction of
        Just a -> runProgram instructions $ decode a (line, registers)
        Nothing -> registers
        where instruction = instructions line

main = do
    input <- readFile "input.txt"
    print $ (runProgram (instructions input) (0, \_ -> 0)) A
