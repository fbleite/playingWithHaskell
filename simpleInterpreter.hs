module SimpleInterpreter (simpleAssembler) where
import qualified Data.Map.Strict as M
import qualified Data.Map as DM (fromList, insert, lookup) 
import qualified Data.List as L (intercalate)

type Registers = M.Map String Int
-- ["mov a 5", "inc a", "dec a", "dec a", "jnz a -1", "inc a"]

-- simpleAssembler :: [String] -> Registers
-- simpleAssembler [] = DM.fromList []
-- simpleAssembler ["mov hello 0"] = DM.fromList [("hello", 0)]
-- simpleAssembler ["mov hello 1", "dec hello"] = DM.fromList [("hello", 0)]
-- simpleAssembler ["mov hello -1", "inc hello"] = DM.fromList [("hello", 0)]
-- simpleAssembler ["mov hello 0", "mov hello 1"] = DM.fromList [("hello", 1)]
-- simpleAssembler ["mov x 0", "jnz x 10"] = DM.fromList [("x", 0)]
-- simpleAssembler ["mov x 0", "mov y 10", "jnz x y"] = DM.fromList [("x", 0), ("y", 10)]
-- simpleAssembler ["mov x 1", "mov y 10", "jnz x y"] = DM.fromList [("x", 1), ("y", 10)]
-- simpleAssembler ["mov x 10", "dec x", "jnz x -1"] = DM.fromList [("x", 0)]
-- simpleAssembler ["mov x 1", "jnz x 10"] = DM.fromList [("x", 1)]
-- simpleAssembler ["mov x -10", "inc x", "jnz x -1"] = DM.fromList [("x", 0)]
-- simpleAssembler ["mov x 0", "jnz x 2", "mov y 1", "mov z 1"] = DM.fromList [("x", 0),("y", 1), ("z", 1)]
-- simpleAssembler ["mov x 1", "jnz x 2", "mov y 1", "mov z 1"] = DM.fromList [("x", 1), ("z", 1)]
-- simpleAssembler ["mov x 0", "jnz x 3", "mov y 1", "jnz 1 2", "mov z 1"] = DM.fromList [("x", 0), ("y", 1)]
-- simpleAssembler ["mov x 1", "jnz x 3", "mov y 1", "jnz 1 2", "mov z 1"] = DM.fromList [("x", 1), ("z", 1)]
-- simpleAssembler ["mov x 1", "mov y 2", "inc x", "inc x", "dec y", "jnz y -2"] = DM.fromList [("x", 4), ("y", 0)]
-- simpleAssembler ["mov x 1", "mov y 2", "mov z -2", "inc x", "inc x", "dec y", "jnz y z"] = DM.fromList [("x",4),("y",0),("z",-2)]
-- simpleAssembler ["mov x 1", "mov y 2", "mov z -1", "dec z", "inc x", "inc x", "dec y", "jnz y z"] = DM.fromList [("x",4),("y",0),("z",-2)]
-- simpleAssembler ["mov a 5", "inc a", "dec a", "dec a", "jnz a -1", "inc a"] = DM.fromList [("a",1)]
-- simpleAssembler ["mov a -10", "mov b a", "inc a", "dec b", "jnz a -2"] = DM.fromList [("a",0),("b",-20)]
-- simpleAssembler ["mov a 1", "mov b 1", "mov c 0", "mov d 26", "jnz c 2", "jnz 1 5", "mov c 7", "inc d", "dec c", "jnz c -2", "mov c a", "inc a", "dec b", "jnz b -2", "mov b c", "dec d", "jnz d -6", "mov c 18", "mov d 11", "inc a", "dec d", "jnz d -2", "dec c", "jnz c -5"] = DM.fromList [("a",0),("b",-20)]
-- simpleAssembler xs = error  (L.intercalate "\", \"" xs)


simpleAssembler :: [String] -> Registers
simpleAssembler xs = processInstruction (DM.fromList []) xs 0

processInstruction :: Registers -> [String] -> Int -> Registers
processInstruction regs instructions currentInstructionIndex
    | isLastInstruction = regs
    | otherwise = processInstruction newRegs instructions newInstructionIndex
    where isLastInstruction = currentInstructionIndex >= length instructions 
          parsedInstruction = words $ instructions !! currentInstructionIndex
          command = head parsedInstruction
          arguments = tail parsedInstruction
          newRegs 
            | command == "mov" = processMov regs arguments
            | command == "inc" = processIncOrDec regs arguments (+1)
            | command == "dec" = processIncOrDec regs arguments (+ (-1))
            | command == "jnz" = regs
            | otherwise = undefined
          newInstructionIndex 
            | command `elem` ["mov", "inc", "dec"] = (currentInstructionIndex + 1)
            | command == "jnz" = processJnz newRegs arguments currentInstructionIndex

processMov :: Registers -> [String] -> Registers
processMov regs args = DM.insert regName valueToCopy regs
    where regName = head args
          value = last $ args
          valueToCopy 
            | all isAlpha value = getRegisterValue regs value
            | otherwise = read value

isAlpha :: Char -> Bool
isAlpha c 
    | c `elem` ['a'..'z'] = True
    | c `elem` ['A'..'Z'] = True
    | otherwise = False

processIncOrDec :: Registers -> [String] -> (Int->Int) -> Registers
processIncOrDec regs args incDec = processMov regs moveArgs
    where regName = head args 
          currentRegValue = getRegisterValue regs regName
          newRegValueString =  show newValue 
          newValue = incDec currentRegValue
          moveArgs = [regName, newRegValueString]

processJnz :: Registers -> [String] -> Int -> Int
processJnz regs args currentInstructionIndex = if(regToCheck /= 0)
                                            then currentInstructionIndex + skipCount
                                            else currentInstructionIndex + 1
    where regName = head args 
          skipCount 
            | all isSkippable stringSkipCount = read stringSkipCount
            | otherwise = getRegisterValue regs stringSkipCount
          stringSkipCount = last args
          regToCheck 
            | all isAlpha regName = getRegisterValue regs regName
            | otherwise = read regName

isSkippable :: Char -> Bool
isSkippable c 
    | c `elem` ['0'..'9'] = True
    | c == '-' = True
    | otherwise = False

getRegisterValue :: Registers -> String -> Int
getRegisterValue regs regName = case DM.lookup regName regs of
                                    Just n -> n
                                    Nothing -> error ("Register not present: " ++ regName)
