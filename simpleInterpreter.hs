import qualified Data.Map.Strict as M
import qualified Data.Map as DM (fromList, insert, lookup) 

type Registers = M.Map String Int
-- ["mov a 5", "inc a", "dec a", "dec a", "jnz a -1", "inc a"]

simpleAssembler :: [String] -> Registers
simpleAssembler xs = processInstruction (DM.fromList []) xs 0

processInstruction :: Registers -> [String] -> Int -> Registers
processInstruction reg instructions currentInstruction
    | currentInstruction == length instructions = reg
    | command == "mov" = processInstruction newRegs instructions (currentInstruction + 1)
    | command == "inc" = processInstruction newRegs instructions (currentInstruction + 1)
    | command == "dec" = processInstruction newRegs instructions (currentInstruction + 1)
    | command == "jnz" = processInstruction newRegs instructions (processJnz newRegs arguments currentInstruction)
    | otherwise = undefined
    where parsedInstruction = words $ instructions !! currentInstruction
          command = head parsedInstruction
          arguments = tail parsedInstruction
          newRegs 
            | command == "mov" = processMov reg arguments
            | command == "inc" = processInc reg arguments
            | command == "dec" = processDec reg arguments
            | command == "jnz" = reg
            | otherwise = undefined


processMov :: Registers -> [String] -> Registers
processMov regs args 
    | all isAlpha value = DM.insert regName valueFromReg regs
    | otherwise = DM.insert regName valueInt regs
    where regName = head args
          value = last $ args
          valueInt = read value
          valueFromReg = getRegisterValue regs value


isAlpha :: Char -> Bool
isAlpha c 
    | c `elem` ['a'..'z'] = True
    | c `elem` ['A'..'Z'] = True
    | otherwise = False

processInc :: Registers -> [String] -> Registers
processInc regs args = processMov regs moveArgs
    where regName = head args 
          currentRegValue = getRegisterValue regs regName
          newRegValueString =  show $ (currentRegValue + 1)
          moveArgs = [regName, newRegValueString]


processDec :: Registers -> [String] -> Registers
processDec regs args = processMov regs moveArgs
    where regName = head args 
          currentRegValue = getRegisterValue regs regName
          newRegValueString =  show $ (currentRegValue - 1)
          moveArgs = [regName, newRegValueString]

processJnz :: Registers -> [String] -> Int -> Int
processJnz regs args currentInstruction = if(regToCheck /= 0)
                                            then currentInstruction + skipCount
                                            else currentInstruction + 1
    where regName = head args 
          skipCount = read . last $ args
          regToCheck = getRegisterValue regs regName

getRegisterValue :: Registers -> String -> Int
getRegisterValue regs regName = case DM.lookup regName regs of
                                    Just n -> n
                                    Nothing -> error ("Register not present: " ++ regName)


          