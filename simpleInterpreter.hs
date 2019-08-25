import qualified Data.Map.Strict as M
import qualified Data.Map as DM (fromList, insert, lookup) 

type Registers = M.Map String Int
-- ["mov a 5", "inc a", "dec a", "dec a", "jnz a -1", "inc a"]

-- simpleAssembler :: [String] -> Registers
-- simpleAssembler xs =  foldl (\reg instruction -> processInstruction reg instruction) (DM.fromList []) xs


simpleAssembler :: [String] -> Registers
simpleAssembler xs =  foldl (\reg instruction -> processInstruction reg instruction) (DM.fromList []) xs
    
processInstruction :: Registers -> String -> Registers
processInstruction reg instruction 
    | command == "mov" = processMov reg arguments
    | command == "inc" = processInc reg arguments
    | command == "dec" = processDec reg arguments
    | command == "jnz" = DM.fromList[("a", 4)]
    | otherwise = undefined
    where parsedInstruction = words instruction
          command = head parsedInstruction
          arguments = tail parsedInstruction

processMov :: Registers -> [String] -> Registers
processMov reg args = DM.insert regName value reg
    where regName = head args
          value = read . last $ args

processInc :: Registers -> [String] -> Registers
processInc reg args = processMov reg moveArgs
    where regName = head args 
          currentRegValue = case DM.lookup regName reg of
                                Just n -> n
                                Nothing -> error "Register not present"
          newRegValueString =  show $ (currentRegValue + 1)
          moveArgs = [regName, newRegValueString]


processDec :: Registers -> [String] -> Registers
processDec reg args = processMov reg moveArgs
    where regName = head args 
          currentRegValue = case DM.lookup regName reg of
                                Just n -> n
                                Nothing -> error "Register not present"
          newRegValueString =  show $ (currentRegValue - 1)
          moveArgs = [regName, newRegValueString]