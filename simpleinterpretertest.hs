module SimpleInterpreter.Test where
import Test.Hspec
import Test.QuickCheck
import SimpleInterpreter
import Data.Map.Strict as M

test = do
    describe "Sample tests" $ do
    it "works with sample1" $ do
        ["mov a 5","inc a","dec a","dec a","jnz a -1", "inc a"] ===> [("a", 1)]
    it "works with sample2" $ do
        ["mov a -10","mov b a","inc a","dec b","jnz a -2"] ===> [("a", 0), ("b", (-20))]
    it "works with sample3" $ do
        [] ===> []
    it "works with sample4" $ do
        ["mov hello 0"] ===> [("hello", 0)]
    it "works with sample5" $ do
        ["mov hello 1", "dec hello"] ===> [("hello", 0)]
    it "works with sample6" $ do
        ["mov hello -1", "inc hello"] ===> [("hello", 0)]
    it "works with sample7" $ do
        ["mov hello 0", "mov hello 1"] ===> [("hello", 1)]
    it "works with sample8" $ do
        ["mov x 0", "jnz x 10"] ===> [("x", 0)]
    it "works with sample9" $ do
        ["mov x 0", "mov y 10", "jnz x y"] ===> [("x", 0), ("y", 10)]
    it "works with sample10" $ do
        ["mov x 1", "mov y 10", "jnz x y"] ===> [("x", 1), ("y", 10)]
    it "works with sample11" $ do
        ["mov x 10", "dec x", "jnz x -1"] ===> [("x", 0)]
    it "works with sample12" $ do
        ["mov x 1", "jnz x 10"] ===> [("x", 1)]
    it "works with sample13" $ do
        ["mov x -10", "inc x", "jnz x -1"] ===> [("x", 0)]
    it "works with sample14" $ do
        ["mov x 0", "jnz x 2", "mov y 1", "mov z 1"] ===> [("x", 0),("y", 1), ("z", 1)]
    it "works with sample15" $ do
        ["mov x 1", "jnz x 2", "mov y 1", "mov z 1"] ===> [("x", 1), ("z", 1)]
    it "works with sample16" $ do
        ["mov x 0", "jnz x 3", "mov y 1", "jnz 1 2", "mov z 1"] ===> [("x", 0), ("y", 1)]
    it "works with sample17" $ do
        ["mov x 1", "jnz x 3", "mov y 1", "jnz 1 2", "mov z 1"] ===> [("x", 1), ("z", 1)]
    it "works with sample18" $ do
        ["mov x 1", "mov y 2", "inc x", "inc x", "dec y", "jnz y -2"] ===> [("x", 4), ("y", 0)]
    it "works with sample19" $ do
        ["mov x 1", "mov y 2", "mov z -2", "inc x", "inc x", "dec y", "jnz y z"] ===> [("x",4),("y",0),("z",-2)]
    it "works with sample20" $ do
        ["mov x 1", "mov y 2", "mov z -1", "dec z", "inc x", "inc x", "dec y", "jnz y z"] ===> [("x",4),("y",0),("z",-2)]
    it "works with sample21" $ do
        ["mov a 5", "inc a", "dec a", "dec a", "jnz a -1", "inc a"] ===> [("a",1)]
    it "works with sample22" $ do
        ["mov a -10", "mov b a", "inc a", "dec b", "jnz a -2"] ===> [("a",0),("b",-20)]
    it "works with sample23" $ do
        ["mov a 1", "mov b 1", "mov c 0", "mov d 26", "jnz c 2", "jnz 1 5", "mov c 7", "inc d", "dec c", "jnz c -2", "mov c a", "inc a", "dec b", "jnz b -2", "mov b c", "dec d", "jnz d -6", "mov c 18", "mov d 11", "inc a", "dec d", "jnz d -2", "dec c", "jnz c -5"] ===> [("a",318009),("b",196418),("c",0),("d",0)]

prog ===> regs = (simpleAssembler prog) `shouldBe` (M.fromList regs)

main = hspec test