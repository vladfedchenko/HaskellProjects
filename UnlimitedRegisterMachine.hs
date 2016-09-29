import Data.List
import System.IO
import Debug.Trace

data Command = Z Int | S Int | C Int Int | J Int Int Int --define the available commands

instance Show Command where
	show (Z n) = "Z(" ++ (show n) ++ ")"
	show (S n) = "S(" ++ (show n) ++ ")"
	show (C m n) = "C(" ++ (show m) ++ ", " ++ (show n) ++ ")"
	show (J m n q) = "J(" ++ (show m) ++ ", " ++ (show n) ++ ", " ++ (show q) ++ ")"

urm :: [Command] -> [Int] --define interface of URM function

urmExec :: Int -> [Command] -> [Command] -> [Int] -> [Int] --interface of URM function with additional information

urm coms = urmExec 1 [] coms [0, 0 ..] --URM function simply calls urmExec with empty list to store processed commands and infinite list of registers

urmExec _ _ [] regList = regList --if no commands left return list of registers

urmExec curComNum prevComsStack ((Z n):restOfComs) regList = urmExec (curComNum + 1) ((Z n):prevComsStack) restOfComs (zeroNReg regList n) -- Z(n) command processing
urmExec curComNum prevComsStack ((S n):restOfComs) regList = urmExec (curComNum + 1) ((S n):prevComsStack) restOfComs (increaseNReg regList n) -- S(n) command processing
urmExec curComNum prevComsStack ((C m n):restOfComs) regList = urmExec (curComNum + 1) ((C m n):prevComsStack) restOfComs (copyMtoN regList m n) -- C(m, n) command processing
urmExec curComNum prevComsStack ((J m n q):restOfComs) regList -- J(m, n, q) command processing
	| (regList !! (m - 1)) /= (regList !! (n - 1)) = urmExec (curComNum + 1) ((J m n q):prevComsStack) restOfComs regList --values in registers m and n are not equal, no jump reqiured
	| otherwise = executeJump curComNum q prevComsStack ((J m n q):restOfComs) regList -- otherwise - execute jump to command #q
	
executeJump :: Int -> Int -> [Command] -> [Command] -> [Int] -> [Int] --jump method definition
executeJump curComNum targetComNum (lastCom:prevComsStack) (nextCom:restOfComs) regList
	| curComNum == targetComNum = urmExec curComNum (lastCom:prevComsStack) (nextCom:restOfComs) regList --current command equals target -> continue URM execution
	| curComNum < targetComNum = executeJump (curComNum + 1 ) targetComNum (nextCom:lastCom:prevComsStack) restOfComs regList -- current command is less than target -> move forward in command list
	| otherwise = executeJump (curComNum - 1) targetComNum prevComsStack (lastCom:nextCom:restOfComs) regList -- current command is greater than target -> move backward in command list

insertAtNReg :: [Int] -> Int -> Int -> [Int] --insert into list (1) a value (3) on the position n(2) and return new list
insertAtNReg regs n val = insertAtNRegExec [] regs n val

insertAtNRegExec :: [Int] -> [Int] -> Int -> Int -> [Int] --insertAtNReg realisation (using extra parameter)
insertAtNRegExec a (x:xs) 1 val = a ++ (val:xs)
insertAtNRegExec a (x:xs) n val = insertAtNRegExec (a ++ (x:[])) xs (n - 1) val

copyMtoN :: [Int] -> Int -> Int -> [Int] -- copy value of m register to n register
copyMtoN regs m n = insertAtNReg regs n (regs !! (m - 1))

zeroNReg :: [Int] -> Int -> [Int] -- sets N-th register to zeroNReg
zeroNReg regs n = insertAtNReg regs n 0

increaseNReg :: [Int] -> Int -> [Int] -- increases N-th register by one
increaseNReg regs n = insertAtNReg regs n ((regs !! (n - 1)) + 1)
