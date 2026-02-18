{-# LANGUAGE BlockArguments #-}

module Windy (solve) where

import Control.Monad (when)
import Control.Monad.ST
import Data.HashMap.Strict qualified as HM
import Data.List (sortOn)
import Data.List.Split
import Data.Ord (Down (..))
import Data.STRef

type Balances = HM.HashMap String Int

data Account s = Account
  { debts :: STRef s [(String, Int)],
    balance :: STRef s Int
  }

type Bank s = HM.HashMap String (Account s)

addToBalance :: Account s -> Int -> ST s ()
addToBalance account amount = do
  modifySTRef' (balance account) (+ amount)

addToDebt :: Account s -> String -> Int -> ST s ()
addToDebt account creditor amount = do
  modifySTRef' (debts account) (++ [(creditor, amount)])

settleBalance :: Bank s -> Account s -> ST s ()
settleBalance bank account = do
  let tryToSettle = do
        currentDebts <- readSTRef (debts account)
        currentBalance <- readSTRef (balance account)
        case (currentBalance, currentDebts) of
          (0, _) -> return ()
          (_, []) -> return ()
          (bal, (creditor, debtAmt) : restDebts) -> do
            let payment = min debtAmt bal
            addToBalance account (-payment)
            if debtAmt == payment
              then writeSTRef (debts account) restDebts
              else writeSTRef (debts account) ((creditor, debtAmt - payment) : restDebts)
            let creditorAccount = HM.lookupDefault (error "Creditor not found") creditor bank
            addToBalance creditorAccount payment
            settleBalance bank creditorAccount
            tryToSettle
  tryToSettle

doTransactionWithDebts :: [(String, Int)] -> [(String, String, Int)] -> [Int]
doTransactionWithDebts initialBalances transactions = runST $ do
  bank <- HM.fromList <$> mapM createAccount initialBalances
  mapM_ (processTransaction bank) transactions
  mapM (readSTRef . balance) (HM.elems bank)
  where
    createAccount (name, bal) = do
      balRef <- newSTRef bal
      debtRef <- newSTRef []
      return (name, Account debtRef balRef)
    processTransaction bank (from, to, amount) = do
      let fromAccount = HM.lookupDefault (error "From account not found") from bank
          toAccount = HM.lookupDefault (error "To account not found") to bank
      fromBalance <- readSTRef (balance fromAccount)
      let actualAmount = min amount fromBalance
      addToBalance fromAccount (-actualAmount)
      when (actualAmount < amount) $ addToDebt fromAccount to (amount - actualAmount)
      when (actualAmount > 0) $ do
        addToBalance toAccount actualAmount
        settleBalance bank toAccount

doTransaction :: Balances -> (String, String, Int) -> Balances
doTransaction balances (from, to, ammount) = finalRes
  where
    fromBalance = HM.lookupDefault 0 from balances
    afterWithdraw = HM.insert from (fromBalance - ammount) balances
    toBalance = HM.lookupDefault 0 to afterWithdraw
    finalRes = HM.insert to (toBalance + ammount) afterWithdraw

doLimitedTransaction :: Balances -> (String, String, Int) -> Balances
doLimitedTransaction balances (from, to, ammount) = finalRes
  where
    fromBalance = HM.lookupDefault 0 from balances
    maxTransfer = min ammount fromBalance
    afterWithdraw = HM.insert from (fromBalance - maxTransfer) balances
    toBalance = HM.lookupDefault 0 to afterWithdraw
    finalRes = HM.insert to (toBalance + maxTransfer) afterWithdraw

parseInitial :: String -> (String, Int)
parseInitial line = case words line of
  (name : _ : value : _) -> (name, read value)
  _ -> error "Invalid initial state format"

parseTransaction :: String -> (String, String, Int)
parseTransaction line = (from, to, ammount)
  where
    (from, to, ammount) = case words line of
      (_ : f : _ : t : _ : a : _) -> (f, t, read a)
      _ -> error "Invalid transaction format"

parseInput :: String -> ([(String, Int)], [(String, String, Int)])
parseInput input = (initialStates, transactionStates)
  where
    (initial, transactions) = case splitOn [""] (lines input) of
      [a, b] -> (a, b)
      _ -> error "Invalid input format"
    initialStates = map parseInitial initial
    transactionStates = map parseTransaction transactions

part1 :: String -> String
part1 input = show res
  where
    (initialStates, transactionStates) = parseInput input
    balances = foldl' doTransaction (HM.fromList initialStates) transactionStates
    sortedBalances = sortOn Down (map snd (HM.toList balances))
    res = sum $ take 3 sortedBalances

part2 :: String -> String
part2 input = show res
  where
    (initialStates, transactionStates) = parseInput input
    balances = foldl' doLimitedTransaction (HM.fromList initialStates) transactionStates
    sortedBalances = sortOn Down (map snd (HM.toList balances))
    res = sum $ take 3 sortedBalances

part3 :: String -> String
part3 input = show res
  where
    (initialStates, transactionStates) = parseInput input
    balances = doTransactionWithDebts initialStates transactionStates
    sortedBalances = sortOn Down balances
    res = sum $ take 3 sortedBalances

solve :: Int -> String -> String
solve n input = case n of
  1 -> part1 input
  2 -> part2 input
  3 -> part3 input
  _ -> error "Invalid part number"