module Windy (solve) where

import Data.HashMap.Strict qualified as HM
import Data.List (sortOn)
import Data.List.Split
import Data.Ord (Down (..))

type Balances = HM.HashMap String Int

type Debts = HM.HashMap String [(String, Int)]

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

settleIncoming :: (Balances, Debts) -> String -> Int -> (Balances, Debts)
settleIncoming (bals, debts) user amount =
  let userDebts = HM.lookupDefault [] user debts
      ((balsAfterPay, debtsAfterPay), remainingDebts, leftoverCash) =
        processDebts (bals, debts) userDebts amount
      finalDebts = HM.insert user remainingDebts debtsAfterPay
      currentBal = HM.lookupDefault 0 user balsAfterPay
      finalBals = HM.insert user (currentBal + leftoverCash) balsAfterPay
   in (finalBals, finalDebts)

processDebts :: (Balances, Debts) -> [(String, Int)] -> Int -> ((Balances, Debts), [(String, Int)], Int)
processDebts state [] available = (state, [], available)
processDebts state debts 0 = (state, debts, 0)
processDebts state ((creditor, debtAmt) : restDebts) available =
  let payment = min debtAmt available
      newState = settleIncoming state creditor payment
      remainingAvailable = available - payment
   in if payment < debtAmt
        then (newState, (creditor, debtAmt - payment) : restDebts, 0)
        else processDebts newState restDebts remainingAvailable

doTransactionWithDebts :: (Balances, Debts) -> (String, String, Int) -> (Balances, Debts)
doTransactionWithDebts (balances, debts) (from, to, amount) =
  let fromBalance = HM.lookupDefault 0 from balances
      maxTransfer = min amount fromBalance
      (balancesAfterWithdraw, debtsAfterWithdraw) =
        if maxTransfer < amount
          then
            let fromDebts = HM.lookupDefault [] from debts
                newFromDebts = (to, amount - maxTransfer) : fromDebts
                newDebts = HM.insert from newFromDebts debts
                newBals = HM.insert from (fromBalance - maxTransfer) balances
             in (newBals, newDebts)
          else
            let newBals = HM.insert from (fromBalance - maxTransfer) balances
             in (newBals, debts)
   in settleIncoming (balancesAfterWithdraw, debtsAfterWithdraw) to maxTransfer

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
    (balances, _) = foldl' doTransactionWithDebts (HM.fromList initialStates, HM.empty) transactionStates
    sortedBalances = sortOn Down (map snd (HM.toList balances))
    res = sum $ take 3 sortedBalances

solve :: Int -> String -> String
solve n input = case n of
  1 -> part1 input
  2 -> part2 input
  3 -> part3 input
  _ -> error "Invalid part number"