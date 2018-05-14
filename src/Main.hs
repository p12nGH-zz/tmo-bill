import Text.Parsing.Report
import Control.Monad (forM_)
import Data.Maybe (catMaybes)
import Text.Printf
import Debug.Trace
-- 
type DollarAmount = Int

printDollarAmount a = "" ++ (show d) ++ "." ++ (if c < 10 then "0" else "") ++ (show c)  where
        (d, c) = quotRem a 100 

dollarAmount :: String -> DollarAmount
dollarAmount s = d * 100 + c where
    (posd, posc) = break ((==) '.') $ tail $ snd $ break ((==) '$') s
    d = read posd
    c = read $ tail $ posc

data LineCharges = LineCharges {
    number :: String,
    total :: DollarAmount,
    accountCharges :: DollarAmount,
    installments :: DollarAmount
} deriving (Show)

find = skipUntil . startsWith
phoneNumber = find "(" *> currentLine
getAmount s = find s *> (dollarAmount <$> currentLine)
getTotal = getAmount "Total: $"

equipment = find "Equipment" *> getAmount "Subtotal"
plan = find "AAL SC N.America UNL" *> getAmount "$"

line = (,) <$> phoneNumber <* find "Plan: SC N.America UnlTT" <*> getTotal

lineAcc = do
    ((n, a), p) <- within' line $ plan 
    return $ LineCharges n a p 0

lineAccInstallments = do
    ((n, a), (p, e)) <- within' line $ (,) <$> plan <*> equipment 
    return $ LineCharges n a p e

lineInstallments = do
    ((n, a), e) <- within' line $ equipment 
    return $ LineCharges n a 0 e

lineNoAcc = do
    (n, a) <- line
    return $ LineCharges n a 0 0

fairSplit :: Int -> Int -> [Int]
fairSplit a n = f e r n where
    (e, r) = quotRem a n
    f _ _ 0 = []
    f e 0 n = e : (f e 0 (n - 1))
    f e r n = (e + 1) : (f e (r - 1) (n - 1)) 

main = do
    (Just (bill_dates, acc_total, r)) <- processStdIn $ do
        (,,) <$> (find "Bill close date" *> currentLine *> currentLine)
             <*> getTotal
             <*> (many $ skipUntil $ choice [lineAccInstallments, lineInstallments, lineAcc, lineNoAcc])
    putStrLn bill_dates
    print acc_total
    forM_ r $ \a -> print $ total a
    forM_ r print

    let
        acc_charges = acc_total + (sum $ map accountCharges r)
        per_line = fairSplit acc_charges $ length r

        d = map f (zip per_line r) where
            f (pl, (LineCharges n t a i)) = (n, t - a - i + pl)
        is = filter ((<) 0) $ map installments r
    forM_ d $ \(n, a) -> putStrLn $ n ++ " " ++ (printDollarAmount a)
    forM_ is $ \i -> putStrLn $ "      installm " ++ (printDollarAmount i)
{-

    let acc = sum $ catMaybes $ map (\(Line _ _ a) -> a) r
    let shared = (acc_total + acc) / (fromIntegral $ length r)
    forM_ r $ \(Line number subtotal acc_charges) -> do
        putStrLn $ number ++ (show (subtotal + shared))
-}
