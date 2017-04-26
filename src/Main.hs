import Text.Parsing.Report
import Control.Monad (forM_)
import Data.Maybe (catMaybes)
import Text.Printf

dollarAmount :: String -> Float
dollarAmount = read . tail . snd . (break ((==) '$'))

data Account = Line String Float (Maybe Float)

find = skipUntil . startsWith
phoneNumber = find "(" *> currentLine
getAmount s = find s *> (dollarAmount <$> currentLine)
getTotal = getAmount "Total: $"

line = (,) <$> phoneNumber <* find "Plan: SC N.America UnlTT" <*> getTotal

lineAcc = do
    ((n, a), _) <- within' line $ do
        find "AAL SC N.America UNL"
    return $ Line n (a - 8.5) (Just 8.5)

lineNoAcc = do
    (n, a) <- line
    return $ Line n a Nothing

main = do
    (Just (bill_dates, acc_total, r)) <- processStdIn $ do
        (,,) <$> (find "Bill close date" *> currentLine *> currentLine)
             <*> getTotal
             <*> (many $ skipUntil $ choice [lineAcc, lineNoAcc])
    putStrLn bill_dates
    let acc = sum $ catMaybes $ map (\(Line _ _ a) -> a) r
    let shared = (acc_total + acc) / (fromIntegral $ length r)
    forM_ r $ \(Line number subtotal acc_charges) -> do
        printf "%s %.02f\n" number (subtotal + shared)
