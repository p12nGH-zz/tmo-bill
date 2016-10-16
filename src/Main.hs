import Text.Parsing.Report
import Control.Monad (forM_)
import Data.Maybe (catMaybes)

dollarAmount :: String -> Float
dollarAmount = read . tail . snd . (break ((==) '$'))

data Account = Line {
    number :: String,
    amount:: Float,
    accCharges :: (Maybe Float) } deriving (Show)

find = skipUntil . startsWith
phoneNumber = find "(" *> currentLine
getAmount s = find s *> (dollarAmount <$> currentLine)

getTotal = getAmount "Total: "

line = (,) <$> phoneNumber <* find "Plan: SC N.America UnlTT" <*> getTotal

lineAcc = do
    ((n, a), ac) <- within' line $ do
        find "AAL SC N.America UNL"
    return $ Line n a (Just 8.5)

lineNoAcc = do
    (n, a) <- line
    return $ Line n a Nothing

main = do
    (Just (a, r)) <- processStdIn $ do
        (,) <$> getTotal
            <*> (many $ skipUntil $ choice [lineAcc, lineNoAcc])
    forM_ r print
    let acc = sum $ catMaybes $ map accCharges r
    print $ a + acc
