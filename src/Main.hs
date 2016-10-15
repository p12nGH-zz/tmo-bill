import Text.Parsing.Report
import Control.Monad (forM_)

dollarAmount :: String -> Double
dollarAmount = read . tail . snd . (break ((==) '$'))

data Account = Line String Double (Maybe Double) deriving (Show)

phoneNumber = do
    skipUntil $ startsWith "("
    currentLine

getAmount s = do
    skipUntil $ startsWith s
    dollarAmount <$> currentLine

getTotal = getAmount "Total: "
getSubtotal = getAmount "Subtotal: "

line = (,) <$> phoneNumber <*> getTotal

lineAcc = do
    (l, a) <- within' line $ do
        skipUntil $ startsWith "AAL SC N.America UNL"
    return $ Line (fst l) (snd l) (Just 8.5)

lineNoAcc = do
    l <- line
    return $ Line (fst l) (snd l) Nothing

main = do
    (Just r) <- processStdIn $ do
        (,) <$> getTotal
            <*> (many $ skipUntil $ choice [lineAcc, lineNoAcc])
    forM_ (snd r) print
    print (fst r)
