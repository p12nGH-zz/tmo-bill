import Text.Parsing.Report
import Control.Monad (forM_)

dollarAmount :: String -> Double
dollarAmount = read . tail . snd . (break ((==) '$'))

data AccountType = VoiceLine | DataLine | VoiceLineWithAcc Double

accountTotal = do 
    skipUntil $ startsWith "Total: "
    dollarAmount <$> currentLine

phoneNumber = do
    skipUntil $ startsWith "("
    currentLine

getTotal = do
    skipUntil $ startsWith "Total: "
    dollarAmount <$> currentLine

line = do
    number <- phoneNumber 
    skip 1
    -- startsWith "Plan" 
    total <- getTotal 
    return (number, total)

line2 = do
    number <- phoneNumber 
    startsWith "T-Mobile fees and charges"
    total <- getTotal 
    return (number, total)

main = do
    (Just r) <- processStdIn $ do
        accountTotal
        many $ choice [line]
    forM_ r print
