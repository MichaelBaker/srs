module Display (showColumns) where

import Data.List(intercalate)

showColumns :: Int -> Int -> ([String], [String]) -> String
showColumns wrapOne wrapTwo (as', bs') = intercalate "\n" $ map joinRow $ zip (map (pad wrapOne) colOneRows) (map (pad wrapTwo) colTwoRows)
  where as        = map (rmLineBreaks . rmLeadingWhitespace) $ concatMap (wrapText wrapOne) as'
        bs        = map (rmLineBreaks . rmLeadingWhitespace) $ concatMap (wrapText wrapTwo) bs'
        rowCount  = max (length as) (length bs)
        colOneLen = 1 + (maximum $ map length as)
        colTwoLen = maximum $ map length bs
        colOneRows = as ++ take (max 0 $ rowCount - length as) (repeat "")
        colTwoRows = bs ++ take (max 0 $ rowCount - length bs) (repeat "")

joinRow (a, b) = a ++ b

pad size s = s ++ take padding (repeat ' ')
  where padding = max (size - length s) 0

wrapText width s | length s <= width = [s]
                 | otherwise = take width s : wrapText width (drop width s)

rmLeadingWhitespace s = dropWhile (\c -> c `elem` " \r\n\t") s

rmLineBreaks []        = []
rmLineBreaks ('\n':ss) = ' ' : rmLineBreaks ss
rmLineBreaks ('\r':ss) = ' ' : rmLineBreaks ss
rmLineBreaks (s:ss)    = s : rmLineBreaks ss
