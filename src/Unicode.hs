module Unicode (superscriptString) where

superscriptChar :: Char -> Char
superscriptChar c =
  case c of
    '-' -> '⁻'
    '0' -> '⁰'
    '1' -> '¹'
    '2' -> '²'
    '3' -> '³'
    '4' -> '⁴'
    '5' -> '⁵'
    '6' -> '⁶'
    '7' -> '⁷'
    '8' -> '⁸'
    '9' -> '⁹'
    _ -> c

superscriptString :: String -> Int -> String
superscriptString _ 0 = ""
superscriptString s 1 = s
superscriptString s n = s ++ map superscriptChar (show n)
