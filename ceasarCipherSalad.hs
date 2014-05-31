import Data.Char

encode :: Int -> String -> String
encode offset = map (\c -> chr $ offset + ord c)

decode :: Int -> String -> String
decode offset = encode (negate offset)
