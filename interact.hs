main = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly = unlines 
	       . filter ((<10) . length) 
               . lines
