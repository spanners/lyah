-- You can control how exactly buffering is done by using the hSetBuffering function.

main = do
	withFile "something.txt" ReadMode (\handle -> do
		-- Reading the file in bigger chunks can help if we want to minimise disk acess or when our file is actually a slow network resource.
		hSetBuffering handle $ BlockBuffering (Just 2048)
		contents <- hGetContents handle
		putStr contents)
