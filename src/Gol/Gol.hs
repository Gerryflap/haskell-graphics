-- Game Of Life, in Haskell
module Gol.Gol (
	runGol
) where
	import Graphics.Gloss
	import Graphics.Gloss.Interface.IO.Game

	runGol :: IO ()
	runGol = playIO  
			(InWindow
			"Hello World" 	 -- window title
			(500, 500) 	 -- window size
			(10, 10)) 	 -- window position
		black			 -- background color
		60				 -- Framerate (idk how this will work lol)
		(WorldState 0.0)	 -- Initial world
		render			 -- render the world
		handleInput
		step


	data World = WorldState Float

	render :: World -> IO Picture
	render (WorldState t)
		= pure $ Translate (-170) (-20) -- shift the text to the middle of the window
		$ Scale 0.5 0.5		 -- display it half the original size
		$ Color white
		$ Text $ "Hello World " ++ (show t)	 -- text to display

	-- World step, based on delta t
	step :: Float -> World -> IO World
	step dt (WorldState t) = pure $ WorldState $ t + dt

	-- Ignore input
	handleInput :: Event -> World -> IO World
	handleInput _ w = pure w