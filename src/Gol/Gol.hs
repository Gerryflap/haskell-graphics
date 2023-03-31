-- Game Of Life, in Haskell
module Gol.Gol (
    runGol

) where
    import Graphics.Gloss
    import Graphics.Gloss.Interface.IO.Game
    import Gol.Logic
    import Data.Bool
    import Data.Array.IArray

    runGol :: IO ()
    runGol = playIO  
            (InWindow
            "Hello World"   -- window title
            (500, 500)      -- window size
            (10, 10))       -- window position
        black               -- background color
        60                  -- Framerate (idk how this will work lol)
        (initialGolState (100, 100))     -- Initial world
        renderBoard         -- render the world
        handleInput
        step

    renderBoard :: GolState -> IO Picture
    renderBoard state = pure $ Pictures $ map renderTile $ assocs $ board state

    renderTile :: ((Int, Int), Bool) -> Picture
    renderTile ((_, _), False) = Blank      -- TODO: Don't
    renderTile ((x, y), True) = Translate ((fromIntegral x) * 5.0) ((fromIntegral y) * 5.0) 
                                $ Color white 
                                $ Polygon [(0,0), (5,0), (5,5), (0,5), (0,0)]

    -- World step, based on delta t
    step :: Float -> GolState -> IO GolState
    step dt state = pure $ golStateStep state

    -- Ignore input
    handleInput :: Event -> GolState -> IO GolState
    handleInput _ w = pure w