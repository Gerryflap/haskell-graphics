
module Gol.Logic (
    GolState,
    initialGolState,
    golStateStep,
    safeGet,
    board
) where

import Data.Array.IArray
import Data.Bool


data GolState = GolState {
    board :: Array (Int, Int) Bool
}

-- Initialize board with all True (for now, random gen can come next)
initialGolState :: (Int, Int) -> GolState
initialGolState (w, h) = GolState {board = array ((0, 0), (w-1, h-1)) [((x, y), True) | x <- [0..(w-1)], y <- [0..(h-1)] ] }


golStateStep :: GolState -> GolState
golStateStep state = GolState newBoard
            where
                oldBoard = board state
                newBoard = array (bounds oldBoard) $ map (cellStep state) $ indices oldBoard

-- Step on a single cell: given old state and coordinate this will output the new ((x, y), alive) state
cellStep :: GolState -> (Int, Int) -> ((Int, Int), Bool)
cellStep state (x, y) = ((x, y), newState)
                        where
                            current = safeGet state (x, y)
                            nbs = neighborCount state (x, y)
                            newState = golRules current nbs

-- The fundamental Game of Life rules, returning alive state based on neigbour count
golRules :: Bool -> Int -> Bool
golRules False 3 = True     -- When 3 cells really love eachother...
golRules False _ = False    -- Stays ded :/
golRules True nbs
        | nbs < 2 = False   -- Lonely :(
        | nbs < 4 = True    -- Happy :)
        | otherwise = False -- Overcrowded :(

neighborCount :: GolState -> (Int, Int) -> Int
neighborCount state (x, y) = length $ filter id             -- Filter out empty tiles 
                                    $ map (safeGet state)   -- Get the boolean from the grid
                                    $ [(x + dx, y + dy) |   dx <- [(-1)..1], 
                                                            dy <- [(-1)..1], 
                                                            dx /= 0 || dy /= 0 ] -- Generate list of surrounding tile coordinates

-- Safely gets the value in the grid at x and y, and returns False when out of bounds
safeGet :: GolState -> (Int, Int) -> Bool
safeGet state (x, y)
    | x >= 0 && y >= 0 && x < w && y < h = (board state) ! (x, y)
    | otherwise = False
    where
        (_, (w, h)) = bounds $ board state