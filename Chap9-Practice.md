module Main where
-- MazeGame.hs

import System.IO
import Data.Char (toLower)

-- Types de mouvements possibles
data Move = GoLeft | GoRight | GoForward deriving (Show, Eq)

-- Structure du labyrinthe
data Maze = Choice Maze Maze Maze | Wall | Exit deriving (Show, Eq)

-- Appliquer un mouvement
move :: Maze -> Move -> Maze
move (Choice left forward right) direction =
  case direction of
    GoLeft    -> left
    GoForward -> forward
    GoRight   -> right
move _ _ = Wall

-- Exemple de labyrinthe
testMaze :: Maze
testMaze =
  Choice
    Wall
    (Choice
      (Choice Exit Wall Wall)
      Wall
      Wall)
    Wall

-- Appliquer tous les mouvements
applyMoves :: Maze -> [Move] -> Maze
applyMoves maze [] = maze
applyMoves maze (m:ms) = applyMoves (move maze m) ms

-- Traduire l'état final en message
showCurrentChoice :: Maze -> String
showCurrentChoice Wall = "You've hit a wall!"
showCurrentChoice Exit = "YOU'VE FOUND THE EXIT!!"
showCurrentChoice (Choice _ _ _) =
  "You're still inside the maze. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."

-- Fonction principale de résolution
solveMaze :: Maze -> [Move] -> String
solveMaze maze moves = showCurrentChoice (applyMoves maze moves)

-- Fonction utilitaire pour parser les entrées
parseMove :: String -> Maybe Move
parseMove s =
  case map toLower s of
    "left"    -> Just GoLeft
    "right"   -> Just GoRight
    "forward" -> Just GoForward
    _         -> Nothing

-- Action main
main :: IO ()
main = do
  putStrLn "Welcome to the Maze Game!"
  putStrLn "Enter your moves as a space-separated list (left, right, forward):"
  putStr "> "
  hFlush stdout
  input <- getLine
  let moveStrings = words input
  let maybeMoves = map parseMove moveStrings
  if all (/= Nothing) maybeMoves
    then do
      let moves = map (\(Just m) -> m) maybeMoves
      let result = solveMaze testMaze moves
      putStrLn $ "\nResult: " ++ result
    else do
      putStrLn "Invalid input detected. Please use only: left, right, forward."

