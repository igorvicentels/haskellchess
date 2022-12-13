module IO where

import Board
import Types
import Endgame

import System.IO

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: (Int, Int) -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

setColorGrey :: IO ()
setColorGrey = putStr "\ESC[30m"

setColorBlack :: IO ()
setColorBlack = putStr "\ESC[90m"

setColorWhite :: IO ()
setColorWhite = putStr "\ESC[97m"

showRow :: [Tile] -> Int -> IO ()
showRow row n = do putStr $ "| " ++ show n ++ " | "
                   setColorWhite
                   go row
                   setColorGrey
                   putStrLn $ " | " ++ show n ++ " |"
    where go []     = return () 
          go [x]    = case getTeam x of
                        Just Black -> do setColorBlack
                                         putStr $ show x
                                         setColorGrey
                        _          -> do setColorWhite
                                         putStr $ show x
                                         setColorGrey
          go (x:xs) = do case getTeam x of
                            Just Black -> do setColorBlack
                                             putStr $ show x
                                             setColorGrey
                            _          -> do setColorWhite
                                             putStr $ show x
                                             setColorGrey
                         putStr " | "
                         go xs 

showLine :: IO ()
showLine = putStrLn "|---|-------------------------------|---|"

showBoard :: Board -> IO ()
showBoard b = do setColorGrey
                 putStrLn "|---| A | B | C | D | E | F | G | H |---|"
                 showLine
                 go b 8
                 showLine
                 putStrLn "|---| A | B | C | D | E | F | G | H |---|"
    where
        go []     _ = return ()
        go [x]    n = showRow x n
        go (x:xs) n = do showRow x n
                         go xs (n - 1)

run :: Game -> IO ()
run game = do cls
              goto (1,1)
              showBoard $ board game
              run' game 
        
run' :: Game -> IO ()
run' game 
    | isCheckmate game = putStrLn $ "Checkmate! " ++ if even $ turn game then 
                                                          "White wins"    
                                                     else 
                                                          "Black wins"
    | isStalemate game = putStrLn "Stalemate! "  
    | isThreeRepetitions game = putStrLn "Draw (Threefold repetition)"
    | isInsufficientMaterial game = putStrLn "Draw (Insufficient material)"
    | fiftyMovesRule game = putStrLn "Draw (50 moves rule)"
    | otherwise        = do putStr $ if odd $ turn game then 
                                        "White move: "    
                                     else 
                                        "Black move: "
                            l <- readLine
                            run $ move l game

echoless :: IO a -> IO a
echoless ax = do oldEcho <- hGetEcho stdin 
                 hSetEcho stdin False
                 x <- ax
                 hSetEcho stdin oldEcho
                 return x

-- Hutton ch10
readLine :: IO String
readLine = go ""
    where go xs = do c <- echoless getChar
                     if c == '\n' then
                         do putChar '\n'
                            return (reverse xs)
                     else if c == '\DEL' then
                         case xs of
                            [] -> go ""
                            (y:ys) ->
                                do putStr "\b \b"
                                   go ys
                     else
                         do putChar c
                            go $ c:xs