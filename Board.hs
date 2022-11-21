module Board where

data Tile = BPawn
           | BRook
           | BKnight
           | BBishop
           | BQueen
           | BKing
           | WPawn
           | WRook
           | WKnight
           | WBishop
           | WQueen
           | WKing
           | Empty
    
instance Show Tile where
    show BPawn   = "p"
    show BRook   = "r"
    show BKnight = "n"
    show BBishop = "b"
    show BQueen  = "q"
    show BKing   = "k"
    show WPawn   = "P"
    show WRook   = "R"
    show WKnight = "N"
    show WBishop = "B"
    show WQueen  = "Q"
    show WKing   = "K"
    show Empty   = " "

type Board = [[Tile]]
-- TODO: verify number of rows and columns

printRow :: [Tile] -> IO ()
printRow []     = return () 
printRow [x]    = putStrLn $ show x 
printRow (x:xs) = do putStr $ show x 
                     putStr " | "
                     printRow xs

printLine :: IO ()
printLine = putStrLn "------------------------------"

printBoard :: Board -> IO ()
printBoard []     = return ()
printBoard [x]    = printRow x
printBoard (x:xs) = do printRow x
                       printLine 
                       printBoard xs

getPiece :: Int -> Int -> Board -> Tile
getPiece x y b
    | x < 0 || y < 0 || x > 7 || y > 7 = error "invalid index"
    | otherwise                        = b !! y !! x

setTile :: (Int, Int) -> Tile -> Board -> Board
setTile (x, 0) t (r:rs) = setTile' x t r : rs
setTile (x, y) t (r:rs) = r : setTile (x, (y - 1)) t rs 

setTile' :: Int -> Tile -> [Tile] -> [Tile]
setTile' 0 t (y:ys) = t : ys
setTile' x t (y:ys) = y : setTile' (x-1) t ys

movePiece :: (Int, Int) -> (Int, Int) -> Board -> Board
movePiece (x1, y1) (x2, y2) b = 
    case (getPiece x1 y1 b) of
        Empty   -> error ""
        x       -> setTile (x1, y1) Empty (setTile (x2, y2) x b)




testRow1 = [BRook, BKnight, BBishop, BQueen, BKing, BBishop, BKnight, BRook]
testRow2 = [BPawn, BPawn, BPawn, BPawn, BPawn, BPawn, BPawn, BPawn]
testRow3 = [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
testRow4 = [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
testRow5 = [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
testRow6 = [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
testRow7 = [WPawn, WPawn, WPawn, WPawn, WPawn, WPawn, WPawn, WPawn]
testRow8 = [WRook, WKnight, WBishop, WQueen, WKing, WBishop, WKnight, WRook]

testBoard = [ testRow1
            , testRow2
            , testRow3
            , testRow4
            , testRow5
            , testRow6
            , testRow7
            , testRow8 ] 
