module Board where

--  todo: change Tile type

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

showRow :: [Tile] -> IO ()
showRow []     = return () 
showRow [x]    = putStrLn $ show x 
showRow (x:xs) = do putStr $ show x 
                    putStr " | "
                    showRow xs

showLine :: IO ()
showLine = putStrLn "------------------------------"

showBoard :: Board -> IO ()
showBoard []     = return ()
showBoard [x]    = showRow x
showBoard (x:xs) = do showRow x
                      showLine 
                      showBoard xs

getTile :: (Int, Int) -> Board -> Tile
getTile (x, y) b
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
    if canMove (x1, y1) (x2, y2) b 
        then
            let x = getTile (x1, y1) b 
            in setTile (x1, y1) Empty (setTile (x2, y2) x b)
        else 
            b

canMove :: (Int, Int) -> (Int, Int) -> Board -> Bool
canMove (x1, y1) (x2, y2) b = 
    case (getTile (x1, y1) b) of
        Empty -> False





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
