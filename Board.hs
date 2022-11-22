module Board where

--  todo: change Tile type
data Color = Black
           | White
           deriving(Eq)

data Tile = Pawn Color
          | Rook Color
          | Knight Color
          | Bishop Color
          | Queen Color
          | King Color
          | Empty
          deriving(Eq)
    
instance Show Tile where
    show (Pawn   Black) = "p"
    show (Rook   Black) = "r"
    show (Knight Black) = "n"
    show (Bishop Black) = "b"
    show (Queen  Black) = "q"
    show (King   Black) = "k"
    show (Pawn   White) = "P"
    show (Rook   White) = "R"
    show (Knight White) = "N"
    show (Bishop White) = "B"
    show (Queen  White) = "Q"
    show (King   White) = "K"
    show Empty          = " "

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
    | otherwise                        = b !! x !! y

setTile :: (Int, Int) -> Tile -> Board -> Board
setTile (0, y) t (r:rs) = setTile' y t r : rs
setTile (x, y) t (r:rs) = r : setTile ((x - 1), y) t rs 

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
canMove (x1, y1) (x2, y2) b
    |(x1 == x2) && (y1 == y2) = False
    |otherwise = 
        case (getTile (x1, y1) b) of
            Empty  -> False
            Pawn   c -> canMovePawn (x1, y1) (x2, y2) c b
            Rook   c -> canMoveRook (x1, y1) (x2, y2) c b
            -- Knight -> canMoveKnight (x1, y1) (x2, y2) b
            Bishop c -> canMoveBishop (x1, y1) (x2, y2) c b
            Queen  c -> canMoveQueen (x1, y1) (x2, y2) c b
            -- King   -> canMoveKing (x1, y1) (x2, y2) b

canMovePawn :: (Int, Int) -> (Int, Int) -> Color -> Board -> Bool
canMovePawn (x1, y1) (x2, y2) c b =
    case c of
        Black -> 
            case getTile (x2, y2) b of
                Empty -> 
                    x2 - x1 == 1 || (x1 == 1 && x2 == 3) 
                x     -> 
                    case getColor x of
                        Black -> False
                        White -> 
                            x2 - x1 == 1 && abs (y1 - y2) == 1
        White -> 
            case getTile (x2, y2) b of
                Empty -> 
                    x1 - x2 == 1 || (x1 == 6 &&  x2 == 4) 
                x     -> 
                    case getColor x of
                        White -> False
                        Black -> 
                            x1 - x2 == 1 && abs (y1 - y2) == 1

canMoveRook :: (Int, Int) -> (Int, Int) -> Color -> Board -> Bool
canMoveRook (x1,y1) (x2,y2) c b = (t2 == Empty || getColor t2 /= c) && canMoveRook' (x1,y1) (x2,y2) b 
    where t2 = getTile (x2,y2) b

canMoveRook' :: (Int, Int) -> (Int, Int) -> Board -> Bool
canMoveRook' (x1,y1) (x2,y2) b
    |(x1 == x2) && (y1 == y2) = True  
    |(x1 == x2) = 
        if (y1 < y2)
            then
                case getTile (x1, y1 + 1) b of 
                    Empty -> canMoveRook' (x1, y1 + 1) (x2, y2) b
                    x     -> (y1 + 1) == y2 
            else
                case getTile (x1, y1 - 1) b of 
                    Empty -> canMoveRook' (x1, y1 - 1) (x2, y2) b
                    x     -> (y1 - 1) == y2
    |(y1 == y2) = 
        if (x1 < x2)
            then
                case getTile (x1 + 1, y1) b of 
                    Empty -> canMoveRook' (x1 + 1, y1) (x2, y2) b
                    x     -> (x1 + 1) == x2
            else
                case getTile (x1 - 1, y2) b of 
                    Empty -> canMoveRook' (x1 - 1,y1) (x2, y2) b
                    x     -> (x1 - 1) == x2  
    |otherwise = False

canMoveBishop :: (Int, Int) -> (Int, Int) -> Color -> Board -> Bool
canMoveBishop (x1,y1) (x2,y2) c b = (t2 == Empty || getColor t2 /= c) && canMoveBishop' (x1,y1) (x2,y2) b 
    where t2 = getTile (x2,y2) b

canMoveBishop' :: (Int, Int) -> (Int, Int) -> Board -> Bool
canMoveBishop' (x1,y1) (x2,y2) b
    |(x1 == x2) && (y1 == y2) = True 
    |abs(x1 - x2) /= abs(y1 - y2) || (x1 == x2) || (y1 == y2) = False
    |otherwise = 
        if (y1 < y2) 
            then
                if (x1 < x2)
                    then
                        case getTile (x1 + 1, y1 + 1) b of 
                            Empty -> canMoveBishop' (x1 + 1, y1 + 1) (x2, y2) b
                            x     -> (x1 + 1, y1 + 1) == (x2, y2) 
                    else
                        case getTile (x1 - 1, y1 + 1) b of 
                            Empty -> canMoveBishop' (x1 - 1, y1 + 1) (x2, y2) b
                            x     -> (x1 - 1, y1 + 1) == (x2, y2)
            else
                if (x1 < x2)
                    then
                        case getTile (x1 + 1, y1 - 1) b of 
                            Empty -> canMoveBishop' (x1 + 1, y1 - 1) (x2, y2) b
                            x     -> (x1 + 1, y1 - 1) == (x2, y2) 
                    else
                        case getTile (x1 - 1, y1 - 1) b of 
                            Empty -> canMoveBishop' (x1 - 1, y1 - 1) (x2, y2) b
                            x     -> (x1 - 1, y1 - 1) == (x2, y2)


canMoveQueen :: (Int, Int) -> (Int, Int) -> Color -> Board -> Bool
canMoveQueen (x1, y1) (x2, y2) c b = canMoveBishop (x1, y1) (x2, y2) c b || canMoveRook (x1, y1) (x2, y2) c b

getColor :: Tile -> Color
getColor Empty = error ""
getColor (Pawn White) = White
getColor (Rook White) = White
getColor (Bishop White) = White
getColor (Knight White) = White
getColor (Queen White) = White
getColor (King White) = White
getColor _            = Black


testRow1 = [Rook Black, Knight Black, Bishop Black, Queen Black, King Black, Bishop Black, Knight Black, Rook Black]
testRow2 = [Pawn Black, Pawn Black, Pawn Black, Pawn Black, Pawn Black, Pawn Black, Pawn Black, Pawn Black]
testRow3 = [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
testRow4 = [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
testRow5 = [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
testRow6 = [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
testRow7 = [Pawn White, Pawn White, Pawn White, Pawn White, Pawn White, Pawn White, Pawn White, Pawn White]
testRow8 = [Rook White, Knight White, Bishop White, Queen White, King White, Bishop White, Knight White, Rook White]

testBoard = [ testRow1
            , testRow2
            , testRow3
            , testRow4
            , testRow5
            , testRow6
            , testRow7
            , testRow8 ] 

b1 = movePiece (1, 0) (3, 0) testBoard
b2 = movePiece (6, 1) (4, 1) b1
b3 = movePiece (3,0) (4,1) b2
 