module Board where

import Data.Maybe

data Team = Black
          | White
    deriving( Show, Eq )

type Coord = (Int, Int)

type Castle = (Bool, Bool, Bool, Bool) 

type Move = (Coord, Coord)

data Tile = Pawn Team
          | Rook Team
          | Knight Team
          | Bishop Team
          | Queen Team
          | King Team
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

data Game = Game { board :: Board
                 , turn :: Int
                 , castle :: Castle
                 , movesList :: [Move]
                 }
        deriving ( Show )

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

getTile :: Coord -> Board -> Maybe Tile
getTile (x, y) b
    | x < 0 || y < 0 || x > 7 || y > 7 = Nothing
    | otherwise                        = Just $ b !! x !! y

setTile :: Coord -> Tile -> Board -> Board
setTile (0, y) t (r:rs) = setTile' y t r : rs
setTile (x, y) t (r:rs) = r : setTile ((x - 1), y) t rs 

setTile' :: Int -> Tile -> [Tile] -> [Tile]
setTile' 0 t (y:ys) = t : ys
setTile' x t (y:ys) = y : setTile' (x-1) t ys

movePiece :: Coord -> Coord -> Game -> Game
movePiece (x1, y1) (x2, y2) game = 
    if canMove (x1, y1) (x2, y2) game
        then
            case t of
                Nothing -> game
                Just (King c) ->
                    if abs(y1 - y2) == 2
                        then
                            game { board = movePiecesinCastle (x1, y1) (x2, y2) c b
                                 , movesList = m : m'
                                 , turn = turn' + 1 }
                        else
                            game { board = setTile (x1, y1) Empty (setTile (x2, y2) (King c) b)
                                 , movesList = m : m'
                                 , turn = turn' + 1 }

                Just (Pawn White) ->
                    if (x1 == 3 && abs (y1 - y2) == 1 && getTile (x2,y2) b == Just Empty) 
                        then game { board = moveEnPassant (x1, y1) (x2, y2) White b
                                  , movesList = m : m'
                                  , turn = turn' + 1 }
                        else game { board = setTile (x1, y1) Empty (setTile (x2, y2) (Pawn White) b)
                                  , movesList = m : m'
                                  , turn = turn' + 1 }

                Just (Pawn Black) ->
                    if (x1 == 4 && abs (y1 - y2) == 1 && getTile (x2,y2) b == Just Empty) 
                        then game { board = moveEnPassant (x1, y1) (x2, y2) Black b
                                  , movesList = m : m'
                                  , turn = turn' + 1 }
                        else game { board = setTile (x1, y1) Empty (setTile (x2, y2) (Pawn Black) b)
                                  , movesList = m : m'
                                  , turn = turn' + 1 }

                Just t' -> game { board = setTile (x1, y1) Empty (setTile (x2, y2) t' b)
                                , movesList = m : m'
                                , turn = turn' + 1 }
        else 
            game
    where t = getTile (x1, y1) b 
          b = board game
          m = ((x1,y1), (x2,y2))
          m' = movesList game
          turn' = turn game


--Auxiliar Function
movePiecesinCastle :: Coord -> Coord -> Team -> Board -> Board
movePiecesinCastle (x1, y1) (x2, y2) c b
    | y2 == 2 = setTile (x1, 0) Empty (setTile (x2, 3) (Rook c) moveKing)
    | y2 == 6 = setTile (x1, 7) Empty (setTile (x2, 5) (Rook c) moveKing)
    | otherwise = b -- "unnecessary"
    where moveKing = setTile (x1, y1) Empty (setTile (x2, y2) (King c) b)

moveEnPassant :: Coord -> Coord -> Team -> Board -> Board
moveEnPassant (x1, y1) (x2, y2) c b = setTile (x1,y1) Empty (setTile (x1, y2) Empty (setTile (x2,y2) (Pawn c) b))

    -- TODO: Check if tile 2 is inside the board
canMove :: Coord -> Coord -> Game -> Bool
canMove (x1, y1) (x2, y2) game
    | (x1 == x2) && (y1 == y2) = False
    | team1 == team2           = False
    | not isPlayerTurn         = False
    | otherwise = 
        case (getTile (x1, y1) b) of
            Nothing         -> False
            Just (Empty)    -> False
            Just (Pawn   c) -> canMovePawn (x1, y1) (x2, y2) c b || enPassant (x1, y1) (x2,y2) c game
            Just (Rook   c) -> canMoveRook (x1, y1) (x2, y2) c b
            Just (Knight c) -> canMoveKnight (x1, y1) (x2, y2) c b
            Just (Bishop c) -> canMoveBishop (x1, y1) (x2, y2) c b
            Just (Queen  c) -> canMoveQueen (x1, y1) (x2, y2) c b
            Just (King   c) -> canMoveKing (x1, y1) (x2, y2) c b || canMakeCastle (x1, y1) (x2, y2) c game
    where team1 = fmap getTeam (getTile (x1, y1) b)
          team2 = fmap getTeam (getTile (x2, y2) b)
          b = board game
          turn' = turn game
          isPlayerTurn = (team1 == Just (Just White) && turn' `mod` 2 == 1) || (team1 == Just (Just Black) && turn' `mod` 2 == 0)

canMovePawn :: Coord -> Coord -> Team -> Board -> Bool
canMovePawn (x1, y1) (x2, y2) c b =
    case c of
        Black -> 
            case getTile (x2, y2) b of
                Nothing -> False
                Just Empty -> 
                    (x2 - x1 == 1 || (x1 == 1 && x2 == 3)) && y1 == y2
                Just x     -> 
                    case getTeam x of
                        Just Black -> False
                        Just White -> x2 - x1 == 1 && abs (y1 - y2) == 1
        White -> 
            case getTile (x2, y2) b of
                Nothing -> False
                Just Empty -> 
                    (x1 - x2 == 1 || (x1 == 6 &&  x2 == 4)) && y1 == y2 
                Just x     -> 
                    case getTeam x of
                        Just White -> False
                        Just Black -> x1 - x2 == 1 && abs (y1 - y2) == 1

canMoveRook :: Coord -> Coord -> Team -> Board -> Bool
canMoveRook (x1,y1) (x2,y2) c b
    | (x1 == x2) && (y1 == y2) = True
    | (x1 == x2) = 
        if (y1 < y2)
            then
                case getTile (x1, y1 + 1) b of 
                    Nothing -> False
                    Just Empty -> canMoveRook (x1, y1 + 1) (x2, y2) c b
                    Just x     -> (y1 + 1) == y2 
            else
                case getTile (x1, y1 - 1) b of 
                    Nothing -> False
                    Just Empty -> canMoveRook (x1, y1 - 1) (x2, y2) c b
                    Just x     -> (y1 - 1) == y2
    | (y1 == y2) = 
        if (x1 < x2)
            then
                case getTile (x1 + 1, y1) b of 
                    Nothing -> False
                    Just Empty -> canMoveRook (x1 + 1, y1) (x2, y2) c b
                    Just x     -> (x1 + 1) == x2
            else
                case getTile (x1 - 1, y2) b of 
                    Nothing -> False
                    Just Empty -> canMoveRook (x1 - 1,y1) (x2, y2) c b
                    Just x     -> (x1 - 1) == x2  
    | otherwise = False

canMoveBishop :: Coord -> Coord -> Team -> Board -> Bool
canMoveBishop (x1,y1) (x2,y2) c b
    | (x1 == x2) && (y1 == y2) = True
    | abs(x1 - x2) /= abs(y1 - y2) = False
    | otherwise = 
        if (y1 < y2) 
            then
                if (x1 < x2)
                    then
                        case getTile (x1 + 1, y1 + 1) b of 
                            Nothing -> False
                            Just Empty -> canMoveBishop (x1 + 1, y1 + 1) (x2, y2) c b
                            Just x     -> (x1 + 1, y1 + 1) == (x2, y2) 
                    else
                        case getTile (x1 - 1, y1 + 1) b of 
                            Nothing -> False
                            Just Empty -> canMoveBishop (x1 - 1, y1 + 1) (x2, y2) c b
                            Just x     -> (x1 - 1, y1 + 1) == (x2, y2)
            else
                if (x1 < x2)
                    then
                        case getTile (x1 + 1, y1 - 1) b of 
                            Nothing -> False
                            Just Empty -> canMoveBishop (x1 + 1, y1 - 1) (x2, y2) c b
                            Just x     -> (x1 + 1, y1 - 1) == (x2, y2) 
                    else
                        case getTile (x1 - 1, y1 - 1) b of 
                            Nothing -> False
                            Just Empty -> canMoveBishop (x1 - 1, y1 - 1) (x2, y2) c b
                            Just x     -> (x1 - 1, y1 - 1) == (x2, y2)


canMoveQueen :: Coord -> Coord -> Team -> Board -> Bool
canMoveQueen (x1, y1) (x2, y2) c b = canMoveBishop (x1, y1) (x2, y2) c b || canMoveRook (x1, y1) (x2, y2) c b

canMoveKnight :: Coord -> Coord -> Team -> Board -> Bool
canMoveKnight (x1, y1) (x2, y2) c b =
    ((abs (x1 - x2) == 1 && abs (y1 - y2) == 2) || (abs (x1 - x2) == 2 && abs (y1 - y2) == 1))

canMoveKing :: Coord -> Coord -> Team -> Board -> Bool
canMoveKing (x1, y1) (x2, y2) c b = abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1

isAttacked :: Coord -> Board -> Bool
isAttacked (x,y) b =  
    case getTile (x,y) b of
        Nothing -> False -- Check if the return type must be maybe bool
        Just t  -> 
            isAttackedByPawn (x,y)   mt b ||
            isAttackedLine   (x,y)   mt b ||
            isAttackedByKnight (x,y) mt b 
            -- isAttackedByKing (x,y) mt b
            where mt = getTeam t

isAttackedByPawn :: Coord -> Maybe Team -> Board -> Bool
isAttackedByPawn (x,y) Nothing      b = False -- TODO: Check what to do here (how to use this functions to check is is possible to castle)
isAttackedByPawn (x,y) (Just White) b =
    getTile (x - 1, y - 1) b == Just (Pawn Black) ||
    getTile (x - 1, y + 1) b == Just (Pawn Black)
isAttackedByPawn (x,y) (Just Black) b =
    getTile (x + 1, y - 1) b == Just (Pawn White) ||
    getTile (x + 1, y + 1) b == Just (Pawn White)

isAttackedLine :: Coord -> Maybe Team -> Board -> Bool
isAttackedLine (x,y) Nothing b = False -- TODO: Check what to do here (how to use this functions to check is is possible to castle)
isAttackedLine (x,y) t       b = 
    isAttackedLine' (x,y) succ id   t b ||
    isAttackedLine' (x,y) succ pred t b ||
    isAttackedLine' (x,y) succ succ t b ||
    isAttackedLine' (x,y) pred id   t b ||
    isAttackedLine' (x,y) pred pred t b ||
    isAttackedLine' (x,y) pred succ t b ||
    isAttackedLine' (x,y) id   pred t b ||
    isAttackedLine' (x,y) id   succ t b

-- TODO: check t2 type
isAttackedLine' :: Coord -> (Int -> Int) -> (Int -> Int) -> Maybe Team -> Board -> Bool
isAttackedLine' (x,y) _ _ Nothing b = False  -- "unnecessary"
isAttackedLine' (x,y) f g t1      b = 
    case fmap getTeam (getTile (f x, g y) b) of
        Nothing -> False
        Just t2 ->
            if t1 == t2 
                then
                    False
                else
                    case getTile (f x, g y) b of
                        Just (Queen t2)  -> True
                        Just (Rook t2)   -> f x == id x || g y == id y
                        Just (Bishop t2) -> f x /= id x && g y /= id y
                        Just Empty       -> isAttackedLine' (f x, g y) f g t1 b
                        _                -> False


isAttackedByKnight :: Coord -> Maybe Team -> Board -> Bool
isAttackedByKnight (x,y) Nothing      b = False -- TODO: Check what to do here (how to use this functions to check is is possible to castle)
isAttackedByKnight (x,y) (Just White) b = 
    getTile (x - 1, y - 2) b == Just (Knight Black) ||
    getTile (x - 1, y + 2) b == Just (Knight Black) ||
    getTile (x + 1, y - 2) b == Just (Knight Black) ||
    getTile (x + 1, y + 2) b == Just (Knight Black) ||
    getTile (x - 2, y - 1) b == Just (Knight Black) ||
    getTile (x - 2, y + 1) b == Just (Knight Black) ||
    getTile (x + 2, y - 1) b == Just (Knight Black) ||
    getTile (x + 2, y + 1) b == Just (Knight Black)
isAttackedByKnight (x,y) (Just Black) b = 
    getTile (x - 1, y - 2) b == Just (Knight White) ||
    getTile (x - 1, y + 2) b == Just (Knight White) ||
    getTile (x + 1, y - 2) b == Just (Knight White) ||
    getTile (x + 1, y + 2) b == Just (Knight White) ||
    getTile (x - 2, y - 1) b == Just (Knight White) ||
    getTile (x - 2, y + 1) b == Just (Knight White) ||
    getTile (x + 2, y - 1) b == Just (Knight White) ||
    getTile (x + 2, y + 1) b == Just (Knight White)


canMakeCastle :: Coord -> Coord -> Team -> Game -> Bool
canMakeCastle (x1, y1) (x2, y2) c game
    | x1 /= x2 || (x1 /= 0 && x1 /= 7) = False
    | y1 == 4 && y2 == 2 && x1 == 0    = m1 && canMakeCastle' (x1, y1) (x2, y2) c game 
    | y1 == 4 && y2 == 6 && x1 == 0    = m2 && canMakeCastle' (x1, y1) (x2, y2) c game 
    | y1 == 4 && y2 == 2 && x1 == 7    = m3 && canMakeCastle' (x1, y1) (x2, y2) c game 
    | y1 == 4 && y2 == 6 && x1 == 7    = m4 && canMakeCastle' (x1, y1) (x2, y2) c game 
    | otherwise                        = False
    where (m1,m2,m3,m4) = castle game

--TODO: check the rule of Castle when the king is already in check 
canMakeCastle' :: Coord -> Coord -> Team -> Game -> Bool
canMakeCastle' (x1, y1) (x2, y2) c game
    | y1 == 4 && y2 == 6 = isEmpty (x2, 5) b && isEmpty (x2, 6) b && (canMove (x1, 4) (x2, 5) game) && (canMove (x2, 5) (x2, 6) (movePiece (x1, 4) (x2, 5) game))
    | y1 == 4 && y2 == 2 = isEmpty (x2, 3) b && isEmpty (x2, 2) b && isEmpty (x2, 1) b && (canMove (x1, 4) (x2, 3) game) && (canMove (x2, 3) (x2, 2) (movePiece (x1, 4) (x2, 3) game)) 
    | otherwise = False
    where b = board game 
          isEmpty(x, y) b = getTile (x, y) b == Just Empty

enPassant :: Coord -> Coord -> Team -> Game -> Bool
enPassant (x1,y1) (x2,y2) White game =
    x1 == 3 && 
    getTile (x1, y2) (board game) == Just (Pawn Black) &&
    head (movesList game) == ((1, y2), (3, y2))
enPassant (x1,y1) (x2,y2) Black game = 
    x1 == 4 && 
    getTile (x1, y2) (board game) == Just (Pawn White) &&
    head (movesList game) == ((6, y2), (4, y2))


getTeam :: Tile -> Maybe Team
getTeam Empty          = Nothing
getTeam (Pawn White)   = Just White
getTeam (Rook White)   = Just White
getTeam (Bishop White) = Just White
getTeam (Knight White) = Just White
getTeam (Queen White)  = Just White
getTeam (King White)   = Just White
getTeam _              = Just Black


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

g1 = Game {board = testBoard, turn = 1, castle = (True, True, True, True), movesList = []}

b1  = movePiece (6, 1) (4, 1) g1 -- Wpawn avança duas casas 
b2  = movePiece (7, 2) (5, 0) b1 -- Wbishop avança diag sup esq
b3  = movePiece (5, 0) (4, 1) b2 -- movimento invalido do Wbishop por peça do mesmo time
b4  = movePiece (4, 1) (3, 1) b3 -- wpawn avança uma casa
b5  = movePiece (5, 0) (1, 4) b4 -- wbishop captura (para diag sup dir) bpawn
b6  = movePiece (1, 4) (4, 7) b5 -- Wbishop move diag inf dir
b7  = movePiece (4, 7) (5, 6) b6 -- Wbishop move diag inf esq
b8  = movePiece (0, 5) (4, 1) b7 -- Bbishop move diag inf esq
b9  = movePiece (4, 1) (7, 4) b8 -- mov inválido do Bbishop por sentido de movimento ocupado
b10 = movePiece (4, 1) (6, 3) b9 -- Bbishop captura (para diag inf dir) Wpawn
b11 = movePiece (6, 3) (4, 3) b10 -- mov inválido do Bbishop (vertical)
b12 = movePiece (5, 6) (5, 0) b11 -- mov inválido do Wbishop (horizontal)

r1  = movePiece (6, 0) (4, 0) g1  
r2  = movePiece (7, 0) (5, 0) r1 --movimento Wrook vertical cima
r3  = movePiece (5, 0) (5, 7) r2 --movimento Wrook horizontal
r4  = movePiece (5, 7) (1, 7) r3 --Wrook captura (para cima) Bpawn
r5  = movePiece (1, 7) (1, 6) r4 --Wrook captura (para esq) Bpawn
r6  = movePiece (1, 6) (0, 5) r5 --mov invalido Wrook diagonal
r7  = movePiece (1, 6) (4, 6) r6 --movimento Wrook vertical baixo
r8  = movePiece (4, 6) (4, 1) r7 --movimento Wrook horizontal esq
r9  = movePiece (4, 1) (0, 1) r8 --mov invalido Wrook por sentido de mov ocupado

n1  = movePiece (7, 1) (5, 0) g1 -- mov Wknight sup esq
n2  = movePiece (5, 0) (3, 1) n1 --mov Wknight sup dir
n3  = movePiece (3, 1) (1, 2) n2 --Wknight captura (para direção sup dir) Bpawn
n4  = movePiece (1, 2) (2, 3) n3 -- movimento invalido Wknight
n5  = movePiece (1, 2) (3, 3) n4 -- movimento Wknight inf dir
n6  = movePiece (3, 3) (5, 2) n5 -- movimento Wknight inf esq
n7  = movePiece (5, 2) (6, 4) n6 -- movimento invalido Wknight por tile ocupado pela mesma cor 
n8  = movePiece (5, 2) (4, 4) n7 -- movimento Wknight dir sup 

k1  = movePiece (6, 4) (4, 4) g1 -- mov Wpawn cima
k2  = movePiece (7, 4) (5, 4) k1 --mov invalido (pular uma casa) Wking sup 
k3  = movePiece (7 ,4) (6, 4) k2 --mov Wking cima
k4  = movePiece (6 ,4) (5, 4) k3 -- mov Wking cima
k5  = movePiece (5 ,4) (5, 5) k4 -- mov Wking direita 
k6  = movePiece (5 ,4) (5, 3) k4 -- mov Wking esquerda 
k7  = movePiece (5 ,4) (4, 5) k4 -- mov Wking direita cima
k8  = movePiece (5 ,4) (4, 3) k4 -- mov Wking esquerda cima
k9  = movePiece (5 ,4) (6, 5) k4 -- mov Wking para tile ocupada por mesmo time
k10  = movePiece (6, 3) (4, 3) k4 -- mov Wpawn cima
k11  = movePiece (5 ,4) (6, 3) k10 -- mov Wking baixo esq
k12  = movePiece (6, 3) (7, 4) k11 -- mov Wking baixo dir

c0 = movePiece (6, 2) (4, 2) g1 --mov wpawn
c1 = movePiece (7, 1) (5, 2) c0        --mov wknight
c2 = movePiece (6, 1) (4, 1) c1        --mov wpawn
c3 = movePiece (7, 2) (5, 0) c2        --mov wbishop
c4 = movePiece (7, 3) (6, 2) c3        --mov wqueen
c5 = movePiece (6, 6) (4, 6) c4        --mov wpawn
c6 = movePiece (7, 6) (5, 5) c5        --mov wknight
c7 = movePiece (7, 5) (5, 7) c6        --mov wbishop
c8 = movePiece (7, 4) (7, 6) c7        --castle dir 
c9 = movePiece (7, 4) (7, 2) c7        --castle esq

e1 = movePiece (6, 1) (4, 1) g1  
e2 = movePiece (1, 2) (3, 2) e1
e3 = movePiece (4, 1) (3, 1) e2
e4 = movePiece (1, 0) (3, 0) e3
e5 = movePiece (3, 1) (2, 2) e4  -- mov invalido (peão a ser capturado não havia se movido na jogada anterior)
e6 = movePiece (3, 1) (2, 0) e4  -- en passant certo
e7 = movePiece (3, 2) (4, 2) e6  
e8 = movePiece (6, 3) (4, 3) e7  
e9 = movePiece (4, 2) (5, 3) e8  -- en passant certo
