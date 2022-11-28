module Board where

import Data.Maybe
import Data.List

data Team = Black
          | White
    deriving( Show, Eq )

type Coord = (Int, Int)

type Castle = (Bool, Bool, Bool, Bool) 

data Move = N Coord Coord
          | PP Coord Coord Tile
          deriving ( Eq, Show )

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
                 , wking :: Coord
                 , bking :: Coord
                 , fiftyMovesCounter :: Int
                 , boards :: [Board] 
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
getTile (file, rank) b
    | rank < 0 || file < 0 || rank > 7 || file > 7 = Nothing
    | otherwise                        = Just $ b !! rank !! file

getTeam :: Tile -> Maybe Team
getTeam Empty          = Nothing
getTeam (Pawn White)   = Just White
getTeam (Rook White)   = Just White
getTeam (Bishop White) = Just White
getTeam (Knight White) = Just White
getTeam (Queen White)  = Just White
getTeam (King White)   = Just White
getTeam _              = Just Black

setTile :: Coord -> Tile -> Board -> Board
setTile (file, 0) t (r:rs) = setTile' file t r : rs
setTile (file, rank) t (r:rs) = r : setTile (file, rank - 1) t rs 

setTile' :: Int -> Tile -> [Tile] -> [Tile]
setTile' 0 t (y:ys) = t : ys
setTile' x t (y:ys) = y : setTile' (x-1) t ys

movePiece :: Move -> Game -> Game
movePiece (N (file1, rank1) (file2, rank2)) game =
    if canMove' && not isPP
        then 
            if isPawnMove || isCapture 
                then 
                    newgame { movesList = m : ms
                            , turn = turn' + 1
                            , fiftyMovesCounter = 0
                            , boards = [board newgame] }
                else
                    newgame { movesList = m : ms
                            , turn = turn' + 1
                            , fiftyMovesCounter = counter + 1 
                            , boards = board newgame : boards newgame
                            }
        else game
    where newgame    = movePiece' (N (file1, rank1) (file2, rank2)) game
          canMove'   = canMove (file1, rank1) (file2, rank2) game 
          m          = N (file1,rank1) (file2, rank2)
          ms         = movesList newgame
          turn'      = turn newgame
          counter    = fiftyMovesCounter newgame
          isPP       = ((getTile (file1, rank1) (board game)) == Just (Pawn Black) && rank1 == 6) || ((getTile (file1, rank1) (board game)) == Just (Pawn White) && rank1 == 1)
          isPawnMove = getTile (file1, rank1) (board game) == Just (Pawn White) || getTile (file1, rank1) (board game) == Just (Pawn Black)
          isCapture  = getTile (file2, rank2) (board game) /= Just Empty
movePiece (PP (file1, rank1) (file2, rank2) tile) game =
    if canMove' && isPP
        then newgame { movesList = m : ms
                     , turn = turn' + 1
                     , fiftyMovesCounter = 0 
                     , boards = [] }
        else game
    where newgame  = movePiece' (PP (file1, rank1) (file2, rank2) tile) game
          canMove' = canMove (file1, rank1) (file2, rank2) game 
          m        = PP (file1,rank1) (file2, rank2) tile
          ms       = movesList newgame
          turn'    = turn newgame
          isPP     = ((getTile (file1, rank1) (board game)) == Just (Pawn Black) && rank1 == 6) || ((getTile (file1, rank1) (board game)) == Just (Pawn White) && rank1 == 1)

movePiece' :: Move -> Game -> Game
movePiece' (N (file1, rank1) (file2, rank2)) game = 
    case t of
        Nothing -> game
        Just (King c) ->
            if abs(file1 - file2) == 2
                then
                    if c == Black
                        then
                            game { board = movePiecesinCastle (file1, rank1) (file2, rank2) c b
                                 , bking = (file2, rank2)}
                        else
                            game { board = movePiecesinCastle (file1, rank1) (file2, rank2) c b
                                 , wking = (file2, rank2)}
                else
                    if c == Black
                        then
                            game { board = setTile (file1, rank1) Empty (setTile (file2, rank2) (King c) b)
                                 , bking = (file2, rank2)}
                        else
                            game { board = setTile (file1, rank1) Empty (setTile (file2, rank2) (King c) b)
                                 , wking = (file2, rank2)}

        Just (Pawn White) ->
            if (rank1 == 3 && abs (file1 - file2) == 1 && getTile (file2, rank2) b == Just Empty) 
                then 
                    game { board = moveEnPassant (file1, rank1) (file2, rank2) White b}
                else 
                    if rank1 == 1 
                        then
                            game
                        else
                            game { board = setTile (file1, rank1) Empty (setTile (file2, rank2) (Pawn White) b)}

        Just (Pawn Black) ->
            if (rank1 == 4 && abs (file1 - file2) == 1 && getTile (file2,rank2) b == Just Empty) 
                then game { board = moveEnPassant (file1, rank1) (file2, rank2) Black b}
                else
                    if rank1 == 6 
                        then
                            game
                        else 
                            game { board = setTile (file1, rank1) Empty (setTile (file2, rank2) (Pawn Black) b)}

        Just t' -> game { board = setTile (file1, rank1) Empty (setTile (file2, rank2) t' b)}
    where t = getTile (file1, rank1) b 
          b = board game
movePiece' (PP (file1, rank1) (file2, rank2) tile) game =
    case t of
        Just (Pawn White) ->
            if rank1 == 1 
                then
                    pawnPromotion (PP (file1, rank1) (file2, rank2) tile) game
                else 
                    game
        Just (Pawn Black) ->
            if rank1 == 6 
                then
                    pawnPromotion (PP (file1, rank1) (file2, rank2) tile) game
                else 
                    game   
        where t = getTile (file1, rank1) b
              b = board game


--Auxiliar Function
movePiecesinCastle :: Coord -> Coord -> Team -> Board -> Board
movePiecesinCastle (file1, rank1) (file2, rank2) c b
    | file2 == 2 = setTile (0, rank1) Empty (setTile (3, rank2) (Rook c) moveKing)
    | file2 == 6 = setTile (7, rank1) Empty (setTile (5, rank2) (Rook c) moveKing)
    | otherwise = b -- "unnecessary"
    where moveKing = setTile (file1, rank1) Empty (setTile (file2, rank2) (King c) b)

moveEnPassant :: Coord -> Coord -> Team -> Board -> Board
moveEnPassant (file1, rank1) (file2, rank2) c b = setTile (file1, rank1) Empty (setTile (file2, rank1) Empty (setTile (file2, rank2) (Pawn c) b))

    -- TODO: Check if tile 2 is inside the board
    -- TODO: Refactor this function
canMove :: Coord -> Coord -> Game -> Bool
canMove (file1, rank1) (file2, rank2) game
    | (rank1 == rank2) && (file1 == file2) = False
    | team1 == team2           = False
    | not isPlayerTurn         = False
    | otherwise = canMovePiece && not(isChecked newgame)        
    where team1 = fmap getTeam (getTile (file1, rank1) b)
          team2 = fmap getTeam (getTile (file2, rank2) b)
          b = board game
          turn' = turn game
          isPlayerTurn = (team1 == Just (Just White) && turn' `mod` 2 == 1) || (team1 == Just (Just Black) && turn' `mod` 2 == 0)
          canMovePiece = 
            case (getTile (file1, rank1) b) of
                Nothing         -> False
                Just (Empty)    -> False
                Just (Pawn   c) -> canMovePawn (file1, rank1) (file2, rank2) c b || enPassant (file1, rank1) (file2, rank2) c game
                Just (Rook   c) -> canMoveRook (file1, rank1) (file2, rank2) c b
                Just (Knight c) -> canMoveKnight (file1, rank1) (file2, rank2) c b
                Just (Bishop c) -> canMoveBishop (file1, rank1) (file2, rank2) c b
                Just (Queen  c) -> canMoveQueen (file1, rank1) (file2, rank2) c b
                Just (King   c) -> canMoveKing (file1, rank1) (file2, rank2) c b || canMakeCastle (file1, rank1) (file2, rank2) c game  
          newgame = movePiece' (N (file1, rank1) (file2, rank2)) game


canMovePawn :: Coord -> Coord -> Team -> Board -> Bool
canMovePawn (file1, rank1) (file2, rank2) c b =
    case c of
        Black -> 
            case getTile (file2, rank2) b of
                Nothing -> False
                Just Empty -> 
                    (rank2 - rank1 == 1 || (rank1 == 1 && rank2 == 3)) && file1 == file2
                Just x     -> 
                    case getTeam x of
                        Just Black -> False
                        Just White -> rank2 - rank1 == 1 && abs (file1 - file2) == 1
        White -> 
            case getTile (file2, rank2) b of
                Nothing -> False
                Just Empty -> 
                    (rank1 - rank2 == 1 || (rank1 == 6 &&  rank2 == 4)) && file1 == file2 
                Just x     -> 
                    case getTeam x of
                        Just White -> False
                        Just Black -> rank1 - rank2 == 1 && abs (file1 - file2) == 1

canMoveRook :: Coord -> Coord -> Team -> Board -> Bool
canMoveRook (file1, rank1) (file2, rank2) c b
    | (rank1 == rank2) && (file1 == file2) = True
    | (rank1 == rank2) = 
        if (file1 < file2)
            then
                case getTile (file1 + 1, rank1) b of 
                    Nothing -> False
                    Just Empty -> canMoveRook (file1 + 1, rank1) (file2, rank2) c b
                    Just x     -> (file1 + 1) == file2 
            else
                case getTile (file1 - 1, rank1) b of 
                    Nothing -> False
                    Just Empty -> canMoveRook (file1 - 1, rank1) (file2, rank2) c b
                    Just x     -> (file1 - 1) == file2
    | (file1 == file2) = 
        if (rank1 < rank2)
            then 
                case getTile (file1, rank1 + 1) b of 
                    Nothing -> False
                    Just Empty -> canMoveRook (file1, rank1 + 1) (file2, rank2) c b
                    Just x     -> (rank1 + 1) == rank2
            else 
                case getTile (file1, rank1 - 1) b of 
                    Nothing -> False
                    Just Empty -> canMoveRook (file1, rank1 - 1) (file2, rank2) c b
                    Just x     -> (rank1 - 1) == rank2  
    | otherwise = False

canMoveBishop :: Coord -> Coord -> Team -> Board -> Bool
canMoveBishop (file1, rank1) (file2, rank2) c b
    | (rank1 == rank2) && (file1 == file2) = True
    | abs(rank1 - rank2) /= abs(file1 - file2) = False
    | otherwise = 
        if (file1 < file2) 
            then
                if (rank1 < rank2)
                    then
                        case getTile (file1 + 1, rank1 + 1) b of 
                            Nothing -> False
                            Just Empty -> canMoveBishop (file1 + 1, rank1 + 1) (file2, rank2) c b
                            Just x     -> (file1 + 1, rank1 + 1) == (file2, rank2) 
                    else 
                        case getTile (file1 + 1, rank1 - 1) b of 
                            Nothing -> False
                            Just Empty -> canMoveBishop (file1 + 1, rank1 - 1) (file2, rank2) c b
                            Just x     -> (file1 + 1, rank1 - 1) == (file2, rank2)
            else
                if (rank1 < rank2)
                    then 
                        case getTile (file1 - 1, rank1 + 1) b of 
                            Nothing -> False
                            Just Empty -> canMoveBishop (file1 - 1, rank1 + 1) (file2, rank2) c b
                            Just x     -> (file1 - 1, rank1 + 1) == (file2, rank2) 
                    else 
                        case getTile (file1 - 1, rank1 - 1) b of 
                            Nothing -> False
                            Just Empty -> canMoveBishop (file1 - 1, rank1 - 1) (file2, rank2) c b
                            Just x     -> (file1 - 1, rank1 - 1) == (file2, rank2)


canMoveQueen :: Coord -> Coord -> Team -> Board -> Bool
canMoveQueen (file1, rank1) (file2, rank2) c b = canMoveBishop (file1, rank1) (file2, rank2) c b || canMoveRook (file1, rank1) (file2, rank2) c b

canMoveKnight :: Coord -> Coord -> Team -> Board -> Bool
canMoveKnight (file1, rank1) (file2, rank2) c b =
    ((abs (rank1 - rank2) == 1 && abs (file1 - file2) == 2) || (abs (rank1 - rank2) == 2 && abs (file1 - file2) == 1))

canMoveKing :: Coord -> Coord -> Team -> Board -> Bool
canMoveKing (file1, rank1) (file2, rank2) c b = abs (rank1 - rank2) <= 1 && abs (file1 - file2) <= 1

isAttacked :: Coord -> Board -> Bool
isAttacked (file, rank) b =  
    case getTile (file, rank) b of
        Nothing -> False -- Check if the return type must be maybe bool
        Just t  -> 
            isAttackedByPawn (file, rank)   mt b ||
            isAttackedLine   (file, rank)   mt b ||
            isAttackedByKnight (file, rank) mt b 
            -- isAttackedByKing (file, rank) mt b
            where mt = getTeam t

isAttackedByPawn :: Coord -> Maybe Team -> Board -> Bool
isAttackedByPawn (file, rank) Nothing      b = False -- TODO: Check what to do here (how to use this functions to check is is possible to castle)
isAttackedByPawn (file, rank) (Just White) b =
    getTile (file - 1, rank - 1) b == Just (Pawn Black) ||
    getTile (file + 1, rank - 1) b == Just (Pawn Black)
isAttackedByPawn (file, rank) (Just Black) b =
    getTile (file - 1, rank + 1) b == Just (Pawn White) ||
    getTile (file + 1, rank + 1) b == Just (Pawn White)

isAttackedLine :: Coord -> Maybe Team -> Board -> Bool
isAttackedLine (file, rank) Nothing b = False -- TODO: Check what to do here (how to use this functions to check is is possible to castle)
isAttackedLine (file, rank) t       b = 
    isAttackedLine' (file, rank) id   succ t b ||
    isAttackedLine' (file, rank) pred succ t b ||
    isAttackedLine' (file, rank) succ succ t b ||
    isAttackedLine' (file, rank) id   pred t b ||
    isAttackedLine' (file, rank) pred pred t b ||
    isAttackedLine' (file, rank) succ pred t b ||
    isAttackedLine' (file, rank) pred id   t b ||
    isAttackedLine' (file, rank) succ id   t b

-- TODO: check t2 type
isAttackedLine' :: Coord -> (Int -> Int) -> (Int -> Int) -> Maybe Team -> Board -> Bool
isAttackedLine' (file, rank) _ _ Nothing b = False  -- "unnecessary"
isAttackedLine' (file, rank) f g t1      b = 
    case fmap getTeam (getTile (f file, g rank) b) of
        Nothing -> False
        Just t2 ->
            if t1 == t2 
                then
                    False
                else
                    case getTile (f file, g rank) b of
                        Just (Queen t2)  -> True
                        Just (Rook t2)   -> f file == id file || g rank == id rank 
                        Just (Bishop t2) -> f file /= id file && g rank /= id rank 
                        Just Empty       -> isAttackedLine' (f file, g rank) f g t1 b
                        _                -> False


isAttackedByKnight :: Coord -> Maybe Team -> Board -> Bool
isAttackedByKnight (file, rank) Nothing      b = False -- TODO: Check what to do here (how to use this functions to check is is possible to castle)
isAttackedByKnight (file, rank) (Just White) b = 
    getTile (file - 2, rank - 1) b == Just (Knight Black) ||
    getTile (file + 2, rank - 1) b == Just (Knight Black) ||
    getTile (file - 2, rank + 1) b == Just (Knight Black) ||
    getTile (file + 2, rank + 1) b == Just (Knight Black) ||
    getTile (file - 1, rank - 2) b == Just (Knight Black) ||
    getTile (file + 1, rank - 2) b == Just (Knight Black) ||
    getTile (file - 1, rank + 2) b == Just (Knight Black) ||
    getTile (file + 1, rank + 2) b == Just (Knight Black)
isAttackedByKnight (file, rank) (Just Black) b = 
    getTile (file - 2, rank - 1) b == Just (Knight White) ||
    getTile (file + 2, rank - 1) b == Just (Knight White) ||
    getTile (file - 2, rank + 1) b == Just (Knight White) ||
    getTile (file + 2, rank + 1) b == Just (Knight White) ||
    getTile (file - 1, rank - 2) b == Just (Knight White) ||
    getTile (file + 1, rank - 2) b == Just (Knight White) ||
    getTile (file - 1, rank + 2) b == Just (Knight White) ||
    getTile (file + 1, rank + 2) b == Just (Knight White)

--TODO: Refactor this function
canMakeCastle :: Coord -> Coord -> Team -> Game -> Bool
canMakeCastle (file1, rank1) (file2, rank2) c game
    | isChecked game                               = False
    | rank1 /= rank2 || (rank1 /= 0 && rank1 /= 7) = False
    | file1 == 4 && file2 == 2 && rank1 == 0       = m1 && canMakeCastle' (file1, rank1) (file2, rank2) c game 
    | file1 == 4 && file2 == 6 && rank1 == 0       = m2 && canMakeCastle' (file1, rank1) (file2, rank2) c game 
    | file1 == 4 && file2 == 2 && rank1 == 7       = m3 && canMakeCastle' (file1, rank1) (file2, rank2) c game 
    | file1 == 4 && file2 == 6 && rank1 == 7       = m4 && canMakeCastle' (file1, rank1) (file2, rank2) c game 
    | otherwise                        = False
    where (m1,m2,m3,m4) = castle game

--TODO: check the rule of Castle when the king is already in check 
canMakeCastle' :: Coord -> Coord -> Team -> Game -> Bool
canMakeCastle' (file1, rank1) (file2, rank2) c game
    | file1 == 4 && file2 == 6 = isEmpty (5, rank2) b && isEmpty (6, rank2) b && (canMove (4, rank1) (5, rank2) game) && (canMove (5, rank2) (6, rank2) (movePiece (N (4, rank1) (5, rank2)) game))
    | file1 == 4 && file2 == 2 = isEmpty (3, rank2) b && isEmpty (2, rank2) b && isEmpty (1, rank2) b && (canMove (4, rank1) (3, rank2) game) && (canMove (3, rank2) (2, rank2) (movePiece (N (4, rank1) (3, rank2)) game)) 
    | otherwise = False
    where b = board game 
          isEmpty(file, rank) b = getTile (file, rank) b == Just Empty

enPassant :: Coord -> Coord -> Team -> Game -> Bool
enPassant (file1, rank1) (file2, rank2) White game =
    rank1 == 3 && 
    getTile (file2, rank1) (board game) == Just (Pawn Black) &&
    head (movesList game) == N (file2, 1) (file2, 3)
enPassant (file1, rank1) (file2, rank2) Black game = 
    rank1 == 4 && 
    getTile (file2, rank1) (board game) == Just (Pawn White) &&
    head (movesList game) == N (file2, 6) (file2, 4)

pawnPromotion :: Move -> Game -> Game
pawnPromotion (N _ _) game = game 
pawnPromotion (PP (file1, rank1) (file2, rank2) tile) game =    
    case team of
        Just (Just Black) ->  
            if testBlackPiece
                then game { board = setTile (file1, rank1) Empty (setTile (file2, rank2) tile (board game)) }
                else game
        Just (Just White) ->  
            if testWhitePiece
                then game { board = setTile (file1, rank1) Empty (setTile (file2, rank2) tile (board game)) }
                else game
    where team = fmap getTeam $ getTile (file1, rank1) (board game)
          testBlackPiece = tile == Queen Black || tile == Rook Black || tile == Knight Black || tile == Bishop Black
          testWhitePiece = tile == Queen White || tile == Rook White || tile == Knight White || tile == Bishop White

isChecked :: Game -> Bool
isChecked game
    | even (turn game) = isAttacked (bking game) (board game)
    | otherwise = isAttacked (wking game) (board game)

isCheckmate :: Game -> Bool
isCheckmate game = isChecked game && not(anyMove game) 

isStalemate :: Game -> Bool
isStalemate game = not(isChecked game) && not(anyMove game)

anyMove :: Game -> Bool
anyMove game = anyMoveAux (0, 0) game

anyMoveAux :: Coord -> Game -> Bool
anyMoveAux (file, rank) game
    |file > 7  = anyMoveAux (0, rank + 1) game
    |rank > 7  = False
    |otherwise = anyMoveTile || anyMoveAux (file + 1, rank) game        
    where b           = board game 
          anyMoveTile = 
            case getTile (rank , file) b of
                Just (King c)   -> anyMoveKing   (file, rank) game
                Just (Pawn c)   -> anyMovePawn c (file, rank) game
                Just (Knight c) -> anyMoveKnight (file, rank) game
                Just (Rook c)   -> anyMoveRook   (file, rank) game
                Just (Bishop c) -> anyMoveBishop (file, rank) game
                Just (Queen c)  -> anyMoveQueen  (file, rank) game
                _               -> False

anyMoveKing :: Coord -> Game -> Bool
anyMoveKing (file, rank) game = 
    canMove (file, rank) (file, rank + 1)     game ||
    canMove (file, rank) (file, rank - 1)     game ||
    canMove (file, rank) (file + 1, rank)     game ||
    canMove (file, rank) (file - 1, rank)     game ||
    canMove (file, rank) (file + 1, rank + 1) game ||
    canMove (file, rank) (file - 1, rank + 1) game ||
    canMove (file, rank) (file + 1, rank - 1) game ||
    canMove (file, rank) (file - 1, rank - 1) game

anyMoveKnight :: Coord -> Game -> Bool
anyMoveKnight (file, rank) game = 
    canMove (file, rank) (file + 1, rank + 2) game ||
    canMove (file, rank) (file + 1, rank - 2) game ||
    canMove (file, rank) (file - 1, rank + 2) game ||
    canMove (file, rank) (file - 1, rank - 2) game ||
    canMove (file, rank) (file - 2, rank + 1) game ||
    canMove (file, rank) (file - 2, rank - 1) game ||
    canMove (file, rank) (file + 2, rank + 1) game ||
    canMove (file, rank) (file + 2, rank - 1) game

anyMovePawn :: Team -> Coord -> Game -> Bool
anyMovePawn c (file, rank) game = 
    case c of
        Black -> canMove (file, rank) (file, rank + 1)     game || 
                 canMove (file, rank) (file, rank + 2)     game || 
                 canMove (file, rank) (file + 1, rank + 1) game ||
                 canMove (file, rank) (file - 1, rank + 1) game

        White -> canMove (file, rank) (file, rank - 1) game     || 
                 canMove (file, rank) (file, rank - 2) game     || 
                 canMove (file, rank) (file + 1, rank - 1) game ||
                 canMove (file, rank) (file - 1, rank - 1) game

anyMoveRook :: Coord -> Game -> Bool
anyMoveRook (file, rank) game = 
    anyMoveLineAux (file, rank) (file + 1, rank) game ||
    anyMoveLineAux (file, rank) (file - 1, rank) game || 
    anyMoveLineAux (file, rank) (file, rank - 1) game || 
    anyMoveLineAux (file, rank) (file, rank + 1) game

anyMoveBishop :: Coord -> Game -> Bool
anyMoveBishop (file, rank) game =
    anyMoveLineAux (file, rank) (file + 1, rank + 1) game ||
    anyMoveLineAux (file, rank) (file + 1, rank - 1) game || 
    anyMoveLineAux (file, rank) (file - 1, rank + 1) game || 
    anyMoveLineAux (file, rank) (file - 1, rank - 1) game

anyMoveLineAux :: Coord -> Coord -> Game -> Bool 
anyMoveLineAux (file, rank) (file2, rank2) game = insideTable && (canMove' || nextTile)
    where insideTable = file2 > 0 && file2 < 7 && rank2 > 0 && rank2 < 7
          canMove'    = canMove (file, rank) (file2, rank2) game
          rankDif     = rank2 - rank
          fileDif     = file2 - file
          nextTile    = anyMoveLineAux (file, rank) (file2 + fileDif, rank2 + rankDif) game

anyMoveQueen :: Coord -> Game -> Bool
anyMoveQueen (file, rank) game = anyMoveBishop (file, rank) game || anyMoveRook (file, rank) game

countBoards :: [Board] -> [(Int, Board)]
countBoards []     = []
countBoards (x:xs) = (length us, x) : countBoards vs
        where (us, vs) = partition (==x) (x:xs)

isThreeRepetitions :: Game -> Bool
isThreeRepetitions game = (maximum . map fst) (countBoards (boards game)) == 3

initFst   = replicate 8 Pawn
initSnd   = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
initEmpty = replicate 8 Empty

testBoard =
  [ fmap ($ White) initSnd
  , fmap ($ White) initFst
  ] ++ replicate 4 initEmpty ++
  [ fmap ($ Black) initFst
  , fmap ($ Black) initSnd
  ]

g1 = Game {board = testBoard, turn = 1, castle = (True, True, True, True), movesList = [], wking = (4, 7), bking = (4, 0), fiftyMovesCounter = 0, boards = [board g1]}

b1  = movePiece (N (1, 6) (1, 4)) g1  -- Wpawn avança duas casas 
b2  = movePiece (N (2, 7) (0, 5)) b1  -- Wbishop avança diag sup esq
b3  = movePiece (N (0, 5) (1, 4)) b2  -- movimento invalido do Wbishop por peça do mesmo time
b4  = movePiece (N (1, 4) (1, 3)) b3  -- wpawn avança uma casa
b5  = movePiece (N (0, 5) (4, 1)) b4  -- wbishop captura (para diag sup dir) bpawn
b6  = movePiece (N (4, 1) (7, 4)) b5  -- Wbishop move diag inf dir
b7  = movePiece (N (7, 4) (6, 5)) b6  -- Wbishop move diag inf esq
b8  = movePiece (N (5, 0) (1, 4)) b7  -- Bbishop move diag inf esq
b9  = movePiece (N (1, 4) (4, 7)) b8  -- mov inválido do Bbishop por sentido de movimento ocupado
b10 = movePiece (N (1, 4) (3, 6)) b9  -- Bbishop captura (para diag inf dir) Wpawn
b11 = movePiece (N (3, 6) (3, 4)) b10 -- mov inválido do Bbishop (vertical)
b12 = movePiece (N (6, 5) (0, 5)) b11 -- mov inválido do Wbishop (horizontal)

r1  = movePiece (N (0, 6) (0, 4)) g1  
r2  = movePiece (N (0, 7) (0, 5)) r1 -- movimento Wrook vertical cima
r3  = movePiece (N (0, 5) (7, 5)) r2 -- movimento Wrook horizontal
r4  = movePiece (N (7, 5) (7, 1)) r3 -- Wrook captura (para cima) Bpawn
r5  = movePiece (N (7, 1) (6, 1)) r4 -- Wrook captura (para esq) Bpawn
r6  = movePiece (N (6, 1) (5, 0)) r5 -- mov invalido Wrook diagonal
r7  = movePiece (N (6, 1) (6, 4)) r6 -- movimento Wrook vertical baixo
r8  = movePiece (N (6, 4) (1, 4)) r7 -- movimento Wrook horizontal esq
r9  = movePiece (N (1, 4) (1, 0)) r8 -- mov invalido Wrook por sentido de mov ocupado

n1  = movePiece (N (1, 7) (0, 5)) g1 -- mov Wknight sup esq
n2  = movePiece (N (0, 5) (1, 3)) n1 -- mov Wknight sup dir
n3  = movePiece (N (1, 3) (2, 1)) n2 -- Wknight captura (para direção sup dir) Bpawn
n4  = movePiece (N (2, 1) (3, 2)) n3 -- movimento invalido Wknight
n5  = movePiece (N (2, 1) (3, 3)) n4 -- movimento Wknight inf dir
n6  = movePiece (N (3, 3) (2, 5)) n5 -- movimento Wknight inf esq
n7  = movePiece (N (2, 5) (4, 6)) n6 -- movimento invalido Wknight por tile ocupado pela mesma cor 
n8  = movePiece (N (2, 5) (4, 4)) n7 -- movimento Wknight dir sup 

k1  = movePiece (N (4, 6) (4, 4)) g1  -- mov Wpawn cima
k2  = movePiece (N (4, 7) (4, 5)) k1  -- mov invalido (pular uma casa) Wking sup 
k3  = movePiece (N (4, 7) (4, 6)) k2  -- mov Wking cima
k4  = movePiece (N (4, 6) (4, 5)) k3  -- mov Wking cima
k5  = movePiece (N (4, 5) (5, 5)) k4  -- mov Wking direita 
k6  = movePiece (N (4, 5) (3, 5)) k4  -- mov Wking esquerda 
k7  = movePiece (N (4, 5) (5, 4)) k4  -- mov Wking direita cima
k8  = movePiece (N (4, 5) (3, 4)) k4  -- mov Wking esquerda cima
k9  = movePiece (N (4, 5) (5, 6)) k4  -- mov Wking para tile ocupada por mesmo time
k10 = movePiece (N (3, 6) (3, 4)) k4  -- mov Wpawn cima
k11 = movePiece (N (4, 5) (3, 6)) k10 -- mov Wking baixo esq
k12 = movePiece (N (3, 6) (4, 7)) k11 -- mov Wking baixo dir

c0  = movePiece (N (2, 6) (2, 4)) g1  --mov wpawn
c1  = movePiece (N (1, 7) (2, 5)) c0  --mov wknight
c2  = movePiece (N (1, 6) (1, 4)) c1  --mov wpawn
c3  = movePiece (N (2, 7) (0, 5)) c2  --mov wbishop
c4  = movePiece (N (3, 7) (2, 6)) c3  --mov wqueen
c5  = movePiece (N (6, 6) (6, 4)) c4  --mov wpawn
c6  = movePiece (N (6, 7) (5, 5)) c5  --mov wknight
c7  = movePiece (N (5, 7) (7, 5)) c6  --mov wbishop
c8  = movePiece (N (4, 7) (6, 7)) c7  --castle dir 
c9  = movePiece (N (4, 7) (2, 7)) c7  --castle esq

e1 = movePiece (N (1, 6) (1, 4)) g1  
e2 = movePiece (N (2, 1) (2, 3)) e1
e3 = movePiece (N (1, 4) (1, 3)) e2
e4 = movePiece (N (0, 1) (0, 3)) e3
e5 = movePiece (N (1, 3) (2, 2)) e4  -- mov invalido (peão a ser capturado não havia se movido na jogada anterior)
e6 = movePiece (N (1, 3) (0, 2)) e4  -- en passant certo
e7 = movePiece (N (2, 3) (2, 4)) e6  
e8 = movePiece (N (3, 6) (3, 4)) e7  
e9 = movePiece (N (2, 4) (3, 5)) e8  -- en passant certo

pp1 = movePiece (N (1, 0) (2, 2)) e8
pp2 = movePiece (N (0, 2) (0, 1)) pp1
pp3 = movePiece (N (0, 0) (1, 0)) pp2
pp4 = movePiece (N (0, 1) (0, 0)) pp3 -- promoção de peão avançando uma casa
pp5 = movePiece (PP (0, 1) (1, 0) (Bishop White)) pp3 -- promoção de peão capturando peça
pp6 = movePiece (N (1, 0) (0, 0)) pp5

t1 = movePiece (N (1, 0) (2, 2)) e8
t2 = movePiece (N (1, 7) (2, 5)) t1
t3 = movePiece (N (0, 0) (1, 0)) t2
t4 = movePiece (N (3, 7) (3, 5)) t3
t5 = movePiece (N (3, 0) (0, 3)) t4
t6 = movePiece (N (3, 5) (2, 4)) t5

rep1 = movePiece (N (1,7) (0, 5)) g1
rep2 = movePiece (N (1,0) (0, 2)) rep1
rep3 = movePiece (N (0,5) (1, 7)) rep2
rep4 = movePiece (N (0,2) (1, 0)) rep3
rep5 = movePiece (N (1,7) (0, 5)) rep4
rep6 = movePiece (N (1,0) (0, 2)) rep5
rep7 = movePiece (N (0,5) (1, 7)) rep6
rep8 = movePiece (N (0,2) (1, 0)) rep7