module Board where

import Data.Maybe
import Data.List
import Data.Char
import System.IO

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
    show (Pawn   Black) = "♟︎"
    show (Rook   Black) = "♜"
    show (Knight Black) = "♞"
    show (Bishop Black) = "♝"
    show (Queen  Black) = "♛"
    show (King   Black) = "♚"
    show (Pawn   White) = "♙"
    show (Rook   White) = "♖"
    show (Knight White) = "♘"
    show (Bishop White) = "♗"
    show (Queen  White) = "♕"
    show (King   White) = "♔"
    show Empty          = " "

type Board = [[Tile]]
-- TODO: verify number of rows and columns

data Game = Game { board :: Board
                 , turn :: Int                 
                 , castle :: Castle           -- possibility to make each castle
                 , movesList :: [Move]        -- list of valid moves made
                 , wking :: Coord             -- white king coords
                 , bking :: Coord             -- black king coords
                 , fiftyMovesCounter :: Int   -- 50 move counter without capture or pawn movement
                 , boards :: [Board]          -- stores the last boards to check repetitions
                 , pieceList :: [Tile]        -- list of pieces on board
                 }
        deriving ( Show ) 

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

newPieceList :: Coord -> Game -> [Tile]
newPieceList (file, rank) game   = go (getTile (file, rank) (board game)) (pieceList game) 
    where go _            []     = []
          go (Just piece) (x:xs) = if x == piece then xs else x : go (Just piece) xs

newPieceListPP :: Coord -> Coord -> Tile -> Game -> [Tile]
newPieceListPP coord1 coord2 tile game = tile : (pieceList deleteCaptured) 
    where deletePawn = game { pieceList = newPieceList coord1 game}
          deleteCaptured = deletePawn { pieceList = newPieceList coord2 deletePawn }

readInput :: String -> Maybe ((Char, Char), (Char, Char), Maybe Tile)
readInput (x:y:w:z:[])    = Just((toLower x, toLower y), (toLower w, toLower z), Nothing)
readInput (x:y:w:z:t)
    | t' == "qw" = Just((toLower x, toLower y), (toLower w, toLower z), Just(Queen White))
    | t' == "qb" = Just((toLower x, toLower y), (toLower w, toLower z), Just(Queen Black))
    | t' == "rw" = Just((toLower x, toLower y), (toLower w, toLower z), Just(Rook White))
    | t' == "rb" = Just((toLower x, toLower y), (toLower w, toLower z), Just(Rook Black))
    | t' == "bw" = Just((toLower x, toLower y), (toLower w, toLower z), Just(Bishop White))
    | t' == "bb" = Just((toLower x, toLower y), (toLower w, toLower z), Just(Bishop Black))
    | t' == "nw" = Just((toLower x, toLower y), (toLower w, toLower z), Just(Knight White))
    | t' == "nb" = Just((toLower x, toLower y), (toLower w, toLower z), Just(Knight Black))
    | otherwise = Nothing
    where t'     = (filter (/= ' ') . map toLower) t
readInput _               = Nothing

charToInt :: Char -> Int
charToInt x
    | x == 'a' = 0
    | x == 'b' = 1
    | x == 'c' = 2
    | x == 'd' = 3
    | x == 'e' = 4
    | x == 'f' = 5
    | x == 'g' = 6
    | x == 'h' = 7
    | otherwise = 8 --erro

charIntToInt :: Char -> Int
charIntToInt y
    | y == '8' = 0
    | y == '7' = 1
    | y == '6' = 2
    | y == '5' = 3
    | y == '4' = 4
    | y == '3' = 5
    | y == '2' = 6
    | y == '1' = 7
    | otherwise = 8 --erro

move :: String -> Game -> Game
move input game 
    | readInput input == Nothing         = game
    | otherwise                          = movePiece move game
        where 
            ((f1, r1), (f2, r2), tile) = fromJust(readInput input)
            file1 = charToInt    f1
            rank1 = charIntToInt r1
            file2 = charToInt    f2
            rank2 = charIntToInt r2
            move =
                case tile of
                    Nothing -> (N (file1, rank1) (file2, rank2))
                    Just t  -> (PP (file1, rank1) (file2, rank2) t)

movePiece :: Move -> Game -> Game
movePiece (N (file1, rank1) (file2, rank2)) game =
    if canMove' && not isPP
        then 
            if enPassant' then
                newgame { movesList = m : ms
                        , turn = turn' + 1
                        , fiftyMovesCounter = 0
                        , boards = [board newgame]
                        , pieceList = if team == White 
                                      then newPieceList (file2, rank2 + 1) game 
                                      else newPieceList (file2, rank2 - 1) game
                        }
            else
                if isCapture then 
                    newgame { movesList = m : ms
                            , turn = turn' + 1
                            , fiftyMovesCounter = 0
                            , boards = [board newgame]
                            , pieceList = newPieceList (file2, rank2) game}
                else
                    if isPawnMove then
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
          isPP       = ((getTile (file1, rank1) b) == Just (Pawn Black) && rank1 == 6) || 
                       ((getTile (file1, rank1) b) == Just (Pawn White) && rank1 == 1)
          isPawnMove = getTile (file1, rank1) b == Just (Pawn White) || 
                       getTile (file1, rank1) b == Just (Pawn Black)
          isCapture  = getTile (file2, rank2) b /= Just Empty 
          team       = if odd (turn game) then White else Black
          b          = board game
          enPassant' = enPassant (file1, rank1) (file2, rank2) team game
movePiece (PP (file1, rank1) (file2, rank2) tile) game =
    if canMove' && isPP && isRightTile
        then newgame { movesList = m : ms
                     , turn = turn' + 1
                     , fiftyMovesCounter = 0 
                     , boards = [] 
                     , pieceList = newPieceListPP (file1, rank1) (file2, rank2) tile game }
        else game
    where newgame     = movePiece' (PP (file1, rank1) (file2, rank2) tile) game
          canMove'    = canMove (file1, rank1) (file2, rank2) game 
          m           = PP (file1,rank1) (file2, rank2) tile
          ms          = movesList newgame
          turn'       = turn newgame
          isPP        = ((getTile (file1, rank1) (board game)) == Just (Pawn Black) && rank1 == 6) || 
                        ((getTile (file1, rank1) (board game)) == Just (Pawn White) && rank1 == 1)
          isCapture   = getTile (file2, rank2) b /= Just Empty
          b           = board game
          isRightTile = fromJust (fmap getTeam (getTile (file1, rank1) b)) == getTeam tile

movePiece' :: Move -> Game -> Game
movePiece' (N (file1, rank1) (file2, rank2)) game = 
    case t of
        Nothing -> game
        Just (King c) ->
            if abs(file1 - file2) == 2 then
                if c == Black then
                    game { board  = movePiecesinCastle (file1, rank1) (file2, rank2) c b
                         , castle = (False, False, m3, m4)
                         , bking  = (file2, rank2)}                            
                else
                    game { board = movePiecesinCastle (file1, rank1) (file2, rank2) c b
                         , castle = (m1, m2, False, False)
                         , wking = (file2, rank2)}
            else
                if c == Black then
                    game { board = setTile (file1, rank1) Empty (setTile (file2, rank2) (King c) b)
                         , castle = (False, False, m3, m4)
                         , bking = (file2, rank2)}
                else
                    game { board = setTile (file1, rank1) Empty (setTile (file2, rank2) (King c) b)
                         , castle = (m1, m2, False, False)
                         , wking = (file2, rank2)}
    
        Just (Rook c) -> 
            if c == Black then
                if file1 == 0 then
                    game { board = setTile (file1, rank1) Empty (setTile (file2, rank2) (Rook c) b)
                            , castle = (False, m2, m3, m4)}
                else
                    game { board = setTile (file1, rank1) Empty (setTile (file2, rank2) (Rook c) b)
                            , castle = (m1, False, m3, m4)}
            else
                if file1 == 0 then
                    game { board = setTile (file1, rank1) Empty (setTile (file2, rank2) (Rook c) b)
                            , castle = (m1, m2, False, m4)}
                else
                    game { board = setTile (file1, rank1) Empty (setTile (file2, rank2) (Rook c) b)
                            , castle = (m1, m2, m3, False)}

        Just (Pawn White) ->
            if (rank1 == 3 && abs (file1 - file2) == 1 && getTile (file2, rank2) b == Just Empty) then 
                game { board = moveEnPassant (file1, rank1) (file2, rank2) White b}
            else 
                if rank1 == 1 then
                    game
                else
                    game { board = setTile (file1, rank1) Empty (setTile (file2, rank2) (Pawn White) b)}

        Just (Pawn Black) ->
            if (rank1 == 4 && abs (file1 - file2) == 1 && getTile (file2,rank2) b == Just Empty) then 
                game { board = moveEnPassant (file1, rank1) (file2, rank2) Black b}
            else
                if rank1 == 6 then
                    game
                else 
                    game { board = setTile (file1, rank1) Empty (setTile (file2, rank2) (Pawn Black) b)}

        Just t' -> game { board = setTile (file1, rank1) Empty (setTile (file2, rank2) t' b)}
    where t = getTile (file1, rank1) b 
          b = board game
          (m1, m2, m3, m4) = castle game
movePiece' (PP (file1, rank1) (file2, rank2) tile) game =
    case t of
        Just (Pawn White) ->
            if rank1 == 1 then
                pawnPromotion (PP (file1, rank1) (file2, rank2) tile) game
            else 
                game
        Just (Pawn Black) ->
            if rank1 == 6 then
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
canMove :: Coord -> Coord -> Game -> Bool
canMove (file1, rank1) (file2, rank2) game = difTile && 
                                             difTeam && 
                                             canMovePiece && 
                                             not(isChecked newgame)
    where difTile = (rank1 /= rank2) || (file1 /= file2)
          difTeam = team1 /= team2 
          team1 = fmap getTeam (getTile (file1, rank1) b)
          team2 = fmap getTeam (getTile (file2, rank2) b)
          b = board game
          turn' = turn game
          isPlayerTurn = (team1 == Just (Just White) && turn' `mod` 2 == 1) || 
                         (team1 == Just (Just Black) && turn' `mod` 2 == 0)
          canMovePiece = 
            case (getTile (file1, rank1) b) of
                Nothing         -> False
                Just (Empty)    -> False
                Just (Pawn   c) -> canMovePawn (file1, rank1) (file2, rank2) c b || 
                                   enPassant (file1, rank1) (file2, rank2) c game
                Just (Rook   c) -> canMoveRook (file1, rank1) (file2, rank2) c b
                Just (Knight c) -> canMoveKnight (file1, rank1) (file2, rank2) c b
                Just (Bishop c) -> canMoveBishop (file1, rank1) (file2, rank2) c b
                Just (Queen  c) -> canMoveQueen (file1, rank1) (file2, rank2) c b
                Just (King   c) -> canMoveKing (file1, rank1) (file2, rank2) c b || 
                                   canMakeCastle (file1, rank1) (file2, rank2) c game  
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
canMoveRook (file1, rank1) (file2, rank2) c b = sameTile || (lineMove && nextTile)
    where sameTile = (rank1 == rank2) && (file1 == file2)
          fileDif  = file2 - file1
          rankDif  = rank2 - rank1
          lineMove = (abs(fileDif) == 0 && abs(rankDif) > 0) || 
                     (abs(fileDif) > 0 && abs(rankDif) == 0)
          file_i   = if fileDif == 0 then 0 else (if fileDif < 0 then -1 else 1)
          rank_i   = if rankDif == 0 then 0 else (if rankDif < 0 then -1 else 1)
          nextTile =
            case getTile (file1 + file_i, rank1 + rank_i) b of 
                Nothing -> False
                Just Empty -> canMoveRook (file1 + file_i, rank1 + rank_i) (file2, rank2) c b
                Just x     -> (file1 + file_i, rank1 + rank_i) == (file2, rank2)

canMoveBishop :: Coord -> Coord -> Team -> Board -> Bool
canMoveBishop (file1, rank1) (file2, rank2) c b = sameTile || (diagMove && nextTile)
    where sameTile = (rank1 == rank2) && (file1 == file2)
          diagMove = abs(rank1 - rank2) == abs(file1 - file2)
          fileDif  = file2 - file1
          rankDif  = rank2 - rank1
          file_i = if fileDif < 0 then -1 else 1
          rank_i = if rankDif < 0 then -1 else 1
          nextTile = 
            case getTile (file1 + file_i, rank1 + rank_i) b of 
                Nothing    -> False
                Just Empty -> canMoveBishop (file1 + file_i, rank1 + rank_i) (file2, rank2) c b
                Just x     -> (file1 + file_i, rank1 + rank_i) == (file2, rank2)

canMoveQueen :: Coord -> Coord -> Team -> Board -> Bool
canMoveQueen (file1, rank1) (file2, rank2) c b = 
    canMoveBishop (file1, rank1) (file2, rank2) c b || 
    canMoveRook (file1, rank1) (file2, rank2) c b

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
            isAttackedByKnight (file, rank) mt b ||
            isAttackedByKing (file, rank) mt b
            where mt = getTeam t

isAttackedByKing :: Coord -> Maybe Team -> Board -> Bool
isAttackedByKing (file, rank) Nothing      b = False
isAttackedByKing (file, rank) (Just White) b =
    getTile (file - 1, rank - 1) b == Just (King Black) ||
    getTile (file - 1, rank    ) b == Just (King Black) ||
    getTile (file - 1, rank + 1) b == Just (King Black) ||
    getTile (file    , rank - 1) b == Just (King Black) ||
    getTile (file    , rank + 1) b == Just (King Black) ||
    getTile (file + 1, rank - 1) b == Just (King Black) ||
    getTile (file + 1, rank    ) b == Just (King Black) ||
    getTile (file + 1, rank + 1) b == Just (King Black)
isAttackedByKing (file, rank) (Just Black) b =
    getTile (file - 1, rank - 1) b == Just (King White) ||
    getTile (file - 1, rank    ) b == Just (King White) ||
    getTile (file - 1, rank + 1) b == Just (King White) ||
    getTile (file    , rank - 1) b == Just (King White) ||
    getTile (file    , rank + 1) b == Just (King White) ||
    getTile (file + 1, rank - 1) b == Just (King White) ||
    getTile (file + 1, rank    ) b == Just (King White) ||
    getTile (file + 1, rank + 1) b == Just (King White)

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
            if t1 == t2 then
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

canMakeCastle' :: Coord -> Coord -> Team -> Game -> Bool
canMakeCastle' (file1, rank1) (file2, rank2) c game
    | file1 == 4 && file2 == 6 = isEmpty (5, rank2) b && 
                                 isEmpty (6, rank2) b && 
                                 (canMove (4, rank1) (5, rank2) game) && 
                                 (canMove (5, rank2) (6, rank2) (movePiece (N (4, rank1) (5, rank2)) game))
    | file1 == 4 && file2 == 2 = isEmpty (3, rank2) b && 
                                 isEmpty (2, rank2) b && 
                                 isEmpty (1, rank2) b && 
                                 (canMove (4, rank1) (3, rank2) game) && 
                                 (canMove (3, rank2) (2, rank2) (movePiece (N (4, rank1) (3, rank2)) game)) 
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
          testBlackPiece = tile == Queen Black || tile == Rook Black || 
                           tile == Knight Black || tile == Bishop Black
          testWhitePiece = tile == Queen White || tile == Rook White || 
                           tile == Knight White || tile == Bishop White

isChecked :: Game -> Bool
isChecked game = (even (turn game) && isAttacked (bking game) (board game)) ||
                 (odd (turn game) && isAttacked (wking game) (board game))

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

-- TODO: Rewrite this function
isInsufficientMaterial :: Game -> Bool
isInsufficientMaterial game = noPawnsLeft || onlyOneBishopLeft || oneBishopOnEachTeamOnTilesOfSameColor
    where list              = pieceList game
          pieceList' []     = []
          pieceList' (x:xs) = replicate (fst x) (snd x) ++ pieceList' xs
          noPawnsLeft       = length (filter (\x -> x == Pawn Black || x == Pawn White) list) == 0 
          onlyOneBishopLeft = length list == 3 && 
                              union list [Rook White, Rook Black, Queen White, Queen Black] == []
          oneBishopOnEachTeamOnTilesOfSameColor = 
                            length list == 4 && 
                            length (filter (== Bishop White) list) == 1 && 
                            length (filter (== Bishop Black) list) == 1
          -- TODO: check if bishops are on tiles of same color

countPieces :: [Tile] -> [(Int, Tile)]
countPieces []     = []
countPieces (x:xs) = (length us, x) : countPieces vs
        where (us, vs) = partition (==x) (x:xs)

initFst   = replicate 8 Pawn
initSnd   = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
initEmpty = replicate 8 Empty

testBoard =
  [ fmap ($ Black) initSnd
  , fmap ($ Black) initFst
  ] ++ replicate 4 initEmpty ++
  [ fmap ($ White) initFst
  , fmap ($ White) initSnd
  ]

g1 = Game { board = testBoard
          , turn = 1
          , castle = (True, True, True, True)
          , movesList = []
          , wking = (4, 7)
          , bking = (4, 0)
          , fiftyMovesCounter = 0
          , boards = [board g1]
          , pieceList = 
                replicate 8 (Pawn White) ++ 
                replicate 2 (Rook White) ++ 
                replicate 2 (Knight White) ++ 
                replicate 2 (Bishop White) ++ 
                [Queen White] ++ 
                [King White] ++ 
                replicate 8 (Pawn Black) ++
                replicate 2 (Rook Black) ++
                replicate 2 (Knight Black) ++
                replicate 2 (Bishop Black) ++
                [Queen Black] ++
                [King Black]
          }



-- testing interactivity

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: (Int, Int) -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

setColorGrey :: IO ()
setColorGrey = putStr "\ESC[30m"

setColorBlack :: IO ()
setColorBlack = putStr "\ESC[31m"

setColorWhite :: IO ()
setColorWhite = putStr "\ESC[97m"

showRow :: [Tile] -> Int -> IO ()
showRow row n = do putStr $ "| " ++ show n ++ " | "
                   setColorWhite
                   go row
                   setColorGrey
                   putStrLn $ " | " ++ show n ++ " |"
    where go []     = return () 
          go [x]    = if isLower (head (show x)) then
                            do setColorBlack
                               putStr $ show x
                               setColorWhite
                         else
                            putStr $ show x
          go (x:xs) = do if isLower (head (show x)) then
                            do setColorBlack
                               putStr $ show x
                               setColorWhite
                         else
                            putStr $ show x
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
    | otherwise        = do putStr $ if odd $ turn game then 
                                        "White move: "    
                                     else 
                                        "Black move: "
                            l <- readLine
                            run $ move l game

-- Hutton ch10
getCh :: IO Char
getCh = do oldEcho <- hGetEcho stdin 
           hSetEcho stdin False
           x <- getChar
           hSetEcho stdin oldEcho
           return x

readLine :: IO String
readLine = go ""
    where go xs = do c <- getCh
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
