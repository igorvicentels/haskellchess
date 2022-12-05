module Endgame where

import Types
import Board

import Data.List

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
                            length (filter (== Bishop Black) list) == 1 &&
                            fmap getTileColor (findPiece (Bishop White) (board game)) /= fmap getTileColor (findPiece (Bishop Black) (board game))