module Types where

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
    show (Pawn   White) = "♟︎"
    show (Rook   White) = "♜"
    show (Knight White) = "♞"
    show (Bishop White) = "♝"
    show (Queen  White) = "♛"
    show (King   White) = "♚"
    show Empty          = " "

type Board = [[Tile]]

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