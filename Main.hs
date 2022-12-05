module Main where

import Types
import Board
import Endgame
import IO

initFst   = replicate 8 Pawn
initSnd   = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
initEmpty = replicate 8 Empty

initBoard =
  [ fmap ($ Black) initSnd
  , fmap ($ Black) initFst
  ] ++ replicate 4 initEmpty ++
  [ fmap ($ White) initFst
  , fmap ($ White) initSnd
  ]

game = Game { board = initBoard
            , turn = 1
            , castle = (True, True, True, True)
            , movesList = []
            , wking = (4, 7)
            , bking = (4, 0)
            , fiftyMovesCounter = 0
            , boards = [board game]
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

main :: IO ()
main = run game