module Havoc.State where

import Data.Array
import Data.Char
import Data.List
import Data.Maybe

data Color = White | Black deriving Eq
data PieceType = King | Queen | Bishop | Knight | Rook | Pawn deriving Eq
data Piece = Piece { colorOf   :: Color
                   , pieceType :: PieceType }
           | Blank
           deriving Eq
type Square = (Int,Int)
type Position = (Square, Piece)
type Board = Array Square Piece
type BoardBounds = (Square, Square)

data State = State { turn      :: Int
                   , turnColor :: Color
                   , board     :: Board }
           deriving Eq

instance Show Color where
    show White = "W"
    show Black = "B"

instance Read Color where
    readsPrec p s = [(color, t) | (c,t) <- lex s, color <- colorOf c]
        where
            colorOf "W" = [White]
            colorOf "B" = [Black]
            colorOf _   = []

invertColor :: Color -> Color
invertColor White = Black
invertColor Black = White

colorName :: Color -> String
colorName White = "White"
colorName Black = "Black"

instance Show PieceType where
    show King   = "K"
    show Queen  = "Q"
    show Bishop = "B"
    show Knight = "N"
    show Rook   = "R"
    show Pawn   = "P"

instance Read PieceType where
    readsPrec p s = [(piecetype, t) | (p,t) <- lex s, piecetype <- pieceTypeOf p]
        where
            pieceTypeOf "K" = [King]
            pieceTypeOf "Q" = [Queen]
            pieceTypeOf "B" = [Bishop]
            pieceTypeOf "N" = [Knight]
            pieceTypeOf "R" = [Rook]
            pieceTypeOf "P" = [Pawn]    
            pieceTypeOf _   = []
    
instance Show Piece where
    show Blank = "."
    show (Piece color piece)
        = map colorCase (show piece)
        where colorCase = case color of
                            White -> toUpper
                            Black -> toLower

instance Read Piece where
    readsPrec p s = [(piece, t') | (s', t) <- lex s, (piece, t') <- pieceOf s']
        where
            pieceOf "." = [(Blank, "")]
            pieceOf s   = [(Piece (colorOf (head s)) piecetype, t)
                              | (piecetype, t) <- readsPrec p (map toUpper s)]
            colorOf c
                | isUpper c  = White
                | otherwise  = Black

readBoard :: String -> Board
readBoard text = listArray ((0,0),(i-1,j-1)) pieces
    where
        ls = lines (dropWhile isSpace text)
        i = length ls
        j = length (head ls)
        pieces = map (read . (:[])) (concat ls)

showBoard :: Board -> String
showBoard board =
    unlines [concat
                [show (board ! (i,j)) | j <- [lj..uj]]
            | i <- [li..ui]]
    where ((li,lj),(ui,uj)) = bounds board

isBlank :: Board -> Square -> Bool
isBlank board square = (board ! square) == Blank

isColor :: Board -> Color -> Square -> Bool
isColor board color square = (colorOf (board ! square)) == color

isTurnColor :: State -> Square -> Bool
isTurnColor (State turn turnColor board) square = isColor board turnColor square

pieces :: Board -> [Piece]
pieces board = filter (/=Blank) (elems board)

positions :: Board -> [Position]
positions board = filter (\(s,p) -> (not . isBlank board) s) (assocs board)

endRow :: Color -> Board -> Int
endRow color board
    = case color of
        White -> li
        Black -> ui
    where ((li,lj),(ui,uj)) = bounds board
        

instance Show State where
    show (State turn turnColor board) =
        show turn ++ " " ++ show turnColor ++ "\n"
        ++ showBoard board
        
instance Read State where
    readsPrec p s = [(State turn turnColor (readBoard u), "")
                        | (turn, t)  <- readsPrec p s
                        , (turnColor, u) <- readsPrec p t]                      
