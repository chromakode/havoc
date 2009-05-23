module Havoc.State where

import Data.Array.ST
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
type Board s = STArray s Square Piece
type BoardBounds = (Square, Square)

data State s = State { turn      :: Int
                     , turnColor :: Color
                     , board     :: Board s }
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

readBoard :: String -> ST s Board
readBoard text = do
    return $ newListArray ((0,0),(i-1,j-1)) pieces
    where
        ls = lines (dropWhile isSpace text)
        i = length ls
        j = length (head ls)
        pieces = map (read . (:[])) (concat ls)

showBoard :: ST s Board -> String
showBoard board = do
    ((li,lj),(ui,uj)) <- getBounds board
    return $ unlines [concat
                       [show (board ! (i,j)) | j <- [lj..uj]]
                     | i <- [li..ui]]

isBlank :: Board -> Square -> ST s Bool
isBlank board square = do
    piece <- readArray board square
    return $ piece == Blank

isColor :: Board -> Color -> Square -> ST s Bool
isColor board color square = do
    piece <- readArray board square
    return $ (colorOf piece) == color

isTurnColor :: State -> Square -> ST s Bool
isTurnColor (State turn turnColor board) square = isColor board turnColor square

pieces :: Board -> ST s [Piece]
pieces board = do
    elems <- getElems board
    return $ filter (/=Blank) elems

positions :: Board -> ST s [Position]
positions board = do
    assocs <- getAssocs board
    return $ filter (\(s,p) -> p /= Blank) assocs

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
