module Havoc.Game.State where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Char
import Data.List
import Data.Maybe

data Color = White | Black deriving Eq
type Piece = Char
type PieceType = Char
type Square = (Int,Int)
type Position = (Square, Piece)
type Board s = STUArray s Square Piece
type BoardBounds = (Square, Square)

data GameState s = GameState { turn      :: Int
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

colorOf :: Piece -> Color
colorOf piece = case isUpper piece of
                  True  -> White
                  False -> Black
                  
toColor :: Piece -> Color -> Piece
toColor piece color = case color of
                        White -> toUpper piece
                        Black -> toLower piece
                  
pieceType :: Piece -> PieceType
pieceType piece = toUpper piece

readBoard :: String -> ST s (Board s)
readBoard text = do
    newListArray ((0,0),(i-1,j-1)) pieces
    where
        ls = lines (dropWhile isSpace text)
        i = length ls
        j = length (head ls)
        pieces = concat ls

showBoard :: Board s -> ST s String
showBoard board = do
    ((li,lj),(ui,uj)) <- getBounds board
    pieces <- (mapM . mapM) (readArray board) [ [(i,j) | j <- [lj..uj]]
                                              | i <- [li..ui]]
    return $ unlines pieces

isBlank :: Board s -> Square -> ST s Bool
isBlank board square = do
    piece <- readArray board square
    return $ piece == '.'

isColor :: Board s -> Color -> Square -> ST s Bool
isColor board color square = do
    piece <- readArray board square
    return $ (colorOf piece) == color

isTurnColor :: GameState s -> Square -> ST s Bool
isTurnColor (GameState turn turnColor board) square = isColor board turnColor square

pieces :: Board s -> ST s [Piece]
pieces board = do
    elems <- getElems board
    return $ filter (/= '.') elems

positions :: Board s -> ST s [Position]
positions board = do
    assocs <- getAssocs board
    return $ filter (\(s,p) -> p /= '.') assocs

endRow :: Color -> Board s -> ST s Int
endRow color board = do
    ((li,lj),(ui,uj)) <- getBounds board
    return $ case color of
               White -> li
               Black -> ui        

showGameState :: GameState s -> ST s String
showGameState (GameState turn turnColor board) = do
    boardText <- showBoard board
    return $ show turn ++ " " ++ show turnColor ++ "\n"
                  ++ boardText

readGameState :: String -> ST s (GameState s)  
readGameState s = do
    let (turn, t)      = head $ readsPrec 0 s
        (turnColor, u) = head $ readsPrec 0 t
    board <- readBoard u
    return $ GameState turn turnColor board
    
copyBoard :: Board s -> ST s (Board s)
copyBoard board = do
    bounds <- getBounds board
    elems  <- getElems board
    newListArray bounds elems
    
copyGameState :: GameState s -> ST s (GameState s)
copyGameState (GameState turn turnColor board) = do
    board' <- copyBoard board
    return $ GameState turn turnColor board'
