module Havoc.Game.State where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Char
import Data.List
import Data.Maybe
import Havoc.Utils (copyMArray)

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

data GameState s = GameState { turn      :: Int
                             , turnColor :: Color
                             , board     :: Board s }

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

readBoard :: String -> ST s (Board s)
readBoard text = do
    newListArray ((0,0),(i-1,j-1)) pieces
    where
        ls = lines (dropWhile isSpace text)
        i = length ls
        j = length (head ls)
        pieces = map (read . (:[])) (concat ls)

showBoard :: Board s -> ST s String
showBoard board = do
    ((li,lj),(ui,uj)) <- getBounds board
    pieces <- (mapM . mapM) (readArray board) [ [(i,j) | j <- [lj..uj]]
                                              | i <- [li..ui]]
    return $ unlines $ (map . concatMap) show pieces

isBlank :: Board s -> Square -> ST s Bool
{-# INLINE isBlank #-}
isBlank board square = do
    piece <- readArray board square
    return $ piece == Blank

isColor :: Board s -> Color -> Square -> ST s Bool
isColor board color square = do
    piece <- readArray board square
    return $ (colorOf piece) == color

isTurnColor :: GameState s -> Square -> ST s Bool
isTurnColor (GameState turn turnColor board) square = isColor board turnColor square

pieces :: Board s -> ST s [Piece]
{-# INLINE pieces #-}
pieces board = do
    elems <- getElems board
    return $ filter (/=Blank) elems

positions :: Board s -> ST s [Position]
{-# INLINE positions #-}
positions board = do
    assocs <- getAssocs board
    return $ filter (\(s,p) -> p /= Blank) assocs

startRow, endRow :: Color -> Board s -> ST s Int
{-# INLINE endRow #-}
startRow color board = do
    ((li,lj),(ui,uj)) <- getBounds board
    return $ case color of
               White -> ui
               Black -> li
               
endRow color = startRow (invertColor color)

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
    
copyGameState :: GameState s -> ST s (GameState s)
copyGameState (GameState turn turnColor board) = do
    board' <- copyMArray board
    return $ GameState turn turnColor board'

boardEq :: Board s -> Board s -> ST s Bool
boardEq board1 board2 = do
    bounds1 <- getBounds board1
    bounds2 <- getBounds board2
    if bounds1 /= bounds2
        then return False
        else (liftM and) $ mapM (\i -> do x1 <- readArray board1 i
                                          x2 <- readArray board2 i
                                          return $ x1 == x2)
                                (range bounds1)

