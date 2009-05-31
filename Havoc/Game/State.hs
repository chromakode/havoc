module Havoc.Game.State where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Char
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List
import Data.Maybe

data Color = White | Black deriving Eq
data PieceType = King | Queen | Bishop | Knight | Rook | Pawn deriving Eq
data Piece = Piece { pieceId   :: Int
                   , colorOf   :: Color
                   , pieceType :: PieceType }
           | Blank
           deriving Eq
type PieceMap = IntMap Position
type Square = (Int,Int)
type Position = (Square, Piece)
type Board s = STArray s Square Piece
type BoardBounds = (Square, Square)

data GameState s = GameState { turn      :: Int
                             , turnColor :: Color
                             , board     :: Board s
                             , pieceMap  :: PieceMap }
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
    show (Piece _ color piece)
        = map colorCase (show piece)
        where colorCase = case color of
                            White -> toUpper
                            Black -> toLower

readPiece :: Char -> Int -> Piece
readPiece c id = pieceOf c
    where
        pieceOf '.' = Blank
        pieceOf c   = Piece id (colorOf c) (read [(toUpper c)])

        colorOf ch
            | isUpper ch = White
            | otherwise  = Black

readBoard :: String -> ST s (Board s, PieceMap)
readBoard text = do
    board <- newListArray ((0,0),(i-1,j-1)) pieces
    positions <- getAssocs board
    let pieceMap = IntMap.fromList [(id, p) | p@(_, Piece id _ _) <- filter (\(s,p) -> p /= Blank) positions]
    return (board, pieceMap)
    where
        ls = lines (dropWhile isSpace text)
        i = length ls
        j = length (head ls)
        pieces = [readPiece c id | (id, c) <- zip [0..] (concat ls)]

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
isTurnColor (GameState turn turnColor board pieceMap) square = isColor board turnColor square

pieces :: GameState s -> [Piece]
{-# INLINE pieces #-}
pieces state = map snd (positions state)

positions :: GameState s -> [Position]
{-# INLINE positions #-}
positions state = IntMap.elems (pieceMap state)

endRow :: Color -> Board s -> ST s Int
{-# INLINE endRow #-}
endRow color board = do
    ((li,lj),(ui,uj)) <- getBounds board
    return $ case color of
               White -> li
               Black -> ui        

showGameState :: GameState s -> ST s String
showGameState (GameState turn turnColor board pieceMap) = do
    boardText <- showBoard board
    return $ show turn ++ " " ++ show turnColor ++ "\n"
                  ++ boardText

readGameState :: String -> ST s (GameState s)  
readGameState s = do
    let (turn, t)      = head $ readsPrec 0 s
        (turnColor, u) = head $ readsPrec 0 t
    (board, pieces) <- readBoard u
    return $ GameState turn turnColor board pieces
    
copyBoard :: Board s -> ST s (Board s)
copyBoard board = do
    bounds <- getBounds board
    elems  <- getElems board
    newListArray bounds elems
    
copyGameState :: GameState s -> ST s (GameState s)
copyGameState (GameState turn turnColor board pieceMap) = do
    board' <- copyBoard board
    return $ GameState turn turnColor board' pieceMap
