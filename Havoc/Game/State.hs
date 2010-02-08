module Havoc.Game.State where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Char
import Data.List
import Data.Maybe
import Havoc.Utils (copyMArray)

data Color = Self | Enemy deriving (Eq, Enum, Ix, Ord)
data PieceType = Player deriving Eq
data Piece = Piece { colorOf   :: Color
                   , pieceType :: PieceType }
           | Blank
           | Wall
           deriving Eq
type Square = (Int,Int)
type Position = (Square, Piece)
type Board s = STArray s Square Piece
type BoardBounds = (Square, Square)

data GameState s = GameState { turn  :: Int
                             , turnColor :: Color
                             , board :: Board s }

instance Show Color where
    show Self  = "1"
    show Enemy = "2"

instance Read Color where
    readsPrec p s = [(color, t) | (c,t) <- lex s, color <- colorOf c]
        where
            colorOf "1" = [Self]
            colorOf "2" = [Enemy]
            colorOf _   = []

invertColor :: Color -> Color
invertColor Self  = Enemy
invertColor Enemy = Self

colorName :: Color -> String
colorName Self  = "Self"
colorName Enemy = "Enemy"

instance Show PieceType where
    show Player = "@"

instance Read PieceType where
    readsPrec p s = [(piecetype, t) | (p,t) <- lex s, piecetype <- pieceTypeOf p]
        where
            pieceTypeOf "@" = [Player]
    
instance Show Piece where
    show Blank = " "
    show Wall  = "#"
    show (Piece Self Player)  = "1"
    show (Piece Enemy Player) = "2"

instance Read Piece where
    readsPrec p s = [(piece, t') | (s', t) <- lex s, (piece, t') <- pieceOf s']
        where
            pieceOf " " = [(Blank, "")]
            pieceOf "#" = [(Wall, "")]
            pieceOf "1" = [(Piece Self Player, "")]
            pieceOf "2" = [(Piece Enemy Player, "")]

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

squareIs :: (Piece -> Bool) -> Board s -> Square -> ST s Bool
squareIs pred board square = do
    piece <- readArray board square
    return $ pred piece

isBlank :: Piece -> Bool
{-# INLINE isBlank #-}
isBlank piece = piece == Blank

isWall :: Piece -> Bool
{-# INLINE isWall #-}
isWall piece = piece == Wall

isPlayer :: Piece -> Bool
{-# INLINE isPlayer #-}
isPlayer Blank = False
isPlayer Wall  = False
isPlayer (Piece _ Player) = True

isColor :: Color -> Piece -> Bool
isColor color piece = (colorOf piece) == color

pieces :: Board s -> ST s [Piece]
{-# INLINE pieces #-}
pieces board = do
    elems <- getElems board
    return $ filter (isPlayer) elems

positions :: Board s -> ST s [Position]
{-# INLINE positions #-}
positions board = do
    assocs <- getAssocs board
    return $ filter (\(s,p) -> isPlayer p) assocs

showGameState :: GameState s -> ST s String
showGameState (GameState turn turnColor board) = do
    boardText <- showBoard board
    return $ show turn ++ " " ++ show turnColor ++ " " ++ "\n"
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

