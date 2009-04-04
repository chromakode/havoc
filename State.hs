import Data.Array
import Data.Char

data Color = White | Black
data PieceType = King | Queen | Bishop | Knight | Rook | Pawn
data Piece = Piece Color PieceType | Blank
type Board = Array (Int, Int) Piece

data State = State { turn  :: Int,
                     color :: Color,
                     board :: Board }

instance Show Color where
    show White = "W"
    show Black = "B"

instance Read Color where
    readsPrec p s = [(color, t) | (c,t) <- lex s, color <- colorOf c]
        where
            colorOf "W" = [White]
            colorOf "B" = [Black]
            colorOf _   = []

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
readBoard text = listArray ((0,0),(x-1,y-1)) pieces
    where
        ls = lines (dropWhile isSpace text)
        x = length ls
        y = length (head ls)
        pieces = map (read . (:[])) (concat ls)

showBoard :: Board -> String
showBoard board =
    unlines [concat
                [show (board ! (y,x)) | x <- [minX..maxX]]
            | y <- [minY..maxY]]
    where ((minY,minX),(maxY,maxX)) = bounds board

instance Show State where
    show (State turn color board) =
        show turn ++ " " ++ show color ++ "\n"
        ++ showBoard board
        
instance Read State where
    readsPrec p s = [(State turn color (readBoard u), "")
                        | (turn, t)  <- readsPrec p s
                        , (color, u) <- readsPrec p t]
