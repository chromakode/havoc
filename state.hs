import Data.Array
import Data.Char

data Color = White | Black
data PieceType = King | Queen | Bishop | Knight | Rook | Pawn
data Piece = Piece Color PieceType | Blank
type Board = Array (Int, Int) Piece
data State = State Int Color Board

instance Show PieceType where
    show King   = "K"
    show Queen  = "Q"
    show Bishop = "B"
    show Knight = "N"
    show Rook   = "R"
    show Pawn   = "P"

instance Read PieceType where
    readsPrec p ('K':s) = [(King, s)]
    readsPrec p ('Q':s) = [(Queen, s)]
    readsPrec p ('B':s) = [(Bishop, s)]
    readsPrec p ('N':s) = [(Knight, s)]
    readsPrec p ('R':s) = [(Rook, s)]
    readsPrec p ('P':s) = [(Pawn, s)]    
    readsPrec p (_:s)   = []
    
instance Show Piece where
    show Blank = "."
    show (Piece color piece)
        = map colorCase (show piece)
        where colorCase = case color of
                            White -> toUpper
                            Black -> toLower

instance Read Piece where
    readsPrec p ('.':s) = [(Blank, s)]
    readsPrec p (c:s)   = [(Piece color piece, s)]
        where
            color = case isUpper(c) of
                      True  -> White
                      False -> Black
            piece = read [(toUpper c)]

readBoard :: String -> Board
readBoard text = listArray ((0,0),(x-1,y-1)) pieces
    where
        ls = lines text
        x = length ls
        y = length (head ls)
        pieces = map (read . (:[])) (concat ls)

showBoard :: Board -> String
showBoard board =
    unlines [concat
                [show (board ! (y,x)) | x <- [minX..maxX]]
            | y <- [minY..maxY]]
    where ((minY,minX),(maxY,maxX)) = bounds board

startBoard :: Board
startBoard = readBoard startBoardText
    where
        startBoardText = 
            "kqbnr\n\
            \ppppp\n\
            \.....\n\
            \.....\n\
            \PPPPP\n\
            \RNBQK\n"

