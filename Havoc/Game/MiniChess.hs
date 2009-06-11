module Havoc.Game.MiniChess where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Havoc.Game
import Havoc.Game.State
import Havoc.Game.Move
import Havoc.Utils (copyMArray, swap)

--
-- Move Generation
--
            
mcMovesT :: MoveType -> GameState s -> Position -> ST s [Square]
mcMovesT mt state (square, Piece _     King)   = dirMoves mt [North .. Northwest] state square
mcMovesT mt state (square, Piece _     Queen)  = lineMoves mt [North .. Northwest] state square
mcMovesT mt state (square, Piece _     Rook)   = lineMoves mt [North, East, South, West] state square
mcMovesT mt state (square, Piece _     Bishop) = (dirMoves Move [North, East, South, West] state square) +..+ (lineMoves MoveCapture [Northeast, Southeast, Southwest, Northwest] state square)
mcMovesT mt state (square, Piece _     Knight) = knightMoves mt state square
mcMovesT mt state (square, Piece White Pawn)   = (dirMoves Move [North] state square) +..+ (dirMoves Capture [Northwest, Northeast] state square)
mcMovesT mt state (square, Piece Black Pawn)   = (dirMoves Move [South] state square) +..+ (dirMoves Capture [Southwest, Southeast] state square)
mcMovesT mt state (square, Blank) = error "Move.mcMoves: moves for blank square requested"

mcMoves, mcMovesXray :: GameState s -> Position -> ST s [Square]
mcMoves     = mcMovesT MoveCapture
mcMovesXray = mcMovesT XRay

handlePromotion :: GameState s -> MoveDiff -> ST s (MoveDiff)
handlePromotion (GameState turn turnColor board) diff@(MoveDiff movedPiece (fromSquare, toSquare) takenPiece _) = do
    let pieceColor = colorOf movedPiece
        isPawn = (pieceType movedPiece) == Pawn

    edge <- endRow pieceColor board
    let isEndRow = (fst toSquare) == edge

    if isPawn && isEndRow
        then do let becomePiece = (Piece pieceColor Queen)
                writeArray board toSquare becomePiece
                return $ MoveDiff movedPiece (fromSquare, toSquare) takenPiece becomePiece
        else return diff

updatePawnFiles :: PawnFiles s -> MoveDiff -> Bool -> ST s ()
updatePawnFiles pawnFiles (MoveDiff movedPiece move takenPiece becomePiece) undo = do
    when (isPawn movedPiece)  (addValue fromFile (-myPawnValue))
    when (isPawn becomePiece) (addValue toFile     myPawnValue)
    when (isPawn takenPiece)  (addValue toFile     myPawnValue)
    where
        ((_, fromFile), (_, toFile)) = move
        mySign = (colorSign . colorOf) movedPiece
        myPawnValue = if not undo then mySign else -mySign
        
        isPawn piece = (piece /= Blank) && (pieceType piece == Pawn)
        
        addValue file value = do
            count <- readArray pawnFiles file
            writeArray pawnFiles file (count + value)

mcMove :: MiniChess s -> Move -> ST s (MiniChess s, Evaluated MoveDiff)
mcMove (MiniChess eS@(Evaluated oldValue state) pF) move@(fromSquare, toSquare) = do
    -- Evaluate the previous state
    undoDelta <- mcEvaluatePreMove state move
    
    -- Perform the move
    (newState, diff) <- chessDoMove state move
    diff <- handlePromotion newState diff
    updatePawnFiles pF diff False
    
    -- Evaluate the new state
    newValue <- mcEvaluateMove oldValue undoDelta newState diff
    return (MiniChess (Evaluated newValue newState) pF, Evaluated oldValue diff)

mcUndoMove :: MiniChess s -> Evaluated MoveDiff -> ST s (MiniChess s)
mcUndoMove (MiniChess eS@(Evaluated v s) pF) eDiff@(Evaluated _ diff) = do
    eS' <- chessUndoMoveEval eS eDiff
    updatePawnFiles pF diff True
    return $ MiniChess eS' pF

--
-- Evaluation
-- 

winScore :: GameState s -> Score
winScore (GameState turn turnColor board) = max_eval_score - (turn * 2)

mcEvaluateResult :: GameState s -> Result -> ST s Score
mcEvaluateResult state@(GameState turn turnColor board) result
    = case result of
        Draw        -> return 0
        Win color   -> let sign = if color == turnColor then 1 else -1 in
                       return $ sign * (winScore state)

colorSign, lastColorSign :: Color -> Int
colorSign color = case color of
                    White -> 1
                    Black -> -1

lastColorSign = colorSign . invertColor

mcEvaluatePreMove :: GameState s -> Move -> ST s Score
mcEvaluatePreMove state@(GameState turn turnColor board) (fromSquare, toSquare) = do
    movedPiece <- readArray board fromSquare
    takenPiece <- readArray board toSquare
    movedPastScore <- positionScore state (fromSquare, movedPiece)
    takenPastScore <- positionScore state (toSquare  , takenPiece)
    return $ -movedPastScore + takenPastScore

mcEvaluateMove :: Score -> Score -> GameState s -> MoveDiff -> ST s Score
mcEvaluateMove oldValue undoDelta state diff@(MoveDiff movedPiece (fromSquare, toSquare) takenPiece becomePiece) = do
    -- In the MiniChess type, scores are stored with static sign, with positive values in White's favor, and negative values in Black's favor. In this module, scores are treated with relative sign, with positive scores in the current player's favor. We convert the sign scheme here.
    let sign = lastColorSign (turnColor state)
    if takenPiece /= Blank && (pieceType takenPiece) == King
      then return $ sign * (winScore state)
      else do materialDelta <- naiveMaterialScore state diff
              positionDelta <- positionScore state (toSquare, becomePiece)
              return $ oldValue + (sign * (materialDelta + positionDelta + undoDelta))

naiveMaterialScore :: GameState s -> MoveDiff -> ST s Score
naiveMaterialScore (GameState turn turnColor board) (MoveDiff movedPiece (fromSquare, toSquare) takenPiece becomePiece) = do
    return $ (score becomePiece - score movedPiece) + score takenPiece
    where
        score Blank                = 0        
        score (Piece color Pawn)   = 100
        score (Piece color Knight) = 200
        score (Piece color Bishop) = 350
        score (Piece color Rook)   = 500
        score (Piece color Queen)  = 700
        score (Piece color King)   = 0
        
positionScore :: GameState s -> Position -> ST s Score
positionScore state          (_     , Blank) = return 0
positionScore state position@(square@(row,col), piece) = do
    playerStartRow <- startRow (colorOf piece) (board state)
    let height = abs (playerStartRow - row)

    classScore <- case piece of
        Piece _ Pawn   -> do
            supporting <- countPawns [Northeast, Northwest, Southeast, Southwest]
            adjacent   <- countPawns [East, West]
            column     <- countPawns [North, South]
            return $ 2*height + 20*supporting + 5*adjacent - 2*column
            where
                countPawns dirs = (liftM length) $ dirMoves (IsPiece piece) dirs state square
        otherwise      -> return height
    
    --movesScore <- moveGenScore state position
    
    return $ classScore

moveGenScore :: GameState s -> Position -> ST s Score
moveGenScore state position@(square, piece) = do
    moves <- moveGenPosition mcMovesXray state position
    isCenter <- isCenterPred state
    let moveDests = map snd moves
    let centerScore = ((10*) . length . filter isCenter) moveDests
    let mobilityScore = length moves
    return $ centerScore + mobilityScore
    where
        isCenterPred state = do
            ((li,lj),(ui,uj)) <- getBounds (board state)
            return (\(i,j) ->
                       abs ((fromIntegral i) - ((fromIntegral (ui-li))/2)) <= 0.5
                    && abs ((fromIntegral j) - ((fromIntegral (uj-lj))/2)) <= 0.5)

--
-- Basic rules and Game instance
--

mcStartBoard :: ST s (Board s)
mcStartBoard = readBoard mcStartBoardText
    where
        mcStartBoardText = 
            "kqbnr\n\
            \ppppp\n\
            \.....\n\
            \.....\n\
            \PPPPP\n\
            \RNBQK\n"

type PawnFiles s = STUArray s Int Int
data MiniChess s = MiniChess { mcState   :: Evaluated (GameState s)
                             , pawnFiles :: PawnFiles s             }
instance Game MiniChess where
    {-# SPECIALIZE instance Game MiniChess #-}
    fromBoard board = do
        ((li,lj),(ui,uj)) <- getBounds board

        let mcState = Evaluated 0 $ GameState 1 White board
        pawnFiles <- newArray (lj,uj) 0
        return $ MiniChess mcState pawnFiles
        
    startState = mcStartBoard >>= fromBoard

    gameStatus mcState@(MiniChess (Evaluated value state) pF) = do
        ps <- pieces (board state)
        
        let kings = filter ((King==) . pieceType) ps
            isDeadKing = (length kings) == 1
            remainingKingColor = (colorOf . head) kings

        if isDeadKing
          then return $ End (Win remainingKingColor)
          else do moves <- moveGen mcState
                  let isDraw = (turn state) > 40
                             || null moves
                  
                  if isDraw
                    then return $ End Draw
                    else return $ Continue moves

    moveGen        (MiniChess    (Evaluated v s) pF)       = chessMoveGen mcMoves s
    pieceMoveGen   (MiniChess    (Evaluated v s) pF)       = moveGenPosition mcMoves s
    validMove      (MiniChess    (Evaluated v s) pF)       = chessValidMove mcMoves s
    doMove         mc                                move  = mcMove mc move
    undoMove       mc                                eDiff = mcUndoMove mc eDiff
    evaluateResult (MiniChess eS@(Evaluated v s) pF)       = mcEvaluateResult s
    score          (MiniChess    (Evaluated v s) pF)       = case turnColor s of
                                                               White -> return v
                                                               Black -> return (-v)
    
    copyState      (MiniChess    (Evaluated v s) pF)       = do gameState' <- copyGameState s
                                                                pawnFiles' <- copyMArray pF
                                                                return $ MiniChess (Evaluated v gameState') pawnFiles'
    gameState      (MiniChess    (Evaluated v s) pF)       = s
