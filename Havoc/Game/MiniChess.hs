module Havoc.Game.MiniChess where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Havoc.Game
import Havoc.Game.State
import Havoc.Game.Move

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

mcMoves, mcMovesXray :: GameState s -> Position  -> ST s [Square]
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

mcMove :: Evaluated (GameState s) -> Move -> ST s (Evaluated (GameState s), Evaluated MoveDiff)
mcMove es@(Evaluated oldValue state) move@(fromSquare, toSquare) = do
    undoDelta <- mcEvaluatePreMove state move
    (newState, diff) <- chessDoMove state move
    diff <- handlePromotion newState diff
    newValue <- mcEvaluateMove oldValue undoDelta newState diff
    return (Evaluated newValue newState, Evaluated oldValue diff)

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

lastColorSign :: Color -> Int
lastColorSign color = case invertColor color of
                    White -> 1
                    Black -> -1

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
      else do materialDelta   <- naiveMaterialScore state diff
              positionDelta <- positionScore state (toSquare  , becomePiece)
              return $ oldValue + (sign * (materialDelta + positionDelta + undoDelta))

naiveMaterialScore :: GameState s -> MoveDiff -> ST s Score
naiveMaterialScore (GameState turn turnColor board) (MoveDiff movedPiece (fromSquare, toSquare) takenPiece becomePiece) = do
    return $ (score becomePiece - score movedPiece) + score takenPiece
    where
        score Blank                = 0        
        score (Piece color Pawn)   = 250
        score (Piece color Knight) = 300
        score (Piece color Bishop) = 500
        score (Piece color Rook)   = 500
        score (Piece color Queen)  = 900
        score (Piece color King)   = 0
        
positionScore :: GameState s -> Position -> ST s Score
positionScore state          (_     , Blank) = return 0
positionScore state position@(square, piece) = do
    classScore <- case piece of
        Piece _ Pawn   -> liftM ((10*) . length) $ dirMoves (IsPiece piece) [Northeast, Northwest, Southeast, Southwest] state square
        otherwise      -> return 0
    
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

newtype MiniChess s = MiniChess (Evaluated (GameState s))
instance Game MiniChess where
    {-# SPECIALIZE instance Game MiniChess #-}
    startState = mcStartBoard >>= (\board -> return $ MiniChess $ Evaluated 0 $ GameState 1 White board)

    gameStatus mcState@(MiniChess (Evaluated value state)) = do
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

    moveGen        (MiniChess    (Evaluated v s))       = chessMoveGen mcMoves s
    pieceMoveGen   (MiniChess    (Evaluated v s))       = moveGenPosition mcMoves s
    validMove      (MiniChess    (Evaluated v s))       = chessValidMove mcMoves s
    doMove         (MiniChess es@(Evaluated v s)) move  = mcMove es move >>= (\(es', d) -> return (MiniChess es', d))
    undoMove       (MiniChess es@(Evaluated v s)) ediff = (chessUndoMoveEval es ediff) >>= return . MiniChess
    evaluateResult (MiniChess es@(Evaluated v s))       = mcEvaluateResult s
    score          (MiniChess    (Evaluated v s))       = case turnColor s of
                                                            White -> return v
                                                            Black -> return (-v)
    
    copyState      (MiniChess    (Evaluated v s))       = copyGameState s >>= (\s' -> return $ MiniChess $ Evaluated v $ s')
    gameState      (MiniChess    (Evaluated v s))       = s
