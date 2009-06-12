module Havoc.Game.MiniChess where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Havoc.Game
import Havoc.Game.State
import Havoc.Game.Move
import Havoc.Globals (checkPawnFiles, checkPawnEval)
import Havoc.Utils (copyMArray, swap)
import Debug.Trace

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
    updatePawnFile movedPiece  fromFile (-pawnValue)
    updatePawnFile becomePiece toFile     pawnValue
    updatePawnFile takenPiece  toFile   (-pawnValue)
    where
        ((_, fromFile), (_, toFile)) = move
        pawnValue = if not undo then 1 else -1
        
        isPawn piece = (piece /= Blank) && (pieceType piece == Pawn)

        updatePawnFile (Piece color Pawn) file value = do
            let index = (color,file)
            count <- readArray pawnFiles index
            writeArray pawnFiles index (count + value)
        updatePawnFile _     _ _ = return ()

mcMove :: MiniChess s -> Move -> ST s (MiniChess s, Evaluated MoveDiff)
mcMove (MiniChess eS@(Evaluated oldValue state) pF) move@(fromSquare, toSquare) = do
    -- Evaluate the previous state
    undoDelta <- mcEvaluatePreMove state pF move
    
    -- Perform the move
    (newState, diff) <- chessDoMove state move
    diff <- handlePromotion newState diff
    updatePawnFiles pF diff False
    
    when checkPawnFiles printPawnFiles
    
    -- Evaluate the new state
    newValue <- mcEvaluateMove newState pF diff oldValue undoDelta
    return (MiniChess (Evaluated newValue newState) pF, Evaluated oldValue diff)
    where
        printPawnFiles = do
            elems <- getElems pF
            st <- showGameState state
            return $! trace (st ++ show elems ++ "\n---") ()

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

mcEvaluatePreMove :: GameState s -> PawnFiles s -> Move -> ST s Score
mcEvaluatePreMove state@(GameState turn turnColor board) pF (fromSquare, toSquare) = do
    movedPiece <- readArray board fromSquare
    takenPiece <- readArray board toSquare
    movedPastScore <- positionScore state pF (fromSquare, movedPiece)
    takenPastScore <- positionScore state pF (toSquare  , takenPiece)
    return $ -movedPastScore + takenPastScore

mcEvaluateMove :: GameState s -> PawnFiles s -> MoveDiff -> Score -> Score -> ST s Score
mcEvaluateMove state pawnFiles diff@(MoveDiff movedPiece (fromSquare, toSquare) takenPiece becomePiece) oldValue undoDelta = do
    -- In the MiniChess type, scores are stored with static sign, with positive values in White's favor, and negative values in Black's favor. In this module, scores are treated with relative sign, with positive scores in the current player's favor. We convert the sign scheme here.
    let sign = lastColorSign (turnColor state)
    if takenPiece /= Blank && (pieceType takenPiece) == King
      then return $ sign * (winScore state)
      else do materialDelta <- naiveMaterialScore state diff
              positionDelta <- positionScore state pawnFiles (toSquare, becomePiece)
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
        
positionScore :: GameState s -> PawnFiles s -> Position -> ST s Score
positionScore state pawnFiles          (_     , Blank) = return 0
positionScore state pawnFiles position@(square@(row,col), piece@(Piece color pieceType)) = do
    playerStartRow <- startRow color (board state)
    let height = abs (playerStartRow - row)
        opponentColor = invertColor color
    
    classScore <- case piece of
        Piece _ Pawn   -> do
            supporting <- countPawns [Northeast, Northwest, Southeast, Southwest]
            adjacent   <- countPawns [East, West]
            column     <- countPawns [North, South]

            ((_,minCol), (_,maxCol)) <- getBounds pawnFiles            
            let files = filter (inRange (minCol,maxCol)) $ map (col+) [-1..1]
            enemyFiles    <- (liftM (length . filter id)) $ countFiles files opponentColor                         
            guardedFiles' <- (liftM (length . filter id)) $ countFiles files color
            let guardedFiles = guardedFiles' - 1
            
            let isPassed = if enemyFiles == 0 then 1 else 0
                score = 2  * height
                      + 20 * supporting 
                      + 5  * adjacent
                      - 2  * column
                      - 10 * enemyFiles
                      + 5  * guardedFiles
                      + 50 * isPassed
                      + 8
            
            when checkPawnEval (return $! trace (show square ++ " | " ++ show [height,supporting,adjacent,column,enemyFiles,guardedFiles,isPassed] ++ ": " ++ show score) ())
            
            return score 
                   
            where
                countPawns dirs = (liftM length) $ dirMoves (IsPiece piece) dirs state square
                countFiles files color = forM files (\file -> do
                        pawns <- readArray pawnFiles (color, file)
                        return (pawns >= 1))
        
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

type PawnFiles s = STUArray s (Color,Int) Int
data MiniChess s = MiniChess { mcState   :: Evaluated (GameState s)
                             , pawnFiles :: PawnFiles s             }
instance Game MiniChess where
    {-# SPECIALIZE instance Game MiniChess #-}
    fromBoard board = do
        ((li,lj),(ui,uj)) <- getBounds board

        let mcState = Evaluated 0 $ GameState 1 White board
        pawnFiles <- newArray ((White,lj), (Black,uj)) 1
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
