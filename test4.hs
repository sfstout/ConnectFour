-- Program structure based off of https://github.com/ku-fpg/blank-canvas/wiki/Tic-Tac-Toe

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Graphics.Blank
import Data.Text (Text)
import Data.List

type Color = Text

data RY = R | Y deriving (Eq, Ord, Show)

-- Ternary operator from:
-- https://wiki.haskell.org/Ternary_operator

data Cond a = a :? a
 
infixl 0 ?
infixl 1 :?
  
(?) :: Bool -> Cond a -> a
True  ? (x :? _) = x
False ? (_ :? y) = y

-- End ternary operator

modifyPlace :: [Int] -> Int -> [Int]
modifyPlace [] _ = []
modifyPlace (0:xs) v = v : xs
modifyPlace (x:xs) v = x : (modifyPlace xs v)

-- Board -> Column -> Color -> Board
modifyBoard :: [[Int]] -> Int -> Int -> [[Int]]
modifyBoard [] _ _ = []
modifyBoard (x:xs) 0 v = (modifyPlace x v) : xs
modifyBoard (x:xs) i v = x : (modifyBoard xs (i-1) v)

-- Board -> Column -> Color -> Board
modBoard :: [[Int]] -> [Maybe Int] -> Int -> [[Int]]
modBoard b [] c = b
modBoard b ((Just x):xs) c = modBoard (modifyBoard b x c) xs c
modBoard b (Nothing:xs) c = modBoard b xs c

changeTurn :: RY -> RY
changeTurn R = Y
changeTurn Y = R

pieceSize = 25

origCols = take 6 (repeat 0)
originalBoard = take 7 (repeat origCols)

main :: IO ()
main = blankCanvas 3000 { events = ["mousedown"] } $ go

go :: DeviceContext -> IO ()
go context = do
        let boardSize  = min (height context) (width context)
        let limit = 3*(boardSize/7) + 4

        let getX :: Double -> Maybe Int
            getX x
              | x > -3.5*(boardSize/8)+(width context / 2) && x < -2.5*(boardSize/8)+(width context / 2) = Just (0)
              | x > -2.5*(boardSize/8)+(width context / 2) && x < -1.5*(boardSize/8)+(width context / 2) = Just (1)
              | x > -1.5*(boardSize/8)+(width context / 2) && x < -0.5*(boardSize/8)+(width context / 2) =Just (2)
              | x > -0.5*(boardSize/8)+(width context / 2) && x < 0.5*(boardSize/8)+(width context / 2) =Just (3)
              | x > 0.5*(boardSize/8)+(width context / 2) && x < 1.5*(boardSize/8)+(width context / 2) =Just (4)
              | x > 1.5*(boardSize/8)+(width context / 2) && x < 2.5*(boardSize/8)+(width context / 2) =Just (5)
              | x > 2.5*(boardSize/8)+(width context / 2) && x < 3.5*(boardSize/8)+(width context / 2) =Just (6)
              | otherwise = Nothing

        let loop (board, turn) = do
                send context $ do 
                        -- Draw the board
                        sequence_ [ do save()
                                       translate (width context / 2, height context / 2)
                                       drawThickLine (x, -limit) (x, limit) 
                                       drawThickLine (-limit, y) (limit, y)
                                       renderAllPieces board boardSize
                                       restore()
                                       | x <- map (*(boardSize/8)) [-3.5..3.5],
                                         y <- map (*(boardSize/7)) [-3..3]
                                  ]

                es <- flush context

                let slot = [getX x | Just (x,y) <- map ePageXY es]
                let moddedBoard = modBoard board slot ((turn == R) ? 1 :? 2)
                threadDelay(20*1000)

                (checkForWinner board == False) ? (loop (moddedBoard, (moddedBoard == board) ? turn :? (changeTurn turn))) :? (winner context "Winner!") -- Only change turns if the previous player went
                        
        loop (originalBoard,R) -- Red goes first

-- Based off of https://github.com/ku-fpg/blank-canvas/wiki/Line
drawLine :: Double -> (Double, Double) -> (Double, Double) -> Canvas ()
drawLine thickness (x,y) (x', y') = do
        beginPath()
        lineWidth thickness
        strokeStyle "#0000ff"
        moveTo(x,y)
        lineTo(x',y')
        stroke()

drawThickLine = drawLine 10

-- Based off of https://github.com/ku-fpg/blank-canvas/wiki/Bounce
showPiece :: (Double, Double) -> Color -> Canvas ()
showPiece (x,y) col = do
        beginPath()
        globalAlpha 0.5
        fillStyle col
        arc(x, y, pieceSize, 0, pi*2, False)
        closePath()
        fill()

winner c s = send c $ do
       let x = width c / 2
       let y = height c / 2
       textAlign "center"
       textBaseline "bottom"
       font "60pt Helvetica"
       lineWidth 3
       fillStyle "#000000"
       fillText(s, x, y)

type Column = Int
type Row = Int
type BSize = Double

translateC :: Int -> Double
translateC 0 = -3.0   
translateC 1 = -2.0   
translateC 2 = -1.0   
translateC 3 = 0.0
translateC 4 = 1.0   
translateC 5 = 2.0   
translateC 6 = 3.0   

translateR :: Int -> Double
translateR 0 = 2.5   
translateR 1 = 1.5   
translateR 2 = 0.5   
translateR 3 = -0.5
translateR 4 = -1.5   
translateR 5 = -2.5   

isMember :: Bool -> [Bool] -> Bool
isMember _ [] = False
isMember b (x:xs) = if x == b then True else isMember b xs

-- List to check through -> Number left to find -> What to search for -> Result
checkForRun :: [Maybe Int] -> Int -> Int -> Bool
checkForRun _ 0 _ = True
checkForRun [] l s = False
checkForRun ((Just 1):xs) l s = if s == 1 then checkForRun xs (l - 1) s else checkForRun xs 4 s
checkForRun ((Just 2):xs) l s = if s == 2 then checkForRun xs (l - 1) s else checkForRun xs 4 s
checkForRun (x:xs) l s = checkForRun xs 4 s

checkColumnEntry :: [[Int]] -> Int -> Bool
checkColumnEntry xs c = if checkForRun [returnNumFromColRow xs (c, 0 + ro) | ro <- [0..5]] 4 1 || checkForRun [returnNumFromColRow xs (c, 0 + ro) | ro <- [0..5]] 4 2 then True else False

checkForWinner :: [[Int]] -> Bool
checkForWinner xxs = if checkFullColumn xxs || checkFullRow xxs || checkFullDiagonal xxs then True else False 

checkFullDiagonal :: [[Int]] -> Bool
checkFullDiagonal xs = isMember True (map (checkDDiagonalEntry xs) [0..11]) || (checkUDiagonalEntry xs [(0,2),(0,1),(0,0),(1,0),(2,0),(3,0)])

getDDiagonal :: Int -> [(Int, Int)]
getDDiagonal n = getDDiagonalH n 0

getDDiagonalH :: Int -> Int -> [(Int, Int)]
getDDiagonalH 0 0 = [(0,0)]
getDDiagonalH 0 n = [(0,n)]
getDDiagonalH n m = (n, m) : (getDDiagonalH (n-1) (m+1))

getUDiagonal :: (Int, Int) -> [(Int, Int)]
getUDiagonal n = getUDiagonalH n 0

getUDiagonalH :: (Int, Int) -> Int -> [(Int, Int)]
getUDiagonalH (i,j) 5 = [(i,j)]
getUDiagonalH (i,j) m = (i, j) : (getUDiagonalH (i+1,j+1) (m + 1))

checkUDiagonalEntry :: [[Int]] -> [(Int, Int)] -> Bool
checkUDiagonalEntry xxs [] = False
checkUDiagonalEntry xxs (x:xs) = if checkForRun (map (returnNumFromColRow xxs) (getUDiagonal x)) 4 1 || checkForRun (map (returnNumFromColRow xxs) (getUDiagonal x)) 4 2 then True else checkUDiagonalEntry xxs xs

checkDDiagonalEntry :: [[Int]] -> Int -> Bool
checkDDiagonalEntry xs d = if checkForRun (map (returnNumFromColRow xs) (getDDiagonal d)) 4 2 || checkForRun (map (returnNumFromColRow xs) (getDDiagonal d)) 4 1 then True else False

checkFullColumn :: [[Int]] -> Bool
checkFullColumn xs = isMember True (map (checkColumnEntry xs) [0..6])

checkRowEntry :: [[Int]] -> Int -> Bool
checkRowEntry xs r = if checkForRun [returnNumFromColRow xs (0 + co, r) | co <- [0..6]] 4 1 || checkForRun [returnNumFromColRow xs (0 + co, r) | co <- [0..6]] 4 2 then True else False 

checkFullRow :: [[Int]] -> Bool
checkFullRow xs = isMember True (map (checkRowEntry xs) [0..5])

returnNumFromColRow :: [[Int]] -> (Column, Row) -> Maybe Int
returnNumFromColRow xs (c,r) = if c >= 0 && c < 7 && r >= 0 && r < 6 then (Just ((xs !! c) !! r)) else Nothing

renderColumn :: [Int] -> BSize -> Column -> Row -> Canvas ()
renderColumn [] _ _ _ = do
                    beginPath()
                    closePath()
renderColumn (0:xs) _ _ _ = do
                       beginPath()
                       closePath()
renderColumn (1:xs) s c r = do 
                             showPiece (translateC (c) *(s/8), translateR (r) *(s/7)) "#ff0000"
                             renderColumn xs s c (r + 1)
renderColumn (2:xs) s c r = do
                             showPiece (translateC (c) *(s/8), translateR (r) *(s/7)) "#ffff00"
                             renderColumn xs s c (r + 1)

renderPieces :: [[Int]] -> BSize -> Column -> Canvas ()
renderPieces [] _  _ = do 
                   beginPath()
                   closePath()
renderPieces (x:xs) s c = do
                        renderColumn x s c 0
                        renderPieces xs s (c + 1)

renderAllPieces :: [[Int]] -> BSize -> Canvas ()
renderAllPieces x s = renderPieces x s 0
