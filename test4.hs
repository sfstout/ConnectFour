{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Graphics.Blank
import Data.Text (Text)

type Piece a = ((Double, Double), Double, a) -- ((X Pos, Y Pos), Speed/momentum, Color )

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

main :: IO ()
main = blankCanvas 3000 { events = ["mousedown"] } $ go

go :: DeviceContext -> IO ()
go context = do
        let boardSize  = min (height context) (width context)
        let limit = 3*(boardSize/7) + 4

        let origCols = take 6 (repeat 0)
        let originalBoard = take 7 (repeat origCols)

        if (True) then print originalBoard else print originalBoard

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
--                                       showPiece (-3*(boardSize/8), 2.5*(boardSize/7)) "red"
                                       restore()
                                       | x <- map (*(boardSize/8)) [-3.5..3.5],
                                         y <- map (*(boardSize/7)) [-3..3]
                                  ]
                        --

                --


                es <- flush context
                if (null es) then return () else print es
                if (null es) then return () else print board

                let slot = [getX x | Just (x,y) <- map ePageXY es]
                if (length slot == 0) then return () else print slot
--                print turn
                threadDelay(20*1000)
-- modifyBoard :: Board -> number of color -> column piece is placed 
                loop (modBoard board slot ((turn == Y) ? 1 :? 2), null es ? turn :? (changeTurn turn)) -- Only change turns if the previous player went
--                loop (board, null es ? turn :? (changeTurn turn)) -- Only change turns if the previous player went
        loop (originalBoard,R) -- Red goes first

drawLine :: Double -> (Double, Double) -> (Double, Double) -> Canvas ()
drawLine thickness (x,y) (x', y') = do
        beginPath()
        lineWidth thickness
        strokeStyle "#0000ff"
        moveTo(x,y)
        lineTo(x',y')
        stroke()

drawThickLine = drawLine 10

showPiece :: (Double, Double) -> Color -> Canvas ()
showPiece (x,y) col = do
        beginPath()
        globalAlpha 0.5
        fillStyle col
        arc(x, y, pieceSize, 0, pi*2, False)
        closePath()
        fill()

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
