module Main (main) where

import Lib

import Graphics.UI.GLUT
import Data.IORef

divisor = 24

data AppState = AS {step :: GLfloat}

initialAppState :: AppState
initialAppState = (AS 0.0)

nextAppState :: AppState -> AppState
nextAppState (AS st) = (AS st')
  where
    st' = if st + increment > 1.0 then 0.0 else st + increment
    increment = 1.0 / divisor

toRect :: (GLfloat,GLfloat,GLfloat) -> [(GLfloat,GLfloat,GLfloat)]
toRect (x, y, z) = [(x, y, z), (x + 0.1, y, z), (x + 0.1, y + 0.1, z), (x, y + 0.1, z)]

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = concatMap toRect $ map (\(x, y, z) -> (x * 0.9, y * 0.9, z * 0.9)) [ (sin (2*pi*k/divisor), cos (2*pi*k/divisor), 0) | k <- [1..divisor] ]

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  stateRef <- newIORef initialAppState
  _window <- createWindow "Hello World"
  displayCallback $= (display stateRef)
  mainLoop

display :: IORef AppState -> DisplayCallback
display _ = do 
  clear [ColorBuffer]
  renderPrimitive Quads $
     mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
  flush