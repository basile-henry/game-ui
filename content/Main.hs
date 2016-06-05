module Main where

import Linear
import Graphics.Gloss.Interface.Pure.Game hiding (Circle)
import Game.UI
import GameGeometry

import Debug.Trace (traceShow)

data State = One | Two deriving Show
type UIState = [State]

demoUI :: UI UIState Float b
demoUI = Container zero All Blank
    [ circleBtn (V2 (-150)  150)  0
    , circleBtn (V2    50 (-100)) 1
    ]
    (\_ -> id)

circleBtn :: Pos Float -> Int -> UI UIState Float b
circleBtn pos i =
    let area r = RoundedBox (V2 r 30.0) (r / 5)
        cBtn c r = Element pos (area r) (color c . drawPolygon . toPolygon $ area r)
        update e (s, ui) = case (s!!i, e) of
            (One, Click _) -> (replace s i Two, b2)
            (Two, Click _) -> (replace s i One, b1)
            _              -> (s, ui)
        b1 = cBtn red   50.0 update
        b2 = cBtn blue 100.0 update
    in b1

main :: IO ()
main = play
    (InWindow "Demo" (500, 500) (10, 10))
    white
    60
    ([One, One], demoUI)
    (drawUI . snd)
    updateE
    (flip const)


updateE :: (Floating a, Ord a) => Event -> (UIState, UI UIState a b) -> (UIState, UI UIState a b)
updateE (EventKey (MouseButton LeftButton) Down _ (x, y)) (s, ui) = updateUI (Click $ V2 (realToFrac x) (realToFrac y)) (s, ui)
updateE _                                                  sui    = sui

replace :: [a] -> Int -> a -> [a]
replace xs i x = take i xs ++ [x] ++ drop (i+1) xs
