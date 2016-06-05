{-# LANGUAGE NamedFieldPuns #-}

module Game.UI where

import Linear
import Graphics.Gloss (Picture, Point, translate, pictures)
import qualified Graphics.Gloss as G
import GameGeometry

data UIEvent a b
    = Click     (Pos a)
    | Release   (Pos a)
    | CustomPos (Pos a) b
    | Custom    b

type Update s a b = UIEvent a b -> (s, UI s a b) -> (s, UI s a b)

data UI s a b
    = Container
        { cPos      :: Pos a
        , cArea     :: Area a
        , cPicture  :: Picture
        , cElements :: [UI s a b]
        , cUpdate   :: Update s a b
        }
    | Element
        { ePos      :: Pos a
        , eArea     :: Area a
        , ePicture  :: Picture
        , eUpdate   :: Update s a b
        }

data Area a
    = None
    | All
    | Box        { bTopRight :: Vec a }
    | RoundedBox { rTopRight :: Vec a, rRadius :: a }
    | Circle     { cRadius :: a }

drawUI :: UI s Float b -> Picture
drawUI (Container p _ pic es _) = translate' p . pictures $ pic : map drawUI es
drawUI (Element   p _ pic _   ) = translate' p pic

updateUI :: (Floating a, Ord a) => UIEvent a b -> (s, UI s a b) -> (s, UI s a b)
updateUI e sUI@(_, Element{ePos, eArea, eUpdate})
    | inside e ePos eArea = eUpdate e sUI
    | otherwise           = sUI
updateUI e sUI@(_, Container{cPos, cArea, cUpdate})
    | inside e cPos cArea = (newS, newUI)
    | otherwise           = sUI
    where
        newE = updatePos e cPos
        (s', ui') = cUpdate e sUI
        (newS, newUI) = case ui' of
            Element{}            -> (s', ui')
            Container{cElements} ->
                (\(s'', es) -> (s'', ui'{cElements=es}))
                $ foldStateUI updateUI newE (s', cElements)

foldStateUI :: (e -> (s, a) -> (s, a)) -> e -> (s, [a]) -> (s, [a])
foldStateUI _ _ (s, [])     = (s, [])
foldStateUI f e (s, (x:xs)) =
    let (s', xs') = foldStateUI f e (s, xs)
    in (:xs') <$> f e (s', x)

inside :: (Floating a, Ord a) => UIEvent a b -> Pos a -> Area a -> Bool
inside (Click     x  ) p a = inAreaPos (x-p) a
inside (Release   x  ) p a = inAreaPos (x-p) a
inside (CustomPos x _) p a = inAreaPos (x-p) a
inside (Custom    _  ) _ _ = True

inAreaPos :: (Floating a, Ord a) => Pos a -> Area a -> Bool
inAreaPos _ None = False
inAreaPos _ All  = True
inAreaPos p area = pointInside p $ toPolygon area

updatePos :: Num a => UIEvent a b -> Vec a -> UIEvent a b
updatePos (Click     p  ) v = Click   $ p - v
updatePos (Release   p  ) v = Release $ p - v
updatePos (CustomPos p e) v = CustomPos (p - v) e
updatePos (Custom      e) _ = Custom e

toPolygon :: (Floating a, Eq a) => Area a -> Polygon a
toPolygon (Box (V2 dx dy)) = polygon
    [ V2   dx    dy
    , V2 (-dx)   dy
    , V2 (-dx) (-dy)
    , V2   dx  (-dy)
    ]
toPolygon (Circle r) = polygon $ getArc r 0 (2*pi)
toPolygon (RoundedBox (V2 dx dy) r) = polygon $ concat
    [ map (+ V2   dx    dy)  $ getArc r 0        (pi/2)
    , map (+ V2 (-dx)   dy)  $ getArc r (pi/2)   (pi/2)
    , map (+ V2 (-dx) (-dy)) $ getArc r pi       (pi/2)
    , map (+ V2   dx  (-dy)) $ getArc r (3*pi/2) (pi/2)
    ]
toPolygon _ = polygon []

translate' :: Pos Float -> Picture -> Picture
translate' p =
    let (x, y) = toPoint p
    in translate x y

drawPolygon :: Polygon Float -> Picture
drawPolygon (Polygon ps) = G.polygon . map toPoint $ ps

toPoint :: Pos Float -> Point
toPoint (V2 x y) = (x, y)

getArc :: (Floating a, Eq a) => a -> a -> a -> [Pos a]
getArc r s e =
    [r *^ V2 (cos t) (sin t) | x <- [0..50], let t = s + (e * (realToFrac x) / 50)]
