import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

data D4 = R0 | R1 | R2 | R3 | V | DR | H | DL
  deriving (Eq, Ord, Bounded, Enum)

eltSign :: D4 -> Diagram B
eltSign R0 = circle 0.05 # fc black
eltSign R1 = arc' arcR xDir (1/4 @@ turn) <> rotHead # rotateBy (1/4)
eltSign R2 = arc' arcR xDir (1/2 @@ turn) <> rotHead # rotateBy (1/2)
eltSign R3 = arc' arcR xDir (3/4 @@ turn) <> rotHead # rotateBy (3/4)
eltSign V  = vrule 1.3 # dashed
eltSign DR = vrule (1.25 * sqrt 2) # rotateBy (-1/8) # dashed
eltSign H  = hrule 1.3 # dashed
eltSign DL = vrule (1.25 * sqrt 2) # rotateBy (1/8) # dashed

rotHead = triangle 0.08 # scaleX 0.8 # translateX arcR

arcR = 0.3
-- rp0 = arcR ^& 0
-- rp1 = rp0 # rotateBy (1/4)
-- rp2 = rp0 # rotateBy (1/2)
-- rp3 = rp0 # rotateBy (3/4)

dashed = dashingL [0.1, 0.1] 0

elt :: D4 -> Diagram B
elt d = eltSign d <> square 1 <> square 1.3 # lw none

tr :: D4 -> Transformation V2 Double
tr R0 = mempty
tr R1 = rotation (1/4 @@ turn)
tr R2 = rotation (1/2 @@ turn)
tr R3 = rotation (3/4 @@ turn)
tr V  = reflectionX
tr DR = conjugate (rotation (1/8 @@ turn)) reflectionX
tr H  = reflectionY
tr DL = conjugate (rotation (-1/8 @@ turn)) reflectionX

eff :: Diagram B
eff = text "F" # fontSizeL 0.7 <> square 1

d4Display :: Diagram B
d4Display = vsep 0.2 (map displayElt [R0 .. DL])

displayElt :: D4 -> Diagram B
displayElt d = hsep 0.5
  [ elt d
  , text ":"
  , eff
  , arrowV unitX
  , transform (tr d) eff
  ]

d4Strip :: [Diagram B]
d4Strip = map elt [R0 .. DL]

d4Table :: Diagram B
d4Table =
  composeAligned alignR (vsep 0.3)
    [ hcat d4Strip
    , composeAligned alignT (hsep 0.3)
      [ vcat d4Strip
      , d4Grid
      ]
    ]

d4Grid = bars <> rotateBy (1/4) bars
  where
    bars = hsep 1.3 (replicate 9 (vrule (8*1.3))) # centerXY

eqn a b c = hsep 0.5
  [ elt a
  , text ";"
  , elt b
  , text "="
  , elt c
  ]
  # fontSizeL 0.7

main = mainWith $
  [ ("table", d4Table # frame 0.5)
  , ("d4", d4Display # frame 0.4)
  , ("dot", elt R0 # lwL 0.05 # frame 0.1)
  , ("r1", elt R1 # lwL 0.05 # frame 0.1)
  , ("r1row", displayElt R1 # lwL 0.05 # frame 0.1)
  , ("f", eff # lwL 0.05 # frame 0.1)
  , ("r1f", transform (tr R1) eff # lwL 0.05 # frame 0.1)
  , ("vv", eqn V V R0 # lwL 0.05 # frame 0.1)
  , ("dh", eqn DR H R3 # lwL 0.05 # frame 0.1)
  ]
