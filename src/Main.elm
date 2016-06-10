import Svg exposing (circle, Svg, svg, g, line)
import Svg.Attributes exposing (cx, cy, r, fill, stroke, width, height, viewBox, x1, x2, y1, y2, style)
import Basics exposing (pi, toFloat, sin, cos)

type alias Model = { radius : Int
                   , factor : Int
                   , divisions : Int
                   , center : (Int, Int)}

initialModel = { radius = 300
               , factor = 2
               , divisions = 100
               , center = (300, 300)
               }

type alias Mark = Int
type alias MarkLine = { from: Mark
                      , to: Mark
                      }

type Msg = NoMsg

circleBoard : Model -> Svg Msg
circleBoard model =
  let (x, y) = model.center
  in circle [ cx (toString x), cy (toString y), r (toString model.radius), fill "white", stroke "black" ] []

range n step init =
  if n == 0 then
    []
  else
    init :: (range (n - 1) step (init + step))

landingMarks : Model -> List Mark
landingMarks model =
  range model.divisions 1 0

createLineFromMark : Model -> Mark -> MarkLine
createLineFromMark model mark =
  let result = (model.factor * mark ) % model.divisions
  in MarkLine mark result

landingMarkLines : Model -> List MarkLine
landingMarkLines model =
  let marks = landingMarks model
  in List.map (createLineFromMark model) marks

getAngle : Model -> Mark -> Float
getAngle model mark =
  2 * pi * (toFloat mark) / (toFloat model.divisions)

getCoordinates : Model -> Mark -> (Float, Float)
getCoordinates model mark =
  let (x, y) = model.center
      radius = toFloat model.radius
      angle = getAngle model mark
  in ( (toFloat x) + (0 - radius * (cos (0 - angle)))
     , (toFloat y) + (0 - radius * (sin (0 - angle)))
     )

markToSvg : Model -> Mark -> Svg Msg
markToSvg model mark =
  let (x, y) = getCoordinates model mark
  in
    circle [cx (toString x), cy (toString y), r "2", fill "black"] []

landingLineToSvg : Model -> MarkLine -> Svg Msg
landingLineToSvg model markline =
  let (rx1, ry1) = getCoordinates model markline.from
      (rx2, ry2) = getCoordinates model markline.to
  in line [ x1 (toString rx1)
          , y1 (toString ry1)
          , x2 (toString rx2)
          , y2 (toString ry2)
          , style "stroke:rgba(64,177,84, 0.5);stroke-width:1"
          ] []

drawMarkPoints : Model -> Svg Msg
drawMarkPoints model =
  let marks = landingMarks model
  in
    g [] (List.map (markToSvg model) marks)

drawMarkLines : Model -> Svg Msg
drawMarkLines model =
  let lines = landingMarkLines model
  in
    g [] (List.map (landingLineToSvg model) lines)

drawFigure : Model -> Svg Msg
drawFigure model =
  svg [ width "600", height "600", viewBox "0 0 600 600"]
    [ circleBoard model
   -- , drawMarkPoints model
    , drawMarkLines model
    ]

main =
  drawFigure initialModel
