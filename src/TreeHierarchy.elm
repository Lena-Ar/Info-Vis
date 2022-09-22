module TreeHierarchy exposing (..)

import Browser
import Http
import Json.Decode
import Dict
import Scale
import Statistics
import Html exposing (Html, div, text)
import TypedSvg exposing (circle, g, line, path, rect, style, svg, text_)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Attributes exposing (d, class, stroke, strokeWidth, fill, textAnchor, transform, fontFamily, fontSize, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, x1, x2, y1, y2, x, y)
import TypedSvg.Types as ST exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))
import Color
import TreeLayout
import Tree exposing (Tree)