module ViewportUtils exposing (scrollPosOf, scrollToTop, viewportWithNewSize)

import AlbumStyles exposing (..)
import Browser.Dom exposing (..)
import ResultUtils exposing (..)
import Task exposing (..)



--TODO won't other things about the viewport change if the resize causes the page to reflow???
--TODO why int vs. float confusion?


viewportWithNewSize : Viewport -> Int -> Int -> Viewport
viewportWithNewSize oldViewport newWidth newHeight =
    let
        ov =
            oldViewport.viewport

        newViewport =
            { ov | width = toFloat newWidth, height = toFloat newHeight }
    in
    { oldViewport | viewport = newViewport }


scrollPosOf : Viewport -> Float
scrollPosOf viewport =
    viewport.viewport.y


scrollToTop : msg -> (String -> msg) -> Cmd msg
scrollToTop okMsg errMsg =
    Task.attempt (either (\_ -> okMsg) (\_ -> errMsg rootDivId)) <|
        setViewportOf rootDivId 0 0
