module ListUtils exposing (shiftToBeginning, shiftLeft, shiftRight)

shiftToBeginning : List a -> a -> List a   -> (a, List a)
shiftToBeginning   prevImgs  img  restImgs =
    case prevImgs of
        [] ->
            (img, restImgs)
        prev1 :: prevRest ->
            (prev1, prevRest ++ (img :: restImgs))

{-return a tuple with the new middle element taken from the beginning of the right hand list,
  the old middle element appended to the left hand list, if possible, otherwise do nothing-}
shiftRight : List a -> a -> List a -> (List a, a, List a)
shiftRight   xLefts    x    xRights =
    case xRights of
        [] ->
            (xLefts, x, xRights)
        xRight :: xRightRights ->
            (xLefts ++ [x], xRight, xRightRights)


{-return a tuple with the new middle element taken from the end of the left hand list,
  the old middle element prepended to the right hand list, if possible, otherwise do nothing-}
shiftLeft : List a -> a -> List a -> (List a, a, List a)
shiftLeft   xLefts    x    xRights =
    case xLefts of
        [] ->
            (xLefts, x, xRights)
        [xLeft] ->
            ([], xLeft, x :: xRights)
        xLeft :: xLeftRights ->
            let
                (xLRss, xss, xRss) = shiftLeft xLeftRights x xRights
            in
                (xLeft :: xLRss, xss, xRss)
