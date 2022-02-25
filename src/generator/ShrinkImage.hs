{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wcompat #-}
{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -Wincomplete-record-updates #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}

module ShrinkImage where

import Codec.Picture (DynamicImage (ImageRGBF), PixelRGBF, convertRGB8, readImage, savePngImage)
import qualified Codec.Picture as P
import Codec.Picture.Types (promoteImage)
import qualified Codec.Picture.Types as M
import Control.Monad.ST (runST)
import qualified Data.List
import Data.Tuple.Extra (both)
import Debug.Trace (trace)
import Safe.Foldable (foldl1Def)

shrinkImg :: Int -> Int -> FilePath -> IO ()
shrinkImg newWidth newHeight imgFile = do
  jpImgOrErr <- readImage imgFile
  case jpImgOrErr of
    Left err ->
      putStrLn $ "error (jp) reading image file '" ++ imgFile ++ "': " ++ err
    Right img -> do
      let srcImg = promoteImage $ convertRGB8 img
          smallImg = scaleDownBoxAverage newWidth newHeight srcImg
      savePngImage "smaller-jpextra.png" $ ImageRGBF smallImg

-- | Scale an image using an average of a box of pixels
scaleDownBoxAverage ::
  -- | new width
  Int ->
  -- | new height
  Int ->
  -- | Original image
  P.Image PixelRGBF ->
  -- | Scaled image
  P.Image PixelRGBF
scaleDownBoxAverage newWidth newHeight origImg@P.Image {..} =
  runST $ do
    let origToNewScaleFactor = fromIntegral newWidth / fromIntegral imageWidth
        scaleNewBackToOrig = (/ origToNewScaleFactor) . fromIntegral
        delta = scaleNewBackToOrig 1
    mimg <- M.newMutableImage newWidth newHeight
    let go xNew yNew
          | xNew >= newWidth = go 0 (yNew + 1)
          | yNew >= newHeight = M.unsafeFreezeImage mimg
          | otherwise = do
            let origUpperLeft = both scaleNewBackToOrig (xNew, yNew)
                --gather as many pixels in the original image as are needed to cover one pixel in the new image
                --by adding the scaled value of 1 to each coordinate
                origLowerRight = both (+ delta) origUpperLeft
                --compute the fractions of area that the "borders" of the scaled-down region take up
                tAreaFraction = 1 - (snd origUpperLeft - fromIntegral (floor (snd origUpperLeft)))
                bAreaFraction = 1 - (fromIntegral (ceiling (snd origLowerRight)) - snd origLowerRight)
                lAreaFraction = 1 - (fst origUpperLeft - fromIntegral (floor (fst origUpperLeft)))
                rAreaFraction = 1 - (fromIntegral (ceiling (fst origLowerRight)) - fst origLowerRight)
                totalArea = scaleNewBackToOrig 1 ^ 2 -- exponent binds more loosely than function application
                areaFactor = 1 / totalArea
                --pull out some coordinates we'll need repeatedly below
                lBoundaryCoord = floor $ fst origUpperLeft
                rBoundaryCoord = ceiling $ fst origLowerRight - 1
                tBoundaryCoord = floor $ snd origUpperLeft
                bBoundaryCoord = ceiling $ snd origLowerRight - 1
                --create a 'hp' helper function that specializes 'handlePixelGroup' to apply the constant areaFactor weighting
                pixelAtOrig i j =
                  M.pixelAt
                    origImg
                    (min (imageWidth - 1) i)
                    (min (imageHeight - 1) j)
                hp label extraFactor = handlePixelGroup pixelAtOrig label (extraFactor * areaFactor)
                --use 'hp' to compute nine sets of new pixels: 4 "edge" areas, 4 "corner" areas, and the inner area
                --applying the correct weighting factor for the area they came from
                tPixels = hp "t" tAreaFraction (lBoundaryCoord + 1) (rBoundaryCoord -1) tBoundaryCoord tBoundaryCoord
                bPixels = hp "b" bAreaFraction (lBoundaryCoord + 1) (rBoundaryCoord -1) bBoundaryCoord bBoundaryCoord
                lPixels = hp "l" lAreaFraction lBoundaryCoord lBoundaryCoord (tBoundaryCoord + 1) (bBoundaryCoord -1)
                rPixels = hp "r" rAreaFraction rBoundaryCoord rBoundaryCoord (tBoundaryCoord + 1) (bBoundaryCoord -1)
                tlPixels = hp "tl" (tAreaFraction * lAreaFraction) lBoundaryCoord lBoundaryCoord tBoundaryCoord tBoundaryCoord
                trPixels = hp "tr" (tAreaFraction * rAreaFraction) rBoundaryCoord rBoundaryCoord tBoundaryCoord tBoundaryCoord
                blPixels = hp "bl" (bAreaFraction * lAreaFraction) lBoundaryCoord lBoundaryCoord bBoundaryCoord bBoundaryCoord
                brPixels = hp "br" (bAreaFraction * rAreaFraction) rBoundaryCoord rBoundaryCoord bBoundaryCoord bBoundaryCoord
                innerPixels = hp "inner" 1 (lBoundaryCoord + 1) (rBoundaryCoord -1) (tBoundaryCoord + 1) (bBoundaryCoord -1)
                --gather all those pixels together, plus some debugging strings
                allPixels = [innerPixels, tPixels, bPixels, lPixels, rPixels, tlPixels, trPixels, blPixels, brPixels]
                c1 =
                  "(" ++ show xNew ++ "," ++ show yNew ++ ") -> "
                    ++ show origUpperLeft
                    ++ " .. "
                    ++ show origLowerRight
                    ++ ", area "
                    ++ show totalArea
                c2 = c1 ++ "\n" ++ Data.List.intercalate "\n" (show <$> allPixels) ++ "\n newPixel      : "
                --and finally, add all the weighted pixels together to get the overall weighted average pixel
                newPixel =
                  logIt c2 $
                    foldl1Def
                      (uncurry pixelAtOrig (both floor origUpperLeft))
                      addp
                      $ concatMap snd allPixels
            --write the new pixel into the image and move on to the next one
            M.writePixel mimg xNew yNew newPixel
            go (xNew + 1) yNew
    go 0 0

{-extracts pixels in the given x & y ranges using the given pixelAtOrig function,
  multiplies them by the given factor, and returns the result in a list.
  plus some debugging.-}
handlePixelGroup :: (Int -> Int -> PixelRGBF) -> String -> Float -> Int -> Int -> Int -> Int -> (String, [PixelRGBF])
handlePixelGroup pixelAtOrig label factor xMin xMax yMin yMax =
  let pixelsRaw =
        [ pixelAtOrig x y
          | x <- [xMin .. xMax],
            y <- [yMin .. yMax]
        ]
      pixels = fmap (`mulp` factor) pixelsRaw
      logStr =
        label
          ++ " @ ("
          ++ show xMin
          ++ ","
          ++ show yMin
          ++ ")..("
          ++ show xMax
          ++ ","
          ++ show yMax
          ++ ") Raw: "
          ++ show pixelsRaw
          ++ "; "
          ++ label
          ++ "Pixels (x"
          ++ show factor
          ++ ")"
   in (logStr, pixels)

loggingOn :: Bool
loggingOn = False

logIt :: Show a => String -> a -> a
logIt msg value =
  case loggingOn of
    True ->
      trace (msg ++ ": " ++ show value) value
    False ->
      value

mulp :: PixelRGBF -> Float -> PixelRGBF
mulp pixel x = M.colorMap (* x) pixel
{-# INLINE mulp #-}

addp :: PixelRGBF -> PixelRGBF -> PixelRGBF
addp = M.mixWith (const (+))
{-# INLINE addp #-}
