module Main where

import Codec.Picture
import Codec.Picture.Types
import Data.Maybe (fromJust)
import Data.Word (Word8)
import Data.List (transpose)

targetWidth :: Int
targetWidth = 80
targetHeight :: Int
targetHeight = 50

main :: IO ()
main = do
       img' <- readImage "test.jpg"
       case img' of
         Left err -> putStrLn err
         Right img -> do
            src <- return $ extractDynImage img
            case src of
              (Just s) -> do
                 pix <- return $ pixelize s targetWidth targetHeight
                 case pix of
                   Nothing -> return ()
                   Just c -> do
                     savePngImage "test.png" (ImageRGB8 c)
                     str <- return $ img2ascii conv c
                     mapM_ putStrLn (transpose $ chunksof targetHeight str)
              Nothing -> return ()

chunksof :: Int -> [a] -> [[a]]
chunksof _ [] = []
chunksof c xs = take c xs : chunksof c (drop c xs)

conv :: Word8 -> Char
conv x 
  | x > 225 = '@'
  | x > 180 = 'O'
  | x > 150 = 'o'
  | x > 50 = '.'
  | otherwise = ' '

img2ascii :: (Word8 -> Char) -> Image PixelRGB8 -> String
img2ascii c im@(Image w h _) = do
   x <- [0..w-1]
   y <- [0..h-1]
   return $ c (255-computeLuma (pixelAt im x y))

pixelize :: Image PixelRGB8 -> Int -> Int -> Maybe (Image PixelRGB8)
pixelize im@(Image iw ih id) tw th =
      if windoww == 0 || windowh == 0 then
          Nothing
      else Just $ snd $ generateFoldImage (folder windoww windowh) im tw th
     where
       windoww = iw `div` tw
       windowh = ih `div` th

folder :: Int -> Int -> Image PixelRGB8 -> Int -> Int -> (Image PixelRGB8, PixelRGB8)
folder ww wh im@(Image iw ih id) x y = (im,(\(a,_,_) -> a) $ foldl1 filterfun
    [ (pixelAt im (x'+dx) (y'+dy),dx,dy)
    | dx <- [-(ww `div` 2)..ww - (ww `div`2)]
    , dy <- [-(ww `div` 2)..ww - (ww `div`2)]
    , x'+dx > 0 && x'+dx < iw
    , y'+dy > 0 && y'+dy < ih
    ])
    where
     x' = x*ww
     y' = y*wh

filterfun :: (PixelRGB8,Int,Int) -> (PixelRGB8, Int, Int) -> (PixelRGB8,Int,Int)
filterfun (x@(PixelRGB8 r g b),_,_) (y@(PixelRGB8 r' g' b'),_,_) = if computeLuma x > computeLuma y then (x,0,0) else (y,0,0)

extractDynImage :: DynamicImage -> Maybe (Image PixelRGB8)
extractDynImage image =
         case image of
                ImageY8 img     -> Just $ promoteImage img
                ImageY16 img    -> Nothing
                ImageYF img     -> Nothing
                ImageYA8 img    -> Just $ promoteImage img
                ImageYA16 img   -> Nothing
                ImageRGB8 img   -> Just img
                ImageRGB16 img  -> Nothing
                ImageRGBF img   -> Nothing
                ImageRGBA8 img  -> Just $ pixelMap dropTransparency img
                ImageRGBA16 img -> Nothing
                ImageYCbCr8 img -> Just $ convertImage img
                ImageCMYK8 img  -> Just $ convertImage img
                ImageCMYK16 img -> Nothing
