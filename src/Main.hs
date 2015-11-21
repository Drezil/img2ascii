module Main where

import Codec.Picture
import Codec.Picture.Types
import Data.Maybe (fromJust)
import Data.Word (Word8)
import Data.List (transpose)
import Text.Printf (printf)
import Control.Arrow ((&&&))

targetWidth :: Int
targetWidth = 80
targetHeight :: Int
targetHeight = 40

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
                   Just (f,b) -> do
                     savePngImage "test.png" (ImageRGB8 b)
                     str <- return $ img2ascii conv (f,b)
                     mapM_ (\x -> putStr x >> putStrLn "\x1b[0m") (concat <$> str)
              Nothing -> return ()

chunksof :: Int -> [a] -> [[a]]
chunksof _ [] = []
chunksof c xs = take c xs : chunksof c (drop c xs)

conv :: (PixelRGB8,PixelRGB8) -> String
conv (fp@(PixelRGB8 fr fg fb),PixelRGB8 br bg bb) = printf "\x1b[48;2;%d;%d;%dm\x1b[38;2;%d;%d;%dm%c" br bg bb fr fg fb (lumi.computeLuma $ fp)
   where
     lumi :: Word8 -> Char
     lumi x 
          | x > 225 = '@'
          | x > 180 = 'O'
          | x > 150 = 'X'
          | x > 50 = 'o'
          | x > 25 = 'x'
          | x > 10 = '.'
          | otherwise = ' '

img2ascii :: ((PixelRGB8,PixelRGB8) -> String) -> (Image PixelRGB8,Image PixelRGB8) -> [[String]]
img2ascii c (fg@(Image w h _),bg@(Image w' h' _)) = (fmap.fmap) (c.(uncurry (pixelAt fg) &&& uncurry (pixelAt bg))) [[(x,y) | x <- [0..w-1]] | y <- [0..h-1]]

pixelize :: Image PixelRGB8 -> Int -> Int -> Maybe (Image PixelRGB8,Image PixelRGB8)
pixelize im@(Image iw ih id) tw th =
      if windoww == 0 || windowh == 0 then
          Nothing
      else Just $ (snd $ generateFoldImage (folder filterfun windoww windowh) im tw th,
                   snd $ generateFoldImage (folder filterfuninv windoww windowh) im tw th)
     where
       windoww = iw `div` tw
       windowh = ih `div` th

folder :: ((PixelRGB8, Int, Int) -> (PixelRGB8, Int, Int) -> (PixelRGB8, Int, Int)) -> Int -> Int -> Image PixelRGB8 -> Int -> Int -> (Image PixelRGB8, PixelRGB8)
folder f ww wh im@(Image iw ih id) x y = (im,(\(a,_,_) -> a) $ foldl1 f
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

filterfuninv :: (PixelRGB8,Int,Int) -> (PixelRGB8, Int, Int) -> (PixelRGB8,Int,Int)
filterfuninv (x@(PixelRGB8 r g b),_,_) (y@(PixelRGB8 r' g' b'),_,_) = if computeLuma x < computeLuma y then (x,0,0) else (y,0,0)

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
