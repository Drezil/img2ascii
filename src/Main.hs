module Main where

import Codec.Picture
import Codec.Picture.Types
import Data.Maybe (fromJust)
import Data.Word (Word8)
import Data.List as L (transpose,foldl')
import Text.Printf (printf)
import Control.Arrow ((&&&))
import Options.Applicative
import qualified Data.ByteString as B
import System.IO (stdin)


data Options = Options
             { srcFile :: String
             , width   :: Int
             , height  :: Int
             , trueColor :: Bool
             }

options :: Parser Options
options = Options
        <$> argument str (metavar "SRC" <> help "source file (or - for stdin)")
        <*> argument auto (metavar "WIDTH" <> help "resulting width")
        <*> argument auto (metavar "HEIGHT" <> help "resulting height")
        <*> switch (long "256-colors" <> short 'c' <> help "only use 256-color-mode for old terminals")

opthelp :: ParserInfo Options
opthelp = info (helper <*> options)
           ( fullDesc
           <> progDesc "An image to ASCII-Converter"
           <> header "img2ascii - convert images to console-compatible text"
           )

main :: IO ()
main = execParser opthelp >>= run

run :: Options -> IO ()
run (Options src w h redcol) = do
       src' <- if src == "-" then B.getContents else B.readFile src
       case decodeImage src' of
         Left err -> putStrLn err
         Right img ->
            case extractDynImage img >>= pixelize w h of
                 Nothing -> return ()
                 Just (f,b) ->
                     let str = if redcol then img2ascii conv256 (f,b) else img2ascii conv (f,b)
                      in mapM_ (\x -> putStr x >> putStrLn "\x1b[0m") (concat <$> str)

chunksof :: Int -> [a] -> [[a]]
chunksof _ [] = []
chunksof c xs = take c xs : chunksof c (drop c xs)

conv :: (PixelRGB8,PixelRGB8) -> String
conv (fp@(PixelRGB8 fr fg fb),PixelRGB8 br bg bb) = printf "\x1b[48;2;%d;%d;%dm\x1b[38;2;%d;%d;%dm%c" br bg bb fr fg fb (lumi.computeLuma $ fp)
   where
     lumi :: Word8 -> Char
     lumi x
          | x > 225   = '@'
          | x > 180   = 'O'
          | x > 150   = 'X'
          | x > 50    = 'o'
          | x > 25    = 'x'
          | x > 10    = '.'
          | otherwise = ' '

conv256 :: (PixelRGB8,PixelRGB8) -> String
conv256 (fp@(PixelRGB8 fr fg fb),PixelRGB8 br bg bb) = printf "\x1b[48;5;%dm\x1b[38;5;%dm%c" bcolor fcolor (lumi.computeLuma $ fp)
   where
     -- converts [0..255] -> [0..5]
     s = (`div` 51)
     -- conversion: 6x6x6 rgb-cube so color is red * 36 + green * 6 + blue + 16 offset with red/green/blue in [0..5]
     bcolor = s br * 36 + s bg * 6 + s bb + 16
     fcolor = s fr * 36 + s fg * 6 + s fb + 16
     lumi :: Word8 -> Char
     lumi x
          | x > 225   = '@'
          | x > 180   = 'O'
          | x > 150   = 'X'
          | x > 50    = 'o'
          | x > 25    = 'x'
          | x > 10    = '.'
          | otherwise = ' '

img2ascii :: ((PixelRGB8,PixelRGB8) -> String) -> (Image PixelRGB8,Image PixelRGB8) -> [[String]]
img2ascii c (fg@(Image w h _),bg@(Image w' h' _)) = (fmap.fmap) (c.(uncurry (pixelAt fg) &&& uncurry (pixelAt bg))) [[(x,y) | x <- [0..w-1]] | y <- [0..h-1]]

pixelize :: Int -> Int -> Image PixelRGB8 -> Maybe (Image PixelRGB8,Image PixelRGB8)
pixelize tw th im@(Image iw ih id) =
      if windoww == 0 || windowh == 0 then
          Nothing
      else Just (snd $ generateFoldImage (folder filterfun windoww windowh) im tw th,
                 snd $ generateFoldImage (folder filterfuninv windoww windowh) im tw th)
     where
       windoww = (fromIntegral iw) / fromIntegral tw
       windowh = fromIntegral ih / fromIntegral th

folder :: ((PixelRGB8, Int, Int) -> (PixelRGB8, Int, Int) -> (PixelRGB8, Int, Int)) -> Double -> Double -> Image PixelRGB8 -> Int -> Int -> (Image PixelRGB8, PixelRGB8)
folder f ww wh im@(Image iw ih id) x y = (im,(\(a,_,_) -> a) $ L.foldl' f (pixelAt im x' y',0,0)
    [ (pixelAt im (x'+dx) (y'+dy),dx,dy)
    | dx <- [-dw..dw]
    , dy <- [-dw..dw]
    , x'+dx > 0 && x'+dx < iw
    , y'+dy > 0 && y'+dy < ih
    ])
    where
     dw = floor $ ww
     x' = floor $ fromIntegral x * ww
     y' = floor $ fromIntegral y * wh

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
