{-# LANGUAGE Strict #-}

module Main where

import Graphics.Gloss

import Data.Foldable (for_)
import Graphics.Gloss.Interface.IO.Animate (animateIO)
import System.Random (randomRIO)
import Control.Exception (bracket)
import SDL.Vect (V2(..), V4(..), Point(..))

import qualified SDL.Video.Renderer as SDL

import Graphics.Gloss.SDL.Surface (CacheTexture(..),  bitmapOfSurface, withSdlSurface)

main :: IO ()
main = do
  stable <- bakeStable
  animateIO FullScreen black (update stable) (\_ctrl -> pure ())

update :: Picture -> Float -> IO Picture
update stable dt = do
  random <- fmap mconcat . sequenceA $ replicate 10 drawRandom
  pure $ random <> stable

bakeStable :: IO Picture
bakeStable = do
  ((dw, dh), bg) <- withSdlSurface (V2 1000 255) $ \scratch -> do
    -- Draw something...
    SDL.surfaceFillRect scratch Nothing (V4 0 0 0 255)

    for_ [0..255] $ \i -> do
      let rect = SDL.Rectangle (P (V2 0 i)) (V2 250 i)
      let col = V4 (fromIntegral i) 0 0 255
      SDL.surfaceFillRect scratch (Just rect) col

    for_ [0..255] $ \i -> do
      let rect = SDL.Rectangle (P (V2 250 i)) (V2 500 i)
      let col = V4 0 (fromIntegral i) 0 255
      SDL.surfaceFillRect scratch (Just rect) col

    for_ [0..255] $ \i -> do
      let rect = SDL.Rectangle (P (V2 500 i)) (V2 750 i)
      let col = V4 0 0 (fromIntegral i) 255
      SDL.surfaceFillRect scratch (Just rect) col

    for_ [0..255] $ \i -> do
      let rect = SDL.Rectangle (P (V2 750 i)) (V2 1000 i)
      let col = V4 0 0 0 (fromIntegral i)
      SDL.surfaceFillRect scratch (Just rect) col

    bitmapOfSurface Cache scratch

  pure $ mconcat
    [ bg
    , color white $ rectangleWire dw dh
    , translate (negate $ dw * 0.475) (-32) . scale 0.5 0.5 $
        color white $
          text "Stroke font over baked back"
    ]

drawRandom :: IO Picture
drawRandom = do
  size <- V2
    <$> randomRIO (1, 500 :: Int)
    <*> randomRIO (1, 500)

  col <- V4
    <$> randomRIO (0, 255)
    <*> randomRIO (0, 255)
    <*> randomRIO (0, 255)
    <*> randomRIO (128, 255)

  ((h, w), pic) <- withSdlSurface size $ \scratch -> do
    -- Draw something...
    SDL.surfaceFillRect scratch Nothing col
    bitmapOfSurface NoCache scratch

  w' <- randomRIO (negate w, w)
  h' <- randomRIO (negate h, h)
  deg <- randomRIO (-180, 180)
  pure $ rotate deg $ translate w' h' pic
