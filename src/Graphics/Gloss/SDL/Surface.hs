{-# LANGUAGE Strict #-}

module Graphics.Gloss.SDL.Surface
  ( bitmapOfSurface
  , bitmapDataOfSurface
  , CacheTexture(..)

  , withSdlSurface
  ) where

import Control.Exception (bracket, bracket_)
import Control.Monad.IO.Class (MonadIO(..))
import Foreign (castPtr, copyBytes, withForeignPtr, mallocForeignPtrBytes)
import Foreign.C.Types (CInt(..))
import Linear.V2 (V2(..))

import qualified Graphics.Gloss.Rendering as Gloss
import qualified SDL.Video.Renderer as SDL

data CacheTexture
  = -- | Surface data is copied to GL texture and never freed.
    Cache
    -- | Copy-draw-free and then again, even if nothing is really changed.
  | NoCache
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Convert SDL Surface to Gloss Picture.
-- Keep the dimensions under the pillow.
bitmapOfSurface
  :: MonadIO m
  => CacheTexture
  -> SDL.Surface
  -> m ((Float, Float), Gloss.Picture)
bitmapOfSurface cache source = do
  copy <- bitmapDataOfSurface cache source
  let (width, height) = Gloss.bitmapSize copy
  pure
    ( (fromIntegral width, fromIntegral height)
    , Gloss.Bitmap copy
    )

-- | Steal pixel data and prepare it for repeated drawing.
bitmapDataOfSurface
  :: MonadIO m
  => CacheTexture
  -> SDL.Surface
  -> m Gloss.BitmapData
bitmapDataOfSurface cache source = liftIO $ do
  dimensions <- SDL.surfaceDimensions source
  let V2 (CInt width) (CInt height) = dimensions

  copy <- withSdlSurface dimensions $ \temporary -> do
    -- XXX: convert whatever the original surface pixel format to OpenGL-ready RGBA8
    _nothing <- SDL.surfaceBlit source Nothing temporary Nothing

    let lock = SDL.lockSurface temporary
    let unlock = SDL.unlockSurface temporary
    bracket_ lock unlock $ do
      pixels <- SDL.surfacePixels temporary
      let size = fromIntegral $ width * height * 4
      destination <- mallocForeignPtrBytes size
      withForeignPtr destination $ \destPtr ->
        copyBytes destPtr (castPtr pixels) size
      pure destination

  pure $
    Gloss.bitmapDataOfForeignPtr
      (fromIntegral width)
      (fromIntegral height)
      (Gloss.BitmapFormat Gloss.TopToBottom Gloss.PxABGR)
      copy
      (cache == Cache)

-- | Do something with a scratch surface, then free it.
withSdlSurface
  :: Integral size
  => V2 size
  -> (SDL.Surface -> IO a)
  -> IO a
withSdlSurface size = bracket create SDL.freeSurface
  where
    sdlSize = fmap fromIntegral size
    create = SDL.createRGBSurface sdlSize SDL.RGBA8888
