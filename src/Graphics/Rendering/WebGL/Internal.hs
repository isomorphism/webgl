module Graphics.Rendering.WebGL.Internal where

import GHCJS.Types
import Data.Word
import Data.Int

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import GHCJS.TypedArray (TypedArray, ArrayBufferView)
import Graphics.Rendering.WebGL.Types

liftGL :: (MonadGL m) => (WebGLContext -> IO a) -> m a
liftGL x = liftIO . x =<< getContext

liftGL1 f x1 = liftGL $ f x1
liftGL2 f x1 x2 = liftGL $ f x1 x2
liftGL3 f x1 x2 x3 = liftGL $ f x1 x2 x3
liftGL4 f x1 x2 x3 x4 = liftGL $ f x1 x2 x3 x4
liftGL5 f x1 x2 x3 x4 x5 = liftGL $ f x1 x2 x3 x4 x5
