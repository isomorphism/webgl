{-# LANGUAGE DataKinds #-}
module Graphics.Rendering.WebGL (
    module GHCJS.DOM.HTMLCanvasElement,
    module Graphics.Rendering.WebGL.Matrix,
    module Graphics.Rendering.WebGL,
    module Graphics.Rendering.WebGL.EnumConvert,
    module Graphics.Rendering.WebGL.Types
    ) where

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.DOM.Types
import GHCJS.DOM.Event
import GHCJS.DOM.EventM

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import GHCJS.TypedArray
import Graphics.Rendering.WebGL.Matrix
import Graphics.Rendering.WebGL.Matrix.Types
import Graphics.Rendering.WebGL.Types
import Graphics.Rendering.WebGL.EnumConvert
import Graphics.Rendering.WebGL.Raw

import GHCJS.DOM.HTMLCanvasElement

setClearColor :: WebGLContext -> Vec 4 Imm Float -> IO ()
setClearColor cxt (Vec v) = js_setClearColor cxt v

uniform3fv :: WebGLContext -> WebGLUniformLocation -> Vec 3 t Float -> IO ()
uniform3fv cxt ufm (Vec v) = js_uniform3fv cxt ufm v

uniformMatrix3fv :: WebGLContext -> WebGLUniformLocation -> Bool -> Mat 3 t Float -> IO ()
uniformMatrix3fv cxt ufm b (Mat v) = js_uniformMatrix3fv cxt ufm b v

uniformMatrix4fv :: WebGLContext -> WebGLUniformLocation -> Bool -> Mat 4 t Float -> IO ()
uniformMatrix4fv cxt ufm b (Mat v) = js_uniformMatrix4fv cxt ufm b v

getAttribLocation :: WebGLContext -> WebGLProgram -> JSString -> IO (Maybe GLuint)
getAttribLocation cxt prog n = do
    loc <- js_getAttribLocation cxt prog n
    return $ if loc < 0 then Nothing else Just (fromIntegral loc)

