module Graphics.Rendering.WebGL.Misc where

import GHCJS.Types
import Data.Word
import Data.Int

import GHCJS.TypedArray (TypedArray, ArrayBufferView)
import Graphics.Rendering.WebGL.Types

-- supposedly WebGLShaderPrecisionFormat's fields are all read-only so I think this can be pure
foreign import javascript unsafe "$1['rangeMin']" js_shaderPrecisionFormat_rangeMin :: WebGLShaderPrecisionFormat -> GLint
foreign import javascript unsafe "$1['rangeMax']" js_shaderPrecisionFormat_rangeMax :: WebGLShaderPrecisionFormat -> GLint
foreign import javascript unsafe "$1['precision']" js_shaderPrecisionFormat_precision :: WebGLShaderPrecisionFormat -> GLint
