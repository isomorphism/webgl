{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module Graphics.Rendering.WebGL.Types where

import GHCJS.Types

import Control.Monad.IO.Class
import Foreign.C.Types
import Data.Int
import Data.Word
import GHCJS.DOM.Types
import GHCJS.TypedArray
import Graphics.Rendering.WebGL.Matrix


newtype Color = Color { getColor :: Vec 3 Imm Float }

newtype Tint = Tint { getTint :: Vec 4 Imm Float }

class (MonadIO m, Monad m) => MonadGL m where
    getContext :: m WebGLContext


class GLObject a where
    create :: (MonadGL m) => m a
    delete :: (MonadGL m) => a -> m ()
    isValid :: (MonadGL m) => a -> m Bool

class Uniform a where 
    uniform :: (MonadGL m) => WebGLUniformLocation -> a -> m ()


-- translating from Web IDL specification:
-- byte = 8 bits
-- short = 16 bits
-- long = 32 bits
-- long long = 64 bits

type GLenum     = Word32
type GLboolean  = Bool  -- this is a JS boolean, unlike GLboolean in OpenGLRaw
type GLbitfield = Word32

type GLbyte   = Int8
type GLshort  = Int16
type GLint    = Int32

type GLsizei  = Int32

type GLintptr   = Int -- the docs say this should be 64-bit but...?
type GLsizeiptr = Int -- the docs say this should be 64-bit but...?

type GLubyte  = Word8
type GLushort = Word16
type GLuint   = Word32

type GLfloat  = Float
type GLclampf = Float

data Image'
type Image = JSRef Image'

instance GObjectClass (JSRef Image') where
    toGObject = GObject . castRef
    unsafeCastGObject (GObject o) = castRef o


data XmlHttpRequest'
type XmlHttpRequest = JSRef XmlHttpRequest'

instance GObjectClass (JSRef XmlHttpRequest') where
    toGObject = GObject . castRef
    unsafeCastGObject (GObject o) = castRef o



type ExtensionName = JSString

data WebGLShaderPrecisionFormat'
type WebGLShaderPrecisionFormat = JSRef WebGLShaderPrecisionFormat'

data WebGLActiveInfo'
type WebGLActiveInfo = JSRef WebGLActiveInfo'



data WebGLExtension'
type WebGLExtension = JSRef WebGLExtension'

data WebGLContext'
type WebGLContext = JSRef WebGLContext'

data WebGLContextAttributes'
type WebGLContextAttributes = JSRef WebGLContextAttributes'

data FragmentShader'
data VertexShader'

data WebGLShader' a
type WebGLShader a = JSRef (WebGLShader' a)

type FragmentShader = WebGLShader FragmentShader'
type VertexShader = WebGLShader VertexShader'

data WebGLProgram'
type WebGLProgram = JSRef WebGLProgram'

data WebGLUniformLocation'
type WebGLUniformLocation = JSRef WebGLUniformLocation'

data WebGLUniformValue'
type WebGLUniformValue = JSRef WebGLUniformValue'


data WebGLBuffer'
type WebGLBuffer = JSRef WebGLBuffer'

data WebGLRenderbuffer'
type WebGLRenderbuffer = JSRef WebGLRenderbuffer'

data WebGLFramebuffer'
type WebGLFramebuffer = JSRef WebGLFramebuffer'


data WebGLTexture'
type WebGLTexture = JSRef WebGLTexture'




