{-# LANGUAGE FlexibleInstances #-}
module Graphics.Rendering.WebGL.Types (
    JSRef,
    
    GLenum, GLboolean, GLbitfield, 
    GLbyte, GLshort, GLint, GLsizei,
    GLintptr, GLsizeiptr, 
    GLubyte, GLushort, GLuint, 
    GLfloat, GLclampf,
    
    Image', Image,
    XmlHttpRequest', XmlHttpRequest,
    
    WebGLContext', WebGLContext,
    WebGLBuffer', WebGLBuffer,
    WebGLTexture', WebGLTexture,
    WebGLContextAttributes', WebGLContextAttributes,
    WebGLShader', WebGLShader,
    WebGLProgram', WebGLProgram,
    WebGLUniformLocation', WebGLUniformLocation,
    WebGLUniformValue', WebGLUniformValue
    ) where

import GHCJS.Types

import Foreign.C.Types
import Data.Int
import Data.Word
import GHCJS.DOM.Types

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




data WebGLContextAttributes'
type WebGLContextAttributes = JSRef WebGLContextAttributes'

data WebGLContext'
type WebGLContext = JSRef WebGLContext'

data WebGLShader'
type WebGLShader = JSRef WebGLShader'

data WebGLProgram'
type WebGLProgram = JSRef WebGLProgram'

data WebGLBuffer'
type WebGLBuffer = JSRef WebGLBuffer'

data WebGLTexture'
type WebGLTexture = JSRef WebGLTexture'

data WebGLUniformLocation'
type WebGLUniformLocation = JSRef WebGLUniformLocation'

data WebGLUniformValue'
type WebGLUniformValue = JSRef WebGLUniformValue'

