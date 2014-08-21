{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
module Graphics.Rendering.WebGL (
    module GHCJS.DOM.HTMLCanvasElement,
    module Graphics.Rendering.WebGL.Matrix,
    module Graphics.Rendering.WebGL,
    module Graphics.Rendering.WebGL.EnumConvert,
    module Graphics.Rendering.WebGL.Types,
    module Graphics.Rendering.WebGL.Misc
    ) where

import Data.Word
import Data.Int

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
import Graphics.Rendering.WebGL.Misc
import Graphics.Rendering.WebGL.Internal
import Graphics.Rendering.WebGL.Constants

import GHCJS.DOM.HTMLCanvasElement


-- TODO: 
--   * rename some types to drop the WebGL prefix
--   * data structure specifying uniforms and vertex attribs
--     * use these to automatically call the set-up functions
--     * use datakind promotion to do type-level tracking as well
--   * fix js_scissor type



instance GLObject WebGLBuffer where
    create = createBuffer
    delete = deleteBuffer
    isValid = isBuffer

instance GLObject WebGLFramebuffer where
    create = createFramebuffer
    delete = deleteFramebuffer
    isValid = isFramebuffer

instance GLObject WebGLRenderbuffer where
    create = createRenderbuffer
    delete = deleteRenderbuffer
    isValid = isRenderbuffer

instance GLObject WebGLProgram where
    create = createProgram
    delete = deleteProgram
    isValid = isProgram

instance GLObject VertexShader where
    create = createVertexShader
    delete = deleteShader
    isValid = isShader

instance GLObject FragmentShader where
    create = createFragmentShader
    delete = deleteShader
    isValid = isShader

instance GLObject WebGLTexture where
    create = createTexture
    delete = deleteTexture
    isValid = isTexture


instance Uniform Float where uniform = uniform1f
instance Uniform (Vec 2 t Float) where uniform = uniform2fv
instance Uniform (Vec 3 t Float) where uniform = uniform3fv
instance Uniform (Vec 4 t Float) where uniform = uniform4fv
instance Uniform (Mat 2 t Float) where uniform u = uniformMatrix2fv u False
instance Uniform (Mat 3 t Float) where uniform u = uniformMatrix3fv u False
instance Uniform (Mat 4 t Float) where uniform u = uniformMatrix4fv u False
instance Uniform Int32 where uniform = uniform1i
instance Uniform (Vec 2 t Int32) where uniform = uniform2iv
instance Uniform (Vec 3 t Int32) where uniform = uniform3iv
instance Uniform (Vec 4 t Int32) where uniform = uniform4iv



-- ---------------------------------------------------
-- get/set GL state


activeTexture :: (MonadGL m) => TextureUnit -> m ()
activeTexture tu = liftGL $ js_activeTexture (toGLEnum tu)

blendColor :: (MonadGL m) => Tint -> m ()
blendColor (Tint (Vec v)) = liftGL $ js_blendColor v 

blendEquation :: (MonadGL m) => BlendEquation -> m ()
blendEquation mode = liftGL $ js_blendEquation (toGLEnum mode)

blendEquationSeparate :: (MonadGL m) => BlendEquation -> BlendEquation -> m ()
blendEquationSeparate modeRGB modeA = liftGL $ js_blendEquationSeparate (toGLEnum modeRGB) (toGLEnum modeA)

blendFunc :: (MonadGL m) => BlendFunc BlendSrc -> BlendFunc BlendDst -> m ()
blendFunc src dst = liftGL $ js_blendFunc (toGLEnum src) (toGLEnum dst)

blendFuncSeparate :: (MonadGL m) => BlendFunc BlendSrc -> BlendFunc BlendDst -> BlendFunc BlendSrc -> BlendFunc BlendDst -> m ()
blendFuncSeparate srcRGB dstRGB srcA dstA = liftGL $ 
    js_blendFuncSeparate (toGLEnum srcRGB) (toGLEnum dstRGB) (toGLEnum srcA) (toGLEnum dstA)

clearColor :: (MonadGL m) => Tint -> m ()
clearColor (Tint (Vec v)) = liftGL $ js_clearColor v

clearDepth :: (MonadGL m) => Float -> m ()
clearDepth = liftGL1 js_clearDepth

clearStencil :: (MonadGL m) => Int32 -> m ()
clearStencil = liftGL1 js_clearStencil

colorMask :: (MonadGL m) => Bool -> Bool -> Bool -> Bool -> m ()
colorMask = liftGL4 js_colorMask

cullFace :: (MonadGL m) => CullFaceMode -> m ()
cullFace mode = liftGL $ js_cullFace (toGLEnum mode)

depthFunc :: (MonadGL m) => CompareFunc -> m ()
depthFunc func = liftGL $ js_depthFunc (toGLEnum func)

depthMask :: (MonadGL m) => Bool -> m ()
depthMask = liftGL1 js_depthMask

depthRange :: (MonadGL m) => Float -> Float -> m ()
depthRange = liftGL2 js_depthRange

disable :: (MonadGL m) => Capability -> m ()
disable cap = liftGL $ js_disable (toGLEnum cap)

enable :: (MonadGL m) => Capability -> m ()
enable cap = liftGL $ js_enable (toGLEnum cap)

frontFace :: (MonadGL m) => FrontFaceDir -> m ()
frontFace dir = liftGL $ js_frontFace (toGLEnum dir)

-- getParameter
-- getError

hint :: (MonadGL m) => HintTarget -> Maybe Hint -> m ()
hint target mode = liftGL $ js_hint (toGLEnum target) (toGLEnum mode)

isEnabled :: (MonadGL m) => Capability -> m Bool
isEnabled cap = liftGL $ js_isEnabled (toGLEnum cap)

lineWidth :: (MonadGL m) => Float -> m ()
lineWidth = liftGL1 js_lineWidth

pixelStoreFlag :: (MonadGL m) => PixelStoreFlag -> Bool -> m ()
pixelStoreFlag flg b = liftGL $ js_pixelStorei (toGLEnum flg) (if b then 1 else 0)

pixelStoreParam :: (MonadGL m) => PixelStoreParam -> Int32 -> m ()
pixelStoreParam param val = liftGL $ js_pixelStorei (toGLEnum param) val

polygonOffset :: (MonadGL m) => Float -> Float -> m ()
polygonOffset = liftGL2 js_polygonOffset


-- sampleCoverage
-- stencilFunc
-- stencilFuncSeparate
-- stencilMask
-- stencilMaskSeparate
-- stencilOp
-- stencilOpSeparate



-- ---------------------------------------------------
-- viewing and clipping

scissor :: (MonadGL m) => Int32 -> Int32 -> Int32 -> Int32 -> m ()
scissor = liftGL4 js_scissor

viewport :: (MonadGL m) => Int32 -> Int32 -> Int32 -> Int32 -> m ()
viewport = liftGL4 js_setViewport



-- ---------------------------------------------------
-- buffer objects

bindBuffer :: (MonadGL m) => BufferTarget -> WebGLBuffer -> m ()
bindBuffer t buf = liftGL $ js_bindBuffer (toGLEnum t) buf

-- TODO? : other bufferData variants
bufferData :: (MonadGL m) => BufferTarget -> TypedArray a -> BufferUsage -> m ()
bufferData t buf use = liftGL $ js_bufferData (toGLEnum t) buf (toGLEnum use)

createBuffer :: (MonadGL m) => m WebGLBuffer
createBuffer = liftGL js_createBuffer

deleteBuffer :: (MonadGL m) => WebGLBuffer -> m ()
deleteBuffer = liftGL1 js_deleteBuffer

-- getBufferParameter

isBuffer :: (MonadGL m) => WebGLBuffer -> m Bool
isBuffer = liftGL1 js_isBuffer



-- ---------------------------------------------------
-- framebuffer objects

bindFramebuffer :: (MonadGL m) => WebGLFramebuffer -> m ()
bindFramebuffer = liftGL1 $ js_bindFramebuffer gl_FRAMEBUFFER

checkFramebufferStatus :: (MonadGL m) => m (Maybe FramebufferStatus)
checkFramebufferStatus = liftM fromGLEnum . liftGL $ js_checkFramebufferStatus gl_FRAMEBUFFER

createFramebuffer :: (MonadGL m) => m WebGLFramebuffer
createFramebuffer = liftGL js_createFramebuffer

deleteFramebuffer :: (MonadGL m) => WebGLFramebuffer -> m ()
deleteFramebuffer = liftGL1 js_deleteFramebuffer

-- framebufferRenderbuffer
-- framebufferTexture2D
-- getFramebufferAttachmentParameter

isFramebuffer :: (MonadGL m) => WebGLFramebuffer -> m Bool
isFramebuffer = liftGL1 js_isFramebuffer


-- ---------------------------------------------------
-- renderbuffer objects

bindRenderbuffer :: (MonadGL m) => WebGLRenderbuffer -> m ()
bindRenderbuffer = liftGL1 $ js_bindRenderbuffer gl_RENDERBUFFER


createRenderbuffer :: (MonadGL m) => m WebGLRenderbuffer
createRenderbuffer = liftGL js_createRenderbuffer

deleteRenderbuffer :: (MonadGL m) => WebGLRenderbuffer -> m ()
deleteRenderbuffer = liftGL1 js_deleteRenderbuffer

isRenderbuffer :: (MonadGL m) => WebGLRenderbuffer -> m Bool
isRenderbuffer = liftGL1 js_isRenderbuffer

-- renderbufferStorage



-- ---------------------------------------------------
-- texture objects

bindTexture :: (MonadGL m) => TextureType TargetTexture -> WebGLTexture -> m ()
bindTexture t tx = liftGL $ js_bindTexture (toGLEnum t) tx

unbindTexture :: (MonadGL m) => TextureType TargetTexture -> m ()
unbindTexture t = liftGL $ js_unbindTexture (toGLEnum t)

-- not supported in default WebGL apparently
-- compressedTexImage2D
-- compressedTexSubImage2D

copyTexImage2D :: (MonadGL m) => TextureType TargetImage -> Int32 -> PixelTypeFormat -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> m ()
copyTexImage2D targ lev tyfm x y w h bord = liftGL $ 
    js_copyTexImage2D (toGLEnum targ) lev (getFormatEnum tyfm) x y w h bord

copyTexSubImage2D :: (MonadGL m) => TextureType TargetImage -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> m ()
copyTexSubImage2D targ lev xoff yoff x y w h = liftGL $ 
    js_copyTexSubImage2D (toGLEnum targ) lev xoff yoff x y w h


createTexture :: (MonadGL m) => m WebGLTexture
createTexture = liftGL js_createTexture

deleteTexture :: (MonadGL m) => WebGLTexture -> m ()
deleteTexture = liftGL1 js_deleteTexture

generateMipmap :: (MonadGL m) => TextureType TargetTexture -> m ()
generateMipmap target = liftGL $ js_generateMipmap (toGLEnum target)

-- getTexParameter

isTexture :: (MonadGL m) => WebGLTexture -> m Bool
isTexture = liftGL1 js_isTexture

-- TODO? : other texImage2D variants
texImage2D :: (MonadGL m) => TextureType TargetImage -> Int32 -> PixelTypeFormat -> Image -> m ()
texImage2D targ lev tyfm tex = liftGL $ 
    js_texImage2D (toGLEnum targ) lev (getFormatEnum tyfm) 
                  (getFormatEnum tyfm) (getTypeEnum tyfm) tex

texWrap :: (MonadGL m) => TextureType TargetTexture -> TexCoordAxis -> TextureWrap -> m ()
texWrap ty ax mode = liftGL $ js_texParameteri (toGLEnum ty) (getWrapParamEnum ax) (toGLEnum mode)

texMinFilter :: (MonadGL m) => TextureType TargetTexture -> TexMinFilter -> m ()
texMinFilter ty mode = liftGL $ js_texParameteri (toGLEnum ty) gl_TEXTURE_MIN_FILTER (toGLEnum mode)

texMagFilter :: (MonadGL m) => TextureType TargetTexture -> TexMagFilter -> m ()
texMagFilter ty mode = liftGL $ js_texParameteri (toGLEnum ty) gl_TEXTURE_MAG_FILTER (toGLEnum mode)

texSubImage2D :: (MonadGL m) => TextureType TargetImage -> Int32 -> Int32 -> Int32 -> PixelTypeFormat -> Image -> m ()
texSubImage2D targ lev x y tyfm tex = liftGL $ 
    js_texSubImage2D (toGLEnum targ) lev x y (getFormatEnum tyfm) (getTypeEnum tyfm) tex



-- ---------------------------------------------------
-- programs and shaders

attachShader :: (MonadGL m) => WebGLProgram -> WebGLShader a -> m ()
attachShader = liftGL2 js_attachShader

-- bindAttribLocation

compileShader :: (MonadGL m) => WebGLShader a -> m ()
compileShader = liftGL1 js_compileShader

createProgram :: (MonadGL m) => m WebGLProgram
createProgram = liftGL js_createProgram

createFragmentShader :: (MonadGL m) => m FragmentShader
createFragmentShader = liftGL $ js_createShader gl_FRAGMENT_SHADER

makeFragmentShader src = do
    shd <- createFragmentShader
    shaderSource shd src
    compileShader shd
    -- TODO: error checks
    return shd

createVertexShader :: (MonadGL m) => m VertexShader
createVertexShader = liftGL $ js_createShader gl_VERTEX_SHADER

makeVertexShader src = do
    shd <- createVertexShader
    shaderSource shd src
    compileShader shd
    -- TODO: error checks
    return shd


deleteProgram :: (MonadGL m) => WebGLProgram -> m ()
deleteProgram = liftGL1 js_deleteProgram

deleteShader :: (MonadGL m) => WebGLShader t -> m ()
deleteShader = liftGL1 js_deleteShader

detachShader :: (MonadGL m) => WebGLProgram -> WebGLShader a -> m ()
detachShader = liftGL2 js_detachShader

-- getAttachedShaders
-- getProgramParameter
-- getProgramInfoLog
-- getShaderParameter
-- getShaderPrecisionFormat
-- getShaderInfoLog
-- getShaderSource

isProgram :: (MonadGL m) => WebGLProgram -> m Bool
isProgram = liftGL1 js_isProgram

isShader :: (MonadGL m) => WebGLShader t -> m Bool
isShader = liftGL1 js_isShader

linkProgram :: (MonadGL m) => WebGLProgram -> m ()
linkProgram = liftGL1 js_linkProgram

shaderSource :: (MonadGL m) => WebGLShader a -> JSString -> m ()
shaderSource = liftGL2 js_shaderSource

useProgram :: (MonadGL m) => WebGLProgram -> m ()
useProgram = liftGL1 js_useProgram

validateProgram :: (MonadGL m) => WebGLProgram -> m ()
validateProgram = liftGL1 js_validateProgram




-- ---------------------------------------------------
-- uniforms and attributes

-- disableVertexAttribArray

enableVertexAttribArray :: (MonadGL m) => Word32 -> m ()
enableVertexAttribArray = liftGL1 js_enableVertexAttribArray

-- getActiveAttrib
-- getActiveUniform

getAttribLocation :: (MonadGL m) => WebGLProgram -> JSString -> m (Maybe GLuint)
getAttribLocation prog n = do
    loc <- liftGL $ js_getAttribLocation prog n
    return $ if loc < 0 then Nothing else Just (fromIntegral loc)

-- getUniform 

getUniformLocation :: (MonadGL m) => WebGLProgram -> JSString -> m WebGLUniformLocation
getUniformLocation = liftGL2 js_getUniformLocation 

-- getVertexAttrib
-- getVertexAttribOffset


uniform1i :: (MonadGL m) => WebGLUniformLocation -> Int32 -> m ()
uniform1i = liftGL2 js_uniform1i

uniform1f :: (MonadGL m) => WebGLUniformLocation -> Float -> m ()
uniform1f = liftGL2 js_uniform1f

uniform2i :: (MonadGL m) => WebGLUniformLocation -> Int32 -> Int32 -> m ()
uniform2i = liftGL3 js_uniform2i

uniform2f :: (MonadGL m) => WebGLUniformLocation -> Float -> Float -> m ()
uniform2f = liftGL3 js_uniform2f

uniform3i :: (MonadGL m) => WebGLUniformLocation -> Int32 -> Int32 -> Int32 -> m ()
uniform3i = liftGL4 js_uniform3i

uniform3f :: (MonadGL m) => WebGLUniformLocation -> Float -> Float -> Float -> m ()
uniform3f = liftGL4 js_uniform3f

uniform4i :: (MonadGL m) => WebGLUniformLocation -> Int32 -> Int32 -> Int32 -> Int32 -> m ()
uniform4i = liftGL5 js_uniform4i

uniform4f :: (MonadGL m) => WebGLUniformLocation -> Float -> Float -> Float -> Float -> m ()
uniform4f = liftGL5 js_uniform4f

uniform1iv :: (MonadGL m) => WebGLUniformLocation -> Vec 1 t Int32 -> m ()
uniform1iv ufm (Vec v) = liftGL $ js_uniform1iv ufm v

uniform1fv :: (MonadGL m) => WebGLUniformLocation -> Vec 1 t Float -> m ()
uniform1fv ufm (Vec v) = liftGL $ js_uniform1fv ufm v

uniform2iv :: (MonadGL m) => WebGLUniformLocation -> Vec 2 t Int32 -> m ()
uniform2iv ufm (Vec v) = liftGL $ js_uniform2iv ufm v

uniform2fv :: (MonadGL m) => WebGLUniformLocation -> Vec 2 t Float -> m ()
uniform2fv ufm (Vec v) = liftGL $ js_uniform2fv ufm v

uniform3iv :: (MonadGL m) => WebGLUniformLocation -> Vec 3 t Int32 -> m ()
uniform3iv ufm (Vec v) = liftGL $ js_uniform3iv ufm v

uniform3fv :: (MonadGL m) => WebGLUniformLocation -> Vec 3 t Float -> m ()
uniform3fv ufm (Vec v) = liftGL $ js_uniform3fv ufm v

uniform4iv :: (MonadGL m) => WebGLUniformLocation -> Vec 4 t Int32 -> m ()
uniform4iv ufm (Vec v) = liftGL $ js_uniform4iv ufm v

uniform4fv :: (MonadGL m) => WebGLUniformLocation -> Vec 4 t Float -> m ()
uniform4fv ufm (Vec v) = liftGL $ js_uniform4fv ufm v

uniformMatrix2fv :: (MonadGL m) => WebGLUniformLocation -> Bool -> Mat 2 t Float -> m ()
uniformMatrix2fv ufm b (Mat v) = liftGL $ js_uniformMatrix2fv ufm b v

uniformMatrix3fv :: (MonadGL m) => WebGLUniformLocation -> Bool -> Mat 3 t Float -> m ()
uniformMatrix3fv ufm b (Mat v) = liftGL $ js_uniformMatrix3fv ufm b v

uniformMatrix4fv :: (MonadGL m) => WebGLUniformLocation -> Bool -> Mat 4 t Float -> m ()
uniformMatrix4fv ufm b (Mat v) = liftGL $ js_uniformMatrix4fv ufm b v

-- vertexAttrib[1234]f
-- vertexAttrib[1234]fv

vertexAttribPointer :: (MonadGL m) => Word32 -> Int32 -> GLDataType -> Bool -> Int32 -> Int -> m ()
vertexAttribPointer ix sz ty normd stride offs = liftGL $ js_vertexAttribPointer ix sz (toGLEnum ty) normd stride offs


-- ---------------------------------------------------
-- writing to the drawing buffer

clear :: (MonadGL m) => ClearBufferMask -> m ()
clear mask = liftGL $ js_clear (toGLEnum mask)

drawArrays :: (MonadGL m) => DrawMode -> Int32 -> Int32 -> m ()
drawArrays mode startIx cnt = liftGL $ js_drawArrays (toGLEnum mode) startIx cnt

drawElements :: (MonadGL m) => DrawMode -> Int32 -> IndexElementType -> Int -> m ()
drawElements mode cnt ixtype offset = liftGL $ js_drawElements (toGLEnum mode) cnt (toGLEnum ixtype) offset

finish :: (MonadGL m) => m ()
finish = liftGL js_finish

flush :: (MonadGL m) => m ()
flush = liftGL js_flush



-- ---------------------------------------------------
-- reading back pixels

-- readPixels


-- ---------------------------------------------------
-- detecting context loss

-- isContextLost


-- ---------------------------------------------------
-- detecting and enabling extensions

-- getSupportedExtensions
-- getExtension

