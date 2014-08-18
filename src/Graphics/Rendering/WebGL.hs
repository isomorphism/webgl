{-# LANGUAGE DataKinds #-}
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


cullFace :: (MonadGL m) => CullFaceMode -> m ()
cullFace mode = liftGL $ js_cullFace (toGLEnum mode)



bindBuffer :: (MonadGL m) => BufferTarget -> WebGLBuffer -> m ()
bindBuffer t buf = liftGL $ js_bindBuffer (toGLEnum t) buf

bufferData :: (MonadGL m) => BufferTarget -> TypedArray a -> BufferUsage -> m ()
bufferData t buf use = liftGL $ js_bufferData (toGLEnum t) buf (toGLEnum use)

createBuffer :: (MonadGL m) => m WebGLBuffer
createBuffer = liftGL js_createBuffer

drawElements :: (MonadGL m) => DrawMode -> Int32 -> IndexElementType -> Int -> m ()
drawElements mode cnt ixtype offset = liftGL $ js_drawElements (toGLEnum mode) cnt (toGLEnum ixtype) offset

vertexAttribPointer :: (MonadGL m) => Word32 -> Int32 -> GLDataType -> Bool -> Int32 -> Int -> m ()
vertexAttribPointer ix sz ty normd stride offs = liftGL $ js_vertexAttribPointer ix sz (toGLEnum ty) normd stride offs




activeTexture :: (MonadGL m) => TextureUnit -> m ()
activeTexture tu = liftGL $ js_activeTexture (toGLEnum tu)

bindTexture :: (MonadGL m) => TextureType -> WebGLTexture -> m ()
bindTexture t tx = liftGL $ js_bindTexture (toGLEnum t) tx

unbindTexture :: (MonadGL m) => TextureType -> m ()
unbindTexture t = liftGL $ js_unbindTexture (toGLEnum t)

createTexture :: (MonadGL m) => m WebGLTexture
createTexture = liftGL js_createTexture

pixelStoreFlag :: (MonadGL m) => PixelStoreFlag -> Bool -> m ()
pixelStoreFlag flg b = liftGL $ js_pixelStorei (toGLEnum flg) (if b then 1 else 0)

pixelStoreParam :: (MonadGL m) => PixelStoreParam -> Int32 -> m ()
pixelStoreParam param val = liftGL $ js_pixelStorei (toGLEnum param) val

texImage2D :: (MonadGL m) => TextureTarget -> Int32 -> PixelTypeFormat -> Image -> m ()
texImage2D targ lev tyfm tex = liftGL $ js_texImage2D (toGLEnum targ) lev 
                                                      (getFormatEnum tyfm) (getFormatEnum tyfm) 
                                                      (getTypeEnum tyfm) tex

texWrap :: (MonadGL m) => TextureType -> TexCoordAxis -> TextureWrap -> m ()
texWrap ty ax mode = liftGL $ js_texParameteri (toGLEnum ty) (getWrapParamEnum ax) (toGLEnum mode)

texMinFilter :: (MonadGL m) => TextureType -> TexMinFilter -> m ()
texMinFilter ty mode = liftGL $ js_texParameteri (toGLEnum ty) gl_TEXTURE_MIN_FILTER (toGLEnum mode)

texMagFilter :: (MonadGL m) => TextureType -> TexMagFilter -> m ()
texMagFilter ty mode = liftGL $ js_texParameteri (toGLEnum ty) gl_TEXTURE_MAG_FILTER (toGLEnum mode)


setClearColor :: (MonadGL m) => Tint -> m ()
setClearColor (Tint (Vec v)) = liftGL $ js_clearColor v

getAttribLocation :: (MonadGL m) => WebGLProgram -> JSString -> m (Maybe GLuint)
getAttribLocation prog n = do
    loc <- liftGL $ js_getAttribLocation prog n
    return $ if loc < 0 then Nothing else Just (fromIntegral loc)

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

{-

getContextAttributes :: WebGLContext -> IO WebGLContextAttributes
isContextLost :: IO Bool

getSupportedExtensions :: WebGLContext -> IO [ExtensionName]
getExtension :: WebGLContext -> ExtensionName -> IO (Maybe WebGLExtension)

activeTexture :: WebGLContext -> TextureUnit -> IO ()
attachShader :: WebGLContext -> WebGLProgram -> WebGLShader -> IO()
bindAttribLocation :: WebGLContext -> WebGLProgram -> Int -> JSString -> IO ()
bindBuffer :: WebGLContext -> BufferTarget -> WebGLBuffer -> IO ()
bindFramebuffer :: WebGLContext -> WebGLFramebuffer -> IO () -- enum argument to raw func is always gl_FRAMEBUFFER
bindRenderbuffer :: WebGLContext -> WebGLRenderbuffer -> IO () -- enum argument to raw func is always gl_RENDERBUFFER
bindTexture :: WebGLContext -> TextureType -> WebGLTexture -> IO () -- TODO: make distinct texture types since the texture type 'target' has to match and they can't change type

blendColor :: WebGLContext -> Tint -> IO ()
blendEquation :: WebGLContext -> BlendEquation -> IO ()
blendEquationSeparate :: WebGLContext -> BlendEquation -> BlendEquation  -> IO ()
blendFunc :: WebGLContext -> BlendFunction -> BlendFunction -> IO ()
blendFuncSeparate :: WebGLContext -> BlendFunction -> BlendFunction -> BlendFunction -> BlendFunction -> IO ()

bufferDataNull :: WebGLContext -> BufferTarget -> GLsizeiptr -> BufferUsage -> IO ()
bufferData :: WebGLContext -> BufferTarget -> ArrayBufferView -> BufferUsage -> IO ()
-- void bufferData(GLenum target, ArrayBuffer? data, GLenum usage);
bufferSubData :: WebGLContext -> BufferTarget -> Int -> ArrayBufferView -> IO ()
-- void bufferSubData(GLenum target, GLintptr offset, ArrayBuffer? data);

checkFramebufferStatus :: WebGLContext -> IO FramebufferStatus -- enum is always gl_FRAMEBUFFER

clear :: WebGLContext -> ClearBufferMask -> IO ()
--    void clearColor(GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha);
clearDepth :: WebGLContext -> Float -> IO ()
clearStencil :: WebGLContext -> Int -> IO ()
colorMask :: WebGLContext -> Bool -> Bool -> Bool -> Bool -> IO ()

compileShader :: WebGLContext -> WebGLShader -> IO ()

-- void compressedTexImage2D(GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLint border,ArrayBufferView data);
-- void compressedTexSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, ArrayBufferView data);

copyTexImage2D :: WebGLContext -> TextureTarget -> (GLint level, GLenum internalformat, GLint x, GLint y, GLsizei width, GLsizei height, GLint border);
    void copyTexSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint x, GLint y, GLsizei width, GLsizei height);

    WebGLBuffer? createBuffer();
    WebGLFramebuffer? createFramebuffer();
    WebGLProgram? createProgram();
    WebGLRenderbuffer? createRenderbuffer();
    WebGLShader? createShader(GLenum type);
    WebGLTexture? createTexture();

    void cullFace(GLenum mode);

    void deleteBuffer(WebGLBuffer? buffer);
    void deleteFramebuffer(WebGLFramebuffer? framebuffer);
    void deleteProgram(WebGLProgram? program);
    void deleteRenderbuffer(WebGLRenderbuffer? renderbuffer);
    void deleteShader(WebGLShader? shader);
    void deleteTexture(WebGLTexture? texture);

    void depthFunc(GLenum func);
    void depthMask(GLboolean flag);
    void depthRange(GLclampf zNear, GLclampf zFar);
    void detachShader(WebGLProgram? program, WebGLShader? shader);
    void disable(GLenum cap);
    void disableVertexAttribArray(GLuint index);
    void drawArrays(GLenum mode, GLint first, GLsizei count);
    void drawElements(GLenum mode, GLsizei count, GLenum type, GLintptr offset);

    void enable(GLenum cap);
    void enableVertexAttribArray(GLuint index);
    void finish();
    void flush();
    void framebufferRenderbuffer(GLenum target, GLenum attachment, 
                                 GLenum renderbuffertarget, 
                                 WebGLRenderbuffer? renderbuffer);
    void framebufferTexture2D(GLenum target, GLenum attachment, GLenum textarget, 
                              WebGLTexture? texture, GLint level);
    void frontFace(GLenum mode);

    void generateMipmap(GLenum target);

    WebGLActiveInfo? getActiveAttrib(WebGLProgram? program, GLuint index);
    WebGLActiveInfo? getActiveUniform(WebGLProgram? program, GLuint index);
    sequence<WebGLShader>? getAttachedShaders(WebGLProgram? program);

    [WebGLHandlesContextLoss] GLint getAttribLocation(WebGLProgram? program, DOMString name);

    any getBufferParameter(GLenum target, GLenum pname);
    any getParameter(GLenum pname);

    [WebGLHandlesContextLoss] GLenum getError();

    any getFramebufferAttachmentParameter(GLenum target, GLenum attachment, 
                                          GLenum pname);
    any getProgramParameter(WebGLProgram? program, GLenum pname);
    DOMString? getProgramInfoLog(WebGLProgram? program);
    any getRenderbufferParameter(GLenum target, GLenum pname);
    any getShaderParameter(WebGLShader? shader, GLenum pname);
    WebGLShaderPrecisionFormat? getShaderPrecisionFormat(GLenum shadertype, GLenum precisiontype);
    DOMString? getShaderInfoLog(WebGLShader? shader);

    DOMString? getShaderSource(WebGLShader? shader);

    any getTexParameter(GLenum target, GLenum pname);

    any getUniform(WebGLProgram? program, WebGLUniformLocation? location);

    WebGLUniformLocation? getUniformLocation(WebGLProgram? program, DOMString name);

    any getVertexAttrib(GLuint index, GLenum pname);

    [WebGLHandlesContextLoss] GLsizeiptr getVertexAttribOffset(GLuint index, GLenum pname);

    void hint(GLenum target, GLenum mode);
    [WebGLHandlesContextLoss] GLboolean isBuffer(WebGLBuffer? buffer);
    [WebGLHandlesContextLoss] GLboolean isEnabled(GLenum cap);
    [WebGLHandlesContextLoss] GLboolean isFramebuffer(WebGLFramebuffer? framebuffer);
    [WebGLHandlesContextLoss] GLboolean isProgram(WebGLProgram? program);
    [WebGLHandlesContextLoss] GLboolean isRenderbuffer(WebGLRenderbuffer? renderbuffer);
    [WebGLHandlesContextLoss] GLboolean isShader(WebGLShader? shader);
    [WebGLHandlesContextLoss] GLboolean isTexture(WebGLTexture? texture);
    void lineWidth(GLfloat width);
    void linkProgram(WebGLProgram? program);
    void pixelStorei(GLenum pname, GLint param);
    void polygonOffset(GLfloat factor, GLfloat units);

    void readPixels(GLint x, GLint y, GLsizei width, GLsizei height, 
                    GLenum format, GLenum type, ArrayBufferView? pixels);

    void renderbufferStorage(GLenum target, GLenum internalformat, 
                             GLsizei width, GLsizei height);
    void sampleCoverage(GLclampf value, GLboolean invert);
    void scissor(GLint x, GLint y, GLsizei width, GLsizei height);

    void shaderSource(WebGLShader? shader, DOMString source);

    void stencilFunc(GLenum func, GLint ref, GLuint mask);
    void stencilFuncSeparate(GLenum face, GLenum func, GLint ref, GLuint mask);
    void stencilMask(GLuint mask);
    void stencilMaskSeparate(GLenum face, GLuint mask);
    void stencilOp(GLenum fail, GLenum zfail, GLenum zpass);
    void stencilOpSeparate(GLenum face, GLenum fail, GLenum zfail, GLenum zpass);

    void texImage2D(GLenum target, GLint level, GLenum internalformat, 
                    GLsizei width, GLsizei height, GLint border, GLenum format, 
                    GLenum type, ArrayBufferView? pixels);
    void texImage2D(GLenum target, GLint level, GLenum internalformat,
                    GLenum format, GLenum type, ImageData? pixels);
    void texImage2D(GLenum target, GLint level, GLenum internalformat,
                    GLenum format, GLenum type, HTMLImageElement image); // May throw DOMException
    void texImage2D(GLenum target, GLint level, GLenum internalformat,
                    GLenum format, GLenum type, HTMLCanvasElement canvas); // May throw DOMException
    void texImage2D(GLenum target, GLint level, GLenum internalformat,
                    GLenum format, GLenum type, HTMLVideoElement video); // May throw DOMException

    void texParameterf(GLenum target, GLenum pname, GLfloat param);
    void texParameteri(GLenum target, GLenum pname, GLint param);

    void texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, 
                       GLsizei width, GLsizei height, 
                       GLenum format, GLenum type, ArrayBufferView? pixels);
    void texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, 
                       GLenum format, GLenum type, ImageData? pixels);
    void texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, 
                       GLenum format, GLenum type, HTMLImageElement image); // May throw DOMException
    void texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, 
                       GLenum format, GLenum type, HTMLCanvasElement canvas); // May throw DOMException
    void texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, 
                       GLenum format, GLenum type, HTMLVideoElement video); // May throw DOMException

    void useProgram(WebGLProgram? program);
    void validateProgram(WebGLProgram? program);

    void vertexAttrib1f(GLuint indx, GLfloat x);
    void vertexAttrib1fv(GLuint indx, Float32Array values);
    void vertexAttrib1fv(GLuint indx, sequence<GLfloat> values);
    void vertexAttrib2f(GLuint indx, GLfloat x, GLfloat y);
    void vertexAttrib2fv(GLuint indx, Float32Array values);
    void vertexAttrib2fv(GLuint indx, sequence<GLfloat> values);
    void vertexAttrib3f(GLuint indx, GLfloat x, GLfloat y, GLfloat z);
    void vertexAttrib3fv(GLuint indx, Float32Array values);
    void vertexAttrib3fv(GLuint indx, sequence<GLfloat> values);
    void vertexAttrib4f(GLuint indx, GLfloat x, GLfloat y, GLfloat z, GLfloat w);
    void vertexAttrib4fv(GLuint indx, Float32Array values);
    void vertexAttrib4fv(GLuint indx, sequence<GLfloat> values);
    void vertexAttribPointer(GLuint indx, GLint size, GLenum type, 
                             GLboolean normalized, GLsizei stride, GLintptr offset);

    void viewport(GLint x, GLint y, GLsizei width, GLsizei height);

-}