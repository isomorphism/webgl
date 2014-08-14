{-# LANGUAGE DataKinds #-}
module Graphics.Rendering.WebGL (
    module GHCJS.DOM.HTMLCanvasElement,
    module Graphics.Rendering.WebGL.Matrix,
    module Graphics.Rendering.WebGL,
    module Graphics.Rendering.WebGL.EnumConvert,
    module Graphics.Rendering.WebGL.Types
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

import GHCJS.DOM.HTMLCanvasElement

setClearColor :: (MonadGL m) => Tint -> m ()
setClearColor (Tint (Vec v)) = do
    cxt <- getContext
    liftIO $ js_clearColor cxt v

getAttribLocation :: (MonadGL m) => WebGLProgram -> JSString -> m (Maybe GLuint)
getAttribLocation prog n = do
    cxt <- getContext
    loc <- liftIO $ js_getAttribLocation cxt prog n
    return $ if loc < 0 then Nothing else Just (fromIntegral loc)



uniform1i :: (MonadGL m) => WebGLUniformLocation -> Int32 -> m ()
uniform1i ufm x1 = do
    cxt <- getContext
    liftIO $ js_uniform1i cxt ufm x1

uniform1f :: (MonadGL m) => WebGLUniformLocation -> Float -> m ()
uniform1f ufm x1 = do
    cxt <- getContext
    liftIO $ js_uniform1f cxt ufm x1

uniform2i :: (MonadGL m) => WebGLUniformLocation -> Int32 -> Int32 -> m ()
uniform2i ufm x1 x2 = do
    cxt <- getContext
    liftIO $ js_uniform2i cxt ufm x1 x2

uniform2f :: (MonadGL m) => WebGLUniformLocation -> Float -> Float -> m ()
uniform2f ufm x1 x2 = do
    cxt <- getContext
    liftIO $ js_uniform2f cxt ufm x1 x2

uniform3i :: (MonadGL m) => WebGLUniformLocation -> Int32 -> Int32 -> Int32 -> m ()
uniform3i ufm x1 x2 x3 = do
    cxt <- getContext
    liftIO $ js_uniform3i cxt ufm x1 x2 x3

uniform3f :: (MonadGL m) => WebGLUniformLocation -> Float -> Float -> Float -> m ()
uniform3f ufm x1 x2 x3 = do
    cxt <- getContext
    liftIO $ js_uniform3f cxt ufm x1 x2 x3

uniform4i :: (MonadGL m) => WebGLUniformLocation -> Int32 -> Int32 -> Int32 -> Int32 -> m ()
uniform4i ufm x1 x2 x3 x4 = do
    cxt <- getContext
    liftIO $ js_uniform4i cxt ufm x1 x2 x3 x4

uniform4f :: (MonadGL m) => WebGLUniformLocation -> Float -> Float -> Float -> Float -> m ()
uniform4f ufm x1 x2 x3 x4 = do
    cxt <- getContext
    liftIO $ js_uniform4f cxt ufm x1 x2 x3 x4

uniform1iv :: (MonadGL m) => WebGLUniformLocation -> Vec 1 t Int32 -> m ()
uniform1iv ufm (Vec v) = do
    cxt <- getContext
    liftIO $ js_uniform1iv cxt ufm v

uniform1fv :: (MonadGL m) => WebGLUniformLocation -> Vec 1 t Float -> m ()
uniform1fv ufm (Vec v) = do
    cxt <- getContext
    liftIO $ js_uniform1fv cxt ufm v

uniform2iv :: (MonadGL m) => WebGLUniformLocation -> Vec 2 t Int32 -> m ()
uniform2iv ufm (Vec v) = do
    cxt <- getContext
    liftIO $ js_uniform2iv cxt ufm v

uniform2fv :: (MonadGL m) => WebGLUniformLocation -> Vec 2 t Float -> m ()
uniform2fv ufm (Vec v) = do
    cxt <- getContext
    liftIO $ js_uniform2fv cxt ufm v

uniform3iv :: (MonadGL m) => WebGLUniformLocation -> Vec 3 t Int32 -> m ()
uniform3iv ufm (Vec v) = do
    cxt <- getContext
    liftIO $ js_uniform3iv cxt ufm v

uniform3fv :: (MonadGL m) => WebGLUniformLocation -> Vec 3 t Float -> m ()
uniform3fv ufm (Vec v) = do
    cxt <- getContext
    liftIO $ js_uniform3fv cxt ufm v

uniform4iv :: (MonadGL m) => WebGLUniformLocation -> Vec 4 t Int32 -> m ()
uniform4iv ufm (Vec v) = do
    cxt <- getContext
    liftIO $ js_uniform4iv cxt ufm v

uniform4fv :: (MonadGL m) => WebGLUniformLocation -> Vec 4 t Float -> m ()
uniform4fv ufm (Vec v) = do
    cxt <- getContext
    liftIO $ js_uniform4fv cxt ufm v


uniformMatrix2fv :: (MonadGL m) => WebGLUniformLocation -> Bool -> Mat 2 t Float -> m ()
uniformMatrix2fv ufm b (Mat v) = do
    cxt <- getContext
    liftIO $ js_uniformMatrix2fv cxt ufm b v

uniformMatrix3fv :: (MonadGL m) => WebGLUniformLocation -> Bool -> Mat 3 t Float -> m ()
uniformMatrix3fv ufm b (Mat v) = do
    cxt <- getContext
    liftIO $ js_uniformMatrix3fv cxt ufm b v

uniformMatrix4fv :: (MonadGL m) => WebGLUniformLocation -> Bool -> Mat 4 t Float -> m ()
uniformMatrix4fv ufm b (Mat v) = do
    cxt <- getContext
    liftIO $ js_uniformMatrix4fv cxt ufm b v




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