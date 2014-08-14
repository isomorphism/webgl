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
setClearColor cxt (Vec v) = js_clearColor cxt v

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

    void uniform1f(WebGLUniformLocation? location, GLfloat x);
    void uniform1fv(WebGLUniformLocation? location, Float32Array v);
    void uniform1fv(WebGLUniformLocation? location, sequence<GLfloat> v);
    void uniform1i(WebGLUniformLocation? location, GLint x);
    void uniform1iv(WebGLUniformLocation? location, Int32Array v);
    void uniform1iv(WebGLUniformLocation? location, sequence<long> v);
    void uniform2f(WebGLUniformLocation? location, GLfloat x, GLfloat y);
    void uniform2fv(WebGLUniformLocation? location, Float32Array v);
    void uniform2fv(WebGLUniformLocation? location, sequence<GLfloat> v);
    void uniform2i(WebGLUniformLocation? location, GLint x, GLint y);
    void uniform2iv(WebGLUniformLocation? location, Int32Array v);
    void uniform2iv(WebGLUniformLocation? location, sequence<long> v);
    void uniform3f(WebGLUniformLocation? location, GLfloat x, GLfloat y, GLfloat z);
    void uniform3fv(WebGLUniformLocation? location, Float32Array v);
    void uniform3fv(WebGLUniformLocation? location, sequence<GLfloat> v);
    void uniform3i(WebGLUniformLocation? location, GLint x, GLint y, GLint z);
    void uniform3iv(WebGLUniformLocation? location, Int32Array v);
    void uniform3iv(WebGLUniformLocation? location, sequence<long> v);
    void uniform4f(WebGLUniformLocation? location, GLfloat x, GLfloat y, GLfloat z, GLfloat w);
    void uniform4fv(WebGLUniformLocation? location, Float32Array v);
    void uniform4fv(WebGLUniformLocation? location, sequence<GLfloat> v);
    void uniform4i(WebGLUniformLocation? location, GLint x, GLint y, GLint z, GLint w);
    void uniform4iv(WebGLUniformLocation? location, Int32Array v);
    void uniform4iv(WebGLUniformLocation? location, sequence<long> v);

    void uniformMatrix2fv(WebGLUniformLocation? location, GLboolean transpose, 
                          Float32Array value);
    void uniformMatrix2fv(WebGLUniformLocation? location, GLboolean transpose, 
                          sequence<GLfloat> value);
    void uniformMatrix3fv(WebGLUniformLocation? location, GLboolean transpose, 
                          Float32Array value);
    void uniformMatrix3fv(WebGLUniformLocation? location, GLboolean transpose, 
                          sequence<GLfloat> value);
    void uniformMatrix4fv(WebGLUniformLocation? location, GLboolean transpose, 
                          Float32Array value);
    void uniformMatrix4fv(WebGLUniformLocation? location, GLboolean transpose, 
                          sequence<GLfloat> value);

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