module Graphics.Rendering.WebGL.Raw (
    module Graphics.Rendering.WebGL.Types,
    module Graphics.Rendering.WebGL.Constants,
    module Graphics.Rendering.WebGL.Raw
    ) where

import Foreign.C.Types
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal

import GHCJS.TypedArray (TypedArray)
import Graphics.Rendering.WebGL.Types
import Graphics.Rendering.WebGL.Constants


-- | void activeTexture(GLenum texture);
foreign import javascript unsafe "$1['activeTexture']($2);"
    js_activeTexture :: WebGLContext -> GLenum -> IO ()

-- |  void attachShader(WebGLProgram? program, WebGLShader? shader);
foreign import javascript unsafe "$1['attachShader']($2, $3);"
    js_attachShader :: WebGLContext -> WebGLProgram -> WebGLShader -> IO ()

-- | void bindAttribLocation(WebGLProgram? program, GLuint index, DOMString name);
foreign import javascript unsafe "$1['bindAttribLocation']($2, $3, $4);"
    js_bindAttribLocation :: WebGLContext -> WebGLProgram -> GLuint -> JSString -> IO ()

-- | void bindBuffer(GLenum target, WebGLBuffer? buffer);
foreign import javascript unsafe "$1['bindBuffer']($2, $3);"
    js_bindBuffer :: WebGLContext -> GLenum -> WebGLBuffer -> IO ()

    --  void bindFramebuffer(GLenum target, WebGLFramebuffer? framebuffer);
    --  void bindRenderbuffer(GLenum target, WebGLRenderbuffer? renderbuffer);
    --  void bindTexture(GLenum target, WebGLTexture? texture);

foreign import javascript unsafe "$1['bindTexture']($2, $3);"
    js_bindTexture :: WebGLContext -> GLenum -> WebGLTexture -> IO ()

foreign import javascript unsafe "$1['bindTexture']($2, null);"
    js_unbindTexture :: WebGLContext -> GLenum -> IO ()

    --  void blendColor(GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha);
    --  void blendEquation(GLenum mode);
    --  void blendEquationSeparate(GLenum modeRGB, GLenum modeAlpha);
    --  void blendFunc(GLenum sfactor, GLenum dfactor);
    --  void blendFuncSeparate(GLenum srcRGB, GLenum dstRGB, 
                           --  GLenum srcAlpha, GLenum dstAlpha);

    --  void bufferData(GLenum target, GLsizeiptr size, GLenum usage);

foreign import javascript unsafe "$1['bufferData']($2, $3, $4);"
    js_bufferData_empty :: WebGLContext -> GLenum -> GLsizeiptr -> GLenum -> IO ()

    --  void bufferData(GLenum target, ArrayBufferView data, GLenum usage);
    --  void bufferData(GLenum target, ArrayBuffer? data, GLenum usage);

foreign import javascript unsafe "$1['bufferData']($2, $3, $4);"
    js_bufferData_from :: WebGLContext -> GLenum -> TypedArray a -> GLenum -> IO ()

    --  void bufferSubData(GLenum target, GLintptr offset, ArrayBufferView data);
    --  void bufferSubData(GLenum target, GLintptr offset, ArrayBuffer? data);

    --  [WebGLHandlesContextLoss] GLenum checkFramebufferStatus(GLenum target);
    --  void clear(GLbitfield mask);

foreign import javascript unsafe "$1['clear']($2);"
    js_clear :: WebGLContext -> GLbitfield -> IO ()

    --  void clearColor(GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha);

--  foreign import javascript unsafe "$1['clearColor']($2, $3, $4, $5);"
    --  js_setClearColor :: WebGLContext -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

foreign import javascript unsafe "$1['clearColor']($2[0], $2[1], $2[2], $2[3]);"
    js_setClearColor :: WebGLContext -> TypedArray Float -> IO ()

-- does this actually work???? noooooo it doesn't
--  foreign import javascript unsafe "var c = $1['getParameter']($1.COLOR_CLEAR_VALUE); $r = h$c4(h$ghczmprimZCGHCziTupleziZLz4cUZR_con_e, c[0], c[1], c[2], c[3]);"
    --  js_getClearColor :: WebGLContext -> IO (GLfloat, GLfloat, GLfloat, GLfloat)

    --  void clearDepth(GLclampf depth);
    --  void clearStencil(GLint s);
    --  void colorMask(GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha);
    --  void compileShader(WebGLShader? shader);

foreign import javascript unsafe "$1['compileShader']($2);"
    js_compileShader :: WebGLContext -> WebGLShader -> IO ()

    --  void compressedTexImage2D(GLenum target, GLint level, GLenum internalformat,
                              --  GLsizei width, GLsizei height, GLint border,
                              --  ArrayBufferView data);
    --  void compressedTexSubImage2D(GLenum target, GLint level,
                                 --  GLint xoffset, GLint yoffset,
                                 --  GLsizei width, GLsizei height, GLenum format,
                                 --  ArrayBufferView data);

    --  void copyTexImage2D(GLenum target, GLint level, GLenum internalformat, 
                        --  GLint x, GLint y, GLsizei width, GLsizei height, 
                        --  GLint border);
    --  void copyTexSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, 
                           --  GLint x, GLint y, GLsizei width, GLsizei height);

    --  WebGLBuffer? createBuffer();

foreign import javascript unsafe "$1['createBuffer']()"
    js_createBuffer :: WebGLContext -> IO WebGLBuffer

    --  WebGLFramebuffer? createFramebuffer();
    --  WebGLProgram? createProgram();

foreign import javascript unsafe "$1['createProgram']()"
    js_createProgram :: WebGLContext -> IO WebGLProgram

    --  WebGLRenderbuffer? createRenderbuffer();
    --  WebGLShader? createShader(GLenum type);

foreign import javascript unsafe "$1['createShader']($2)"
    js_createShader :: WebGLContext -> GLenum -> IO WebGLShader

    --  WebGLTexture? createTexture();

foreign import javascript unsafe "$1['createTexture']()"
    js_createTexture :: WebGLContext -> IO WebGLTexture

    --  void cullFace(GLenum mode);
foreign import javascript unsafe "$1['cullFace']($2);"
    js_cullFace :: WebGLContext -> GLenum -> IO ()

    --  void deleteBuffer(WebGLBuffer? buffer);
    --  void deleteFramebuffer(WebGLFramebuffer? framebuffer);
    --  void deleteProgram(WebGLProgram? program);
    --  void deleteRenderbuffer(WebGLRenderbuffer? renderbuffer);
    --  void deleteShader(WebGLShader? shader);
    --  void deleteTexture(WebGLTexture? texture);

    --  void depthFunc(GLenum func);
    --  void depthMask(GLboolean flag);
    --  void depthRange(GLclampf zNear, GLclampf zFar);
    --  void detachShader(WebGLProgram? program, WebGLShader? shader);
    --  void disable(GLenum cap);

foreign import javascript unsafe "$1['disable']($2);"
    js_disable :: WebGLContext -> GLenum -> IO ()

    --  void disableVertexAttribArray(GLuint index);
    --  void drawArrays(GLenum mode, GLint first, GLsizei count);

foreign import javascript unsafe "$1['drawArrays']($2, $3, $4);"
    js_drawArrays :: WebGLContext -> GLenum -> GLint -> GLsizei -> IO ()


    --  void drawElements(GLenum mode, GLsizei count, GLenum type, GLintptr offset);

foreign import javascript unsafe "$1['drawElements']($2, $3, $4, $5);"
    js_drawElements :: WebGLContext -> GLenum -> GLsizei -> GLenum -> GLintptr -> IO ()

    --  void enable(GLenum cap);

foreign import javascript unsafe "$1['enable']($2);"
    js_enable :: WebGLContext -> GLenum -> IO ()

    --  void enableVertexAttribArray(GLuint index);

foreign import javascript unsafe "$1['enableVertexAttribArray']($2);"
    js_enableVertexAttribArray :: WebGLContext -> GLuint -> IO ()

    --  void finish();
    --  void flush();
    --  void framebufferRenderbuffer(GLenum target, GLenum attachment, 
                                 --  GLenum renderbuffertarget, 
                                 --  WebGLRenderbuffer? renderbuffer);
    --  void framebufferTexture2D(GLenum target, GLenum attachment, GLenum textarget, 
                              --  WebGLTexture? texture, GLint level);
    --  void frontFace(GLenum mode);

foreign import javascript unsafe "$1['frontFace']($2);"
    js_frontFace :: WebGLContext -> GLenum -> IO ()

    --  void generateMipmap(GLenum target);

    --  WebGLActiveInfo? getActiveAttrib(WebGLProgram? program, GLuint index);
    --  WebGLActiveInfo? getActiveUniform(WebGLProgram? program, GLuint index);
    --  sequence<WebGLShader>? getAttachedShaders(WebGLProgram? program);

    --  [WebGLHandlesContextLoss] GLint getAttribLocation(WebGLProgram? program, DOMString name);

foreign import javascript unsafe "$1['getAttribLocation']($2, $3)"
    js_getAttribLocation :: WebGLContext -> WebGLProgram -> JSString -> IO GLint

    --  any getBufferParameter(GLenum target, GLenum pname);
    --  any getParameter(GLenum pname);

    --  [WebGLHandlesContextLoss] GLenum getError();

    --  any getFramebufferAttachmentParameter(GLenum target, GLenum attachment, 
                                          --  GLenum pname);
    --  any getProgramParameter(WebGLProgram? program, GLenum pname);

foreign import javascript unsafe "$1['getProgramParameter']($2, $1['DELETE_STATUS'])"
    js_getProgramParameter_delete :: WebGLContext -> WebGLProgram -> IO GLboolean
foreign import javascript unsafe "$1['getProgramParameter']($2, $1['LINK_STATUS'])"
    js_getProgramParameter_link :: WebGLContext -> WebGLProgram -> IO GLboolean
foreign import javascript unsafe "$1['getProgramParameter']($2, $1['VALIDATE_STATUS'])"
    js_getProgramParameter_validate :: WebGLContext -> WebGLProgram -> IO GLboolean
foreign import javascript unsafe "$1['getProgramParameter']($2, $1['ATTACHED_SHADERS'])"
    js_getProgramParameter_shaders :: WebGLContext -> WebGLProgram -> IO GLint
foreign import javascript unsafe "$1['getProgramParameter']($2, $1['ATTACHED_ATTRIBUTES'])"
    js_getProgramParameter_attributes :: WebGLContext -> WebGLProgram -> IO GLint
foreign import javascript unsafe "$1['getProgramParameter']($2, $1['ATTACHED_UNIFORMS'])"
    js_getProgramParameter_uniforms :: WebGLContext -> WebGLProgram -> IO GLint

    --  DOMString? getProgramInfoLog(WebGLProgram? program);
    --  any getRenderbufferParameter(GLenum target, GLenum pname);
    --  any getShaderParameter(WebGLShader? shader, GLenum pname);
    --  WebGLShaderPrecisionFormat? getShaderPrecisionFormat(GLenum shadertype, GLenum precisiontype);
    --  DOMString? getShaderInfoLog(WebGLShader? shader);

    --  DOMString? getShaderSource(WebGLShader? shader);

    --  any getTexParameter(GLenum target, GLenum pname);

    --  any getUniform(WebGLProgram? program, WebGLUniformLocation? location);

foreign import javascript unsafe "$1['getUniform']($2, $3)"
    js_getUniform :: WebGLContext -> WebGLProgram -> WebGLUniformLocation -> IO WebGLUniformValue

    --  WebGLUniformLocation? getUniformLocation(WebGLProgram? program, DOMString name);

foreign import javascript unsafe "$1['getUniformLocation']($2, $3)"
    js_getUniformLocation :: WebGLContext -> WebGLProgram -> JSString -> IO WebGLUniformLocation

    --  any getVertexAttrib(GLuint index, GLenum pname);

    --  [WebGLHandlesContextLoss] GLsizeiptr getVertexAttribOffset(GLuint index, GLenum pname);

    --  void hint(GLenum target, GLenum mode);
    --  [WebGLHandlesContextLoss] GLboolean isBuffer(WebGLBuffer? buffer);
    --  [WebGLHandlesContextLoss] GLboolean isEnabled(GLenum cap);
    --  [WebGLHandlesContextLoss] GLboolean isFramebuffer(WebGLFramebuffer? framebuffer);
    --  [WebGLHandlesContextLoss] GLboolean isProgram(WebGLProgram? program);
    --  [WebGLHandlesContextLoss] GLboolean isRenderbuffer(WebGLRenderbuffer? renderbuffer);
    --  [WebGLHandlesContextLoss] GLboolean isShader(WebGLShader? shader);
    --  [WebGLHandlesContextLoss] GLboolean isTexture(WebGLTexture? texture);
    --  void lineWidth(GLfloat width);
    --  void linkProgram(WebGLProgram? program);

foreign import javascript unsafe "$1['linkProgram']($2);"
    js_linkProgram :: WebGLContext -> WebGLProgram -> IO ()

    --  void pixelStorei(GLenum pname, GLint param);

foreign import javascript unsafe "$1['pixelStorei']($2, $3);"
    js_pixelStorei :: WebGLContext -> GLenum -> GLint -> IO ()

foreign import javascript unsafe "$1['pixelStorei']($2, $3 ? 1 : 0);"
    js_pixelStorei_bool :: WebGLContext -> GLenum -> GLboolean -> IO ()

    --  void polygonOffset(GLfloat factor, GLfloat units);

    --  void readPixels(GLint x, GLint y, GLsizei width, GLsizei height, 
                    --  GLenum format, GLenum type, ArrayBufferView? pixels);

    --  void renderbufferStorage(GLenum target, GLenum internalformat, 
                             --  GLsizei width, GLsizei height);
    --  void sampleCoverage(GLclampf value, GLboolean invert);
    --  void scissor(GLint x, GLint y, GLsizei width, GLsizei height);

    --  void shaderSource(WebGLShader? shader, DOMString source);

    --  void stencilFunc(GLenum func, GLint ref, GLuint mask);
    --  void stencilFuncSeparate(GLenum face, GLenum func, GLint ref, GLuint mask);
    --  void stencilMask(GLuint mask);
    --  void stencilMaskSeparate(GLenum face, GLuint mask);
    --  void stencilOp(GLenum fail, GLenum zfail, GLenum zpass);
    --  void stencilOpSeparate(GLenum face, GLenum fail, GLenum zfail, GLenum zpass);

    --  void texImage2D(GLenum target, GLint level, GLenum internalformat, 
                    --  GLsizei width, GLsizei height, GLint border, GLenum format, 
                    --  GLenum type, ArrayBufferView? pixels);
    --  void texImage2D(GLenum target, GLint level, GLenum internalformat,
                    --  GLenum format, GLenum type, ImageData? pixels);
    --  void texImage2D(GLenum target, GLint level, GLenum internalformat,
                    --  GLenum format, GLenum type, HTMLImageElement image); // May throw DOMException
    --  void texImage2D(GLenum target, GLint level, GLenum internalformat,
                    --  GLenum format, GLenum type, HTMLCanvasElement canvas); // May throw DOMException
    --  void texImage2D(GLenum target, GLint level, GLenum internalformat,
                    --  GLenum format, GLenum type, HTMLVideoElement video); // May throw DOMException

foreign import javascript unsafe "$1['texImage2D']($2, $3, $4, $5, $6, $7);"
    js_texImage2D :: WebGLContext -> GLenum -> GLint -> GLenum -> GLenum -> GLenum -> Image -> IO ()

    --  void texParameterf(GLenum target, GLenum pname, GLfloat param);

foreign import javascript unsafe "$1['texParameterf']($2, $3, $4);"
    js_texParameterf :: WebGLContext -> GLenum -> GLenum -> GLfloat -> IO ()

    --  void texParameteri(GLenum target, GLenum pname, GLint param);

foreign import javascript unsafe "$1['texParameteri']($2, $3, $4);"
    js_texParameteri :: WebGLContext -> GLenum -> GLenum -> GLint -> IO ()

foreign import javascript unsafe "$1['texParameteri']($2, $3, $4);"
    js_texParameteri_enum :: WebGLContext -> GLenum -> GLenum -> GLenum -> IO ()

    --  void texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, 
                       --  GLsizei width, GLsizei height, 
                       --  GLenum format, GLenum type, ArrayBufferView? pixels);
    --  void texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, 
                       --  GLenum format, GLenum type, ImageData? pixels);
    --  void texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, 
                       --  GLenum format, GLenum type, HTMLImageElement image); // May throw DOMException
    --  void texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, 
                       --  GLenum format, GLenum type, HTMLCanvasElement canvas); // May throw DOMException
    --  void texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, 
                       --  GLenum format, GLenum type, HTMLVideoElement video); // May throw DOMException

foreign import javascript unsafe "$1['uniform1f']($2, $3);"
    js_uniform1f :: WebGLContext -> WebGLUniformLocation -> Float -> IO ()

    --  void uniform1fv(WebGLUniformLocation? location, Float32Array v);
    --  void uniform1fv(WebGLUniformLocation? location, sequence<GLfloat> v);
    --  void uniform1i(WebGLUniformLocation? location, GLint x);

foreign import javascript unsafe "$1['uniform1i']($2, $3);"
    js_uniform1i :: WebGLContext -> WebGLUniformLocation -> GLint -> IO ()

    --  void uniform1iv(WebGLUniformLocation? location, Int32Array v);
    --  void uniform1iv(WebGLUniformLocation? location, sequence<long> v);
    --  void uniform2f(WebGLUniformLocation? location, GLfloat x, GLfloat y);
    --  void uniform2fv(WebGLUniformLocation? location, Float32Array v);
    --  void uniform2fv(WebGLUniformLocation? location, sequence<GLfloat> v);
    --  void uniform2i(WebGLUniformLocation? location, GLint x, GLint y);
    --  void uniform2iv(WebGLUniformLocation? location, Int32Array v);
    --  void uniform2iv(WebGLUniformLocation? location, sequence<long> v);
    --  void uniform3f(WebGLUniformLocation? location, GLfloat x, GLfloat y, GLfloat z);

foreign import javascript unsafe "$1['uniform3f']($2, $3, $4, $5);"
    js_uniform3f :: WebGLContext -> WebGLUniformLocation -> Float -> Float -> Float -> IO ()

    --  void uniform3fv(WebGLUniformLocation? location, Float32Array v);
    --  void uniform3fv(WebGLUniformLocation? location, sequence<GLfloat> v);

foreign import javascript unsafe "$1['uniform3fv']($2, $3);"
    js_uniform3fv :: WebGLContext -> WebGLUniformLocation -> TypedArray Float -> IO ()

    --  void uniform3i(WebGLUniformLocation? location, GLint x, GLint y, GLint z);
    --  void uniform3iv(WebGLUniformLocation? location, Int32Array v);
    --  void uniform3iv(WebGLUniformLocation? location, sequence<long> v);
    --  void uniform4f(WebGLUniformLocation? location, GLfloat x, GLfloat y, GLfloat z, GLfloat w);
    --  void uniform4fv(WebGLUniformLocation? location, Float32Array v);
    --  void uniform4fv(WebGLUniformLocation? location, sequence<GLfloat> v);
    --  void uniform4i(WebGLUniformLocation? location, GLint x, GLint y, GLint z, GLint w);
    --  void uniform4iv(WebGLUniformLocation? location, Int32Array v);
    --  void uniform4iv(WebGLUniformLocation? location, sequence<long> v);

    --  void uniformMatrix2fv(WebGLUniformLocation? location, GLboolean transpose, 
                          --  Float32Array value);
    --  void uniformMatrix2fv(WebGLUniformLocation? location, GLboolean transpose, 
                          --  sequence<GLfloat> value);
    --  void uniformMatrix3fv(WebGLUniformLocation? location, GLboolean transpose, 
                          --  Float32Array value);
    --  void uniformMatrix3fv(WebGLUniformLocation? location, GLboolean transpose, 
                          --  sequence<GLfloat> value);

foreign import javascript unsafe "$1['uniformMatrix3fv']($2, $3, $4);"
    js_uniformMatrix3fv :: WebGLContext -> WebGLUniformLocation -> GLboolean -> TypedArray Float -> IO ()


    --  void uniformMatrix4fv(WebGLUniformLocation? location, GLboolean transpose, 
                          --  Float32Array value);
    --  void uniformMatrix4fv(WebGLUniformLocation? location, GLboolean transpose, 
                          --  sequence<GLfloat> value);

foreign import javascript unsafe "$1['uniformMatrix4fv']($2, $3, $4);"
    js_uniformMatrix4fv :: WebGLContext -> WebGLUniformLocation -> GLboolean -> TypedArray Float -> IO ()

    --  void useProgram(WebGLProgram? program);

foreign import javascript unsafe "$1['useProgram']($2);"
    js_useProgram :: WebGLContext -> WebGLProgram -> IO ()

    --  void validateProgram(WebGLProgram? program);

    --  void vertexAttrib1f(GLuint indx, GLfloat x);
    --  void vertexAttrib1fv(GLuint indx, Float32Array values);
    --  void vertexAttrib1fv(GLuint indx, sequence<GLfloat> values);
    --  void vertexAttrib2f(GLuint indx, GLfloat x, GLfloat y);
    --  void vertexAttrib2fv(GLuint indx, Float32Array values);
    --  void vertexAttrib2fv(GLuint indx, sequence<GLfloat> values);
    --  void vertexAttrib3f(GLuint indx, GLfloat x, GLfloat y, GLfloat z);
    --  void vertexAttrib3fv(GLuint indx, Float32Array values);
    --  void vertexAttrib3fv(GLuint indx, sequence<GLfloat> values);
    --  void vertexAttrib4f(GLuint indx, GLfloat x, GLfloat y, GLfloat z, GLfloat w);
    --  void vertexAttrib4fv(GLuint indx, Float32Array values);
    --  void vertexAttrib4fv(GLuint indx, sequence<GLfloat> values);
    --  void vertexAttribPointer(GLuint indx, GLint size, GLenum type, 
                             --  GLboolean normalized, GLsizei stride, GLintptr offset);

foreign import javascript unsafe "$1['vertexAttribPointer']($2, $3, $4, $5, $6, $7);"
    js_vertexAttribPointer :: WebGLContext -> GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLintptr -> IO ()

    --  void viewport(GLint x, GLint y, GLsizei width, GLsizei height);

foreign import javascript unsafe "$1['viewport']($2, $3, $4, $5);"
    js_setViewport :: WebGLContext -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()

