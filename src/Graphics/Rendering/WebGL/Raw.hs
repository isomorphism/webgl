module Graphics.Rendering.WebGL.Raw (
    module Graphics.Rendering.WebGL.Types,
    module Graphics.Rendering.WebGL.Raw
    ) where

import GHCJS.Types
import Data.Word
import Data.Int

import GHCJS.TypedArray (TypedArray, ArrayBufferView)
import Graphics.Rendering.WebGL.Types


-- | [WebGLHandlesContextLoss] WebGLContextAttributes? getContextAttributes();
foreign import javascript unsafe "$1['getContextAttributes']();"
    js_getContextAttributes :: WebGLContext -> IO WebGLContextAttributes

-- | [WebGLHandlesContextLoss] boolean isContextLost();
foreign import javascript unsafe "$1['isContextLost']();"
    js_isContextLost :: WebGLContext -> IO Bool

-- | sequence<DOMString>? getSupportedExtensions();
foreign import javascript unsafe "$1['getSupportedExtensions']();"
    js_getSupportedExtensions :: WebGLContext -> IO (JSArray JSString)

-- | object? getExtension(DOMString name);
foreign import javascript unsafe "$1['getExtension']($2);"
    js_getExtension :: WebGLContext -> JSString -> IO WebGLExtension

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

-- | void bindFramebuffer(GLenum target, WebGLFramebuffer? framebuffer);
foreign import javascript unsafe "$1['bindFrameBuffer']($2, $3);"
    js_bindFrameBuffer :: WebGLContext -> GLenum -> WebGLFrameBuffer -> IO ()

-- | void bindRenderbuffer(GLenum target, WebGLRenderbuffer? renderbuffer);
foreign import javascript unsafe "$1['bindRenderBuffer']($2, $3);"
    js_bindRenderBuffer :: WebGLContext -> GLenum -> WebGLRenderBuffer -> IO ()

-- | void bindTexture(GLenum target, WebGLTexture? texture);
foreign import javascript unsafe "$1['bindTexture']($2, $3);"
    js_bindTexture :: WebGLContext -> GLenum -> WebGLTexture -> IO ()

-- | void bindTexture(GLenum target, null);
foreign import javascript unsafe "$1['bindTexture']($2, null);"
    js_unbindTexture :: WebGLContext -> GLenum -> IO ()

-- | void blendColor(GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha);
foreign import javascript unsafe "$1['blendColor']($2, $3, $4, $5);"
    js_blendColor :: WebGLContext -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

-- | void blendEquation(GLenum mode);
foreign import javascript unsafe "$1['blendEquation']($2);"
    js_blendEquation :: WebGLContext -> GLenum -> IO ()

-- | void blendEquationSeparate(GLenum modeRGB, GLenum modeAlpha);
foreign import javascript unsafe "$1['blendEquationSeparate']($2, $3);"
    js_blendEquationSeparate :: WebGLContext -> GLenum -> GLenum -> IO ()

-- | void blendFunc(GLenum sfactor, GLenum dfactor);
foreign import javascript unsafe "$1['blendFunc']($2, $3);"
    js_blendFunc :: WebGLContext -> GLenum -> GLenum -> IO ()

-- | void blendFuncSeparate(GLenum srcRGB, GLenum dstRGB, GLenum srcAlpha, GLenum dstAlpha);
foreign import javascript unsafe "$1['blendFuncSeparate']($2, $3, $4, $5);"
    js_blendFuncSeparate :: WebGLContext -> GLenum -> GLenum -> GLenum -> GLenum -> IO ()

-- | void bufferData(GLenum target, GLsizeiptr size, GLenum usage);
foreign import javascript unsafe "$1['bufferData']($2, $3, $4);"
    js_bufferData_empty :: WebGLContext -> GLenum -> GLsizeiptr -> GLenum -> IO ()

-- | void bufferData(GLenum target, ArrayBufferView data, GLenum usage);
-- | void bufferData(GLenum target, ArrayBuffer? data, GLenum usage);
foreign import javascript unsafe "$1['bufferData']($2, $3, $4);"
    js_bufferData :: WebGLContext -> GLenum -> TypedArray a -> GLenum -> IO ()

-- | void bufferSubData(GLenum target, GLintptr offset, ArrayBufferView data);
-- | void bufferSubData(GLenum target, GLintptr offset, ArrayBuffer? data);
foreign import javascript unsafe "$1['bufferSubData']($2, $3, $4);"
    js_bufferSubData :: WebGLContext -> GLenum -> GLintptr -> TypedArray a -> IO ()

-- | [WebGLHandlesContextLoss] GLenum checkFramebufferStatus(GLenum target);
foreign import javascript unsafe "$1['checkFramebufferStatus']($2);"
    js_checkFramebufferStatus :: WebGLContext -> GLenum -> IO GLenum

-- | void clear(GLbitfield mask);
foreign import javascript unsafe "$1['clear']($2);"
    js_clear :: WebGLContext -> GLbitfield -> IO ()

-- | void clearColor(GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha);
foreign import javascript unsafe "$1['clearColor']($2[0], $2[1], $2[2], $2[3]);"
    js_clearColor :: WebGLContext -> TypedArray Float -> IO ()

-- | void clearDepth(GLclampf depth);
foreign import javascript unsafe "$1['clearDepth']($2);"
    js_clearDepth :: WebGLContext -> GLfloat -> IO ()

-- | void clearStencil(GLint s);
foreign import javascript unsafe "$1['clearStencil']($2);"
    js_clearStencil :: WebGLContext -> GLint -> IO ()

-- | void colorMask(GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha);
foreign import javascript unsafe "$1['colorMask']($2, $3, $4, $5);"
    js_colorMask :: WebGLContext -> GLboolean -> GLboolean -> GLboolean -> GLboolean -> IO ()

-- | void compileShader(WebGLShader? shader);
foreign import javascript unsafe "$1['compileShader']($2);"
    js_compileShader :: WebGLContext -> WebGLShader -> IO ()

{- default WebGL doesn't support compressed textures I think, so skipping these for now

-- | void compressedTexImage2D(GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLint border, ArrayBufferView data);
foreign import javascript unsafe "$1['compressedTexImage2D']($2, $3, $4, $5, $6, $7, $8);"
    js_compressedTexImage2D :: WebGLContext -> GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> ArrayBufferView -> IO ()

-- | void compressedTexSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, ArrayBufferView data);
foreign import javascript unsafe "$1['compressedTexSubImage2D']($2, $3, $4, $5, $6, $7, $8, $9);"
    js_compressedTexSubImage2D :: WebGLContext -> GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> ArrayBufferView -> IO ()
-}

-- | void copyTexImage2D(GLenum target, GLint level, GLenum internalformat, GLint x, GLint y, GLsizei width, GLsizei height, GLint border);
foreign import javascript unsafe "$1['copyTexImage2D']($2, $3, $4, $5, $6, $7, $8, $9);"
    js_copyTexImage2D :: WebGLContext -> GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> IO ()

-- | void copyTexSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint x, GLint y, GLsizei width, GLsizei height);
foreign import javascript unsafe "$1['copyTexSubImage2D']($2, $3, $4, $5, $6, $7, $8, $9);"
    js_copyTexSubImage2D :: WebGLContext -> GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()

-- | WebGLBuffer? createBuffer();
foreign import javascript unsafe "$1['createBuffer']()"
    js_createBuffer :: WebGLContext -> IO WebGLBuffer

-- | WebGLFramebuffer? createFramebuffer();
foreign import javascript unsafe "$1['createFrameBuffer']()"
    js_createFrameBuffer :: WebGLContext -> IO WebGLFrameBuffer

-- | WebGLProgram? createProgram();
foreign import javascript unsafe "$1['createProgram']()"
    js_createProgram :: WebGLContext -> IO WebGLProgram

-- | WebGLRenderbuffer? createRenderbuffer();
foreign import javascript unsafe "$1['createRenderBuffer']()"
    js_createRenderBuffer :: WebGLContext -> IO WebGLRenderBuffer

-- | WebGLShader? createShader(GLenum type);
foreign import javascript unsafe "$1['createShader']($2)"
    js_createShader :: WebGLContext -> GLenum -> IO WebGLShader

-- | WebGLTexture? createTexture();
foreign import javascript unsafe "$1['createTexture']()"
    js_createTexture :: WebGLContext -> IO WebGLTexture

-- | void cullFace(GLenum mode);
foreign import javascript unsafe "$1['cullFace']($2);"
    js_cullFace :: WebGLContext -> GLenum -> IO ()

-- | void deleteBuffer(WebGLBuffer? buffer);
foreign import javascript unsafe "$1['deleteBuffer']($2)"
    js_deleteBuffer :: WebGLContext -> WebGLBuffer -> IO ()

-- | void deleteFramebuffer(WebGLFramebuffer? framebuffer);
foreign import javascript unsafe "$1['deleteFrameBuffer']($2)"
    js_deleteFrameBuffer :: WebGLContext -> WebGLFrameBuffer -> IO ()

-- | void deleteProgram(WebGLProgram? program);
foreign import javascript unsafe "$1['deleteProgram']($2)"
    js_deleteProgram :: WebGLContext -> WebGLProgram -> IO ()

-- | void deleteRenderbuffer(WebGLRenderbuffer? renderbuffer);
foreign import javascript unsafe "$1['deleteRenderBuffer']($2)"
    js_deleteRenderBuffer :: WebGLContext -> WebGLRenderBuffer -> IO ()

-- | void deleteShader(WebGLShader? shader);
foreign import javascript unsafe "$1['deleteShader']($2)"
    js_deleteShader :: WebGLContext -> WebGLShader -> IO ()

-- | void deleteTexture(WebGLTexture? texture);
foreign import javascript unsafe "$1['deleteTexture']($2)"
    js_deleteTexture :: WebGLContext -> WebGLTexture -> IO ()

-- | void depthFunc(GLenum func);
foreign import javascript unsafe "$1['depthFunc']($2)"
    js_depthFunc :: WebGLContext -> GLenum -> IO ()

-- | void depthMask(GLboolean flag);
foreign import javascript unsafe "$1['depthMask']($2)"
    js_depthMask :: WebGLContext -> GLboolean -> IO ()

-- | void depthRange(GLclampf zNear, GLclampf zFar);
foreign import javascript unsafe "$1['depthRange']($2, $3)"
    js_depthRange :: WebGLContext -> GLfloat -> GLfloat -> IO ()

-- | void detachShader(WebGLProgram? program, WebGLShader? shader);
foreign import javascript unsafe "$1['detachShader']($2)"
    js_detachShader :: WebGLContext -> WebGLProgram -> WebGLShader -> IO ()

-- | void disable(GLenum cap);
foreign import javascript unsafe "$1['disable']($2);"
    js_disable :: WebGLContext -> GLenum -> IO ()

-- | void disableVertexAttribArray(GLuint index);
foreign import javascript unsafe "$1['disableVertexAttribArray']($2)"
    js_disableVertexAttribArray :: WebGLContext -> GLuint -> IO ()

-- | void drawArrays(GLenum mode, GLint first, GLsizei count);
foreign import javascript unsafe "$1['drawArrays']($2, $3, $4);"
    js_drawArrays :: WebGLContext -> GLenum -> GLint -> GLsizei -> IO ()

-- | void drawElements(GLenum mode, GLsizei count, GLenum type, GLintptr offset);
foreign import javascript unsafe "$1['drawElements']($2, $3, $4, $5);"
    js_drawElements :: WebGLContext -> GLenum -> GLsizei -> GLenum -> GLintptr -> IO ()

-- | void enable(GLenum cap);
foreign import javascript unsafe "$1['enable']($2);"
    js_enable :: WebGLContext -> GLenum -> IO ()

-- | void enableVertexAttribArray(GLuint index);
foreign import javascript unsafe "$1['enableVertexAttribArray']($2);"
    js_enableVertexAttribArray :: WebGLContext -> GLuint -> IO ()

-- | void finish();
foreign import javascript unsafe "$1['finish']();"
    js_finish :: WebGLContext -> IO ()

-- | void flush();
foreign import javascript unsafe "$1['flush']();"
    js_flush :: WebGLContext -> IO ()

-- | void framebufferRenderbuffer(GLenum target, GLenum attachment, GLenum renderbuffertarget, WebGLRenderbuffer? renderbuffer);
foreign import javascript unsafe "$1['framebufferRenderbuffer']($2, $3, $4, $5);"
    js_framebufferRenderbuffer :: WebGLContext -> GLenum -> GLenum -> GLenum -> WebGLRenderBuffer -> IO ()

-- | void framebufferTexture2D(GLenum target, GLenum attachment, GLenum textarget, WebGLTexture? texture, GLint level);
foreign import javascript unsafe "$1['framebufferTexture2D']($2, $3, $4, $5, $6);"
    js_framebufferTexture2D :: WebGLContext -> GLenum -> GLenum -> GLenum -> WebGLTexture -> GLint -> IO ()

-- | void frontFace(GLenum mode);
foreign import javascript unsafe "$1['frontFace']($2);"
    js_frontFace :: WebGLContext -> GLenum -> IO ()

-- | void generateMipmap(GLenum target);
foreign import javascript unsafe "$1['generateMipmap']($2);"
    js_generateMipmap :: WebGLContext -> GLenum -> IO ()

-- | WebGLActiveInfo? getActiveAttrib(WebGLProgram? program, GLuint index);
foreign import javascript unsafe "$1['getActiveAttrib']($2, $3);"
    js_getActiveAttrib :: WebGLContext -> WebGLProgram -> GLuint -> IO WebGLActiveInfo

-- | WebGLActiveInfo? getActiveUniform(WebGLProgram? program, GLuint index);
foreign import javascript unsafe "$1['getActiveUniform']($2, $3);"
    js_getActiveUniform :: WebGLContext -> WebGLProgram -> GLuint -> IO WebGLActiveInfo

-- | sequence<WebGLShader>? getAttachedShaders(WebGLProgram? program);
foreign import javascript unsafe "$1['getAttachedShaders']($2);"
    js_getAttachedShaders :: WebGLContext -> WebGLProgram -> IO (JSArray WebGLShader)

-- | [WebGLHandlesContextLoss] GLint getAttribLocation(WebGLProgram? program, DOMString name);
foreign import javascript unsafe "$1['getAttribLocation']($2, $3)"
    js_getAttribLocation :: WebGLContext -> WebGLProgram -> JSString -> IO GLint

-- | any getBufferParameter(GLenum target, GLenum pname);
foreign import javascript unsafe "$1['getBufferParameter']($2, $1['BUFFER_SIZE'])"
    js_getBufferSize :: WebGLContext -> GLenum -> IO GLint
foreign import javascript unsafe "$1['getBufferParameter']($2, $1['BUFFER_USAGE'])"
    js_getBufferUsage :: WebGLContext -> GLenum -> IO GLenum

-- | any getParameter(GLenum pname);
-- this is so terrible
foreign import javascript unsafe "$1['getParameter']($2)" js_getParameter_GLboolean :: WebGLContext -> GLenum -> IO GLboolean
foreign import javascript unsafe "$1['getParameter']($2)" js_getParameter_GLenum :: WebGLContext -> GLenum -> IO GLenum
foreign import javascript unsafe "$1['getParameter']($2)" js_getParameter_GLuint :: WebGLContext -> GLenum -> IO GLuint
foreign import javascript unsafe "$1['getParameter']($2)" js_getParameter_GLint :: WebGLContext -> GLenum -> IO GLint
foreign import javascript unsafe "$1['getParameter']($2)" js_getParameter_GLfloat :: WebGLContext -> GLenum -> IO GLfloat

foreign import javascript unsafe "$1['getParameter']($2)" js_getParameter_string :: WebGLContext -> GLenum -> IO JSString

foreign import javascript unsafe "$1['getParameter']($2)" js_getParameter_GLbooleanArray :: WebGLContext -> GLenum -> IO (JSArray GLboolean)

foreign import javascript unsafe "$1['getParameter']($2)" js_getParameter_Uint32Array :: WebGLContext -> GLenum -> IO (TypedArray Word32)
foreign import javascript unsafe "$1['getParameter']($2)" js_getParameter_Int32Array :: WebGLContext -> GLenum -> IO (TypedArray Int32)
foreign import javascript unsafe "$1['getParameter']($2)" js_getParameter_Float32Array :: WebGLContext -> GLenum -> IO (TypedArray Float)

foreign import javascript unsafe "$1['getParameter']($2)" js_getParameter_buffer :: WebGLContext -> GLenum -> IO WebGLBuffer
foreign import javascript unsafe "$1['getParameter']($2)" js_getParameter_framebuffer :: WebGLContext -> GLenum -> IO WebGLFrameBuffer
foreign import javascript unsafe "$1['getParameter']($2)" js_getParameter_renderbuffer :: WebGLContext -> GLenum -> IO WebGLRenderBuffer
foreign import javascript unsafe "$1['getParameter']($2)" js_getParameter_program :: WebGLContext -> GLenum -> IO WebGLProgram
foreign import javascript unsafe "$1['getParameter']($2)" js_getParameter_texture :: WebGLContext -> GLenum -> IO WebGLTexture


-- | [WebGLHandlesContextLoss] GLenum getError();
foreign import javascript unsafe "$1['getError']();"
    js_getError :: WebGLContext -> IO GLenum

-- | any getFramebufferAttachmentParameter(GLenum target, GLenum attachment, GLenum pname);
foreign import javascript unsafe "$1['getFramebufferAttachmentParameter']($2, $3, $1['FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE'])"
    js_getFramebufferAttachmentParameter_objType :: WebGLContext -> GLenum -> GLenum -> IO GLint
--  foreign import javascript unsafe "$1['getFramebufferAttachmentParameter']($2, $3, $1['FRAMEBUFFER_ATTACHMENT_OBJECT_NAME'])"
    --  js_getFramebufferAttachmentParameter_objName :: WebGLContext -> GLenum -> GLenum -> IO ??
    -- this can be either a WebGLRenderbuffer or WebGLTexture apparently?
foreign import javascript unsafe "$1['getFramebufferAttachmentParameter']($2, $3, $1['FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL'])"
    js_getFramebufferAttachmentParameter_texLevel :: WebGLContext -> GLenum -> GLenum -> IO GLint
foreign import javascript unsafe "$1['getFramebufferAttachmentParameter']($2, $3, $1['FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE'])"
    js_getFramebufferAttachmentParameter_cubeFace :: WebGLContext -> GLenum -> GLenum -> IO GLint

-- | any getProgramParameter(WebGLProgram? program, GLenum pname);
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

-- | DOMString? getProgramInfoLog(WebGLProgram? program);
foreign import javascript unsafe "$1['getProgramInfoLog']($2)"
    js_getProgramInfoLog :: WebGLContext -> WebGLProgram -> IO JSString

-- | any getRenderbufferParameter(GLenum target, GLenum pname);
foreign import javascript unsafe "$1['getRenderbufferParameter']($2, $1['RENDERBUFFER_WIDTH'])"
    js_getRenderbufferParameter_width :: WebGLContext -> GLenum -> IO GLint
foreign import javascript unsafe "$1['getRenderbufferParameter']($2, $1['RENDERBUFFER_HEIGHT'])"
    js_getRenderbufferParameter_height :: WebGLContext -> GLenum -> IO GLint
foreign import javascript unsafe "$1['getRenderbufferParameter']($2, $1['RENDERBUFFER_RED_SIZE'])"
    js_getRenderbufferParameter_redSize :: WebGLContext -> GLenum -> IO GLint
foreign import javascript unsafe "$1['getRenderbufferParameter']($2, $1['RENDERBUFFER_GREEN_SIZE'])"
    js_getRenderbufferParameter_greenSize :: WebGLContext -> GLenum -> IO GLint
foreign import javascript unsafe "$1['getRenderbufferParameter']($2, $1['RENDERBUFFER_BLUE_SIZE'])"
    js_getRenderbufferParameter_blueSize :: WebGLContext -> GLenum -> IO GLint
foreign import javascript unsafe "$1['getRenderbufferParameter']($2, $1['RENDERBUFFER_ALPHA_SIZE'])"
    js_getRenderbufferParameter_alphaSize :: WebGLContext -> GLenum -> IO GLint
foreign import javascript unsafe "$1['getRenderbufferParameter']($2, $1['RENDERBUFFER_DEPTH_SIZE'])"
    js_getRenderbufferParameter_depthSize :: WebGLContext -> GLenum -> IO GLint
foreign import javascript unsafe "$1['getRenderbufferParameter']($2, $1['RENDERBUFFER_STENCIL_SIZE'])"
    js_getRenderbufferParameter_stencilSize :: WebGLContext -> GLenum -> IO GLint
foreign import javascript unsafe "$1['getRenderbufferParameter']($2, $1['RENDERBUFFER_INTERNAL_FORMAT'])"
    js_getRenderbufferParameter_format :: WebGLContext -> GLenum -> IO GLenum

-- | any getShaderParameter(WebGLShader? shader, GLenum pname);
foreign import javascript unsafe "$1['getShaderParameter']($2, $1['SHADER_TYPE'])"
    js_getShaderParameter_shaderType :: WebGLContext -> WebGLShader -> IO GLenum
foreign import javascript unsafe "$1['getShaderParameter']($2, $1['DELETE_STATUS'])"
    js_getShaderParameter_deleteStatus :: WebGLContext -> WebGLShader -> IO GLboolean
foreign import javascript unsafe "$1['getShaderParameter']($2, $1['COMPILE_STATUS'])"
    js_getShaderParameter_compileStatus :: WebGLContext -> WebGLShader -> IO GLboolean

-- | WebGLShaderPrecisionFormat? getShaderPrecisionFormat(GLenum shadertype, GLenum precisiontype);
foreign import javascript unsafe "$1['getShaderPrecisionFormat']($2, $3)"
    js_getShaderPrecisionFormat :: WebGLContext -> GLenum -> GLenum -> IO WebGLShaderPrecisionFormat

-- supposedly WebGLShaderPrecisionFormat's fields are all read-only so I think this can be pure
foreign import javascript unsafe "$1['rangeMin']" js_shaderPrecisionFormat_rangeMin :: WebGLShaderPrecisionFormat -> GLint
foreign import javascript unsafe "$1['rangeMax']" js_shaderPrecisionFormat_rangeMax :: WebGLShaderPrecisionFormat -> GLint
foreign import javascript unsafe "$1['precision']" js_shaderPrecisionFormat_precision :: WebGLShaderPrecisionFormat -> GLint

-- | DOMString? getShaderInfoLog(WebGLShader? shader);
foreign import javascript unsafe "$1['getShaderInfoLog']($2)"
    js_getShaderInfoLog :: WebGLContext -> WebGLShader -> IO JSString

-- | DOMString? getShaderSource(WebGLShader? shader);
foreign import javascript unsafe "$1['getShaderSource']($2)"
    js_getShaderSource :: WebGLContext -> WebGLShader -> IO JSString

-- | any getTexParameter(GLenum target, GLenum pname);
foreign import javascript unsafe "$1['getTexParameter']($2, $1['TEXTURE_MAG_FILTER'])"
    js_getTexParameter_magFilter :: WebGLContext -> GLenum -> IO GLenum
foreign import javascript unsafe "$1['getTexParameter']($2, $1['TEXTURE_MIN_FILTER'])"
    js_getTexParameter_minFilter :: WebGLContext -> GLenum -> IO GLenum
foreign import javascript unsafe "$1['getTexParameter']($2, $1['TEXTURE_WRAP_S'])"
    js_getTexParameter_wrapS :: WebGLContext -> GLenum -> IO GLenum
foreign import javascript unsafe "$1['getTexParameter']($2, $1['TEXTURE_WRAP_T'])"
    js_getTexParameter_wrapT :: WebGLContext -> GLenum -> IO GLenum

-- | any getUniform(WebGLProgram? program, WebGLUniformLocation? location);
foreign import javascript unsafe "$1['getUniform']($2, $3)"
    js_getUniform :: WebGLContext -> WebGLProgram -> WebGLUniformLocation -> IO WebGLUniformValue

-- | WebGLUniformLocation? getUniformLocation(WebGLProgram? program, DOMString name);
foreign import javascript unsafe "$1['getUniformLocation']($2, $3)"
    js_getUniformLocation :: WebGLContext -> WebGLProgram -> JSString -> IO WebGLUniformLocation

-- | any getVertexAttrib(GLuint index, GLenum pname);

-- | [WebGLHandlesContextLoss] GLsizeiptr getVertexAttribOffset(GLuint index, GLenum pname);
foreign import javascript unsafe "$1['getVertexAttribOffset']($2, $3)"
    js_getVertexAttribOffset :: WebGLContext -> GLuint -> GLenum -> IO GLsizeiptr

-- | void hint(GLenum target, GLenum mode);
foreign import javascript unsafe "$1['hint']($2, $3);"
    js_hint :: WebGLContext -> GLenum -> GLenum -> IO ()

-- | [WebGLHandlesContextLoss] GLboolean isBuffer(WebGLBuffer? buffer);

-- | [WebGLHandlesContextLoss] GLboolean isEnabled(GLenum cap);

-- | [WebGLHandlesContextLoss] GLboolean isFramebuffer(WebGLFramebuffer? framebuffer);

-- | [WebGLHandlesContextLoss] GLboolean isProgram(WebGLProgram? program);

-- | [WebGLHandlesContextLoss] GLboolean isRenderbuffer(WebGLRenderbuffer? renderbuffer);

-- | [WebGLHandlesContextLoss] GLboolean isShader(WebGLShader? shader);

-- | [WebGLHandlesContextLoss] GLboolean isTexture(WebGLTexture? texture);

-- | void lineWidth(GLfloat width);
foreign import javascript unsafe "$1['lineWidth']($2);"
    js_lineWidth :: WebGLContext -> GLfloat -> IO ()

-- | void linkProgram(WebGLProgram? program);
foreign import javascript unsafe "$1['linkProgram']($2);"
    js_linkProgram :: WebGLContext -> WebGLProgram -> IO ()

-- | void pixelStorei(GLenum pname, GLint param);
foreign import javascript unsafe "$1['pixelStorei']($2, $3);"
    js_pixelStorei :: WebGLContext -> GLenum -> GLint -> IO ()

foreign import javascript unsafe "$1['pixelStorei']($2, $3 ? 1 : 0);"
    js_pixelStorei_bool :: WebGLContext -> GLenum -> GLboolean -> IO ()

-- | void polygonOffset(GLfloat factor, GLfloat units);
foreign import javascript unsafe "$1['polygonOffset']($2, $3);"
    js_polygonOffset :: WebGLContext -> GLfloat -> GLfloat -> IO ()

-- | void readPixels(GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type, ArrayBufferView? pixels);

-- | void renderbufferStorage(GLenum target, GLenum internalformat, GLsizei width, GLsizei height);
-- | void sampleCoverage(GLclampf value, GLboolean invert);
-- | void scissor(GLint x, GLint y, GLsizei width, GLsizei height);
foreign import javascript unsafe "$1['scissor']($2, $3);"
    js_scissor :: WebGLContext -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()

-- | void shaderSource(WebGLShader? shader, DOMString source);
foreign import javascript unsafe "$1['shaderSource']($2, $3);"
    js_shaderSource :: WebGLContext -> WebGLShader -> JSString -> IO ()

-- | void stencilFunc(GLenum func, GLint ref, GLuint mask);
-- | void stencilFuncSeparate(GLenum face, GLenum func, GLint ref, GLuint mask);
-- | void stencilMask(GLuint mask);
-- | void stencilMaskSeparate(GLenum face, GLuint mask);
-- | void stencilOp(GLenum fail, GLenum zfail, GLenum zpass);
-- | void stencilOpSeparate(GLenum face, GLenum fail, GLenum zfail, GLenum zpass);

-- | void texImage2D(GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLint border, GLenum format, GLenum type, ArrayBufferView? pixels);
-- | void texImage2D(GLenum target, GLint level, GLenum internalformat, GLenum format, GLenum type, ImageData? pixels);
-- | void texImage2D(GLenum target, GLint level, GLenum internalformat, GLenum format, GLenum type, HTMLImageElement image); // May throw DOMException
-- | void texImage2D(GLenum target, GLint level, GLenum internalformat, GLenum format, GLenum type, HTMLCanvasElement canvas); // May throw DOMException
-- | void texImage2D(GLenum target, GLint level, GLenum internalformat, GLenum format, GLenum type, HTMLVideoElement video); // May throw DOMException
foreign import javascript unsafe "$1['texImage2D']($2, $3, $4, $5, $6, $7);"
    js_texImage2D :: WebGLContext -> GLenum -> GLint -> GLenum -> GLenum -> GLenum -> Image -> IO ()

-- | void texParameterf(GLenum target, GLenum pname, GLfloat param);
foreign import javascript unsafe "$1['texParameterf']($2, $3, $4);"
    js_texParameterf :: WebGLContext -> GLenum -> GLenum -> GLfloat -> IO ()

-- | void texParameteri(GLenum target, GLenum pname, GLint param);
foreign import javascript unsafe "$1['texParameteri']($2, $3, $4);"
    js_texParameteri :: WebGLContext -> GLenum -> GLenum -> GLint -> IO ()

foreign import javascript unsafe "$1['texParameteri']($2, $3, $4);"
    js_texParameteri_enum :: WebGLContext -> GLenum -> GLenum -> GLenum -> IO ()

-- | void texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLenum type, ArrayBufferView? pixels);
-- | void texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLenum format, GLenum type, ImageData? pixels);
-- | void texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLenum format, GLenum type, HTMLImageElement image); // May throw DOMException
-- | void texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLenum format, GLenum type, HTMLCanvasElement canvas); // May throw DOMException
-- | void texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLenum format, GLenum type, HTMLVideoElement video); // May throw DOMException
foreign import javascript unsafe "$1['texSubImage2D']($2, $3, $4, $5, $6, $7);"
    js_texSubImage2D :: WebGLContext -> GLenum -> GLint -> GLint -> GLint -> GLenum -> GLenum -> Image -> IO ()

-- | void uniform1f(WebGLUniformLocation? location, GLfloat x);
foreign import javascript unsafe "$1['uniform1f']($2, $3);"
    js_uniform1f :: WebGLContext -> WebGLUniformLocation -> GLfloat -> IO ()

-- | void uniform1fv(WebGLUniformLocation? location, Float32Array v);
-- | void uniform1fv(WebGLUniformLocation? location, sequence<GLfloat> v);
foreign import javascript unsafe "$1['uniform1fv']($2, $3);"
    js_uniform1fv :: WebGLContext -> WebGLUniformLocation -> TypedArray Float -> IO ()

-- | void uniform1i(WebGLUniformLocation? location, GLint x);
foreign import javascript unsafe "$1['uniform1i']($2, $3);"
    js_uniform1i :: WebGLContext -> WebGLUniformLocation -> GLint -> IO ()

-- | void uniform1iv(WebGLUniformLocation? location, Int32Array v);
-- | void uniform1iv(WebGLUniformLocation? location, sequence<long> v);
foreign import javascript unsafe "$1['uniform1iv']($2, $3);"
    js_uniform1iv :: WebGLContext -> WebGLUniformLocation -> TypedArray Int32 -> IO ()

-- | void uniform2f(WebGLUniformLocation? location, GLfloat x, GLfloat y);
foreign import javascript unsafe "$1['uniform2f']($2, $3, $4);"
    js_uniform2f :: WebGLContext -> WebGLUniformLocation -> GLfloat -> GLfloat -> IO ()

-- | void uniform2fv(WebGLUniformLocation? location, Float32Array v);
-- | void uniform2fv(WebGLUniformLocation? location, sequence<GLfloat> v);
foreign import javascript unsafe "$1['uniform2fv']($2, $3);"
    js_uniform2fv :: WebGLContext -> WebGLUniformLocation -> TypedArray Float -> IO ()

-- | void uniform2i(WebGLUniformLocation? location, GLint x, GLint y);
foreign import javascript unsafe "$1['uniform2i']($2, $3, $4);"
    js_uniform2i :: WebGLContext -> WebGLUniformLocation -> GLint -> GLint -> IO ()

-- | void uniform2iv(WebGLUniformLocation? location, Int32Array v);
-- | void uniform2iv(WebGLUniformLocation? location, sequence<long> v);
foreign import javascript unsafe "$1['uniform2iv']($2, $3);"
    js_uniform2iv :: WebGLContext -> WebGLUniformLocation -> TypedArray Int32 -> IO ()

-- | void uniform3f(WebGLUniformLocation? location, GLfloat x, GLfloat y, GLfloat z);
foreign import javascript unsafe "$1['uniform3f']($2, $3, $4, $5);"
    js_uniform3f :: WebGLContext -> WebGLUniformLocation -> GLfloat -> GLfloat -> GLfloat -> IO ()

-- | void uniform3fv(WebGLUniformLocation? location, Float32Array v);
-- | void uniform3fv(WebGLUniformLocation? location, sequence<GLfloat> v);
foreign import javascript unsafe "$1['uniform3fv']($2, $3);"
    js_uniform3fv :: WebGLContext -> WebGLUniformLocation -> TypedArray Float -> IO ()

-- | void uniform3i(WebGLUniformLocation? location, GLint x, GLint y, GLint z);
foreign import javascript unsafe "$1['uniform3i']($2, $3, $4, $5);"
    js_uniform3i :: WebGLContext -> WebGLUniformLocation -> GLint -> GLint -> GLint -> IO ()

-- | void uniform3iv(WebGLUniformLocation? location, Int32Array v);
-- | void uniform3iv(WebGLUniformLocation? location, sequence<long> v);
foreign import javascript unsafe "$1['uniform3iv']($2, $3);"
    js_uniform3iv :: WebGLContext -> WebGLUniformLocation -> TypedArray Int32 -> IO ()

-- | void uniform4f(WebGLUniformLocation? location, GLfloat x, GLfloat y, GLfloat z, GLfloat w);
foreign import javascript unsafe "$1['uniform4f']($2, $3, $4, $5, $6);"
    js_uniform4f :: WebGLContext -> WebGLUniformLocation -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()


-- | void uniform4fv(WebGLUniformLocation? location, Float32Array v);
-- | void uniform4fv(WebGLUniformLocation? location, sequence<GLfloat> v);
foreign import javascript unsafe "$1['uniform4fv']($2, $3);"
    js_uniform4fv :: WebGLContext -> WebGLUniformLocation -> TypedArray Float -> IO ()
    
-- | void uniform4i(WebGLUniformLocation? location, GLint x, GLint y, GLint z, GLint w);
foreign import javascript unsafe "$1['uniform4i']($2, $3, $4, $5);"
    js_uniform4i :: WebGLContext -> WebGLUniformLocation -> GLint -> GLint -> GLint -> GLint -> IO ()

-- | void uniform4iv(WebGLUniformLocation? location, Int32Array v);
-- | void uniform4iv(WebGLUniformLocation? location, sequence<long> v);
foreign import javascript unsafe "$1['uniform4iv']($2, $3);"
    js_uniform4iv :: WebGLContext -> WebGLUniformLocation -> TypedArray Int32 -> IO ()

-- | void uniformMatrix2fv(WebGLUniformLocation? location, GLboolean transpose, Float32Array value);
-- | void uniformMatrix2fv(WebGLUniformLocation? location, GLboolean transpose, sequence<GLfloat> value);
foreign import javascript unsafe "$1['uniformMatrix2fv']($2, $3, $4);"
    js_uniformMatrix2fv :: WebGLContext -> WebGLUniformLocation -> GLboolean -> TypedArray Float -> IO ()

-- | void uniformMatrix3fv(WebGLUniformLocation? location, GLboolean transpose, Float32Array value);
-- | void uniformMatrix3fv(WebGLUniformLocation? location, GLboolean transpose, sequence<GLfloat> value);
foreign import javascript unsafe "$1['uniformMatrix3fv']($2, $3, $4);"
    js_uniformMatrix3fv :: WebGLContext -> WebGLUniformLocation -> GLboolean -> TypedArray Float -> IO ()

-- | void uniformMatrix4fv(WebGLUniformLocation? location, GLboolean transpose, Float32Array value);
-- | void uniformMatrix4fv(WebGLUniformLocation? location, GLboolean transpose, sequence<GLfloat> value);
foreign import javascript unsafe "$1['uniformMatrix4fv']($2, $3, $4);"
    js_uniformMatrix4fv :: WebGLContext -> WebGLUniformLocation -> GLboolean -> TypedArray Float -> IO ()

-- | void useProgram(WebGLProgram? program);
foreign import javascript unsafe "$1['useProgram']($2);"
    js_useProgram :: WebGLContext -> WebGLProgram -> IO ()

-- | void validateProgram(WebGLProgram? program);
foreign import javascript unsafe "$1['validateProgram']($2);"
    js_validateProgram :: WebGLContext -> WebGLProgram -> IO ()

-- | void vertexAttrib1f(GLuint indx, GLfloat x);
foreign import javascript unsafe "$1['vertexAttrib1f']($2, $3);"
    js_vertexAttrib1f :: WebGLContext -> GLuint -> GLfloat -> IO ()

-- | void vertexAttrib1fv(GLuint indx, Float32Array values);
-- | void vertexAttrib1fv(GLuint indx, sequence<GLfloat> values);
foreign import javascript unsafe "$1['vertexAttrib1fv']($2, $3);"
    js_vertexAttrib1fv :: WebGLContext -> GLuint -> TypedArray Float -> IO ()

-- | void vertexAttrib2f(GLuint indx, GLfloat x, GLfloat y);
foreign import javascript unsafe "$1['vertexAttrib2f']($2, $3, $4);"
    js_vertexAttrib2f :: WebGLContext -> GLuint -> GLfloat -> GLfloat -> IO ()
    
-- | void vertexAttrib2fv(GLuint indx, Float32Array values);
-- | void vertexAttrib2fv(GLuint indx, sequence<GLfloat> values);
foreign import javascript unsafe "$1['vertexAttrib2fv']($2, $3);"
    js_vertexAttrib2fv :: WebGLContext -> GLuint -> TypedArray Float -> IO ()

-- | void vertexAttrib3f(GLuint indx, GLfloat x, GLfloat y, GLfloat z);
foreign import javascript unsafe "$1['vertexAttrib3f']($2, $3, $4, $5);"
    js_vertexAttrib3f :: WebGLContext -> GLuint -> GLfloat -> GLfloat -> GLfloat -> IO ()

-- | void vertexAttrib3fv(GLuint indx, Float32Array values);
-- | void vertexAttrib3fv(GLuint indx, sequence<GLfloat> values);
foreign import javascript unsafe "$1['vertexAttrib3fv']($2, $3);"
    js_vertexAttrib3fv :: WebGLContext -> GLuint -> TypedArray Float -> IO ()

-- | void vertexAttrib4f(GLuint indx, GLfloat x, GLfloat y, GLfloat z, GLfloat w);
foreign import javascript unsafe "$1['vertexAttrib4f']($2, $3, $4, $5, $6);"
    js_vertexAttrib4f :: WebGLContext -> GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()

-- | void vertexAttrib4fv(GLuint indx, Float32Array values);
-- | void vertexAttrib4fv(GLuint indx, sequence<GLfloat> values);
foreign import javascript unsafe "$1['vertexAttrib4fv']($2, $3);"
    js_vertexAttrib4fv :: WebGLContext -> GLuint -> TypedArray Float -> IO ()

-- | void vertexAttribPointer(GLuint indx, GLint size, GLenum type, GLboolean normalized, GLsizei stride, GLintptr offset);
foreign import javascript unsafe "$1['vertexAttribPointer']($2, $3, $4, $5, $6, $7);"
    js_vertexAttribPointer :: WebGLContext -> GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLintptr -> IO ()

-- | void viewport(GLint x, GLint y, GLsizei width, GLsizei height);
foreign import javascript unsafe "$1['viewport']($2, $3, $4, $5);"
    js_setViewport :: WebGLContext -> GLint -> GLint -> GLsizei -> GLsizei -> IO ()

