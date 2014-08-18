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
foreign import javascript unsafe "$2['getExtension']($1);"
    js_getExtension :: JSString -> WebGLContext -> IO WebGLExtension

-- | void activeTexture(GLenum texture);
foreign import javascript unsafe "$2['activeTexture']($1);"
    js_activeTexture :: GLenum -> WebGLContext -> IO ()

-- |  void attachShader(WebGLProgram? program, WebGLShader? shader);
foreign import javascript unsafe "$3['attachShader']($1, $2);"
    js_attachShader :: WebGLProgram -> WebGLShader -> WebGLContext -> IO ()

-- | void bindAttribLocation(WebGLProgram? program, GLuint index, DOMString name);
foreign import javascript unsafe "$4['bindAttribLocation']($1, $2, $3);"
    js_bindAttribLocation :: WebGLProgram -> GLuint -> JSString -> WebGLContext -> IO ()

-- | void bindBuffer(GLenum target, WebGLBuffer? buffer);
foreign import javascript unsafe "$3['bindBuffer']($1, $2);"
    js_bindBuffer :: GLenum -> WebGLBuffer -> WebGLContext -> IO ()

-- | void bindFramebuffer(GLenum target, WebGLFramebuffer? framebuffer);
foreign import javascript unsafe "$3['bindFrameBuffer']($1, $2);"
    js_bindFrameBuffer :: GLenum -> WebGLFrameBuffer -> WebGLContext -> IO ()

-- | void bindRenderbuffer(GLenum target, WebGLRenderbuffer? renderbuffer);
foreign import javascript unsafe "$3['bindRenderBuffer']($1, $2);"
    js_bindRenderBuffer :: GLenum -> WebGLRenderBuffer -> WebGLContext -> IO ()

-- | void bindTexture(GLenum target, WebGLTexture? texture);
foreign import javascript unsafe "$3['bindTexture']($1, $2);"
    js_bindTexture :: GLenum -> WebGLTexture -> WebGLContext -> IO ()

-- | void bindTexture(GLenum target, null);
foreign import javascript unsafe "$2['bindTexture']($1, null);"
    js_unbindTexture :: GLenum -> WebGLContext -> IO ()

-- | void blendColor(GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha);
foreign import javascript unsafe "$5['blendColor']($1, $2, $3, $4);"
    js_blendColor :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> WebGLContext -> IO ()

-- | void blendEquation(GLenum mode);
foreign import javascript unsafe "$2['blendEquation']($1);"
    js_blendEquation :: GLenum -> WebGLContext -> IO ()

-- | void blendEquationSeparate(GLenum modeRGB, GLenum modeAlpha);
foreign import javascript unsafe "$3['blendEquationSeparate']($1, $2);"
    js_blendEquationSeparate :: GLenum -> GLenum -> WebGLContext -> IO ()

-- | void blendFunc(GLenum sfactor, GLenum dfactor);
foreign import javascript unsafe "$3['blendFunc']($1, $2);"
    js_blendFunc :: GLenum -> GLenum -> WebGLContext -> IO ()

-- | void blendFuncSeparate(GLenum srcRGB, GLenum dstRGB, GLenum srcAlpha, GLenum dstAlpha);
foreign import javascript unsafe "$5['blendFuncSeparate']($1, $2, $3, $4);"
    js_blendFuncSeparate :: GLenum -> GLenum -> GLenum -> GLenum -> WebGLContext -> IO ()

-- | void bufferData(GLenum target, GLsizeiptr size, GLenum usage);
foreign import javascript unsafe "$4['bufferData']($1, $2, $3);"
    js_bufferData_empty :: GLenum -> GLsizeiptr -> GLenum -> WebGLContext -> IO ()

-- | void bufferData(GLenum target, ArrayBufferView data, GLenum usage);
-- | void bufferData(GLenum target, ArrayBuffer? data, GLenum usage);
foreign import javascript unsafe "$4['bufferData']($1, $2, $3);"
    js_bufferData :: GLenum -> TypedArray a -> GLenum -> WebGLContext -> IO ()

-- | void bufferSubData(GLenum target, GLintptr offset, ArrayBufferView data);
-- | void bufferSubData(GLenum target, GLintptr offset, ArrayBuffer? data);
foreign import javascript unsafe "$4['bufferSubData']($1, $2, $3);"
    js_bufferSubData :: GLenum -> GLintptr -> TypedArray a -> WebGLContext -> IO ()

-- | [WebGLHandlesContextLoss] GLenum checkFramebufferStatus(GLenum target);
foreign import javascript unsafe "$2['checkFramebufferStatus']($1);"
    js_checkFramebufferStatus :: GLenum -> WebGLContext -> IO GLenum

-- | void clear(GLbitfield mask);
foreign import javascript unsafe "$2['clear']($1);"
    js_clear :: GLbitfield -> WebGLContext -> IO ()

-- | void clearColor(GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha);
foreign import javascript unsafe "$2['clearColor']($1[0], $1[1], $1[2], $1[3]);"
    js_clearColor :: TypedArray Float -> WebGLContext -> IO ()

-- | void clearDepth(GLclampf depth);
foreign import javascript unsafe "$2['clearDepth']($1);"
    js_clearDepth :: GLfloat -> WebGLContext -> IO ()

-- | void clearStencil(GLint s);
foreign import javascript unsafe "$2['clearStencil']($1);"
    js_clearStencil :: GLint -> WebGLContext -> IO ()

-- | void colorMask(GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha);
foreign import javascript unsafe "$5['colorMask']($1, $2, $3, $4);"
    js_colorMask :: GLboolean -> GLboolean -> GLboolean -> GLboolean -> WebGLContext -> IO ()

-- | void compileShader(WebGLShader? shader);
foreign import javascript unsafe "$2['compileShader']($1);"
    js_compileShader :: WebGLShader -> WebGLContext -> IO ()

{- default WebGL doesn't support compressed textures I think, so skipping these for now

-- | void compressedTexImage2D(GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLint border, ArrayBufferView data);
foreign import javascript unsafe "$8['compressedTexImage2D']($1, $2, $3, $4, $5, $6, $7);"
    js_compressedTexImage2D :: GLenum -> GLint -> GLenum -> GLsizei -> GLsizei -> GLint -> ArrayBufferView -> WebGLContext -> IO ()

-- | void compressedTexSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, ArrayBufferView data);
foreign import javascript unsafe "$9['compressedTexSubImage2D']($1, $2, $3, $4, $5, $6, $7, $8);"
    js_compressedTexSubImage2D :: GLenum -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> GLenum -> ArrayBufferView -> WebGLContext -> IO ()
-}

-- | void copyTexImage2D(GLenum target, GLint level, GLenum internalformat, GLint x, GLint y, GLsizei width, GLsizei height, GLint border);
foreign import javascript unsafe "$9['copyTexImage2D']($1, $2, $3, $4, $5, $6, $7, $8);"
    js_copyTexImage2D :: GLenum -> GLint -> GLenum -> GLint -> GLint -> GLsizei -> GLsizei -> GLint -> WebGLContext -> IO ()

-- | void copyTexSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint x, GLint y, GLsizei width, GLsizei height);
foreign import javascript unsafe "$9['copyTexSubImage2D']($1, $2, $3, $4, $5, $6, $7, $8);"
    js_copyTexSubImage2D :: GLenum -> GLint -> GLint -> GLint -> GLint -> GLint -> GLsizei -> GLsizei -> WebGLContext -> IO ()

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
foreign import javascript unsafe "$2['createShader']($1)"
    js_createShader :: GLenum -> WebGLContext -> IO WebGLShader

-- | WebGLTexture? createTexture();
foreign import javascript unsafe "$1['createTexture']()"
    js_createTexture :: WebGLContext -> IO WebGLTexture

-- | void cullFace(GLenum mode);
foreign import javascript unsafe "$2['cullFace']($1);"
    js_cullFace :: GLenum -> WebGLContext -> IO ()

-- | void deleteBuffer(WebGLBuffer? buffer);
foreign import javascript unsafe "$2['deleteBuffer']($1)"
    js_deleteBuffer :: WebGLBuffer -> WebGLContext -> IO ()

-- | void deleteFramebuffer(WebGLFramebuffer? framebuffer);
foreign import javascript unsafe "$2['deleteFrameBuffer']($1)"
    js_deleteFrameBuffer :: WebGLFrameBuffer -> WebGLContext -> IO ()

-- | void deleteProgram(WebGLProgram? program);
foreign import javascript unsafe "$2['deleteProgram']($1)"
    js_deleteProgram :: WebGLProgram -> WebGLContext -> IO ()

-- | void deleteRenderbuffer(WebGLRenderbuffer? renderbuffer);
foreign import javascript unsafe "$2['deleteRenderBuffer']($1)"
    js_deleteRenderBuffer :: WebGLRenderBuffer -> WebGLContext -> IO ()

-- | void deleteShader(WebGLShader? shader);
foreign import javascript unsafe "$2['deleteShader']($1)"
    js_deleteShader :: WebGLShader -> WebGLContext -> IO ()

-- | void deleteTexture(WebGLTexture? texture);
foreign import javascript unsafe "$2['deleteTexture']($1)"
    js_deleteTexture :: WebGLTexture -> WebGLContext -> IO ()

-- | void depthFunc(GLenum func);
foreign import javascript unsafe "$2['depthFunc']($1)"
    js_depthFunc :: GLenum -> WebGLContext -> IO ()

-- | void depthMask(GLboolean flag);
foreign import javascript unsafe "$2['depthMask']($1)"
    js_depthMask :: GLboolean -> WebGLContext -> IO ()

-- | void depthRange(GLclampf zNear, GLclampf zFar);
foreign import javascript unsafe "$3['depthRange']($1, $2)"
    js_depthRange :: GLfloat -> GLfloat -> WebGLContext -> IO ()

-- | void detachShader(WebGLProgram? program, WebGLShader? shader);
foreign import javascript unsafe "$2['detachShader']($1)"
    js_detachShader :: WebGLProgram -> WebGLShader -> WebGLContext -> IO ()

-- | void disable(GLenum cap);
foreign import javascript unsafe "$2['disable']($1);"
    js_disable :: GLenum -> WebGLContext -> IO ()

-- | void disableVertexAttribArray(GLuint index);
foreign import javascript unsafe "$2['disableVertexAttribArray']($1)"
    js_disableVertexAttribArray :: GLuint -> WebGLContext -> IO ()

-- | void drawArrays(GLenum mode, GLint first, GLsizei count);
foreign import javascript unsafe "$4['drawArrays']($1, $2, $3);"
    js_drawArrays :: GLenum -> GLint -> GLsizei -> WebGLContext -> IO ()

-- | void drawElements(GLenum mode, GLsizei count, GLenum type, GLintptr offset);
foreign import javascript unsafe "$5['drawElements']($1, $2, $3, $4);"
    js_drawElements :: GLenum -> GLsizei -> GLenum -> GLintptr -> WebGLContext -> IO ()

-- | void enable(GLenum cap);
foreign import javascript unsafe "$2['enable']($1);"
    js_enable :: GLenum -> WebGLContext -> IO ()

-- | void enableVertexAttribArray(GLuint index);
foreign import javascript unsafe "$2['enableVertexAttribArray']($1);"
    js_enableVertexAttribArray :: GLuint -> WebGLContext -> IO ()

-- | void finish();
foreign import javascript unsafe "$1['finish']();"
    js_finish :: WebGLContext -> IO ()

-- | void flush();
foreign import javascript unsafe "$1['flush']();"
    js_flush :: WebGLContext -> IO ()

-- | void framebufferRenderbuffer(GLenum target, GLenum attachment, GLenum renderbuffertarget, WebGLRenderbuffer? renderbuffer);
foreign import javascript unsafe "$5['framebufferRenderbuffer']($1, $2, $3, $4);"
    js_framebufferRenderbuffer :: GLenum -> GLenum -> GLenum -> WebGLRenderBuffer -> WebGLContext -> IO ()

-- | void framebufferTexture2D(GLenum target, GLenum attachment, GLenum textarget, WebGLTexture? texture, GLint level);
foreign import javascript unsafe "$6['framebufferTexture2D']($1, $2, $3, $4, $5);"
    js_framebufferTexture2D :: GLenum -> GLenum -> GLenum -> WebGLTexture -> GLint -> WebGLContext -> IO ()

-- | void frontFace(GLenum mode);
foreign import javascript unsafe "$2['frontFace']($1);"
    js_frontFace :: GLenum -> WebGLContext -> IO ()

-- | void generateMipmap(GLenum target);
foreign import javascript unsafe "$2['generateMipmap']($1);"
    js_generateMipmap :: GLenum -> WebGLContext -> IO ()

-- | WebGLActiveInfo? getActiveAttrib(WebGLProgram? program, GLuint index);
foreign import javascript unsafe "$3['getActiveAttrib']($1, $2);"
    js_getActiveAttrib :: WebGLProgram -> GLuint -> WebGLContext -> IO WebGLActiveInfo

-- | WebGLActiveInfo? getActiveUniform(WebGLProgram? program, GLuint index);
foreign import javascript unsafe "$3['getActiveUniform']($1, $2);"
    js_getActiveUniform :: WebGLProgram -> GLuint -> WebGLContext -> IO WebGLActiveInfo

-- | sequence<WebGLShader>? getAttachedShaders(WebGLProgram? program);
foreign import javascript unsafe "$2['getAttachedShaders']($1);"
    js_getAttachedShaders :: WebGLProgram -> WebGLContext -> IO (JSArray WebGLShader)

-- | [WebGLHandlesContextLoss] GLint getAttribLocation(WebGLProgram? program, DOMString name);
foreign import javascript unsafe "$3['getAttribLocation']($1, $2)"
    js_getAttribLocation :: WebGLProgram -> JSString -> WebGLContext -> IO GLint

-- | any getBufferParameter(GLenum target, GLenum pname);
foreign import javascript unsafe "$2['getBufferParameter']($1, $2['BUFFER_SIZE'])"
    js_getBufferSize :: GLenum -> WebGLContext -> IO GLint
foreign import javascript unsafe "$2['getBufferParameter']($1, $2['BUFFER_USAGE'])"
    js_getBufferUsage :: GLenum -> WebGLContext -> IO GLenum

-- | any getParameter(GLenum pname);
-- this is so terrible
foreign import javascript unsafe "$2['getParameter']($1)" js_getParameter_GLboolean :: GLenum -> WebGLContext -> IO GLboolean
foreign import javascript unsafe "$2['getParameter']($1)" js_getParameter_GLenum :: GLenum -> WebGLContext -> IO GLenum
foreign import javascript unsafe "$2['getParameter']($1)" js_getParameter_GLuint :: GLenum -> WebGLContext -> IO GLuint
foreign import javascript unsafe "$2['getParameter']($1)" js_getParameter_GLint :: GLenum -> WebGLContext -> IO GLint
foreign import javascript unsafe "$2['getParameter']($1)" js_getParameter_GLfloat :: GLenum -> WebGLContext -> IO GLfloat

foreign import javascript unsafe "$2['getParameter']($1)" js_getParameter_string :: GLenum -> WebGLContext -> IO JSString

foreign import javascript unsafe "$2['getParameter']($1)" js_getParameter_GLbooleanArray :: GLenum -> WebGLContext -> IO (JSArray GLboolean)

foreign import javascript unsafe "$2['getParameter']($1)" js_getParameter_Uint32Array :: GLenum -> WebGLContext -> IO (TypedArray Word32)
foreign import javascript unsafe "$2['getParameter']($1)" js_getParameter_Int32Array :: GLenum -> WebGLContext -> IO (TypedArray Int32)
foreign import javascript unsafe "$2['getParameter']($1)" js_getParameter_Float32Array :: GLenum -> WebGLContext -> IO (TypedArray Float)

foreign import javascript unsafe "$2['getParameter']($1)" js_getParameter_buffer :: GLenum -> WebGLContext -> IO WebGLBuffer
foreign import javascript unsafe "$2['getParameter']($1)" js_getParameter_framebuffer :: GLenum -> WebGLContext -> IO WebGLFrameBuffer
foreign import javascript unsafe "$2['getParameter']($1)" js_getParameter_renderbuffer :: GLenum -> WebGLContext -> IO WebGLRenderBuffer
foreign import javascript unsafe "$2['getParameter']($1)" js_getParameter_program :: GLenum -> WebGLContext -> IO WebGLProgram
foreign import javascript unsafe "$2['getParameter']($1)" js_getParameter_texture :: GLenum -> WebGLContext -> IO WebGLTexture


-- | [WebGLHandlesContextLoss] GLenum getError();
foreign import javascript unsafe "$1['getError']();"
    js_getError :: WebGLContext -> IO GLenum

-- | any getFramebufferAttachmentParameter(GLenum target, GLenum attachment, GLenum pname);
foreign import javascript unsafe "$3['getFramebufferAttachmentParameter']($1, $2, $3['FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE'])"
    js_getFramebufferAttachmentParameter_objType :: GLenum -> GLenum -> WebGLContext -> IO GLint
--  foreign import javascript unsafe "$3['getFramebufferAttachmentParameter']($1, $2, $3['FRAMEBUFFER_ATTACHMENT_OBJECT_NAME'])"
    --  js_getFramebufferAttachmentParameter_objName :: GLenum -> GLenum -> WebGLContext -> IO ??
    -- this can be either a WebGLRenderbuffer or WebGLTexture apparently?
foreign import javascript unsafe "$3['getFramebufferAttachmentParameter']($1, $2, $3['FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL'])"
    js_getFramebufferAttachmentParameter_texLevel :: GLenum -> GLenum -> WebGLContext -> IO GLint
foreign import javascript unsafe "$3['getFramebufferAttachmentParameter']($1, $2, $3['FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE'])"
    js_getFramebufferAttachmentParameter_cubeFace :: GLenum -> GLenum -> WebGLContext -> IO GLint

-- | any getProgramParameter(WebGLProgram? program, GLenum pname);
foreign import javascript unsafe "$2['getProgramParameter']($1, $2['DELETE_STATUS'])"
    js_getProgramParameter_delete :: WebGLProgram -> WebGLContext -> IO GLboolean
foreign import javascript unsafe "$2['getProgramParameter']($1, $2['LINK_STATUS'])"
    js_getProgramParameter_link :: WebGLProgram -> WebGLContext -> IO GLboolean
foreign import javascript unsafe "$2['getProgramParameter']($1, $2['VALIDATE_STATUS'])"
    js_getProgramParameter_validate :: WebGLProgram -> WebGLContext -> IO GLboolean
foreign import javascript unsafe "$2['getProgramParameter']($1, $2['ATTACHED_SHADERS'])"
    js_getProgramParameter_shaders :: WebGLProgram -> WebGLContext -> IO GLint
foreign import javascript unsafe "$2['getProgramParameter']($1, $2['ATTACHED_ATTRIBUTES'])"
    js_getProgramParameter_attributes :: WebGLProgram -> WebGLContext -> IO GLint
foreign import javascript unsafe "$2['getProgramParameter']($1, $2['ATTACHED_UNIFORMS'])"
    js_getProgramParameter_uniforms :: WebGLProgram -> WebGLContext -> IO GLint

-- | DOMString? getProgramInfoLog(WebGLProgram? program);
foreign import javascript unsafe "$2['getProgramInfoLog']($1)"
    js_getProgramInfoLog :: WebGLProgram -> WebGLContext -> IO JSString

-- | any getRenderbufferParameter(GLenum target, GLenum pname);
foreign import javascript unsafe "$2['getRenderbufferParameter']($1, $2['RENDERBUFFER_WIDTH'])"
    js_getRenderbufferParameter_width :: GLenum -> WebGLContext -> IO GLint
foreign import javascript unsafe "$2['getRenderbufferParameter']($1, $2['RENDERBUFFER_HEIGHT'])"
    js_getRenderbufferParameter_height :: GLenum -> WebGLContext -> IO GLint
foreign import javascript unsafe "$2['getRenderbufferParameter']($1, $2['RENDERBUFFER_RED_SIZE'])"
    js_getRenderbufferParameter_redSize :: GLenum -> WebGLContext -> IO GLint
foreign import javascript unsafe "$2['getRenderbufferParameter']($1, $2['RENDERBUFFER_GREEN_SIZE'])"
    js_getRenderbufferParameter_greenSize :: GLenum -> WebGLContext -> IO GLint
foreign import javascript unsafe "$2['getRenderbufferParameter']($1, $2['RENDERBUFFER_BLUE_SIZE'])"
    js_getRenderbufferParameter_blueSize :: GLenum -> WebGLContext -> IO GLint
foreign import javascript unsafe "$2['getRenderbufferParameter']($1, $2['RENDERBUFFER_ALPHA_SIZE'])"
    js_getRenderbufferParameter_alphaSize :: GLenum -> WebGLContext -> IO GLint
foreign import javascript unsafe "$2['getRenderbufferParameter']($1, $2['RENDERBUFFER_DEPTH_SIZE'])"
    js_getRenderbufferParameter_depthSize :: GLenum -> WebGLContext -> IO GLint
foreign import javascript unsafe "$2['getRenderbufferParameter']($1, $2['RENDERBUFFER_STENCIL_SIZE'])"
    js_getRenderbufferParameter_stencilSize :: GLenum -> WebGLContext -> IO GLint
foreign import javascript unsafe "$2['getRenderbufferParameter']($1, $2['RENDERBUFFER_INTERNAL_FORMAT'])"
    js_getRenderbufferParameter_format :: GLenum -> WebGLContext -> IO GLenum

-- | any getShaderParameter(WebGLShader? shader, GLenum pname);
foreign import javascript unsafe "$2['getShaderParameter']($1, $2['SHADER_TYPE'])"
    js_getShaderParameter_shaderType :: WebGLShader -> WebGLContext -> IO GLenum
foreign import javascript unsafe "$2['getShaderParameter']($1, $2['DELETE_STATUS'])"
    js_getShaderParameter_deleteStatus :: WebGLShader -> WebGLContext -> IO GLboolean
foreign import javascript unsafe "$2['getShaderParameter']($1, $2['COMPILE_STATUS'])"
    js_getShaderParameter_compileStatus :: WebGLShader -> WebGLContext -> IO GLboolean

-- | WebGLShaderPrecisionFormat? getShaderPrecisionFormat(GLenum shadertype, GLenum precisiontype);
foreign import javascript unsafe "$3['getShaderPrecisionFormat']($1, $2)"
    js_getShaderPrecisionFormat :: GLenum -> GLenum -> WebGLContext -> IO WebGLShaderPrecisionFormat

-- | DOMString? getShaderInfoLog(WebGLShader? shader);
foreign import javascript unsafe "$2['getShaderInfoLog']($1)"
    js_getShaderInfoLog :: WebGLShader -> WebGLContext -> IO JSString

-- | DOMString? getShaderSource(WebGLShader? shader);
foreign import javascript unsafe "$2['getShaderSource']($1)"
    js_getShaderSource :: WebGLShader -> WebGLContext -> IO JSString

-- | any getTexParameter(GLenum target, GLenum pname);
foreign import javascript unsafe "$2['getTexParameter']($1, $2['TEXTURE_MAG_FILTER'])"
    js_getTexParameter_magFilter :: GLenum -> WebGLContext -> IO GLenum
foreign import javascript unsafe "$2['getTexParameter']($1, $2['TEXTURE_MIN_FILTER'])"
    js_getTexParameter_minFilter :: GLenum -> WebGLContext -> IO GLenum
foreign import javascript unsafe "$2['getTexParameter']($1, $2['TEXTURE_WRAP_S'])"
    js_getTexParameter_wrapS :: GLenum -> WebGLContext -> IO GLenum
foreign import javascript unsafe "$2['getTexParameter']($1, $2['TEXTURE_WRAP_T'])"
    js_getTexParameter_wrapT :: GLenum -> WebGLContext -> IO GLenum

-- | any getUniform(WebGLProgram? program, WebGLUniformLocation? location);
foreign import javascript unsafe "$3['getUniform']($1, $2)"
    js_getUniform :: WebGLProgram -> WebGLUniformLocation -> WebGLContext -> IO WebGLUniformValue

-- | WebGLUniformLocation? getUniformLocation(WebGLProgram? program, DOMString name);
foreign import javascript unsafe "$3['getUniformLocation']($1, $2)"
    js_getUniformLocation :: WebGLProgram -> JSString -> WebGLContext -> IO WebGLUniformLocation

-- | any getVertexAttrib(GLuint index, GLenum pname);

-- | [WebGLHandlesContextLoss] GLsizeiptr getVertexAttribOffset(GLuint index, GLenum pname);
foreign import javascript unsafe "$3['getVertexAttribOffset']($1, $2)"
    js_getVertexAttribOffset :: GLuint -> GLenum -> WebGLContext -> IO GLsizeiptr

-- | void hint(GLenum target, GLenum mode);
foreign import javascript unsafe "$3['hint']($1, $2);"
    js_hint :: GLenum -> GLenum -> WebGLContext -> IO ()

-- | [WebGLHandlesContextLoss] GLboolean isBuffer(WebGLBuffer? buffer);

-- | [WebGLHandlesContextLoss] GLboolean isEnabled(GLenum cap);

-- | [WebGLHandlesContextLoss] GLboolean isFramebuffer(WebGLFramebuffer? framebuffer);

-- | [WebGLHandlesContextLoss] GLboolean isProgram(WebGLProgram? program);

-- | [WebGLHandlesContextLoss] GLboolean isRenderbuffer(WebGLRenderbuffer? renderbuffer);

-- | [WebGLHandlesContextLoss] GLboolean isShader(WebGLShader? shader);

-- | [WebGLHandlesContextLoss] GLboolean isTexture(WebGLTexture? texture);

-- | void lineWidth(GLfloat width);
foreign import javascript unsafe "$2['lineWidth']($1);"
    js_lineWidth :: GLfloat -> WebGLContext -> IO ()

-- | void linkProgram(WebGLProgram? program);
foreign import javascript unsafe "$2['linkProgram']($1);"
    js_linkProgram :: WebGLProgram -> WebGLContext -> IO ()

-- | void pixelStorei(GLenum pname, GLint param);
foreign import javascript unsafe "$3['pixelStorei']($1, $2);"
    js_pixelStorei :: GLenum -> GLint -> WebGLContext -> IO ()

--  foreign import javascript unsafe "$3['pixelStorei']($1, $2 ? 1 : 0);"
    --  js_pixelStorei_bool :: GLenum -> GLboolean -> WebGLContext -> IO ()

-- | void polygonOffset(GLfloat factor, GLfloat units);
foreign import javascript unsafe "$3['polygonOffset']($1, $2);"
    js_polygonOffset :: GLfloat -> GLfloat -> WebGLContext -> IO ()

-- | void readPixels(GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type, ArrayBufferView? pixels);

-- | void renderbufferStorage(GLenum target, GLenum internalformat, GLsizei width, GLsizei height);
-- | void sampleCoverage(GLclampf value, GLboolean invert);
-- | void scissor(GLint x, GLint y, GLsizei width, GLsizei height);
foreign import javascript unsafe "$3['scissor']($1, $2);"
    js_scissor :: GLint -> GLint -> GLsizei -> GLsizei -> WebGLContext -> IO ()

-- | void shaderSource(WebGLShader? shader, DOMString source);
foreign import javascript unsafe "$3['shaderSource']($1, $2);"
    js_shaderSource :: WebGLShader -> JSString -> WebGLContext -> IO ()

-- | void stencilFunc(GLenum func, GLint ref, GLuint mask);
-- | void stencilFuncSeparate(GLenum face, GLenum func, GLint ref, GLuint mask);
-- | void stencilMask(GLuint mask);
-- | void stencilMaskSeparate(GLenum face, GLuint mask);
-- | void stencilOp(GLenum fail, GLenum zfail, GLenum zpass);
-- | void stencilOpSeparate(GLenum face, GLenum fail, GLenum zfail, GLenum zpass);

-- | void texImage2D(GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLint border, GLenum format, GLenum type, ArrayBufferView? pixels);
-- | void texImage2D(GLenum target, GLint level, GLenum internalformat, GLenum format, GLenum type, TexImageSource? source);
foreign import javascript unsafe "$7['texImage2D']($1, $2, $3, $4, $5, $6);"
    js_texImage2D :: GLenum -> GLint -> GLenum -> GLenum -> GLenum -> Image -> WebGLContext -> IO ()

-- | void texParameterf(GLenum target, GLenum pname, GLfloat param);
foreign import javascript unsafe "$4['texParameterf']($1, $2, $3);"
    js_texParameterf :: GLenum -> GLenum -> GLfloat -> WebGLContext -> IO ()

-- | void texParameteri(GLenum target, GLenum pname, GLint param);
foreign import javascript unsafe "$4['texParameteri']($1, $2, $3);"
    js_texParameteri :: GLenum -> GLenum -> GLint -> WebGLContext -> IO ()

foreign import javascript unsafe "$4['texParameteri']($1, $2, $3);"
    js_texParameteri_enum :: GLenum -> GLenum -> GLenum -> WebGLContext -> IO ()

-- | void texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLenum type, ArrayBufferView? pixels);
-- | void texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLenum format, GLenum type, ImageData? pixels);
-- | void texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLenum format, GLenum type, HTMLImageElement image); // May throw DOMException
-- | void texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLenum format, GLenum type, HTMLCanvasElement canvas); // May throw DOMException
-- | void texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLenum format, GLenum type, HTMLVideoElement video); // May throw DOMException
foreign import javascript unsafe "$7['texSubImage2D']($1, $2, $3, $4, $5, $6);"
    js_texSubImage2D :: GLenum -> GLint -> GLint -> GLint -> GLenum -> GLenum -> Image -> WebGLContext -> IO ()

-- | void uniform1f(WebGLUniformLocation? location, GLfloat x);
foreign import javascript unsafe "$3['uniform1f']($1, $2);"
    js_uniform1f :: WebGLUniformLocation -> GLfloat -> WebGLContext -> IO ()

-- | void uniform1fv(WebGLUniformLocation? location, Float32Array v);
-- | void uniform1fv(WebGLUniformLocation? location, sequence<GLfloat> v);
foreign import javascript unsafe "$3['uniform1fv']($1, $2);"
    js_uniform1fv :: WebGLUniformLocation -> TypedArray Float -> WebGLContext -> IO ()

-- | void uniform1i(WebGLUniformLocation? location, GLint x);
foreign import javascript unsafe "$3['uniform1i']($1, $2);"
    js_uniform1i :: WebGLUniformLocation -> GLint -> WebGLContext -> IO ()

-- | void uniform1iv(WebGLUniformLocation? location, Int32Array v);
-- | void uniform1iv(WebGLUniformLocation? location, sequence<long> v);
foreign import javascript unsafe "$3['uniform1iv']($1, $2);"
    js_uniform1iv :: WebGLUniformLocation -> TypedArray Int32 -> WebGLContext -> IO ()

-- | void uniform2f(WebGLUniformLocation? location, GLfloat x, GLfloat y);
foreign import javascript unsafe "$4['uniform2f']($1, $2, $3);"
    js_uniform2f :: WebGLUniformLocation -> GLfloat -> GLfloat -> WebGLContext -> IO ()

-- | void uniform2fv(WebGLUniformLocation? location, Float32Array v);
-- | void uniform2fv(WebGLUniformLocation? location, sequence<GLfloat> v);
foreign import javascript unsafe "$3['uniform2fv']($1, $2);"
    js_uniform2fv :: WebGLUniformLocation -> TypedArray Float -> WebGLContext -> IO ()

-- | void uniform2i(WebGLUniformLocation? location, GLint x, GLint y);
foreign import javascript unsafe "$4['uniform2i']($1, $2, $3);"
    js_uniform2i :: WebGLUniformLocation -> GLint -> GLint -> WebGLContext -> IO ()

-- | void uniform2iv(WebGLUniformLocation? location, Int32Array v);
-- | void uniform2iv(WebGLUniformLocation? location, sequence<long> v);
foreign import javascript unsafe "$3['uniform2iv']($1, $2);"
    js_uniform2iv :: WebGLUniformLocation -> TypedArray Int32 -> WebGLContext -> IO ()

-- | void uniform3f(WebGLUniformLocation? location, GLfloat x, GLfloat y, GLfloat z);
foreign import javascript unsafe "$5['uniform3f']($1, $2, $3, $4);"
    js_uniform3f :: WebGLUniformLocation -> GLfloat -> GLfloat -> GLfloat -> WebGLContext -> IO ()

-- | void uniform3fv(WebGLUniformLocation? location, Float32Array v);
-- | void uniform3fv(WebGLUniformLocation? location, sequence<GLfloat> v);
foreign import javascript unsafe "$3['uniform3fv']($1, $2);"
    js_uniform3fv :: WebGLUniformLocation -> TypedArray Float -> WebGLContext -> IO ()

-- | void uniform3i(WebGLUniformLocation? location, GLint x, GLint y, GLint z);
foreign import javascript unsafe "$5['uniform3i']($1, $2, $3, $4);"
    js_uniform3i :: WebGLUniformLocation -> GLint -> GLint -> GLint -> WebGLContext -> IO ()

-- | void uniform3iv(WebGLUniformLocation? location, Int32Array v);
-- | void uniform3iv(WebGLUniformLocation? location, sequence<long> v);
foreign import javascript unsafe "$3['uniform3iv']($1, $2);"
    js_uniform3iv :: WebGLUniformLocation -> TypedArray Int32 -> WebGLContext -> IO ()

-- | void uniform4f(WebGLUniformLocation? location, GLfloat x, GLfloat y, GLfloat z, GLfloat w);
foreign import javascript unsafe "$6['uniform4f']($1, $2, $3, $4, $5);"
    js_uniform4f :: WebGLUniformLocation -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> WebGLContext -> IO ()


-- | void uniform4fv(WebGLUniformLocation? location, Float32Array v);
-- | void uniform4fv(WebGLUniformLocation? location, sequence<GLfloat> v);
foreign import javascript unsafe "$3['uniform4fv']($1, $2);"
    js_uniform4fv :: WebGLUniformLocation -> TypedArray Float -> WebGLContext -> IO ()
    
-- | void uniform4i(WebGLUniformLocation? location, GLint x, GLint y, GLint z, GLint w);
foreign import javascript unsafe "$5['uniform4i']($1, $2, $3, $4);"
    js_uniform4i :: WebGLUniformLocation -> GLint -> GLint -> GLint -> GLint -> WebGLContext -> IO ()

-- | void uniform4iv(WebGLUniformLocation? location, Int32Array v);
-- | void uniform4iv(WebGLUniformLocation? location, sequence<long> v);
foreign import javascript unsafe "$3['uniform4iv']($1, $2);"
    js_uniform4iv :: WebGLUniformLocation -> TypedArray Int32 -> WebGLContext -> IO ()

-- | void uniformMatrix2fv(WebGLUniformLocation? location, GLboolean transpose, Float32Array value);
-- | void uniformMatrix2fv(WebGLUniformLocation? location, GLboolean transpose, sequence<GLfloat> value);
foreign import javascript unsafe "$4['uniformMatrix2fv']($1, $2, $3);"
    js_uniformMatrix2fv :: WebGLUniformLocation -> GLboolean -> TypedArray Float -> WebGLContext -> IO ()

-- | void uniformMatrix3fv(WebGLUniformLocation? location, GLboolean transpose, Float32Array value);
-- | void uniformMatrix3fv(WebGLUniformLocation? location, GLboolean transpose, sequence<GLfloat> value);
foreign import javascript unsafe "$4['uniformMatrix3fv']($1, $2, $3);"
    js_uniformMatrix3fv :: WebGLUniformLocation -> GLboolean -> TypedArray Float -> WebGLContext -> IO ()

-- | void uniformMatrix4fv(WebGLUniformLocation? location, GLboolean transpose, Float32Array value);
-- | void uniformMatrix4fv(WebGLUniformLocation? location, GLboolean transpose, sequence<GLfloat> value);
foreign import javascript unsafe "$4['uniformMatrix4fv']($1, $2, $3);"
    js_uniformMatrix4fv :: WebGLUniformLocation -> GLboolean -> TypedArray Float -> WebGLContext -> IO ()

-- | void useProgram(WebGLProgram? program);
foreign import javascript unsafe "$2['useProgram']($1);"
    js_useProgram :: WebGLProgram -> WebGLContext -> IO ()

-- | void validateProgram(WebGLProgram? program);
foreign import javascript unsafe "$2['validateProgram']($1);"
    js_validateProgram :: WebGLProgram -> WebGLContext -> IO ()

-- | void vertexAttrib1f(GLuint indx, GLfloat x);
foreign import javascript unsafe "$3['vertexAttrib1f']($1, $2);"
    js_vertexAttrib1f :: GLuint -> GLfloat -> WebGLContext -> IO ()

-- | void vertexAttrib1fv(GLuint indx, Float32Array values);
-- | void vertexAttrib1fv(GLuint indx, sequence<GLfloat> values);
foreign import javascript unsafe "$3['vertexAttrib1fv']($1, $2);"
    js_vertexAttrib1fv :: GLuint -> TypedArray Float -> WebGLContext -> IO ()

-- | void vertexAttrib2f(GLuint indx, GLfloat x, GLfloat y);
foreign import javascript unsafe "$4['vertexAttrib2f']($1, $2, $3);"
    js_vertexAttrib2f :: GLuint -> GLfloat -> GLfloat -> WebGLContext -> IO ()
    
-- | void vertexAttrib2fv(GLuint indx, Float32Array values);
-- | void vertexAttrib2fv(GLuint indx, sequence<GLfloat> values);
foreign import javascript unsafe "$3['vertexAttrib2fv']($1, $2);"
    js_vertexAttrib2fv :: GLuint -> TypedArray Float -> WebGLContext -> IO ()

-- | void vertexAttrib3f(GLuint indx, GLfloat x, GLfloat y, GLfloat z);
foreign import javascript unsafe "$5['vertexAttrib3f']($1, $2, $3, $4);"
    js_vertexAttrib3f :: GLuint -> GLfloat -> GLfloat -> GLfloat -> WebGLContext -> IO ()

-- | void vertexAttrib3fv(GLuint indx, Float32Array values);
-- | void vertexAttrib3fv(GLuint indx, sequence<GLfloat> values);
foreign import javascript unsafe "$3['vertexAttrib3fv']($1, $2);"
    js_vertexAttrib3fv :: GLuint -> TypedArray Float -> WebGLContext -> IO ()

-- | void vertexAttrib4f(GLuint indx, GLfloat x, GLfloat y, GLfloat z, GLfloat w);
foreign import javascript unsafe "$6['vertexAttrib4f']($1, $2, $3, $4, $5);"
    js_vertexAttrib4f :: GLuint -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> WebGLContext -> IO ()

-- | void vertexAttrib4fv(GLuint indx, Float32Array values);
-- | void vertexAttrib4fv(GLuint indx, sequence<GLfloat> values);
foreign import javascript unsafe "$3['vertexAttrib4fv']($1, $2);"
    js_vertexAttrib4fv :: GLuint -> TypedArray Float -> WebGLContext -> IO ()

-- | void vertexAttribPointer(GLuint indx, GLint size, GLenum type, GLboolean normalized, GLsizei stride, GLintptr offset);
foreign import javascript unsafe "$7['vertexAttribPointer']($1, $2, $3, $4, $5, $6);"
    js_vertexAttribPointer :: GLuint -> GLint -> GLenum -> GLboolean -> GLsizei -> GLintptr -> WebGLContext -> IO ()

-- | void viewport(GLint x, GLint y, GLsizei width, GLsizei height);
foreign import javascript unsafe "$5['viewport']($1, $2, $3, $4);"
    js_setViewport :: GLint -> GLint -> GLsizei -> GLsizei -> WebGLContext -> IO ()

