module Graphics.Rendering.WebGL.EnumConvert where

import Data.Bits
import Data.Monoid
import Graphics.Rendering.WebGL.Types
import Graphics.Rendering.WebGL.Constants

class GLEnum a where
    toGLEnum :: a -> GLenum
    fromGLEnum :: GLenum -> Maybe a


newtype TextureUnit = TextureUnit GLuint
  deriving (Eq, Ord, Read, Show)

instance GLEnum TextureUnit where
    toGLEnum (TextureUnit n) = gl_TEXTURE0 + n
    fromGLEnum e 
        | e < gl_TEXTURE0        = Nothing
        | e > gl_TEXTURE31       = Nothing
        | otherwise              = Just $ TextureUnit (e - gl_TEXTURE0)

data TextureTarget = Texture2D
                   | TextureCubePosX
                   | TextureCubeNegX
                   | TextureCubePosY
                   | TextureCubeNegY
                   | TextureCubePosZ
                   | TextureCubeNegZ
  deriving (Eq, Ord, Read, Show)

instance GLEnum TextureTarget where
    toGLEnum Texture2D = gl_TEXTURE_2D
    toGLEnum TextureCubePosX = gl_TEXTURE_CUBE_MAP_POSITIVE_X
    toGLEnum TextureCubeNegX = gl_TEXTURE_CUBE_MAP_NEGATIVE_X
    toGLEnum TextureCubePosY = gl_TEXTURE_CUBE_MAP_POSITIVE_Y
    toGLEnum TextureCubeNegY = gl_TEXTURE_CUBE_MAP_NEGATIVE_Y
    toGLEnum TextureCubePosZ = gl_TEXTURE_CUBE_MAP_POSITIVE_Z
    toGLEnum TextureCubeNegZ = gl_TEXTURE_CUBE_MAP_NEGATIVE_Z
    fromGLEnum e
        | e == gl_TEXTURE_CUBE_MAP_POSITIVE_X = Just TextureCubePosX
        | e == gl_TEXTURE_CUBE_MAP_NEGATIVE_X = Just TextureCubeNegX 
        | e == gl_TEXTURE_CUBE_MAP_POSITIVE_Y = Just TextureCubePosY
        | e == gl_TEXTURE_CUBE_MAP_NEGATIVE_Y = Just TextureCubeNegY
        | e == gl_TEXTURE_CUBE_MAP_POSITIVE_Z = Just TextureCubePosZ
        | e == gl_TEXTURE_CUBE_MAP_NEGATIVE_Z = Just TextureCubeNegZ
        | otherwise                           = Nothing

data BufferTarget = ArrayBuffer | ElementArrayBuffer
  deriving (Eq, Ord, Read, Show)

instance GLEnum BufferTarget where
    toGLEnum ArrayBuffer = gl_ARRAY_BUFFER
    toGLEnum ElementArrayBuffer = gl_ELEMENT_ARRAY_BUFFER
    fromGLEnum e
        | e == gl_ARRAY_BUFFER         = Just ArrayBuffer
        | e == gl_ELEMENT_ARRAY_BUFFER = Just ElementArrayBuffer
        | otherwise                    = Nothing

data TextureType = TextureType2D | TextureTypeCube
  deriving (Eq, Ord, Read, Show)

instance GLEnum TextureType where
    toGLEnum TextureType2D = gl_TEXTURE_2D
    toGLEnum TextureTypeCube = gl_TEXTURE_CUBE_MAP
    fromGLEnum e
        | e == gl_TEXTURE_2D       = Just TextureType2D
        | e == gl_TEXTURE_CUBE_MAP = Just TextureTypeCube
        | otherwise                = Nothing


data BlendEquation = BlendAdd | BlendSubtract | BlendReverseSubtract
  deriving (Eq, Ord, Read, Show)

instance GLEnum BlendEquation where
    toGLEnum BlendAdd = gl_FUNC_ADD
    toGLEnum BlendSubtract = gl_FUNC_SUBTRACT
    toGLEnum BlendReverseSubtract = gl_FUNC_REVERSE_SUBTRACT
    fromGLEnum e
        | e == gl_FUNC_ADD              = Just BlendAdd
        | e == gl_FUNC_SUBTRACT         = Just BlendSubtract
        | e == gl_FUNC_REVERSE_SUBTRACT = Just BlendReverseSubtract
        | otherwise                     = Nothing

data BlendFunction -- TODO
-- e.g., zero, one, src_color, one_minus_src_color, etc
instance GLEnum BlendFunction -- TODO

data BufferUsage = StaticDraw | DynamicDraw | StreamDraw
  deriving (Eq, Ord, Read, Show)

instance GLEnum BufferUsage where
    toGLEnum StaticDraw = gl_STATIC_DRAW
    toGLEnum DynamicDraw = gl_DYNAMIC_DRAW
    toGLEnum StreamDraw = gl_STREAM_DRAW
    fromGLEnum e
        | e == gl_STATIC_DRAW  = Just StaticDraw
        | e == gl_DYNAMIC_DRAW = Just DynamicDraw
        | e == gl_STREAM_DRAW  = Just StreamDraw
        | otherwise            = Nothing


newtype ClearBufferMask = CBM { getCBM :: GLenum }
instance Monoid ClearBufferMask where
    mempty = CBM 0
    mappend (CBM x) (CBM y) = CBM $ x .|. y

depthBufferMask = CBM gl_DEPTH_BUFFER_BIT

stencilBufferMask = CBM gl_STENCIL_BUFFER_BIT

colorBufferMask = CBM gl_COLOR_BUFFER_BIT

allBufferMask = depthBufferMask <> stencilBufferMask <> colorBufferMask

instance GLEnum ClearBufferMask where
    toGLEnum (CBM x) = x
    fromGLEnum e 
        | e' == 0   = Just $ CBM e
        | otherwise = Nothing
      where e' = e .&. complement (getCBM allBufferMask)

data PixelFormat = Alpha | RGB | RGBA | Luminance | LuminanceAlpha
    deriving (Eq, Ord, Read, Show)

instance GLEnum PixelFormat where
    toGLEnum Alpha = gl_ALPHA
    toGLEnum RGB = gl_RGB
    toGLEnum RGBA = gl_RGBA
    toGLEnum Luminance = gl_LUMINANCE
    toGLEnum LuminanceAlpha = gl_LUMINANCE_ALPHA
    fromGLEnum e
        | e == gl_ALPHA           = Just Alpha
        | e == gl_RGB             = Just RGB
        | e == gl_RGBA            = Just RGBA
        | e == gl_LUMINANCE       = Just Luminance
        | e == gl_LUMINANCE_ALPHA = Just LuminanceAlpha
        | otherwise               = Nothing

data PixelTypeFormat = RGBA8888 | RGB888 | RGBA4444 | RGBA5551 | RGB565
                     | LumA88 | Lum8 | A8
  deriving (Eq, Ord, Read, Show)

getFormatEnum RGBA8888 = gl_RGBA
getFormatEnum RGB888   = gl_RGB
getFormatEnum RGBA4444 = gl_RGBA
getFormatEnum RGBA5551 = gl_RGBA
getFormatEnum RGB565   = gl_RGB
getFormatEnum LumA88   = gl_LUMINANCE_ALPHA
getFormatEnum Lum8     = gl_LUMINANCE
getFormatEnum A8       = gl_ALPHA

getTypeEnum RGBA8888 = gl_UNSIGNED_BYTE
getTypeEnum RGB888   = gl_UNSIGNED_BYTE
getTypeEnum RGBA4444 = gl_UNSIGNED_SHORT_4_4_4_4
getTypeEnum RGBA5551 = gl_UNSIGNED_SHORT_5_5_5_1
getTypeEnum RGB565   = gl_UNSIGNED_SHORT_5_6_5
getTypeEnum LumA88   = gl_UNSIGNED_BYTE
getTypeEnum Lum8     = gl_UNSIGNED_BYTE
getTypeEnum A8       = gl_UNSIGNED_BYTE

data PixelStoreFlag = UnpackFlipY | UnpackPremultiplyAlpha | UnpackColorspaceConversion
    deriving (Eq, Ord, Read, Show)

instance GLEnum PixelStoreFlag where
    toGLEnum UnpackFlipY = gl_UNPACK_FLIP_Y_WEBGL
    toGLEnum UnpackPremultiplyAlpha = gl_UNPACK_PREMULTIPLY_ALPHA_WEBGL
    toGLEnum UnpackColorspaceConversion = gl_UNPACK_COLORSPACE_CONVERSION_WEBGL
    fromGLEnum e
        | e == gl_UNPACK_FLIP_Y_WEBGL                = Just UnpackFlipY
        | e == gl_UNPACK_PREMULTIPLY_ALPHA_WEBGL     = Just UnpackPremultiplyAlpha
        | e == gl_UNPACK_COLORSPACE_CONVERSION_WEBGL = Just UnpackColorspaceConversion
        | otherwise                                  = Nothing

data PixelStoreParam = UnpackAlignment
    deriving (Eq, Ord, Read, Show)

instance GLEnum PixelStoreParam where
    toGLEnum UnpackAlignment = gl_UNPACK_ALIGNMENT
    fromGLEnum e
        | e == gl_UNPACK_ALIGNMENT = Just UnpackAlignment
        | otherwise                = Nothing










