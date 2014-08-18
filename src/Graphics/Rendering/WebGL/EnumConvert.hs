{-# LANGUAGE TypeSynonymInstances #-}
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

data TextureTarget = TextureTarget2D
                   | TextureCubePosX
                   | TextureCubeNegX
                   | TextureCubePosY
                   | TextureCubeNegY
                   | TextureCubePosZ
                   | TextureCubeNegZ
  deriving (Eq, Ord, Read, Show)

instance GLEnum TextureTarget where
    toGLEnum TextureTarget2D = gl_TEXTURE_2D
    toGLEnum TextureCubePosX = gl_TEXTURE_CUBE_MAP_POSITIVE_X
    toGLEnum TextureCubeNegX = gl_TEXTURE_CUBE_MAP_NEGATIVE_X
    toGLEnum TextureCubePosY = gl_TEXTURE_CUBE_MAP_POSITIVE_Y
    toGLEnum TextureCubeNegY = gl_TEXTURE_CUBE_MAP_NEGATIVE_Y
    toGLEnum TextureCubePosZ = gl_TEXTURE_CUBE_MAP_POSITIVE_Z
    toGLEnum TextureCubeNegZ = gl_TEXTURE_CUBE_MAP_NEGATIVE_Z
    fromGLEnum e
        | e == gl_TEXTURE_2D                  = Just TextureTarget2D
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

data TextureType = Texture2D | TextureCube
  deriving (Eq, Ord, Read, Show)

instance GLEnum TextureType where
    toGLEnum Texture2D = gl_TEXTURE_2D
    toGLEnum TextureCube = gl_TEXTURE_CUBE_MAP
    fromGLEnum e
        | e == gl_TEXTURE_2D       = Just Texture2D
        | e == gl_TEXTURE_CUBE_MAP = Just TextureCube
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


newtype ClearBufferMask = CBM { getCBM :: GLbitfield }
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

data CullFaceMode = CullFront | CullBack | CullBoth
    deriving (Eq, Ord, Read, Show)

instance GLEnum CullFaceMode where
    toGLEnum CullFront = gl_FRONT
    toGLEnum CullBack  = gl_BACK
    toGLEnum CullBoth  = gl_FRONT_AND_BACK
    fromGLEnum e
        | e == gl_FRONT           = Just CullFront
        | e == gl_BACK            = Just CullBack
        | e == gl_FRONT_AND_BACK  = Just CullBoth
        | otherwise               = Nothing

data FrontFaceDir = FrontCW | FrontCCW
    deriving (Eq, Ord, Read, Show)

instance GLEnum FrontFaceDir where
    toGLEnum FrontCW = gl_CW
    toGLEnum FrontCCW = gl_CCW
    fromGLEnum e
        | e == gl_CW  = Just FrontCW
        | e == gl_CCW = Just FrontCCW
        | otherwise   = Nothing

data IndexElementType = ByteIndices | ShortIndices
    deriving (Eq, Ord, Read, Show)

instance GLEnum IndexElementType where
    toGLEnum ByteIndices = gl_UNSIGNED_BYTE
    toGLEnum ShortIndices  = gl_UNSIGNED_SHORT
    fromGLEnum e
        | e == gl_UNSIGNED_BYTE  = Just ByteIndices
        | e == gl_UNSIGNED_SHORT = Just ShortIndices
        | otherwise              = Nothing

data DrawMode = DrawPoints | DrawLines | DrawLineLoop | DrawLineStrip 
              | DrawTriangles | DrawTriangleStrip | DrawTriangleFan
    deriving (Eq, Ord, Read, Show)

instance GLEnum DrawMode where
    toGLEnum DrawPoints = gl_POINTS
    toGLEnum DrawLines = gl_LINES
    toGLEnum DrawLineLoop = gl_LINE_LOOP
    toGLEnum DrawLineStrip = gl_LINE_STRIP
    toGLEnum DrawTriangles = gl_TRIANGLES
    toGLEnum DrawTriangleStrip = gl_TRIANGLE_STRIP
    toGLEnum DrawTriangleFan = gl_TRIANGLE_FAN
    fromGLEnum e
        | e == gl_POINTS         = Just DrawPoints
        | e == gl_LINES          = Just DrawLines
        | e == gl_LINE_LOOP      = Just DrawLineLoop
        | e == gl_LINE_STRIP     = Just DrawLineStrip
        | e == gl_TRIANGLES      = Just DrawTriangles
        | e == gl_TRIANGLE_STRIP = Just DrawTriangleStrip
        | e == gl_TRIANGLE_FAN   = Just DrawTriangleFan
        | otherwise              = Nothing

data GLDataType = GLByte | GLUByte | GLShort | GLUShort | GLFloat
    deriving (Eq, Ord, Read, Show)

instance GLEnum GLDataType where
    toGLEnum GLByte = gl_BYTE
    toGLEnum GLUByte = gl_UNSIGNED_BYTE
    toGLEnum GLShort = gl_SHORT
    toGLEnum GLUShort = gl_UNSIGNED_SHORT
    toGLEnum GLFloat = gl_FLOAT
    fromGLEnum e
        | e == gl_BYTE           = Just GLByte
        | e == gl_UNSIGNED_BYTE  = Just GLUByte
        | e == gl_SHORT          = Just GLShort
        | e == gl_UNSIGNED_SHORT = Just GLUShort
        | e == gl_FLOAT          = Just GLFloat
        | otherwise              = Nothing

data TextureWrap = ClampToEdge | Repeat | MirroredRepeat
    deriving (Eq, Ord, Read, Show)

instance GLEnum TextureWrap where
    toGLEnum ClampToEdge    = gl_CLAMP_TO_EDGE
    toGLEnum Repeat         = gl_REPEAT
    toGLEnum MirroredRepeat = gl_MIRRORED_REPEAT
    fromGLEnum e
        | e == gl_CLAMP_TO_EDGE   = Just ClampToEdge
        | e == gl_REPEAT          = Just Repeat
        | e == gl_MIRRORED_REPEAT = Just MirroredRepeat
        | otherwise               = Nothing

data TexCoordAxis = SAxis | TAxis
  deriving (Eq, Ord, Read, Show)

getWrapParamEnum SAxis = gl_TEXTURE_WRAP_S
getWrapParamEnum TAxis = gl_TEXTURE_WRAP_T


data TexFilterMode = Nearest | Linear
    deriving (Eq, Ord, Read, Show)

type TexMagFilter = TexFilterMode

instance GLEnum TexMagFilter where
    toGLEnum Nearest = gl_NEAREST
    toGLEnum Linear = gl_LINEAR
    fromGLEnum e
        | e == gl_NEAREST                = Just Nearest
        | e == gl_LINEAR                 = Just Linear
        | otherwise                      = Nothing

data TexMinFilter = TexMinFilter TexFilterMode (Maybe TexFilterMode)
    deriving (Eq, Ord, Read, Show)

instance GLEnum TexMinFilter where
    toGLEnum (TexMinFilter Nearest Nothing) = gl_NEAREST
    toGLEnum (TexMinFilter Linear Nothing) = gl_LINEAR
    toGLEnum (TexMinFilter Nearest (Just Nearest)) = gl_NEAREST_MIPMAP_NEAREST
    toGLEnum (TexMinFilter Nearest (Just Linear)) = gl_NEAREST_MIPMAP_LINEAR
    toGLEnum (TexMinFilter Linear (Just Nearest)) = gl_LINEAR_MIPMAP_NEAREST
    toGLEnum (TexMinFilter Linear (Just Linear)) = gl_LINEAR_MIPMAP_LINEAR
    fromGLEnum e
        | e == gl_NEAREST                = Just (TexMinFilter Nearest Nothing)
        | e == gl_LINEAR                 = Just (TexMinFilter Linear Nothing)
        | e == gl_NEAREST_MIPMAP_NEAREST = Just (TexMinFilter Nearest (Just Nearest))
        | e == gl_NEAREST_MIPMAP_LINEAR  = Just (TexMinFilter Nearest (Just Linear))
        | e == gl_LINEAR_MIPMAP_NEAREST  = Just (TexMinFilter Linear (Just Nearest))
        | e == gl_LINEAR_MIPMAP_LINEAR   = Just (TexMinFilter Linear (Just Linear))
        | otherwise                      = Nothing


data Capability = Blend | CullFace | DepthTest | Dither | PolygonOffsetFill
                | SampleAlphaToCoverage | SampleCoverage | ScissorTest | StencilTest
    deriving (Eq, Ord, Read, Show)

instance GLEnum Capability where
    toGLEnum Blend = gl_BLEND
    toGLEnum CullFace = gl_CULL_FACE
    toGLEnum DepthTest = gl_DEPTH_TEST
    toGLEnum Dither = gl_DITHER
    toGLEnum PolygonOffsetFill = gl_POLYGON_OFFSET_FILL
    toGLEnum SampleAlphaToCoverage = gl_SAMPLE_ALPHA_TO_COVERAGE
    toGLEnum SampleCoverage = gl_SAMPLE_COVERAGE
    toGLEnum ScissorTest = gl_SCISSOR_TEST
    toGLEnum StencilTest = gl_STENCIL_TEST
    fromGLEnum e
        | e == gl_BLEND                    = Just Blend
        | e == gl_CULL_FACE                = Just CullFace
        | e == gl_DEPTH_TEST               = Just DepthTest
        | e == gl_DITHER                   = Just Dither
        | e == gl_POLYGON_OFFSET_FILL      = Just PolygonOffsetFill
        | e == gl_SAMPLE_ALPHA_TO_COVERAGE = Just SampleAlphaToCoverage
        | e == gl_SAMPLE_COVERAGE          = Just SampleCoverage
        | e == gl_SCISSOR_TEST             = Just ScissorTest
        | e == gl_STENCIL_TEST             = Just StencilTest
        | otherwise                        = Nothing

















