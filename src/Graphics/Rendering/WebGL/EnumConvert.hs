{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
module Graphics.Rendering.WebGL.EnumConvert where

import Data.Bits
import Data.Monoid
import Graphics.Rendering.WebGL.Types
import Graphics.Rendering.WebGL.Constants

class ToGLEnum a where
    toGLEnum :: a -> GLenum

class FromGLEnum a where
    fromGLEnum :: GLenum -> Maybe a


newtype TextureUnit = TextureUnit GLuint
  deriving (Eq, Ord, Read, Show)

instance ToGLEnum TextureUnit where
    toGLEnum (TextureUnit n) = gl_TEXTURE0 + n
instance FromGLEnum TextureUnit where
    fromGLEnum e 
        | e < gl_TEXTURE0        = Nothing
        | e > gl_TEXTURE31       = Nothing
        | otherwise              = Just $ TextureUnit (e - gl_TEXTURE0)

data TextureTargetType = TargetImage | TargetTexture
data TextureType (t :: TextureTargetType) where
    Texture2D       :: TextureType t
    TextureCube     :: TextureType TargetTexture
    TextureCubePosX :: TextureType TargetImage
    TextureCubeNegX :: TextureType TargetImage
    TextureCubePosY :: TextureType TargetImage
    TextureCubeNegY :: TextureType TargetImage
    TextureCubePosZ :: TextureType TargetImage
    TextureCubeNegZ :: TextureType TargetImage


--  data TextureTarget = TextureTarget2D
                   --  | TextureCubePosX
                   --  | TextureCubeNegX
                   --  | TextureCubePosY
                   --  | TextureCubeNegY
                   --  | TextureCubePosZ
                   --  | TextureCubeNegZ
  --  deriving (Eq, Ord, Read, Show)

--  data TextureType = Texture2D | TextureCube
  --  deriving (Eq, Ord, Read, Show)

--  instance GLEnum TextureType where
    --  toGLEnum Texture2D = gl_TEXTURE_2D
    --  toGLEnum TextureCube = gl_TEXTURE_CUBE_MAP
    --  fromGLEnum e
        --  | e == gl_TEXTURE_2D       = Just Texture2D
        --  | e == gl_TEXTURE_CUBE_MAP = Just TextureCube
        --  | otherwise                = Nothing


instance ToGLEnum (TextureType t) where
    toGLEnum Texture2D       = gl_TEXTURE_2D
    toGLEnum TextureCube     = gl_TEXTURE_CUBE_MAP
    toGLEnum TextureCubePosX = gl_TEXTURE_CUBE_MAP_POSITIVE_X
    toGLEnum TextureCubeNegX = gl_TEXTURE_CUBE_MAP_NEGATIVE_X
    toGLEnum TextureCubePosY = gl_TEXTURE_CUBE_MAP_POSITIVE_Y
    toGLEnum TextureCubeNegY = gl_TEXTURE_CUBE_MAP_NEGATIVE_Y
    toGLEnum TextureCubePosZ = gl_TEXTURE_CUBE_MAP_POSITIVE_Z
    toGLEnum TextureCubeNegZ = gl_TEXTURE_CUBE_MAP_NEGATIVE_Z
    --  fromGLEnum e
        --  | e == gl_TEXTURE_2D                  = Just TextureTarget2D
        --  | e == gl_TEXTURE_CUBE_MAP_POSITIVE_X = Just TextureCubePosX
        --  | e == gl_TEXTURE_CUBE_MAP_NEGATIVE_X = Just TextureCubeNegX 
        --  | e == gl_TEXTURE_CUBE_MAP_POSITIVE_Y = Just TextureCubePosY
        --  | e == gl_TEXTURE_CUBE_MAP_NEGATIVE_Y = Just TextureCubeNegY
        --  | e == gl_TEXTURE_CUBE_MAP_POSITIVE_Z = Just TextureCubePosZ
        --  | e == gl_TEXTURE_CUBE_MAP_NEGATIVE_Z = Just TextureCubeNegZ
        --  | otherwise                           = Nothing

data BufferTarget = ArrayBuffer | ElementArrayBuffer
  deriving (Eq, Ord, Read, Show)

instance ToGLEnum BufferTarget where
    toGLEnum ArrayBuffer = gl_ARRAY_BUFFER
    toGLEnum ElementArrayBuffer = gl_ELEMENT_ARRAY_BUFFER
instance FromGLEnum BufferTarget where
    fromGLEnum e
        | e == gl_ARRAY_BUFFER         = Just ArrayBuffer
        | e == gl_ELEMENT_ARRAY_BUFFER = Just ElementArrayBuffer
        | otherwise                    = Nothing


data BlendEquation = BlendAdd | BlendSubtract | BlendReverseSubtract
  deriving (Eq, Ord, Read, Show)

instance ToGLEnum BlendEquation where
    toGLEnum BlendAdd = gl_FUNC_ADD
    toGLEnum BlendSubtract = gl_FUNC_SUBTRACT
    toGLEnum BlendReverseSubtract = gl_FUNC_REVERSE_SUBTRACT

instance FromGLEnum BlendEquation where
    fromGLEnum e
        | e == gl_FUNC_ADD              = Just BlendAdd
        | e == gl_FUNC_SUBTRACT         = Just BlendSubtract
        | e == gl_FUNC_REVERSE_SUBTRACT = Just BlendReverseSubtract
        | otherwise                     = Nothing

data BlendTarget = BlendSrc | BlendDst
data BlendFuncInput = SrcColor | SrcAlpha | ConstColor 
                    | DstColor | DstAlpha | ConstAlpha 
                    | ZeroInput
data BlendFunc (t :: BlendTarget) where
    BlendFunc :: Bool -> BlendFuncInput -> BlendFunc t
    BlendFuncSrcAlphaSaturate :: BlendFunc BlendSrc

instance ToGLEnum (BlendFunc t) where
    toGLEnum BlendFuncSrcAlphaSaturate = gl_SRC_ALPHA_SATURATE
    toGLEnum (BlendFunc False SrcColor) = gl_SRC_COLOR
    toGLEnum (BlendFunc False DstColor) = gl_DST_COLOR
    toGLEnum (BlendFunc False ConstColor) = gl_CONSTANT_COLOR
    toGLEnum (BlendFunc False SrcAlpha) = gl_SRC_ALPHA
    toGLEnum (BlendFunc False DstAlpha) = gl_DST_ALPHA
    toGLEnum (BlendFunc False ConstAlpha) = gl_CONSTANT_ALPHA
    toGLEnum (BlendFunc False ZeroInput) = gl_ZERO
    toGLEnum (BlendFunc True SrcColor) = gl_ONE_MINUS_SRC_COLOR
    toGLEnum (BlendFunc True DstColor) = gl_ONE_MINUS_DST_COLOR
    toGLEnum (BlendFunc True ConstColor) = gl_ONE_MINUS_CONSTANT_COLOR
    toGLEnum (BlendFunc True SrcAlpha) = gl_ONE_MINUS_SRC_ALPHA
    toGLEnum (BlendFunc True DstAlpha) = gl_ONE_MINUS_DST_ALPHA
    toGLEnum (BlendFunc True ConstAlpha) = gl_ONE_MINUS_CONSTANT_ALPHA
    toGLEnum (BlendFunc True ZeroInput) = gl_ONE

data BufferUsage = StaticDraw | DynamicDraw | StreamDraw
  deriving (Eq, Ord, Read, Show)

instance ToGLEnum BufferUsage where
    toGLEnum StaticDraw = gl_STATIC_DRAW
    toGLEnum DynamicDraw = gl_DYNAMIC_DRAW
    toGLEnum StreamDraw = gl_STREAM_DRAW
instance FromGLEnum BufferUsage where
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

instance ToGLEnum ClearBufferMask where
    toGLEnum (CBM x) = x
instance FromGLEnum ClearBufferMask where
    fromGLEnum e 
        | e' == 0   = Just $ CBM e
        | otherwise = Nothing
      where e' = e .&. complement (getCBM allBufferMask)

data PixelFormat = Alpha | RGB | RGBA | Luminance | LuminanceAlpha
    deriving (Eq, Ord, Read, Show)

instance ToGLEnum PixelFormat where
    toGLEnum Alpha = gl_ALPHA
    toGLEnum RGB = gl_RGB
    toGLEnum RGBA = gl_RGBA
    toGLEnum Luminance = gl_LUMINANCE
    toGLEnum LuminanceAlpha = gl_LUMINANCE_ALPHA
instance FromGLEnum PixelFormat where
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

instance ToGLEnum PixelStoreFlag where
    toGLEnum UnpackFlipY = gl_UNPACK_FLIP_Y_WEBGL
    toGLEnum UnpackPremultiplyAlpha = gl_UNPACK_PREMULTIPLY_ALPHA_WEBGL
    toGLEnum UnpackColorspaceConversion = gl_UNPACK_COLORSPACE_CONVERSION_WEBGL
instance FromGLEnum PixelStoreFlag where
    fromGLEnum e
        | e == gl_UNPACK_FLIP_Y_WEBGL                = Just UnpackFlipY
        | e == gl_UNPACK_PREMULTIPLY_ALPHA_WEBGL     = Just UnpackPremultiplyAlpha
        | e == gl_UNPACK_COLORSPACE_CONVERSION_WEBGL = Just UnpackColorspaceConversion
        | otherwise                                  = Nothing

data PixelStoreParam = UnpackAlignment
    deriving (Eq, Ord, Read, Show)

instance ToGLEnum PixelStoreParam where
    toGLEnum UnpackAlignment = gl_UNPACK_ALIGNMENT
instance FromGLEnum PixelStoreParam where
    fromGLEnum e
        | e == gl_UNPACK_ALIGNMENT = Just UnpackAlignment
        | otherwise                = Nothing

data CullFaceMode = CullFront | CullBack | CullBoth
    deriving (Eq, Ord, Read, Show)

instance ToGLEnum CullFaceMode where
    toGLEnum CullFront = gl_FRONT
    toGLEnum CullBack  = gl_BACK
    toGLEnum CullBoth  = gl_FRONT_AND_BACK
instance FromGLEnum CullFaceMode where
    fromGLEnum e
        | e == gl_FRONT           = Just CullFront
        | e == gl_BACK            = Just CullBack
        | e == gl_FRONT_AND_BACK  = Just CullBoth
        | otherwise               = Nothing

data FrontFaceDir = FrontCW | FrontCCW
    deriving (Eq, Ord, Read, Show)

instance ToGLEnum FrontFaceDir where
    toGLEnum FrontCW = gl_CW
    toGLEnum FrontCCW = gl_CCW
instance FromGLEnum FrontFaceDir where
    fromGLEnum e
        | e == gl_CW  = Just FrontCW
        | e == gl_CCW = Just FrontCCW
        | otherwise   = Nothing

data IndexElementType = ByteIndices | ShortIndices
    deriving (Eq, Ord, Read, Show)

instance ToGLEnum IndexElementType where
    toGLEnum ByteIndices = gl_UNSIGNED_BYTE
    toGLEnum ShortIndices  = gl_UNSIGNED_SHORT
instance FromGLEnum IndexElementType where
    fromGLEnum e
        | e == gl_UNSIGNED_BYTE  = Just ByteIndices
        | e == gl_UNSIGNED_SHORT = Just ShortIndices
        | otherwise              = Nothing

data DrawMode = DrawPoints | DrawLines | DrawLineLoop | DrawLineStrip 
              | DrawTriangles | DrawTriangleStrip | DrawTriangleFan
    deriving (Eq, Ord, Read, Show)

instance ToGLEnum DrawMode where
    toGLEnum DrawPoints = gl_POINTS
    toGLEnum DrawLines = gl_LINES
    toGLEnum DrawLineLoop = gl_LINE_LOOP
    toGLEnum DrawLineStrip = gl_LINE_STRIP
    toGLEnum DrawTriangles = gl_TRIANGLES
    toGLEnum DrawTriangleStrip = gl_TRIANGLE_STRIP
    toGLEnum DrawTriangleFan = gl_TRIANGLE_FAN
instance FromGLEnum DrawMode where
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

instance ToGLEnum GLDataType where
    toGLEnum GLByte = gl_BYTE
    toGLEnum GLUByte = gl_UNSIGNED_BYTE
    toGLEnum GLShort = gl_SHORT
    toGLEnum GLUShort = gl_UNSIGNED_SHORT
    toGLEnum GLFloat = gl_FLOAT
instance FromGLEnum GLDataType where
    fromGLEnum e
        | e == gl_BYTE           = Just GLByte
        | e == gl_UNSIGNED_BYTE  = Just GLUByte
        | e == gl_SHORT          = Just GLShort
        | e == gl_UNSIGNED_SHORT = Just GLUShort
        | e == gl_FLOAT          = Just GLFloat
        | otherwise              = Nothing

data TextureWrap = ClampToEdge | Repeat | MirroredRepeat
    deriving (Eq, Ord, Read, Show)

instance ToGLEnum TextureWrap where
    toGLEnum ClampToEdge    = gl_CLAMP_TO_EDGE
    toGLEnum Repeat         = gl_REPEAT
    toGLEnum MirroredRepeat = gl_MIRRORED_REPEAT
instance FromGLEnum TextureWrap where
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

instance ToGLEnum TexMagFilter where
    toGLEnum Nearest = gl_NEAREST
    toGLEnum Linear = gl_LINEAR
instance FromGLEnum TexMagFilter where
    fromGLEnum e
        | e == gl_NEAREST                = Just Nearest
        | e == gl_LINEAR                 = Just Linear
        | otherwise                      = Nothing

data TexMinFilter = TexMinFilter TexFilterMode (Maybe TexFilterMode)
    deriving (Eq, Ord, Read, Show)

instance ToGLEnum TexMinFilter where
    toGLEnum (TexMinFilter Nearest Nothing) = gl_NEAREST
    toGLEnum (TexMinFilter Linear Nothing) = gl_LINEAR
    toGLEnum (TexMinFilter Nearest (Just Nearest)) = gl_NEAREST_MIPMAP_NEAREST
    toGLEnum (TexMinFilter Nearest (Just Linear)) = gl_NEAREST_MIPMAP_LINEAR
    toGLEnum (TexMinFilter Linear (Just Nearest)) = gl_LINEAR_MIPMAP_NEAREST
    toGLEnum (TexMinFilter Linear (Just Linear)) = gl_LINEAR_MIPMAP_LINEAR
instance FromGLEnum TexMinFilter where
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

instance ToGLEnum Capability where
    toGLEnum Blend = gl_BLEND
    toGLEnum CullFace = gl_CULL_FACE
    toGLEnum DepthTest = gl_DEPTH_TEST
    toGLEnum Dither = gl_DITHER
    toGLEnum PolygonOffsetFill = gl_POLYGON_OFFSET_FILL
    toGLEnum SampleAlphaToCoverage = gl_SAMPLE_ALPHA_TO_COVERAGE
    toGLEnum SampleCoverage = gl_SAMPLE_COVERAGE
    toGLEnum ScissorTest = gl_SCISSOR_TEST
    toGLEnum StencilTest = gl_STENCIL_TEST
instance FromGLEnum Capability where
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


data CompareFunc = DepthAlways | DepthNever 
                 | DepthLT | DepthLTE
                 | DepthGT | DepthGTE
                 | DepthEQ | DepthNEQ
  deriving (Eq, Ord, Read, Show)

instance ToGLEnum CompareFunc where
    toGLEnum DepthAlways = gl_ALWAYS
    toGLEnum DepthNever = gl_NEVER
    toGLEnum DepthLT = gl_LESS
    toGLEnum DepthLTE = gl_LEQUAL
    toGLEnum DepthGT = gl_GREATER
    toGLEnum DepthGTE = gl_GEQUAL
    toGLEnum DepthEQ = gl_EQUAL
    toGLEnum DepthNEQ = gl_NOTEQUAL
instance FromGLEnum CompareFunc where
    fromGLEnum e
        | e == gl_ALWAYS = Just DepthAlways
        | e == gl_NEVER = Just DepthNever
        | e == gl_LESS = Just DepthLT
        | e == gl_LEQUAL = Just DepthLTE
        | e == gl_GREATER = Just DepthGT
        | e == gl_GEQUAL = Just DepthGTE
        | e == gl_EQUAL = Just DepthEQ
        | e == gl_NOTEQUAL = Just DepthNEQ
        | otherwise = Nothing

data HintTarget = GenerateMimap
  deriving (Eq, Ord, Read, Show)

instance ToGLEnum HintTarget where
    toGLEnum GenerateMimap = gl_GENERATE_MIPMAP_HINT
instance FromGLEnum HintTarget where
    fromGLEnum e
        | e == gl_GENERATE_MIPMAP_HINT = Just GenerateMimap
        | otherwise = Nothing

data Hint = Nicest | Fastest
  deriving (Eq, Ord, Read, Show)

instance ToGLEnum (Maybe Hint) where
    toGLEnum (Just Nicest) = gl_NICEST
    toGLEnum (Just Fastest) = gl_FASTEST
    toGLEnum nothing = gl_DONT_CARE
instance FromGLEnum (Maybe Hint) where
    fromGLEnum e
        | e == gl_NICEST = Just (Just Nicest)
        | e == gl_FASTEST = Just (Just Fastest)
        | e == gl_DONT_CARE = Just Nothing
        | otherwise = Nothing

data StencilOp = StencilKeep | StencilZero | StencilReplace | StencilInvert
               | StencilInc  | StencilDec  | StencilIncWrap | StencilDecWrap
  deriving (Eq, Ord, Read, Show)

instance ToGLEnum StencilOp where
    toGLEnum StencilKeep    = gl_KEEP
    toGLEnum StencilZero    = gl_ZERO
    toGLEnum StencilReplace = gl_REPLACE
    toGLEnum StencilInvert  = gl_INVERT
    toGLEnum StencilInc     = gl_INCR
    toGLEnum StencilDec     = gl_DECR
    toGLEnum StencilIncWrap = gl_INCR_WRAP
    toGLEnum StencilDecWrap = gl_DECR_WRAP
instance FromGLEnum StencilOp where
    fromGLEnum e
        | e == gl_KEEP      = Just StencilKeep
        | e == gl_ZERO      = Just StencilZero
        | e == gl_REPLACE   = Just StencilReplace
        | e == gl_INVERT    = Just StencilInvert
        | e == gl_INCR      = Just StencilInc
        | e == gl_DECR      = Just StencilDec
        | e == gl_INCR_WRAP = Just StencilIncWrap
        | e == gl_DECR_WRAP = Just StencilDecWrap
        | otherwise         = Nothing

data FramebufferStatus = FramebufferComplete
                       | FramebufferIncompleteAttachment
                       | FramebufferIncompleteMissingAttachment
                       | FramebufferIncompleteDimensions
                       | FramebufferUnsupported
  deriving (Eq, Ord, Read, Show)


instance ToGLEnum FramebufferStatus where
    toGLEnum FramebufferComplete                    = gl_FRAMEBUFFER_COMPLETE
    toGLEnum FramebufferIncompleteAttachment        = gl_FRAMEBUFFER_INCOMPLETE_ATTACHMENT
    toGLEnum FramebufferIncompleteMissingAttachment = gl_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT
    toGLEnum FramebufferIncompleteDimensions        = gl_FRAMEBUFFER_INCOMPLETE_DIMENSIONS
    toGLEnum FramebufferUnsupported                 = gl_FRAMEBUFFER_UNSUPPORTED
instance FromGLEnum FramebufferStatus where
    fromGLEnum e
        | e == gl_FRAMEBUFFER_COMPLETE                      = Just FramebufferComplete
        | e == gl_FRAMEBUFFER_INCOMPLETE_ATTACHMENT         = Just FramebufferIncompleteAttachment
        | e == gl_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = Just FramebufferIncompleteMissingAttachment
        | e == gl_FRAMEBUFFER_INCOMPLETE_DIMENSIONS         = Just FramebufferIncompleteDimensions
        | e == gl_FRAMEBUFFER_UNSUPPORTED                   = Just FramebufferUnsupported
        | otherwise                                         = Nothing











