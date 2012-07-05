/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009-2012 Pritpal Bedi <bedipritpal@hotmail.com>
 * http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#ifndef _HBQTOPENGL_CH
   #define _HBQTOPENGL_CH

#define QGL_DoubleBuffer                          0x0001                       // Specifies the use of double buffering.
#define QGL_DepthBuffer                           0x0002                       // Enables the use of a depth buffer.
#define QGL_Rgba                                  0x0004                       // Specifies that the context should use RGBA as its pixel format.
#define QGL_AlphaChannel                          0x0008                       // Enables the use of an alpha channel.
#define QGL_AccumBuffer                           0x0010                       // Enables the use of an accumulation buffer.
#define QGL_StencilBuffer                         0x0020                       // Enables the use of a stencil buffer.
#define QGL_StereoBuffers                         0x0040                       // Enables the use of a stereo buffers for use with visualization hardware.
#define QGL_DirectRendering                       0x0080                       // Specifies that the context is used for direct rendering to a display.
#define QGL_HasOverlay                            0x0100                       // Enables the use of an overlay.
#define QGL_SampleBuffers                         0x0200                       // Enables the use of sample buffers.
#define QGL_DeprecatedFunctions                   0x0400                       // Enables the use of deprecated functionality for OpenGL 3.x contexts. A context with deprecated functionality enabled is called a full context in the OpenGL specification.
#define QGL_SingleBuffer                          QGLDoubleBuffer<<16          // Specifies the use of a single buffer, as opposed to double buffers.
#define QGL_NoDepthBuffer                         QGLDepthBuffer<<16           // Disables the use of a depth buffer.
#define QGL_ColorIndex                            QGLRgba<<16                  // Specifies that the context should use a color index as its pixel format.
#define QGL_NoAlphaChannel                        QGLAlphaChannel<<16          // Disables the use of an alpha channel.
#define QGL_NoAccumBuffer                         QGLAccumBuffer<<16           // Disables the use of an accumulation buffer.
#define QGL_NoStencilBuffer                       QGLStencilBuffer<<16         // Disables the use of a stencil buffer.
#define QGL_NoStereoBuffers                       QGLStereoBuffers<<16         // Disables the use of stereo buffers.
#define QGL_IndirectRendering                     QGLDirectRendering<<16       // Specifies that the context is used for indirect rendering to a buffer.
#define QGL_NoOverlay                             QGLHasOverlay<<16            // Disables the use of an overlay.
#define QGL_NoSampleBuffers                       QGLSampleBuffers<<16         // Disables the use of sample buffers.
#define QGL_NoDeprecatedFunctions                 QGLDeprecatedFunctions<<16   // Disables the use of deprecated functionality for OpenGL 3.x contexts. A context with deprecated functionality disabled is called a forward compatible context in the OpenGL specification.

#define QGLBuffer_ReadOnly                        0x88B8                       // The buffer will be mapped for reading only.
#define QGLBuffer_WriteOnly                       0x88B9                       // The buffer will be mapped for writing only.
#define QGLBuffer_ReadWrite                       0x88BA                       // The buffer will be mapped for reading and writing

#define QGLBuffer_VertexBuffer                    0x8892                       // Vertex buffer object for use when specifying vertex arrays.
#define QGLBuffer_IndexBuffer                     0x8893                       // Index buffer object for use with glDrawElements().
#define QGLBuffer_PixelPackBuffer                 0x88EB                       // Pixel pack buffer object for reading pixel data from the GL server (for example, with glReadPixels()). Not supported under OpenGL/ES.
#define QGLBuffer_PixelUnpackBuffer               0x88EC                       // Pixel unpack buffer object for writing pixel data to the GL server (for example, with glTexImage2D()). Not supported under OpenGL/ES.

#define QGLBuffer_StreamDraw                      0x88E0                       // The data will be set once and used a few times for drawing operations. Under OpenGL/ES 1.1 this is identical to StaticDraw.
#define QGLBuffer_StreamRead                      0x88E1                       // The data will be set once and used a few times for reading data back from the GL server. Not supported under OpenGL/ES.
#define QGLBuffer_StreamCopy                      0x88E2                       // The data will be set once and used a few times for reading data back from the GL server for use in further drawing operations. Not supported under OpenGL/ES.
#define QGLBuffer_StaticDraw                      0x88E4                       // The data will be set once and used many times for drawing operations.
#define QGLBuffer_StaticRead                      0x88E5                       // The data will be set once and used many times for reading data back from the GL server. Not supported under OpenGL/ES.
#define QGLBuffer_StaticCopy                      0x88E6                       // The data will be set once and used many times for reading data back from the GL server for use in further drawing operations. Not supported under OpenGL/ES.
#define QGLBuffer_DynamicDraw                     0x88E8                       // The data will be modified repeatedly and used many times for drawing operations.
#define QGLBuffer_DynamicRead                     0x88E9                       // The data will be modified repeatedly and used many times for reading data back from the GL server. Not supported under OpenGL/ES.
#define QGLBuffer_DynamicCopy                     0x88EA                       // The data will be modified repeatedly and used many times for reading data back from the GL server for use in further drawing operations. Not supported under OpenGL/ES.

#define QGLContext_NoBindOption                   0x0000                       // Don't do anything, pass the texture straight through.
#define QGLContext_InvertedYBindOption            0x0001                       // Specifies that the texture should be flipped over the X axis so that the texture coordinate 0,0 corresponds to the top left corner. Inverting the texture implies a deep copy prior to upload.
#define QGLContext_MipmapBindOption               0x0002                       // Specifies that bindTexture() should try to generate mipmaps. If the GL implementation supports the GL_SGIS_generate_mipmap extension, mipmaps will be automatically generated for the texture. Mipmap generation is only supported for the GL_TEXTURE_2D target.
#define QGLContext_PremultipliedAlphaBindOption   0x0004                       // Specifies that the image should be uploaded with premultiplied alpha and does a conversion accordingly.
#define QGLContext_LinearFilteringBindOption      0x0008                       // Specifies that the texture filtering should be set to GL_LINEAR. Default is GL_NEAREST. If mipmap is also enabled, filtering will be set to GL_LINEAR_MIPMAP_LINEAR.
#define QGLContext_DefaultBindOption              LinearFilteringBindOption | InvertedYBindOption | MipmapBindOption  // In Qt 4.5 and earlier, bindTexture() would mirror the image and automatically generate mipmaps. This option helps preserve this default behavior.

#define QGLFormat_NoProfile                       0                            // OpenGL version is lower than 3.2.
#define QGLFormat_CoreProfile                     1                            // Functionality deprecated in OpenGL version 3.0 is not available.
#define QGLFormat_CompatibilityProfile            2                            // Functionality from earlier OpenGL versions is available.

#define QGLFormat_OpenGL_Version_None             0x00000000                   // If no OpenGL is present or if no OpenGL context is current.
#define QGLFormat_OpenGL_Version_1_1              00000001                     // OpenGL version 1.1 or higher is present.
#define QGLFormat_OpenGL_Version_1_2              00000002                     // OpenGL version 1.2 or higher is present.
#define QGLFormat_OpenGL_Version_1_3              00000004                     // OpenGL version 1.3 or higher is present.
#define QGLFormat_OpenGL_Version_1_4              00000008                     // OpenGL version 1.4 or higher is present.
#define QGLFormat_OpenGL_Version_1_5              00000010                     // OpenGL version 1.5 or higher is present.
#define QGLFormat_OpenGL_Version_2_0              00000020                     // OpenGL version 2.0 or higher is present. Note that version 2.0 supports all the functionality of version 1.5.
#define QGLFormat_OpenGL_Version_2_1              00000040                     // OpenGL version 2.1 or higher is present.
#define QGLFormat_OpenGL_Version_3_0              00001000                     // OpenGL version 3.0 or higher is present.
#define QGLFormat_OpenGL_Version_3_1              00002000                     // OpenGL version 3.1 or higher is present. Note that OpenGL version 3.1 or higher does not necessarily support all the features of version 3.0 and lower.
#define QGLFormat_OpenGL_Version_3_2              00004000                     // OpenGL version 3.2 or higher is present.
#define QGLFormat_OpenGL_Version_3_3              00008000                     // OpenGL version 3.3 or higher is present.
#define QGLFormat_OpenGL_Version_4_0              00010000                     // OpenGL version 4.0 or higher is present.
#define QGLFormat_OpenGL_ES_CommonLite_Version_1_0 0x00000100                  // OpenGL ES version 1.0 Common Lite or higher is present.
#define QGLFormat_OpenGL_ES_Common_Version_1_0     0x00000080                  // OpenGL ES version 1.0 Common or higher is present. The Common profile supports all the features of Common Lite.
#define QGLFormat_OpenGL_ES_CommonLite_Version_1_1 0x00000400                  // OpenGL ES version 1.1 Common Lite or higher is present.
#define QGLFormat_OpenGL_ES_Common_Version_1_1     0x00000200                  // OpenGL ES version 1.1 Common or higher is present. The Common profile supports all the features of Common Lite.
#define QGLFormat_OpenGL_ES_Version_2_0            0x00000800                  // OpenGL ES version 2.0 or higher is present. Note that OpenGL ES version 2.0 does not support all the features of OpenGL ES 1.x. So if OpenGL_ES_Version_2_0 is returned, none of the ES 1.x flags are returned.

#define QGLFunctions_Multitexture                 0x0001                       // glActiveTexture() function is available.
#define QGLFunctions_Shaders                      0x0002                       // Shader functions are available.
#define QGLFunctions_Buffers                      0x0004                       // Vertex and index buffer functions are available.
#define QGLFunctions_Framebuffers                 0x0008                       // Framebuffer object functions are available.
#define QGLFunctions_BlendColor                   0x0010                       // glBlendColor() is available.
#define QGLFunctions_BlendEquation                0x0020                       // glBlendEquation() is available.
#define QGLFunctions_BlendEquationSeparate        0x0040                       // glBlendEquationSeparate() is available.
#define QGLFunctions_BlendFuncSeparate            0x0080                       // glBlendFuncSeparate() is available.
#define QGLFunctions_BlendSubtract                0x0100                       // Blend subtract mode is available.
#define QGLFunctions_CompressedTextures           0x0200                       // Compressed texture functions are available.
#define QGLFunctions_Multisample                  0x0400                       // glSampleCoverage() function is available.
#define QGLFunctions_StencilSeparate              0x0800                       // Separate stencil functions are available.
#define QGLFunctions_NPOTTextures                 0x1000                       // Non power of two textures are available.

#define QGLShader_Vertex                          0x0001                       // Vertex shader written in the OpenGL Shading Language (GLSL).
#define QGLShader_Fragment                        0x0002                       // Fragment shader written in the OpenGL Shading Language (GLSL).
#define QGLShader_Geometry                        0x0004                       // Geometry shaders written in the OpenGL Shading Language (GLSL), based on the GL_EXT_geometry_shader4 extension.

#endif
