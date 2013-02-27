/*
 * $Id$
 */

/*
 * Copyright 2005 Francesco Saverio Giudice <info@fsgiudice.com>
 *
 * FreeImage API test file
 */

#require "hbfimage"

#include "fileio.ch"
#include "simpleio.ch"

#define IMAGES_IN  ""
#define IMAGES_OUT "imgs_out" + hb_ps()

PROCEDURE Main()

   LOCAL im, clone, rotated, rotatedEx, rescale, im2, im3
   LOCAL centerX, centerY, width, height

#if 0
   LOCAL bmpinfoheader
   LOCAL iccprofile
   LOCAL appo
#endif
   LOCAL bkcolor
   LOCAL bmpinfo
   LOCAL nH, nLen, cStr

#if 0
   ? "Press Alt-D + Enter to activate debug"
   AltD( .T. )
   Inkey( 0 )
#endif
   AltD()
   // Check output directory
   IF ! hb_DirExists( IMAGES_OUT )
      hb_DirCreate( IMAGES_OUT )
   ENDIF

   ? "Initialise"
   fi_Initialise()

   //

   ? "Version          :", fi_GetVersion()
   ? "Copyright        :", fi_GetCopyrightMessage()
   ? "File type        :", fi_GetFileType( IMAGES_IN + "sample1.jpg" )

   ? "Set Error Message (symbol):", fi_SetOutputMessage( @fi_Error() )
   im := fi_Load( FIF_JPEG, IMAGES_IN + "nothere.jpg", JPEG_DEFAULT )
   ? "Set Error Message (block):", fi_SetOutputMessage( {| cFormat, cMessage | fi_Error( cFormat, cMessage ) } )
   im := fi_Load( FIF_JPEG, IMAGES_IN + "nothere.jpg", JPEG_DEFAULT )

   ? "Load JPEG directly from file"
   im := fi_Load( FIF_JPEG, IMAGES_IN + "sample1.jpg", JPEG_DEFAULT )

   ? "Clone image"
   clone := fi_Clone( im )

   ? "Pointer          :", hb_ValToExp( im )

   ? "Image Type       :", fi_GetImageType( im )
   ? "Color Used       :", fi_GetColorsUsed( im )
   ? "Pixel size       :", fi_GetBPP( im )
   ? "Width            :", fi_GetWidth( im )
   ? "Height           :", fi_GetHeight( im )
   ? "Byte Size        :", fi_GetLine( im )
   ? "Pitch            :", fi_GetPitch( im )
   ? "DIB Size         :", fi_GetDIBSize( im )
   ? "Dots per Meter X :", fi_GetDotsPerMeterX( im )
   ? "Dots per Meter Y :", fi_GetDotsPerMeterY( im )
   ? "Color Type       :", fi_GetColorType( im )
   ? "Red Mask         :", fi_GetRedMask( im )
   ? "Green Mask       :", fi_GetGreenMask( im )
   ? "Blue Mask        :", fi_GetBlueMask( im )
   ? "Transp. Count    :", fi_GetTransparencyCount( im )
   ? "Is Transparent ? :", fi_IsTransparent( im )
   ?
   ? "Save BMP ?       :", fi_Save( FIF_BMP , im, IMAGES_OUT + "sample1.bmp", BMP_DEFAULT  )
   ? "Save JPG ?       :", fi_Save( FIF_JPEG, im, IMAGES_OUT + "sample1.jpg", JPEG_DEFAULT )
   ? "Save PNG ?       :", fi_Save( FIF_PNG , im, IMAGES_OUT + "sample1.png", PNG_DEFAULT  )

   ? "Save TIFF ?      :", fi_Save( FIF_TIFF, clone, IMAGES_OUT + "sample1.tif", TIFF_DEFAULT )
   ? "Flip Horizontal ?:", fi_FlipHorizontal( clone )
   ? "Save JPG ?       :", fi_Save( FIF_JPEG, clone, IMAGES_OUT + "horizont.jpg", JPEG_DEFAULT )
   ? "Flip Vertical ?  :", fi_FlipVertical( clone )
   ? "Save JPG ?       :", fi_Save( FIF_JPEG, clone, IMAGES_OUT + "vertical.jpg", JPEG_DEFAULT )

   ? "Rotate Classic   :", hb_ValToExp( rotated := fi_RotateClassic( clone, 90 ) )
   ? "Save JPG ?       :", fi_Save( FIF_JPEG, rotated, IMAGES_OUT + "rotate.jpg", JPEG_DEFAULT )

   centerx := fi_GetWidth( clone ) / 2
   centery := fi_GetHeight( clone ) / 2
   ? "Rotate Ex        :", hb_ValToExp( rotatedEx := fi_RotateEx( clone, 15, 0, 0, centerx, centery, .T. ) )
   ? "Save JPG ?       :", fi_Save( FIF_JPEG, rotatedEx, IMAGES_OUT + "rotateex.jpg", JPEG_DEFAULT )

   width   := fi_GetWidth( im )
   height  := fi_GetHeight( im )

   ? "Rescale          :", hb_ValToExp( rescale := fi_Rescale( im, width / 2, height / 2, FILTER_BICUBIC ) )
   ? "Save JPG ?       :", fi_Save( FIF_JPEG, rescale, IMAGES_OUT + "rescale.jpg", JPEG_DEFAULT )

   im2 := fi_Clone( im )
   ? "Adjust Gamma ?   :", fi_AdjustGamma( im2, 3.0 )
   ? "Save JPG ?       :", fi_Save( FIF_JPEG, im2, IMAGES_OUT + "adjgamma.jpg", JPEG_DEFAULT )

   im2 := fi_Clone( im )
   ? "Adjust Brightness:", fi_AdjustBrightness( im2, - 30 )
   ? "Save JPG ?       :", fi_Save( FIF_JPEG, im2, IMAGES_OUT + "adjbrigh.jpg", JPEG_DEFAULT )

   im2 := fi_Clone( im )
   ? "Adjust Contrast ?:", fi_AdjustContrast( im2, - 30 )
   ? "Save JPG ?       :", fi_Save( FIF_JPEG, im2, IMAGES_OUT + "adjcontr.jpg", JPEG_DEFAULT )

   im2 := fi_Clone( im )
   ? "Invert ?         :", fi_Invert( im2 )
   ? "Save JPG ?       :", fi_Save( FIF_JPEG, im2, IMAGES_OUT + "invert.jpg", JPEG_DEFAULT )

   ? "Red Channel      :", hb_ValToExp( im2 := fi_GetChannel( im, FICC_RED ) )
   ? "Save JPG ?       :", fi_Save( FIF_JPEG, im2, IMAGES_OUT + "red.jpg", JPEG_DEFAULT )

   ? "Green Channel    :", hb_ValToExp( im2 := fi_GetChannel( im, FICC_GREEN ) )
   ? "Save JPG ?       :", fi_Save( FIF_JPEG, im2, IMAGES_OUT + "green.jpg", JPEG_DEFAULT )

   ? "Blue Channel     :", hb_ValToExp( im2 := fi_GetChannel( im, FICC_BLUE ) )
   ? "Save JPG ?       :", fi_Save( FIF_JPEG, im2, IMAGES_OUT + "blue.jpg", JPEG_DEFAULT )

   ? "Copy             :", hb_ValToExp( im2 := fi_Copy( im, 300, 100, 800, 200 ) )
   ? "Save JPG ?       :", fi_Save( FIF_JPEG, im2, IMAGES_OUT + "copy.jpg", JPEG_DEFAULT )

   im3 := fi_Clone( im )
   ? "Paste ?          :", fi_Paste( im3, im2, 10, 10, 70 )
   ? "Save JPG ?       :", fi_Save( FIF_JPEG, im3, IMAGES_OUT + "paste.jpg", JPEG_DEFAULT )

   ? "Allocate Bitmap  :", hb_ValToExp( im3 := fi_AllocateT( FIT_BITMAP, 320, 200, 32 ) )
   ? "Save JPG ?       :", fi_Save( FIF_JPEG, im3, IMAGES_OUT + "allocate.jpg", JPEG_DEFAULT )

   ? "Create ERROR     :"
   ? "Save GIF ?       :", fi_Save( FIF_GIF, im, IMAGES_OUT + "wrong.gif", 0 )

   bkcolor := hb_BChar( 0 ) + hb_BChar( 0 ) + hb_BChar( 205 ) + hb_BChar( 0 ) /* RGBA */
   ? fi_SetBackgroundColor( im, bkcolor )
   ? fi_GetBackgroundColor( im, @bkcolor )
   ? hb_StrToHex( bkcolor )

#if 0
   ? hb_ValToExp( fi_GetInfoHeader( im ) )
   bmpinfoheader:Buffer( fi_GetInfoHeader( im ), .T. )
   bmpinfoheader:Pointer( fi_GetInfoHeader( im ) )
   ? "Header           :", hb_ValToExp( bmpinfoheader )
   ? bmpinfoheader:SayMembers( " ", .T., .T. )

   bmpinfo:Pointer( fi_GetInfo( im ) )
   bmpinfo := NIL // To fix warning
   ? "Info           :", hb_ValToExp( bmpinfo )
   ? bmpinfo:SayMembers( " ", .T., .T. )
   ? "-----------------------------------------------------"

   ? ValType( bmpinfo:Devalue() )
   hb_traceLog( "bmpinfoheader", hb_ValToExp( bmpinfoheader ), ;
      infoheader:SayMembers(, .T. ), bmpinfoheader:Value(), bmpinfoheader:DeValue(), hb_DumpVar( bmpinfoheader:Array() ), hb_DumpVar( bmpinfoheader:acMembers ) )

   hb_traceLog( "line 179" )
   iccprofile:Pointer( fi_GetICCProfile( im ) )
   hb_traceLog( "line 181" )
   ? "Header           :", hb_ValToExp( iccprofile )
   hb_traceLog( "line 183" )
   ? iccprofile:SayMembers( " ", .T., .T. )

   bmpinfoheader:Reset()
   appo := NIL
   bmpinfoheader := NIL
   hb_gcAll( .T. )
#endif

   //

   IF ( nH := FOpen( IMAGES_IN + "sample1.jpg" ) ) != F_ERROR
      nLen := FSeek( nH, 0, FS_END )
      FSeek( nH, 0, FS_SET )
      cStr := Space( nLen )
      FRead( nH, @cStr, nLen )
      FClose( nH )

      ? "Load JPEG from memory"
      im := fi_LoadFromMemory( FIF_JPEG, cStr, JPEG_DEFAULT )

      ? "Pointer          :", hb_ValToExp( im )
      ? "Image Type       :", fi_GetImageType( im )
      ? "Save PNG ?       :", fi_Save( FIF_PNG, im, IMAGES_OUT + "sample2.png", PNG_DEFAULT  )
   ENDIF

   //

   ? "DeInitialise"
   fi_DeInitialise()

   ?
   ? "Look at " + IMAGES_OUT + " folder for output images"
   ?

   RETURN

STATIC PROCEDURE fi_Error( cFormat, cMessage )

   ? "ERROR!.."
   ? "Format : ", cFormat
   ? "Message: ", cMessage
   ?

   RETURN
