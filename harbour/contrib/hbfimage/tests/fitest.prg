/*
 * $Id$
 */

/*
 * Copyright 2005 Francesco Saverio Giudice <info@fsgiudice.com>
 *
 * FreeImage API test file
 */

#include "freeimag.ch"

#include "fileio.ch"

#define IMAGES_IN  ""
#define IMAGES_OUT "imgs_out/"

PROCEDURE Main()

   LOCAL im, clone, rotated, rotatedEx, rescale, im2, im3
   LOCAL centerX, centerY, width, height

// LOCAL bmpinfoheader
   LOCAL bmpinfo
// LOCAL bkcolor
// LOCAL iccprofile
   LOCAL nH, nLen, cStr
// LOCAL appo

   //? "Press Alt-D + Enter to activate debug"
   //AltD( .T. )
   //Inkey(0)
   AltD()
   // Check output directory
   IF !hb_DirExists( IMAGES_OUT )
#ifdef HB_COMPAT_C53
      hb_DirCreate( IMAGES_OUT )
#endif
   ENDIF

   ? "Initialise"
   fi_Initialise()
   //---------------------------//

   ? "Set Error Message:", fi_SetOutPutMessage( fi_Error() )
   //? "Set Error Message:", fi_SetOutPutMessage( NIL )

   ? "Version          :", fi_GetVersion()
   ? "Copyright        :", fi_GetCopyrightMessage()
   ? "File type        :", fi_GetFileType( IMAGES_IN + "sample1.jpg" )

   ? "Load JPEG directly from file"
   im := fi_Load( FIF_JPEG, IMAGES_IN + "sample1.jpg", JPEG_DEFAULT )

   ? "Clone image"
   clone := fi_Clone( im )

   ? "Pointer          :", ValToPrg( im )

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

   ? "Rotate Classic   :", ValToPrg( rotated := fi_RotateClassic( clone, 90 ) )
   ? "Save JPG ?       :", fi_Save( FIF_JPEG, rotated, IMAGES_OUT + "rotate.jpg", JPEG_DEFAULT )
   fi_Unload( rotated )

   centerx := fi_GetWidth( clone ) / 2
   centery := fi_GetHeight( clone ) / 2
   ? "Rotate Ex        :", ValToPrg( rotatedEx := fi_RotateEx( clone, 15, 0, 0, centerx, centery, .T. ) )
   ? "Save JPG ?       :", fi_Save( FIF_JPEG, rotatedEx, IMAGES_OUT + "rotateex.jpg", JPEG_DEFAULT )
   fi_Unload( rotatedEx )

   width   := fi_GetWidth( im )
   height  := fi_GetHeight( im )

   ? "Rescale          :", ValToPrg( rescale := fi_Rescale( im, width / 2, height / 2, FILTER_BICUBIC ) )
   ? "Save JPG ?       :", fi_Save( FIF_JPEG, rescale, IMAGES_OUT + "rescale.jpg", JPEG_DEFAULT )
   fi_Unload( rescale )

   im2 := fi_Clone( im )
   ? "Adjust Gamma ?   :", fi_AdjustGamma( im2, 3.0 )
   ? "Save JPG ?       :", fi_Save( FIF_JPEG, im2, IMAGES_OUT + "adjgamma.jpg", JPEG_DEFAULT )
   fi_Unload( im2 )

   im2 := fi_Clone( im )
   ? "Adjust Brightness:", fi_AdjustBrightness( im2, - 30 )
   ? "Save JPG ?       :", fi_Save( FIF_JPEG, im2, IMAGES_OUT + "adjbrigh.jpg", JPEG_DEFAULT )
   fi_Unload( im2 )

   im2 := fi_Clone( im )
   ? "Adjust Contrast ?:", fi_AdjustContrast( im2, - 30 )
   ? "Save JPG ?       :", fi_Save( FIF_JPEG, im2, IMAGES_OUT + "adjcontr.jpg", JPEG_DEFAULT )
   fi_Unload( im2 )

   im2 := fi_Clone( im )
   ? "Invert ?         :", fi_Invert( im2 )
   ? "Save JPG ?       :", fi_Save( FIF_JPEG, im2, IMAGES_OUT + "invert.jpg", JPEG_DEFAULT )
   fi_Unload( im2 )

   ? "Red Channel      :", ValToPrg( im2 := fi_GetChannel( im, FICC_RED ) )
   ? "Save JPG ?       :", fi_Save( FIF_JPEG, im2, IMAGES_OUT + "red.jpg", JPEG_DEFAULT )
   fi_Unload( im2 )

   ? "Green Channel    :", ValToPrg( im2 := fi_GetChannel( im, FICC_GREEN ) )
   ? "Save JPG ?       :", fi_Save( FIF_JPEG, im2, IMAGES_OUT + "green.jpg", JPEG_DEFAULT )
   fi_Unload( im2 )

   ? "Blue Channel     :", ValToPrg( im2 := fi_GetChannel( im, FICC_BLUE ) )
   ? "Save JPG ?       :", fi_Save( FIF_JPEG, im2, IMAGES_OUT + "blue.jpg", JPEG_DEFAULT )
   fi_Unload( im2 )

   ? "Copy             :", ValToPrg( im2 := fi_Copy( im, 300, 100, 800, 200 ) )
   ? "Save JPG ?       :", fi_Save( FIF_JPEG, im2, IMAGES_OUT + "copy.jpg", JPEG_DEFAULT )

   im3 := fi_Clone( im )
   ? "Paste ?          :", fi_Paste( im3, im2, 10, 10, 70 )
   ? "Save JPG ?       :", fi_Save( FIF_JPEG, im3, IMAGES_OUT + "paste.jpg", JPEG_DEFAULT )
   fi_Unload( im2 )
   fi_Unload( im3 )

   ? "Allocate Bitmap  :", ValToPrg( im3 := fi_AllocateT( FIT_BITMAP, 320, 200, 32 ) )
   ? "Save JPG ?       :", fi_Save( FIF_JPEG, im3, IMAGES_OUT + "allocate.jpg", JPEG_DEFAULT )
   fi_Unload( im3 )

   ? "Create ERROR     :"
   ? "Save GIF ?       :", fi_Save( FIF_GIF, im, IMAGES_OUT + "wrong.gif", 0 )

   //? ValToPrg( fi_GetInfoHeader( im ) )
   //bmpinfoheader:Buffer( fi_GetInfoHeader( im ), .T. )
   //bmpinfoheader:Pointer( fi_GetInfoHeader( im ) )
   //? "Header           :", ValToPrg( bmpinfoheader )
   //? bmpinfoheader:SayMembers(" ", .t., .t.)

   //bmpinfo:Pointer( fi_GetInfo( im ) )
   bmpinfo := NIL // To fix warning
   ? "Info           :", ValToPrg( bmpinfo )
   //? bmpinfo:SayMembers(" ", .t., .t.)
   ? "-----------------------------------------------------"
   //? ValType( bmpinfo:Devalue() )
   //Tracelog( "bmpinfoheader", ValToPrg( bmpinfoheader ), ;
   //          bmpinfoheader:SayMembers(, .t.), bmpinfoheader:Value(), bmpinfoheader:DeValue(), hb_dumpvar( bmpinfoheader:Array() ), hb_dumpvar( bmpinfoheader:acMembers ) )

   //appo := bkcolor:Value()
   //? bkcolor:Pointer( fi_GetBackgroundColor( im ) )
   //? fi_GetBackgroundColor( im, @bkcolor:Value() )
   //bkcolor:Buffer( appo )
   //? bkcolor:SayMembers(" ", .t., .t.)

   //bkcolor:rgbBlue := 205
   //? fi_SetBackgroundColor( im, hb_String2Pointer( bkcolor:Value() ) )
   Tracelog( "linha 168" )
   //? fi_SetBackgroundColor( im, bkcolor:Value() )
   Tracelog( "linha 170" )
   //? bkcolor:SayMembers(" ", .t., .t.)
   Tracelog( "linha 162" )
   //? bkcolor:Pointer( fi_GetBackgroundColor( im ) )
   //? fi_GetBackgroundColor( im, @bkcolor:Value() )
   //bkcolor:Buffer( appo )
   Tracelog( "linha 176" )
   //? bkcolor:SayMembers(" ", .t., .t.)

   Tracelog( "linha 179" )
   //iccprofile:Pointer( fi_GetICCProfile( im ) )
   Tracelog( "linha 181" )
   //? "Header           :", ValToPrg( iccprofile )
   Tracelog( "linha 183" )
   //? iccprofile:SayMembers(" ", .T., .T. )

   //bmpinfoheader:Reset()
   //appo := NIL
   //bmpinfoheader := NIL
   //hb_GCAll( .T. )

   ? "Unload images from memory"
   fi_Unload( im )
   fi_Unload( clone )

   //

   IF ( nH := FOpen( IMAGES_IN + "sample1.jpg" ) ) != F_ERROR
      nLen := FSeek( nH, 0, FS_END )
      FSeek( nH, 0, FS_SET )
      cStr := Space( nLen )
      FRead( nH, @cStr, nLen )
      FClose( nH )

      ? "Load JPEG from memory"
      im := fi_LoadFromMem( FIF_JPEG, cStr, JPEG_DEFAULT )

      ? "Pointer          :", ValToPrg( im )
      ? "Image Type       :", fi_GetImageType( im )
      ? "Save PNG ?       :", fi_Save( FIF_PNG, im, IMAGES_OUT + "sample2.png", PNG_DEFAULT  )
   ENDIF

   //---------------------------//
   ? "DeInitialise"
   fi_Deinitialise()

   ?
   ? "Look at " + IMAGES_OUT + " folder for output images"
   ?

   RETURN

PROCEDURE fi_Error( cFormat, cMessage )

   ? "ERROR!..."
   ? "Format  : ", cFormat
   ? "Message : ", cMessage

   RETURN

PROCEDURE TraceLog( c )

   HB_SYMBOL_UNUSED( c )

   RETURN

FUNCTION ValToPrg( xValue )

   LOCAL cType := ValType( xValue )

   DO CASE
   CASE cType == "C"

      xValue := StrTran( xValue, Chr( 0 ), '" + Chr( 0 ) + "' )
      xValue := StrTran( xValue, Chr( 9 ), '" + Chr( 9 ) + "' )
      xValue := StrTran( xValue, Chr( 10 ), '" + Chr( 10 ) + "' )
      xValue := StrTran( xValue, Chr( 13 ), '" + Chr( 13 ) + "' )
      xValue := StrTran( xValue, Chr( 26 ), '" + Chr( 26 ) + "' )

      RETURN '"' + xValue + '"'

   CASE cType == "N" ; RETURN hb_ntos( xValue )
   CASE cType == "D" ; RETURN 'HB_SToD("' + DToS( xValue ) + '")'
   CASE cType == "L" ; RETURN iif( xValue, ".T.", ".F." )
   CASE cType == "O" ; RETURN xValue:className() + " Object"
   CASE cType == "U" ; RETURN "NIL"
   CASE cType == "B" ; RETURN '{||...}'
   CASE cType == "A" ; RETURN '{.[' + hb_ntos( Len( xValue ) ) + '].}'
   CASE cType == "M" ; RETURN 'M:"' + xValue + '"'
   ENDCASE

   RETURN ""
