/*
 * $Id$
 */

/*
 * Koch Flake -- for testing gdImage*Polygon()
 */

#include "gd.ch"
#include "simpleio.ch"

#command TurnRight( <x> ) => nAngle += M_PI / 3 * <x>
#command TurnLeft( <x> )  => nAngle -= M_PI / 3 * <x>

#define M_PI 3.14159265358979323846
#define IMAGES_OUT "imgs_out" + hb_ps()

STATIC s_aCoords
STATIC s_nSides, s_nSideLen
STATIC s_nAngle, s_nCoordX, s_nCoordY

PROCEDURE Main()

   LOCAL nDepth, nSide
   LOCAL gdImage, gdColor

   s_nSides := 3
   s_nSideLen := 1500
   nDepth := 7

   s_nCoordX := 200
   s_nCoordY := 600
   s_aCoords := { { s_nCoordX, s_nCoordY } }
   s_nAngle := 0

   /* Regular flake */
   FOR nSide := 1 TO s_nSides
      KochFlake( nDepth, s_nSideLen, .T. )
      s_nAngle += M_PI * 2 / s_nSides
   NEXT

   /* Flake inside out */
   FOR nSide := 1 TO s_nSides
      KochFlake( nDepth, s_nSideLen, .F. )
      s_nAngle += M_PI * 2 / s_nSides
   NEXT

   OutStd( hb_strFormat( "Drawing %d vertices%s", Len( s_aCoords ), hb_eol() ) )

   gdImage := gdImageCreate( 1900, 2100 )
   gdImageColorAllocate( gdImage, 0, 0, 0 )
   gdColor := gdImageColorAllocate( gdImage, 255, 255, 0 )
   gdImagePolygon( gdImage, s_aCoords, gdColor )
   gdImagePng( gdImage, IMAGES_OUT + "flake.png" )
   gdImageDestroy( gdImage )

   gdImage := gdImageCreate( 1900, 2100 )
   gdImageColorAllocate( gdImage, 0, 0, 0 )
   gdColor := gdImageColorAllocate( gdImage, 255, 255, 0 )
   gdImageOpenPolygon( gdImage, s_aCoords, gdColor )
   gdImagePng( gdImage, IMAGES_OUT + "flakeo.png" )
   gdImageDestroy( gdImage )

   RETURN

PROCEDURE KochFlake( nDepth, s_nSideLen, lLeftFirst )

   IF nDepth == 0
      AAdd( s_aCoords, {;
            s_nCoordX += Cos( s_nAngle ) * s_nSideLen,;
            s_nCoordY += Sin( s_nAngle ) * s_nSideLen;
      })
   ELSE
      KochFlake( nDepth - 1, s_nSideLen  / 3, lLeftFirst )

      IF lLeftFirst
         TurnLeft( 1 )
      ELSE
         TurnRight( 1 )
      ENDIF
      KochFlake( nDepth - 1, s_nSideLen  / 3, lLeftFirst )

      IF lLeftFirst
         TurnRight( 2 )
      ELSE
         TurnLeft( 2 )
      ENDIF
      KochFlake( nDepth - 1, s_nSideLen  / 3, lLeftFirst )

      IF lLeftFirst
         TurnLeft( 1 )
      ELSE
         TurnRight( 1 )
      ENDIF
      KochFlake( nDepth - 1, s_nSideLen  / 3, lLeftFirst )
   ENDIF

   RETURN
