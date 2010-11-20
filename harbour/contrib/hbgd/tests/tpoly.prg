/* 
 * $Id$
 */

/*
 * Not Quite a Koch Flake -- but good for testing gdImage*Polygon()
 */

#include "gd.ch"

REQUEST HB_GT_CGI_DEFAULT

#define M_PI 3.14159265358979323846
#define IMAGES_OUT "imgs_out/"

STATIC aCoords
STATIC nSideLen
STATIC nAngle, nCoordX, nCoordY

PROCEDURE Main()

   LOCAL nDepth, nSide
   LOCAL gdImage, gdColor

   nSideLen := 100
   nDepth := 4

   nCoordX := 50
   nCoordY := 50
   aCoords := { { nCoordX, nCoordY } }
   nAngle := 0

   FOR nSide := 1 TO 3
      NQKochFlake( nDepth, nSideLen )
      nAngle += M_PI * 2 / 3
   NEXT

   NQKochFlake( nDepth, nSideLen )

   OutStd( hb_strFormat( "Drawing %d coordinate pairs%s", Len( aCoords ), hb_eol() ) )

   gdImage := gdImageCreate( 8200, 7115 )
   gdImageColorAllocate( gdImage, 0, 0, 0 )
   gdColor := gdImageColorAllocate( gdImage, 255, 255, 255 )
   gdImagePolygon( gdImage, aCoords, gdColor )
   gdImagePng( gdImage, IMAGES_OUT + "flake.png" )
   gdImageDestroy( gdImage )

   gdImage := gdImageCreate( 8200, 7115 )
   gdImageColorAllocate( gdImage, 0, 0, 0 )
   gdColor := gdImageColorAllocate( gdImage, 255, 255, 255 )
   gdImageOpenPolygon( gdImage, aCoords, gdColor )
   gdImagePng( gdImage, IMAGES_OUT + "flakeo.png" )
   gdImageDestroy( gdImage )

   RETURN

PROCEDURE NQKochFlake( nDepth, nSideLen )

   IF nDepth == 0
      AAdd( aCoords, {;
            nCoordX += Cos( nAngle ) * nSideLen,;
            nCoordY += Sin( nAngle ) * nSideLen;
      })
   ELSE
      NQKochFlake( nDepth - 1, nSideLen  )
      nAngle += M_PI / 3
      NQKochFlake( nDepth - 1, nSideLen  )
      nAngle -= M_PI * 2 / 3
      NQKochFlake( nDepth - 1, nSideLen  )
      nAngle += M_PI / 3
      NQKochFlake( nDepth - 1, nSideLen  )
   ENDIF

   RETURN

