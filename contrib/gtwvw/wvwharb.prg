/* Copyright 2014 Viktor Szakats (vszakats.net/harbour) */

#include "hbgtinfo.ch"

FUNCTION wvw_GetClipboard()
   RETURN hb_gtInfo( HB_GTI_CLIPBOARDDATA )

FUNCTION wvw_SetClipboard( cData )

   IF HB_ISSTRING( cData )
      hb_gtInfo( HB_GTI_CLIPBOARDDATA, cData )
      RETURN .T.
   ENDIF

   RETURN .F.

PROCEDURE wvw_PasteFromClipboard()

   hb_gtInfo( HB_GTI_CLIPBOARDPASTE )

   RETURN

FUNCTION wvw_GetPalette()
   RETURN hb_gtInfo( HB_GTI_PALETTE )

/* wvw_SetPalette( aRGBValues ) -> An array of 16 elements with RGB values */
PROCEDURE wvw_SetPalette( aPalette )

   IF ! HB_ISARRAY( aPalette )
      aPalette := AFill( Array( 16 ), 0 )
   ENDIF

   hb_gtInfo( HB_GTI_PALETTE, aPalette )

   RETURN

FUNCTION wvw_GetRGBColor( nColor )

   hb_default( @nColor, -1 )

   RETURN iif( nColor >= 0 .AND. nColor <= 15, hb_gtInfo( HB_GTI_PALETTE, nColor ), 0 )
