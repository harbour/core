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
