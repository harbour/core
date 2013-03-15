/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    GDI calls and passing structures.
 *
 * Copyright 2010 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
 *
 */

#require "hbwin"

#include "simpleio.ch"

PROCEDURE Main()

   LOCAL cPrinterName := "Microsoft XPS Document Writer"

   LOCAL hDC
   LOCAL pDEVMODE
   LOCAL hRECT
   LOCAL aRECT
   LOCAL hOBJECT

   pDEVMODE := __wapi_DEVMODE_New( cPrinterName )
   __wapi_DEVMODE_Set( pDEVMODE, { "dmPaperSize" => WIN_DMPAPER_A3 } )
   ? hDC := wapi_CreateDC( NIL, cPrinterName, NIL, pDEVMODE )

   ? wapi_StartDoc( hDC, { "lpszDocName" => "test job" } /* DOCINFO */ )
   ? wapi_StartPage( hDC )
   ? hOBJECT := wapi_CreateFont( ,,,,,,,,,,,,, "Arial" )
   ? wapi_SelectObject( hDC, hOBJECT )

   ? "in ARR"
   aRECT := { 100, 150, 450, 250 }
   ? wapi_DrawText( hDC, "0TEST", aRECT )

   ? "inout ARR"
   aRECT := { 100, 150, 450, 250 }
   ? wapi_DrawText( hDC, "1TEST", aRECT, WIN_DT_CALCRECT )
   ? aRECT[ 1 ], aRECT[ 2 ], aRECT[ 3 ], aRECT[ 4 ]

   ? "out HASH"
   hRECT := { => }
   ? wapi_DrawText( hDC, "2TEST", hRECT, WIN_DT_CALCRECT )
   ? hRECT[ "left" ], hRECT[ "top" ], hRECT[ "bottom" ], hRECT[ "right" ]

   ? "inout HASH"
   hRECT := { => }
   hRECT[ "left" ] := 300
   hRECT[ "top" ] := 350
   ? wapi_DrawText( hDC, "3TEST", hRECT, WIN_DT_CALCRECT )
   ? hRECT[ "left" ], hRECT[ "top" ], hRECT[ "bottom" ], hRECT[ "right" ]

   ? "in HASH"
   hRECT := { => }
   hRECT[ "left" ] := 300
   hRECT[ "top" ] := 350
   hRECT[ "right" ] := 650
   hRECT[ "bottom" ] := 450
   ? wapi_DrawText( hDC, "4TEST", hRECT )

   ? wapi_EndPage( hDC )
   ? wapi_EndDoc( hDC )

   hDC := NIL
   hOBJECT := NIL

   RETURN
