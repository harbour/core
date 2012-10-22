/*
 * $Id$
 */

#require "hbnf"

PROCEDURE Main( cArg1 )

   LOCAL nErrCode

   IF HB_ISSTRING( cArg1 )
      nErrCode := FT_FLOPTST( Asc( Upper( cArg1 ) ) - Asc( "A" ) )
      ? "Return Code is " + hb_ntos( nErrCode )
   ELSE
      ? "Usage: floptst cDrive" + hb_eol() + " where cDrive is 'A' or 'B' etc..."
   ENDIF

   RETURN
