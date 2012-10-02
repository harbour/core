/*
 * $Id$
 */

#require "hbnf"

PROCEDURE Main( cArg1 )

   LOCAL nErrCode

   IF HB_ISSTRING( cArg1 )
      nErrCode := FT_FLOPTST( Asc( Upper( cArg1 ) ) - Asc( "A" ) )
      OutStd( "Return Code is " + hb_ntos( nErrCode ) + hb_eol() )
   ELSE
      OutStd( "Usage: floptst cDrive" + hb_eol() + " where cDrive is 'A' or 'B' etc..." + hb_eol() )
   ENDIF

   RETURN
