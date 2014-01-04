#require "hbnf"

PROCEDURE Main( cArg1 )

   IF HB_ISSTRING( cArg1 )
      ? "Return Code is", hb_ntos( ft_FlopTst( Asc( Upper( cArg1 ) ) - Asc( "A" ) ) )
   ELSE
      ? "Usage: floptst cDrive"
      ? "where cDrive is 'A' or 'B' etc..."
   ENDIF

   RETURN
