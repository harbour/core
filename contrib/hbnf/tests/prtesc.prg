#require "hbnf"

PROCEDURE Main( cParm1 )

   IF HB_ISSTRING( cParm1 )
      ? ft_EscCode( cParm1 )
   ELSE
      ? "Usage: prtesc 'escape code sequence'"
      ? "            outputs converted code to standard output"
   ENDIF

   RETURN
