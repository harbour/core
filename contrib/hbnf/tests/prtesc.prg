#require "hbnf"

PROCEDURE Main( cParm1 )

   IF cParm1 != NIL
      ? ft_EscCode( cParm1 )
   ELSE
      ? "Usage: prtesc 'escape code sequence'"
      ? "            outputs converted code to standard output"
   ENDIF

   RETURN
