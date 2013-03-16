
#require "hbnf"

PROCEDURE Main( cParm1 )

   IF PCount() > 0
      ? ft_EscCode( cParm1 )
   ELSE
      ? "Usage: PRT_ESC  'escape code sequence' "
      ? "            outputs converted code to  standard output"
      ?
   ENDIF

   RETURN
