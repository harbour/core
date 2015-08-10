#require "hbnf"

PROCEDURE Main( cPath, cHide )

   LOCAL cFile := ft_TempFil( cPath, hb_defaultValue( cHide, "N" ) == "Y" )

   IF hb_BLen( cFile ) > 0
      ? cFile
      hb_MemoWrit( cFile, "This is a test!" )
   ELSE
      ? "An error occurred"
   ENDIF

   RETURN
