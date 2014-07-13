#require "hbnf"

PROCEDURE Main( cPath, cHide )

   LOCAL cFile := ft_TempFil( cPath, hb_defaultValue( cHide, "N" ) == "Y" )

   IF Empty( cFile )
      ? "An error occurred"
   ELSE
      ? cFile
      hb_MemoWrit( cFile, "This is a test!" )
   ENDIF

   RETURN
