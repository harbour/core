#require "hbnf"

PROCEDURE Main( cPath, cHide )

   LOCAL cFile

   cFile := ft_TempFil( cPath, hb_defaultValue( cHide, "N" ) == "Y" )

   IF ! Empty( cFile )
      ? cFile
      hb_MemoWrit( cFile, "This is a test!" )
   ELSE
      ? "An error occurred"
   ENDIF

   RETURN
