#require "hbodbc"

PROCEDURE Main()

   LOCAL oDS := TODBC():New( "DBQ=" + hb_FNameMerge( hb_DirBase(), "test.mdb" ) + ";Driver={Microsoft Access Driver (*.mdb)}" )

   oDS:SetSQL( "SELECT * FROM test" )
   ? "Open()", oDS:Open()

   #translate Disp() => ? oDS:RecNo(), "/", oDS:LastRec(), iif( oDS:Bof(), "B", " " ) + iif( oDS:Eof(), "E", " " ), "|" + oDS:FieldByName( "First" ):Value + "|"

   Disp() ; ? oDS:Skip()
   Disp() ; ? oDS:GoTo( 1 )
   Disp() ; ? oDS:Prior()
   Disp() ; ? oDS:First()
   Disp() ; ? oDS:Last()
   Disp() ; ? oDS:Prior()
   Disp() ; ? oDS:MoveBy( -2 )
   Disp() ; ? oDS:Last()
   Disp() ; ? oDS:Skip()
   Disp()

   ? "Close()", oDS:Close()
   oDS:Destroy()

   RETURN
