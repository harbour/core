#require "hbodbc"

PROCEDURE Main()

   LOCAL dsFunctions := TODBC():New( "DBQ=" + hb_FNameMerge( hb_DirBase(), "test.mdb" ) + ";Driver={Microsoft Access Driver (*.mdb)}" )

   dsFunctions:SetSQL( "SELECT * FROM test" )
   ? dsFunctions:Open()

   #translate Disp() => ? dsFunctions:RecNo(), "|" + dsFunctions:FieldByName( "First" ):Value + "|"

   Disp() ; ? dsFunctions:Skip()
   Disp() ; ? dsFunctions:GoTo( 1 )
   Disp() ; ? dsFunctions:Prior()
   Disp() ; ? dsFunctions:First()
   Disp() ; ? dsFunctions:Last()
   Disp() ; ? dsFunctions:Prior()
   Disp() ; ? dsFunctions:Last()
   Disp() ; ? dsFunctions:Skip()
   Disp()

   ? dsFunctions:Close()
   dsFunctions:Destroy()

   RETURN
