#require "hbodbc"

PROCEDURE Main()

   LOCAL dsFunctions := TODBC():New( "DBQ=" + hb_FNameMerge( hb_DirBase(), "test.mdb" ) + ";Driver={Microsoft Access Driver (*.mdb)}" )

   dsFunctions:SetSQL( "SELECT * FROM test" )
   dsFunctions:Open()
   ? dsFunctions:FieldByName( "First" ):Value
   ? dsFunctions:Skip()
   ? dsFunctions:FieldByName( "First" ):Value
   ? dsFunctions:GoTo( 1 )
   ? dsFunctions:FieldByName( "First" ):Value
   ? dsFunctions:Prior()
   ? dsFunctions:FieldByName( "First" ):Value
   ? dsFunctions:First()
   ? dsFunctions:FieldByName( "First" ):Value
   ? dsFunctions:Last()
   ? dsFunctions:FieldByName( "First" ):Value
   ? dsFunctions:Close()

   dsFunctions:Destroy()

   RETURN
