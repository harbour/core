#xcommand WITH <oObject> DO => Self := <oObject>
#xcommand ENDWITH           => Self := NIL

FUNCTION Main()

   LOCAL cExePath  := substr( hb_argv(0), 1, rat( "\", hb_argv(0) ) - 1 )
   LOCAL cConStr   := ;
      "DBQ=" + cExePath + "\harbour.mdb;" + ;
      "Driver={Microsoft Access Driver (*.mdb)}"

   LOCAL dsFunctions := TODBC():New( "xx" ) // cConStr )

   WITH dsFunctions DO

      ::SetSQL( "SELECT * FROM Functions" )
      ::Open()
      ? ::FieldByName( "Code" ):Value
      ? ::Skip()
      ? ::FieldByName( "Code" ):Value
      ? ::GoTo( 1 )
      ? ::FieldByName( "Code" ):Value
      ? ::Prior()
      ? ::FieldByName( "Code" ):Value
      ? ::First()
      ? ::FieldByName( "Code" ):Value
      ? ::Last()
      ? ::FieldByName( "Code" ):Value
      ? ::Close()

   ENDWITH

   dsFunctions:Destroy()

   RETURN( NIL )


