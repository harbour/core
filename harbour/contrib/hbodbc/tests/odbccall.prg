/*
 * $Id$
 */

#xcommand WITH <oObject> DO => Self := <oObject>
#xcommand ENDWITH           => Self := NIL

FUNCTION Main()

   LOCAL cConStr
   LOCAL cDir
   LOCAL dsFunctions

   hb_FNameSplit( hb_ArgV( 0 ), @cDir )

   cConStr := "DBQ=" + hb_FNameMerge( cDir, "harbour.mdb" ) + ";Driver={Microsoft Access Driver (*.mdb)}"

   dsFunctions := TODBC():New( cConStr )

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


