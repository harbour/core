/*
 * $Id$
 */

#include "simpleio.ch"

PROCEDURE Main()

   LOCAL cConStr
   LOCAL dsFunctions

   cConStr := "DBQ=" + hb_FNameMerge( hb_DirBase(), "test.mdb" ) + ";Driver={Microsoft Access Driver (*.mdb)}"

   dsFunctions := TODBC():New( cConStr )

   WITH OBJECT dsFunctions

      :SetSQL( "SELECT * FROM test" )
      :Open()
      ? :FieldByName( "First" ):Value
      ? :Skip()
      ? :FieldByName( "First" ):Value
      ? :GoTo( 1 )
      ? :FieldByName( "First" ):Value
      ? :Prior()
      ? :FieldByName( "First" ):Value
      ? :First()
      ? :FieldByName( "First" ):Value
      ? :Last()
      ? :FieldByName( "First" ):Value
      ? :Close()

   ENDWITH

   dsFunctions:Destroy()

   RETURN
