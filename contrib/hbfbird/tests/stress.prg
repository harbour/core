/* VERY IMPORTANT: Don't use this query as sample, they are used for stress tests !!! */

#require "hbfbird"

PROCEDURE Main()

   LOCAL oServer, oQuery, oRow, i, x

   LOCAL cServer := "localhost:"
   LOCAL cDatabase
   LOCAL cUser := "SYSDBA"
   LOCAL cPass := "masterkey"
   LOCAL nPageSize := 1024
   LOCAL cCharSet := "UTF8"
   LOCAL nDialect := 1
   LOCAL cQuery, cName

   CLS

   hb_FNameSplit( hb_argv( 0 ), NIL, @cName, NIL )
   cDatabase := hb_DirTemp() + cName + ".fdb"

   IF ! hb_FileExists( cDatabase )
      ? FBCreateDB( cServer + cDatabase, cUser, cPass, nPageSize, cCharSet, nDialect )
   ENDIF

   ? "Connecting..."

   oServer := TFBServer():New( cServer + cDatabase, cUser, cPass, nDialect )

   IF oServer:NetErr()
      ? oServer:Error()
      RETURN
   ENDIF

   IF oServer:TableExists( "test" )
      ? oServer:Execute( "DROP TABLE Test" )
      ? oServer:Execute( "DROP DOMAIN boolean_field" )
   ENDIF

   ? "Creating domain for boolean fields..."

   ? oServer:Execute( "create domain boolean_field as smallint default 0 not null check (value in (0,1))" )

   ? "Creating test table..."
   cQuery := ;
      "CREATE TABLE test(" + ;
      "     Code SmallInt not null primary key," + ;
      "     dept Integer," + ;
      "     Name Varchar(40)," + ;
      "     Sales boolean_field," + ;
      "     Tax Float," + ;
      "     Salary Double Precision," + ;
      "     Budget Numeric(12,2)," + ;
      "     Discount Decimal(5,2)," + ;
      "     Creation Date," + ;
      "     Description blob sub_type 1 segment size 40 )"

   ? "CREATE TABLE:", oServer:Execute( cQuery )

   oQuery := oServer:Query( "SELECT code, dept, name, sales, salary, creation FROM test" )

   oServer:StartTransaction()

   FOR i := 1 TO 10000
      @ 15, 0 SAY "Inserting values.... " + Str( i )

      oRow := oQuery:Blank()

      oRow:FieldPut( 1, i )
      oRow:FieldPut( 2, i + 1 )
      oRow:FieldPut( 3, "DEPARTMENT NAME " + StrZero( i ) )
      oRow:FieldPut( 4, ( i % 10 ) == 0 )
      oRow:FieldPut( 5, 3000 + i )
      oRow:FieldPut( 6, Date() )

      oServer:Append( oRow )

      IF i % 100 == 0
         oServer:Commit()
         oServer:StartTransaction()
      ENDIF
   NEXT

   FOR i := 5000 TO 7000
      @ 16, 0 SAY "Deleting values.... " + Str( i )

      oRow := oQuery:Blank()
      oServer:Delete( oRow, "Code = " + hb_ntos( i ) )

      IF i % 100 == 0
         oServer:Commit()
         oServer:StartTransaction()
      ENDIF
   NEXT

   FOR i := 2000 TO 3000
      @ 17, 0 SAY "Updating values.... " + Str( i )

      oRow := oQuery:Blank()
      oRow:FieldPut( 5, 4000 + i )
      oServer:update( oRow, "Code = " + hb_ntos( i ) )

      IF i % 100 == 0
         oServer:Commit()
         oServer:StartTransaction()
      ENDIF
   NEXT

   oQuery := oServer:Query( "SELECT sum(salary) sum_salary FROM test WHERE code between 1 and 4000" )

   IF ! oQuery:NetErr()
      oQuery:Fetch()
      @ 18, 0 SAY "Sum values.... " + Str( oQuery:FieldGet( 1 ) )
      oQuery:Destroy()
   ENDIF

   x := 0
   FOR i := 1 TO 4000
      oQuery := oServer:Query( "SELECT * FROM test WHERE code = " + Str( i ) )

      IF ! oQuery:NetErr()
         oQuery:Fetch()
         oRow := oQuery:getrow()

         oQuery:destroy()
         x += oRow:FieldGet( oRow:FieldPos( "salary" ) )

         @ 19, 0 SAY "Sum values.... " + Str( x )
      ENDIF
   NEXT

   oServer:Destroy()

   ? "Closing..."

   RETURN
