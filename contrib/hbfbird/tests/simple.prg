#require "hbfbird"

PROCEDURE Main()

   LOCAL oServer, oQuery, oRow, i, x, aTables, aStruct, aKey

   LOCAL cServer := "localhost:"
   LOCAL cDatabase
   LOCAL cUser := "SYSDBA"
   LOCAL cPass := "masterkey"
   LOCAL nPageSize := 1024
   LOCAL cCharSet := "ASCII"
   LOCAL nDialect := 1
   LOCAL cQuery, cName

   hb_FNameSplit( hb_argv( 0 ), NIL, @cName, NIL )
   cDatabase := hb_DirTemp() + cName + ".fdb"

   IF hb_FileExists( cDatabase )
      FErase( cDatabase )
   ENDIF

   ? FBCreateDB( cServer + cDatabase, cUser, cPass, nPageSize, cCharSet, nDialect )

   ? "Connecting..."

   oServer := TFBServer():New( cServer + cDatabase, cUser, cPass, nDialect )

   IF oServer:NetErr()
      ? oServer:Error()
      QUIT
   ENDIF

   ? "Tables..."

   FOR x := 1 TO 1
      aTables := oServer:ListTables()

      FOR i := 1 TO Len( aTables )
         ? aTables[ i ]
      NEXT
   NEXT

   ? "Using implicit transaction..."

   IF oServer:TableExists( "TEST" )
      oServer:Execute( "DROP TABLE Test" )
      oServer:Execute( "DROP DOMAIN boolean_field" )
   ENDIF

   ? "Creating domain for boolean fields..."
   oServer:Execute( "create domain boolean_field as smallint default 0 not null check (value in (0,1))" )

   oServer:StartTransaction()
   ? "Creating test table..."
   cQuery := "CREATE TABLE test("
   cQuery += "     Code SmallInt not null primary key, "
   cQuery += "     dept Integer, "
   cQuery += "     Name Varchar(40), "
   cQuery += "     Sales boolean_field, "
   cQuery += "     Tax Float, "
   cQuery += "     Salary Double Precision, "
   cQuery += "     Budget Numeric(12,2), "
   cQuery += "     Discount Decimal(5,2), "
   cQuery += "     Creation Date, "
   cQuery += "     Description blob sub_type 1 segment size 40 ) "

   oServer:Execute( cQuery )

   IF oServer:NetErr()
      ? oServer:Error()
   ENDIF

   oServer:Commit()

   oServer:Query( "SELECT code, dept, name, sales, salary, creation FROM test" )
   WAIT


   ? "Structure of test table"
   aStruct := oServer:TableStruct( "test" )

   FOR i := 1 TO Len( aStruct )
      ?
      FOR x := 1 TO Len( aStruct[ i ] )
         ?? aStruct[ i, x ]
      NEXT
   NEXT

   ? "Inserting, declared transaction control "
   oServer:StartTransaction()

   FOR i := 1 TO 100
      cQuery := "INSERT INTO test(code, dept, name, sales, tax, salary, budget, Discount, Creation, Description) "
      cQuery += 'VALUES( ' + hb_ntos( i ) + ', 2, "TEST", 1, 5, 3000, 1500.2, 7.5, "2003-12-22", "Short Description about what ? ")'

      oServer:Execute( cQuery )

      IF oServer:NetErr()
         ? oServer:error()
      ENDIF
   NEXT

   oServer:Commit()

   oQuery := oServer:Query( "SELECT code, name, description, sales FROM test" )

   aStruct := oQuery:Struct()

   FOR i := 1 TO Len( aStruct )
      ? aStruct[ i, 1 ], aStruct[ i, 2 ], aStruct[ i, 3 ], aStruct[ i, 4 ]
   NEXT

   aKey := oQuery:GetKeyField()

   ? "Fields: ", oQuery:FCount(), "Primary Key: ", aKey[ 1 ]

   oRow := oQuery:Blank()

   ? ;
      oRow:FCount(), ;
      oRow:FieldPos( "code" ), ;
      oRow:FieldGet( 1 ), ;
      oRow:FieldName( 1 ), ;
      oRow:FieldType( 1 ), ;
      oRow:FieldDec( 1 ), ;
      oRow:FieldLen( 1 ), ;
      Len( oRow:Getkeyfield() )

   oRow:FieldPut( 1, 150 )
   oRow:FieldPut( 2, "MY TEST" )

   ? oRow:FieldGet( 1 ), oRow:FieldGet( 2 )

   ? oServer:Append( oRow )

   ? oServer:Delete( oQuery:blank(), "code = 200" )

   ? oServer:Execute( "error caused intentionaly" )

   DO WHILE ! oQuery:Eof()
      oQuery:Skip()
      ? ;
         oQuery:FieldGet( oQuery:FieldPos( "code" ) ), ;
         oQuery:FieldGet( 4 ), ;
         oQuery:FieldGet( 2 ), ;
         oQuery:FieldName( 1 ), ;
         oQuery:FieldType( 1 ), ;
         oQuery:FieldDec( 1 ), ;
         oQuery:FieldLen( 1 ), ;
         oQuery:FieldGet( 3 )

      IF oQuery:RecNo() == 50
         oRow := oQuery:getrow()

         oRow:FieldPut( 2, "My Second test" )
         ? "Update: ", oServer:Update( oRow )
      ENDIF

      IF oQuery:RecNo() == 60
         oRow := oQuery:getrow()
         ? "Delete: ", oServer:Delete( oRow )
      ENDIF
   ENDDO

   ? "Delete: ", oServer:Delete( oQuery:Blank(), "code = 70" )

   oQuery:Refresh()

   DO WHILE oQuery:Fetch()
      oRow := oQuery:getrow()

      ? ;
         oRow:FieldGet( oRow:FieldPos( "code" ) ), ;
         oRow:FieldGet( 4 ), ;
         oRow:FieldGet( 2 ), ;
         oRow:FieldName( 1 ), ;
         oRow:FieldType( 1 ), ;
         oRow:FieldDec( 1 ), ;
         oRow:FieldLen( 1 ), ;
         oRow:FieldGet( 3 )
   ENDDO

   oQuery:Destroy()

   oServer:Destroy()

   ? "Closing..."

   RETURN
