/*
 * $Id$
 */

#require "hbpgsql"

PROCEDURE Main( cHost, cDatabase, cUser, cPass )

   LOCAL oServer, oQuery, oRow, i, x, aTables, aStruct

   LOCAL cQuery

   oServer := TPQServer():New( cHost, cDatabase, cUser, cPass )

   IF oServer:NetErr()
      ? oServer:ErrorMsg()
      QUIT
   ENDIF

   oServer:SetVerbosity( 2 )
   oServer:traceon( "simple.log" )

   ? "Tables..."

   FOR x := 1 TO 1
      aTables := oServer:ListTables()

      FOR i := 1 TO Len( aTables )
         ? aTables[ i ]
      NEXT
   NEXT

   IF oServer:TableExists( "TEST" )
      ? oQuery := oServer:Execute( "DROP TABLE Test" )

      oQuery:Destroy()
   ENDIF

   ? "Creating test table..."
   cQuery := "CREATE TABLE test("
   cQuery += "     Code integer not null primary key, "
   cQuery += "     dept Integer, "
   cQuery += "     Name Varchar(40), "
   cQuery += "     Sales boolean, "
   cQuery += "     Tax Float4, "
   cQuery += "     Salary Double Precision, "
   cQuery += "     Budget Numeric(12,2), "
   cQuery += "     Discount Numeric(5,2), "
   cQuery += "     Creation Date, "
   cQuery += "     Description text ) "

   oQuery := oServer:Query( cQuery )

   IF oQuery:NetErr()
      ? oQuery:ErrorMsg()
   ENDIF

   oQuery:Destroy()

   ? "Structure of test table"
   aStruct := oServer:TableStruct( "test" )

   FOR i := 1 TO Len( aStruct )
      ?
      FOR x := 1 TO Len( aStruct[ i ] )
         ?? aStruct[ i ][ x ], " "
      NEXT
   NEXT

   ? "Inserting, declared transaction control "
   oServer:StartTransaction()

   FOR i := 1 TO 10
      cQuery := "INSERT INTO test(code, dept, name, sales, tax, salary, budget, Discount, Creation, Description) " + ;
         "VALUES( " + Str( i ) + ", 2, 'TEST', 'y', 5, 3000, 1500.2, 7.5, '2003-12-17', 'Short Description about what ? ')"

      oQuery := oServer:Query( cQuery )

      IF oQuery:NetErr()
         ? oQuery:errorMsg()
      ENDIF

      oQuery:destroy()
   NEXT

   oServer:Commit()

   oQuery := oServer:Query( "SELECT code, name, description, sales FROM test" )

   aStruct := oQuery:Struct()

   FOR i := 1 TO Len( aStruct )
      ? aStruct[ i ][ 1 ], aStruct[ i ][ 2 ], aStruct[ i ][ 3 ], aStruct[ i ][ 4 ]
   NEXT

   ? "Fields: ", oQuery:FCount()

   oRow := oQuery:Blank()

   ? ;
      oRow:FCount(), ;
      oRow:FieldPos( "sales" ), ;
      oRow:FieldGet( 1 ), ;
      oRow:FieldName( 2 ), ;
      oRow:FieldType( 1 ), ;
      oRow:FieldDec( 1 ), ;
      oRow:FieldLen( 1 )

   oRow:FieldPut( 1, 150 )
   oRow:FieldPut( 2, "MY TEST" )

   ? oRow:FieldGet( 1 ), oRow:FieldGet( 2 )

   ? oRow:aRow[ 1 ], oRow:aRow[ 2 ], oRow:aOld[ 1 ], oRow:aOld[ 2 ]

   ? oQuery:Append( oRow )

   ? oQuery:ErrorMsg()

   DO WHILE ! oQuery:Eof()
      ? ;
         oQuery:RecNo(), ;
         oQuery:FieldPos( "code" ), ;
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
         ? "Update: ", oQuery:Update( oRow )
      ENDIF

      IF oQuery:RecNo() == 60
         oRow := oQuery:getrow()
         ? "Delete: ", oQuery:Delete( oRow )
      ENDIF

      oQuery:Skip()

   ENDDO

   oQuery:Refresh()

   FOR i := 1 TO oQuery:LastRec()
      oRow := oQuery:getrow( i )

      ? i, ;
         oRow:FieldGet( oRow:FieldPos( "code" ) ), ;
         oRow:FieldGet( 4 ), ;
         oRow:FieldGet( 2 ), ;
         oRow:FieldName( 1 ), ;
         oRow:FieldType( 1 ), ;
         oRow:FieldDec( 1 ), ;
         oRow:FieldLen( 1 ), ;
         oRow:FieldGet( i, 3 )

   NEXT

   oQuery:Destroy()

   oServer:Destroy()

   ? "Closing..."

   RETURN
