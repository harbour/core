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

   IF oQuery:neterr()
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
      cQuery := "INSERT INTO test(code, dept, name, sales, tax, salary, budget, Discount, Creation, Description) " +;
                "VALUES( " + Str( i ) + ', 2, "TEST", "y", 5, 3000, 1500.2, 7.5, "2003-12-17", "Short Description about what ? ")'

      oQuery := oServer:Query( cQuery )

      IF oQuery:neterr()
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

   ? "Fields: ", oQuery:Fcount()

   oRow := oQuery:Blank()

   ? oRow:FCount(), ;
     oRow:Fieldpos( "sales" ), ;
     oRow:Fieldget( 1 ), ;
     oRow:Fieldname( 2 ), ;
     oRow:Fieldtype( 1 ), ;
     oRow:Fielddec( 1 ), ;
     oRow:Fieldlen( 1 )

   oRow:Fieldput( 1, 150 )
   oRow:Fieldput( 2, "MY TEST" )

   ? oRow:Fieldget( 1 ), oRow:Fieldget( 2 )

   ? oRow:aRow[ 1 ], oRow:aRow[ 2 ], oRow:aOld[ 1 ], oRow:aOld[ 2 ]

   ? oQuery:Append( oRow )

   ? oQuery:ErrorMsg()

   DO WHILE ! oQuery:Eof()
      ? oQuery:Recno(),;
        oQuery:Fieldpos( "code" ),;
        oQuery:Fieldget( oQuery:Fieldpos( "code" ) ), ;
        oQuery:Fieldget( 4 ), ;
        oQuery:Fieldget( 2 ), ;
        oQuery:Fieldname( 1 ),;
        oQuery:Fieldtype( 1 ), ;
        oQuery:Fielddec( 1 ), ;
        oQuery:Fieldlen( 1 ),;
        oQuery:Fieldget( 3 )

      IF oQuery:Recno() == 50
         oRow := oQuery:getrow()

         oRow:Fieldput( 2, "My Second test" )
         ? "Update: ", oQuery:Update( oRow )
      ENDIF

      IF oQuery:Recno() == 60
         oRow := oQuery:getrow()
         ? "Delete: ", oQuery:Delete( oRow )
      ENDIF

      oQuery:Skip()

   ENDDO

   oQuery:Refresh()

   FOR i := 1 TO oQuery:Lastrec()
      oRow := oQuery:getrow( i )

      ? i, oRow:Fieldget( oRow:Fieldpos( "code" ) ),;
        oRow:Fieldget( 4 ),;
        oRow:Fieldget( 2 ),;
        oRow:Fieldname( 1 ),;
        oRow:Fieldtype( 1 ),;
        oRow:Fielddec( 1 ),;
        oRow:Fieldlen( 1 ),;
        oRow:Fieldget( i, 3 )

   NEXT

   oQuery:Destroy()

   oServer:Destroy()

   ? "Closing..."

   RETURN
