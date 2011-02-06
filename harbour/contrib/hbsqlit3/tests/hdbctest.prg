/*
 * $Id$
 */

#include "common.ch"

FUNCTION Main()

   LOCAL oConn, oMeta, oStmt, cSql, n, oRs

   oConn := hdbcSQLTConnection():New( "test.db", .T. )

   oMeta := oConn:getMetaData()

   ? hb_ValToExp( oMeta:getTables() )

   IF AScan( oMeta:getTables(), { | a | "test" == a[ 3 ] } ) > 0
      ? "test table already exist let's drop it"
      oStmt := oConn:createStatement()
      oStmt:executeUpdate( "DROP TABLE test" )
      oStmt:Close()
      ? "dropped"
   ENDIF

   ? 'Creating test table...'
   cSql := 'CREATE TABLE test('
   cSql += '     Code integer not null primary key, '
   cSql += '     dept Integer, '
   cSql += '     Name Varchar(40), '
   cSql += '     Sales boolean, '
   cSql += '     Tax Float4, '
   cSql += '     Salary Double Precision, '
   cSql += '     Budget Numeric(12,2), '
   cSql += '     Discount Numeric (5,2), '
   cSql += '     Creation Date, '
   cSql += '     Description text ) '

   oStmt := oConn:createStatement()
   oStmt:executeUpdate( cSql )
   oStmt:Close()
   ? "created"

   ? 'Inserting, declared transaction control '
   oConn:StartTransaction()

   ? "Inserting using direct statement..."

   #define _NUMROWS_ 10

   ? Time()
   FOR n := 1 TO _NUMROWS_
      cSql := "INSERT INTO test(code, dept, name, sales, tax, salary, budget, Discount, Creation, Description) "
      cSql += "VALUES( " + str( n ) + ", 2, 'TEST', '" + iif( n % 2 != 0, "y", "n" ) + "', 5, 3000, 1500.2, 7.5, '12-22-2003', 'Short Description ')"

      oStmt := oConn:createStatement()
      oStmt:executeUpdate( cSql )

      oStmt:close()
   NEXT
   ? Time()

/*
   ? "Creating prepared statement"

   ? Time()
   oStmt := oConn:prepareStatement( "INSERT INTO test(code, dept, name, sales, tax, salary, budget, Discount, Creation, Description) VALUES ( $1, $2, $3, $4, $5, $6, $7, $8, $9, $10 )")

   FOR n := _NUMROWS_ + 1 TO _NUMROWS_ * 2
      oStmt:SetNumber( 1, n )
      oStmt:SetNumber( 2, 2 )
      oStmt:SetString( 3, "TEST" )
      oStmt:SetBoolean( 4, iif( n % 2 != 0, .T., .F. ) )
      oStmt:SetNumber( 5, 5 )
      oStmt:SetNumber( 6, 3000 )
      oStmt:SetNumber( 7, 1500 )
      oStmt:SetNumber( 8, 7.5 )
      oStmt:SetDate( 9, Date() )
      oStmt:SetString( 10, "Short' Description" )
      oStmt:executeUpdate()
   NEXT

   oStmt:close()
   ? Time()
*/
   oConn:Commit()

   oStmt := oConn:createStatement()

   ? "Excecuting query"

   oRs := oStmt:executeQuery( "SELECT code, name, description, sales FROM test" )

   ? "Showing metadata"

   ? oRs:getMetaData():getColumnCount()

   ? "Showing results"

   ? "nRows", oRs:nRows
   ? "getrow", oRs:getRow()
   ? "isbeforefirst", oRs:isBeforeFirst()
   ? "next", oRs:next()
   ? "isfirst", oRs:isFirst()
   ? "getrow", oRs:getRow()
   ? "previous", oRs:previous()
   ? "getrow", oRs:getRow()
   ? "first", oRs:first()
   ? "getrow", oRs:getRow()
   ? "last", oRs:last()
   ? "getrow", oRs:getRow()
   ? "isbeforefirst", oRs:isBeforeFirst()
   oRs:beforeFirst()

   ? "ascending"

   DO WHILE oRs:next()
      ? oRs:getrow(), oRs:getString( "code" ), oRs:getString( "name" ), oRs:getString( "description" ), oRs:getBoolean( "sales" )
   ENDDO

   ? "isafterlast", oRs:isAfterLast()

   oRs:AfterLast()

   ? "descending"

   DO WHILE oRs:previous()
      ? oRs:getrow(), oRs:getString( "code" ), oRs:getString( "name" ), oRs:getString( "description" ), oRs:getBoolean( "sales" )
   ENDDO

   ? "isbeforefirst", oRs:isBeforeFirst()

   oRs:Close()

   oStmt:Close()

   ? hb_ValToExp( oConn:getMetaData():getPrimaryKeys( "", "public", "test" ) )

   oStmt := oConn:createStatement()

   ? "Excecuting query"

   oRs := oStmt:executeQuery( "SELECT * FROM test" )

   oRs:setTableName( "TEST" )
   oRs:setPrimaryKeys( { { "code", "N" } } )

   oRs:moveToInsertRow()
   oRs:updateNumber( 1, 11 )
   oRs:updateString( "description", "Inserted" )
   oRs:insertRow()

   oRs:first()
   oRs:updateNumber( 8, 99.99 )
   oRs:updateDate( 9, date() )
   oRs:updateString( "description", "Updated" )
   oRs:updateRow()

   oRs:next()
   oRs:deleteRow()
   oRs:next()
   oRs:deleteRow()

   oRs:Close()

   oRs := oStmt:executeQuery( "SELECT * FROM test order by code" )

   DO WHILE oRs:next()
      ? oRs:getrow(), oRs:getString( "code" ), oRs:getString( "name" ), oRs:getString( "description" ), oRs:getBoolean( "sales" ), oRs:getString( "Creation" )
   ENDDO

   oRs:close()
   oStmt:close()

/*
   ? "Creating query prepared statement"

   oStmt := oConn:prepareStatement( "SELECT code FROM test WHERE name = $1" )

   oStmt:SetString( 1, "TEST" )
   oStmt:executeQuery()

   ? oRs:getMetaData():getColumnCount()
   ? oRs:getMetaData():getColumnName( 1 )
   ?

   DO WHILE oRs:next()
      FOR n := 1 TO 1
         ? oRs:getString( n )
      next
   ENDDO

   oStmt:close()

*/

   oConn:Close()

   ? "Closing..."

   RETURN NIL
