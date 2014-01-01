/*
 * This samples show how to use dbf to cache postgres records.
 */

#require "hbpgsql"

#define DB_ALIAS                        1
#define DB_FILE                         2
#define DB_QUERY                        3
#define DB_ROW                          4
#define DB_FETCH                        5

STATIC s_oServer
STATIC s_aTableTemp := {}
STATIC s_aTempDBF   := {}

PROCEDURE Main( cServer, cDatabase, cUser, cPass )

   LOCAL i
   LOCAL cQuery

   IF SQLConnect( cServer, cDatabase, cUser, cPass )
      QuickQuery( "DROP TABLE test" )

      cQuery := "CREATE TABLE test ( "
      cQuery += "  codigo integer primary key, "
      cQuery += "  descri char(50), "
      cQuery += "  email varchar(50) ) "
      SQLQuery( cQuery )

      SQLOpen( "nomes", "SELECT * FROM test" )

      FOR i := 1 TO 50
         APPEND BLANK
         REPLACE codigo WITH i
         REPLACE descri WITH "test " + hb_ntos( i )
      NEXT

      SQLApplyUpdates()

      cQuery := "SELECT * FROM test WHERE codigo >= :1 ORDER BY codigo"
      cQuery := SQLPrepare( cQuery, 1 )
      SQLOpen( "nomes", cQuery )

      DO WHILE ! Eof()
         ? RecNo(), nomes->Codigo, nomes->descri, nomes->email

         IF RecNo() == 10
            DELETE
         ENDIF

         IF RecNo() == 20
            REPLACE email WITH "teste"
         ENDIF

         SQLFetch()
      ENDDO

      SQLApplyUpdates()
   ENDIF

   SQLGarbageCollector()

   RETURN


/* Put theses functions in a library */

FUNCTION SQLApplyUpdates()

   LOCAL cAlias := Upper( Alias() )
   LOCAL i, x
   LOCAL oQuery
   LOCAL oRow
   LOCAL lUpdate
   LOCAL lError := .F.
   LOCAL cError

   i := AScan( s_aTableTemp, {| aVal | aVal[ DB_ALIAS ] == cAlias } )

   IF i != 0

      oQuery := s_aTableTemp[ i ][ 3 ]

      FOR i := 1 TO LastRec()

         dbGoto( i )

         IF i > oQuery:LastRec()

            /* Verifica se eh um registro novo */
            IF ! Deleted()

               oRow := oQuery:GetBlankRow()

               FOR x := 1 TO FCount()
                  IF oRow:FieldPos( FieldName( x ) ) != 0
                     oRow:FieldPut( FieldName( x ), FieldGet( x ) )
                  ENDIF
               NEXT

               oQuery:Append( oRow )

               cError := oQuery:ErrorMsg()

               lError := oQuery:NetErr()

            ENDIF
         ELSE

            oRow := oQuery:GetRow( i )

            lUpdate := .F.

            IF Deleted()

               oQuery:Delete( oRow )
               cError := oQuery:ErrorMsg()
               lError := oQuery:NetErr()
            ELSE

               /* Faz update, mas compara quais campos sao diferentes */

               FOR x := 1 TO FCount()

                  IF oRow:FieldPos( FieldName( x ) ) != 0

                     IF ! ( FieldGet( x ) == oRow:FieldGet( FieldName( x ) ) )
                        oRow:FieldPut( FieldName( x ), FieldGet( x ) )
                        lUpdate := .T.
                     ENDIF
                  ENDIF
               NEXT

               IF lUpdate

                  oQuery:Update( oRow )
                  cError := oQuery:ErrorMsg()
                  lError := oQuery:NetErr()

               ENDIF
            ENDIF
         ENDIF

         IF lError
            EXIT
         ENDIF
      NEXT
   ENDIF

   IF lError
      ? cError
   ENDIF

   RETURN ! lError


PROCEDURE SQLCloseTemp( cAlias )

   LOCAL x

   IF ! Empty( Select( cAlias ) )
      ( cAlias )->( dbCloseArea() )
   ENDIF

   x := AScan( s_aTableTemp, {| aVal | aVal[ DB_ALIAS ] == cAlias } )

   IF ! Empty( x )
      ADel( s_aTableTemp, x )
      // ASize( s_aTableTemp, Len( s_aTableTemp ) - 1 )
   ENDIF

   RETURN


PROCEDURE SQLGarbageCollector()

   LOCAL i
   LOCAL oQuery

   dbCloseAll()

   FOR i := 1 TO Len( s_aTableTemp )
      /* Apaga arquivos dbfs criados */
      FErase( s_aTableTemp[ i ][ DB_FILE ] )
      oQuery := s_aTableTemp[ i ][ DB_QUERY ]

      IF oQuery != NIL
         oQuery:Destroy()
      ENDIF
   NEXT

   FOR i := 1 TO Len( s_aTempDBF )
      IF hb_FileExists( s_aTempDBF[ i ] )
         FErase( s_aTempDBF[ i ] )
      ENDIF

      IF hb_FileExists( StrTran( s_aTempDBF[ i ], ".tmp", ".dbf" ) )
         FErase( StrTran( s_aTempDBF[ i ], ".tmp", ".dbf" ) )
      ENDIF

      IF hb_FileExists( StrTran( s_aTempDBF[ i ], ".tmp", ".dbt" ) )
         FErase( StrTran( s_aTempDBF[ i ], ".tmp", ".dbt" ) )
      ENDIF
   NEXT

   s_aTableTemp := {}
   s_aTempDBF   := {}

   RETURN


FUNCTION SQLFetch( fetchall )

   LOCAL oQuery
   LOCAL oRow
   LOCAL cAlias := Upper( Alias() )
   LOCAL i, x, y
   LOCAL nPos
   LOCAL lEof := .F.

   hb_default( @Fetchall, .F. )

   /* Procura pela tabela no array */
   i := AScan( s_aTableTemp, {| aVal | aVal[ DB_ALIAS ] == cAlias } )

   IF i != 0
      /* Traz registros da base de dados */

      oQuery := s_aTableTemp[ i ][ DB_QUERY ]
      nPos   := s_aTableTemp[ i ][ DB_ROW ] + 1

      IF Fetchall
         s_aTableTemp[ i ][ DB_FETCH ] := .T.
      ENDIF

      IF oQuery:LastRec() >= nPos

         y := nPos

         DO WHILE nPos <= iif( FetchAll, oQuery:LastRec(), y )
            oRow := oQuery:GetRow( nPos )
            dbAppend()

            FOR x := 1 TO oRow:FCount()
               FieldPut( FieldPos( oRow:FieldName( x ) ), oRow:FieldGet( x ) )
            NEXT

            s_aTableTemp[ i ][ DB_ROW ] := nPos
            nPos++
         ENDDO

      ELSE
         // Posiciona registro no eof
         dbSkip()
      ENDIF

      lEof := nPos > oQuery:LastRec()
   ENDIF

   RETURN lEof


PROCEDURE SQLFetchAll()

   SQLFetch( .T. )
   dbGoTop()

   RETURN


FUNCTION SQLOpen( cAlias, cQuery, xFetch, cOrder )

   LOCAL cFile
   LOCAL Result := .T.
   LOCAL x
   LOCAL s_oServer
   LOCAL oQuery
   LOCAL aStrudbf
   LOCAL lFetch

   s_oServer := SQLCurrentServer()
   cAlias := Upper( cAlias )

   /* Procura por query na area temporaria */
   x := AScan( s_aTableTemp, {| aVal | aVal[ DB_ALIAS ] == cAlias } )

   IF ! Empty( x )
      oQuery := s_aTableTemp[ x ][ 3 ]
      oQuery:Destroy()
   ENDIF

   IF cQuery == NIL
      cQuery := "SELECT * FROM " + cAlias
      IF ! Empty( cOrder )
         cQuery += " ORDER BY " + cOrder
      ENDIF
   ENDIF

   cQuery := cQuery
   oQuery := s_oServer:Query( cQuery )

   IF oQuery:NetErr()
      ? oQuery:ErrorMsg()
      RETURN .F.
   ENDIF

   IF Empty( Select( cAlias ) )
      /* Pega estrutura da base de dados */
      aStrudbf := oQuery:Struct()

      /* Cria tabela */
      cFile := TempFile()
      dbCreate( cFile, aStrudbf )

      /* Abre Tabela */
      dbUseArea( .T., NIL, cFile, cAlias, .F. )

   ELSE
      Select( cAlias )
      ZAP
   ENDIF

   IF xFetch != NIL
      lFetch := xFetch
   ELSE
      lFetch := .F.
   ENDIF

   /* Se nao houver query na area temporaria entao adiciona, caso contrario, apenas atualiza */
   IF Empty( x )
      AAdd( s_aTableTemp, { ;
         cAlias, ;  // Table Name
         cFile, ;   // Temporary File Name
         oQuery, ;  // Object Query
         0, ;       // Current Row
         lFetch } ) // Fetch Status
   ELSE

      s_aTableTemp[ x ][ DB_QUERY ] := oQuery
      s_aTableTemp[ x ][ DB_ROW ]   := 0
      s_aTableTemp[ x ][ DB_FETCH ] := lFetch

   ENDIF

   /* Traz registros da base de dados */
   SQLFetch( lFetch )

   IF lFetch
      dbGoTop()
   ENDIF

   RETURN result


FUNCTION SQLConnect( cServer, cDatabase, cUser, cPassword, cSchema )

   LOCAL lRetval := .T.

   s_oServer := TPQServer():New( cServer, cDatabase, cUser, cPassWord, 5432, cSchema )
   IF s_oServer:NetErr()
      ? s_oServer:ErrorMsg()
      lRetval := .F.
   ENDIF
   s_oServer:lAllCols := .F.

   RETURN lRetval


PROCEDURE SQLDestroy()

   IF s_oServer != NIL
      s_oServer:Destroy()
   ENDIF

   RETURN


FUNCTION SQLCurrentServer
   RETURN s_oServer


FUNCTION SQLQuery( cQuery )

   LOCAL oQuery := s_oServer:Query( cQuery )

   IF oQuery:NetErr()
      ? cQuery + ":" + oQuery:ErrorMsg()
   ENDIF

   RETURN oQuery


FUNCTION SQLExecQuery( cQuery )

   LOCAL oQuery
   LOCAL result := .T.

   oQuery := s_oServer:Query( cQuery )
   IF oQuery:NetErr()
      ? "Cannot execute " + cQuery + ":" + oQuery:ErrorMsg()

      result := .F.
   ELSE
      oQuery:Destroy()
   ENDIF

   RETURN result


FUNCTION SQLPrepare( cQuery, ... )

   LOCAL i, x

   IF PCount() >= 2
      /* Limpa espacos desnecessarios */
      DO WHILE At( Space( 2 ), cQuery ) != 0
         cQuery := StrTran( cQuery, Space( 2 ), Space( 1 ) )
      ENDDO

      /* Coloca {} nos parametros */
      FOR i := 1 TO PCount() - 1
         IF ! Empty( x := At( ":" + hb_ntos( i ), cQuery ) )
            cQuery := Stuff( cQuery, x, 0, "{" )
            cQuery := Stuff( cQuery, x + Len( hb_ntos( i ) ) + 2, 0, "}" )
         ENDIF
      NEXT

      /* Substitui parametros por valores passados */
      FOR i := 2 TO PCount()
         x := hb_PValue( i )

         IF x != NIL .AND. Empty( x )
            x := "null"

         ELSEIF HB_ISNUMERIC( x )
            x := hb_ntos( x )

         ELSEIF HB_ISDATE( x )
            x := DToQ( x )

         ELSEIF HB_ISLOGICAL( x )
            x := iif( x, "'t'", "'f'" )

         ELSEIF HB_ISSTRING( x )
            x := SToQ( RTrim( x ) )

         ELSE
            x := "null"
         ENDIF

         cQuery := StrTran( cQuery, "{:" + hb_ntos( i - 1 ) + "}", x )
      NEXT
   ENDIF

   cQuery := StrTran( cQuery, "==", "=" )
   cQuery := StrTran( cQuery, "!=", "<>" )
   cQuery := StrTran( cQuery, ".and.", "and" )
   cQuery := StrTran( cQuery, ".or.", "or" )
   cQuery := StrTran( cQuery, ".not.", "not" )

   RETURN cQuery


/* Pega resultado de uma sequence */
FUNCTION SQLSequence( Sequence_name )
   RETURN Val( QuickQuery( "SELECT nextval(" + SToQ( sequence_name ) + ")" ) )


PROCEDURE SQLStartTrans()

   IF PQtransactionStatus( s_oServer:pDB ) != PQTRANS_INTRANS
      s_oServer:StartTransaction()
   ENDIF

   RETURN


FUNCTION SQLInTrans()
   RETURN PQtransactionStatus( s_oServer:pDB ) == PQTRANS_INTRANS


PROCEDURE SQLCommitTrans()

   s_oServer:Commit()

   RETURN


PROCEDURE SQLRollbackTrans()

   s_oServer:rollback()

   RETURN


/* Faz querie que retorna apenas 1 valor de coluna */
FUNCTION QuickQuery( cQuery )

   LOCAL pQuery
   LOCAL result := ""
   LOCAL temp, aTemp
   LOCAL x, y

   pQuery := PQexec( s_oServer:pDB, cQuery )

   IF PQresultStatus( pQuery ) == PGRES_TUPLES_OK
      IF PQlastrec( pQuery ) != 0
         IF PQfcount( pQuery ) == 1 .AND. PQlastrec( pQuery ) == 1
            temp := PQgetvalue( pQuery, 1, 1 )
            result := iif( temp == NIL, "", temp )
         ELSE
            result := {}
            FOR x := 1 TO PQlastrec( pQuery )
               aTemp := {}
               FOR y := 1 TO PQfcount( pQuery )
                  temp := PQgetvalue( pQuery, x, y )
                  AAdd( aTemp, iif( temp == NIL, "", temp ) )
               NEXT
               AAdd( result, aTemp )
            NEXT
         ENDIF
      ENDIF
   ENDIF

   RETURN result


PROCEDURE MakeDBF( cAlias, aStructure, aIndex )

   LOCAL cFile, i, cIndex, cKey

   hb_default( @aIndex, {} )

   cFile := TempFile()
   dbCreate( cFile, aStructure )

   /* Open table */
   dbUseArea( .T., NIL, cFile, cAlias, .F. )

   FOR i := 1 TO Len( aIndex )
      cKey := aIndex[ i ]
      cIndex := TempFile()

      INDEX ON &cKey TO ( cIndex )

      AAdd( s_aTempDBF, cIndex )
   NEXT

   AAdd( s_aTempDBF, cFile )

   RETURN


FUNCTION TempFile( cPath, cExt )

   LOCAL cString

   hb_default( @cPath, hb_DirTemp() )
   hb_default( @cExt, "tmp" )

   cString := cPath + StrZero( Int( hb_Random( Val( StrTran( Time(), ":" ) ) ) ), 8 ) + "." + cExt

   DO WHILE hb_FileExists( cString )
      cString := cPath + StrZero( Int( hb_Random( Val( StrTran( Time(), ":" ) ) ) ), 8 ) + "." + cExt
   ENDDO

   RETURN cString


FUNCTION DToQ( cData )
   RETURN "'" + Str( Year( cData ), 4 ) + "-" + StrZero( Month( cData ), 2 ) + "-" + StrZero( Day( cData ), 2 ) + "'"

FUNCTION SToQ( cData )
   RETURN "'" + cData + "'"
