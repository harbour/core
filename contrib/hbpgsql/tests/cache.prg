/* This samples show how to use dbf to cache postgres records. */

#require "hbpgsql"

#define DB_ALIAS            1  // Table Name
#define DB_QUERY            2  // Object Query
#define DB_ROW              3  // Current Row
#define DB_FETCH            4  // Fetch Status

THREAD STATIC t_oServer
THREAD STATIC t_aTableTemp := {}

PROCEDURE Main( cHost, cDatabase, cUser, cPass )

   LOCAL i

   IF SQLConnect( cHost, hb_defaultValue( cDatabase, "postgres" ), cUser, cPass )

      QuickQuery( "DROP TABLE test" )

      SQLQuery( "CREATE TABLE test (" + ;
         "  codigo integer primary key," + ;
         "  descri char(50)," + ;
         "  email varchar(50) )" )

      SQLOpen( "nomes", "SELECT * FROM test" )

      FOR i := 1 TO 50
         dbAppend()
         nomes->codigo := i
         nomes->descri := "test " + hb_ntos( i )
      NEXT

      SQLApplyUpdates()

      SQLOpen( "nomes", SQLPrepare( "SELECT * FROM test WHERE codigo >= :1 ORDER BY codigo", 1 ) )

      DO WHILE ! Eof()
         ? RecNo(), nomes->Codigo, nomes->descri, nomes->email

         IF RecNo() == 10
            dbDelete()
         ENDIF

         IF RecNo() == 20
            nomes->email := "teste"
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

   IF ( i := AScan( t_aTableTemp, {| aVal | aVal[ DB_ALIAS ] == cAlias } ) ) > 0

      oQuery := t_aTableTemp[ i ][ DB_QUERY ]

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

                     IF !( FieldGet( x ) == oRow:FieldGet( FieldName( x ) ) )
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

   IF Select( cAlias ) != 0
      ( cAlias )->( dbCloseArea() )
   ENDIF

   IF ( x := AScan( t_aTableTemp, {| aVal | aVal[ DB_ALIAS ] == cAlias } ) ) > 0
      hb_ADel( t_aTableTemp, x, .F. /* .T. */ )
   ENDIF

   RETURN


PROCEDURE SQLGarbageCollector()

   LOCAL item
   LOCAL oQuery

   dbCloseAll()

   FOR EACH item IN t_aTableTemp
      IF ( oQuery := item[ DB_QUERY ] ) != NIL
         oQuery:Destroy()
      ENDIF
   NEXT

   ASize( t_aTableTemp, 0 )

   RETURN


FUNCTION SQLFetch( lFetchAll )

   LOCAL oQuery
   LOCAL oRow
   LOCAL cAlias := Upper( Alias() )
   LOCAL i, x, y
   LOCAL nPos
   LOCAL lEof := .F.

   hb_default( @lFetchAll, .F. )

   /* Procura pela tabela no array */

   IF ( i := AScan( t_aTableTemp, {| aVal | aVal[ DB_ALIAS ] == cAlias } ) ) > 0
      /* Traz registros da base de dados */

      oQuery := t_aTableTemp[ i ][ DB_QUERY ]
      nPos   := t_aTableTemp[ i ][ DB_ROW ] + 1

      IF lFetchAll
         t_aTableTemp[ i ][ DB_FETCH ] := .T.
      ENDIF

      IF oQuery:LastRec() >= nPos

         y := nPos

         DO WHILE nPos <= iif( lFetchAll, oQuery:LastRec(), y )
            oRow := oQuery:GetRow( nPos )
            dbAppend()

            FOR x := 1 TO oRow:FCount()
               FieldPut( FieldPos( oRow:FieldName( x ) ), oRow:FieldGet( x ) )
            NEXT

            t_aTableTemp[ i ][ DB_ROW ] := nPos
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

   LOCAL x
   LOCAL oServer
   LOCAL oQuery
   LOCAL lFetch

   oServer := SQLCurrentServer()
   cAlias := Upper( cAlias )

   /* Procura por query na area temporaria */
   IF ( x := AScan( t_aTableTemp, {| aVal | aVal[ DB_ALIAS ] == cAlias } ) ) > 0
      oQuery := t_aTableTemp[ x ][ DB_QUERY ]
      oQuery:Destroy()
   ENDIF

   IF cQuery == NIL
      cQuery := "SELECT * FROM " + cAlias
      IF ! Empty( cOrder )
         cQuery += " ORDER BY " + cOrder
      ENDIF
   ENDIF

   oQuery := oServer:Query( cQuery )

   IF oQuery:NetErr()
      ? oQuery:ErrorMsg()
      RETURN .F.
   ENDIF

   IF Select( cAlias ) == 0
      hb_dbCreateTemp( cAlias, oQuery:Struct() )
   ELSE
      dbSelectArea( cAlias )
      hb_dbZap()
   ENDIF

   IF xFetch != NIL
      lFetch := xFetch
   ELSE
      lFetch := .F.
   ENDIF

   /* Se nao houver query na area temporaria entao adiciona, caso contrario, apenas atualiza */
   IF x == 0
      AAdd( t_aTableTemp, { ;
         cAlias, ;  // DB_ALIAS
         oQuery, ;  // DB_QUERY
         0, ;       // DB_ROW
         lFetch } ) // DB_FETCH
   ELSE

      t_aTableTemp[ x ][ DB_QUERY ] := oQuery
      t_aTableTemp[ x ][ DB_ROW ]   := 0
      t_aTableTemp[ x ][ DB_FETCH ] := lFetch

   ENDIF

   /* Traz registros da base de dados */
   SQLFetch( lFetch )

   IF lFetch
      dbGoTop()
   ENDIF

   RETURN .T.


FUNCTION SQLConnect( cHost, cDatabase, cUser, cPassword, cSchema )

   LOCAL lRetval := .T.

   t_oServer := TPQServer():New( cHost, cDatabase, cUser, cPassword, 5432, cSchema )
   IF t_oServer:NetErr()
      ? t_oServer:ErrorMsg()
      lRetval := .F.
   ENDIF
   t_oServer:lAllCols := .F.

   RETURN lRetval


PROCEDURE SQLDestroy()

   IF t_oServer != NIL
      t_oServer:Destroy()
   ENDIF

   RETURN


FUNCTION SQLCurrentServer
   RETURN t_oServer


FUNCTION SQLQuery( cQuery )

   LOCAL oQuery := t_oServer:Query( cQuery )

   IF oQuery:NetErr()
      ? cQuery + ":" + oQuery:ErrorMsg()
   ENDIF

   RETURN oQuery


FUNCTION SQLExecQuery( cQuery )

   LOCAL oQuery := t_oServer:Query( cQuery )

   IF oQuery:NetErr()
      ? "Cannot execute", cQuery + ":" + oQuery:ErrorMsg()
      RETURN .F.
   ENDIF

   oQuery:Destroy()

   RETURN .T.


FUNCTION SQLPrepare( cQuery, ... )

   LOCAL i, x

   IF PCount() >= 2
      /* Limpa espacos desnecessarios */
      DO WHILE Space( 2 ) $ cQuery
         cQuery := StrTran( cQuery, Space( 2 ), Space( 1 ) )
      ENDDO

      /* Coloca {} nos parametros */
      FOR i := 1 TO PCount() - 1
         IF ( x := At( ":" + hb_ntos( i ), cQuery ) ) > 0
            cQuery := Stuff( cQuery, x, 0, "{" )
            cQuery := Stuff( cQuery, x + Len( hb_ntos( i ) ) + 2, 0, "}" )
         ENDIF
      NEXT

      /* Substitui parametros por valores passados */
      FOR i := 2 TO PCount()
         x := hb_PValue( i )

         DO CASE
         CASE x != NIL .AND. Empty( x )
            x := "null"

         CASE HB_ISNUMERIC( x )
            x := hb_ntos( x )

         CASE HB_ISDATE( x )
            x := "'" + hb_DToC( x, "yyyy-mm-dd" ) + "'"

         CASE HB_ISLOGICAL( x )
            x := iif( x, "'t'", "'f'" )

         CASE HB_ISSTRING( x )
            x := SToQ( RTrim( x ) )

         OTHERWISE
            x := "null"
         ENDCASE

         cQuery := StrTran( cQuery, "{:" + hb_ntos( i - 1 ) + "}", x )
      NEXT
   ENDIF

   RETURN hb_StrReplace( cQuery, { ;
      "=="    => "="   , ;
      "!="    => "<>"  , ;
      ".and." => "and" , ;
      ".or."  => "or"  , ;
      ".not." => "not" } )


/* Pega resultado de uma sequence */
FUNCTION SQLSequence( Sequence_name )
   RETURN Val( QuickQuery( "SELECT nextval(" + SToQ( sequence_name ) + ")" ) )


PROCEDURE SQLStartTrans()

   IF PQtransactionStatus( t_oServer:pDB ) != PQTRANS_INTRANS
      t_oServer:StartTransaction()
   ENDIF

   RETURN


FUNCTION SQLInTrans()
   RETURN PQtransactionStatus( t_oServer:pDB ) == PQTRANS_INTRANS


PROCEDURE SQLCommitTrans()

   t_oServer:Commit()

   RETURN


PROCEDURE SQLRollbackTrans()

   t_oServer:rollback()

   RETURN


/* Faz querie que retorna apenas 1 valor de coluna */
FUNCTION QuickQuery( cQuery )

   LOCAL result := ""
   LOCAL temp, aTemp
   LOCAL x, y

   LOCAL pQuery := PQexec( t_oServer:pDB, cQuery )

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


FUNCTION SToQ( cData )
   RETURN "'" + cData + "'"
