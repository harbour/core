/*
 * $Id$
 */

/*
 * This samples show how to use dbf to cache postgres records.
 */

#include "common.ch"

#define CONNECTION_OK                   0
#define CONNECTION_BAD                  1
#define CONNECTION_STARTED              2
#define CONNECTION_MADE                 3
#define CONNECTION_AWAITING_RESPONSE    4
#define CONNECTION_AUTH_OK              5
#define CONNECTION_SETENV               6
#define CONNECTION_SSL_STARTUP          7
#define CONNECTION_NEEDED               8

#define PGRES_EMPTY_QUERY               0
#define PGRES_COMMAND_OK                1
#define PGRES_TUPLES_OK                 2
#define PGRES_COPY_OUT                  3
#define PGRES_COPY_IN                   4
#define PGRES_BAD_RESPONSE              5
#define PGRES_NONFATAL_ERROR            6
#define PGRES_FATAL_ERROR               7

#define PQTRANS_IDLE                    0
#define PQTRANS_ACTIVE                  1
#define PQTRANS_INTRANS                 2
#define PQTRANS_INERROR                 3
#define PQTRANS_UNKNOWN                 4


#define DB_ALIAS 1
#define DB_FILE  2
#define DB_QUERY 3
#define DB_ROW   4
#define DB_FETCH 5

STATIC oServer
STATIC aTableTemp := {}
STATIC aTempDBF   := {}

Function Main( cServer, cDatabase, cUser, cPass )
   Local i
   Local cQuery

   SetMode( 25, 80 )

   if SQLConnect( cServer, cDatabase, cUser, cPass )
      QuickQuery('DROP TABLE test')

      cQuery := 'CREATE TABLE test ( '
      cQuery += '  codigo integer primary key, '
      cQuery += '  descri char(50), '
      cQuery += '  email varchar(50) ) '
      SQLQuery(cQuery)

      SQLOpen( 'nomes', 'SELECT * FROM test')

      for i := 1 to 50
         append blank
         replace codigo with i
         replace descri with 'test ' + str(i)
      next

      SQLApplyUpdates()

      cQuery := 'SELECT * FROM test WHERE codigo >= :1 ORDER BY codigo'
      cQuery := SQLPrepare( cQuery, 1 )
      SQLOpen( 'nomes', cQuery)

      Do while  ! Eof()
         ? recno(), nomes->Codigo, nomes->descri, nomes->email

         if recno() == 10
            delete
         endif

         if recno() == 20
            REPLACE email WITH 'teste'
         endif

         SQLFetch()
      enddo

      SQLApplyUpdates()
   endif
   Return SQLGarbageCollector()


/* Put theses functions in a library */

Function SQLApplyUpdates()
   Local cAlias := Upper(Alias())
   Local i, x
   Local oQuery
   Local oRow
   Local lUpdate
   Local lError := .F.
   Local cError

   i := ASCAN(aTableTemp, {|aVal| aVal[DB_ALIAS] == cAlias})

   IF i != 0

      oQuery := aTableTemp[i, 3]

      FOR i := 1 TO Lastrec()

        DBGoto(i)

        IF i > oQuery:Lastrec()

           /* Verifica se eh um registro novo */
           if ! Deleted()

              oRow := oQuery:GetBlankRow()

              FOR x := 1 TO FCount()
                 if oRow:Fieldpos( Fieldname(x) ) != 0
                    oRow:FieldPut(Fieldname(x), Fieldget(x))
                 endif
              NEXT

              oQuery:Append(oRow)

              cError := oQuery:ErrorMsg()

              lError := oQuery:NetErr()

           endif

        ELSE

           oRow := oQuery:GetRow(i)

           lUpdate := .F.

           IF Deleted()

              oQuery:Delete(oRow)
              cError := oQuery:ErrorMsg()
              lError := oQuery:NetErr()
           ELSE

             /* Faz update, mas compara quais campos sao diferentes */

             FOR x := 1 TO Fcount()

                if oRow:Fieldpos( Fieldname(x) ) != 0

                   if .not. (Fieldget(x) == oRow:Fieldget(Fieldname(x)))
                      oRow:Fieldput(Fieldname(x), Fieldget(x))
                      lUpdate := .t.
                   endif
                endif
             NEXT

             IF lUpdate

                oQuery:Update(oRow)
                cError := oQuery:ErrorMsg()
                lError := oQuery:NetErr()

             ENDIF
           ENDIF
        ENDIF

        if lError
           exit
        endif
      NEXT
   ENDIF

   IF lError
      Alert(cError)
   ENDIF

   Return ! lError


Procedure SQLCloseTemp( cAlias )
   Local x

   IF ! Empty(Select(cAlias))
      CLOSE &calias
   ENDIF

   x := ASCAN(aTableTemp, {|aVal| aVal[DB_ALIAS] == cAlias})

   IF ! Empty(x)
      ADel( aTableTemp, x )
      //ASize( aTableTemp, Len(aTableTemp) - 1 )
   ENDIF
   Return


Procedure SQLGarbageCollector()
   Local i
   Local oQuery

   DBCloseAll()

   FOR i := 1 TO Len(aTableTemp)
      /* Apaga arquivos dbfs criados */
      FErase(aTableTemp[i, DB_FILE])
      oQuery := aTableTemp[i, DB_QUERY]

      IF oQuery != NIL
         oQuery:Destroy()
      ENDIF

   NEXT

   FOR i := 1 TO Len(aTempDBF)
      IF File(aTempDBF[i])
         FErase(aTempDBF[i])
      ENDIF

      IF File(strtran(aTempDBF[i], '.tmp', '.dbf'))
         FErase(strtran(aTempDBF[i], '.tmp', '.dbf'))
      ENDIF

      IF File(strtran(aTempDBF[i], '.tmp', '.dbt'))
         FErase(strtran(aTempDBF[i], '.tmp', '.dbt'))
      ENDIF
   NEXT

   aTableTemp := {}
   aTempDBF   := {}
   Return


Function SQLFetch( fetchall )
   Local oQuery
   Local oRow
   Local cAlias := Upper(Alias())
   Local i, x, y
   Local nPos
   Local lEof := .F.

   Default Fetchall TO .f.

   /* Procura pela tabela no array */
   i := ASCAN(aTableTemp, {|aVal| aVal[DB_ALIAS] == cAlias})

   IF i != 0
      /* Traz registros da base de dados */

      oQuery := aTableTemp[i, DB_QUERY]
      nPos   := aTableTemp[i, DB_ROW] + 1

      if Fetchall
         aTableTemp[i, DB_FETCH] := .t.
      endif

      IF oQuery:Lastrec() >= nPos

         y := nPos

         do while nPos <= IIF( FetchAll, oQuery:Lastrec(), y )
            oRow := oQuery:GetRow(nPos)
            DBAppend()

            FOR x := 1 TO oRow:FCount()
               FieldPut( FieldPos( oRow:FieldName(x) ), oRow:FieldGet(x) )
            NEXT

            aTableTemp[i, DB_ROW] := nPos
            nPos++
         enddo

      ELSE
         // Posiciona registro no eof
         DBSkip()
      ENDIF

      lEof := nPos > oQuery:Lastrec()
   ENDIF
   return lEof


Procedure SQLFetchAll()
   SQLFetch(.t.); DBGotop()
   Return


Function SQLOpen( cAlias, cQuery, xFetch, cOrder )
   Local cFile
   Local Result := .t.
   Local x
   Local oServer
   Local oQuery
   Local aStrudbf
   Local lFetch

   oServer := SQLCurrentServer()
   cAlias := Upper(cAlias)

   /* Procura por query na area temporaria */
   x := ASCAN(aTableTemp, {|aVal| aVal[DB_ALIAS] == cAlias})

   IF ! Empty(x)
      oQuery := aTableTemp[x, 3]
      oQuery:Destroy()
   ENDIF

   IF cQuery == NIL
      cQuery := 'SELECT * FROM ' + cAlias
      IF ! Empty( cOrder )
         cQuery += ' ORDER BY ' + cOrder
      ENDIF
   ENDIF

   cQuery := cQuery
   oQuery := oServer:Query(cQuery)

   IF oQuery:NetErr()
      Alert(oQuery:ErrorMsg())
      RETURN .F.
   ENDIF

   IF Empty(Select(cAlias))
      /* Pega estrutura da base de dados */
      aStrudbf := oQuery:Struct()

      /* Cria tabela */
      cFile := TempFile()
      DBCreate( cFile, aStrudbf )

      /* Abre Tabela */
      DBUseArea(.T., NIL, cFile, cAlias, .F.)

   ELSE
      SELECT &cAlias
      Zap

   ENDIF

   IF xFetch != NIL
      lFetch := xFetch
   ELSE
      lFetch := .F.
   ENDIF

   /* Se nao houver query na area temporaria entao adiciona, caso contrario, apenas atualiza */
   IF Empty(x)
      AADD( aTableTemp, { cAlias,; // Table Name
                          cFile,;  // Temporary File Name
                          oQuery,; // Object Query
                          0,;      // Current Row
                          lFetch } ) // Fetch Status
   ELSE

      aTableTemp[ x, DB_QUERY ] := oQuery
      aTableTemp[ x, DB_ROW ]   := 0
      aTableTemp[ x, DB_FETCH ] := lFetch

   ENDIF

   /* Traz registros da base de dados */
   SQLFetch(lFetch)

   IF lFetch
      DBGotop()
   ENDIF

   Return result


Function SQLConnect( cServer, cDatabase, cUser, cPassword, cSchema )
   Local lRetval := .t.

   oServer := TPQServer():New(cServer, cDatabase, cUser, cPassWord, 5432, cSchema)
   if oServer:NetErr()
      Alert(oServer:ErrorMsg())
      lRetval := .f.
   endif
   oServer:lAllCols := .F.
   Return lRetval


Procedure SQLDestroy()
   if oServer != NIL
      oServer:Destroy()
   endif
   return


Function SQLCurrentServer
   Return oServer


Function SQLQuery( cQuery )
   Local oQuery

   oQuery := oServer:Query(cQuery)
   IF oQuery:NetErr()
      Alert(cQuery + ':' + oQuery:ErrorMsg())
   ENDIF
   Return oQuery


Function SQLExecQuery( cQuery )
   Local oQuery
   Local result := .T.

   oQuery := oServer:Query(cQuery)
   IF oQuery:NetErr()
      Alert('Nao foi possível executar ' + cQuery + ':' + oQuery:ErrorMsg())

      result := .F.
   ELSE
      oQuery:Destroy()
   ENDIF
   Return result


Function SQLPrepare( cQuery, ... )
   Local i, x

   if Pcount() >= 2
      /* Limpa espacos desnecessarios */
      do while at( Space(2), cQuery ) != 0
         cQuery := strtran( cQuery, Space(2), Space(1) )
      enddo

      /* Coloca {} nos parametros */
      for i := 1 to Pcount() - 1
         if ! empty(x := at( ':' + ltrim(str(i)), cQuery))
            cQuery := stuff( cQuery, x, 0, '{' )
            cQuery := stuff( cQuery, x + len(ltrim(str(i))) + 2, 0, '}' )
         endif
      next

      /* Substitui parametros por valores passados */
      for i := 2 to PCount()
         x := hb_PValue(i)

         if x != NIL .and. Empty(x)
            x := 'null'

         elseif valtype(x) == 'N'
            x := ltrim(str(x))

         elseif valtype(x) == 'D'
            x := DtoQ(x)

         elseif valtype(x) == 'L'
            x := IIF( x, "'t'", "'f'" )

         elseif valtype(x) == "C" .or. valtype(x) == 'M'
            x := StoQ(Trim(x))

         else
            x := 'null'
         endif

         cQuery := strtran(cQuery, '{:' + ltrim(str(i-1)) + '}', x)
      next
   endif

   cQuery := strtran(cQuery, '==', '=')
   cQuery := strtran(cQuery, '!=', '<>')
   cQuery := strtran(cQuery, '.and.', 'and')
   cQuery := strtran(cQuery, '.or.', 'or')
   cQuery := strtran(cQuery, '.not.', 'not')
   Return cQuery


/* Pega resultado de uma sequence */
Function SQLSequence( Sequence_name )
   Local nValue
   nValue := Val(QuickQuery("SELECT nextval(" + StoQ(sequence_name) + ")" ))
   Return nValue


Function SQLStartTrans()
   if PQtransactionstatus(oServer:pDB) != PQTRANS_INTRANS
      oServer:StartTransaction()
   endif
   Return nil


Function SQLInTrans()
   Local result
   result := (PQtransactionstatus(oServer:pDB) == PQTRANS_INTRANS)
   Return result


Function SQLCommitTrans()
   oServer:Commit()
   Return nil


Function SQLRollbackTrans()
   oServer:rollback()
   Return nil


/* Faz querie que retorna apenas 1 valor de coluna */
Function QuickQuery( cQuery )
   Local pQuery
   Local result := ""
   Local temp, aTemp
   Local x, y

   pQuery := PQexec( oServer:pDB, cQuery )

   if PQresultstatus(pQuery) == PGRES_TUPLES_OK
      if PQLastrec(pQuery) != 0
         if PQFcount(pQuery) == 1 .and. PQLastrec(pQuery) == 1
            temp := PQGetValue( pQuery, 1, 1 )
            result := iif( temp == NIL, "", temp )
         else
            result := {}
            for x := 1 to PQLastrec(pQuery)
               aTemp := {}
               for y := 1 to PQfcount(pQuery)
                  temp := PQGetValue( pQuery, x, y )
                  aadd( aTemp, iif( temp == NIL, "", temp ) )
               next
               aadd(result, aTemp)
            next
         endif
      endif
   endif
   Return result


Procedure MakeDBF( cAlias, aStructure, aIndex )
   Local cFile, i, cIndex, cKey

   Default aIndex TO {}

   cFile := TempFile()
   DBCreate( cFile, aStructure )

   /* Abre Tabela */
   DBUseArea(.T., NIL, cFile, cAlias, .F.)

   For i := 1 to Len(aIndex)
      cKey := aIndex[i]
      cIndex := TempFile()
      Index On &cKey To &cIndex

      aadd( aTempDBF, cIndex)
   Next

   AADD( aTempDBF, cFile )

  return


Function DirTmp()
   Local xDirectory
   xDirectory := IIF(Empty(Getenv("TMP")), Getenv("TEMP"), Getenv("TMP"))

   IF Empty(xDirectory)
       xDirectory := ''
   ENDIF

   IF ';' $ xDirectory
       xDirectory := LEFT( xDirectory, AT( ';', xDirectory ) - 1 )
   ENDIF

   RETURN xDirectory + IIF( Right(xDirectory, 1) != '\' .and. ! Empty(xDirectory), '\', '' )


Function TempFile( cPath, cExt )
   Local cString

   Default cPath to DirTmp(),;
           cExt  to 'tmp'

   cString := cPath + strzero(int(hb_random(val(strtran(time(), ":", "")))), 8) + '.' + cExt

   DO WHILE File( cString )
      cString := cPath + strzero(int(hb_random(val(strtran(time(), ":", "")))), 8) + '.' + cExt
   ENDDO
   Return  cString


Function DtoQ(cData)
   Return "'" + Str(Year(cData),4) + "-" + StrZero(Month(cData), 2) + "-" + StrZero(Day(cData), 2) + "'"

Function StoQ(cData)
   Return "'" + cData + "'"
