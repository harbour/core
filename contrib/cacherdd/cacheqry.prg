/*
 * Copyright 2006-2015 Pritpal Bedi <bedipritpal@hotmail.com>
 * Copyright 2006-2015 CURACAO - http://www.icuracao.com
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */


#include "cacherdd.ch"

#include "common.ch"
#include "fileio.ch"
#include "hbclass.ch"


//  Structure of the CacheQuery_ Array
//
#define QRY_HQUERY                                1
#define QRY_NUMPARAS                              2
#define QRY_PARTYPES                              3
#define QRY_NUMCOLS                               4
#define QRY_COLTYPES                              5
#define QRY_COLLENGTHS                            6
#define QRY_COLNAMESLEN                           6
#define QRY_COLNAMES                              7
#define QRY_COLCLPTYPES                           8
#define QRY_PARCLPTYPES                           9

#define QF_STATUS                                 1
#define QF_HANDLE                                 2
#define QF_STMTTYPE                               3
#define QF_NUMPARAM                               4
#define QF_NUMCOLS                                5
#define QF_COLTYPES                               6
#define QF_STRUCT                                 7


THREAD STATIC cacheQuery_:= { { -1, {} } }



FUNCTION CacheQuery_New( nConxn )                 // => hQuery, 0 IF fails
   DEFAULT nConxn TO Cache_CurConxn()
   RETURN CacheQueryOpen( nConxn )


FUNCTION CacheQuery_Close( hQuery )
   RETURN ( CacheQueryClose( hQuery ) == 0 )


FUNCTION CacheQuery_Destroy( hQuery )
   cacheQuery_[ hQuery ] := NIL
   RETURN ( CacheQueryFree( hQuery ) == 0 )


FUNCTION CacheQuery_Prepare( hQuery, cSqlCode )
   LOCAL i, nSqlCode, nNumParas, nNumCols, aStatus
   LOCAL aParTyp  := {}
   LOCAL aParClp  := {}
   LOCAL aColType := {}
   LOCAL aColLen  := {}
   LOCAL aColNme  := {}
   LOCAL aColClp  := {}

   nSqlCode := CacheQueryPrepare( hQuery, cSqlCode )

   IF nSqlCode == 0
      nNumParas := CacheQueryGetNumParas( hQuery )
      FOR i := 1 TO nNumParas
         AAdd( aParTyp, CacheQueryGetParSqlType( hQuery, i ) )

         SWITCH aParTyp[ i ]

         CASE SQL_NUMERIC
         CASE SQL_DECIMAL
         CASE SQL_INTEGER
         CASE SQL_SMALLINT
         CASE SQL_FLOAT
         CASE SQL_REAL
         CASE SQL_DOUBLE
            AAdd( aParClp, "N" )
            EXIT
         CASE SQL_CHAR
         CASE SQL_VARCHAR
            AAdd( aParClp, "C" )
            EXIT
         CASE SQL_DATETIME
            AAdd( aParClp, "D" )
            EXIT
         CASE -7
            AAdd( aParClp, "L" )
            EXIT
         CASE -4
            AAdd( aParClp, "M" )
            EXIT
         ENDSWITCH
      NEXT

      nNumCols := CacheQueryGetNumCols( hQuery )
      FOR i := 1 TO nNumCols
         AAdd( aColType, CacheQueryGetColSqlType( hQuery, i ) )
         AAdd( aColLen,  CacheQueryGetColNameLen( hQuery, i ) )
         AAdd( aColNme,  CacheQueryGetColName( hQuery, i )    )

         SWITCH aColType[ i ]

         CASE SQL_NUMERIC
         CASE SQL_DECIMAL
         CASE SQL_INTEGER
         CASE SQL_SMALLINT
         CASE SQL_FLOAT
         CASE SQL_REAL
         CASE SQL_DOUBLE
            AAdd( aColClp, "N" )
            EXIT
         CASE SQL_CHAR
         CASE SQL_VARCHAR
            AAdd( aColClp, "C" )
            EXIT
         CASE SQL_DATETIME
            AAdd( aColClp, "D" )
            EXIT
         CASE -7
            AAdd( aColClp, "L" )
            EXIT
         CASE -4
            AAdd( aColClp, "M" )
            EXIT
         ENDSWITCH
      NEXT

      aStatus := { hQuery, nNumParas, aParTyp, nNumCols, aColType, ;
                                aColLen, aColNme, aColClp, aParClp }

      IF hQuery > Len( cacheQuery_ )
         AAdd( cacheQuery_, aStatus )
      ELSE
         cacheQuery_[ hQuery ] := aStatus
      ENDIF
   ENDIF

   RETURN ( nSqlCode == 0 )


FUNCTION CacheQuery_GetNumCols( hQuery )
   RETURN  CacheQueryGetNumCols( hQuery )


FUNCTION CacheQuery_GetColSqlType( hQuery, nCol )
   RETURN CacheQueryGetColSqlType( hQuery, nCol )


FUNCTION CacheQuery_GetColName( hQuery, nCol )
   RETURN CacheQueryGetColName( hQuery, nCol )


FUNCTION CacheQuery_GetColNames( hQuery )
   RETURN cacheQuery_[ hQuery, QRY_COLNAMES ]


FUNCTION CacheQuery_GetColNameLen( hQuery, nCol )
   RETURN CacheQueryGetColNameLen( hQuery, nCol )


FUNCTION CacheQuery_GetColNamesLen( hQuery )
   RETURN cacheQuery_[ hQuery, QRY_COLNAMESLEN ]


FUNCTION CacheQuery_GetNumParas( hQuery )
   RETURN CacheQueryGetNumParas( hQuery )


FUNCTION CacheQuery_GetParSqlType( hQuery, nParam )
   RETURN CacheQueryGetParSqlType( hQuery, nParam )


FUNCTION CacheQuery_GetParTypes( hQuery )
   RETURN cacheQuery_[ hQuery, QRY_PARCLPTYPES ]


FUNCTION CacheQuery_GetColTypes( hQuery )
   RETURN cacheQuery_[ hQuery, QRY_COLCLPTYPES ]


FUNCTION CacheQuery_GetParSqlTypes( hQuery )
   RETURN cacheQuery_[ hQuery, QRY_PARTYPES ]


FUNCTION CacheQuery_GetColSqlTypes( hQuery )
   RETURN cacheQuery_[ hQuery, QRY_COLTYPES ]


FUNCTION CacheQuery_Execute( hQuery, aParam )     // => lSuccess
   LOCAL nSqlType, i, cType, aParType
   LOCAL nParam := 0

   DEFAULT aParam TO {}

   IF hQuery > len( cacheQuery_ ) .OR. cacheQuery_[ hQuery ] == NIL
      // raise r/t ERROR
   ELSE
      nParam   := cacheQuery_[ hQuery, QRY_NUMPARAS ]
      aParType := cacheQuery_[ hQuery, QRY_PARTYPES ]
   ENDIF

   IF nParam > 0
      IF ! ( nParam == Len( aParam ) )
         // Raise r/t ERROR
      ENDIF

      IF ! Empty( aParam )
         FOR i := 1 TO nParam
            nSqlType := aParType[ i ]
            cType    := ValType( aParam[ i ] )

            SWITCH ( nSqlType )

            CASE SQL_CHAR
            CASE SQL_VARCHAR                      // 12
               IF ! ( cType == "C" )
                  // RT ERROR
               ENDIF
               CacheQuerySetParam( hQuery, SQL_VARCHAR, i, aParam[ i ] )
               EXIT

            CASE SQL_INTEGER
            CASE SQL_SMALLINT
               /*
               IF ! ( cType == 'N' )
                  // RT ERROR
               ENDIF
               CacheQuerySetParam( hQuery, SQL_INTEGER, i, aParam[ i ] )
               EXIT
               */
            CASE SQL_NUMERIC                      // 2
            CASE SQL_DECIMAL
            CASE SQL_FLOAT
            CASE SQL_REAL
            CASE SQL_DOUBLE
               IF ! ( cType == "N" )
                  // RT ERROR
               ENDIF
               CacheQuerySetParam( hQuery, SQL_NUMERIC, i, aParam[ i ] )
               EXIT

            CASE SQL_DATETIME                     // 9
               IF ! ( cType == "D" )
                  // RT ERROR
               ENDIF
               CacheQuerySetParam( hQuery, SQL_DATETIME, i, Year( aParam[ i ] ), Month( aParam[ i ] ), Day( aParam[ i ] ) )
               EXIT

            CASE -7                               // -7   Logical
               IF ! ( cType == "L" )
                  // RT ERROR
               ENDIF
               CacheQuerySetParam( hQuery, nSqlType, i, aParam[ i ] )
               EXIT

            CASE -4                               // -7   Logical
               IF ! ( cType == "M" )
                  // RT ERROR
               ENDIF
               CacheQuerySetParam( hQuery, nSqlType, i, aParam[ i ] )
               EXIT

            END
         NEXT
      ENDIF
   ELSE
      CacheQuerySetParam( hQuery, -7, 0, 1 )
   ENDIF

   RETURN CacheQueryExecute( hQuery )


FUNCTION CacheQuery_Next( hQuery )
   RETURN CacheQueryNext( hQuery )


FUNCTION CacheQuery_GetField( hQuery, nField )
   LOCAL xVal, aSqlType, nCurFld

   aSqlType := cacheQuery_[ hQuery,QRY_COLTYPES ]

   IF nField <= cacheQuery_[ hQuery,QRY_NUMCOLS ]
      nCurFld := CacheQueryGetCurIdx( hQuery )

      CacheQuerySkip( hQuery, nField - nCurFld )

      xVal := CacheQueryGetParam( hQuery, aSqlType[ nField ] )

      IF aSqlType[ nField ] == 9
         xVal := SToD( StrZero( xVal[ 1 ], 4 ) + StrZero( xVal[ 2 ], 2 ) + StrZero( xVal[ 3 ], 2 ) )
      ENDIF
   ENDIF

   RETURN xVal


FUNCTION CacheQuery_GetFieldByName( hQuery, cField )
   LOCAL nField, xVal, aSqlType, nCurFld

   cField := Upper( cField )

   IF ( nField := AScan( cacheQuery_[ hQuery, QRY_COLNAMES ], cField ) ) > 0
      aSqlType := cacheQuery_[ hQuery, QRY_COLTYPES ]
      nCurFld  := CacheQueryGetCurIdx( hQuery )

      CacheQuerySkip( hQuery, nField - nCurFld )

      xVal := CacheQueryGetParam( hQuery, aSqlType[ nField ] )

      IF aSqlType[ nField ] == 9
         xVal := SToD( StrZero( xVal[ 1 ], 4 ) + StrZero( xVal[ 2 ], 2 ) + StrZero( xVal[ 3 ], 2 ) )
      ENDIF
   ENDIF

   RETURN xVal


FUNCTION CacheQuery_GetFieldsAll( hQuery )
   LOCAL xVal, aSqlType, i
   LOCAL aValues := {}

   aSqlType := cacheQuery_[ hQuery,QRY_COLTYPES ]

   FOR i := 1 TO cacheQuery_[ hQuery,QRY_NUMCOLS ]
      CacheQuerySkip( hQuery, 0 )

      xVal := CacheQueryGetParam( hQuery, aSqlType[ i ] )

      IF aSqlType[ i ] == 9
         xVal := SToD( StrZero( xVal[ 1 ], 4 ) + StrZero( xVal[ 2 ], 2 ) + StrZero( xVal[ 3 ], 2 ) )
      ENDIF

      AAdd( aValues, xVal )
   NEXT

   RETURN aValues


/*----------------------------------------------------------------------*/
//                         CLASS CacheSQLQuery
/*----------------------------------------------------------------------*/


CREATE CLASS CacheSQLQuery

   DATA   hQuery                                  INIT 0      READONLY
   DATA   hConxn                                  INIT 0      READONLY

   DATA   nNumParas                               INIT 0
   DATA   aParTypes                               INIT {}
   DATA   nNumCols                                INIT 0
   DATA   aColTypes                               INIT {}
   DATA   aColNames                               INIT {}

   DATA   cSql                                    INIT ""

   DATA   lPrepared                               INIT .F.
   DATA   lExecuted                               INIT .F.
   DATA   lEof                                    INIT .T.
   DATA   lClose                                  INIT .F.
   DATA   aParam                                  INIT {}
   DATA   cLastError                              INIT ''

   DATA   aRecords
   DATA   nNumOfRecords                           INIT 0
   DATA   cTable                                  INIT NIL
   DATA   aStruct                                 INIT {}
   DATA   cDriver                                 INIT "CACHERDD"
   DATA   hConnection
   DATA   nArea                                   INIT Select()
   DATA   lPutInTable                             INIT .F.
   DATA   cAlias
   DATA   aNames                                  INIT {}
   DATA   aInfo
   DATA   qHandle
   DATA   lExeOnServer                            INIT .F.
   DATA   cListPos                                INIT ""
   DATA   dataLink
   DATA   lDataBlock                              INIT .F.
   DATA   lFinished                               INIT .F.
   DATA   bWhenFetching
   DATA   nEvery                                  INIT 1000
   DATA   lBlockFetch                             INIT .F.

   /* TO export DATA TO */
   DATA   fileFormat                              INIT ""
   DATA   fileName                                INIT ""
   DATA   fileHandle

   METHOD new( hConxn, cSql )
   METHOD create( hConxn, cSql )
   METHOD skip()
   METHOD eof()                                   INLINE ::lEof
   METHOD destroy()
   METHOD close()
   METHOD getField( xField )
   METHOD getAllFields()
   METHOD fetchRecords( nCount )
   METHOD execute( aParam,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15 )
   METHOD prepare()

   METHOD getInfo( nInfo )

   METHOD getNumParam()                           INLINE ::getInfo( QRY_NUMPARAS    )
   METHOD getParTypes()                           INLINE ::getInfo( QRY_PARCLPTYPES )
   METHOD getParSqlTypes()                        INLINE ::getInfo( QRY_PARTYPES    )
   METHOD getNumCols()                            INLINE ::getInfo( QRY_NUMCOLS     )
   METHOD getColTypes()                           INLINE ::getInfo( QRY_COLCLPTYPES )
   METHOD getColSqlTypes()                        INLINE ::getInfo( QRY_COLTYPES    )
   METHOD getColNames()                           INLINE ::getInfo( QRY_COLNAMES    )
   METHOD getColNamesLen()                        INLINE ::getInfo( QRY_COLNAMESLEN )

   METHOD parseQryInfo( lFlag )
   METHOD createTable()
   METHOD prepareForExport()
   METHOD exportRecord( aRecord )
   METHOD execBlock( nCounter, nMode )

   DATA   preparedForExport                       INIT .F.

   ENDCLASS


METHOD getInfo( nInfo ) CLASS CacheSQLQuery
   LOCAL xValue

   IF ::hQuery > 0 .AND. ::hQuery <= Len( cacheQuery_ ) .AND. ! ( CacheQuery_[ ::hQuery ] == NIL )
      xValue := CacheQuery_[ ::hQuery, nInfo ]
   ENDIF

   RETURN xValue


METHOD new( hConxn, cSql ) CLASS CacheSQLQuery

   DEFAULT hConxn TO CacheSetConnection()

   ::hConxn := hConxn
   ::cSql   := cSql

   RETURN SELF


METHOD create( hConxn, cSql ) CLASS CacheSQLQuery

   CacheDebug( "METHOD Create", 0, cSql )

   DEFAULT hConxn TO ::hConxn
   DEFAULT cSql   TO ::cSql

   ::hConxn := hConxn
   ::cSql   := cSql

   DEFAULT ::hConnection TO ::hConxn

   IF ! ( ::hConxn == NIL ) .AND. ValType( ::hConxn ) == "N" .AND. ::hConxn > 0
      ::hQuery := CacheQuery_New( ::hConxn )

      IF ::hQuery > 0
         IF ! ( ::cSql == NIL ) .AND. ValType( ::cSql ) == "C" .AND. ! Empty( ::cSql )
            ::Prepare( cSql )
         ENDIF
      ELSE
         ::cLastError := "Memory could not been Allocated !"
      ENDIF
   ELSE
      ::cLastError := "Cache Connection Handle not Valid !"
   ENDIF

   ::lDataBlock := HB_ISBLOCK( ::dataLink )

   CacheDebug( "METHOD Create", 1, ::cLastError )
   RETURN SELF


METHOD Prepare() CLASS CacheSQLQuery

   CacheDebug( "METHOD Prepare()", 0, ::lPrepared, ::cSql, ::hQuery )

   IF ! ::lPrepared .AND. ::hQuery > 0
      IF ! ( ::cSql == NIL ) .AND. ValType( ::cSql ) == "C"

         ::lPrepared := CacheQuery_Prepare( ::hQuery, ::cSql )

         IF ! ::lPrepared
            ::cLastError := "Query could not been Prepared !"
         ELSE
#IF 0
            IF ::lExeOnServer
               ::parseQryInfo()
            ENDIF
#else
            ::parseQryInfo( .F. )
#endif
            ::nNumParas := CacheQuery_[ ::hQuery, QRY_NUMPARAS    ]
            ::aParTypes := CacheQuery_[ ::hQuery, QRY_PARCLPTYPES ]

            ::nNumCols  := CacheQuery_[ ::hQuery, QRY_NUMCOLS     ]
            ::aColTypes := CacheQuery_[ ::hQuery, QRY_COLCLPTYPES ]
            ::aColNames := CacheQuery_[ ::hQuery, QRY_COLNAMES    ]
         ENDIF
      ELSE
         ::cLastError := "SQL Statement not Provided !"
      ENDIF
   ENDIF

   CacheDebug( "METHOD Prepare()", 1, ::cLastError )
   RETURN ::lPrepared


METHOD Execute( aParam, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15 ) CLASS CacheSQLQuery
   LOCAL aP, nParams, cParam, cFileSpec, cNameSpace

   IF ! ::lPrepared
      RETURN SELF
   ENDIF

   aP := {}
   nParams := PCount()

   IF nParams == 1
      IF ! ( ValType( aParam ) == "A" )
         aP := { aParam }
      ELSE
         aP := aParam
      ENDIF

   ELSEIF nParams > 1
      AAdd( aP, aParam )
      AAdd( aP, p2 )
      IF ! ( p3 == NIL )
         AAdd( aP, p3 )
         IF ! ( p4 == NIL )
            AAdd( aP, p4 )
            IF ! ( p5 == NIL )
               AAdd( aP, p5 )
               IF ! ( p6 == NIL )
                  AAdd( aP, p6 )
                  IF ! ( p7 == NIL )
                     AAdd( aP, p7 )
                     IF ! ( p8 == NIL )
                        AAdd( aP, p8 )
                        IF ! ( p9 == NIL )
                           AAdd( aP, p9 )
                           IF ! ( p10 == NIL )
                              AAdd( aP, p10 )
                              IF ! ( p11 == NIL )
                                 AAdd( aP, p11 )
                                 IF ! ( p12 == NIL )
                                    AAdd( aP, p12 )
                                    IF ! ( p13 == NIL )
                                       AAdd( aP, p13 )
                                       IF ! ( p14 == NIL )
                                          AAdd( aP, p14 )
                                          IF ! ( p15 == NIL )
                                             AAdd( aP, p15 )
                                          ENDIF
                                       ENDIF
                                    ENDIF
                                 ENDIF
                              ENDIF
                           ENDIF
                        ENDIF
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   ::aParam := aP

   IF ::lExeOnServer
      cParam    := " "
      cFileSpec := " "

      IF !Empty( ::cTable )
         ::CreateTable()
         ::aNames := CacheResolveNames( ::cTable )

         AEval( ::aParam, {|e,i| cParam += ::aParTypes[ i ] + " " + RddXtoS( e ) +"|^|" } )
         cNameSpace := CacheGetConnectionInfo( 4, ::hConnection )
         cFileSpec := ::aNames[ NME_CACHECLASSNAME ] + "|^|" + cNameSpace + "|^|" + ::cListPos + "|^|"
      ENDIF

      CacheExeQuery( 1, ::qHandle, /*cSql*/, cParam, cFileSpec )
      ::lExecuted := .T.
   ELSE
      CacheQuery_Execute( ::hQuery, ::aParam )

      IF ! Empty( ::cTable )
         ::CreateTable()
      ENDIF

      ::lExecuted := .T.
      ::lEof      := .F.
      ::aRecords  := {}
   ENDIF

   RETURN ::lExecuted


METHOD createTable()  CLASS  CacheSQLQuery

   IF ! ( ::cTable == NIL )
      IF ! Empty( ::aStruct )
         IF Upper( ::cDriver ) == "CACHERDD"
            CacheDropTable( ::cTable, ::hConnection  )
         ENDIF

         dbCreate( ::cTable, ::aStruct, ::cDriver, , , , , ::hConnection )

         ::cAlias := "T" + LTrim( Str( hb_Random( 9999999 ), 7, 0 ) )

#ifndef __HARBOUR__
         USE ( ::cTable ) ALIAS ( ::cAlias ) NEW EXCLUSIVE VIA ( ::cDriver ) CONNECTION ( ::hConnection )
#else
         USE ( ::cTable ) ALIAS ( ::cAlias ) NEW EXCLUSIVE VIA ( ::cDriver )
#endif
         IF ! NetErr()
            ::cListPos := CacheGetListPos( NIL, ::hConnection )
            ::lPutInTable := .T.
         ENDIF
      ENDIF
   ENDIF

   RETURN SELF


METHOD parseQryInfo( lFlag ) CLASS  CacheSQLQuery
   LOCAL cInfo, i, cStatus, aInfo, d_, a_
   LOCAL cDlm  := "|^|"
   LOCAL aStruct := {}

   DEFAULT lFlag TO .T.

   cInfo := CacheExeQuery( 0, 0, ::cSql )
   CacheDebug( "METHOD ParseQryInfo", 0, cInfo, 1 )

   cStatus := SubStr( cInfo, 1, 1 )
   IF Empty( cStatus ) .OR. cStatus == "0"
      IF lFlag
         ::lPrepared := .F.
      ENDIF
      RETURN Self
   ENDIF

   aInfo := Str2A( cInfo, cDlm )

   IF ! Empty( aInfo )
      aInfo[ QF_STATUS   ] := Val( aInfo[ QF_STATUS   ] )
      aInfo[ QF_HANDLE   ] := Val( aInfo[ QF_HANDLE   ] )
      aInfo[ QF_STMTTYPE ] := Val( aInfo[ QF_STMTTYPE ] )
      aInfo[ QF_NUMPARAM ] := Val( aInfo[ QF_NUMPARAM ] )
      aInfo[ QF_NUMCOLS  ] := Val( aInfo[ QF_NUMCOLS  ] )

      IF aInfo[ QF_NUMCOLS ] > 0
         aInfo[ QF_COLTYPES ] := Str2A( aInfo[ QF_COLTYPES ], " " )

         a_:= Str2A( aInfo[ QF_STRUCT ], "|" )

         FOR i := 1 TO len( a_ )
            d_:= Str2A( a_[ i ], " " )

            d_[ 2 ] := Val( d_[ 2 ] )
            d_[ 3 ] := Val( d_[ 3 ] )
            d_[ 4 ] := Val( d_[ 4 ] )

            d_[ 3 ] += iif( d_[ 4 ] > 0, 1, 0 )  /* Due TO a bug IN ODBC driver info lapse */

            Odbc2Clip( d_ )

            AAdd( aStruct, d_ )
         NEXT

         aInfo[ QF_STRUCT ] := aStruct
      ELSE
         aInfo[ QF_STRUCT   ] := {}
         aInfo[ QF_COLTYPES ] := {}
      ENDIF

      ::aInfo   := aInfo
      ::aStruct := aInfo[ QF_STRUCT ]
      ::qHandle := aInfo[ QF_HANDLE ]
   ENDIF
   CacheDebug( "METHOD ParseQryInfo", 1 )
   RETURN SELF


METHOD Skip() CLASS CacheSQLQuery
   IF ! ::lEof
      ::lEof := ! CacheQuery_Next( ::hQuery )
   ENDIF
   RETURN ! ::lEof


METHOD GetField( xField ) CLASS CacheSQLQuery
   LOCAL xValue

   IF ::lExecuted .AND. !( ::lEof )
      IF ValType( xField ) == "N"
         xValue := CacheQuery_GetField( ::hQuery, xField )
      ELSEIF ValType( xField ) == "C"
         xValue := CacheQuery_GetFieldByName( ::hQuery, xField )
      ENDIF
   ENDIF

   RETURN xValue


METHOD GetAllFields() CLASS CacheSQLQuery
   LOCAL aValues := {}

   IF ::lExecuted .AND. ! ::lEof
      aValues := CacheQuery_GetFieldsAll( ::hQuery )
   ENDIF

   RETURN aValues


METHOD FetchRecords( nCount ) CLASS CacheSQLQuery
   LOCAL i, a_
   LOCAL nCounter := 0
   LOCAL bError := ErrorBlock( {|| Break() } )

   BEGIN SEQUENCE

   IF ! ::lExeOnServer
      ::prepareForExport()

      DEFAULT nCount TO 0

      ::lBlockFetch := ValType( ::bWhenFetching ) == "B"
      ::execBlock( nCounter, 0 )
      //
      IF nCount == 0
         DO WHILE ::Skip()
            nCounter++

            a_:= CacheQuery_GetFieldsAll( ::hQuery )

            IF ::preparedForExport
               ::exportRecord( a_ )
            ELSE
               IF ::lDataBlock
                  Eval( ::dataLink, a_ )
               ELSEIF ::lPutInTable
                  APPEND BLANK
                  AEval( a_, {|e,i| fieldput( i,e ) } )
               ELSE
                  AAdd( ::aRecords, a_ )
               ENDIF
            ENDIF

            ::execBlock( nCounter, 1 )
         ENDDO
         ::execBlock( nCounter, 2 )

         IF ::preparedForExport
            ::nNumOfRecords := nCounter
         ELSE
            ::nNumOfRecords := IF( ::lPutInTable, LastRec(), len( ::aRecords ) )
         ENDIF

         ::lFinished := .T.
      ELSE
         FOR i := 1 TO nCount
            IF ! ::Skip()
               EXIT
            ENDIF
            nCounter++

            a_:= CacheQuery_GetFieldsAll( ::hQuery )

            IF ::lPutInTable
               APPEND BLANK
               AEval( a_, {|e,i| fieldput( i,e ) } )
            ELSE
               AAdd( ::aRecords, a_ )
            ENDIF

            ::execBlock( nCounter, 1 )
         NEXT
         ::execBlock( nCounter, 2 )

         ::nNumOfRecords := IF( ::lPutInTable, LastRec(), len( ::aRecords ) )
      ENDIF
   ENDIF

   END SEQUENCE

   ErrorBlock( bError )

   RETURN ::aRecords


METHOD execBlock( nCounter, nMode ) CLASS CacheSQLQuery

   IF ::lBlockFetch
      IF nCounter % ::nEvery == 0
         Eval( ::bWhenFetching, nCounter, nMode )
      ENDIF
   ENDIF

   RETURN SELF


METHOD prepareForExport() CLASS CacheSQLQuery
   LOCAL cFile, cPath, cExt

   IF Empty( ::fileFormat )
      RETURN SELF
   ENDIF
   IF Empty( ::fileName )
      RETURN SELF
   ENDIF

   hb_fNameSplit( ::fileName, @cPath, @cFile, @cExt )
   cExt := "." + ::fileFormat
   ::fileName := cPath + cFile + cExt

   ::fileFormat := Upper( ::fileFormat )

   CacheDebug( "::fileName", ::fileName, ::fileFormat )

   SWITCH Upper( ::fileFormat )
   CASE "DBF"
      IF Empty( ::aStruct )
         RETURN SELF
      ENDIF

      DbCreate( ::fileName, ::aStruct, "DBFCDX" )
      USE ( ::fileName ) EXCLUSIVE NEW VIA "DBFCDX"

      IF NetErr()
         Alert( "ERROR opening file" )
         RETURN SELF
      ENDIF
      EXIT
   CASE "CSV"
   CASE "HTM"
   CASE "XML"
      ::fileHandle := fcreate( ::fileName, FC_NORMAL )
      IF ::fileHandle == -1
         RETURN SELF
      ENDIF
      EXIT
   OTHERWISE
      RETURN SELF

   ENDSWITCH

   ::preparedForExport := .T.

   RETURN SELF


METHOD ExportRecord( aRecord ) CLASS CacheSQLQuery
   LOCAL s, n

   n := Len( aRecord )

   SWITCH Upper( ::fileFormat )
   CASE "CSV"
      s := ""
      AEval( aRecord, {|e,i| s += X2Csv( e, ::aStruct[ i, 2 ] ) + iif( i == n, "", "," ) } )
      s += Chr( 13 ) + Chr( 10 )
      FWrite( ::fileHandle, s, Len( s ) )
      EXIT
   CASE "DBF"
      APPEND BLANK
      AEval( aRecord, {|e,i| FieldPut( i, e ) } )
      EXIT
   CASE "HTM"
   CASE "XML"
   ENDSWITCH

   RETURN SELF


STATIC FUNCTION X2Csv( x, cType )

   SWITCH cType
   CASE "C"
      IF ( '"' $ x ) .OR. ( "," $ x )
         RETURN '"' + StrTran( x, '"', '""' ) + '"'
      ELSEIF IsDigit( Left( x, 1 ) )
         RETURN '="' + x + '"'
      ELSE
         RETURN x
      ENDIF
      EXIT
   CASE "N" ; RETURN LTrim( Str( x ) )
   CASE "D" ; RETURN DToC( x )
   CASE "L" ; RETURN iif( x, "Yes", "No" )
   ENDSWITCH

   RETURN ""


METHOD Close() CLASS CacheSQLQuery

   IF ::hQuery > 0
      ::lClose   := CacheQuery_Close( ::hQuery )
      hb_gcall( .T. )

      IF ::lExeOnServer
         CacheExeQuery( 2, ::qHandle )
      ENDIF
   ENDIF

   RETURN ::lClose


METHOD Destroy() CLASS CacheSQLQuery
   LOCAL lDestroyed

   lDestroyed := CacheQuery_Destroy( ::hQuery )

   IF ::preparedForExport
      IF ::fileFormat == "DBF"
         dbCloseArea()
      ELSE
         FClose( ::fileHandle )
      ENDIF
   ENDIF

   IF ::lPutInTable
      Select( ::cAlias )
      dbCloseArea()
      Select( ::nArea )
   ENDIF

   ::hQuery    := 0
   ::hConxn    := 0
   ::lEof      := .T.
   ::lPrepared := .F.
   ::lExecuted := .F.
   SELF := NIL
   hb_gcall( .T. )

   RETURN lDestroyed


STATIC FUNCTION Str2A( cStr, cDel )
   LOCAL n, nlen
   LOCAL a_:= {}

   DEFAULT cDel TO ","
   nLen := Len( cDel )
   DO WHILE .T.
      IF ( n := At( cDel, cStr ) ) == 0
         EXIT
      ENDIF
      AAdd( a_, SubStr( cStr, 1, n - 1 ) )
      cStr := SubStr( cStr, n + nLen )
   ENDDO
   IF ! Empty( cStr )
      AAdd( a_, cStr )
   ENDIF
   RETURN a_


STATIC FUNCTION ODBC2Clip( d_ )

   SWITCH d_[ 2 ]
   CASE SQL_DOUBLE
      d_[ 2 ] := "N"
      d_[ 3 ] := 15
      d_[ 4 ] :=  4
      EXIT
   CASE SQL_NUMERIC
   CASE SQL_DECIMAL
   CASE SQL_INTEGER
   CASE SQL_SMALLINT
   CASE SQL_FLOAT
   CASE SQL_REAL
      d_[ 2 ] := "N"
      EXIT
   CASE SQL_CHAR
   CASE SQL_VARCHAR
      d_[ 2 ] := "C"
      EXIT
   CASE SQL_DATETIME
      d_[ 2 ] := "D"
      EXIT
   CASE -7
      d_[ 2 ] := "L"
      EXIT
   CASE -4
      d_[ 2 ] := "M"
      EXIT
   ENDSWITCH

   RETURN d_

