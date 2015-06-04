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
#include "dbinfo.ch"
#include "set.ch"
#include "fileio.ch"
#include "hbtrace.ch"


/* DBOPENINFO */
#define UR_OI_AREA                                1
#define UR_OI_NAME                                2
#define UR_OI_ALIAS                               3
#define UR_OI_SHARED                              4
#define UR_OI_READONLY                            5
#define UR_OI_CDPID                               6
#define UR_OI_CONNECT                             7
#define UR_OI_HEADER                              8
#define UR_OI_CARGO                               9
#define UR_OI_SIZE                                9


#define LEN_BUFFER                                31000


FUNCTION CacheMgrSetUsage( lYes )
   THREAD STATIC sYes := .F.
   LOCAL oYes := sYes
   IF ValType( lYes ) == "L"
      sYes := lYes
   ENDIF
   RETURN oYes


FUNCTION CacheMgrUseTblMgr( lExclusive )
   LOCAL cSchema    := "RDDMGR"
   LOCAL cTable     := "TABLEMGR"
   LOCAL cTableName := cSchema + "||" + cTable

   IF ! CacheExistTable( cTableName )
      IF ! CacheMgrCreateTable()
         Alert( "Sync table could not been created!." )
         RETURN .F.
      ENDIF
   ENDIF

   IF Select( "TableMGR" ) > 0
      Select( "TableMGR" )
      SET ORDER TO 1
      RETURN .T.
   ENDIF

   DEFAULT lExclusive TO .F.

   IF lExclusive
      USE ( cTableName ) NEW EXCLUSIVE  VIA "CACHERDD"
   ELSE
      USE ( cTableName ) ALIAS "TABLEMGR" NEW SHARED  VIA "CACHERDD"
   ENDIF

   IF ! NetErr()
      IF ! CacheExistIndex( cTableName )
         INDEX ON TableMGR->UncPath   TAG "UNCPATH"   TO ( cTableName ) ADDITIVE
         INDEX ON TableMGR->TableName TAG "TABLENAME" TO ( cTableName ) ADDITIVE
      ENDIF
   ENDIF

   SET ORDER TO 1
   RETURN ! NetErr()


FUNCTION CacheMgrCreateTable()
   LOCAL cSchema    := "RDDMGR"
   LOCAL cTable     := "TABLEMGR"
   LOCAL cTableName := cSchema + "||" + cTable
   LOCAL aStruct    := {}
   LOCAL nArea      := Select()
   LOCAL lRet

   IF CacheExistTable( cTableName )
      RETURN .F.
   ENDIF

   aadd( aStruct, { "UNCPATH"  , "C", 55, 0 } )
   aadd( aStruct, { "UPLOAD"   , "C",  3, 0 } )
   aadd( aStruct, { "INDEX"    , "C",  3, 0 } )
   aadd( aStruct, { "ULDDED"   , "C",  3, 0 } )
   aadd( aStruct, { "IXD"      , "C",  3, 0 } )
   aadd( aStruct, { "USEIT"    , "C",  3, 0 } )
   aadd( aStruct, { "TABLENAME", "C",  8, 0 } )
   aadd( aStruct, { "USEALIAS" , "C", 10, 0 } )
   aadd( aStruct, { "SCHEMA"   , "C", 10, 0 } )
   aadd( aStruct, { "NAMESPACE", "C", 10, 0 } )
   aadd( aStruct, { "SERVERIP" , "C", 15, 0 } )
   aadd( aStruct, { "UPLBGNDTE", "D",  8, 0 } )
   aadd( aStruct, { "UPLBGNTME", "C",  8, 0 } )
   aadd( aStruct, { "UPLENDDTE", "D",  8, 0 } )
   aadd( aStruct, { "UPLENDTME", "C",  8, 0 } )
   aadd( aStruct, { "RECORDS"  , "N", 12, 0 } )
   aadd( aStruct, { "INDEXSTAT", "C",100, 0 } )
   aadd( aStruct, { "RECSINTBL", "N", 12, 0 } )

   DbCreate( cTableName, aStruct, "CACHERDD" )
   lRet := ! NetErr()

   IF lRet
      USE ( cTableName ) ALIAS "TableMGR" NEW SHARED VIA "CACHERDD"
      IF ! NetErr()
         IF FLock()
            INDEX ON TableMGR->UncPath   TAG "UNCPATH"   TO ( cTableName ) ADDITIVE
            INDEX ON TableMGR->TableName TAG "TABLENAME" TO ( cTableName ) ADDITIVE
         ENDIF
         dbCloseArea()
      ENDIF
   ENDIF

   Select( nArea )
   RETURN lRet


STATIC FUNCTION AppendNewTable( cFile )
   LOCAL n, cTable
   LOCAL lRet := .F.

   IF ( n := RAt( "\", cFile ) ) > 0
      cTable := SubStr( cFile, n + 1 )
   ELSE
      cTable := cFile
   ENDIF
   IF ( n := At( ".", cTable ) ) > 0
      cTable := SubStr( cTable, 1, n - 1 )
   ENDIF

   IF Len( cTable ) <= 8
      APPEND BLANK

      REPLACE TableMGR->UncPath   WITH cFile
      REPLACE TableMGR->TableName WITH cTable
      REPLACE TableMGR->Schema    WITH "SQLUSER"
      lRet := .t.
   ENDIF

   RETURN lRet


FUNCTION CacheMgrUpdTblMgr( aFiles )
   LOCAL i, cFile
   LOCAL lRet := .F.
   LOCAL nArea := Select()

   IF ! ( ValType( aFiles ) == "A" ) .OR. Empty( aFiles )
      RETURN .F.
   ENDIF
   IF CacheMgrUseTblMgr()
      lRet := .t.
      FOR i := 1 TO Len( aFiles )
         cFile := Upper( Trim( aFiles[ i ] ) )
         IF ! dbSeek( cFile )
            AppendNewTable( cFile )
         ENDIF
      NEXT
      dbCommit()
      dbCloseArea()
   ENDIF
   Select( nArea )
   RETURN lRet


FUNCTION CacheMgrUploadOne( cDbf, lEraseIfExist, bExeBefore, bExeDuring, nEvery, bExeAfter )
   LOCAL lRet  := .T.
   LOCAL nArea := Select()

   IF ! ( ValType( cDbf ) == "C" ) .AND. Empty( cDbf )
      RETURN .f.
   ENDIF
   cDbf := Upper( trim( cDbf ) )

   DEFAULT lEraseIfExist TO .T.

   IF CacheMgrUseTblMgr()
      IF ! dbSeek( cDbf )
         lRet := AppendNewTable()
      ENDIF
      IF lRet
         CacheUploadCurrent( lEraseIfExist, bExeBefore, bExeDuring, nEvery, bExeAfter )
      ENDIF
      Select( "TableMGR" )
      dbCloseArea()
   ENDIF

   Select( nArea )
   RETURN lRet


FUNCTION CacheMgrUploadOneEx( cDbf, lEraseIfExist, bExeBefore, bExeDuring, nEvery, bExeAfter )
   LOCAL lRet  := .t.
   LOCAL nArea := Select()

   IF ! ( ValType( cDbf ) == "C" ) .AND. Empty( cDbf )
      RETURN .f.
   ENDIF
   cDbf := Upper( Trim( cDbf ) )

   DEFAULT lEraseIfExist TO .t.

   IF CacheMgrUseTblMgr()
      IF ! dbSeek( cDbf )
         lRet := AppendNewTable()
      ENDIF
      IF lRet
         CacheUploadCurrentEx( lEraseIfExist, bExeBefore, bExeDuring, nEvery, bExeAfter )
      ENDIF
      Select( "TableMGR" )
      dbCloseArea()
   ENDIF

   Select( nArea )
   RETURN lRet


FUNCTION CacheMgrUploadAll( lEraseIfExist, bExeBefore, bExeDuring, nEvery, bExeAfter )
   LOCAL lRet  := .t.
   LOCAL nArea := Select()

   DEFAULT lEraseIfExist TO .t.

   IF CacheMgrUseTblMgr()
      DbGotop()
      DO WHILE !eof()
         CacheUploadCurrent( lEraseIfExist, bExeBefore, bExeDuring, nEvery, bExeAfter )

         Select( "TableMGR" )
         DbSkip()
      ENDDO
      Select( "TableMGR" )
      DbCloseArea()
   ENDIF

   Select( nArea )
   RETURN lRet


STATIC FUNCTION CacheUploadCurrent( lEraseIfExist, bExeBefore, bExeDuring, nEvery, bExeAfter )
   LOCAL cDbf, cTableName, cTable, aStruct
   LOCAL nRecords, cSchema, lExit, nCurRec, nAppendMode, nRecsInTable

   IF TableMGR->Upload == "YES" .AND. TableMGR->Uldded == "   " .AND. RLock()
      cSchema    := trim( TableMGR->Schema )
      cTable     := trim( TableMGR->TableName )
      cTableName := cSchema + "||" + cTable

      lExit := .F.
      IF ! lEraseIfExist .AND. CacheExistTable( cTableName )
         lExit := .T.
      ENDIF

      IF ! lExit
         cDbf := Trim( TableMGR->UncPath )

         IF File( cDbf )
            USE ( cDbf ) NEW SHARED ALIAS "SOURCE" VIA "DBFCDX"

            IF ! NetErr()
               nRecords := LastRec()
               aStruct  := DbStruct()
               dbCloseArea()

               Select( "TableMGR" )
               CacheDropTable( cTableName )
               dbCreate( cTableName, aStruct, "CACHERDD" )

               USE ( cTableName ) NEW SHARED VIA "CACHERDD" ALIAS "DESTIN"
               IF ! NetErr()
                  nAppendMode := CacheInsertLockMode( 3 )

                  Select( "TableMGR" )
                  REPLACE TableMGR->UplBgnDte WITH CacheGetServerDate()
                  REPLACE TableMGR->UplBgnTme WITH CacheGetServerTime()
                  REPLACE TableMGR->Records   WITH nRecords
                  REPLACE TableMGR->UplEndDte WITH CToD( "" )
                  REPLACE TableMGR->UplEndTme WITH "  "
                  REPLACE TableMGR->Uldded    WITH "  "
                  TableMGR->( DbCommit() )
                  Select( "DESTIN" )

                  IF ValType( bExeBefore ) == "B"
                     Eval( bExeBefore, cDbf, nRecords )
                  ENDIF

                  IF ValType( bExeDuring ) == "B"
                     DEFAULT nEvery TO 10000

                     nCurRec := 0
                     Eval( bExeDuring, nCurRec )

                     APPEND FROM ( cDbf ) VIA "DBFCDX" FOR {|| nCurRec++, ;
                           iif( nCurRec%nEvery == 0, Eval( bExeDuring, nCurRec ), NIL ),  ! Deleted() }
                  ELSE
                     APPEND FROM ( cDbf ) VIA "DBFCDX" FOR ! Deleted()
                  ENDIF

                  nRecsInTable := LastRec()

                  IF valtype( bExeAfter ) == "B"
                     Eval( bExeAfter, cDbf, cTableName, nRecords, nRecsInTable )
                  ENDIF

                  SELECT( "DESTIN" )
                  DbCloseArea()

                  Select( "TableMGR" )
                  REPLACE TableMGR->UplEndDte WITH CacheGetServerDate()
                  REPLACE TableMGR->UplEndTme WITH CacheGetServerTime()
                  REPLACE TableMGR->Uldded    WITH "YES"
                  //REPLACE TableMGR->RecsInTbl WITH nRecsInTable
                  dbCommit()

                  CacheInsertLockMode( nAppendMode )
               ELSE
                  CacheDebug( "could not open" )
               ENDIF

               Select( "TableMGR" )
               dbCommit()
            ELSE
               CacheDebug( "Dbf could not been opened" )
            ENDIF
         ELSE
            CacheDebug( cDBF )
         ENDIF
      ELSE
         CacheDebug( "LEXIT = .T." )
      ENDIF

      Select( "TableMGR" )
      dbRUnlock()
   ELSE
      CacheDebug( "DDDD" )
   ENDIF

   RETURN .T.


FUNCTION CacheMgrIndexOne( cDbf, bExeBefore, bExeDuring, nEvery, bExeAfter )
   LOCAL lRet  := .T.
   LOCAL nArea := Select()

   IF ! ( ValType( cDbf ) == "C" ) .AND. Empty( cDbf )
      RETURN .f.
   ENDIF
   cDbf := Upper( Trim( cDbf ) )

   IF CacheMgrUseTblMgr()
      IF ( lRet := dbSeek( cDbf ) )
         CacheIndexCurrent( bExeBefore, bExeDuring, nEvery, bExeAfter )
      ENDIF
      Select( "TableMGR" )
      dbCloseArea()
   ENDIF

   Select( nArea )
   RETURN lRet


FUNCTION CacheMgrIndexAll( bExeBefore, bExeDuring, nEvery, bExeAfter )
   LOCAL lRet  := .T.
   LOCAL nArea := Select()

   IF CacheMgrUseTblMgr()
      dbGoTop()
      DO WHILE ! Eof()
         CacheIndexCurrent( bExeBefore, bExeDuring, nEvery, bExeAfter )
         Select( "TableMGR" )
         dbSkip()
      ENDDO

      Select( "TableMGR" )
      dbCloseArea()
   ENDIF

   Select( nArea )
   RETURN lRet


STATIC FUNCTION CacheIndexCurrent( bExeBefore, bExeDuring, nEvery, bExeAfter )
   LOCAL cDbf, cTableName, nSeconds, cTable
   LOCAL nRecords, cSchema, aIndex, i, cLog, cIdx

   HB_SYMBOL_UNUSED( bExeDuring )
   DEFAULT nEvery TO 1000

   // Just TO modify the index definitions
   // IF TableMGR->Uldded == "YES" .AND. RLock()
   //
   IF TableMGR->Ixd == "   " .AND. TableMGR->Uldded == "YES" .AND. RLock()
      cSchema    := Trim( TableMGR->Schema )
      cTable     := Trim( TableMGR->TableName )
      cTableName := cSchema + "||" + cTable
      cDbf       := Trim( TableMGR->UncPath )

      IF CacheExistTable( cTableName )
         USE ( cTableName ) NEW SHARED VIA "CACHERDD" ALIAS "DESTIN"
         IF ! NetErr()
            nRecords := LastRec()

            IF ValType( bExeBefore ) == "B"
               Eval( bExeBefore, cDbf, nRecords, cTable, cSchema )
            ENDIF

            REPLACE TableMGR->Ixd       WITH "  "
            REPLACE TableMGR->IndexStat WITH "  "
            TableMGR->( DbCommit() )

            // Need to have index definition in place
            //
            aIndex := CacheMgrIndexDefinition( cDbf )

            IF Valtype( aIndex ) == "A"
               IF Empty( aIndex )
                  cIdx := "WTH"
                  cLog := "   < NO INDEX DEFINED >"


               ELSEIF ValType( aIndex[ 1 ] ) == "A"
                  cLog    := ""
                  cIdx    := "YES"

                  FOR i := 1 TO Len( aIndex )
                     nSeconds := Seconds()

                     IF Len( aIndex[ i ] ) > 2 .AND. ! Empty( aIndex[ i,3 ] )    //  CONDITIONAL Index
                        INDEX ON &( aIndex[ i,2 ] ) TAG ( aIndex[ i,1 ] ) TO ( cTable ) FOR &( aIndex[ i,3 ] )
                     ELSE                                                        //  REGULAR Index
                        INDEX ON &( aIndex[ i,2 ] ) TAG ( aIndex[ i,1 ] ) TO ( cTable )
                     ENDIF

                     nSeconds := Abs( Seconds() - nSeconds )

                     cLog += "<" + NTRIM( i ) + ":" + LTrim( Str( nSeconds, 10, 0 ) ) + ">"

                     IF ValType( bExeAfter ) == "B"
                        Eval( bExeAfter, cLog )
                     ENDIF
                  NEXT

               ELSEIF ValType( aIndex[ 1 ] ) == "N"
                  cIdx := "   "
                  cLog := "   "
               ELSE
                  cIdx := "WTH"
                  cLog := "   < NO INDEX DEFINED >"
               ENDIF

               REPLACE TableMGR->Ixd       WITH cIdx
               REPLACE TableMGR->IndexStat WITH cLog
               TableMGR->( dbCommit() )
            ENDIF

            Select( "DESTIN" )
            dbCloseArea()
         ENDIF
      ENDIF

      Select( "TableMGR" )
      DbrUnlock()
   ENDIF

   RETURN .T.


//  Load Index definition off Multiple Sources.
//  1. Get/Set FUNCTION
//  2. API based repository
//  3. Disk based text file
//
FUNCTION CacheMgrIndexDefinition( cDbf )
   RETURN CacheMgrSetIndexDefinition( cDbf )


FUNCTION CacheMgrSetIndexDefinition( cDbf, aIndex )
   LOCAL lIndex, n

   THREAD STATIC sIndex := { { "", {} } }

   cDbf := Upper( Trim( cDbf ) )
   n    := AScan( sIndex, {|e_| e_[ 1 ] == cDbf } )

   lIndex := iif( n > 0, sIndex[ n,2 ], {} )

   IF ValType( aIndex ) == "A"
      IF n == 0
         AAdd( sIndex, { cDbf, aIndex } )
      ELSE
         sIndex[ n,2 ] := aIndex
      ENDIF
   ENDIF

   RETURN lIndex


STATIC FUNCTION CacheUploadCurrentEx( lEraseIfExist, bExeBefore, bExeDuring, nEvery, bExeAfter )
   LOCAL cDbf, cTableName, cTable, aStruct, nConxn
   LOCAL nRecords, cSchema, lExit, nCurRec, nDeletds
   LOCAL nAppendMode, nRecsInTable, aRecord, nFields, nWA, nAdded, nUploaded, nEmpty
   LOCAL bFor, cRec, nRec
   LOCAL cRecBuffer := ""
   LOCAL lExeDuring := valtype( bExeDuring ) == "B"
   LOCAL lSetDeleted := Set( _SET_DELETED, .t. )

   DEFAULT nEvery TO 10000

   IF TableMGR->Upload == "YES" .AND. TableMGR->Uldded == "   " .AND. RLock()
      cSchema    := trim( TableMGR->Schema )
      cTable     := trim( TableMGR->TableName )
      cTableName := cSchema + "||" + cTable

      lExit := .F.
      IF ! lEraseIfExist .AND. CacheExistTable( cTableName )
         lExit := .T.
      ENDIF

      IF ! lExit
         cDbf := Trim( TableMGR->UncPath )

         IF File( cDbf )
            USE ( cDbf ) NEW SHARED ALIAS "SOURCE" VIA "DBFCDX" READONLY

            IF ! NetErr()
               SET ORDER TO 0
               nRecords := LastRec()
               aStruct  := DbStruct()
               nFields  := FCount()

               Select( "TableMGR" )

               CacheDropTable( cTableName )
               DbCreate( cTableName, aStruct, "CACHERDD" )

               USE ( cTableName ) NEW SHARED VIA "CACHERDD" ALIAS "DESTIN"
               IF ! NetErr()
                  nConxn      := CacheSetConnection()
                  nWA         := Select()
                  nAppendMode := CacheInsertLockMode( 3 )

                  Select( "TableMGR" )
                  REPLACE TableMGR->UplBgnDte  WITH CacheGetServerDate()
                  REPLACE TableMGR->UplBgnTme  WITH CacheGetServerTime()
                  REPLACE TableMGR->Records    WITH nRecords
                  REPLACE TableMGR->UplEndDte  WITH CTOD("")
                  REPLACE TableMGR->UplEndTme  WITH "  "
                  REPLACE TableMGR->Uldded     WITH "  "
                  REPLACE TableMGR->RecsInTbl  WITH 0
                  REPLACE TableMGR->ServerIP   WITH "  "
                  TableMGR->( DbCommit() )

                  Select( "DESTIN" )
                  IF ValType( bExeBefore ) == "B"
                     Eval( bExeBefore, cDbf, nRecords )
                  ENDIF

                  Select( "SOURCE" )
                  dbGoTop()

                  IF lExeDuring
                     nCurRec := 0
                     bFor    := {|| nCurRec++, IF( nCurRec%nEvery == 0, Eval( bExeDuring, nCurRec ), NIL ), .T. }
                     Eval( bExeDuring, nCurRec )
                  ELSE
                     bFor    := {|| .T. }
                  ENDIF

                  aRecord   := Array( nFields )
                  nRec      := 0
                  nUploaded := 0
                  nEmpty    := 0
                  DO WHILE ! Eof()
                     IF Eval( bFor )
                        AEval( aRecord, {|e,i| HB_SYMBOL_UNUSED( e ), aRecord[ i ] := FieldGet( i ) } )

                        cRec := CacheRecToBuffer( aRecord )
#if 0
IF ( Chr( 0 ) $ cRec )
   CacheDebug( cTableName, "Record:" + NTRIM( Recno() ), FieldGet( 1 ) )
ENDIF
#endif
                        IF ! Empty( cRec )
                           IF ( Len( cRecBuffer ) + Len( cRec ) ) >= 31500
                              cRecBuffer := LTrim( Str( nRec ) ) + "[^]" + ;
                                            LTrim( Str( Len( cRecBuffer ) ) ) + "[^]" + ;
                                            cRecBuffer
                              nAdded     := CacheAppendRecords( nConxn, nWA, cRecBuffer, Len(cRecBuffer) )
                              nUpLoaded  += nAdded
#if 0
IF ! ( nAdded == nRec )
   CacheDebug( cTableName, "Sent:" + NTRIM( nRec ), "Inserted:" + NTRIM( nAdded ), nUploaded, nCurRec, "During" )
ENDIF
#endif
                              cRecBuffer := ( cRec + "\~.~/" )
                              nRec       := 1
                           ELSE
                              cRecBuffer += ( cRec + "\~.~/" )
                              nRec++
                           ENDIF
                        ELSE
                           nEmpty++
                        ENDIF
                     ENDIF
                     dbSkip()
                  ENDDO

                  IF ! Empty( cRecBuffer )
                     cRecBuffer := LTrim( Str( nRec ) ) + "[^]" + ;
                                   LTrim( Str( Len( cRecBuffer ) ) ) + "[^]" + ;
                                   cRecBuffer
                     nAdded     := CacheAppendRecords( nConxn, nWA, cRecBuffer, Len(cRecBuffer) )
                     nUpLoaded  += nAdded
#if 0
IF ! nAdded == nRec
   CacheDebug( nRec, nAdded, nUploaded, nCurRec, "Last" )
ENDIF
#endif
                  ENDIF

                  IF lExeDuring
                     Eval( bExeDuring, nCurRec )
                  ENDIF
#if 0
CacheDebug( nRec, nAdded, nUploaded, nEmpty, nCurRec, "Last" )
#endif
                  nRecsInTable := LastRec()

                  IF ValType( bExeAfter ) == "B"
                     Eval( bExeAfter, cDbf, cTableName, nRecords, nRecsInTable )
                  ENDIF

                  Select( "DESTIN" )
                  dbCloseArea()

                  Select( "SOURCE" )
                  Set( _SET_DELETED, .F. )
                  dbGoTop()
                  nDeletds := 0
                  DO WHILE ! Eof()
                     IF Deleted()
                        nDeletds++
                     ENDIF
                     dbSkip()
                  ENDDO
                  dbCloseArea()

                  Select( "TableMGR" )
                  REPLACE TableMGR->UplEndDte   WITH CacheGetServerDate()
                  REPLACE TableMGR->UplEndTme   WITH CacheGetServerTime()
                  REPLACE TableMGR->Uldded      WITH "YES"
                  REPLACE TableMGR->RecsInTbl   WITH nUpLoaded
                  REPLACE TableMGR->ServerIP    WITH LTrim( Str( nDeletds ) ) + ":" + LTrim( Str( nEmpty ) )
                  DbCommit()

                  CacheInsertLockMode( nAppendMode )
               ELSE
                  CacheDebug( "could not open" )
               ENDIF

               Select( "TableMGR" )
               dbCommit()
            ELSE
               CacheDebug( "Dbf could not been opened" )
            ENDIF
         ELSE
            CacheDebug( cDBF )
         ENDIF
      ELSE
         CacheDebug( "LEXIT = .T." )
      ENDIF

      Select( "TableMGR" )
      dbRUnlock()
   ELSE
      CacheDebug( "DDDD" )
   ENDIF

   Set( _SET_DELETED, lSetDeleted )
   RETURN .T.


FUNCTION CacheUploadByDbf( cDbf, cTable, cSchema, lEraseIfExist, bExeBefore, ;
                                                   bExeDuring, nEvery, bExeAfter )
   LOCAL cTableName, aStruct, nConxn
   LOCAL nRecords, lExit, nCurRec, nDeletds
   LOCAL nRecsInTable, aRecord, nFields, nWA, nAdded, nUploaded, nEmpty
   LOCAL bFor, cRec, nRec
   LOCAL cRecBuffer := ""
   LOCAL lExeDuring  := ValType( bExeDuring ) == "B"
   LOCAL lSetDeleted := Set( _SET_DELETED, .t. )

   DEFAULT cSchema       TO "SQLUSER"
   DEFAULT lEraseIfExist TO .F.
   DEFAULT nEvery        TO 10000

   cDbf    := Upper( Trim( cDbf ) )
   cTable  := Upper( cTable )
   cSchema := Upper( cSchema )

   IF .T.
      cTableName := cSchema + "||" + cTable

      lExit := .f.
      IF ! lExit
         IF File( cDbf )
            USE ( cDbf ) NEW SHARED ALIAS "SOURCE" VIA "DBFCDX" READONLY

            IF ! NetErr()
               SET ORDER TO 0
               nRecords := LastRec()
               aStruct  := DbStruct()
               nFields  := FCount()

               IF lEraseIfExist .OR. ! CacheExistTable( cTableName )
                  CacheDropTable( cTableName )
                  DbCreate( cTableName, aStruct, "CACHERDD" )
               ENDIF

               USE ( cTableName ) NEW SHARED VIA "CACHERDD" ALIAS "DESTIN"
               IF ! NetErr()
                  nConxn := CacheSetConnection()
                  nWA := Select()

                  InitLog( cDBF, cTable, cSchema, nRecords )

                  Select( "DESTIN" )
                  IF ValType( bExeBefore ) == "B"
                     Eval( bExeBefore, cDbf, nRecords )
                  ENDIF

                  Select( "SOURCE" )
                  dbGoTop()

                  IF lExeDuring
                     nCurRec := 0
                     bFor    := {|| nCurRec++, iif( nCurRec%nEvery == 0, Eval( bExeDuring, nCurRec ), NIL ), .T. }
                     Eval( bExeDuring, nCurRec )
                  ELSE
                     bFor    := {|| .T. }
                  ENDIF

                  aRecord   := Array( nFields )
                  nRec      := 0
                  nUploaded := 0
                  nEmpty    := 0
                  DO WHILE ! Eof()
                     IF Eval( bFor )
                        AEval( aRecord, {|e,i| HB_SYMBOL_UNUSED( e ), aRecord[ i ] := FieldGet( i ) } )

                        cRec := CacheRecToBuffer( aRecord )
#if 0
IF ( Chr( 0 ) $ cRec )
   CacheDebug( Alias(), "Record:" + NTRIM( Recno() ), fieldget( 1 ) )
ENDIF
#endif
                        IF ! Empty( cRec )
                           IF ( Len( cRecBuffer )+Len( cRec ) ) >= 31500
                              cRecBuffer := LTrim( Str( nRec ) ) + "[^]" + ;
                                            LTrim( Str( Len( cRecBuffer ) ) ) + "[^]" + ;
                                            cRecBuffer
                              nAdded     := CacheAppendRecords( nConxn, nWA, cRecBuffer, Len( cRecBuffer ) )
                              nUpLoaded  += nAdded
#if 0
IF ! nAdded == nRec
   CacheDebug( Alias(), "Sent:" + NTRIM( nRec ), "Inserted:" + NTRIM( nAdded ), nUploaded, nCurRec, "During" )
ENDIF
#endif
                              cRecBuffer := ( cRec + "\~.~/" )
                              nRec       := 1
                           ELSE
                              cRecBuffer += ( cRec + "\~.~/" )
                              nRec++
                           ENDIF
                        ELSE
                           nEmpty++
                        ENDIF
                     ENDIF
                     dbSkip()
                  ENDDO

                  IF ! Empty( cRecBuffer )
                     cRecBuffer := LTrim( Str( nRec ) ) + "[^]" + ;
                                   LTrim( Str( Len( cRecBuffer ) ) ) + "[^]" + ;
                                   cRecBuffer
                     nAdded     := CacheAppendRecords( nConxn, nWA, cRecBuffer, Len( cRecBuffer ) )
                     nUpLoaded  += nAdded
#if 0
IF ! nAdded == nRec
   CacheDebug( nRec, nAdded, nUploaded, nCurRec, "Last" )
ENDIF
#endif
                  ENDIF

                  IF lExeDuring
                     Eval( bExeDuring, nCurRec )
                  ENDIF
#if 0
CacheDebug( nRec, nAdded, nUploaded, nEmpty, nCurRec, "Last" )
#endif
                  nRecsInTable := LastRec()

                  IF ValType( bExeAfter ) == "B"
                     Eval( bExeAfter, cDbf, cTableName, nRecords, nRecsInTable )
                  ENDIF

                  Select( "DESTIN" )
                  dbCloseArea()

                  Select( "SOURCE" )
                  Set( _SET_DELETED, .F. )
                  DbGotop()
                  nDeletds := 0
                  DO WHILE ! Eof()
                     IF Deleted()
                        nDeletds++
                     ENDIF
                     dbSkip()
                  ENDDO
                  dbCloseArea()

                  FinalyzeLog( cDBF, nUploaded, nDeletds, nEmpty )
               ELSE
                  CacheDebug( "could not open" )
               ENDIF
            ELSE
               CacheDebug( "Dbf could not been opened" )
            ENDIF
         ELSE
            CacheDebug( cDBF )
         ENDIF
      ELSE
         CacheDebug( "LEXIT = .T." )
      ENDIF
   ELSE
      CacheDebug( "DDDD" )
   ENDIF

   Set( _SET_DELETED, lSetDeleted )
   RETURN .T.


STATIC FUNCTION InitLog( cDBF, cTable, cSchema, nRecords )
   LOCAL nArea := Select()

   IF CacheMgrUseTblMgr()
      IF !dbSeek( cDbf )
         dbAppend()
         REPLACE TableMGR->UncPath    WITH cDBF
         REPLACE TableMGR->Upload     WITH "YES"
         REPLACE TableMGR->TableName  WITH cTable
         REPLACE TableMGR->Schema     WITH cSchema
      ENDIF
      IF RLock()
         REPLACE TableMGR->UplBgnDte  WITH CacheGetServerDate()
         REPLACE TableMGR->UplBgnTme  WITH CacheGetServerTime()
         REPLACE TableMGR->Records    WITH nRecords
         REPLACE TableMGR->UplEndDte  WITH CToD( "" )
         REPLACE TableMGR->UplEndTme  WITH "  "
         REPLACE TableMGR->Uldded     WITH "  "
         REPLACE TableMGR->RecsInTbl  WITH 0
         REPLACE TableMGR->ServerIP   WITH "  "
         TableMGR->( dbCommit() )
      ENDIF
      USE
   ENDIF

   Select( nArea )
   RETURN NIL


STATIC FUNCTION FinalyzeLog( cDBF, nUploaded, nDeletds, nEmpty )
   LOCAL nArea := Select()

   IF CacheMgrUseTblMgr()
      IF dbSeek( cDbf ) .AND. RLock()
         REPLACE TableMGR->UplEndDte   WITH CacheGetServerDate()
         REPLACE TableMGR->UplEndTme   WITH CacheGetServerTime()
         REPLACE TableMGR->Uldded      WITH "YES"
         REPLACE TableMGR->RecsInTbl   WITH nUpLoaded
         REPLACE TableMGR->ServerIP    WITH LTrim( Str( nDeletds ) ) + ":" + LTrim( Str( nEmpty ) )
         dbCommit()
      ENDIF
      dbCloseArea()
   ENDIF
   Select( nArea )
   RETURN NIL


FUNCTION CacheUploadRaw( cDbf, cTable, cSchema, lEraseIfExist, bExeBefore, ;
                                                   bExeDuring, nEvery, bExeAfter )
   LOCAL cTableName, aStruct, nBuffLen, nHandle, cBuffer, nRead, nConxn
   LOCAL nRecords, lExit, nCurRec, nLenRec, nDeletds, nHdrSize
   LOCAL nRecsInTable, nFields, nWA, nAdded, nUploaded, nEmpty, nOff, bFor, cRec, nRec
   LOCAL nLoop, nLoops, cBuff64, i, nTotalLen, nRecsInBuff, nLastBatch
   LOCAL cRecBuffer := ""
   LOCAL off_:= {}
   LOCAL lExeDuring  := ValType( bExeDuring ) == "B"
   LOCAL lSetDeleted := Set( _SET_DELETED, .T. )

   DEFAULT cSchema       TO "SQLUSER"
   DEFAULT lEraseIfExist TO .F.
   DEFAULT nEvery        TO 10000

   cDbf    := Upper( Trim( cDbf ) )
   cTable  := Upper( cTable )
   cSchema := Upper( cSchema )

   IF .T.
      cTableName := cSchema + "||" + cTable

      lExit := .F.
      IF ! lExit
         IF File( cDbf )
            USE ( cDbf ) NEW SHARED ALIAS "SOURCE" VIA "DBFCDX" READONLY

            IF ! NetErr()
               SET ORDER TO 0

               nRecords  := LastRec()
               aStruct   := dbStruct()
               nFields   := FCount()
               nLenRec   := RecSize()
               nHdrSize  := Header()
               cBuffer   := Space( nLenRec )

               nOff := 2
               AEval( aStruct, {|e_| AAdd( off_, { nOff, e_[ 3 ] } ), nOff += e_[ 3 ] } )

               dbCloseArea()

               nHandle := FOpen( cDBF, FO_READ )
               IF nHandle == -1
                  CacheDebug( cDbf, "Table could not been opened!" )
                  RETURN NIL
               ENDIF
               // Position the file pointer at first byte where 1st record begins
               FSeek( nHandle, nHdrSize, FS_SET )

               IF lEraseIfExist .OR. ! CacheExistTable( cTableName )
                  CacheDropTable( cTableName )
                  dbCreate( cTableName, aStruct, "CACHERDD" )
               ENDIF
               USE ( cTableName ) NEW SHARED VIA "CACHERDD" ALIAS "DESTIN"
               IF NetErr()
                  FClose( nHandle )
               ELSE
                  nConxn := CacheSetConnection()
                  nWA := Select()

                  InitLog( cDBF, cTable, cSchema, nRecords )

                  Select( "DESTIN" )
                  IF ValType( bExeBefore ) == "B"
                     Eval( bExeBefore, cDbf, nRecords )
                  ENDIF

                  IF lExeDuring
                     nCurRec := 0
                     bFor    := {|| nCurRec++, iif( nCurRec%nEvery == 0, Eval( bExeDuring, nCurRec ), NIL ), .T. }
                     Eval( bExeDuring, nCurRec )
                  ELSE
                     bFor    := {|| .T. }
                  ENDIF

                  nRec      := 0
                  nUploaded := 0
                  nEmpty    := 0
                  nDeletds  := 0

                  nTotalLen   := ( nRecords * nLenRec )
                  nBuffLen    := nLenRec * Int( ( LEN_BUFFER / nLenRec ) + 1 )
                  nRecsInBuff := nBuffLen / nLenRec
                  nLastBatch  := ( nTotalLen % nBuffLen ) / nLenRec
                  nLoops      := Int( nTotalLen / nBuffLen ) + 1

                  CacheDebug( "---------------------", nTotalLen, nBuffLen, nRecsInBuff, nLastBatch, nRecords, nLoops )

                  cBuff64     := Space( nBuffLen )

                  FOR nLoop := 1 TO nLoops
                     nRead := FRead( nHandle, @cBuff64, nBuffLen )
                     IF ! ( nRead == nBuffLen )
                        cBuffer += SubStr( cBuff64,1,nRead )
                     ELSE
                        cBuffer += cBuff64
                     ENDIF

                     IF nLoop == nLoops
                        nRecsInBuff := nLastBatch
                     ENDIF

                     FOR i := 1 TO nRecsInBuff
                        IF Eval( bFor )
                           cRec := CacheRawToBuffer( @cBuffer, off_, nFields, aStruct, @nDeletds, nLenRec )
                           IF ! Empty( cRec )
                              IF ( Len( cRecBuffer ) + Len( cRec ) ) >= LEN_BUFFER + 5000  // 500
                                 cRecBuffer := LTrim( Str( nRec ) ) + "[^]" + ;
                                               LTrim( Str( Len( cRecBuffer ) ) ) + "[^]" + ;
                                               cRecBuffer
                                 nAdded     := CacheAppendRecords( nConxn, nWA, cRecBuffer, Len( cRecBuffer ) )
                                 nUpLoaded  += nAdded
                                 cRecBuffer := ( cRec + "\~.~/" )
                                 nRec       := 1
                              ELSE
                                 cRecBuffer += ( cRec + "\~.~/" )
                                 nRec++
                              ENDIF
                           ELSE
                              nEmpty++
                           ENDIF
                        ENDIF
                     NEXT
                  NEXT

                  IF ! Empty( cRecBuffer )
                     cRecBuffer := LTrim( Str( nRec ) ) + "[^]" + ;
                                   LTrim( Str( Len( cRecBuffer ) ) ) + "[^]" + ;
                                   cRecBuffer
                     nAdded     := CacheAppendRecords( nConxn, nWA, cRecBuffer, Len( cRecBuffer ) )
                     nUpLoaded  += nAdded
                  ENDIF

                  IF lExeDuring
                     Eval( bExeDuring, nCurRec )
                  ENDIF

                  nRecsInTable := LastRec()
                  Select( "DESTIN" )
                  dbCloseArea()

                  IF ValType( bExeAfter ) == "B"
                     Eval( bExeAfter, cDbf, cTableName, nRecords, nRecsInTable )
                  ENDIF

                  FinalyzeLog( cDBF, nUploaded, nDeletds, nEmpty )
               ENDIF

               FClose( nHandle )
            ELSE
               CacheDebug( "Dbf could not been opened" )
            ENDIF
         ELSE
            CacheDebug( cDBF )
         ENDIF
      ELSE
         CacheDebug( "LEXIT = .T." )
      ENDIF
   ELSE
      CacheDebug( "DDDD" )
   ENDIF

   Set( _SET_DELETED, lSetDeleted )
   RETURN .t.


STATIC FUNCTION CacheRawToBuffer( cBuff, off_, nFields, aStruct, nDeletds, nLenRec )
   LOCAL nField, cField, cRec
   LOCAL cInfo := ""
   local cFields := ""
   LOCAL nn := 0

   cRec  := SubStr( cBuff, 1, nLenRec )
   cBuff := SubStr( cBuff, nLenRec + 1 )

   IF Left( cRec, 1 ) == " "
      FOR nField := 1 TO nFields
         cField := AllTrim( SubStr( cRec, off_[ nField, 1 ], off_[ nField, 2 ] ) )

         IF ! Empty( iif( aStruct[ nField, 2 ] == "N", Val( cField ), cField ) )
            cFields += + LTrim( Str( nField, 4, 0 ) ) + ";"
            nn++
            cInfo += cField + "|~|"
         ENDIF
      NEXT

      IF nn > 0
         cInfo := LTrim( Str( nn, 5, 0 ) ) + "|^|" + cFields + "|^|" + cInfo + "|^|"
      ENDIF
   ELSE
      nDeletds++
   ENDIF
   RETURN cInfo


FUNCTION CacheUploadViaServer( cDbf, cTable, cSchema, lEraseIfExist, bExeBefore, ;
                                                   bExeDuring, nEvery, bExeAfter )
   LOCAL cTableName, aStruct, nBuffLen
   LOCAL nRecords, lExit, nCurRec, nLenRec, nDeletds, nHdrSize
   LOCAL nRecsInTable, nFields, nWA, nUploaded, nEmpty, nOff
   LOCAL nLoops, nTotalLen, nRecsInBuff, nLastBatch, nConxn, cDbfAtSvr, cc
   LOCAL lExeDuring  := ValType( bExeDuring ) == "B"
   LOCAL lSetDeleted := Set( _SET_DELETED, .t. )
   LOCAL off_:={}

   DEFAULT cSchema       TO "SQLUSER"
   DEFAULT lEraseIfExist TO .F.
   DEFAULT nEvery        TO 10000

   cDbf    := Upper( trim( cDbf ) )
   cTable  := Upper( cTable )
   cSchema := Upper( cSchema )

   // Just for testings
   //
   hb_fNameSplit( cDbf, , @cc )
   cDbfAtSvr := "/cache-data2/upload/" + Lower( cc ) + ".dbf"

   IF .T.
      cTableName := cSchema + "||" + cTable

      lExit := .f.
      IF ! lExit
         IF File( cDbf )
            USE ( cDbf ) NEW SHARED ALIAS "SOURCE" VIA "DBFCDX" READONLY

            IF ! NetErr()
               SET ORDER TO 0

               nRecords  := LastRec()
               aStruct   := DbStruct()
               nFields   := FCount()
               nLenRec   := RecSize()
               nHdrSize  := Header()
               nOff      := 2
               AEval( aStruct, {|e_| aadd( off_, { nOff,e_[ 3 ] } ), nOff += e_[ 3 ] } )

               DbCloseArea()

               IF lEraseIfExist .OR. ! CacheExistTable( cTableName )
                  CacheDropTable( cTableName )
                  DbCreate( cTableName, aStruct, "CACHERDD" )
               ENDIF
               USE ( cTableName ) NEW SHARED VIA "CACHERDD" ALIAS "DESTIN"
               IF ! NetErr()
                  nConxn := CacheSetConnection()
                  nWA := Select()

                  InitLog( cDBF, cTable, cSchema, nRecords )

                  Select( "DESTIN" )
                  IF valtype( bExeBefore ) == "B"
                     Eval( bExeBefore, cDbf, nRecords )
                  ENDIF

                  IF lExeDuring
                     nCurRec := 0
                     Eval( bExeDuring, nCurRec )
                  ENDIF

                  nUploaded := 0
                  nEmpty    := 0
                  nDeletds  := 0

                  nTotalLen   := ( nRecords * nLenRec )
                  nBuffLen    := nLenRec * Int( ( LEN_BUFFER / nLenRec ) + 1 )
                  nRecsInBuff := nBuffLen / nLenRec
                  nLastBatch  := ( nTotalLen % nBuffLen ) / nLenRec
                  nLoops      := Int( nTotalLen / nBuffLen )+1

                  CacheAppendViaServer( nConxn, nWA, cDbfAtSvr, nHdrSize, nLenRec, ;
                                    nBuffLen, nRecsInBuff, nLastBatch, nLoops, nFields, off_ )

                  IF lExeDuring
                     Eval( bExeDuring, nCurRec )
                  ENDIF

                  nRecsInTable := LastRec()
                  Select( "DESTIN" )
                  dbCloseArea()

                  IF ValType( bExeAfter ) == "B"
                     Eval( bExeAfter, cDbf, cTableName, nRecords, nRecsInTable )
                  ENDIF

                  FinalyzeLog( cDBF, nUploaded, nDeletds, nEmpty )
               ENDIF

            ELSE
               CacheDebug( "Dbf could not been opened" )
            ENDIF
         ELSE
            CacheDebug( cDBF )
         ENDIF
      ELSE
         CacheDebug( "LEXIT = .T." )
      ENDIF
   ELSE
      CacheDebug( "DDDD" )
   ENDIF

   Set( _SET_DELETED, lSetDeleted )
   RETURN .t.


STATIC FUNCTION CacheAppendViaServer( nConxn, nWA, cDBF, nHdrSize, nLenRec, ;
                             nBuffLen, nRecsInBuff, nLastBatch, nLoops, nFields, off_ )
   LOCAL cInfo, cUplInfo
   LOCAL aInfo := {}
   LOCAL s := ""

   cDBF := Lower( cDBF )

   AEval( off_, {|e_| s += NTRIM( e_[1] ) + "." + NTRIM( e_[ 2 ] ) + ";" } )

   cUplInfo := cDbf                 + "|" + ;
               NTRIM( nHdrSize    ) + "|" + ;
               NTRIM( nLenRec     ) + "|" + ;
               NTRIM( nBuffLen    ) + "|" + ;
               NTRIM( nRecsInBuff ) + "|" + ;
               NTRIM( nLastBatch  ) + "|" + ;
               NTRIM( nLoops      ) + "|" + ;
               NTRIM( nFields     ) + "|" + ;
               s                    + "|"

   cInfo := CacheSetGet( nConxn, nWA, 1700, cUplInfo )
   IF ! Empty( cInfo )
      // Parse it AND put IN aInfo
   ENDIF
   RETURN aInfo


FUNCTION CacheUploadByBuffer( cDbf, cTable, cSchema, lEraseIfExist, bExeBefore, bExeDuring, nEvery, bExeAfter )
   LOCAL cTableName, aStruct, nBuffLen, nHandle
   LOCAL nRecords, nLenRec, nDeletds, nHdrSize
   LOCAL nRecsInTable, nFields, nWA, nUploaded, nEmpty, nOff
   LOCAL nLoop, nLoops, cBuff64, nTotalLen, nRecsInBuff, nLastBatch
   LOCAL nConxn, cMode, aInfo

   LOCAL lExeDuring  := ValType( bExeDuring ) == "B"
   LOCAL lSetDeleted := Set( _SET_DELETED, .t. )
   LOCAL off_ := {}
   LOCAL lRet := .F.
   LOCAL nCurRec := 0

   DEFAULT cSchema       TO "SQLUSER"
   DEFAULT lEraseIfExist TO .F.
   DEFAULT nEvery        TO 10000

   cDbf    := Upper( Trim( cDbf ) )
   cTable  := Upper( cTable )
   cSchema := Upper( cSchema )

   cTableName := cSchema + "||" + cTable

   IF File( cDbf )
      USE ( cDbf ) NEW SHARED VIA "DBFCDX" ALIAS "SOURCE" READONLY

      IF ! NetErr()
         nHdrSize  := Header()
         nLenRec   := RecSize()
         nRecords  := LastRec()
         nFields   := FCount()
         aStruct   := dbStruct()

         dbCloseArea()

         nOff := 2
         AEval( aStruct, {|e_| AAdd( off_, { nOff, e_[ 3 ] } ), nOff += e_[ 3 ] } )

         nHandle := FOpen( cDBF, FO_READ + FO_SHARED )
         IF nHandle == -1
            CacheDebug( cDbf, "Table could not been opened!" )
            RETURN NIL
         ENDIF
         // Position the file pointer at first byte where 1st record begins
         FSeek( nHandle, nHdrSize, FS_SET )

         IF ( lEraseIfExist ) .OR. ! CacheExistTable( cTableName )
            CacheDropTable( cTableName )
            dbCreate( cTableName, aStruct, "CACHERDD" )
         ENDIF
         USE ( cTableName ) NEW SHARED VIA "CACHERDD" ALIAS "DESTIN"
         IF NetErr()
            FClose( nHandle )

         ELSE
            nConxn      := CacheSetConnection()
            nWA         := Select()

            InitLog( cDBF, cTable, cSchema, nRecords )

            Select( "DESTIN" )
            IF ValType( bExeBefore ) == "B"
               Eval( bExeBefore, cDbf, nRecords )
            ENDIF

            IF lExeDuring
               Eval( bExeDuring, 0 )
            ENDIF

            nEmpty      := 0
            nTotalLen   := ( nRecords * nLenRec )
            nBuffLen    := nLenRec * max( 1, Int( LEN_BUFFER / nLenRec ) )
            nRecsInBuff := nBuffLen / nLenRec
            nLastBatch  := ( nTotalLen % nBuffLen ) / nLenRec
            nLoops      := Int( nTotalLen / nBuffLen ) + 1

            CacheDebug( "nRecords", nRecords, "nLenRec:", nLenRec, "nRecsInBuff:", nRecsInBuff, ;
                                 "nTotalLen:", nTotalLen, "nBuffLen:", nBuffLen, "nLoops:", nLoops )

            cMode := "BEGIN"
            IF CacheByBuffer( nConxn, nWA, cMode, { nLenRec, nRecsInBuff, nLastBatch, nFields, off_ }, cTable )

               cBuff64 := space( nBuffLen )
               cMode   := "CHUNK"
               FOR nLoop := 1 TO nLoops
                  FRead( nHandle, @cBuff64, nBuffLen )

                  IF nLoop == nLoops
                     cMode := "LAST."
                     nRecsInBuff := nLastBatch
                  ENDIF

                  aInfo := CacheByBuffer( nConxn, nWA, cMode, @cBuff64, cTable )

                  nCurRec += nRecsInBuff

                  IF aInfo[ 1 ] + aInfo[ 2 ] < nRecsInBuff
                     CacheDebug( Left( cBuff64, nLenRec ) )
                  ENDIF

                  IF lExeDuring
                     IF nCurRec >= nEvery
                        nCurRec := 0
                        Eval( bExeDuring, aInfo[ 1 ] + aInfo[ 2 ] )
                     ENDIF
                  ENDIF
               NEXT
            ENDIF

            IF lExeDuring
               Eval( bExeDuring, aInfo[ 1 ] + aInfo[ 2 ] )
            ENDIF

            nUploaded := aInfo[ 1 ]
            nDeletds  := aInfo[ 2 ]

            nRecsInTable := LastRec()
            Select( "DESTIN" )
            dbCloseArea()

            IF ValType( bExeAfter ) == "B"
               Eval( bExeAfter, cDbf, cTableName, nRecords, nRecsInTable )
            ENDIF

            FinalyzeLog( cDBF, nUploaded, nDeletds, nEmpty )
         ENDIF

         FClose( nHandle )
         lRet := .T.
      ELSE
         CacheDebug( cDBF + ":" + " could not been opened!" )
      ENDIF
   ELSE
      CacheDebug( cDBF + ":" + " Not present on device!" )
   ENDIF

   Set( _SET_DELETED, lSetDeleted )
   RETURN lRet


STATIC FUNCTION CacheByBuffer( nConxn, nWA, cMode, xData, cTable )
   LOCAL cUplInfo, n, cInfo
   LOCAL s      := ""
   LOCAL aInfo  := {0,0}
   LOCAL cParam := cMode + "\/"

   DO CASE
   CASE cMode == "BEGIN"
      CacheDebug( "BEGIN", cTable, time(), xData[ 1 ], xData[ 2 ], CacheGetConnectionInfo( 2 ) )

      AEval( xData[ 5 ], {|e_| s += NTRIM( e_[ 1 ] ) + "." + NTRIM( e_[ 2 ] ) + ";" } )

      cUplInfo := NTRIM( xData[ 1 ]/*nLenRec*/     ) + "|" +;
                  NTRIM( xData[ 2 ]/*nRecsInBuff*/ ) + "|" +;
                  NTRIM( xData[ 3 ]/*nLastBatch*/  ) + "|" +;
                  NTRIM( xData[ 4 ]/*nFields*/     ) + "|" +;
                  s                                  + "|"
      cParam += cUplInfo
      cInfo := CacheSetGet( nConxn, nWA, 1705, cParam )

      RETURN cInfo == "DONE"

   CASE cMode == "CHUNK"
      cParam += xData
      cInfo := CacheSetGet( nConxn, nWA, 1705, cParam )
      n     := At( ":", cInfo )
      aInfo := { Val( SubStr( cInfo, 1, n - 1 ) ), Val( SubStr( cInfo, n + 1 ) ) }

   CASE cMode == "LAST."
      cParam += xData
      cInfo := CacheSetGet( nConxn, nWA, 1705, cParam )
      n     := At( ":", cInfo )
      aInfo := { Val( SubStr( cInfo, 1, n - 1 ) ), Val( SubStr( cInfo, n + 1 ) ) }

      CacheDebug( "END  ", cTable, time(), cInfo )
   ENDCASE

   RETURN aInfo


FUNCTION CacheDownloadByBuffer( cTable, cSchema, cDbf, lEraseIfExist, bExeBefore, ;
                                                   bExeDuring, nEvery, bExeAfter )
   LOCAL nWA, cRes, nCols, nRecords, cMsg, aStr, n
   LOCAL lRet       := .t.
   LOCAL nConxn     := CacheSetConnection()
   LOCAL recDelim   := Chr( 15 ) + Chr( 16 ) + Chr( 15 ) //"/.\"
   LOCAL fldDelim   := Chr( 21 ) + Chr( 22 ) + Chr( 21 ) //"|~|"
   LOCAL lExeDuring := ValType( bExeDuring ) == "B"
   LOCAL nRecs := 0, nn := 0

   DEFAULT cSchema       TO "SQLUSER"
   DEFAULT lEraseIfExist TO .F.
   DEFAULT nEvery        TO 10000

   HB_SYMBOL_UNUSED( bExeAfter )

   cDbf    := Upper( Trim( cDbf ) )
   cTable  := Upper( cTable )
   cSchema := Upper( cSchema )

   USE ( cSchema + "||" + cTable ) NEW SHARED VIA "CACHERDD" ALIAS "SOURCE"
   IF NetErr()
      RETURN .f.
   ENDIF

   aStr     := dbStruct()
   nCols    := Len( aStr )
   nWA      := Select()
   nRecords := LastRec()

   dbCreate( cDBF, aStr, "DBFCDX" )
   USE ( cDBF ) NEW EXCLUSIVE ALIAS "TARGET"
   IF NetErr()
      dbCloseAll()
      RETURN .f.
   ENDIF

   IF ValType( bExeBefore ) == "B"
      Eval( bExeBefore, cDbf, nRecords )
   ENDIF
   IF lExeDuring
      Eval( bExeDuring, 0 )
   ENDIF

   cRes := CacheSetGet( nConxn, nWA, 1706, NTRIM( RecSize() ) )
   cMsg := cRes
   CacheDebug( Pad( cTable, 10 ), "Download Begins:", Time(), nRecords, cMsg )

   IF cRes == "NORECORDS"
   ELSEIF cRes == "ERROR"
   ELSE
      DO WHILE .T.
         cRes := CacheSetGet( nConxn, nWA, 1706, "0" )
         IF Empty( cRes ) .OR. cRes == "ERROR" .OR. cRes == "EOF"
            cMsg := cRes
            EXIT
         ENDIF

         n := AppendRecords( @cRes, aStr, @nCols, @recDelim, @fldDelim )
         nn += n
         nRecs += n

         IF lExeDuring
            IF nn > nEvery
               nn := 0
               Eval( bExeDuring, nRecs )
            ENDIF
         ENDIF

         IF Inkey() == 27
            EXIT
         ENDIF
      ENDDO
      IF lExeDuring
         Eval( bExeDuring, nRecs )
      ENDIF
   ENDIF

   CacheDebug( Pad( cTable, 10 ), 'Download Ends  :', time(), nRecs, Pad( cMsg,10 ) )

   dbCloseAll()
   RETURN lRet


STATIC FUNCTION AppendRecords( cRes, aStr, nCols, recDelim, fldDelim )
   LOCAL i, j, aRec, aRecs, nRecs
   LOCAL bError := ErrorBlock( {|| __ErrorBreak() } )

   aRecs := hb_aTokens( @cRes, recDelim )
   nRecs := Len( aRecs ) - 1
   FOR j := 1 TO nRecs
      aRec := hb_aTokens( aRecs[ j ], fldDelim )
      APPEND BLANK
      BEGIN SEQUENCE
      FOR i := 1 TO nCols
         IF ! Empty( aRec[ i ] )
            FieldPut( i, RddStoX( aRec[ i ], i, aStr ) )
         ENDIF
      NEXT
      RECOVER
      END SEQUENCE
   NEXT
   ErrorBlock( bError )
   RETURN nRecs


FUNCTION __ErrorBreak()
   IF .T.
      BREAK
   ENDIF
   RETURN NIL


FUNCTION CacheMgrTfrSrv2Srv( cTable, nSrcConxn, nTgtConxn, lEraseIfExist, bExeBefore, ;
                                                          bExeDuring, nEvery, bExeAfter )
   LOCAL nWA, cRes, nRecSize, nRecords, nConxn, cReturn, aStr, n, cSchema, nWATgt
   LOCAL lSuccess := .F.
   LOCAL lExeDuring := ValType( bExeDuring ) == "B"
   LOCAL nRecs := 0
   local nn := 0

   DEFAULT cSchema       TO "SQLUSER"
   DEFAULT lEraseIfExist TO .F.
   DEFAULT nEvery        TO 10000

   HB_SYMBOL_UNUSED( bExeAfter )

   cTable  := Upper( cTable )
   cSchema := Upper( cSchema )

   nConxn := CacheSetConnection( nSrcConxn )

   USE ( cSchema +"||"+ cTable ) NEW SHARED VIA "CACHERDD" ALIAS "SOURCE"
   IF NetErr()
      RETURN .f.
   ENDIF

   aStr     := dbStruct()
   nWA      := Select()
   nRecords := LastRec()
   nRecSize := RecSize()

   CacheSetConnection( nTgtConxn )
   dbCreate( cTable, aStr, "CACHERDD" )
   USE ( cSchema +"||"+ cTable ) NEW SHARED VIA "CACHERDD" ALIAS "TARGET"
   nWATgt := Select()

   CacheSetConnection( nSrcConxn )
   Select( "SOURCE" )

   IF ValType( bExeBefore ) == "B"
      Eval( bExeBefore, cTable, nRecords )
   ENDIF
   IF lExeDuring
      Eval( bExeDuring, 0 )
   ENDIF

#if 0
   xLog := Vou_LogInit( "c:\MyLog.log" )
   CacheDebug( Pad( cTable,10 ),"Transfer Begins:", time(), nRecords, nRecSize, nSrcConxn, nTgtConxn )
#endif

   cRes := CacheSetGet( nSrcConxn, nWA, 1707, NTRIM( nRecSize ) )

   IF     cRes == "NORECORDS"
   ELSEIF cRes == "ERROR"
   ELSE
      DO WHILE .t.
         cRes := CacheSetGet( nSrcConxn, nWA, 1707, "0" )
         IF Empty( cRes ) .OR. cRes == "ERROR" .OR. cRes == "EOF"
            EXIT
         ENDIF
#if 0
         Vou_Log( xLog, cRes )
#endif
         cReturn := CacheSetGet( nTgtConxn, nWATgt, 1708, cRes )
         n := Val( cReturn )
         nn += n
         nRecs += n

         IF lExeDuring
            IF nn > nEvery
               nn := 0
               Eval( bExeDuring, nRecs )
            ENDIF
         ENDIF

         IF Inkey() == 27
            EXIT
         ENDIF
      ENDDO
      IF lExeDuring
         Eval( bExeDuring, nRecs )
      ENDIF
   ENDIF
#if 0
   Vou_LogEnd( xLog )
   CacheDebug( "Ends:",time() )
#endif

   Select( "SOURCE" )
   dbCloseArea()
   CacheSetConnection( nTgtConxn )
   Select( "TARGET" )
   dbCloseArea()

   CacheSetConnection( nConxn )
   RETURN lSuccess


FUNCTION CacheRecToBuffer( aRecord )
   LOCAL nField
   LOCAL cInfo   := ""
   LOCAL cFields := ""
   LOCAL nn      := 0
   LOCAL nFields := Len( aRecord )

   FOR nField := 1 TO nFields
      IF ! Empty( aRecord[ nField ] ) .AND. iif( ValType( aRecord[ nField ] ) == "C", ! aRecord[ nField ] == Chr( 0 ), .T. )
         cFields := cFields + LTrim( Str( nField,4,0 ) ) + ";"
         nn++
         cInfo := cInfo + RddXtoS( aRecord[ nField ] ) + "|~|"
      ENDIF
   NEXT
   IF nn > 0
      cInfo := LTrim( Str( nn, 5, 0 ) ) + "|^|" + cFields + "|^|" + cInfo + "|^|"
   ENDIF
   RETURN cInfo

//--------------------------------------------------------------------//
//             Logging Functions - Called Inside CacheRDD
//--------------------------------------------------------------------//

FUNCTION CacheDebug( p1, p2, p3, p4, p5, p6, p7, p8, p9, p10 )
   LOCAL cDebug := ""

   IF ! ( p1 == NIL )
      cDebug += uiXtos( p1 )
   ENDIF
   IF ! ( p2 == NIL )
      cDebug += "   " + uiXtos( p2 )
   ENDIF
   IF ! ( p3 == NIL )
      cDebug += "   " + uiXtos( p3 )
   ENDIF
   IF ! ( p4 == NIL )
      cDebug += "   " + uiXtos( p4 )
   ENDIF
   IF ! ( p5 == NIL )
      cDebug += "   " + uiXtos( p5 )
   ENDIF
   IF ! ( p6 == NIL )
      cDebug += "   " + uiXtos( p6 )
   ENDIF
   IF ! ( p7 == NIL )
      cDebug += "   " + uiXtos( p7 )
   ENDIF
   IF ! ( p8 == NIL )
      cDebug += "   " + uiXtos( p8 )
   ENDIF
   IF ! ( p9 == NIL )
      cDebug += "   " + uiXtos( p9 )
   ENDIF
   IF ! ( p10 == NIL )
      cDebug += "   " + uiXtos( p10 )
   ENDIF

   HB_TRACE( HB_TR_ALWAYS, cDebug )

   RETURN NIL


STATIC FUNCTION uiXtos( xVar )

   SWITCH ValType( xVar )
   CASE "C" ; RETURN xVar
   CASE "N" ; RETURN Str( xVar )
   CASE "I" ; RETURN Str( xVar )
   CASE "D" ; RETURN dtoc( xVar )
   CASE "L" ; RETURN iif( xVar, "Yes", "No " )
   CASE "M" ; RETURN ""
   END
   RETURN ""


FUNCTION __rddDebug( nWA, cAction, xSome, xSome1, xSome2 )
   LOCAL aWAData, nNext
   LOCAL bError := ErrorBlock( {|| NIL } )

   HB_SYMBOL_UNUSED( cAction )
   HB_SYMBOL_UNUSED( xSome   )

   BEGIN SEQUENCE
   IF ! ( nWA == NIL ) .AND. nWA > 0
      aWAData  := USRRDD_AREADATA( nWA )
      IF ! ( aWAData == NIL )
         IF .T.
         /*
         CacheDebug( Pad( cAlias,8 ),;
                  Pad( aWAData[ WA_CACHENAME ], 8 ),;
                  Str( nWA, 2 ),;
                  Str( aWAData[ WA_RECNO ],8,0 ),;
                  Pad( StrTran(ProcName( 1 ),'CACHE_','$_'),10 ),;
                  Pad( StrTran(ProcName( 2 ),'CACHE_','$_'),10 ),;
                  '<'+Pad( ProcName( 3 ),10 )+Str(ProcLine(3),5)+'>',;
                  '<'+Pad( ProcName( 4 ),10 )+Str(ProcLine(4),5)+'>',;
                  xSome1, ;
                  xSome2, ;
               )
         */
         /*
         CacheDebug( ;
                  Pad( cAlias, 8 ),;
                  Str( nWA, 2 ),;
                  Str( aWAData[ WA_RECNO ],8,0 ),;
                  Pad( StrTran(ProcName( 2 ),'CACHE_','$_'),10 ),;
                  Pad( StrTran(ProcName( 3 ),'CACHE_','$_'),10 ),;
                  '<'+Pad( ProcName( 4 ),10 )+Str(ProcLine(4),5)+'>',;
                  '<'+Pad( ProcName( 5 ),10 )+Str(ProcLine(5),5)+'>',;
                  xSome1, ;
                  xSome2, ;
               )
         */
         /*
         CacheDebug( ;
                  Pad( aWAData[ WA_CACHENAME ], 8 ),;
                  Str( nWA, 2 ),;
                  Str( aWAData[ WA_RECNO ],8,0 ),;
                  Pad( StrTran(ProcName( 1 ),'CACHE_','$_'),10 ),;
                  Pad( StrTran(ProcName( 2 ),'CACHE_','$_'),10 ),;
                  '<'+Pad( ProcName( 3 ),10 )+Str( ProcLine( 3 ),5 )+'>',;
                  '<'+Pad( ProcName( 4 ),10 )+Str( ProcLine( 4 ),5 )+'>',;
                  xSome1, ;
                  xSome2, ;
               )
         */
         nNext := iif( ProcName( 1 ) $ "CACHE_GETVALUE,CACHE_PUTVALUE",1,0 )
         CacheDebug( ;
                  Pad( aWAData[ WA_CACHENAME ], 8 )                                  ,;
                  Str( nWA, 4 )                                                      ,;
                  Str( aWAData[ WA_RECNO ],8,0 )                                     ,;
                  Pad( StrTran( ProcName( 1 ), "CACHE_", "$_" ), 10 )                ,;
                  Pad( StrTran( ProcName( 2-nNext ), "CACHE_", "$_"), 10 )           ,;
                  "<"+Pad( StrTran( ProcName( 3-nNext ), "CACHE_", "$_" ), 10 ) + Str( ProcLine( 3-nNext ),5 ) + ">" ,;
                  "<"+Pad( ProcName( 4-nNext ),10 )+Str( ProcLine( 4-nNext ),5 )+">" ,;
                  "<"+Pad( ProcName( 5-nNext ),10 )+Str( ProcLine( 5-nNext ),5 )+">" ,;
                  xSome1, ;
                  xSome2, ;
               )
         ENDIF
      ENDIF
   ENDIF
   END SEQUENCE

   ErrorBlock( bError )
   RETURN NIL

#if 0
STATIC FUNCTION Vou_LogInit( cFile )
   LOCAL nHandle

   nHandle := FCreate( cFile, FO_READWRITE )
   IF nHandle == -1
      RETURN NIL
   ENDIF
   RETURN nHandle


STATIC FUNCTION Vou_Log( xLog, cBuffer )

   IF ! xLog == NIL
      FSeek( xLog, 0, FS_END )
      FWrite( xLog, cBuffer, Len( cBuffer ) )
   ENDIF
   RETURN NIL


STATIC FUNCTION Vou_LogEnd( xLog )

   IF ! xLog == NIL
      FClose( xLog )
   ENDIF
   RETURN NIL
#endif

FUNCTION dbgTraceLog( cString, cFile  )
   LOCAL lRet := .F.
   LOCAL nHandle, nBytes

   DEFAULT cFile TO "DbgTrace.Log"

   IF ( nHandle := FOpen( cFile, FO_WRITE ) ) == F_ERROR
      IF ( nHandle := FCreate( cFile ) ) == F_ERROR
         RETURN .F.
      ENDIF
   ENDIF
   IF ! ( nHandle == F_ERROR )
      FSeek( nHandle, 0, FS_END )
      nBytes := FWrite( nHandle, cString + Chr( 13 ) + Chr( 10 ), Len( cString ) + 2 )

      lRet := nBytes == Len( cString ) + 2

      FClose( nHandle )
   ENDIF
   RETURN lRet


