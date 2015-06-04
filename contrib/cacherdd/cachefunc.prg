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
#include "fileio.ch"


STATIC lConnected   := .F.
STATIC aConnections := {}
STATIC nCurrConxn   := -1

//----------------------------------------------------------------------//
//                       Connections Management
//----------------------------------------------------------------------//

FUNCTION CacheRDDVersion()
   RETURN "1.0"


FUNCTION CacheSetServerParams( cServerIpOrAddress, nPort, cUserName, cPassword, nTimeout )
   LOCAL aParam

   STATIC sParam := {}

   aParam := aclone( sParam )
   IF ! ( PCount() == 0 )
      sParam := { cServerIpOrAddress, nPort, cUserName, cPassword, nTimeout }
   ENDIF

   RETURN aParam


STATIC FUNCTION CacheMapPackages( cNameSpace )
   LOCAL n, cInfo
   LOCAL lSuccess := .T.
   LOCAL aParam   := CacheSetServerParams()
   LOCAL nConxn   := CacheSetConnection()
   LOCAL cConxn   := aParam[ 1 ] + "[" + NTRIM( aParam[ 2 ] ) + "]:" + "CACHERDD"

   IF ( n := CacheConnect( cConxn, aParam[ 3 ], aParam[ 4 ], aParam[ 5 ] ) ) > 0

      cInfo    := CacheSetGet( n, 0, 107, cNameSpace )
      lSuccess := cInfo == "DONE"

      CacheDisconnectThis( n )
   ELSE
      //? 'Cannot connect TO CacheRDD'
   ENDIF
   CacheSetConnection( nConxn )

   RETURN lSuccess


FUNCTION CacheAddConnection( cNameSpace )
   LOCAL nConxn, n, n1, cConxn, cServerIP, cPort
   LOCAL aParam := CacheSetServerParams()

   cNameSpace := Upper( cNameSpace )
   cConxn     := aParam[ 1 ] + "[" + NTRIM( aParam[ 2 ] ) + "]:" + cNameSpace

   IF ( nConxn := AScan( aConnections, {|e_| e_[ 6 ] == cConxn } ) ) == 0
      CacheMapPackages( cNameSpace )

      nConxn := CacheConnect( cConxn, aParam[ 3 ], aParam[ 4 ], aParam[ 5 ] )
      IF nConxn >= 1
         cServerIP  := SubStr( cConxn, 1     , ( n  := At( "[", cConxn ) ) - 1   )
         cPort      := SubStr( cConxn, n +  1, ( n1 := At( "]", cConxn ) ) - 1 - n )
         cNameSpace := SubStr( cConxn, n1 + 2 )

         IF ( n := AScan( aConnections, {|e_| e_[ 1 ] == NIL } ) ) == 0
            AAdd( aConnections, { nConxn, cServerIP, cPort, cNameSpace, "SQLUSER", cConxn, 0 } )
         ELSE
            aConnections[ n ] := { nConxn, cServerIP, cPort, cNameSpace, "SQLUSER", cConxn, 0 }
         ENDIF

         nCurrConxn := nConxn
      ENDIF
   ELSE
      nCurrConxn := nConxn
   ENDIF

   RETURN nConxn


FUNCTION CacheAddConnectionEx( cServerIP, nPort, cUserName, cPassWord, nTimeOut, cNameSpace )
   LOCAL cConxn, nConxn, n
   LOCAL cPort  := LTrim( Str( nPort, 10, 0 ) )

   cServerIP  := Upper( cServerIP  )
   cNameSpace := Upper( cNameSpace )
   cConxn     := cServerIP + "[" + cPort + "]:" + cNameSpace

   IF Empty( aConnections ) .OR. ( n := AScan( aConnections, {|e_| e_[ 6 ] == cConxn } ) ) == 0
      AAdd( aConnections, { NIL, cServerIP, cPort, cNameSpace, "SQLUSER", cConxn, 0 } )
      n := Len( aConnections )
   ENDIF

   IF aConnections[ n, 1 ] == NIL
      CacheMapPackages( cNameSpace )
      nConxn := CacheConnect( cConxn, cUserName, cPassWord, nTimeOut )
      IF nConxn >= 1
         aConnections[ n, 1 ] := nConxn
      ENDIF
   ENDIF

   IF !Empty( aConnections[ n, 1 ] )
      nCurrConxn := aConnections[ n, 1 ]
      nConxn     := aConnections[ n, 1 ]
   ELSE
      nConxn := 0
   ENDIF

   RETURN nConxn


FUNCTION CacheSetConnection( nConxn )
   LOCAL n
   LOCAL nOld := nCurrConxn

   IF ValType( nConxn ) == 'N'
      IF ( n := AScan( aConnections, {|e_| e_[ 1 ] == nConxn } ) ) > 0
         IF ! ( aConnections[ n, 1 ] == NIL )
            nCurrConxn := aConnections[ n, 1 ]
         ENDIF
      ENDIF
   ENDIF

   RETURN nOld


FUNCTION CacheCloseConnection( nConxn )
   LOCAL n, lSuccess := .F.

   IF ValType( nConxn ) == 'N'
      IF ( n := AScan( aConnections, {|e_| e_[ 1 ] == nConxn } ) ) > 0
         IF ! ( aConnections[ n, 1 ] == NIL )
            IF ( lSuccess := CacheDisconnectThis( aConnections[ n, 1 ] ) )
               aConnections[ n, 1 ] := NIL
               aConnections[ n, 6 ] := NIL
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   RETURN lSuccess


FUNCTION CacheGetConnectionInfo( nInfo, nConxn )
   LOCAL xValue, n

   DEFAULT nConxn TO CacheSetConnection()

   IF nInfo <= DBCI_SIZE
      IF ( n := AScan( aConnections, {|e_| e_[ 1 ] == nConxn } ) ) > 0
         IF ! ( aConnections[ n, 1 ] == NIL )
            xValue := aConnections[ n, nInfo ]
         ENDIF
      ENDIF
   ENDIF

   RETURN xValue


FUNCTION Cache_CurConxn()
   RETURN CacheSetConnection()


FUNCTION Cache_DefConxn( nConxn )
   IF ValType( nConxn ) == "N" .AND. nConxn > 0 .AND. nConxn <= Len( aConnections )
      RETURN nConxn
   ENDIF
   RETURN iif( Used() .AND. RDDName() == "CACHERDD", USRRDD_AREADATA( Select() )[ WA_CONXN ], CacheSetConnection() )

//----------------------------------------------------------------------//
//                           Utility Functions
//----------------------------------------------------------------------//

FUNCTION CacheNameSplit( cTable, cSchema )
   LOCAL n

   /* parse fullname if schema name has been supplied */
   IF ( n := At( "||", cTable ) ) > 0
      cSchema  := SubStr( cTable, 1, n - 1 )
      cTable   := SubStr( cTable, n + 2 )
      IF ! CacheSetSchemaAsIs()
         cSchema := Upper( cSchema )
      ENDIF
   ELSE
      cSchema := CacheSetSchema()                 // 'SQLUSER'
   ENDIF

   IF Empty( cSchema )
      cSchema := CacheSetSchema()
   ENDIF
   RETURN NIL


FUNCTION CacheSetTableNameAsIs( lYes )
   LOCAL  lUpper
   STATIC sUpper := .F.

   lUpper := sUpper

   IF ValType( lYes ) == 'L'
      sUpper := lYes
   ENDIF

   RETURN lUpper


FUNCTION CacheSetSchemaAsIs( lYes )
   LOCAL  lUpper
   STATIC sUpper := .F.

   lUpper := sUpper

   IF ValType( lYes ) == 'L'
      sUpper := lYes
   ENDIF

   RETURN lUpper


FUNCTION CacheSetSchema( cSchema, nConxn )
   LOCAL n, oSchema

   DEFAULT nConxn TO CacheSetConnection()

   IF ( n := AScan( aConnections, {|e_| e_[ 1 ] == nConxn } ) ) > 0
      IF ! ( aConnections[ n, 1 ] == NIL )
         oSchema := aConnections[ n, DBCI_SCHEMA ]
         IF ValType( cSchema ) == 'C' .AND. ! Empty( cSchema )
            aConnections[ n, DBCI_SCHEMA ] := cSchema
         ENDIF
      ENDIF
   ENDIF

   RETURN oSchema


FUNCTION CacheSetLocks( acLocks, nConxn )
   LOCAL i
   LOCAL cLockStr := ''

   DEFAULT nConxn TO CacheSetConnection()

   IF ValType( acLocks ) == 'C'
      acLocks := { acLocks }
   ENDIF

   cLockStr += NTRIM( Len( acLocks ) ) + '~'
   FOR i := 1 TO Len( acLocks )
      cLockStr += acLocks[ i ] + '~'
   NEXT

   RETURN CacheManageLocks( nConxn, 1, cLockStr )


FUNCTION CacheReleaseLocks( acLocks, nConxn )
   LOCAL i
   LOCAL cLockStr := ''

   DEFAULT nConxn TO CacheSetConnection()

   IF ValType( acLocks ) == 'C'
      acLocks := { acLocks }
   ENDIF

   cLockStr += NTRIM( Len( acLocks ) ) + '~'
   FOR i := 1 TO Len( acLocks )
      cLockStr += acLocks[ i ] + '~'
   NEXT

   RETURN CacheManageLocks( nConxn, 2, cLockStr )


FUNCTION CacheGetCurrentProcessID( nConxn )
   DEFAULT nConxn TO CacheSetConnection()
   RETURN Val( CacheSetGet( nConxn, 0, 53, ' ' ) )


// As this function could be used to set a field value keep this as is
//
FUNCTION CacheGetServerDate( nConxn )
   RETURN SToD( CacheDateTime( Cache_DefConxn( nConxn ), 1 ) )


// As this function could be used to set a field value keep this as is
//
FUNCTION CacheGetServerTime( nConxn )
   RETURN CacheDateTime( Cache_DefConxn( nConxn ), 2 )


FUNCTION CacheExistTable( cTable, nConxn )
   DEFAULT nConxn TO CacheSetConnection()
   RETURN CacheSetGet( nConxn, 0, CACHE_EXISTTABLE, CacheResolveNames( cTable )[ NME_CACHECLASSNAME ] ) == 'YES'


FUNCTION CacheDropTable( cTable, nConxn )
   DEFAULT nConxn TO CacheSetConnection()
   RETURN CacheSetGet( nConxn, 0, CACHE_DROPTABLE, CacheResolveNames( cTable )[ NME_CACHECLASSNAME ] )


FUNCTION CacheTuneTable( cTable, nConxn )
   DEFAULT nConxn TO CacheSetConnection()
   RETURN CacheSetGet( nConxn, 0, CACHE_TUNETABLE, CacheResolveNames( cTable )[ NME_CACHECLASSNAME ] )


FUNCTION CacheExistIndex( cTable, nConxn )
   LOCAL cRet := CacheSetGet( Cache_DefConxn( nConxn ), Select(), CACHE_EXISTINDEXBYBAG, ;
                                                CacheResolveNames( cTable )[ NME_CACHECLASSNAME ] )
   RETURN ( cRet == 'YES' )


FUNCTION CacheExistIndexByTag( cTag )
   LOCAL cRet := CacheSetGet( Cache_DefConxn(), Select(), CACHE_EXISTINDEXBYTAG, Upper( cTag ) )
   RETURN ( cRet == 'YES' )


FUNCTION CacheDropIndex( cTable, nConxn )
   RETURN CacheSetGet( Cache_DefConxn( nConxn ), 0, CACHE_DROPINDEX, ;
                                 CacheResolveNames( cTable )[ NME_CACHECLASSNAME ] )


FUNCTION CacheBeginTransaction( nConxn )
   DEFAULT nConxn TO CacheSetConnection()
   RETURN CacheTStart( nConxn ) == 0


FUNCTION CacheEndTransaction( nConxn )
   DEFAULT nConxn TO CacheSetConnection()
   RETURN CacheTCommit( nConxn ) == 0


FUNCTION CacheRollBackTransaction( nConxn )
   DEFAULT nConxn TO CacheSetConnection()
   RETURN CacheTRollback( nConxn ) == 0


FUNCTION CacheTransactionLevel( nConxn )
   DEFAULT nConxn TO CacheSetConnection()
   RETURN CacheTLevel( nConxn )


FUNCTION CacheInsertLockMode( nMode, nConxn )
   DEFAULT nMode  TO -1
   DEFAULT nConxn TO CacheSetConnection()
   RETURN CacheLockMode( nConxn, nMode )


FUNCTION CacheLockTimeout( nSeconds, nConxn )
   DEFAULT nSeconds TO -1
   DEFAULT nConxn TO CacheSetConnection()
   RETURN CacheLockSeconds( nConxn, nSeconds )


FUNCTION CacheStopJournal( nConxn )
   DEFAULT nConxn TO CacheSetConnection()
   RETURN CacheSetGet( nConxn, 0, 111, " " ) == "DONE"


FUNCTION CacheStartJournal( nConxn )
   DEFAULT nConxn TO CacheSetConnection()
   RETURN CacheSetGet( nConxn, 0, 112, " " ) == "DONE"


FUNCTION CacheGetTables( cSchema, nConxn )
   LOCAL cTables, n, cTable
   LOCAL aTables := {}

   DEFAULT cSchema TO CacheSetSchema( , nConxn )
   DEFAULT nConxn  TO CacheSetConnection()

   cTables := CacheSetGet( nConxn, 0, 113, cSchema )

   DO WHILE .T.
      IF ( n := At( '|',cTables ) ) == 0
         EXIT
      ENDIF
      IF !Empty( cTable := SubStr( cTables, 1, n - 1 ) )
         AAdd( aTables, cTable )
      ENDIF
      cTables := SubStr( cTables, n + 1 )
   ENDDO

   RETURN aTables


FUNCTION CacheGetTableSizes( cDestDir, nConxn )
   LOCAL cTables, n1, cTmp, a_
   LOCAL aTables := {}

   DEFAULT nConxn  TO CacheSetConnection()

   cTables := CacheSetGet( nConxn, 0, 1903, cDestDir )

   DO WHILE .T.
      IF ( n1 := At( '|', cTables ) ) == 0
         EXIT
      ENDIF
      IF ! Empty( cTmp := SubStr( cTables, 1, n1 - 1 ) )
         a_:= hb_ATokens( cTmp, " " )
         IF Len( a_ ) >= 3
            a_[ 2 ] := Val( a_[ 2 ] )
            a_[ 3 ] := Val( a_[ 3 ] )
            AAdd( aTables, a_ )
         ENDIF
      ENDIF
      cTables := SubStr( cTables, n1 + 1 )
   ENDDO

   RETURN aTables


STATIC FUNCTION ParseTableInfo( cInfo )
   LOCAL i, n, a_, aInfo
   LOCAL b_:= {}

   IF ! Empty( cInfo )
      a_:= hb_ATokens( cInfo, ";" )

      FOR i := 1 TO Len( a_ )
         IF ( n := At( "=", a_[ i ] ) ) > 0
            AAdd( b_, { AllTrim( SubStr( a_[ i ], 1, n - 1 ) ), AllTrim( SubStr( a_[ i ], n + 1 ) ) } )
         ENDIF
      NEXT
   ENDIF

   aInfo := AFill( Array( 6 ), "" )
   aInfo[ 1 ] := CToD( "" )
   IF ! Empty( b_ )
      FOR i := 1 TO Len( aInfo )
         DO CASE
         CASE b_[ i,1 ] == "CreationDate"
            aInfo[ 1 ] := SToD( b_[ i, 2 ] )
         CASE b_[ i,1 ] == "CreationTime"
            aInfo[ 2 ] := b_[ i, 2 ]
         CASE b_[ i,1 ] == "UserName"
            aInfo[ 3 ] := b_[ i, 2 ]
         CASE b_[ i,1 ] == "IP"
            aInfo[ 4 ] := b_[ i, 2 ]
         CASE b_[ i,1 ] == "Node"
            aInfo[ 5 ] := b_[ i, 2 ]
         CASE b_[ i,1 ] == "Executable"
            aInfo[ 6 ] := b_[ i, 2 ]
         ENDCASE
      NEXT
   ENDIF

   RETURN aInfo


FUNCTION CacheGetTableInfoByName( cTable, nConxn )
   DEFAULT nConxn TO CacheSetConnection()
   RETURN ParseTableInfo( CacheSetGet( nConxn, 0, CACHE_TABLEINFO, CacheResolveNames( cTable )[ NME_CACHECLASSNAME ] ) )


FUNCTION CacheGetTableInfoByArea( nWA, nConxn )
   DEFAULT nConxn TO CacheSetConnection()
   RETURN ParseTableInfo( CacheSetGet( nConxn, nWA, CACHE_TABLEINFO, " " ) )


FUNCTION CacheGetLastError( nWA, nConxn )
   DEFAULT nConxn TO CacheSetConnection()
   RETURN CacheSetGet( nConxn, nWA, CACHE_LASTERROR, " " )


FUNCTION CacheGetLockTableInfo( nConxn )
   DEFAULT nConxn TO CacheSetConnection()
   RETURN CacheSetGet( nConxn, 0, 1071, " " )


FUNCTION CacheGetProcesses( nConxn )
   LOCAL n, i, cRec, cJobs, d_
   LOCAL aProcesses := {}

   DEFAULT nConxn TO CacheSetConnection()

   cJobs := CacheSetGet( nConxn, 0, 105, " " )

   DO WHILE .T.
      IF ( n := At( "||", cJobs ) ) == 0
         EXIT
      ENDIF
      cRec := SubStr( cJobs, 1, n - 1 )
      cJobs := SubStr( cJobs, n + 2 )

      d_:= {}
      FOR i := 1 TO 10
         n := At( "~", cRec )
         AAdd( d_, SubStr( cRec, 1, n - 1 ) )
         cRec := SubStr( cRec, n + 1 )
      NEXT
      AAdd( d_, cRec )
      AAdd( aProcesses, d_ )
   ENDDO

   RETURN aProcesses


FUNCTION CacheGetProcessInfo( nProcess, nConxn )
   LOCAL n, i, cRec
   LOCAL aProcesses := {}
   LOCAL d_ := {}

   DEFAULT nConxn TO CacheSetConnection()

   cRec := CacheSetGet( nConxn, 0, 118, NTRIM( nProcess ) )

   FOR i := 1 TO 10
      n := At( "~", cRec )
      AAdd( d_, SubStr( cRec, 1, n - 1 ) )
      cRec := SubStr( cRec, n + 1 )
   NEXT
   AAdd( d_, cRec )
   AAdd( aProcesses, d_ )

   RETURN aProcesses


FUNCTION CacheGetCurrentNameSpace( nConxn )
   DEFAULT nConxn TO CacheSetConnection()
   RETURN CacheSetGet( nConxn, 0, 1902, " " )


FUNCTION CacheSetUserInfo( cInfo, nConxn )
   DEFAULT cInfo  TO " "
   DEFAULT nConxn TO CacheSetConnection()
   RETURN CacheSetGet( nConxn, 0, 123, cInfo )


FUNCTION CacheGetVersionInfo( nConxn )
   DEFAULT nConxn TO CacheSetConnection()
   RETURN __str2Array( CacheSetGet( nConxn, 0, 131, "VERSIONINFO" ), "|" )


FUNCTION CacheGetLicenseInfo( nConxn )
   DEFAULT nConxn TO CacheSetConnection()
   RETURN __str2Array( CacheSetGet( nConxn, 0, 131, "LICENSEINFO" ),"|" )


FUNCTION CacheGetInstallInfo( nConxn )
   DEFAULT nConxn TO CacheSetConnection()
   RETURN __str2Array( CacheSetGet( nConxn, 0, 131, "INSTALLINFO" ),"|" )


//----------------------------------------------------------------------//
// The function is designed to bypass acquisition of locks for shared/exclusive use
//
// nMode == 0 DEFAULT, as Clipper
// nMode == 1 Treat Exclusive Use as Shared and apply FLock() to succeed
// nMode == 2 Deny Exclusive Use and Raise an error
//
// This function be called immediately after CacheAddConnection() to be effective.
//
FUNCTION CacheSetUseExclusive( nMode, nConxn )
   LOCAL n, nOldSetting := 0

   DEFAULT nConxn TO CacheSetConnection()

   IF nMode == NIL
      IF ( n := AScan( aConnections, {|e_| e_[ 1 ] == nConxn } ) ) > 0
         nOldSetting := aConnections[ n,DBCI_USEEXCLUSIVE ]
      ENDIF
   ENDIF

   IF ValType( nMode ) == "N"
      IF ( n := AScan( aConnections, {|e_| e_[ 1 ] == nConxn } ) ) > 0
         nOldSetting := aConnections[ n,DBCI_USEEXCLUSIVE ]
         aConnections[ n,DBCI_USEEXCLUSIVE ] := nMode
         CacheSetGet( nConxn, 0, 132, LTrim( Str( nMode ) ) )
      ENDIF
   ENDIF

   RETURN nOldSetting


FUNCTION CacheCreateDatabase( cDatabase, cFolder, nSize, nGrowthBlockSize, nConxn )
   DEFAULT cFolder          TO ""
   DEFAULT nSize            TO 1
   DEFAULT nGrowthBlockSize TO 8192
   DEFAULT nConxn           TO CacheSetConnection()

   cDatabase := Upper( cDatabase )

   cFolder := Trim( cFolder )
   IF ! Empty( cFolder ) .AND. ! ( Right( cFolder, 1 ) == "\" )
      cFolder += "\"
   ENDIF

   RETURN CacheSetGet( nConxn, 0, 108, cFolder + "|" + cDatabase + "|" + NTRIM( nSize ) + "|" + NTRIM( nGrowthBlockSize ) )


FUNCTION CacheCreateNamespace( cNamespace, cDatabase, nConxn )
   DEFAULT nConxn  TO CacheSetConnection()

   cNamespace := Upper( cNamespace )
   cDatabase  := Upper( cDatabase )

   RETURN CacheSetGet( nConxn, 0, 109, cNamespace + "|" + cDatabase )


FUNCTION CacheCreateTableExtensions( cTable, nConxn )
   LOCAL aStruct, aNames, cTableInfo
   LOCAL cStr  := ""
   LOCAL nArea := Select()
   LOCAL lRet  := .F.

   DEFAULT nConxn TO CacheSetConnection()

#ifdef __HARBOUR__
   USE ( cTable ) NEW SHARED VIA "CACHERDD" ALIAS "XXX00001"
#else
   USE ( cTable ) NEW SHARED VIA "CACHERDD" ALIAS "XXX00001" CONNECTION nConxn
#endif
   IF ! NetErr()
      aStruct := DbStruct()
      DbCloseArea()

      AEval( aStruct, {|e_| cStr += Upper( e_[ 1 ] ) + " " + e_[ 2 ] + " " + ;
                               LTrim( Str( e_[ 3 ] ) ) + " " + LTrim( Str( e_[ 4 ] ) ) + "~" } )

      aNames := CacheResolveNames( cTable )
      cTableInfo := ( aNames[ NME_CACHESQLNAME ] + "~" + aNames[ NME_CACHECLASSNAME ] + "~" )

      lRet := CacheCreateTableExt( nConxn, cTableInfo, cStr )
   ENDIF

   Select( nArea )
   RETURN lRet


FUNCTION CacheSumSql( cSql )
   RETURN Val( CacheExeQuery( 11, 0, cSql ) )


FUNCTION CacheExeQuery( nMode, nHandle, cSql, cParam, cTableInfo, nConxn )
   DEFAULT nMode      TO 0
   DEFAULT nHandle    TO 0
   DEFAULT cSql       TO " "
   DEFAULT cParam     TO " "
   DEFAULT cTableInfo TO " "
   DEFAULT nConxn     TO CacheSetConnection()

   RETURN CacheExecuteQuery( nConxn, nMode, nHandle, cSql, cParam, cTableInfo )


FUNCTION CacheMergeGlobals( cSource, cDestntn, nConxn )
   DEFAULT nConxn  TO CacheSetConnection()
   RETURN CacheSetGet( nConxn, 0, 41, cSource + "|~|" + cDestntn )


FUNCTION CachePerfMon( nMode, nConxn )
   LOCAL cRet := ""

   DEFAULT nMode   TO 1
   DEFAULT nConxn  TO CacheSetConnection()

   DO CASE
   CASE nMode == 1   // 46.Report
      cRet := CacheSetGet( nConxn, 0, 46, " " )
   CASE nMode == 2   // 45.Metrics
      cRet := CacheSetGet( nConxn, 0, 45, " " )
   CASE nMode == 3   // 44.Enable
      cRet := CacheSetGet( nConxn, 0, 44, " " )
   CASE nMode == 4   // 46.Clear
      cRet := CacheSetGet( nConxn, 0, 46, " " )
   CASE nMode == 5   // 47.Disable
      cRet := CacheSetGet( nConxn, 0, 47, " " )
   ENDCASE

   RETURN cRet


FUNCTION CacheGetAllLocks( nWA )
   LOCAL aLocks

   DEFAULT nWA TO Select()
   aLocks := __str2Array( CacheSetGet( USRRDD_AREADATA( nWA )[ WA_CONXN ], nWA, 119, " " ), "~" )
   AEval( aLocks, {| e, i | aLocks[ i ] := Val( e ) } )

   RETURN aLocks


FUNCTION CacheGetListPos( nWA )
   DEFAULT nWA TO Select()
   RETURN CacheSetGet(  USRRDD_AREADATA( nWA )[ WA_CONXN ], nWA, 143, " " )


FUNCTION CacheRecallDeleted( nWA, nRecNo )
   DEFAULT nWA    TO Select()
   DEFAULT nRecNo TO RecNo()
   RETURN CacheSetGet( USRRDD_AREADATA( nWA )[ WA_CONXN ], nWA, 201, NTRIM( nRecNo ) )


FUNCTION CacheGetLockInfo( nWA, nRecNo )
   LOCAL cJob
   LOCAL aInfo := {}

   DEFAULT nWA    TO Select()
   DEFAULT nRecNo TO RecNo()

   cJob := CacheSetGet( USRRDD_AREADATA( nWA )[ WA_CONXN ], nWA, 117, NTRIM( nRecNo ) )
   IF ! Empty( cJob )
      aInfo := CacheGetProcessInfo( Val( cJob ), USRRDD_AREADATA( nWA )[ WA_CONXN ] )
   ENDIF
   RETURN aInfo


FUNCTION CacheIsRecLocked( nWA, nRecNo )
   DEFAULT nWA    TO Select()
   DEFAULT nRecNo TO RecNo()
   RETURN CacheIsLocked( USRRDD_AREADATA( nWA )[ WA_CONXN ], nWA, nRecNo )


FUNCTION CacheJustZap()
   RETURN CacheZap( Cache_DefConxn(), Select() )


FUNCTION CacheGetCountersInfo( nConxn )
   LOCAL aKeys, aVals, cRet, n, cKeys, cVals
   LOCAL aInfo := {}

   DEFAULT nConxn TO CacheSetConnection()

   IF ! Empty( cRet := CacheSetGet( nConxn, 0, 39, " " ) )
      IF ( n := At( '~~', cRet ) ) > 0
         cKeys := SubStr( cRet, 1, n - 1 )
         cVals := SubStr( cRet, n + 2 )
         aKeys := hb_ATokens( cKeys, "," )
         aVals := hb_ATokens( cVals, "," )
         IF Len( aKeys ) == Len( aVals )
            AEval( aKeys, {|e,i| iif( ! Empty( e ), AAdd( aInfo, { e, Val( aVals[ i ] ) } ), NIL ) } )
         ENDIF
      ENDIF
   ENDIF
   RETURN aInfo


FUNCTION CacheGetCounterInfoByKey( cKey, nConxn )
   DEFAULT nConxn TO CacheSetConnection()
   RETURN Val( CacheSetGet( nConxn, 0, 38, cKey ) )


FUNCTION CacheSetCounter( cKeyWord, nInitValue, nConxn )
   LOCAL cRet

   IF Empty( cKeyWord )
      RETURN .F.
   ENDIF
   DEFAULT nInitValue TO 0
   DEFAULT nConxn     TO CacheSetConnection()

   cRet := CacheSetGet( nConxn, 0, 40, cKeyWord + "~" + LTrim( Str( nInitValue, 15, 0 ) ) )
   RETURN ( cRet == "OK" )


FUNCTION CacheGetCounter( cKeyWord, nConxn )
   DEFAULT cKeyWord TO "GENERIC"
   DEFAULT nConxn   TO CacheSetConnection()
   RETURN Val( CacheSetGet( nConxn, 0, 42, cKeyWord ) )


FUNCTION CacheSetStatic( cKeyWord, cValue, nConxn )
   DEFAULT nConxn TO CacheSetConnection()
   RETURN Val( CacheSetGet( nConxn, 0, 43, cKeyWord + "~" + cValue ) )


/* Can be extended to implement more features via table name
 */
FUNCTION CacheResolveNames( cFullName )
   LOCAL n, cTable
   LOCAL cSchema := ""
   LOCAL aNames  := Array( NME_SIZE )

   IF ( n := At( "||", cFullName ) ) > 0
      cSchema   := SubStr( cFullName, 1, n - 1 )
      cFullName := SubStr( cFullName, n + 2    )
   ENDIF

   IF Empty( cSchema )
      cSchema := CacheSetSchema()
   ENDIF
   IF ! CacheSetSchemaAsIs()
      cSchema := Upper( cSchema )
   ENDIF
   HB_FNameSplit( cFullName, , @cTable )
   IF ! CacheSetTableNameAsIs()
      cTable := Upper( cTable )
   ENDIF

   // Compatible WITH RDD behavior
   aNames[ NME_FULLNAME       ] := Upper( cFullName )
   aNames[ NME_SCHEMA         ] := cSchema
   aNames[ NME_TABLE          ] := cTable
   aNames[ NME_CACHETABLE     ] := StrTran( cTable, "_", "" )
   aNames[ NME_ALIAS          ] := Upper( cTable )
   aNames[ NME_CACHESQLNAME   ] := cSchema + "." + cTable
   aNames[ NME_CACHECLASSNAME ] := cSchema + "." + StrTran( cTable, "_", "" )

   RETURN aNames


FUNCTION CacheGetLockList( nConxn )
   DEFAULT nConxn TO CacheSetConnection()
   RETURN CacheSetGet( nConxn, 0, 102, "NumberOfAllLocks" )


FUNCTION Cache_SomeInfo( nRequest, cCargo )
   LOCAL cRet

   DEFAULT nRequest TO 0
   DEFAULT cCargo   TO " "

   cRet := CacheSetGet( Cache_DefConxn(), Select(), nRequest, cCargo )
   RETURN  iif( cRet == NIL, "", cRet )


FUNCTION Cache_JustATrip()
   RETURN CacheJustATrip( Cache_DefConxn(), Select() )


FUNCTION CacheOpenClass( nConxn, cClass )
   DEFAULT nConxn TO CacheSetConnection()
   RETURN CacheClassOpen( nConxn, cClass )


FUNCTION CacheCloseClass( nConxn )
   DEFAULT nConxn TO CacheSetConnection()
   RETURN CacheClassFree( nConxn )


FUNCTION CacheAddRecords( nWA, cRecBuffer )
   RETURN CacheAppendRecords(  USRRDD_AREADATA( nWA )[ WA_CONXN ], nWA, cRecBuffer )


STATIC FUNCTION __str2Array( cStr, cDel )
   LOCAL n, nlen
   LOCAL a_:={}

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

