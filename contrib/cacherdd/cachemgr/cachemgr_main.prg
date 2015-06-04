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


#include 'cacherdd.ch'

#include 'common.ch'
#include 'inkey.ch'
#include 'hbgtinfo.ch'
#include 'hbclass.ch'


ENABLE TYPE CLASS ALL

REQUEST DBFCDX
REQUEST CACHERDD

THREAD STATIC lByPass := .F.


INIT PROCEDURE Wvt_Start()

   ReadINI()

   hb_gtInfo( HB_GTI_FONTNAME, GetINIValue( "Font", "Courier" ) )
   SetMode( 25,80 )
   SetCursor( 0 )

   InitMutex()
   RETURN


FUNCTION Main( cServerIP, cPort, cNameSpace, cUser, cPassword, cSecs )
   LOCAL nConxn, i, bError

   DEFAULT cServerIP  TO '127.0.0.1'
   DEFAULT cPort      TO '1972'
   DEFAULT cNameSpace TO 'USER'
   DEFAULT cUser      TO '_system'
   DEFAULT cPassword  TO 'SYS'
   DEFAULT cSecs      TO '30'

   SET( _SET_EVENTMASK, INKEY_ALL )
   SET SCOREBOARD OFF
   SET EPOCH TO 1950

   mSetCursor( .T. )

   SetColor( 'N/W' )
   CLS
   Wvt_SetIcon( 1 )

   RestAppnInfo()

   CacheSetServerParams( cServerIP, val( cPort ), cUser, cPassword, val( cSecs ) )

   FOR i = 1 TO 3
      IF ( nConxn := CacheAddConnection( cNameSpace ) ) > 0
         EXIT
      ELSEIF Inkey() == 27
         RETURN NIL
      ELSEIF i == 3
         Alert( "Connection not established!" )
         RETURN  NIL
      ENDIF
   NEXT

   bError := ErrorBlock( {|oError| MyErrorBlock( oError ) } )

   MySetConxn( 'TEST', nConxn )
   MySetConxn( 'TEMP', nConxn )

   CacheSetConnection( MySetConxn( 'TEST' ) )
   CacheSetUserInfo( "IT IS CacheMGR" )
   CacheLockTimeout( 0 )
   CacheSetUseExclusive( 1 )

   hb_gtInfo( HB_GTI_WINTITLE, ProcessInfo() )

   MGR_DoMenu( MenuManageCache() )

   SaveAppnInfo()
   SaveINI()
   ErrorBlock( bError )
   RETURN 0


FUNCTION HB_GTSYS()
   REQUEST HB_GT_WVG_DEFAULT
   RETURN NIL


STATIC FUNCTION MenuManageCache()
   LOCAL aOptions := {}

   AAdd( aOptions, { "Browse a Table"        , {|| MyBrowseFile( NIL, NIL, 0 )             } } )
   AAdd( aOptions, { "Browse Deleted Records", {|| MyBrowseFile( NIL, .T. )                } } )
   AAdd( aOptions, { "Fetch Table Info"      , {|| MGR_TableInfo()                         } } )
   AAdd( aOptions, { " "                     , {|| NIL                                     } } )
   AAdd( aOptions, { "SQL Query"             , {|| Sql_LoadCacheQuery()                    } } )  /* Not usable : Cache Trial Version excludes SQL access */
// AAdd( aOptions, { " "                     , {|| NIL                                     } } )
   AAdd( aOptions, { "Show Running Processes", {|| MGR_ShowProcesses( CacheGetProcesses() )} } )
   AAdd( aOptions, { "Fetch Counters Info"   , {|| MGR_ShowCounters()                      } } )
// AAdd( aOptions, { "Dash Board"            , {|| hb_threadStart( {|| ShowDashBoard() } ) } } )  /* Not ready yet */
   AAdd( aOptions, { " "                     , {|| NIL                                     } } )
   AAdd( aOptions, { "MyTable - A Test Bed"  , {|| MGR_TestTable()                         } } )
   AAdd( aOptions, { " "                     , {|| NIL                                     } } )
   AAdd( aOptions, { "Extended Functions"    , {|v| v := __extendedFunc( .T. ), iif( ! Empty( v ), MGR_DoMenu( v ), NIL ) } } )

   RETURN aOptions


STATIC FUNCTION __extendedFunc()
   LOCAL aOptions := {}

   AAdd( aOptions, { "Browse a Table"        , {|| MyBrowseFile( NIL, NIL, 0 ) } } )
   AAdd( aOptions, { " "                     , {|| NIL                         } } )
   AAdd( aOptions, { "Table Sizes"           , {|| MGR_GetTableSizes()         } } )
   AAdd( aOptions, { "Tune Tables"           , {|| MGR_TuneTable()             } } )
   AAdd( aOptions, { "Create Counter"        , {|| MGR_CreateCounter()         } } )
   AAdd( aOptions, { " "                     , {|| NIL                         } } )
   AAdd( aOptions, { "Create Index by Exp"   , {|| MGR_CreateIndexByExp()      } } )
   AAdd( aOptions, { "Drop Indexes"          , {|| MGR_DropIndex()             } } )
   AAdd( aOptions, { " "                     , {|| NIL                         } } )
   AAdd( aOptions, { "Upload a DbfCDX"       , {|| MGR_Upload( .F. )           } } )
   AAdd( aOptions, { "Synchronize Uploads"   , {|| MGR_Sync()                  } } )
   AAdd( aOptions, { "Download as DbfCDX"    , {|| MGR_Download()              } } )

   RETURN aOptions


FUNCTION MGR_DoMenu( aOptions )
   LOCAL aOpt := {}
   LOCAL nChoice

   AEval( aOptions, {|e_| AAdd( aOpt, e_[ 1 ] ) } )

   SET( _SET_EVENTMASK, HB_INKEY_ALL )
   DO WHILE .T.
      Wvt_SetTitle( ProcessInfo() )
      nChoice := VouPopup( aOpt, 'Select an Option' )
      IF nChoice == 0
         EXIT
      ENDIF
      Eval( aOptions[ nChoice,2 ] )
   ENDDO
   RETURN NIL


FUNCTION ProcessInfo()
   LOCAL s := 'H'

   RETURN Str( CacheGetCurrentProcessID(), 5, 0 ) + ;
          ' [' + CacheGetConnectionInfo( DBCI_DATABASE ) + ']' +;
          ' [' + CacheGetConnectionInfo( DBCI_IP       ) + ']' +;
          ' [' + CacheGetConnectionInfo( DBCI_PORT     ) + ']' +;
          ' [' + s + ']' +;
          ' : CacheMGR'

FUNCTION MGR_ShowProcesses( aData, cTitle )
   LOCAL aAttr := {}

   IF Empty( aData ) .or. Empty( aData[ 1,1 ] )
      RETURN NIL
   ENDIF

   DEFAULT cTitle TO 'Running Processes'

   AAdd( aAttr, { PROCESS_NUMBER        , "Procs"       , "C",  5, 0, "@ " } )
   AAdd( aAttr, { PROCESS_NAMESPACE     , "NameSpace"   , "C", 10, 0, "@ " } )
   AAdd( aAttr, { PROCESS_IP            , "Server IP"   , "C", 15, 0, "@ " } )
   AAdd( aAttr, { PROCESS_INTRANSACTION , "Trn"         , "C",  3, 0, "@ " } )
   AAdd( aAttr, { PROCESS_STATE         , "State"       , "C",  6, 0, "@ " } )
   AAdd( aAttr, { PROCESS_LINES         , "Lines"       , "N",  7, 0, "@Z 9999999" } )
   AAdd( aAttr, { PROCESS_EXE           , "Application ", "C", 14, 0, "@ " } )
   AAdd( aAttr, { PROCESS_NODE          , "Client"      , "C", 12, 0, "@ " } )
   AAdd( aAttr, { PROCESS_OSUSERNAME    , "OsUser"      , "C", 10, 0, "@ " } )
   AAdd( aAttr, { PROCESS_CACHEUSERNAME , "CacheUser "  , "C", 10, 0, "@ " } )
   AAdd( aAttr, { PROCESS_USERINFO      , "UserInfo"    , "C", 16, 0, "@ " } )

   AEval( aData, {|e_| e_[ PROCESS_INTRANSACTION ] := iif( e_[ PROCESS_INTRANSACTION ] == "1", "YES", "NO" ) } )
   AEval( aData, {|e_| e_[ PROCESS_LINES         ] := Val( e_[ PROCESS_LINES ] ) } )

   MGR_ArrayBrowse( aData, aAttr, cTitle )

   RETURN NIL


FUNCTION BrwArrayRefreshData( oBrw, aData )
   oBrw:Cargo[ 1 ] := aData
   oBrw:RefreshAll()
   RETURN NIL


FUNCTION SaveAppnInfo()
#if 0
   LOCAL aPos := Wvg_GetWindowRect( Wvt_GetWindowHandle() )
   LOCAL cText := ""

   IF ! ( "ar32cache" $ Lower( hb_DirBase() ) )
      cText += hb_ntos( aPos[ 1 ]   ) + ";"
      cText += hb_ntos( aPos[ 2 ]   ) + ";"
      SetINIValue( "MainWindow", cText )
   ENDIF
#endif
   RETURN NIL


FUNCTION RestAppnInfo()
#if 0
   LOCAL aInfo := hb_ATokens( GetINIValue( "MainWindow" ), ";" )
   LOCAL nXX, nYY

   IF Len( aInfo ) >= 2
      nXX   := val( aInfo[ 1 ] )
      nYY   := val( aInfo[ 2 ] )
      hb_gtInfo( HB_GTI_SETPOS_XY, nXX, nYY )
   ENDIF
#endif
   RETURN NIL


FUNCTION TestLocks()
   LOCAL i

   CacheInsertLockMode( 2 )
   CacheSetUseExclusive( 1 )

   USE "MYTABLE" SHARED NEW VIA "CACHERDD"

   FOR i := 1 TO 4
      APPEND BLANK
      QOut( Len( dbrLockList() ) )
      DO WHILE inkey() != 27
      ENDDO
      REPLACE MYTABLE->code WITH str(i)
   NEXT

   dbCloseArea()
   RETURN NIL


FUNCTION MGR_GetTableSizes()
   LOCAL  a_, b_, d_, e_, t_:={}
   LOCAL cDir, cMask, cMaskD, cMaskI, nA, nU, n, cTable

   IF Empty( cDir := VouGetSome( "Database Directory on the Server?", Space( 100 ) ) )
      RETURN NIL
   ENDIF
   IF Empty( cMask := VouGetSome( "Filter to pullout sizes of tables?", space( 12 ), "@! " ) )
      RETURN NIL
   ENDIF
   cMask := Trim( cMask )
   IF Right( cMask, 1 ) != "*"
      cMask += "*"
   ENDIF

   cMaskD := cMask + "D"
   cMaskI := cMask + "I"
   cMaskD := Upper( cMaskD )
   cMaskI := Upper( cMaskI )

   IF ! Empty( a_:= CacheGetTableSizes( cDir + "~SQLUSER." + cMaskD + "~" ) )
      AEval( a_, {|e_| e_[ 1 ] := Left( e_[ 1 ], Len( e_[ 1 ] ) - 1 ) } )

      b_:= CacheGetTableSizes( cDir + "~SQLUSER." + cMaskI + "~" )
      AEval( b_, {|e_| e_[ 1 ] := Left( e_[ 1 ], Len( e_[ 1 ] ) - 1 ) } )

      d_:={}
      FOR EACH e_ IN a_
         cTable := e_[ 1 ]
         IF ( n := AScan( b_, {|e_| e_[ 1 ] == cTable } ) ) > 0
            nA := b_[ n,2 ]
            nU := b_[ n,3 ]
         ELSE
            nA := nU := 0
         ENDIF
         AAdd( d_, { cTable, e_[ 2 ], e_[ 3 ], nA, nU } )
      NEXT

      AAdd( t_, { 1, "Table"    , "C", 10, 0, "@! "              } )
      AAdd( t_, { 2, "Allocated", "N", 13, 0, "@Z 9,999,999,999" } )
      AAdd( t_, { 3, "Used"     , "N", 13, 0, "@Z 9,999,999,999" } )
      AAdd( t_, { 4, "Idx-Alloc", "N", 13, 0, "@Z 9,999,999,999" } )
      AAdd( t_, { 5, "Idx-Used" , "N", 13, 0, "@Z 9,999,999,999" } )

      MGR_ArrayBrowse( d_, t_, "Table Sizes in KB" )
   ENDIF

   RETURN NIL


FUNCTION MGR_ShowCounters()
   LOCAL aAttr := {}
   LOCAL aData := CacheGetCountersInfo()

   IF ! Empty( aData )
      AAdd( aAttr, { 1, "Key"    , "C", 20, 0, "@ " } )
      AAdd( aAttr, { 2, "Counter", "N", 12, 0, "@Z 999999999999" } )
      MGR_ArrayBrowse( aData, aAttr, "Current Counters Information [F12 Refresh]", ;
                 { { K_F12, {|oBrw| BrwArrayRefreshData( oBrw, CacheGetCountersInfo() ) } } } )
   ENDIF

   RETURN NIL


FUNCTION MGR_CreateCounter()
   LOCAL cTag, nVal
   LOCAL nMaxRow := MaxRow()
   LOCAL nMaxCol := MaxCol()
   LOCAL aScr    := VouWinSave( nMaxRow - 2, 0, nMaxRow, nMaxCol )
   LOCAL GetList := {}

   cTag := space( 20 )
   nVal := 0

   UiDispOut( nMaxRow - 2, 0, padc( 'Create a New Counter', nMaxCol + 1 ), 'W+/B' )
   UiDispOut( nMaxRow - 1, 0, pad( 'Counter Tag', nMaxCol + 1 ), 'N/W' )
   UiDispOut( nMaxRow - 1, nMaxCol - 12 + 1, PadL( 'Value', 12 ), 'N/W' )
   UiDispOut( nMaxRow    , 0, space( nMaxCol + 1 ), 'N/W' )

   @nMaxRow, 0 GET cTag PICTURE '@ ' COLOR 'W+/G,W+/G'
   @nMaxRow,nMaxCol-12+1 GET nVal PICTURE '@Z 999999999999' COLOR 'W+/G,W+/G'
   READ

   cTag := Trim( cTag )
   IF ! Empty( cTag )
      CacheSetCounter( cTag, nVal )
   endif
   VouWinRest( aScr )
   RETURN NIL


FUNCTION MGR_TuneTable()
   LOCAL cDbf, xRet

   cDbf := Upper( Trim( VouGetSome( 'Table to Tune for SQL Queries ?', Space( 40 ) ) ) )

   IF ! Empty( cDbf )
      xRet := CacheTuneTable( cDbf )
   ENDIF
   ? xRet
   RETURN xRet


FUNCTION MGR_DropIndex()
   LOCAL cDbf, xRet

   cDbf := Upper( Trim( VouGetSome( 'Table to Drop Indexes?', Space( 40 ) ) ) )
   IF ! Empty( cDbf )
      xRet := CacheDropIndex( cDbf )
   ENDIF
   RETURN xRet


FUNCTION MGR_CreateIndexByExp()
   LOCAL cTag, cExp, hKey, nArea
   LOCAL nMaxRow := MaxRow()
   LOCAL nMaxCol := MaxCol()
   LOCAL aScr    := VouWinSave( nMaxRow - 2, 0, nMaxRow, nMaxCol )
   LOCAL cTable  := ' '
   LOCAL GetList := {}

   hKey   := SetKey( K_F2, {|| __getATable() } )
   cTable := Upper( Trim( VouGetSome( 'Table to Browse? <F2 List>', Pad( cTable, 40 ) ) ) )
   SetKey( K_F2, hKey )
   IF Empty( cTable )
      RETURN NIL
   ENDIF

   cTag := Space( 16  )
   cExp := Space( 100 )

   UiDispOut( nMaxRow-2, 0, PadC( 'Expression for New Index : ' + cTable, nMaxCol + 1 ), 'W+/B' )
   UiDispOut( nMaxRow-1, 0, Pad( 'Tag                 Expression', nMaxCol + 1 ), 'N/W' )
   UiDispOut( nMaxRow  , 0, Space( nMaxCol + 1 ), 'N/W' )

   @nMaxRow, 0 GET cTag PICTURE '@! '  COLOR 'W+/G,W+/G'
   @nMaxRow,20 GET cExp PICTURE '@S60' COLOR 'W+/G,W+/G'
   READ

   cTag := Trim( cTag )
   cExp := Trim( cExp )

   IF Empty( cTag ) .OR. Empty( cExp )
      VouWinRest( aScr )
      RETURN NIL
   ENDIF

   IF Alert( 'About to Create Index!', { 'Continue', 'Exit' } ) == 1
      nArea := select()
      USE ( cTable ) SHARED VIA 'CACHERDD'
      IF NetErr()
         Alert( cTable +' : could not been opened!' )
         RETURN NIL
      ENDIF

      INDEX ON &cExp. TAG ( cTag ) TO ( cTable )

      DbCloseArea()
      Select( nArea )
   ENDIF
   VouWinRest( aScr )
   RETURN NIL


FUNCTION __getATable()
   LOCAL nChoice
   LOCAL g := GetActive()
   LOCAL aTables := CacheGetTables()

   IF ! Empty( aTables )
      nChoice := VouPopup( aTables, 'Table', { 3, 33, 22, 47 } )
      if nChoice > 0
         g:varPut( Pad( aTables[ nChoice ], Len( g:varGet() ) ) )
         g:display()
         KEYBOARD K_ENTER
      ENDIF
   ENDIF
   RETURN NIL


FUNCTION MGR_Upload( lTemp )
   Local cTable
   Local cDbf       := Pad( 'C:\Pritpal\JustNotToMistake.dbf', 100 )
   Local nEvery     := 10000
   Local bExeBefore := {| cDbf, nRecords | DispProgBefore( cDbf, nRecords ) }
   Local bExeDuring := {| nCurRec| DispProgDuring( nCurRec ) }

   DEFAULT lTemp TO .F.

   cDbf := Upper( AllTrim( VouGetSome( 'Table to upload', cDbf ) ) )
   IF hb_fileExists( cDbf )
      hb_fNameSplit( cDbf, NIL, @cTable )
      CacheUploadByBuffer( cDBF, cTable, iif( lTemp, "RDDTEMP", 'SQLUSER' ), .T., bExeBefore, bExeDuring, nEvery, /*bExeAfter*/ )
   ENDIF
   RETURN NIL


FUNCTION MGR_Download( ccTable, cPath )
   LOCAL bKey, cSchema, scr, cColor, n
   LOCAL cTable     := "    "
   LOCAL bExeBefore := {| cDbf, nRecords | DispProgBefore( cDbf, nRecords ) }
   LOCAL bExeDuring := {| nCurRec| DispProgDuring( nCurRec ) }

   DEFAULT ccTable TO ""
   DEFAULT cPath   TO "C:\temp\"

   IF Empty( ccTable )
      bKey   := SetKey( K_F2, {|| __getATable() } )
      cTable := Upper( Trim( VouGetSome( "Table to Download <F2> ?", pad( cTable, 40 ) ) ) )
      SetKey( K_F2, bKey )
      IF LastKey() == K_ESC
         RETURN NIL
      ENDIF
   ELSE
      cTable := Trim( ccTable )
   ENDIF

   IF Empty( cTable )
      RETURN NIL
   ENDIF

   IF ( n := At( "||", cTable ) ) > 0
      cSchema := SubStr( cTable, 1, n - 1 )
      cTable  := SubStr( cTable, n + 2 )
   ENDIF

   scr := SaveScreen( 0, 0, MaxRow(), MaxCol() )
   cColor := SetColor( "N/W" )
   CLS

   CacheDownloadByBuffer( cTable, cSchema, ( cPath + cTable + ".dbf" ), .T., bExeBefore, bExeDuring, 10000, NIL )

   RestScreen( 0, 0, MaxRow(), MaxCol(), scr )
   SetColor( cColor )
   RETURN NIL


STATIC FUNCTION DispProgBefore( cDbf, nRecords )
   @ MaxRow(), 0 SAY ( Pad( cDbf, 35 ) + " " + DToC( Date() ) + " " + Time() + " " ) COLOR "W+/BG"
   @ MaxRow(),71 SAY str( nRecords,9,0 ) COLOR "W+/G"
   RETURN .T.


STATIC FUNCTION DispProgDuring( nCurRec )
   @ MaxRow(), 53 SAY ( Time() + " " + Str( nCurRec, 9, 0 ) ) COLOR "W+/B"
   RETURN .T.


FUNCTION MGR_Sync()
   IF CacheMgrUseTblMgr()
      DbCloseArea()
      MyBrowseFile( "RDDMGR||TABLEMGR" )
   ENDIF
   RETURN NIL


#define MYTESTDBF                                 "C:\Temp\MyTable.dbf"
#define MYTESTIDX                                 "C:\Temp\MyTable.cdx"
#define MYTESTALIAS                               "MYTABLE"


FUNCTION MGR_TestTable( lCDX )
   LOCAL lDelete
   LOCAL scr := VouSaveScrn( 0, 0, MaxRow(), MaxCol() )

   SetColor( "N/W,W/N,,,W+/N" )
   CLS

   lDelete := Alert( "Delete Table?", { "No", "Yes" } ) == 2

   __testMyTable( lCDX, lDelete )

   VouRestScrn( scr )
   RETURN NIL


STATIC FUNCTION __testMyTable( lCdx, lDelete )
   Local nChoice, lBlank
   Local aStr     := {}
   Local nConxn   := MySetConxn( "TEST" )
   Local nArea    := Select()
   Local cDbfFile := MYTESTDBF
   Local cIdxFile := MYTESTIDX

   SET DELETED ON
   DEFAULT lCdx TO .F.

   SetPos( 0,5 )

   AAdd( aStr, { "Code"    , "C",  8, 0 } )
   AAdd( aStr, { "Name"    , "C", 25, 0 } )
   AAdd( aStr, { "Salary"  , "N", 10, 2 } )
   AAdd( aStr, { "Dob"     , "D",  8, 0 } )
   AAdd( aStr, { "Mrd"     , "L",  1, 0 } )
   AAdd( aStr, { "Text"    , "M", 10, 0 } )
   AAdd( aStr, { "ProcName", "C", 32, 0 } )
   AAdd( aStr, { "ProcLine", "C",  7, 0 } )
   AAdd( aStr, { "One"     , "C",  1, 0 } )

   IF .T.
      IF lDelete
         IF ( "RDDTEMP" $ cDbfFile )
            dbCreate( cDbfFile, aStr, "CACHERDD", , , , , nConxn )
         ELSE
            ? CacheDropTable( cDbfFile, nConxn )
         ENDIF
      ENDIF
      IF ! CacheExistTable( cDbfFile, nConxn )
         dbCreate( cDbfFile, aStr, "CACHERDD", , , , , nConxn )
      ENDIF
   ENDIF

   IF .T.
      CLS
      nChoice := 1

      IF nChoice = 0
         RETURN NIL
      ELSEIF nChoice == 1
         USE ( cDbfFile ) ALIAS ( MYTESTALIAS ) NEW SHARED    VIA "CACHERDD"
      ELSE
         USE ( cDbfFile ) ALIAS ( MYTESTALIAS ) NEW EXCLUSIVE VIA "CACHERDD"
      ENDIF

      IF NetErr()
         Alert( "Error Opening " + cDbfFile + " : " + iif( nChoice == 1, "SHARED", "EXCLUSIVELY" ) )
         RETURN NIL
      ENDIF
   ENDIF

   SetPos( MaxRow() - 1, 0 )

   __setIndex( cIdxFile, nConxn, .F., lCdx )
   IF LastRec() == 0
      lBlank := .F.
      __addFewRecords( lCdx, NIL, lBlank )
   ENDIF
   dbCloseArea()

   MyBrowseFile( "MYTABLE" )

   ?
   ? "Press ESC"
   DO WHILE Inkey() != 27
   ENDDO
   Select( nArea )
   RETURN NIL


STATIC FUNCTION __setIndex( cIdxFile, nConxn, lForce, lCdx )
   LOCAL a_,i, nStart
   LOCAL lCreate

   DEFAULT lForce TO .F.
   DEFAULT lCdx   TO .F.

   lCreate := ! CacheExistIndex( cIdxFile, nConxn ) .OR. lForce
   IF lCreate
      a_:={}
      //           TAG         EXPRESSION             FOR CONDITION
      AAdd( a_, { "CODE"    , "Code"                                     } )
      AAdd( a_, { "NAME"    , "Name"                                     } )
      AAdd( a_, { "SALARY"  , "Salary"                                   } )
      AAdd( a_, { "DOB"     , "DOB"                , 'Left(name,1)="P"'  } )
      AAdd( a_, { "SOURCE"  , "ProcName + ProcLine"                      } )
      AAdd( a_, { "ONE"     , "One"                                      } )

      IF FLock()
         SetPos( MaxRow() - 1, 0 )

         FOR i := 1 to Len( a_ )
            nStart := Seconds()
            ? Pad( cIdxFile, 8 ), Time(), Pad( a_[ i,2 ],50 ), " "
            IF Len( a_[ i ] ) > 2
               INDEX ON &( a_[ i,2 ] ) TAG ( a_[ i,1 ] ) TO ( cIdxFile ) FOR &( a_[ i,3 ] ) ADDITIVE
            ELSE
               INDEX ON &( a_[ i,2 ] ) TAG ( a_[ i,1 ] ) TO ( cIdxFile ) ADDITIVE
            ENDIF
            ? Seconds() - nStart
         NEXT
         DbUnlock()
      ENDIF
   ELSE
      Alert( 'Index Exists!' )
   ENDIF

   SET ORDER TO 1
   HB_SYMBOL_UNUSED( lCdx )
   RETURN NIL


STATIC FUNCTION __addFewRecords( lCdx, nHowMany, lBlank )
   LOCAL i, n, nStart
   LOCAL nam_  := { "John","Pritpal","Augusto","Tony","Loie","Steve","Eric","Baruch","Roberto" }
   LOCAL cText := "This is Just Memo Text"

   HB_SYMBOL_UNUSED( lCdx )

   DEFAULT nHowMany TO VouGetSome( "Records to Add ?", 10000, "@Z 9999999999" )
   DEFAULT lBlank   TO .F.
   ?
   ? "Appends : "+ltrim( str( nHowMany ) )+" Records : ",time(), "  "
   nStart := seconds()

   IF lBlank
      FOR i := 1 to nHowMany
         DbAppend()

         IF ! NetErr()
            REPLACE MyTable1->Code WITH  LTrim( Str( i, 8 ) )
         ELSE
            Alert( "Network Error" )
         ENDIF
      NEXT
   ELSE
      FOR i := 1 to nHowMany
         DbAppend()

         IF ! NetErr()
            n := i%9
            n := if( n == 0,9,n )

            REPLACE ( MYTESTALIAS )->Code    WITH  LTrim( Str( i, 8 ) )
            REPLACE ( MYTESTALIAS )->Name    WITH  nam_[ n ]
            IF i%9 <> 0
               REPLACE ( MYTESTALIAS )->Salary  WITH  i/105
            ENDIF
            IF i%3 <> 0
               REPLACE ( MYTESTALIAS )->Dob     WITH  date() - ( i%50 )
            ENDIF
            IF i%5 <> 0
               REPLACE ( MYTESTALIAS )->Mrd     WITH  ( i%3 == 0 )
            ENDIF
            IF i%7 <> 0
               REPLACE ( MYTESTALIAS )->Text    WITH cText
            ENDIF
            REPLACE ( MYTESTALIAS )->ProcName WITH  LTrim( Str( i, 8 ) ) + "PROCEDURE"
            REPLACE ( MYTESTALIAS )->ProcLine WITH  PadC( LTrim( Str( i, 8 ) ), 7 )
         ELSE
            Alert( 'Network Error' )
         ENDIF
      NEXT
   ENDIF

   DbCommit()
   DbUnlock()
   ? time(), seconds()-nStart
   ?
   RETURN NIL


FUNCTION MGR_TableInfo( cTable )
   LOCAL bKey, aInfo
   LOCAL cAlert := ""

   IF Empty( cTable )
      bKey := SetKey( K_F2, {|| __getATable() } )
      cTable := Upper( Trim( VouGetSome( 'Table name to pull info <F2 Select>', Space( 30 ) ) ) )
      SetKey( K_F2, bKey )
   ENDIF

   IF ! Empty( cTable )
      IF ! Empty( aInfo := CacheGetTableInfoByName( cTable ) )
         cAlert += ( "Date Created: " + DToC( aInfo[ 1 ] ) + ";" )
         cAlert += ( "Time Created: " + iif( HB_ISCHAR( aInfo[ 2 ] ), aInfo[ 2 ], "" ) + ";" )
         cAlert += ( "User Name   : " + iif( HB_ISCHAR( aInfo[ 3 ] ), aInfo[ 3 ], "" ) + ";" )
         cAlert += ( "IP          : " + iif( HB_ISCHAR( aInfo[ 4 ] ), aInfo[ 4 ], "" ) + ";" )
         cAlert += ( "Node        : " + iif( HB_ISCHAR( aInfo[ 5 ] ), aInfo[ 5 ], "" ) + ";" )
         cAlert += ( "Executable  : " + iif( HB_ISCHAR( aInfo[ 6 ] ), aInfo[ 6 ], "" ) )
         Alert( cAlert )
      ELSE
         Alert( "Table is missing extended info!" )
      ENDIF
   ENDIF
   RETURN NIL


FUNCTION MyErrorBlock( oError )
   LOCAL cMsg := ProcName( 2 ) + " " + Str( ProcLine( 2 ) ) + ";" + ;
                 ProcName( 3 ) + " " + Str( ProcLine( 3 ) ) + ";" + ;
                 oError:description + ";" + ;
                 hb_ValToExp( oError:args )
   Alert( cMsg )
   RETURN NIL


