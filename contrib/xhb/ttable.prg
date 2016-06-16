/*
 * Table, Record and Field Class
 *
 * Copyright 2000-2003 Manos Aspradakis maspr@otenet.gr
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net> (Porting this library to Harbour)
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#include "hbclass.ch"

#include "ttable.ch"
#include "ord.ch"
#include "inkey.ch"
#include "dbinfo.ch"
#include "dbstruct.ch"
#include "error.ch"

THREAD STATIC t_aTables := {}

/* Network functions */
STATIC s_nNetDelay    := 30
STATIC s_lNetOk       := .F.
STATIC s_cNetMsgColor := "GR+/R"

FUNCTION NetDbUse( cDataBase, cAlias, nSeconds, cDriver, lNew, lShared, lReadOnly )

   LOCAL nKey
   LOCAL lForever
   LOCAL cOldScreen := SaveScreen( MaxRow(), 0, MaxRow(), MaxCol() + 1 )
   LOCAL lFirstPass := .T.

   hb_default( @nSeconds, s_nNetDelay )

   s_lNetOk := .F.
   lForever := ( nSeconds == 0 )

   hb_keyIns( 255 )
   Inkey()

   DO WHILE ( lForever .OR. nSeconds > 0 ) .AND. hb_keyStd( LastKey() ) != K_ESC
      IF ! lFirstPass
         hb_DispOutAt( MaxRow(), 0, ;
            PadC( "Network retry | " + ;
            LTrim( Str( nSeconds, 4, 1 ) ) + " | ESCape : Exit ", ;
            MaxCol() + 1 ), ;
            s_cNetMsgColor )
         lFirstPass := .F.
      ENDIF

      dbUseArea( ;
         hb_defaultValue( lNew, .T. ), ;
         hb_defaultValue( cDriver, "DBFCDX" ), ;
         cDatabase, cAlias, ;
         hb_defaultValue( lShared, .T. ), ;
         hb_defaultValue( lReadOnly, .F. ) )

      IF NetErr()
         lFirstPass := .F.
      ELSE
         RestScreen( MaxRow(), 0, MaxRow(), MaxCol() + 1, cOldScreen )
         s_lNetOk := .T.
      ENDIF

      IF s_lNetOk
         RestScreen( MaxRow(), 0, MaxRow(), MaxCol() + 1, cOldScreen )
         EXIT
      ELSE
         nKey := hb_keyStd( Inkey( 0.5 ) )
         nSeconds -= 0.5
      ENDIF

      IF nKey == K_ESC
         RestScreen( MaxRow(), 0, MaxRow(), MaxCol() + 1, cOldScreen )
         EXIT
      ENDIF

   ENDDO

   RestScreen( MaxRow(), 0, MaxRow(), MaxCol() + 1, cOldScreen )

   RETURN s_lNetOk

FUNCTION NetLock( nType, lReleaseLocks, nSeconds )

   LOCAL cSave     := SaveScreen( MaxRow(), 0, MaxRow(), MaxCol() + 1 )
   LOCAL lContinue := .T.
   LOCAL lSuccess  := .F.
   LOCAL nWaitTime
   LOCAL bOperation
   LOCAL xIdentifier
   LOCAL cWord

   IF ! HB_ISNUMERIC( nType ) .OR. ;
      ( nType != 1 .AND. ;
        nType != 2 .AND. ;
        nType != 3 )
      Alert( "Invalid Argument passed to NetLock()" )
      RETURN lSuccess
   ENDIF

   hb_default( @lReleaseLocks, .F. )
   hb_default( @nSeconds, s_nNetDelay )

   nWaitTime := nSeconds

   SWITCH nType
   CASE NET_RECLOCK
      xIdentifier := iif( lReleaseLocks, NIL, RecNo() )
      bOperation  := {| x | dbRLock( x ) }
      EXIT
   CASE NET_FILELOCK
      bOperation := {|| FLock() }
      EXIT
   CASE NET_APPEND
      xIdentifier := lReleaseLocks
      bOperation  := {| x | dbAppend( x ), ! NetErr() }
      EXIT
   ENDSWITCH

   s_lNetOk := .F.

   DO WHILE lContinue

#if 0
      IF hb_keyStd( Inkey() ) == K_ESC
         RestScreen( MaxRow(), 0, MaxRow(), MaxCol() + 1, cSave )
         EXIT
      ENDIF
#endif

      DO WHILE nSeconds > 0 .AND. lContinue

         IF Eval( bOperation, xIdentifier )
            nSeconds  := 0
            lSuccess  := .T.
            lContinue := .F.
            s_lNetOk  := .T.
            EXIT
         ELSE
            SWITCH nType
            CASE 1    ; cWord := "( " + dbInfo( DBI_ALIAS ) + " - Record Lock )" ; EXIT
            CASE 2    ; cWord := "( " + dbInfo( DBI_ALIAS ) + " - File Lock )" ; EXIT
            CASE 3    ; cWord := "( " + dbInfo( DBI_ALIAS ) + " - File Append )" ; EXIT
            OTHERWISE ; cWord := "( " + dbInfo( DBI_ALIAS ) + " - ???" ; EXIT
            ENDSWITCH

            hb_DispOutAt( MaxRow(), 0, ;
               PadC( "Network Retry " + cWord + " | " + Str( nSeconds, 3 ) + " | ESC Exit", MaxCol() + 1 ), ;
               s_cNetMsgColor )

            nSeconds--
            IF hb_keyStd( Inkey( 1 ) ) == K_ESC
               RestScreen( MaxRow(), 0, MaxRow(), MaxCol() + 1, cSave )
               EXIT
            ENDIF
         ENDIF
      ENDDO

      IF LastKey() == K_ESC
         RestScreen( MaxRow(), 0, MaxRow(), MaxCol() + 1, cSave )
         EXIT
      ENDIF

      IF ! lSuccess
         nSeconds := nWaitTime
         IF Alert( "NETWORK ERROR;Continue Attempt to Lock Record/File ?", { "  Yes  ", "  No  " } ) != 1
            EXIT
         ENDIF
      ENDIF
   ENDDO

   RestScreen( MaxRow(), 0, MaxRow(), MaxCol() + 1, cSave )

   RETURN lSuccess

FUNCTION NetFunc( bBlock, nSeconds )

   LOCAL lForever      // Retry forever?

   hb_default( @nSeconds, s_nNetDelay )
   lForever := ( nSeconds == 0 )

   // Keep trying as long as specified or default
   DO WHILE lForever .OR. nSeconds > 0

      IF Eval( bBlock )
         RETURN .T.
      ENDIF

      hb_idleSleep( 0.5 )
      nSeconds -= 0.5
   ENDDO

   RETURN .F.      // Not locked

// { DBFName, Alias, { idx Names } }
// Returns:   0   All Ok
//           -1   DBF File not found
//           -2   DBF File open Error
//           -3   Index File open Error

FUNCTION NetOpenFiles( aFiles )

   LOCAL xFile, cIndex

   FOR EACH xFile IN aFiles

      IF ! hb_dbExists( xFile[ 1 ] )
         RETURN -1
      ENDIF

      IF NetDbUse( xFile[ 1 ], xFile[ 2 ], s_nNetDelay, "DBFCDX" )
         IF HB_ISARRAY( xFile[ 3 ] )
            FOR EACH cIndex IN xFile[ 3 ]
               IF hb_dbExists( xFile[ 1 ], cIndex )
                  ordListAdd( cIndex )
               ELSE
                  RETURN -3
               ENDIF
            NEXT
         ENDIF
      ELSE
         RETURN -2
      ENDIF
   NEXT

   RETURN 0

/* Network methods */

FUNCTION NetDelete()

   s_lNetOk := .F.

   IF NetLock( NET_RECLOCK )
      dbDelete()
      s_lNetOk := .T.
   ENDIF

   IF NetErr()
      s_lNetOk := .T.
      Alert( " Failed to DELETE Record -> " + hb_ntos( RecNo() ) )
   ELSE
      dbSkip( 0 )
      dbCommit()
   ENDIF

   RETURN s_lNetOk

FUNCTION NetRecall()

   s_lNetOk := .F.

   IF NetLock( NET_RECLOCK )
      dbRecall()
      s_lNetOk := .T.
   ENDIF

   IF NetErr()
      s_lNetOk := .T.
      Alert( " Failed to RECALL Record -> " + hb_ntos( RecNo() ) )
   ELSE
      dbSkip( 0 )
      dbCommit()
   ENDIF

   RETURN s_lNetOk

FUNCTION NetRecLock( nSeconds )
   RETURN s_lNetOk := NetLock( NET_RECLOCK,, hb_defaultValue( nSeconds, s_nNetDelay ) )

FUNCTION NetFileLock( nSeconds )
   RETURN s_lNetOk := NetLock( NET_FILELOCK,, hb_defaultValue( nSeconds, s_nNetDelay ) )

FUNCTION NetAppend( nSeconds, lReleaseLocks )

   LOCAL nOrd := ordSetFocus( 0 )      // Set order to 0 to append ???

   HB_SYMBOL_UNUSED( lReleaseLocks )

   s_lNetOk := .F.

   IF NetLock( NET_APPEND,, hb_defaultValue( nSeconds, s_nNetDelay ) )
#if 0
      dbGoBottom()
#endif
      s_lNetOk := .T.
   ENDIF

   ordSetFocus( nOrd )

   RETURN s_lNetOk

PROCEDURE NetFlush()

   dbCommitAll()
   dbUnlockAll()
   dbSkip( 0 )

   RETURN

PROCEDURE NetCommitAll()

   LOCAL n

   FOR n := 1 TO MAX_TABLE_AREAS
      IF ! Empty( Alias( n ) )
         ( Alias( n ) )->( dbCommit(), dbUnlock() )
      ENDIF
   NEXT

   RETURN

FUNCTION IsLocked( nRecId )
   RETURN AScan( dbRLockList(), hb_defaultValue( nRecID, RecNo() ) ) > 0

FUNCTION NetError()
   RETURN ! s_lNetOk

FUNCTION SetNetDelay( nSecs )

   LOCAL nTemp := s_nNetDelay

   IF HB_ISNUMERIC( nSecs )
      s_nNetDelay := nSecs
   ENDIF

   RETURN nTemp

FUNCTION SetNetMsgColor( cColor )

   LOCAL cTemp := s_cNetMsgColor

   IF HB_ISSTRING( cColor )
      s_cNetMsgColor := cColor
   ENDIF

   RETURN cTemp


/* Utility functions */

FUNCTION TableNew( cDbf, cAlias, cOrderBag, cDriver, lNet, cPath, lNew, lReadOnly )

   LOCAL lAuto := Set( _SET_AUTOPEN, .F. )
   LOCAL nPos
   LOCAL oDB
   LOCAL o

   hb_default( @cAlias, hb_FNameName( cDbf ) )

   IF ( nPos := AScan( t_aTables, {| e | e[ 1 ] == Upper( cAlias ) } ) ) > 0
      oDB := t_aTables[ nPos ][ 2 ]
   ELSE
      o := HBTable():New( cDbf, cAlias, cOrderBag, cDriver, ;
         hb_defaultValue( lNet, .T. ), cPath, lNew, lReadOnly )
      IF o:Open()
         oDB := o:FldInit()
      ENDIF

      AAdd( t_aTables, { Upper( cAlias ), oDB } )
   ENDIF

   Set( _SET_AUTOPEN, lAuto )

   RETURN oDB

FUNCTION GetTable( cAlias )

   LOCAL nPos

   IF ( nPos := AScan( t_aTables, {| e | e[ 1 ] == Upper( cAlias ) } ) ) > 0
      RETURN t_aTables[ nPos ][ 2 ]
   ENDIF

   RETURN NIL


CREATE CLASS HBField

   VAR Alias INIT Alias()
   VAR Name  INIT ""
   VAR TYPE  INIT "C"
   VAR Len   INIT 0
   VAR Dec   INIT 0
   VAR ORDER INIT 0
   VAR Value

   METHOD Get() INLINE ::value := ( ::alias )->( FieldGet( ::order ) )
   METHOD Put( x ) INLINE ::value := x, ;
      ( ::alias )->( FieldPut( ::order, x ) )

ENDCLASS


CREATE CLASS HBRecord

   VAR Buffer  INIT {}
   VAR Alias   INIT Alias()
   VAR Number  INIT 0
   VAR aFields INIT {}

   METHOD New( cAlias )
   METHOD Get()
   METHOD Put()

ENDCLASS

METHOD New( cAlias ) CLASS HBRecord

   LOCAL i
   LOCAL oFld
   LOCAL aStruc
   LOCAL aItem

   ::Alias   := hb_defaultValue( cAlias, Alias() )
   ::Buffer  := {}
   ::aFields := Array( ( ::alias )->( FCount() ) )

   aStruc := ( ::alias )->( dbStruct() )

   FOR EACH aItem in ::aFields
      i          := aItem:__enumIndex()
      oFld       := HBField()
      oFld:order := i
      oFld:Name  := ( ::alias )->( FieldName( i ) )
      oFld:Type  := aStruc[ i ][ DBS_TYPE ]
      oFld:LEN   := aStruc[ i ][ DBS_LEN ]
      oFld:Dec   := aStruc[ i ][ DBS_DEC ]
      oFld:Alias := ::alias
      aItem      := oFld
   NEXT

   RETURN Self

METHOD PROCEDURE Get() CLASS HBRecord

   LOCAL xField

   FOR EACH xField IN ::aFields
      xField:Get()
      ::buffer[ xField:__enumIndex() ] := xField:value
   NEXT

   RETURN

METHOD PROCEDURE Put() CLASS HBRecord

   LOCAL xField

   FOR EACH xField IN ::aFields
      IF !( xField:Value == ::buffer[ xField:__enumIndex() ] )
         xField:Put( ::buffer[ xField:__enumIndex() ] )
         ::buffer[ xField:__enumIndex() ] := xField:value
      ENDIF
   NEXT

   RETURN

/* CLASS HBTable */

CREATE CLASS HBTable

   VAR Buffer        INIT {}       // 1
   VAR Alias         INIT Alias()  // 2
   VAR Area          INIT 0        // 3

   VAR oRec
   VAR aStruc        INIT {}
   VAR nRecno        INIT 0
   VAR cDbf          INIT ""
   VAR cOrderBag     INIT ""
   VAR cOrderFile    INIT ""
   VAR cPath         INIT ""
   VAR Driver        INIT "DBFCDX"
   VAR IsNew         INIT .T.
   VAR IsReadOnly    INIT .F.
   VAR IsNet         INIT .T.
   VAR aSaveState    INIT {}
   VAR lMonitor      INIT .F.
   VAR ReadBuffers   INIT {}
   VAR WriteBuffers  INIT {}
   VAR DeleteBuffers INIT {}
   VAR nDataOffset   INIT 0
   VAR BlankBuffer   INIT {}
   VAR aOrders       INIT {}
   VAR aChildren     INIT {}
   VAR oParent

   METHOD Eof() INLINE ( ::Alias )->( Eof() )
   METHOD Bof() INLINE ( ::Alias )->( Bof() )
   METHOD RecNo() INLINE ( ::Alias )->( RecNo() )
   METHOD LastRec() INLINE ( ::Alias )->( LastRec() )
   METHOD Skip( n ) INLINE ( ::Alias )->( dbSkip( n ) ), ;
      ::nRecno := ( ::Alias )->( RecNo() )

   METHOD Goto( n ) INLINE ( ::Alias )->( dbGoto( n ) )
   METHOD goTop() INLINE ( ::Alias )->( dbGoTop() )
   METHOD goBottom() INLINE ( ::Alias )->( dbGoBottom() )
   METHOD SetFocus() INLINE ( ::Alias )->( Select( ::Alias ) )
   METHOD Append( l ) INLINE iif( ::isNet, ( ::Alias )->( NetAppend( l ) ), ;
      ( ::alias )->( dbAppend() ) )
   METHOD Recall() INLINE ( ::Alias )->( NetRecall() )

   METHOD Locate( bFor, bWhile, nNext, nRec, lRest ) INLINE ;
      ( ::Alias )->( __dbLocate( bFor, bWhile, nNext, nRec, lRest ) )
   METHOD Continue() INLINE ( ::Alias )->( __dbContinue() )
   METHOD Found() INLINE ( ::Alias )->( Found() )
   METHOD Kill() INLINE ;
      ( ::Alias )->( dbCommit() ), ;
      ( ::Alias )->( dbUnlock() ), ;
      ( ::Alias )->( dbCloseArea() ), ;
      ::ClearBuffers()
   METHOD ClearBuffers() INLINE ;
      ::ReadBuffers := {}, ;
      ::WriteBuffers := {}, ;
      ::DeleteBuffers := {}

   METHOD dbIsShared() INLINE ( ::Alias )->( dbInfo( DBI_SHARED ) )
   METHOD dbIsFLocked() INLINE ( ::Alias )->( dbInfo( DBI_ISFLOCK ) )
   METHOD dbLockCount() INLINE ( ::Alias )->( dbInfo( DBI_LOCKCOUNT ) )
   METHOD dbInfo( n, x ) INLINE ( ::Alias )->( dbInfo( n, x ) )
   METHOD dbGetAlias() INLINE ( ::Alias )
   METHOD dbFullPath() INLINE ( ::Alias )->( dbInfo( DBI_FULLPATH ) )
   METHOD IsRLocked( n ) INLINE ( ::Alias )->( dbRecordInfo( DBRI_LOCKED, n ) )
   METHOD IsRUpdated( n ) INLINE ( ::Alias )->( dbRecordInfo( DBRI_UPDATED, n ) )
   METHOD dbRecordInfo( n, x ) INLINE ( ::Alias )->( dbRecordInfo( n,, x ) )
   METHOD dbOrderInfo( n, x, u ) INLINE ( ::Alias )->( dbOrderInfo( n, ::cOrderFile, x, u ) )
   METHOD OrderCount() INLINE ( ::Alias )->( dbOrderInfo( DBOI_ORDERCOUNT, ::cOrderFile ) )
   METHOD AutoOpen( l ) INLINE ( ::Alias )->( dbOrderInfo( DBOI_AUTOOPEN, ::cOrderFile,, l ) )
   METHOD AutoShare( l ) INLINE ( ::Alias )->( dbOrderInfo( DBOI_AUTOSHARE, ::cOrderFile,, l ) )
   METHOD Used() INLINE Select( ::Alias ) > 0
   METHOD ordSetFocus( ncTag ) INLINE ( ::Alias )->( ordSetFocus( ncTag ) )
   METHOD ordName( nOrder ) INLINE ( ::Alias )->( ordName( nOrder, ::cOrderBag ) )
   METHOD ordNumber( cOrder ) INLINE ( ::Alias )->( ordNumber( cOrder, ::cOrderBag ) )
   METHOD ordScope( n, u ) INLINE ( ::Alias )->( ordScope( n, u ) )
   METHOD ordIsUnique( nc ) INLINE ( ::Alias )->( ordIsUnique( nc, ::cOrderBag ) )
   METHOD ordSkipUnique( n ) INLINE ( ::Alias )->( ordSkipUnique( n ) )
   METHOD ordSetRelation( n, b, c ) INLINE ( ::Alias )->( ordSetRelation( n, b, c ) )
   METHOD SetTopScope( xScope ) INLINE ( ::alias )->( ordScope( TOPSCOPE, xScope ) )
   METHOD SetBottomScope( xScope ) INLINE ( ::alias )->( ordScope( BOTTOMSCOPE, xScope ) )
   METHOD KillScope() INLINE ;
      ( ::alias )->( ordScope( TOPSCOPE, NIL ) ), ;
      ( ::alias )->( ordScope( BOTTOMSCOPE, NIL ) )

   METHOD New( cDbf, cAlias, cOrderBag, cDriver, lNet, cPath, lNew, lReadOnly )

   METHOD Open()

   METHOD dbMove( nDirection )
   METHOD FldInit()
   METHOD Read( lKeepBuffer )
   METHOD ReadBlank( lKeepBuffer )
   METHOD Write( lKeepBuffer )
   METHOD BufWrite( aBuffer )
   METHOD Delete( lKeepBuffer )
   METHOD SetMonitor( l )
   METHOD Undo( nBuffer, nLevel )

   METHOD dbSkip( n ) INLINE ( ::Alias )->( dbSkip( n ) ), ;
      ::nRecno := ( ::alias )->( RecNo() )

   METHOD dbGoto( n ) INLINE ( ::Alias )->( dbGoto( n ) )

   METHOD dbEval( a, b, c, d, e, f ) INLINE ( ::Alias )->( dbEval( a, b, c, d, e, f ) )
   METHOD dbSeek( a, b, c ) INLINE ( ::Alias )->( dbSeek( a, b, c ) )


   METHOD dbFilter() INLINE ( ::Alias )->( dbFilter() )
   METHOD SetFilter( c ) INLINE ;
      iif( HB_ISSTRING( c ), ( ::Alias )->( dbSetFilter( hb_macroBlock( c ), c ) ), ;
      ( ::Alias )->( dbClearFilter() ) )

   METHOD AddChild( oChild, cKey )

   METHOD AddOrder( cTag, cKey, cLabel, cFor, cWhile, lUnique, bEval, nInterval )
   METHOD GetOrderLabels()
   METHOD SetOrder( xTag )
   METHOD GetOrder( xOrder )
   METHOD FastReindex()
   METHOD ReIndex()
   METHOD CreateTable( cFile )
   METHOD AddField( f, t, l, d )
   METHOD Gentable()

   ERROR HANDLER OnError( uParam )

ENDCLASS

// Constructor

METHOD New( cDbf, cAlias, cOrderBag, cDriver, lNet, cPath, lNew, lReadOnly ) CLASS HBTable

   LOCAL cOldRdd

   ::IsNew      := hb_defaultValue( lNew, .T. )
   ::IsNet      := hb_defaultValue( lNet, .F. )
   ::IsReadOnly := hb_defaultValue( lReadOnly, .F. )
   ::cDbf       := cDbf
   ::cPath      := hb_defaultValue( cPath, Set( _SET_DEFAULT ) )
   ::cOrderBag  := hb_FNameName( hb_defaultValue( cOrderBag, hb_FNameName( cDbf ) ) )
   cOldRdd      := rddSetDefault( ::driver )
   ::cOrderFile := ::cOrderBag + ordBagExt()
   rddSetDefault( cOldRdd )
   ::Driver      := hb_defaultValue( cDriver, "DBFCDX" )
   ::aOrders     := {}
   ::Area        := 0
   ::Alias       := hb_defaultValue( cAlias, hb_FNameName( cDbf ) )
   ::nDataOffset := Len( self )

   RETURN Self

METHOD Open() CLASS HBTable

   dbUseArea( ::IsNew, ::Driver, ::cDbf, ::Alias, ::IsNET, ::IsREADONLY )

   IF ::IsNET .AND. NetErr()
      Alert( "Net Open Fail !!!" )
      RETURN .F.
   ENDIF

   dbSelectArea( ::Alias )
   ::Area := Select()
   IF HB_ISSTRING( ::cOrderBag ) .AND. hb_dbExists( ::cDbf, ::cPath + ::cOrderFile )
      SET INDEX TO ( ::cPath + ::cOrderBag )
      ( ::Alias )->( ordSetFocus( 1 ) )
   ENDIF

   ::Buffer := Array( ( ::Alias )->( FCount() ) )
   ::aStruc := ( ::Alias )->( dbStruct() )

   ::dbMove( _DB_TOP )

   RETURN .T.

METHOD PROCEDURE DBMove( nDirection ) CLASS HBTable

   SWITCH hb_defaultValue( nDirection, 0 )
   CASE 0
      ( ::Alias )->( dbSkip( 0 ) )
      EXIT
   CASE _DB_TOP
      ( ::Alias )->( dbGoTop() )
      EXIT
   CASE _DB_BOTTOM
      ( ::Alias )->( dbGoBottom() )
      EXIT
   CASE _DB_BOF
      ( ::Alias )->( dbGoTop() )
      ( ::Alias )->( dbSkip( -1 ) )
      EXIT
   CASE _DB_EOF
      ( ::Alias )->( dbGoBottom() )
      ( ::Alias )->( dbSkip() )
      EXIT
   OTHERWISE
      ( ::Alias )->( dbGoto( nDirection ) )
   ENDSWITCH

   RETURN

// Insert field definitions and generate virtual child class

METHOD FldInit() CLASS HBTable

   LOCAL i
   LOCAL aDb
   LOCAL oNew
   LOCAL nScope := 1

   ::nDataOffset := Len( self ) - 1

   ::Buffer := Array( ( ::Alias )->( FCount() ) )
   IF Empty( ::Buffer )
      ::Read()
   ENDIF

   // Create new oObject class from this one

   adb := HBClass():new( ::alias, { "hbtable" } )

   FOR i := 1 TO FCount()
      adb:AddData( ( ::Alias )->( FieldName( i ) ), , , nScope )
   NEXT

   aDB:create()

   oNew := adb:Instance()

   oNew:IsNew       := ::IsNew
   oNew:IsNet       := ::IsNet
   oNew:IsReadOnly  := ::IsReadOnly
   oNew:cDbf        := ::cDbf
   oNew:cPath       := ::cPath
   oNew:cOrderBag   := ::cOrderBag
   oNew:cOrderFile  := ::cOrderFile
   oNew:Driver      := ::Driver
   oNew:Area        := ::Area
   oNew:Alias       := ::Alias
   oNew:aStruc      := ::aStruc
   oNew:BlankBuffer := ::BlankBuffer
   oNew:aOrders     := ::aOrders
   oNew:oParent     := ::oParent
   oNew:Buffer      := ::buffer

   dbSelectArea( oNew:Alias )

   oNew:Area := Select()

   oNew:Read()

   IF HB_ISSTRING( oNew:cOrderBag ) .AND. hb_dbExists( oNew:cDbf, oNew:cPath + oNew:cOrderFile )
      SET INDEX TO ( oNew:cPath + oNew:cOrderBag )
      ( oNew:Alias )->( ordSetFocus( 1 ) )
   ENDIF

   oNew:buffer := Array( ( oNew:alias )->( FCount() ) )
   oNew:aStruc := ( oNew:alias )->( dbStruct() )

   IF oNew:Used()
      oNew:dbMove( _DB_TOP )
      oNew:Read()
   ENDIF

   RETURN oNew

METHOD PROCEDURE Read( lKeepBuffer ) CLASS HBTable

   LOCAL i
   LOCAL nSel  := Select( ::Alias )
   LOCAL adata := Array( 1, 2 )
   LOCAL Buffer

   FOR EACH Buffer in ::Buffer
      i      := Buffer:__enumIndex()
      Buffer := ( ::Alias )->( FieldGet( i ) )

      adata[ 1 ][ 1 ] := ( ::Alias )->( FieldName( i ) )
      adata[ 1 ][ 2 ] := ( ::Alias )->( FieldGet( i ) )
      __objSetValueList( Self, aData )
   NEXT

   IF hb_defaultValue( lKeepBuffer, .F. ) .OR. ::lMonitor
      AAdd( ::ReadBuffers, { ( ::Alias )->( RecNo() ), ::Buffer } )
   ENDIF

   dbSelectArea( nSel )

   RETURN

METHOD PROCEDURE ReadBlank( lKeepBuffer ) CLASS HBTable

   LOCAL i
   LOCAL nSel   := Select( ::Alias )
   LOCAL nRec   := ( ::Alias )->( RecNo() )
   LOCAL adata  := Array( 1, 2 )
   LOCAL Buffer

   ( ::Alias )->( dbGoBottom() )
   ( ::Alias )->( dbSkip() )         // go EOF

   FOR EACH Buffer in ::Buffer
      i      := Buffer:__enumIndex()
      Buffer := ( ::Alias )->( FieldGet( i ) )

      adata[ 1 ][ 1 ] := ( ::Alias )->( FieldName( i ) )
      adata[ 1 ][ 2 ] := ( ::Alias )->( FieldGet( i ) )
      __objSetValueList( Self, aData )
   NEXT

   IF hb_defaultValue( lKeepBuffer, .F. ) .OR. ::lMonitor
      AAdd( ::ReadBuffers, { ( ::Alias )->( RecNo() ), ::Buffer } )
   ENDIF

   ( ::Alias )->( dbGoto( nRec ) )
   dbSelectArea( nSel )

   RETURN

METHOD Write( lKeepBuffer ) CLASS HBTable

   LOCAL i
   LOCAL aOldBuffer := Array( ( ::Alias )->( FCount() ) )
   LOCAL nSel       := Select( ::Alias )
   LOCAL nOrd       := ( ::Alias )->( ordSetFocus() )
   LOCAL aData      := __objGetValueList( Self )
   LOCAL xBuffer
   LOCAL n

   IF hb_defaultValue( lKeepBuffer, .F. ) .OR. ::lMonitor
      // --> save old record in temp buffer
      FOR EACH xBuffer IN aOldBuffer
         xBuffer := ( ::Alias )->( FieldGet( xBuffer:__enumIndex() ) )
      NEXT

      AAdd( ::WriteBuffers, { ( ::Alias )->( RecNo() ), aOldBuffer } )
   ENDIF

   IF ::isNet .AND. !( ::Alias )->( NetRecLock() )
      RETURN .F.
   ENDIF

   ( ::Alias )->( ordSetFocus( 0 ) )

   FOR i := 1 TO ( ::Alias )->( FCount() )
      n := AScan( adata, {| a | a[ 1 ] == ( ::Alias )->( FieldName( i ) ) } )
      ( ::Alias )->( FieldPut( i, adata[ n ][ 2 ] ) )
   NEXT

   ( ::Alias )->( dbSkip( 0 ) )         // same as commit
   IF ::isNet
      ( ::Alias )->( dbRUnlock() )
   ENDIF
   ( ::Alias )->( ordSetFocus( nOrd ) )
   dbSelectArea( nSel )

   RETURN .T.

METHOD BUFWrite( aBuffer ) CLASS HBTable

   LOCAL nSel := Select( ::Alias )
   LOCAL nOrd := ( ::Alias )->( ordSetFocus() )
   LOCAL Buffer

   IF ::isNet .AND. !( ::Alias )->( NetRecLock() )
      RETURN .F.
   ENDIF

   ( ::Alias )->( ordSetFocus( 0 ) )

   FOR EACH Buffer IN hb_defaultValue( aBuffer, ::Buffer )
      ( ::Alias )->( FieldPut( Buffer:__enumIndex(), Buffer ) )
   NEXT

   ( ::Alias )->( dbSkip( 0 ) )
   IF ::isNet
      ( ::Alias )->( dbRUnlock() )
   ENDIF
   ( ::Alias )->( ordSetFocus( nOrd ) )
   dbSelectArea( nSel )

   RETURN .T.

METHOD Delete( lKeepBuffer ) CLASS HBTable

   LOCAL lRet
   LOCAL lDeleted := Set( _SET_DELETED, .F. )  // make deleted records visible

   ::Read()

   IF ::isNet
      lRet := ( ::Alias )->( NetDelete() )
   ELSE
      ( ::alias )->( dbDelete() )
      lRet := .T.
   ENDIF

   IF ( hb_defaultValue( lKeepBuffer, .F. ) .OR. ::lMonitor ) .AND. lRet
      AAdd( ::DeleteBuffers, { ( ::Alias )->( RecNo() ), ::Buffer } )
   ENDIF

   IF ::isNet
      ( ::Alias )->( dbUnlock() )
   ENDIF

   Set( _SET_DELETED, lDeleted )

   RETURN lRet

METHOD SetMonitor( l ) CLASS HBTable

   LOCAL lTemp := ::lMonitor

   ::lMonitor := ! l

   RETURN lTemp

// Transaction control subsystem

METHOD Undo( nBuffer, nLevel ) CLASS HBTable

   LOCAL nLen
   LOCAL lRet      := .F.
   LOCAL lDelState := Set( _SET_DELETED )
   LOCAL nRec      := ::RecNo()
   LOCAL aBuffers

   SWITCH hb_defaultValue( nBuffer, _WRITE_BUFFER )
   CASE _DELETE_BUFFER

      IF ! Empty( ::DeleteBuffers )

         Set( _SET_DELETED, .F. )       // make deleted records visible temporarily

         nLen := Len( ::deleteBuffers )
         hb_default( @nLevel, nLen )

         IF nLevel == 0 .OR. nLevel == nLen     // DO ALL

            FOR EACH aBuffers IN ::deleteBuffers
               ( ::Alias )->( dbGoto( aBuffers[ 1 ] ) )
               lRet := ( ::Alias )->( NetRecall() )
            NEXT

            IF lRet
               ::deleteBuffers := {}
            ENDIF

         ELSE  // DO CONTROLLED

            FOR EACH aBuffers IN ::deleteBuffers
               IF aBuffers:__enumIndex() > ( nLen - nLevel )
                  ( ::Alias )->( dbGoto( aBuffers[ 1 ] ) )
                  lRet := ( ::Alias )->( NetRecall() )
               ENDIF
            NEXT

            IF lRet
               ASize( ::deleteBuffers, nLen - nLevel )
            ENDIF
         ENDIF

         Set( _SET_DELETED, lDelState )
      ENDIF
      EXIT

   CASE _WRITE_BUFFER

      IF ! Empty( ::WriteBuffers )

         nLen := Len( ::WriteBuffers )
         hb_default( @nLevel, nLen )

         IF nLevel == 0 .OR. nLen == nLevel   // Do All

            FOR EACH aBuffers IN ::writeBuffers

               ( ::Alias )->( dbGoto( aBuffers[ 1 ] ) )

               IF ::BufWrite( aBuffers[ 2 ] )
                  lRet := .T.
               ELSE
                  Alert( "Rollback Failed..." )
                  lRet := .F.
               ENDIF
            NEXT

            IF lRet
               ::WriteBuffers := {}  // erase entries
            ENDIF

         ELSE       // do controlled

            FOR EACH aBuffers IN ::writeBuffers
               IF aBuffers:__enumIndex() > ( nLen - nLevel )

                  ( ::Alias )->( dbGoto( aBuffers[ 1 ] ) )

                  IF ::BufWrite( aBuffers[ 2 ] )
                     lRet := .T.
                  ELSE
                     Alert( "Rollback Failed..." )
                     lRet := .F.
                  ENDIF
               ENDIF
            NEXT

            // erase entries
            IF lRet
               ASize( ::WriteBuffers, nLen - nLevel )
            ENDIF
         ENDIF
      ENDIF
      EXIT

   ENDSWITCH

   ( ::Alias )->( dbUnlock() )
   ( ::Alias )->( dbGoto( nRec ) )
   ::Read()

   RETURN lRet

// Order management

METHOD AddOrder( cTag, cKey, cLabel, cFor, cWhile, lUnique, bEval, nInterval ) CLASS HBTable

   LOCAL oOrd := HBOrder():New( cTag, cKey, cLabel, cFor, cWhile, lUnique, bEval, nInterval )

   oOrd:oTable    := Self
   oOrd:cOrderBag := ::cOrderBag

   AAdd( ::aOrders, oOrd )

   RETURN oOrd

METHOD Reindex() CLASS HBTable

   LOCAL nSel := Select( ::Alias )
   LOCAL nOrd := ( ::Alias )->( ordSetFocus( 0 ) )

   IF Len( ::aOrders ) > 0

      IF ::Used()
         ::Kill()
      ENDIF

      ::Isnet := .F.

      IF hb_dbExists( ::cDbf, ::cPath + ::cOrderFile )
         IF ! hb_dbDrop( ::cDbf, ::cPath + ::cOrderFile )
#if 0
            Alert( "Index *NOT* Deleted !!!" )
#endif
         ENDIF
      ENDIF

      IF ! ::Open()
         RETURN .F.
      ENDIF

      AEval( ::aOrders, {| o | o:Create() } )

      ::Kill()
      ::IsNet := .T.

      IF ! ::Open()
         RETURN .F.
      ENDIF
   ENDIF

   ( ::Alias )->( dbSetIndex( ::cOrderBag ) )
   ( ::Alias )->( ordSetFocus( nOrd ) )
   ( ::Alias )->( dbGoTop() )
   ( ::Alias )->( dbUnlock() )
   dbSelectArea( nSel )

   RETURN .T.

METHOD FastReindex() CLASS HBTable

   LOCAL nSel := Select( ::Alias )
   LOCAL nOrd := ( ::Alias )->( ordSetFocus( 0 ) )

   IF Len( ::aOrders ) > 0

      ::Kill()

      ::Isnet := .F.
      IF hb_dbExists( ::cDbf, ::cPath + ::cOrderFile )
         IF ! hb_dbDrop( ::cDbf, ::cPath + ::cOrderFile )
#if 0
            Alert( "Index *NOT* Deleted !!!" )
#endif
         ENDIF
      ENDIF

      IF ! ::Open()
         RETURN .F.
      ENDIF

      ( ::Alias )->( ordListRebuild() )

      ::Kill()
      ::IsNet := .T.

      IF ! ::Open()
         RETURN .F.
      ENDIF
   ENDIF

   ( ::Alias )->( dbSetIndex( ::cOrderBag ) )
   ( ::Alias )->( ordSetFocus( nOrd ) )
   ( ::Alias )->( dbGoTop() )
   ( ::Alias )->( dbUnlock() )
   dbSelectArea( nSel )

   RETURN .T.

METHOD GetOrder( xOrder ) CLASS HBTable

   LOCAL nPos
   LOCAL xType := ValType( xOrder )

   DO CASE
   CASE xType == "C"
      nPos := AScan( ::aOrders, {| e | e:Tag == xOrder } )
   CASE xType == "N" .AND. xOrder > 0
      nPos := xOrder
   OTHERWISE
      nPos := 0
   ENDCASE

   IF nPos == 0
      nPos := 1
   ENDIF

   RETURN ::aOrders[ nPos ]    // returns oOrder

METHOD SetOrder( xTag ) CLASS HBTable

   LOCAL nOldOrd := ( ::Alias )->( ordSetFocus() )

   SWITCH ValType( xTag )
   CASE "C"                    // we have an Order-TAG
      ( ::Alias )->( ordSetFocus( xTag ) )
      EXIT
   CASE "N"                    // we have an Order-Number
      IF xTag <= 0
         ( ::Alias )->( ordSetFocus( 0 ) )
      ELSE
         ::Getorder( xTag ):SetFocus()
      ENDIF
      EXIT
   CASE "O"                    // we have an Order-Object
      xTag:SetFocus()
      EXIT
   OTHERWISE
      ( ::Alias )->( ordSetFocus( 0 ) )
   ENDSWITCH

   RETURN nOldOrd

METHOD GetOrderLabels() CLASS HBTable

   LOCAL aRet := {}

   IF ! Empty( ::aOrders )
      AEval( ::aOrders, {| e | AAdd( aRet, e:Label ) } )
   ENDIF

   RETURN aRet

// Relation Methods

PROCEDURE AddChild( oChild, cKey ) CLASS HBTable

   AAdd( ::aChildren, { oChild, cKey } )
   oChild:oParent := Self
   ( ::Alias )->( ordSetRelation( oChild:Alias, hb_macroBlock( cKey ), cKey ) )

   RETURN

METHOD CreateTable( cFile ) CLASS HBTable

   ::cDbf := cFile
   IF Len( ::aStruc ) > 0
      ::aStruc  := {}
      ::aOrders := {}
   ENDIF

   RETURN Self

METHOD PROCEDURE AddField( f, t, l, d ) CLASS HBTable

   AAdd( ::aStruc, { f, t, l, d } )

   RETURN

METHOD PROCEDURE Gentable() CLASS HBTable

   dbCreate( ::cDbf, ::aStruc, ::Driver )

   RETURN

METHOD OnError( uParam ) CLASS HBTable

   LOCAL cMsg := __GetMessage()
   LOCAL nPos
   LOCAL oErr

   IF uParam != NIL .AND. hb_LeftEq( cMsg, "_" )
      cMsg := SubStr( cMsg, 2 )
   ENDIF

   IF ( nPos := ( ::Alias )->( FieldPos( cMsg ) ) ) == 0

      oErr := ErrorNew()
      oErr:Args          := { Self, cMsg, uParam }
      oErr:CanDefault    := .F.
      oErr:CanRetry      := .F.
      oErr:CanSubstitute := .T.
      oErr:Description   := "Invalid class member"
      oErr:GenCode       := EG_NOVARMETHOD
      oErr:Operation     := "HBTable:" + cMsg
      oErr:Severity      := ES_ERROR
      oErr:SubCode       := -1
      oErr:SubSystem     := "HBTable"

      RETURN Eval( ErrorBlock(), oErr )
   ENDIF

   RETURN ( ::Alias )->( iif( uParam == NIL, FieldGet( nPos ), FieldPut( nPos, uParam ) ) )

CREATE CLASS HBOrder

   VAR oTable
   VAR cOrderBag
   VAR Label, TAG
   VAR cKey, bKey
   VAR cFor, bFor
   VAR cWhile, bWhile
   VAR unique INIT .F.
   VAR bEval
   VAR nInterval

   METHOD Alias() INLINE ::oTable:Alias

   METHOD New( cTag, cKey, cLabel, cFor, cWhile, lUnique, bEval, nInterval, cOrderBag )
   METHOD Create()

   METHOD SetFocus() INLINE ( ::alias )->( ordSetFocus( ::Tag, ::cOrderBag ) )
   METHOD Destroy() INLINE ( ::alias )->( ordDestroy( ::Tag, ::cOrderBag ) )
   METHOD ordDestroy() INLINE ( ::alias )->( ordDestroy( ::Tag, ::cOrderBag ) )
   METHOD ordBagExt() INLINE ( ::alias )->( ordBagExt() )
   METHOD ordKeyCount() INLINE ( ::alias )->( ordKeyCount( ::Tag, ::cOrderBag ) )
   METHOD ordFor() INLINE ( ::alias )->( ordFor( ::Tag, ::cOrderBag ) )
   METHOD ordIsUnique() INLINE ( ::alias )->( ordIsUnique( ::Tag, ::cOrderBag ) )
   METHOD ordKey() INLINE ( ::alias )->( ordKey( ::Tag, ::cOrderBag ) )
   METHOD ordKeyNo() INLINE ( ::alias )->( ordKeyNo( ::Tag, ::cOrderBag ) )
   METHOD ordKeyVal() INLINE ( ::alias )->( ordKeyVal( ::Tag, ::cOrderBag ) )

ENDCLASS

METHOD New( cTag, cKey, cLabel, cFor, cWhile, lUnique, bEval, nInterval, cOrderBag ) CLASS HBOrder

   hb_default( @cKey, ".T." )
   hb_default( @cFor, ".T." )
   hb_default( @cWhile, ".T." )

   __defaultNIL( @cLabel, cTag )

   ::cOrderBag := cOrderBag
   ::Tag       := cTag
   ::cKey      := cKey
   ::cFor      := cFor
   ::cWhile    := cWhile
   ::bKey      := hb_macroBlock( cKey )
   ::bFor      := hb_macroBlock( cFor )
   ::bWhile    := hb_macroBlock( cWhile )
   ::unique    := hb_defaultValue( lUnique, .F. )
   ::bEval     := hb_defaultValue( bEval, {|| .T. } )
   ::nInterval := hb_defaultValue( nInterval, 1 )
   ::Label     := cLabel

   RETURN Self

METHOD PROCEDURE Create() CLASS HBOrder

   __defaultNIL( @::cOrderBag, ::oTable:cOrderBag )

   ( ::alias )->( ordCondSet( ::cFor, ::bFor, .T., ::bWhile, ::bEval, ::nInterval ) )
   ( ::alias )->( ordCreate( ::cOrderBag, ::Tag, ::cKey, ::bKey, ::Unique ) )

   RETURN
