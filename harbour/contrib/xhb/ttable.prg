/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Table,Record and Field Class
 *
 * Copyright 2000-2003 Manos Aspradakis maspr@otenet.gr
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 *
 * Copyright 2000 -2002 Luiz Rafael Culik
 * Methods CreateTable(),Gentable(),AddField()
 * Plus optimization for Xharbour
 *
 */

#include "hbclass.ch"

#include "ttable.ch"
#include "set.ch"
#include "ord.ch"
#include "inkey.ch"
#include "dbinfo.ch"
#include "error.ch"

STATIC s_aTables := {}

/* NetWork Functions */
STATIC s_nNetDelay    := 30
STATIC s_lNetOk       := .F.
STATIC s_cNetMsgColor := "GR+/R"

FUNCTION NetDbUse( cDataBase, cAlias, nSeconds, cDriver, ;
      lNew, lOpenMode, lReadOnly )

   LOCAL nKey
   LOCAL lForever
   LOCAL cOldScreen := SaveScreen( MaxRow(), 0, MaxRow(), MaxCol() + 1 )
   LOCAL lFirstPass := .T.

   __defaultNIL( @cDriver, "DBFCDX" )
   __defaultNIL( @lNew, .T. )
   __defaultNIL( @lOpenMode, NET_OPEN_MODE )
   __defaultNIL( @lReadOnly, .F. )
   __defaultNIL( @nSeconds, s_nNetDelay )

   s_lNetOk := .F.
   nSeconds *= 1.00
   lForever := ( nSeconds == 0 )

   hb_keyIns( 255 )
   Inkey()

   DO WHILE ( lForever .OR. nSeconds > 0 ) .AND. LastKey() != K_ESC
      IF ! lFirstPass
         hb_DispOutAt( MaxRow(), 0, ;
            PadC( "Network retry | " + ;
            LTrim( Str( nSeconds, 4, 1 ) ) + " | ESCape = Exit ", ;
            MaxCol() + 1 ), ;
            s_cNetMsgColor )
         lFirstPass := .F.
      ENDIF

      dbUseArea( lNew, ;
         cDriver, cDatabase, cAlias, ;
         lOpenMode, ;
         .F. )

      IF ! NetErr()  // USE SUCCEEDS
         RestScreen( MaxRow(), 0, MaxRow(), MaxCol() + 1, cOldScreen )
         s_lNetOk := .T.
      ELSE
         lFirstPass := .F.
      ENDIF

      IF ! s_lNetOk
         nKey := Inkey( 0.5 )        // WAIT 1 SECOND
         nSeconds -= 0.5
      ELSE
         RestScreen( MaxRow(), 0, MaxRow(), MaxCol() + 1, cOldScreen )
         EXIT
      ENDIF

      IF nKey == K_ESC
         RestScreen( MaxRow(), 0, MaxRow(), MaxCol() + 1, cOldScreen )
         EXIT
      ENDIF

   ENDDO

   RestScreen( MaxRow(), 0, MaxRow(), MaxCol() + 1, cOldScreen )

   RETURN s_lNetOk

FUNCTION NetLock( nType, lReleaseLocks, nSeconds )

   LOCAL cSave       := SaveScreen( MaxRow(), 0, MaxRow(), MaxCol() + 1 )
   LOCAL lContinue   := .T.
   LOCAL lSuccess    := .F.
   LOCAL nWaitTime
   LOCAL bOperation
   LOCAL xIdentifier
   LOCAL nKey
   LOCAL nCh
   LOCAL cWord

   IF ! HB_ISNUMERIC( nType ) .OR. ;
         ( nType != 1 .AND. ;
           nType != 2 .AND. ;
           nType != 3 )
      Alert( "Invalid Argument passed to NETLOCK()" )
      RETURN lSuccess
   ENDIF

   __defaultNIL( @lReleaseLocks, .F. )
   __defaultNIL( @nSeconds, s_nNetDelay )

   nWaitTime := nSeconds

   SWITCH nType
   CASE NET_RECLOCK                        // 1 = Record Lock...
      xIdentifier := iif( lReleaseLocks, NIL, RecNo() )
      bOperation  := {| x | dbRLock( x ) }
      EXIT
   CASE NET_FILELOCK                       // 2 = File Lock...
      bOperation := {|| FLock() }
      EXIT
   CASE NET_APPEND                         // 3 = Append Blank...
      xIdentifier := lReleaseLocks
      bOperation  := {| x | dbAppend( x ), !NetErr() }
      EXIT
   ENDSWITCH

   s_lNetOk := .F.

   WHILE lContinue

      #if 0
      IF ( nKey := Inkey() ) == K_ESC
         RestScreen( MaxRow(), 0, MaxRow(), MaxCol() + 1, cSave )
         EXIT
      ENDIF
      #endif

      WHILE nSeconds > 0 .AND. lContinue
         IF Eval( bOperation, xIdentifier )
            nSeconds  := 0
            lSuccess  := .T.
            lContinue := .F.
            s_lNetOk  := .T.
            EXIT
         ELSE
            IF nType == 1
               cWord := "( " + dbInfo( DBI_ALIAS ) + " - Record Lock )"
            ELSEIF nType == 2
               cWord := "( " + dbInfo( DBI_ALIAS ) + " - File Lock )"
            ELSEIF nType == 3
               cWord := "( " + dbInfo( DBI_ALIAS ) + " - File Append )"
            ELSE
               cWord := "( " + dbInfo( DBI_ALIAS ) + " -  ??? "
            ENDIF

            hb_DispOutAt( MaxRow(), 0, ;
               PadC( "Network Retry " + cWord + " | " + Str( nSeconds, 3 ) + " | ESC Exit", MaxCol() + 1 ), ;
               s_cNetMsgColor )

            nKey := Inkey( 1 )          // Tone( 1, 1 )
            nSeconds--                  // .5
            IF nKey == K_ESC
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
         nCh := Alert( RETRY_MSG, { "  YES  ", "  NO  " } )

         lContinue := ( nCh == 1 )

         IF ! lContinue
            //EXIT
            RestScreen( MaxRow(), 0, MaxRow(), MaxCol() + 1, cSave )
            RETURN lSuccess
         ENDIF

      ENDIF
   ENDDO

   RestScreen( MaxRow(), 0, MaxRow(), MaxCol() + 1, cSave )

   RETURN lSuccess

FUNCTION NetFunc( bBlock, nSeconds )

   LOCAL lForever      // Retry forever?

   __defaultNIL( @nSeconds, s_nNetDelay )
   lForever := ( nSeconds == 0 )

   // Keep trying as long as specified or default
   DO WHILE lForever .OR. nSeconds > 0

      IF Eval( bBlock )
         RETURN .T.                 // NOTE
      ENDIF

      Inkey( 1 )    // Wait 0.5 seconds
      nSeconds -= 0.5
   ENDDO

   RETURN .F.      // Not locked

// { DBFName, Alias, { idx Names } }
// Returns:   0   All Ok
//           -1   DBF File not found
//           -2   DBF File open Error
//           -3   Index File open Error

FUNCTION NetOpenFiles( aFiles )

   LOCAL nRet := 0
   LOCAL xFile, cIndex

   FOR EACH xFile IN aFiles

      IF ! hb_FileExists( xFile[ 1 ] )
         nRet := -1
         EXIT
      ENDIF

      IF NetDbUse( xFile[ 1 ], xFile[ 2 ], s_nNetDelay, "DBFCDX" )
         IF HB_ISARRAY( xFile[ 3 ] )
            FOR EACH cIndex IN xFile[ 3 ]
               IF hb_FileExists( cIndex )
                  ordListAdd( cIndex )
               ELSE
                  nRet := -3
                  EXIT
               ENDIF
            NEXT
         ENDIF
      ELSE
         nRet := -2
         EXIT
      ENDIF
   NEXT

   RETURN nRet

/* NETWORK METHODS */

FUNCTION NetDelete()

   s_lNetOk := .F.

   IF NetLock( NET_RECLOCK )
      dbDelete()
      s_lNetOk := .T.
   ENDIF

   IF ! NetErr()
      dbSkip( 0 )
      dbCommit()
   ELSE
      s_lNetOk := .T.
      Alert( " Failed to DELETE Record -> " + Str( RecNo() ) )
   ENDIF

   RETURN s_lNetOk

FUNCTION NetReCall()

   s_lNetOk := .F.

   IF NetLock( NET_RECLOCK )
      dbRecall()
      s_lNetOk := .T.
   ENDIF

   IF ! NetErr()
      dbSkip( 0 )
      dbCommit()
   ELSE
      s_lNetOk := .T.
      Alert( " Failed to RECALL Record -> " + Str( RecNo() ) )
   ENDIF

   RETURN s_lNetOk

FUNCTION NetRecLock( nSeconds )

   __defaultNIL( @nSeconds, s_nNetDelay )

   s_lNetOk := .F.

   IF NetLock( NET_RECLOCK, , nSeconds )                     // 1
      s_lNetOk := .T.
   ENDIF

   RETURN s_lNetOk

FUNCTION NetFileLock( nSeconds )

   s_lNetOk := .F.
   __defaultNIL( @nSeconds, s_nNetDelay )

   IF NetLock( NET_FILELOCK, , nSeconds )
      s_lNetOk := .T.
   ENDIF

   RETURN s_lNetOk

FUNCTION NetAppend( nSeconds, lReleaseLocks )

   LOCAL nOrd

   __defaultNIL( @lReleaseLocks, .T. )
   __defaultNIL( @nSeconds, s_nNetDelay )
   s_lNetOk := .F.
   nOrd := ordSetFocus( 0 )          // --> set order to 0 to append ???

   IF NetLock( NET_APPEND, , nSeconds )
      // dbGoBottom()
      s_lNetOk := .T.
   ENDIF

   ordSetFocus( nOrd )

   RETURN s_lNetOk

PROCEDURE NetFlush()

   dbCommitAll()
   dbUnlockAll()
   dbSkip( 0 )

   RETURN

FUNCTION NetCommitAll()

   LOCAL n

   FOR n := 1 TO MAX_TABLE_AREAS
      IF ! Empty( Alias( n ) )
         ( Alias( n ) )->( dbCommit(), dbUnlock() )
      ENDIF
   NEXT

   RETURN n

FUNCTION IsLocked( nRecId )

   __defaultNIL( @nRecID, RecNo() )

   RETURN AScan( dbRLockList(), {| n | n == nRecID } ) > 0

FUNCTION NetError()

   RETURN !s_lNetOk

FUNCTION SetNetDelay( nSecs )

   LOCAL nTemp := s_nNetDelay

   IF nSecs != NIL
      s_nNetDelay := nSecs
   ENDIF

   RETURN nTemp

FUNCTION SetNetMsgColor( cColor )

   LOCAL cTemp := s_cNetMsgColor

   IF cColor != NIL
      s_cNetMsgColor := cColor
   ENDIF

   RETURN cTemp


/****
*     Utility functions
*
*     TableNew()
*
*     getTable()
*/

FUNCTION TableNew( cDBF, cALIAS, cOrderBag, cDRIVER, ;
      lNET, cPATH, lNEW, lREADONLY )

   LOCAL nPos
   LOCAL lAuto
   LOCAL oDB
   LOCAL o

   __defaultNIL( @lNET, .T. )
   __defaultNIL( @lNEW, .T. )
   __defaultNIL( @lREADONLY, .F. )
   __defaultNIL( @cDRIVER, "DBFCDX" )
   __defaultNIL( @cPATH, Set( _SET_DEFAULT ) )
   __defaultNIL( @cAlias, FixExt( cDbf ) )
   __defaultNIL( @cOrderBag, FixExt( cDbf ) )

   lAuto := Set( _SET_AUTOPEN, .F. )

   IF ( nPos := AScan( s_aTables, {| e | e[ 1 ] == Upper( cALIAS ) } ) ) > 0

      oDB := s_aTables[ nPos, 2 ]

   ELSE
      o := HBTable():New( cDBF, cALIAS, cOrderBag, cDRIVER, ;
         lNET, cPATH, lNEW, lREADONLY )
      IF o:Open()
         oDB := o:FldInit()
      ENDIF

      AAdd( s_aTables, { Upper( cAlias ), oDB } )

   ENDIF

   Set( _SET_AUTOPEN, lAuto )

   RETURN oDB

FUNCTION GetTable( cAlias )

   LOCAL nPos
   LOCAL oDB

   IF ( nPos := AScan( s_aTables, {| e | e[ 1 ] == Upper( cALIAS ) } ) ) > 0
      oDB := s_aTables[ nPos, 2 ]
   ENDIF

   RETURN oDB


/****
*
*     CLASS HBField()
*
*/

CREATE CLASS HBField

   VAR ALIAS INIT Alias()
   VAR Name INIT ""
   VAR TYPE INIT "C"
   VAR Len INIT 0
   VAR Dec INIT 0
   VAR ORDER INIT 0
   VAR Value

   METHOD Get() INLINE ::value := ( ::alias )->( FieldGet( ::order ) )
   METHOD Put( x ) INLINE ::value := x, ;
      ( ::alias )->( FieldPut( ::order, x ) )

ENDCLASS

/****
*
*     CLASS HBRecord()
*
*
*
*/

CREATE CLASS HBRecord

   VAR Buffer INIT {}
   VAR ALIAS INIT Alias()
   VAR Number INIT 0
   VAR aFields INIT {}

   METHOD New( cAlias )
   METHOD Get()
   METHOD Put()

ENDCLASS

METHOD NEW( cAlias ) CLASS HBRecord

   LOCAL i
   LOCAL oFld
   LOCAL aStruc
   LOCAL aItem

   __defaultNIL( @cAlias, Alias() )

   ::Alias   := cAlias
   ::Buffer  := {}
   ::aFields := Array( ( ::alias )->( FCount() ) )

   aStruc := ( ::alias )->( dbStruct() )

   FOR EACH aItem in ::aFields
      i          := aItem:__EnumIndex()
      oFld       := HBField()
      oFld:order := i
      oFld:Name  := ( ::alias )->( FieldName( i ) )
      oFld:Type  := aStruc[ i, 2 ]
      oFld:LEN   := aStruc[ i, 3 ]
      oFld:Dec   := aStruc[ i, 4 ]
      oFld:Alias := ::alias
      aItem      := oFld
   NEXT

   RETURN Self

METHOD PROCEDURE Get() CLASS HBRecord

   LOCAL xField

   FOR EACH xField IN ::aFields
      xField:Get()
      ::buffer[ xField:__EnumIndex() ] := xField:value
   NEXT

   RETURN

METHOD PROCEDURE Put() CLASS HBRecord

   LOCAL xField

   FOR EACH xField IN ::aFields
      IF !( xField:Value == ::buffer[ xField:__EnumIndex() ] )
         xField:PUT( ::buffer[ xField:__EnumIndex() ] )
         ::buffer[ xField:__EnumIndex() ] := xField:value
      ENDIF
   NEXT

   RETURN

/****
*
*     CLASS HBTable
*
*/

//METHOD SetFocus()    INLINE ( ::Alias )->( Select( ::Area ) )
//
//
//encapsulated methods
//
//
//Methods
//
//
//table movement
//
//
//RELATION
//
//
//ORDER Management
//

CREATE CLASS HBTable

   VAR Buffer INIT {}                  // 1
   VAR ALIAS INIT Alias()              // 2
   VAR Area INIT 0 // 3

   VAR oRec
   VAR aStruc INIT {}
   VAR nRecno INIT 0
   VAR cDBF INIT ""
   VAR cOrderBag INIT ""
   VAR cOrderFile INIT ""
   VAR cPATH INIT ""
   VAR Driver INIT "DBFCDX"
   VAR IsNew INIT .T.
   VAR IsReadOnly INIT .F.
   VAR IsNet INIT .T.
   VAR aSaveState INIT {}
   VAR lMonitor INIT .F.
   VAR ReadBuffers INIT {}
   VAR WriteBuffers INIT {}
   VAR DeleteBuffers INIT {}
   VAR nDataOffset INIT 0
   VAR BlankBuffer INIT {}
   VAR aOrders INIT {}
   VAR aChildren INIT {}
   VAR oParent

   METHOD EOF() INLINE ( ::Alias )->( EOF() )
   METHOD BOF() INLINE ( ::Alias )->( BOF() )
   METHOD RecNo() INLINE ( ::Alias )->( RecNo() )
   METHOD LastRec() INLINE ( ::Alias )->( LastRec() )
   METHOD SKIP( n ) INLINE ( ::Alias )->( dbSkip( n ) ), ;
      ::nRecno := ( ::Alias )->( RecNo() )

   METHOD GOTO( n ) INLINE ( ::Alias )->( dbGoto( n ) )
   METHOD goTop() INLINE ( ::Alias )->( dbGoTop() )
   METHOD goBottom() INLINE ( ::Alias )->( dbGoBottom() )
   METHOD SetFocus() INLINE ( ::Alias )->( Select( ::ALias ) )
   METHOD APPEND( l ) INLINE iif( ::isNet, ( ::Alias )->( NetAppend( l ) ), ;
      ( ::alias )->( dbAppend() ) )
   METHOD RECALL(  ) INLINE ( ::Alias )->( NetRecall(  ) )

   METHOD LOCATE( bFor, bWhile, nNext, nRec, lRest ) INLINE ;
      ( ::Alias )->( __dbLocate( bFor, bWhile, ;
      nNext, nRec, lRest ) )
   METHOD CONTINUE() INLINE ( ::Alias )->( __dbContinue() )
   METHOD Found() INLINE ( ::Alias )->( Found() )
   METHOD Kill() INLINE ( ::Alias )->( dbCommit() ), ;
      ( ::Alias )->( dbUnlock() ), ;
      ( ::Alias )->( dbCloseArea() ), ;
      ::ClearBuffers()
   METHOD ClearBuffers() INLINE ::ReadBuffers := {}, ;
      ::WriteBuffers := {}, ;
      ::DeleteBuffers := {}

   METHOD dbIsShared() INLINE ( ::Alias )->( dbInfo( DBI_SHARED ) )

   METHOD dbIsFLocked(  ) INLINE ( ::Alias )->( dbInfo( DBI_ISFLOCK ) )

   METHOD dbLockCount() INLINE ( ::Alias )->( dbInfo( DBI_LOCKCOUNT ) )

   METHOD dbInfo( n, x ) INLINE ( ::Alias )->( dbInfo( n, x ) )

   METHOD dbGetAlias() INLINE ( ::Alias )

   METHOD dbFullPath() INLINE ( ::Alias )->( dbInfo( DBI_FULLPATH ) )

   METHOD IsRLocked( n ) INLINE ( ::Alias )->( dbRecordInfo( DBRI_LOCKED, n ) )

   METHOD IsRUpdated( n ) INLINE ( ::Alias )->( dbRecordInfo( DBRI_UPDATED, n ) )

   METHOD dbRecordInfo( n, x ) INLINE ( ::Alias )->( dbRecordInfo( n,, x ) )

   METHOD dbOrderInfo( n, x, u ) INLINE ( ::Alias )->( dbOrderInfo( n, ::cOrderFile, x, u ) )

   METHOD OrderCount() INLINE ;
      ( ::Alias )->( dbOrderInfo( DBOI_ORDERCOUNT, ::cOrderFile ) )

   METHOD AutoOpen( l ) INLINE ;
      ( ::Alias )->( dbOrderInfo( DBOI_AUTOOPEN, ::cOrderFile,, l ) )

   METHOD AutoShare( l ) INLINE ;
      ( ::Alias )->( dbOrderInfo( DBOI_AUTOSHARE, ::cOrderFile,, l ) )

   METHOD Used() INLINE Select( ::Alias ) > 0

   METHOD ordSetFocus( ncTag ) INLINE ( ::Alias )->( ordSetFocus( ncTag ) )

   METHOD ordName( nOrder ) INLINE ;
      ( ::Alias )->( ordName( nOrder, ::cOrderBag ) ) ;

   METHOD ordNumber( cOrder ) INLINE ;
   ( ::Alias )->( ordNumber( cOrder, ::cOrderBag ) ) ;

   METHOD ordScope( n, u ) INLINE ( ::Alias )->( ordScope( n, u ) )

   METHOD ordIsUnique( nc ) INLINE ( ::Alias )->( ordIsUnique( nc, ;
      ::cOrderBag ) ) ;

   METHOD ordSkipUnique( n ) INLINE ( ::Alias )->( ordSkipUnique( n ) )
   METHOD ordSetRelation( n, b, c ) INLINE ( ::Alias )->( ordSetRelation( n, b, c ) )

   METHOD SetTopScope( xScope ) INLINE ;
      ( ::alias )->( ordScope( TOPSCOPE, xScope ) )
   METHOD SetBottomScope( xScope ) INLINE ;
      ( ::alias )->( ordScope( BOTTOMSCOPE, xScope ) )
   METHOD KillScope() INLINE ( ::alias )->( ordScope( TOPSCOPE, NIL ) ), ;
      ( ::alias )->( ordScope( BOTTOMSCOPE, NIL ) )

   METHOD New( cDBF, cALIAS, cOrderBag, cDRIVER, ;
      lNET, cPATH, lNEW, lREADONLY )

   METHOD OPEN()

   METHOD dbMove( nDirection )
   METHOD FldInit()
   METHOD READ( lKeepBuffer )
   METHOD ReadBLANK( lKeepBuffer )
   METHOD Write( lKeepBuffer )
   METHOD BufWrite( aBuffer )
   MESSAGE DELETE() METHOD __oTDelete( lKeepBuffer ) // reserved word - *HAS* to be renamed...
   METHOD SetMonitor( l )
   METHOD Undo( nBuffer, nLevel )

   METHOD dbSkip( n ) INLINE ( ::Alias )->( dbSkip( n ) ), ;
      ::nRecno := ( ::alias )->( RecNo() )

   METHOD dbGoto( n ) INLINE ( ::Alias )->( dbGoto( n ) )

   METHOD dbEval( a, b, c, d, e, f ) INLINE ( ::Alias )->( dbEval( a, b, c, d, e, f ) )
   METHOD dbSeek( a, b, c ) INLINE ( ::Alias )->( dbSeek( a, b, c ) )




   METHOD dbFilter() INLINE ( ::Alias )->( dbFilter() )
   METHOD SetFilter( c ) INLINE ;
      iif( c != NIL, ( ::Alias )->( dbSetFilter( hb_macroBlock( c ), c ) ), ;
      ( ::Alias )->( dbClearFilter() ) )

   METHOD AddChild( oChild, cKey )

   METHOD AddOrder( cTag, cKey, cLabel, ;
      cFor, cWhile, ;
      lUnique, ;
      bEval, nInterval, cOrderFile )
   METHOD GetOrderLabels()
   METHOD SetOrder( xTag )
   METHOD GetOrder( xOrder )
   METHOD FastReindex()
   METHOD REINDEX()
   METHOD CreateTable( cFile )
   METHOD AddField( f, t, l, d )
   METHOD Gentable()

   ERROR HANDLER OnError( uParam )

ENDCLASS

//---------------------
//  Constructor...
//---------------------

METHOD New( cDBF, cALIAS, cOrderBag, cDRIVER, ;
      lNET, cPATH, lNEW, lREADONLY ) CLASS HBTable

   LOCAL cOldRdd

   __defaultNIL( @lNET, .F. )
   __defaultNIL( @lNEW, .T. )
   __defaultNIL( @lREADONLY, .F. )
   __defaultNIL( @cDRIVER, "DBFCDX" )
   __defaultNIL( @cPATH, Set( _SET_DEFAULT ) )
   __defaultNIL( @cAlias, FixExt( cDbf ) )
   __defaultNIL( @cOrderBag, FixExt( cDbf ) )


   ::IsNew      := lNEW
   ::IsNet      := lNET
   ::IsReadOnly := lREADONLY
   ::cDBF       := cDBF
   ::cPath      := cPATH
   ::cOrderBag  := FixExt( cOrderBag )
   cOldRdd      := rddSetDefault( ::driver )

   ::cOrderFile := ::cOrderBag + ordBagExt()                //".cdx"
   rddSetDefault( cOldRdd )
   ::Driver      := cDRIVER
   ::aOrders     := {}
   ::Area        := 0
   ::Alias       := cALIAS
   ::nDataOffset := Len( self )         //66

   RETURN Self

METHOD OPEN() CLASS HBTable

   LOCAL lSuccess := .T.

   dbUseArea( ::IsNew, ::Driver, ::cDBF, ::Alias, ::IsNET, ::IsREADONLY )

   IF ::IsNET
      IF NetErr()
         Alert( _NET_USE_FAIL_MSG )
         lSuccess := .F.
         RETURN lSuccess
      ENDIF
   ENDIF

   SELECT( ::Alias )
   ::Area := Select()
   IF ::cOrderBag != NIL .AND. hb_FileExists( ::cPath + ::cOrderFile )

      SET INDEX TO ( ::cPath + ::cOrderBag )
      ( ::Alias )->( ordSetFocus( 1 ) )

   ENDIF

   ::Buffer := Array( ( ::Alias )->( FCount() ) )
   ::aStruc := ( ::Alias )->( dbStruct() )

   ::dbMove( _DB_TOP )

   RETURN lSuccess

METHOD PROCEDURE DBMove( nDirection ) CLASS HBTable

   __defaultNIL( @nDirection, 0 )

   DO CASE
   CASE nDirection == 0
      ( ::Alias )->( dbSkip( 0 ) )
   CASE nDirection == _DB_TOP
      ( ::Alias )->( dbGoTop() )
   CASE nDirection == _DB_BOTTOM
      ( ::Alias )->( dbGoBottom() )
   CASE nDirection == _DB_BOF
      ( ::Alias )->( dbGoTop() )
      ( ::Alias )->( dbSkip( -1 ) )
   CASE nDirection == _DB_EOF
      ( ::Alias )->( dbGoBottom() )
      ( ::Alias )->( dbSkip( 1 ) )
   OTHERWISE
      ( ::Alias )->( dbGoto( nDirection ) )
   ENDCASE

   RETURN

// -->
// -->
// --> Insert field definitions and generate virtual child class...
// -->
// -->

METHOD FldInit() CLASS HBTable

   LOCAL i
   LOCAL aDb
   LOCAL oNew
   LOCAL nScope    := 1

   ::nDataOffset := Len( self ) - 1

   ::Buffer := Array( ( ::Alias )->( FCount() ) )
   IF Empty( ::Buffer )
      ::Read()
   ENDIF

   // --> create new oObject class from this one...

   adb := HBClass():new( ::alias, { "hbtable" } )

   FOR i := 1 TO FCount()
      adb:AddData( ( ::Alias )->( FieldName( i ) ), , , nScope )
   NEXT

   aDB:create()

   oNew := adb:Instance()

   oNew:IsNew       := ::IsNew
   oNew:IsNet       := ::IsNet
   oNew:IsReadOnly  := ::IsReadOnly
   oNew:cDBF        := ::cDBF
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

   SELECT( oNew:Alias )

   oNew:Area := Select()

   oNew:Read()

   IF oNew:cOrderBag != NIL .AND. hb_FileExists( oNew:cPath + oNew:cOrderFile )
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

METHOD PROCEDURE READ( lKeepBuffer ) CLASS HBTable

   LOCAL i
   LOCAL nSel   := Select( ::Alias )
   LOCAL adata  := Array( 1, 2 )
   LOCAL Buffer

   __defaultNIL( @lKeepBuffer, .F. )

// ? Len( ::Buffer )

   FOR EACH Buffer in ::Buffer

      i      := Buffer:__EnumIndex()
      Buffer := ( ::Alias )->( FieldGet( i ) )

      adata[ 1, 1 ] := ( ::Alias )->( FieldName( i ) )
      adata[ 1, 2 ] := ( ::Alias )->( FieldGet( i ) )
      __objSetValueList( Self, aData )

   NEXT

   IF lKeepBuffer .OR. ::lMonitor
      AAdd( ::ReadBuffers, { ( ::Alias )->( RecNo() ), ::Buffer } )
   ENDIF

   SELECT( nSel )

   RETURN

METHOD PROCEDURE ReadBlank( lKeepBuffer ) CLASS HBTable

   LOCAL i
   LOCAL nSel   := Select( ::Alias )
   LOCAL nRec   := ( ::Alias )->( RecNo() )
   LOCAL adata  := Array( 1, 2 )
   LOCAL Buffer

   __defaultNIL( @lKeepBuffer, .F. )

   ( ::Alias )->( dbGoBottom() )
   ( ::Alias )->( dbSkip( 1 ) )         // go EOF

   FOR EACH Buffer in ::Buffer
      i      := Buffer:__EnumIndex()
      Buffer := ( ::Alias )->( FieldGet( i ) )

      adata[ 1, 1 ] := ( ::Alias )->( FieldName( i ) )
      adata[ 1, 2 ] := ( ::Alias )->( FieldGet( i ) )
      __objSetValueList( Self, aData )

   NEXT

   IF lKeepBuffer .OR. ::lMonitor
      AAdd( ::ReadBuffers, { ( ::Alias )->( RecNo() ), ::Buffer } )
   ENDIF

   ( ::Alias )->( dbGoto( nRec ) )
   SELECT( nSel )

   RETURN

METHOD Write( lKeepBuffer ) CLASS HBTable

   LOCAL i
   LOCAL aOldBuffer := Array( ( ::Alias )->( FCount() ) )
   LOCAL nSel       := Select( ::Alias )
   LOCAL nOrd       := ( ::Alias )->( ordSetFocus() )
   LOCAL aData      := __objGetValueLIST( Self )
   LOCAL xBuffer
   LOCAL n

   __defaultNIL( @lKeepBuffer, .F. )

   IF lKeepBuffer .OR. ::lMonitor

      // --> save old record in temp buffer
      FOR EACH xBuffer IN aOldBuffer
         xBuffer := ( ::Alias )->( FieldGet( xBuffer:__EnumIndex() ) )
      NEXT

      AAdd( ::WriteBuffers, { ( ::Alias )->( RecNo() ), aOldBuffer } )

   ENDIF

   IF ::isNet
      IF !( ::Alias )->( NetRecLock() )
         RETURN .F.
      ENDIF
   ENDIF

   ( ::Alias )->( ordSetFocus( 0 ) )

   FOR i := 1 TO ( ::Alias )->( FCount() )
      n := AScan( adata, {| a | a[ 1 ] == ( ::Alias )->( FieldName( i ) ) } )
      ( ::Alias )->( FieldPut( i, adata[ n, 2 ] ) )
   NEXT

   ( ::Alias )->( dbSkip( 0 ) )         // same as commit
   IF ::isNet
      ( ::Alias )->( dbRUnlock() )
   ENDIF
   ( ::Alias )->( ordSetFocus( nOrd ) )
   SELECT( nSel )

   RETURN .T.

METHOD BUFWrite( aBuffer ) CLASS HBTable

   LOCAL nSel       := Select( ::Alias )
   LOCAL nOrd       := ( ::Alias )->( ordSetFocus() )
   LOCAL Buffer

   __defaultNIL( @aBuffer, ::Buffer )

   IF ::isNet
      IF !( ::Alias )->( NetRecLock() )
         RETURN .F.
      ENDIF
   ENDIF

   ( ::Alias )->( ordSetFocus( 0 ) )

   FOR EACH Buffer in aBuffer
      ( ::Alias )->( FieldPut( Buffer:__EnumIndex(), Buffer ) )
   NEXT

   ( ::Alias )->( dbSkip( 0 ) )
   IF ::isNet
      ( ::Alias )->( dbRUnlock() )
   ENDIF
   ( ::Alias )->( ordSetFocus( nOrd ) )
   SELECT( nSel )

   RETURN .T.

METHOD __oTDelete( lKeepBuffer )        // ::Delete()

   LOCAL lRet
   LOCAL lDeleted := Set( _SET_DELETED, .F. )                  // make deleted records visible

   // temporarily...
   __defaultNIL( @lKeepBuffer, .F. )

   ::Read()

   IF ::isNet
      lRet := ( ::Alias )->( NetDelete() )
   ELSE
      ( ::alias )->( dbDelete() )
      lRet := .T.
   ENDIF

   IF ( lKeepBuffer .OR. ::lMonitor ) .AND. lRet
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

//
// Transaction control subsystem...
//

METHOD Undo( nBuffer, nLevel ) CLASS HBTable

   LOCAL nLen
   LOCAL lRet      := .F.
   LOCAL lDelState := Set( _SET_DELETED )
   LOCAL nRec      := ::RecNo()
   LOCAL aBuffers

   __defaultNIL( @nBuffer, _WRITE_BUFFER )

   IF nLevel == NIL
      nLevel := 0
   ENDIF

   SWITCH nBuffer

   CASE _DELETE_BUFFER

      IF ! Empty( ::DeleteBuffers )

         Set( _SET_DELETED, .F. )       // make deleted records visible temporarily...

         nLen := Len( ::deleteBuffers )

         __defaultNIL( @nLevel, nLen )

         IF nLevel == 0 .OR. nLevel == nLen     // DO ALL...

            FOR EACH aBuffers IN ::deleteBuffers

               ( ::Alias )->( dbGoto( aBuffers[ 1 ] ) )

               lRet := ( ::Alias )->( NetRecall() )

            NEXT

            IF lRet
               ::deleteBuffers := {}
            ENDIF

         ELSE       // DO CONTROLLED...

            FOR EACH aBuffers IN ::deleteBuffers
               IF aBuffers:__EnumIndex() > ( nLen - nLevel )

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
         __defaultNIL( @nLevel, nLen )

         IF nLevel == 0 .OR. nLen == nLevel   // Do All...

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
               // erase entries
               ::WriteBuffers := {}
            ENDIF

         ELSE       // do controlled...

            FOR EACH aBuffers IN ::writeBuffers
               IF aBuffers:__EnumIndex() > ( nLen - nLevel )

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

   OTHERWISE

   ENDSWITCH

   ( ::Alias )->( dbUnlock() )
   ( ::Alias )->( dbGoto( nRec ) )
   ::Read()

   RETURN lRet

//
//   ORDER MANAGEMENT
//

METHOD AddOrder( cTag, cKey, cLabel, ;
      cFor, cWhile, ;
      lUnique, ;
      bEval, nInterval, cOrderFile ) CLASS HBTable

   LOCAL oOrd

   __defaultNIL( @cOrderFile, ::cOrderBag )

   oOrd := HBOrder():New( cTag, cKey, cLabel, ;
      cFor, cWhile, ;
      lUnique, ;
      bEval, nInterval )

   oOrd:oTable    := Self
   oOrd:cOrderBag := ::cOrderBag

   AAdd( ::aOrders, oOrd )

   RETURN oOrd

METHOD REINDEX() CLASS HBTable

   LOCAL nSel := Select( ::Alias )
   LOCAL nOrd := ( ::Alias )->( ordSetFocus( 0 ) )

   IF Len( ::aOrders ) > 0

      IF ::Used()
         ::Kill()
      ENDIF

      ::Isnet := .F.

      IF hb_FileExists( ::cPath + ::cOrderFile )
         IF FErase( ::cPath + ::cOrderFile ) != 0
            // --> ALERT(".cdx *NOT* Deleted !!!" )
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
   SELECT( nSel )

   RETURN .T.

METHOD FastReindex() CLASS HBTable

   LOCAL nSel := Select( ::Alias )
   LOCAL nOrd := ( ::Alias )->( ordSetFocus( 0 ) )

   IF Len( ::aOrders ) > 0

      ::Kill()

      ::Isnet := .F.
      IF hb_FileExists( ::cPath + ::cOrderFile )
         IF FErase( ::cPath + ::cOrderFile ) != 0
            // --> ALERT(".cdx *NOT* Deleted !!!" )
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
   SELECT( nSel )

   RETURN .T.

METHOD GetOrder( xOrder ) CLASS HBTable

   LOCAL nPos
   LOCAL xType := ValType( xOrder )

   IF xType == "C"
      nPos := AScan( ::aOrders, {| e | e:Tag == xOrder } )
   ELSEIF xType == "N" .AND. xOrder > 0
      nPos := xOrder
   ELSE
      nPos := 0
   ENDIF

   IF nPos == 0
      nPos := 1
   ENDIF

   RETURN ::aOrders[ nPos ]                // returns oOrder

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

//
// Relation Methods
//

PROCEDURE AddChild( oChild, cKey ) CLASS HBTable                 // ::addChild()

   AAdd( ::aChildren, { oChild, cKey } )
   oChild:oParent := Self
   ( ::Alias )->( ordSetRelation( oChild:Alias, hb_macroBlock( cKey ), cKey ) )

   RETURN

/****
*  FixExt( cFileName )
*  extract .cdx filename from .dbf filename
*/

STATIC FUNCTION FixExt( cFileName )

   LOCAL nLeft := At( ".", cFilename )

   RETURN Left( cFileName, iif( nLeft == 0, ;
      Len( cFilename ), ;
      nLeft - 1 ) )

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
   LOCAL uRet, oErr

   IF uParam != NIL .AND. Left( cMsg, 1 ) == '_'
      cMsg := SubStr( cMsg, 2 )
   ENDIF
   nPos := ( ::Alias )->( FieldPos( cMsg ) )

   IF nPos != 0
      uRet := ( ::Alias )->( iif( uParam == NIL, FieldGet(nPos ), FieldPut(nPos, uParam ) ) )
   ELSE

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
      uRet := Eval( ErrorBlock(), oErr )

   ENDIF

   RETURN uRet

CREATE CLASS HBOrder

   VAR oTable
   VAR cOrderBag
   VAR Label, TAG
   VAR cKey, bKey
   VAR cFor, bFor
   VAR cWhile, bWhile
   VAR UNIQUE INIT .F.
   VAR bEval
   VAR nInterval
   METHOD Alias() INLINE ::oTable:Alias

   METHOD New( cTag, cKey, cLabel, cFor, cWhile, lUnique, bEval, nInterval, cOrderBag )
   METHOD CREATE()

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

   __defaultNIL( @cKey, ".T." )
   __defaultNIL( @lUnique, .F. )
   __defaultNIL( @cFor, ".T." )
   __defaultNIL( @cWhile, ".T." )
   __defaultNIL( @bEval, {|| .T. } )
   __defaultNIL( @nInterval, 1 )
   __defaultNIL( @cLabel, cTag )

   ::cOrderBag := cOrderBag
   ::Tag       := cTag
   ::cKey      := cKey
   ::cFor      := cFor
   ::cWhile    := cWhile
   ::bKey      := hb_macroBlock( cKey )
   ::bFor      := hb_macroBlock( cFor )
   ::bWhile    := hb_macroBlock( cWhile )
   ::bEval     := bEval
   ::nInterval := nInterval
   ::Label     := cLabel

   RETURN Self

METHOD PROCEDURE CREATE() CLASS HBOrder

   __defaultNIL( @::cOrderBag, ::oTable:cOrderBag )

// ? "<<<", ::alias, ::cOrderBag

   ( ::alias )->( ordCondSet( ::cFor, ::bFor, ;
      .T., ;
      ::bWhile, ;
      ::bEval, ::nInterval ) )

   ( ::alias )->( ordCreate( ::cOrderBag, ::Tag, ::cKey, ;
      ::bKey, ::Unique ) )

   RETURN
