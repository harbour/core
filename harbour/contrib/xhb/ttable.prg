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
#include "common.ch"
#include "inkey.ch"
#include "dbinfo.ch"
#include "error.ch"

STATIC saTables := {}
/* NetWork Functions */
STATIC snNetDelay    := 30
STATIC slNetOk       := .F.
STATIC scNetMsgColor := "GR+/R"

FUNCTION NetDbUse( cDataBase, cAlias, nSeconds, cDriver, ;
                      lNew, lOpenMode, lReadOnly )
   LOCAL nKey
   LOCAL lForever
   LOCAL cOldScreen := SAVESCREEN( MAXROW(), 0, MAXROW(), MAXCOL() + 1 )
   LOCAL lFirstPass := .T.

   DEFAULT cDriver TO "DBFCDX"
   DEFAULT lNew TO .T.
   DEFAULT lOpenMode TO NET_OPEN_MODE
   DEFAULT lReadOnly TO .F.
   DEFAULT nSeconds TO snNetDelay

   slNetOk  := .F.
   nSeconds *= 1.00
   lforever := ( nSeconds == 0 )

   KEYBOARD CHR( 255 )
   INKEY()

   DO WHILE ( lforever .or. nSeconds > 0 ) .and. LASTKEY() != K_ESC
      IF !lfirstPass
         DISPOUTAT( MAXROW(), 0, ;
                    PADC( "Network retry ³ " + ;
                    LTRIM( STR( nSeconds, 4, 1 ) ) + " ³ ESCape = Exit ", ;
                    MAXCOL() + 1 ), ;
                    scNetMsgColor )
         lFirstPass := .F.
      ENDIF

      DBUSEAREA( lNew, ;
                 ( cDriver ), ( cDatabase ), ( cAlias ), ;
                 lOpenMode, ;
                 .F. )

      IF !NETERR()  // USE SUCCEEDS
         RESTSCREEN( MAXROW(), 0, MAXROW(), MAXCOL() + 1, cOldScreen )
         slNetOk := .T.
      ELSE
         lFirstPass := .F.
      ENDIF

      IF !slNetOK
         nKey     := INKEY( .5 )        // WAIT 1 SECOND
         nSeconds -= .5
      ELSE
         RESTSCREEN( MAXROW(), 0, MAXROW(), MAXCOL() + 1, cOldScreen )
         EXIT
      ENDIF

      IF nKey == K_ESC
         RESTSCREEN( MAXROW(), 0, MAXROW(), MAXCOL() + 1, cOldScreen )
         EXIT
      ENDIF

   ENDDO

   RESTSCREEN( MAXROW(), 0, MAXROW(), MAXCOL() + 1, cOldScreen )

RETURN slNetOk


FUNCTION NetLock( nType, lReleaseLocks, nSeconds )

   LOCAL cSave       := SAVESCREEN( MAXROW(), 0, MAXROW(), MAXCOL() + 1 )
   LOCAL lContinue   := .T.
   LOCAL lSuccess    := .F.
   LOCAL nWaitTime
   LOCAL bOperation
   LOCAL xIdentifier
   LOCAL nKey
   LOCAL nCh
   LOCAL cWord

   IF ! HB_ISNUMERIC( nType ) .or. ;
        ( ( nType != 1 ) .and. ;
          ( nType != 2 ) .and. ;
          ( nType != 3 ) )
      ALERT( "Invalid Argument passed to NETLOCK()" )
      RETURN lSuccess
   ENDIF

   DEFAULT lReleaseLocks TO .F.
   DEFAULT nSeconds TO snNetDelay

   nWaitTime := nSeconds

   SWITCH nType
   CASE NET_RECLOCK                        // 1 = Record Lock...
      xIdentifier := IF( lReleaseLocks, NIL, RECNO() )
      bOperation  := { | x | DBRLOCK( x ) }
      exit
   CASE NET_FILELOCK                       // 2 = File Lock...
      bOperation := { || FLOCK() }
      exit
   CASE NET_APPEND                         // 3 = Append Blank...
      xIdentifier := lReleaseLocks
      bOperation  := { | x | DBAPPEND( x ), !NETERR() }
      exit
   ENDSWITCH

   slNetOk := .F.

   WHILE lContinue == .T.

   /*
   IF (nKey := INKEY()) == K_ESC
      RestScreen( maxrow(),0,maxrow(),maxcol()+1, cSave)
      EXIT
   ENDIF
   */

      WHILE nSeconds > 0 .and. lContinue == .T.
         IF EVAL( bOperation, xIdentifier )
            nSeconds  := 0
            lSuccess  := .T.
            lContinue := .F.
            slNetOK   := .T.
            EXIT
         ELSE
            IF nType == 1
               cWord := "( " + DBINFO( 33 ) + " - Record Lock )"
            ELSEIF nType == 2
               cWord := "( " + DBINFO( 33 ) + " - File Lock )"
            ELSEIF nType == 3
               cWord := "( " + DBINFO( 33 ) + " - File Append )"
            ELSE
               cWord := "( " + DBINFO( 33 ) + " -  ??? "
            ENDIF

            DISPOUTAT( MAXROW(), 0, ;
                       PADC( "Network Retry " + cWord + " ³ " + STR( nSeconds, 3 ) + " ³ ESC Exit", MAXCOL() + 1 ), ;
                       scNetMsgColor )

            nKey := INKEY( 1 )          //TONE( 1,1 )
            nSeconds --                 //.5
            IF nKey == K_ESC
               RESTSCREEN( MAXROW(), 0, MAXROW(), MAXCOL() + 1, cSave )
               EXIT
            ENDIF
         ENDIF
      ENDDO

      IF LASTKEY() == K_ESC
         RESTSCREEN( MAXROW(), 0, MAXROW(), MAXCOL() + 1, cSave )
         EXIT
      ENDIF

      IF !lSuccess
         nSeconds := nWaitTime
         nCh      := ALERT( RETRY_MSG, { "  YES  ", "  NO  " } )

         IF nCh == 1
            lContinue := .T.
         ELSE
            lContinue := .F.
         ENDIF

         IF lContinue == .F.
            //EXIT
            RESTSCREEN( MAXROW(), 0, MAXROW(), MAXCOL() + 1, cSave )
            RETURN lSuccess
         ENDIF

      ENDIF
   ENDDO

   RESTSCREEN( MAXROW(), 0, MAXROW(), MAXCOL() + 1, cSave )

RETURN lSuccess


FUNCTION NetFunc( bBlock, nSeconds )

   LOCAL lForever      // Retry forever?

   DEFAULT nSeconds TO snNetDelay
   lForever := ( nSeconds == 0 )

   // Keep trying as long as specified or default
   DO WHILE ( lForever .or. ( nSeconds > 0 ) )

      IF EVAL( bBlock )
         RETURN .T.                 // NOTE
      ENDIF

      INKEY( 1 )    // Wait 0.5 seconds
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

      IF !hb_FileExists( xFile[ 1 ] )
         nRet := - 1
         EXIT
      ENDIF

      IF NetDbUse( xFile[ 1 ], xFile[ 2 ], snNetDelay, "DBFCDX" )
         IF HB_ISARRAY( xFile[ 3 ] )
            FOR EACH cIndex IN xFile[ 3 ]
               IF hb_FileExists( cIndex )
                  ORDLISTADD( cIndex )
               ELSE
                  nRet := - 3
                  EXIT
               ENDIF
            NEXT
         ENDIF
      ELSE
         nRet := - 2
         EXIT
      ENDIF
   NEXT

   RETURN nRet


/* NETWORK METHODS */

FUNCTION NetDelete()

   slNetOK := .F.

   IF NetLock( NET_RECLOCK ) == .T.
      DBDELETE()
      slNetOK := .T.
   ENDIF

   IF !NETERR()
      DBSKIP( 0 )
      DBCOMMIT()
   ELSE
      slNetOK := .T.
      ALERT( " Failed to DELETE Record -> " + STR( RECNO() ) )
   ENDIF
RETURN slNetOk


FUNCTION NetReCall()

   slNetOk := .F.

   IF NetLock( NET_RECLOCK ) == .T.
      DBRECALL()
      slNetOk := .T.
   ENDIF

   IF !NETERR()
      DBSKIP( 0 )
      DBCOMMIT()
   ELSE
      slNetOK := .T.
      ALERT( " Failed to RECALL Record -> " + STR( RECNO() ) )
   ENDIF

RETURN slNetOk


FUNCTION NetRecLock( nSeconds )

   DEFAULT nSeconds TO snNetDelay

   slNetOK := .F.

   IF NetLock( NET_RECLOCK,, nSeconds )                     // 1
      slNetOK := .T.
   ENDIF

RETURN slNetOK


FUNCTION NetFileLock( nSeconds )

   slNetOK := .F.
   DEFAULT nSeconds TO snNetDelay

   IF NetLock( NET_FILELOCK,, nSeconds )
      slNetOK := .T.
   ENDIF

RETURN slNetOK


FUNCTION NetAppend( nSeconds, lReleaseLocks )

   LOCAL nOrd
   DEFAULT lReleaseLocks TO .T.
   DEFAULT nSeconds TO snNetDelay
   slNetOK := .F.
   nOrd    := ORDSETFOCUS( 0 )          // --> set order to 0 to append ???

   IF NetLock( NET_APPEND,, nSeconds )
      //DbGoBottom()
      slNetOK := .T.
   ENDIF

   ORDSETFOCUS( nOrd )

RETURN slNetOK


PROCEDURE NetFlush()

   DBCOMMITALL()
   DBUNLOCKALL()
   DBSKIP( 0 )
RETURN


FUNCTION NetCommitAll()

   LOCAL n

   FOR n := 1 TO MAX_TABLE_AREAS
      IF !EMPTY( ALIAS( n ) )
         ( ALIAS( n ) )->( DBCOMMIT(), DBUNLOCK() )
      ENDIF
   NEXT

RETURN n


FUNCTION IsLocked( nRecId )
DEFAULT nRecID TO recno()

RETURN ASCAN( DBRLOCKLIST(), { | n | n == nRecID } ) > 0


FUNCTION NetError()
RETURN !slNetOK


FUNCTION SetNetDelay( nSecs )

   LOCAL nTemp := snNetDelay
   IF nSecs != NIL
      snNetDelay := nSecs
   ENDIF
RETURN nTemp


FUNCTION SetNetMsgColor( cColor )

   LOCAL cTemp := scNetMsgColor
   IF cColor != NIL
      scNetmsgColor := cColor
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
   DEFAULT lNET TO .T.
   DEFAULT lNEW TO .T.
   DEFAULT lREADONLY TO .F.
   DEFAULT cDRIVER TO "DBFCDX"
   DEFAULT cPATH TO SET( _SET_DEFAULT )
   DEFAULT cAlias TO FixExt( cDbf )
   DEFAULT cOrderBag TO FixExt( cDbf )  //+".CDX"

   lAuto := SET( _SET_AUTOPEN, .F. )

   IF ( nPos := ASCAN( saTables, { | e | e[ 1 ] == UPPER( cALIAS ) } ) ) > 0

      oDB := saTables[ nPos, 2 ]

   ELSE
      o := HBTable():New( cDBF, cALIAS, cOrderBag, cDRIVER, ;
                          lNET, cPATH, lNEW, lREADONLY )
      IF o:Open()
         oDB := o:FldInit()
      ENDIF

      AADD( saTables, { UPPER( cAlias ), oDB } )

   ENDIF

   SET( _SET_AUTOPEN, lAuto )

RETURN oDB


FUNCTION GetTable( cAlias )

   LOCAL nPos
   LOCAL oDB
   IF ( nPos := ASCAN( saTables, { | e | e[ 1 ] == UPPER( cALIAS ) } ) ) > 0
      oDB := saTables[ nPos, 2 ]
   ENDIF
RETURN oDB


/****
*
*     CLASS HBField()
*
*
*
*/

CLASS HBField

   DATA Alias INIT ALIAS()
   DATA Name INIT ""
   DATA Type INIT "C"
   DATA Len INIT 0
   DATA Dec INIT 0
   DATA order INIT 0
   DATA Value

   METHOD GET() INLINE ::value := ( ::alias )->( FIELDGET( ::order ) )
   METHOD Put( x ) INLINE ::value := x ,;
          ( ::alias )->( FIELDPUT( ::order, x ) )

ENDCLASS

/****
*
*     CLASS HBRecord()
*
*
*
*/

CLASS HBRecord

   DATA Buffer INIT {}
   DATA Alias INIT ALIAS()
   DATA Number INIT 0
   DATA aFields INIT {}

   METHOD New( cAlias )
   METHOD GET()
   METHOD Put()

ENDCLASS


METHOD NEW( cAlias ) CLASS HBRecord

   LOCAL i
   LOCAL oFld
   LOCAL aStruc
   LOCAL aItem

   DEFAULT cAlias TO ALIAS()

   ::Alias   := cAlias
   ::Buffer  := {}
   ::aFields := ARRAY( ( ::alias )->( FCOUNT() ) )

   aStruc := ( ::alias )->( DBSTRUCT() )

   FOR EACH aItem in ::aFields
      i          := aItem:__EnumIndex()
      oFld       := HBField()
      oFld:order := i
      oFld:Name  := ( ::alias )->( FIELDNAME( i ) )
      oFld:Type  := aStruc[ i, 2 ]
      oFld:LEN   := aStruc[ i, 3 ]
      oFld:Dec   := aStruc[ i, 4 ]
      oFld:Alias := ::alias
      aItem      := oFld
   NEXT

RETURN Self


METHOD PROCEDURE GET() CLASS HBRecord

   LOCAL xField

   FOR EACH xField IN ::aFields
      xField:GET()
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
*
*
*/

   //METHOD SetFocus()    INLINE (::Alias)->(Select( ::Area ))
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
CLASS HBTable

   DATA Buffer INIT {}                  // 1
   DATA Alias INIT ALIAS()              // 2
   DATA Area INIT 0 // 3

   DATA oRec
   DATA aStruc INIT {}
   DATA nRecno INIT 0
   DATA cDBF INIT ""
   DATA cOrderBag INIT ""
   DATA cOrderFile INIT ""
   DATA cPATH INIT ""
   DATA Driver INIT "DBFCDX"
   DATA IsNew INIT .T.
   DATA IsReadOnly INIT .F.
   DATA IsNet INIT .T.
   DATA aSaveState INIT {}
   DATA lMonitor INIT .F.
   DATA ReadBuffers INIT {}
   DATA WriteBuffers INIT {}
   DATA DeleteBuffers INIT {}
   DATA nDataOffset INIT 0
   DATA BlankBuffer INIT {}
   DATA aOrders INIT {}
   DATA aChildren INIT {}
   DATA oParent

   METHOD EOF() INLINE ( ::Alias )->( EOF() )
   METHOD BOF() INLINE ( ::Alias )->( BOF() )
   METHOD RECNO() INLINE ( ::Alias )->( RECNO() )
   METHOD LASTREC() INLINE ( ::Alias )->( LASTREC() )
   METHOD SKIP( n ) INLINE ( ::Alias )->( DBSKIP( n ) ),;
   ::nRecno := ( ::Alias )->( RECNO() )

   METHOD GOTO( n ) INLINE ( ::Alias )->( DBGOTO( n ) )
   METHOD goTop() INLINE ( ::Alias )->( DBGOTOP() )
   METHOD goBottom() INLINE ( ::Alias )->( DBGOBOTTOM() )
   METHOD SetFocus() INLINE ( ::Alias )->( SELECT( ::ALias ) )
   METHOD Append( l ) INLINE IF( ::isNet, ( ::Alias )->( NetAppend( l ) ), ;
   ( ::alias )->( DBAPPEND() ) )
   METHOD RECALL(  ) INLINE ( ::Alias )->( NetRecall(  ) )

   METHOD LOCATE( bFor, bWhile, nNext, nRec, lRest ) INLINE ;
   ( ::Alias )->( __dbLocate( bFor, bWhile, ;
   nNext, nRec, lRest ) )
   METHOD CONTINUE() INLINE ( ::Alias )->( __dbContinue() )
   METHOD FOUND() INLINE ( ::Alias )->( FOUND() )
   METHOD Kill() INLINE ( ::Alias )->( DBCOMMIT() ),;
          ( ::Alias )->( DBUNLOCK() ) ,;
          ( ::Alias )->( DBCLOSEAREA() ),;
          ::ClearBuffers()
   METHOD ClearBuffers() INLINE ::ReadBuffers := {},;
         ::WriteBuffers := {},;
         ::DeleteBuffers := {}

   METHOD dbIsShared() INLINE ( ::Alias )->( DBINFO( DBI_SHARED ) )

   METHOD dbIsFLocked(  ) INLINE ( ::Alias )->( DBINFO( DBI_ISFLOCK ) )

   METHOD dbLockCount() INLINE ( ::Alias )->( DBINFO( DBI_LOCKCOUNT ) )

   METHOD DBINFO( n, x ) INLINE ( ::Alias )->( DBINFO( n, x ) )

   METHOD dbGetAlias() INLINE ( ::Alias )

   METHOD dbFullPath() INLINE ( ::Alias )->( DBINFO( DBI_FULLPATH ) )

   METHOD IsRLocked( n ) INLINE ( ::Alias )->( DBRECORDINFO( DBRI_LOCKED, n ) )

   METHOD IsRUpdated( n ) INLINE ( ::Alias )->( DBRECORDINFO( DBRI_UPDATED, n ) )

   METHOD DBRECORDINFO( n, x ) INLINE ( ::Alias )->( DBRECORDINFO( n,, x ) )

   METHOD DBORDERINFO( n, x, u ) INLINE ( ::Alias )->( DBORDERINFO( n, ::cOrderFile, x, u ) )

   METHOD OrderCount() INLINE ;
   ( ::Alias )->( DBORDERINFO( DBOI_ORDERCOUNT, ::cOrderFile ) )

   METHOD AutoOpen( l ) INLINE ;
   ( ::Alias )->( DBORDERINFO( DBOI_AUTOOPEN, ::cOrderFile,, l ) )

   METHOD AutoShare( l ) INLINE ;
   ( ::Alias )->( DBORDERINFO( DBOI_AUTOSHARE, ::cOrderFile,, l ) )

   METHOD USED() INLINE SELECT( ::Alias ) > 0

   METHOD ORDSETFOCUS( ncTag ) INLINE ( ::Alias )->( ORDSETFOCUS( ncTag ) )
   METHOD ORDNAME( nOrder ) INLINE ;
   ( ::Alias )->( ORDNAME( nOrder, ::cOrderBag ) ) ;

   METHOD ORDNUMBER( cOrder ) INLINE ;
   ( ::Alias )->( ORDNUMBER( cOrder, ::cOrderBag ) ) ;

   METHOD ORDSCOPE( n, u ) INLINE ( ::Alias )->( ORDSCOPE( n, u ) )

   METHOD ORDISUNIQUE( nc ) INLINE ( ::Alias )->( ORDISUNIQUE( nc, ;
   ::cOrderBag ) ) ;

   METHOD ORDSKIPUNIQUE( n ) INLINE ( ::Alias )->( ORDSKIPUNIQUE( n ) )
   METHOD ORDSETRELATION( n, b, c ) INLINE ( ::Alias )->( ORDSETRELATION( n, b, c ) )

   METHOD SetTopScope( xScope ) INLINE ;
   ( ::alias )->( ORDSCOPE( TOPSCOPE, xScope ) )
   METHOD SetBottomScope( xScope ) INLINE ;
   ( ::alias )->( ORDSCOPE( BOTTOMSCOPE, xScope ) )
   METHOD KillScope() INLINE ( ::alias )->( ORDSCOPE( TOPSCOPE, NIL ) )  ,;
          ( ::alias )->( ORDSCOPE( BOTTOMSCOPE, NIL ) )

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

   METHOD DBSKIP( n ) INLINE ( ::Alias )->( DBSKIP( n ) ),;
          ::nRecno := ( ::alias )->( RECNO() )

   METHOD DBGOTO( n ) INLINE ( ::Alias )->( DBGOTO( n ) )

   METHOD DBEVAL( a, b, c, d, e, f ) INLINE ( ::Alias )->( DBEVAL( a, b, c, d, e, f ) )
   METHOD DBSEEK( a, b, c ) INLINE ( ::Alias )->( DBSEEK( a, b, c ) )




   METHOD DBFILTER() INLINE ( ::Alias )->( DBFILTER() )
   METHOD SetFilter( c ) INLINE ;
   IF( c != NIL, ( ::Alias )->( DBSETFILTER( hb_macroBlock( c ), c ) ), ;
   ( ::Alias )->( DBCLEARFILTER() ) )

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
   Local cOldRdd
   DEFAULT lNET TO .F.
   DEFAULT lNEW TO .T.
   DEFAULT lREADONLY TO .F.
   DEFAULT cDRIVER TO "DBFCDX"
   DEFAULT cPATH TO SET( _SET_DEFAULT )
   DEFAULT cAlias TO FixExt( cDbf )
   DEFAULT cOrderBag TO FixExt( cDbf )  //+".CDX"


   ::IsNew      := lNEW
   ::IsNet      := lNET
   ::IsReadOnly := lREADONLY
   ::cDBF       := cDBF
   ::cPath      := cPATH
   ::cOrderBag  := FixExt( cOrderBag )
   cOldRdd      := rddsetdefault( ::driver )

   ::cOrderFile := ::cOrderBag + ORDBAGEXT()                //".CDX"
   rddsetdefault( cOldRdd )
   ::Driver      := cDRIVER
   ::aOrders     := {}
   ::Area        := 0
   ::Alias       := cALIAS
   ::nDataOffset := LEN( self )         //66

RETURN Self


METHOD OPEN() CLASS HBTable

   LOCAL lSuccess := .T.

   DBUSEAREA( ::IsNew, ::Driver, ::cDBF, ::Alias, ::IsNET, ::IsREADONLY )

   IF ::IsNET == .T.
      IF NETERR()
         ALERT( _NET_USE_FAIL_MSG )
         lSuccess := .F.
         RETURN lSuccess
      ENDIF
   ENDIF

   SELECT( ::Alias )
   ::Area := SELECT()
   IF ::cOrderBag != NIL .and. hb_FileExists( ::cPath + ::cOrderFile )

      SET INDEX TO ( ::cPath + ::cOrderBag )
      ( ::Alias )->( ORDSETFOCUS( 1 ) )

   ENDIF

   ::Buffer := ARRAY( ( ::Alias )->( FCOUNT() ) )
   ::aStruc := ( ::Alias )->( DBSTRUCT() )

   ::dbMove( _DB_TOP )

RETURN lSuccess


METHOD PROCEDURE DBMove( nDirection ) CLASS HBTable

   DEFAULT nDirection TO 0

   DO CASE
   CASE nDirection == 0
      ( ::Alias )->( DBSKIP( 0 ) )
   CASE nDirection == _DB_TOP
      ( ::Alias )->( DBGOTOP() )
   CASE nDirection == _DB_BOTTOM
      ( ::Alias )->( DBGOBOTTOM() )
   CASE nDirection == _DB_BOF
      ( ::Alias )->( DBGOTOP() )
      ( ::Alias )->( DBSKIP( - 1 ) )
   CASE nDirection == _DB_EOF
      ( ::Alias )->( DBGOBOTTOM() )
      ( ::Alias )->( DBSKIP( 1 ) )
   OTHERWISE
      ( ::Alias )->( DBGOTO( nDirection ) )
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

   ::nDataOffset := LEN( self ) - 1

   ::Buffer := ARRAY( ( ::Alias )->( FCOUNT() ) )
   IF EMPTY( ::Buffer )
      ::Read()
   ENDIF

   // --> create new oObject class from this one...

   adb := hbclass():new( ::alias, { "hbtable" } )

   FOR i := 1 TO FCOUNT()
      adb:AddData( ( ::Alias )->( FIELDNAME( i ) ),,, nScope )
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

   oNew:Area := SELECT()

   oNew:Read()

   IF oNew:cOrderBag != NIL .and. hb_FileExists( oNew:cPath + oNew:cOrderFile )
      SET INDEX TO ( oNew:cPath + oNew:cOrderBag )
      ( oNew:Alias )->( ORDSETFOCUS( 1 ) )
   ENDIF

   oNew:buffer := ARRAY( ( oNew:alias )->( FCOUNT() ) )
   oNew:aStruc := ( oNew:alias )->( DBSTRUCT() )

   IF oNew:Used()
      oNew:dbMove( _DB_TOP )
      oNew:Read()
   ENDIF

RETURN oNew


METHOD PROCEDURE READ( lKeepBuffer ) CLASS HBTable

   LOCAL i
   LOCAL nSel   := SELECT( ::Alias )
   LOCAL adata  := ARRAY( 1, 2 )
   LOCAL Buffer
   DEFAULT lKeepBuffer TO .F.

   //? len( ::Buffer )

   FOR Each Buffer in ::Buffer

      i      := Buffer:__EnumIndex()
      Buffer := ( ::Alias )->( FIELDGET( i ) )

      adata[ 1, 1 ] := ( ::Alias )->( FIELDNAME( i ) )
      adata[ 1, 2 ] := ( ::Alias )->( FIELDGET( i ) )
      __ObjSetValueList( Self, aData )

   NEXT

   IF ( lKeepBuffer == .T. ) .or. ( ::lMonitor == .T. )
      AADD( ::ReadBuffers, { ( ::Alias )->( RECNO() ), ::Buffer } )
   ENDIF

   SELECT( nSel )

RETURN


METHOD PROCEDURE ReadBlank( lKeepBuffer ) CLASS HBTable

   LOCAL i
   LOCAL nSel   := SELECT( ::Alias )
   LOCAL nRec   := ( ::Alias )->( RECNO() )
   LOCAL adata  := ARRAY( 1, 2 )
   LOCAL Buffer
   DEFAULT lKeepBuffer TO .F.

   ( ::Alias )->( DBGOBOTTOM() )
   ( ::Alias )->( DBSKIP( 1 ) )         // go EOF

   FOR each Buffer in ::Buffer
      i      := Buffer:__EnumIndex()
      Buffer := ( ::Alias )->( FIELDGET( i ) )

      adata[ 1, 1 ] := ( ::Alias )->( FIELDNAME( i ) )
      adata[ 1, 2 ] := ( ::Alias )->( FIELDGET( i ) )
      __ObjSetValueList( Self, aData )

   NEXT

   IF ( lKeepBuffer == .T. ) .or. ( ::lMonitor == .T. )
      AADD( ::ReadBuffers, { ( ::Alias )->( RECNO() ), ::Buffer } )
   ENDIF

   ( ::Alias )->( DBGOTO( nRec ) )
   SELECT( nSel )

RETURN


METHOD Write( lKeepBuffer ) CLASS HBTable

   LOCAL i
   LOCAL aOldBuffer := ARRAY( ( ::Alias )->( FCOUNT() ) )
   LOCAL nSel       := SELECT( ::Alias )
   LOCAL nOrd       := ( ::Alias )->( ORDSETFOCUS() )
   LOCAL aData      := __objGetValueList( Self )
   LOCAL xBuffer
   LOCAL n

   DEFAULT lKeepBuffer TO .F.

   IF ( lKeepBuffer == .T. ) .or. ( ::lMonitor == .T. )

      // --> save old record in temp buffer
      FOR EACH xBuffer IN aOldBuffer
         xBuffer := ( ::Alias )->( FIELDGET( xBuffer:__EnumIndex() ) )
      NEXT

      AADD( ::WriteBuffers, { ( ::Alias )->( RECNO() ), aOldBuffer } )

   ENDIF

   IF ::isNet
      IF !( ::Alias )->( NetRecLock() )
         RETURN .F.
      ENDIF
   ENDIF

   ( ::Alias )->( ORDSETFOCUS( 0 ) )

   FOR i := 1 TO ( ::Alias )->( FCOUNT() )
      n := ASCAN( adata, { | a | a[ 1 ] == ( ::Alias )->( FIELDNAME( i ) ) } )
      ( ::Alias )->( FIELDPUT( i, adata[ n, 2 ] ) )
   NEXT

   ( ::Alias )->( DBSKIP( 0 ) )         // same as commit
   IF ::isNet
      ( ::Alias )->( DBRUNLOCK() )
   ENDIF
   ( ::Alias )->( ORDSETFOCUS( nOrd ) )
   SELECT( nSel )

RETURN .T.


METHOD BUFWrite( aBuffer ) CLASS HBTable

   LOCAL nSel       := SELECT( ::Alias )
   LOCAL nOrd       := ( ::Alias )->( ORDSETFOCUS() )
   LOCAL Buffer
   DEFAULT aBuffer TO ::Buffer

   IF ::isNet
      IF !( ::Alias )->( NetRecLock() )
         RETURN .F.
      ENDIF
   ENDIF

   ( ::Alias )->( ORDSETFOCUS( 0 ) )

   FOR each Buffer in aBuffer
      ( ::Alias )->( FIELDPUT( Buffer:__EnumIndex(), Buffer ) )
   NEXT

   ( ::Alias )->( DBSKIP( 0 ) )
   IF ::isNet
      ( ::Alias )->( DBRUNLOCK() )
   ENDIF
   ( ::Alias )->( ORDSETFOCUS( nOrd ) )
   SELECT( nSel )

RETURN .T.


METHOD __oTDelete( lKeepBuffer )        // ::Delete()

   LOCAL lRet
   LOCAL lDeleted := SET( _SET_DELETED, .F. )                  // make deleted records visible
   // temporarily...
   DEFAULT lKeepBuffer TO .F.

   ::Read()

   IF ::isNet
      lRet := IF( ( ::Alias )->( NetDelete() ), .T., .F. )
   ELSE
      ( ::alias )->( DBDELETE() ) ; lRet := .T.
   ENDIF

   IF ( ( lKeepBuffer == .T. ) .or. ( ::lMonitor == .T. ) ) .and. ;
          ( lRet == .T. )
      AADD( ::DeleteBuffers, { ( ::Alias )->( RECNO() ), ::Buffer } )
   ENDIF

   IF ::isNet
      ( ::Alias )->( DBUNLOCK() )
   ENDIF

   SET( _SET_DELETED, lDeleted )

RETURN lRet


METHOD SetMonitor( l ) CLASS HBTable

   LOCAL lTemp := ::lMonitor
   ::lMonitor := !(  l )
RETURN lTemp

//
//   Transaction control subsystem...
//

METHOD Undo( nBuffer, nLevel ) CLASS HBTable


   LOCAL nLen
   LOCAL lRet      := .F.
   LOCAL lDelState := SET( _SET_DELETED )
   LOCAL nRec      :=::RECNO()
   LOCAL aBuffers

   DEFAULT nBuffer TO _WRITE_BUFFER

   IF nLevel == NIL
      nLevel := 0
   ENDIF

   SWITCH nBuffer

   CASE _DELETE_BUFFER

      IF !EMPTY( ::DeleteBuffers )

         SET( _SET_DELETED, .F. )       // make deleted records visible temporarily...

         nLen := LEN( ::deleteBuffers )

         DEFAULT nLevel TO nLen

         IF nLevel == 0 .OR. nLevel == nLen     // DO ALL...
            FOR EACH aBuffers IN ::deleteBuffers

               ( ::Alias )->( DBGOTO( aBuffers[ 1 ] ) )

               IF ( ::Alias )->( NetRecall() )
                  lRet := .T.
               ELSE
                  lRet := .F.
               ENDIF

            NEXT

            IF lRet
               ::deleteBuffers := {}
            ENDIF

         ELSE       // DO CONTROLLED...

            FOR EACH aBuffers IN ::deleteBuffers
               IF aBuffers:__EnumIndex() > ( nLen - nLevel )

                  ( ::Alias )->( DBGOTO( aBuffers[ 1 ] ) )

                  IF ( ::Alias )->( NetRecall() )
                     lRet := .T.
                  ELSE
                     lRet := .F.
                  ENDIF
               ENDIF
            NEXT

            IF lRet
               ASIZE( ::deleteBuffers, ( nLen - nLevel ) )
            ENDIF

         ENDIF

         SET( _SET_DELETED, lDelState )

      ENDIF
      EXIT

   CASE _WRITE_BUFFER
      IF !EMPTY( ::WriteBuffers )

         nLen := LEN( ::WriteBuffers )
         DEFAULT nLevel TO nLen

         IF nLevel == 0 .OR. nLen == nLevel   // Do All...

            FOR EACH aBuffers IN ::writeBuffers

               ( ::Alias )->( DBGOTO( aBuffers[ 1 ] ) )

               IF ::BufWrite( aBuffers[ 2 ] )
                  lRet := .T.
               ELSE
                  ALERT( "Rollback Failed..." )
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

                  ( ::Alias )->( DBGOTO( aBuffers[ 1 ] ) )

                  IF ::BufWrite( aBuffers[ 2 ] )
                     lRet := .T.
                  ELSE
                     ALERT( "Rollback Failed..." )
                     lRet := .F.
                  ENDIF
               ENDIF
            NEXT

            // erase entries
            IF lRet == .t.
               ASIZE( ::WriteBuffers, ( nLen - nLevel ) )
            ENDIF

         ENDIF

      ENDIF
      EXIT

   OTHERWISE

   ENDSWITCH

   ( ::Alias )->( DBUNLOCK() )
   ( ::Alias )->( DBGOTO( nRec ) )
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
   DEFAULT cOrderFile TO ::cOrderBag

   oOrd := HBOrder():New( cTag, cKey, cLabel, ;
                          cFor, cWhile, ;
                          lUnique, ;
                          bEval, nInterval )

   oOrd:oTable    := Self
   oOrd:cOrderBag := ::cOrderBag

   AADD( ::aOrders, oOrd )

RETURN oOrd


METHOD Reindex() CLASS HBTable


   LOCAL nSel := SELECT( ::Alias )
   LOCAL nOrd := ( ::Alias )->( ORDSETFOCUS( 0 ) )

   IF LEN( ::aOrders ) > 0

      IF ::USED()
         ::Kill()
      ENDIF

      ::Isnet := .F.

      IF hb_FileExists( ::cPath + ::cOrderFile )
         IF FERASE( ::cPath + ::cOrderFile ) != 0
            // --> ALERT(".CDX *NOT* Deleted !!!" )
         ENDIF
      ENDIF

      IF !::Open()
         RETURN .F.
      ENDIF

      AEVAL( ::aOrders, { | o | o:Create() } )

      ::Kill()
      ::IsNet := .T.

      IF !::Open()
         RETURN .F.
      ENDIF

   ENDIF

   ( ::Alias )->( DBSETINDEX( ::cOrderBag ) )
   ( ::Alias )->( ORDSETFOCUS( nOrd ) )
   ( ::Alias )->( DBGOTOP() )
   ( ::Alias )->( DBUNLOCK() )
   SELECT( nSel )

RETURN .T.


METHOD FastReindex() CLASS HBTable


   LOCAL nSel := SELECT( ::Alias )
   LOCAL nOrd := ( ::Alias )->( ORDSETFOCUS( 0 ) )

   IF LEN( ::aOrders ) > 0

      ::Kill()

      ::Isnet := .F.
      IF hb_FileExists( ::cPath + ::cOrderFile )
         IF FERASE( ::cPath + ::cOrderFile ) != 0
            // --> ALERT(".CDX *NOT* Deleted !!!" )
         ENDIF
      ENDIF

      IF !::Open()
         RETURN .F.
      ENDIF

      ( ::Alias )->( ORDLISTREBUILD() )

      ::Kill()
      ::IsNet := .T.

      IF !::Open()
         RETURN .F.
      ENDIF

   ENDIF

   ( ::Alias )->( DBSETINDEX( ::cOrderBag ) )
   ( ::Alias )->( ORDSETFOCUS( nOrd ) )
   ( ::Alias )->( DBGOTOP() )
   ( ::Alias )->( DBUNLOCK() )
   SELECT( nSel )

RETURN .T.


METHOD GetOrder( xOrder ) CLASS HBTable

   LOCAL nPos
   LOCAL xType := VALTYPE( xOrder )

   IF xType == "C"
      nPos := ASCAN( ::aOrders, { | e | e:Tag == xOrder } )
   ELSEIF xType == "N" .and. xOrder > 0
      nPos := xOrder
   ELSE
      nPos := 0
   ENDIF

   IF nPos == 0
      nPos := 1
   ENDIF

RETURN ::aOrders[ nPos ]                // returns oOrder


METHOD SetOrder( xTag ) CLASS HBTable

   LOCAL nOldOrd := ( ::Alias )->( ORDSETFOCUS() )

   SWITCH VALTYPE( xTag )
   CASE "C"                    // we have an Order-TAG
      ( ::Alias )->( ORDSETFOCUS( xTag ) )
      EXIT
   CASE "N"                    // we have an Order-Number
      IF xTag <= 0
         ( ::Alias )->( ORDSETFOCUS( 0 ) )
      ELSE
         ::Getorder( xTag ):SetFocus()
      ENDIF
      EXIT
   CASE "O"                    // we have an Order-Object
      xTag:SetFocus()
      EXIT
   OTHERWISE
      ( ::Alias )->( ORDSETFOCUS( 0 ) )
   ENDSWITCH
RETURN nOldOrd


METHOD GetOrderLabels() CLASS HBTable

   LOCAL aRet := {}
   IF !EMPTY( ::aOrders )
      AEVAL( ::aOrders, { | e | AADD( aRet, e:Label ) } )
   ENDIF
RETURN aRet

//
// Relation Methods
//

PROCEDURE AddChild( oChild, cKey ) CLASS HBTable                 // ::addChild()

   AADD( ::aChildren, { oChild, cKey } )
   oChild:oParent := Self
   ( ::Alias )->( ORDSETRELATION( oChild:Alias, hb_macroBlock( cKey ), cKey ) )
RETURN

/****
*     FixExt( cFileName )
*     extract .CDX filename from .DBF filename
*/

STATIC FUNCTION FixExt( cFileName )

   LOCAL nLeft := AT( ".", cFilename )
RETURN LEFT( cFileName, IF( nLeft == 0, ;
       LEN( cFilename ), ;
       nLeft - 1 ) )


METHOD CreateTable( cFile ) CLASS HBTable

   ::cDbf := cFile
   IF LEN( ::aStruc ) > 0
      ::aStruc  := {}
      ::aOrders := {}
   ENDIF
RETURN Self


METHOD PROCEDURE AddField( f, t, l, d ) CLASS HBTable

   AADD( ::aStruc, { f, t, l, d } )
RETURN


METHOD PROCEDURE Gentable() CLASS HBTable

   DBCREATE( ::cDbf, ::aStruc, ::Driver )
RETURN


METHOD OnError( uParam ) CLASS HBTable

   LOCAL cMsg := __GetMessage()
   LOCAL nPos
   LOCAL uRet, oErr

   if uParam != nil .and. LEFT( cMsg, 1 ) == '_'
      cMsg := SubStr( cMsg, 2 )
   endif
   nPos := (::Alias)->( FieldPos(cMsg) )

   if nPos != 0
      uRet := (::Alias)->( if(uParam == nil, FieldGet(nPos), FieldPut(nPos, uParam)) )
   else

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

   endif

   RETURN uRet


CLASS HBOrder

   DATA oTable
   DATA cOrderBag
   DATA Label, TAG
   DATA cKey, bKey
   DATA cFor, bFor
   DATA cWhile, bWhile
   DATA Unique INIT .F.
   DATA bEval
   DATA nInterval
   METHOD ALIAS() INLINE ::oTable:Alias

   METHOD New( cTag, cKey, cLabel, cFor, cWhile, lUnique, bEval, nInterval, cOrderBag )
   METHOD Create()

   METHOD SetFocus() INLINE ( ::alias )->( ORDSETFOCUS( ::Tag, ::cOrderBag ) )
   METHOD Destroy() INLINE ( ::alias )->( ORDDESTROY( ::Tag, ::cOrderBag ) )
   METHOD ORDDESTROY() INLINE ( ::alias )->( ORDDESTROY( ::Tag, ::cOrderBag ) )
   METHOD ORDBAGEXT() INLINE ( ::alias )->( ORDBAGEXT() )
   METHOD ORDKEYCOUNT() INLINE ( ::alias )->( ORDKEYCOUNT( ::Tag, ::cOrderBag ) )
   METHOD ORDFOR() INLINE ( ::alias )->( ORDFOR( ::Tag, ::cOrderBag ) )
   METHOD ORDISUNIQUE() INLINE ( ::alias )->( ORDISUNIQUE( ::Tag, ::cOrderBag ) )
   METHOD ORDKEY() INLINE ( ::alias )->( ORDKEY( ::Tag, ::cOrderBag ) )
   METHOD ORDKEYNO() INLINE ( ::alias )->( ORDKEYNO( ::Tag, ::cOrderBag ) )
   METHOD ORDKEYVAL() INLINE ( ::alias )->( ORDKEYVAL( ::Tag, ::cOrderBag ) )

ENDCLASS

METHOD New( cTag, cKey, cLabel, cFor, cWhile, lUnique, bEval, nInterval, cOrderBag ) CLASS HBOrder

   DEFAULT cKey TO ".T."
   DEFAULT lUnique TO .F.
   DEFAULT cFor TO ".T."
   DEFAULT cWhile TO ".T."
   DEFAULT bEval TO { || .T. }
   DEFAULT nInterval TO 1
   DEFAULT cLabel TO cTag
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


METHOD PROCEDURE Create() CLASS HBOrder

   DEFAULT ::cOrderBag TO ::oTable:cOrderBag
   //? "<<<",::alias, ::cOrderBag
   ( ::alias )->( ORDCONDSET( ::cFor, ::bFor, ;
     .T., ;
     ::bWhile, ;
     ::bEval, ::nInterval ) )

   ( ::alias )->( ORDCREATE( ::cOrderBag, ::Tag, ::cKey, ;
     ::bKey, ::Unique ) )
RETURN
