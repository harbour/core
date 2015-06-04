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
#include 'cachemgr.ch'

#include 'common.ch'
#include 'inkey.ch'
#include 'dbinfo.ch'
#include 'hbgtinfo.ch'
#include 'hbgtwvg.ch'
#include 'wvtwin.ch'
#include "wvgparts.ch"


THREAD STATIC lIsLockInfo := .F.
THREAD STATIC aLockList   := {}
THREAD STATIC aLockListG  := {}


FUNCTION MGR_TableBrowse( oCrt, aBeforeKeys, aInfo )
   LOCAL oBrw, lContinue, nLastKey, nRec, xVar, bBlock
   LOCAL cFilter, nChoice, cTitle, nStart, nSel, nFld
   LOCAL aStr    := DbStruct()
   LOCAL a_      := {}, v_
   LOCAL scr     := VouSaveScrn( 0,0,MaxRow(),MaxCol() )
   LOCAL ved_    := VouSetVstk()
   LOCAL lGUI    := .F.

#ifdef __GUI__
   IF hb_isObject( oCrt )
      lGUI := .T.
      Wvt_SetGUI( .T. )
      oCrt:cargo := CrtObjects():new( oCrt )
   ENDIF
#endif

   SET CONFIRM ON
   SET DATE FORMAT TO 'MM/DD/YYYY'
   SetCursor( 0 )

   oBrw := TBrowseNew( iif( lGUI, 3, 0 ), iif( lGUI, 20, 0 ), iif( lGUI, MaxRow()-3, maxrow()-1 ), iif( lGUI, MaxCol()-2, maxcol() ) )

   oBrw:SkipBlock     := {|x| CacheSkipper( x ) }
   oBrw:GoTopBlock    := {| | dbGoTop()         }
   oBrw:GoBottomBlock := {| | dbGoBottom()      }

   oBrw:HeadSep   := Chr( 196 ) + Chr( 194 ) + Chr( 196 )
   oBrw:ColSep    := ' ' + Chr( 179 ) + ' '
   oBrw:ColorSpec := 'N/W,W+/R,N/W,N/W'

   FOR nFld := 1 TO Len( aStr )
      AddAField( oBrw, nFld, aStr, .F. )
   NEXT

   IF lGUI
      BrwBuildComponents( oCrt, oBrw, aStr )
   ENDIF

   IF Len( aInfo ) >= 8
      oBrw:rowPos := val( aInfo[ 5 ] )
      oBrw:colPos := val( aInfo[ 6 ] )
   ENDIF
   WHILE ( !oBrw:stabilize() ) ; END

   IF Len( aInfo ) >= 8
      dbSetOrder( Val( aInfo[ 7 ] ) )
      dbGoto( val( aInfo[ 8 ] ) )
      oBrw:refreshAll()
   ENDIF
   WHILE ( !oBrw:stabilize() ) ; END

   DispStat()

   lContinue := .T.
   DO WHILE lContinue
      DO WHILE ( ( nLastKey := Inkey( , INKEY_ALL + HB_INKEY_GTEVENT ) ) == 0 .or. nLastKey == 1001 ) .and. !( oBrw:Stabilize() )
      ENDDO

      IF nLastKey == 0
         oCrt:cargo:oHBar:setData( oBrw:colPos )
         IF lIsLockInfo
            IF oBrw:colpos == 1
               oBrw:dehilite()
               oBrw:colpos := 2
               oBrw:hilite()
               oBrw:RefreshCurrent()
               oBrw:ForceStable()
            ENDIF
         ENDIF

         cTitle := HB_GtInfo( HB_GTI_WINTITLE )
         HB_GtInfo( HB_GTI_WINTITLE, substr( cTitle,1,rat('>',cTitle) )+;
                  '  [ '+ltrim( transform( recno(),"999,999,999,999" ) )+'/'+ ltrim( transform( lastrec(),"999,999,999,999" ) )+' ]' )

         nStart := seconds()
         DO WHILE ( nLastKey == 0 .or. nLastKey == 1001 )
            nLastKey := Inkey( 0, INKEY_ALL + HB_INKEY_GTEVENT )
            hb_idleSleep()
            IF ( Seconds() - nStart ) > 10
               aLockList  := DbrLockList()
               aLockListG := CacheGetAllLocks()
               oBrw:RefreshAll()
               oBrw:ForceStable()
               nStart := Seconds()
            ENDIF
         ENDDO
      ENDIF


      DO CASE
      CASE Navigate( oBrw, nLastKey )

      CASE MyBeforeKeys( oBrw, nLastKey, aBeforeKeys )

      CASE nLastkey == K_ESC .OR. nLastKey == HB_K_CLOSE
         lContinue := .F.

      CASE nLastKey == K_F1                       // HELP
         BrwHelp( oBrw )

      CASE nLastKey == K_F2                       // ORDER ZERO
         Set Order To 0
         oBrw:RefreshAll()
         oBrw:ForceStable()
         DispStat()

      CASE nLastKey == K_F3 .OR. nLastKey == K_ALT_O  // ORDER
         BrwOrder( oBrw )

      CASE nLastKey == K_F4                       // GOTO
         nRec := VouGetSome( 'Goto Record ?', RecNo(), , 0 )
         BrwDbgoto( oBrw, nRec )

      CASE nLastKey == K_SH_F5                    // Move Column Right
         BrwMoveRight( oBrw )

      CASE nLastKey == K_SH_F6                    // Move Column Left
         BrwMoveLeft( oBrw )

      CASE nLastKey == K_ALT_F5                   // AddColumn
         BrwAddColumn( oBrw )

      CASE nLastKey == K_ALT_F6                   // DeleteColumn
         BrwDelColumn( oBrw )

      CASE nLastKey == K_F5                       // SCROLL
         nSel := max(1,Alert( 'Direction Please!', { 'Bottom-Top-Bottom','Bottom','Always Bottom' } ))
         BrwMove( oBrw, nSel )

      CASE nLastKey == K_F6                       // UTILITIES
         BrwUtils( oBrw )

      CASE nLastKey == K_F7                       // SEEK
         BrwSeek( oBrw, .F., .f., .f. )

      CASE nLastKey == K_SH_F7                    // SEEK
         BrwSeek( oBrw, .F., .f., .T. )

      CASE nLastKey == K_F8                       // FREEZE
         oBrw:Freeze++
         oBrw:ForceStable()

      CASE nLastKey == K_SH_F8                    // UNFREEZE
         IF oBrw:freeze > 0
            oBrw:freeze--
            oBrw:forceStable()
         ENDIF

      CASE nLastKey == K_F9                       // TABLE STRUCTURE
         BrwStructure( oBrw )

      CASE nLastKey == K_F10                      // LOCKs Menu
         BrwLocks( oBrw )

      CASE nLastKey == K_F11                      // ZOOM - TOGGLE
         //BrwFullScreen( oBrw )
         //DispStat()
         Alert( "Please click 'Maximize' button on the titlebar" )
         // TODO: Fix SetMode() IN GTWVG

      CASE nLastKey == K_F12                      // Save View
         BrwSaveView( oBrw )

      CASE nLastKey == K_ALT_L                    // LOCK
         IF ! DbrLock( RecNo() )
            Alert( 'Record could not been locked!' )
         ENDIF
         aLockList  := DbrLockList()
         aLockListG := CacheGetAllLocks()
         oBrw:RefreshCurrent()
         oBrw:forceStable()

      CASE nLastKey == K_ALT_U                    // UNLOCK
         DbrUnlock( RecNo() )
         aLockList  := DbrLockList()
         aLockListG := CacheGetAllLocks()
         oBrw:RefreshAll()
         oBrw:forceStable()

      CASE nLastKey == K_ALT_S                    // LOCK LIST
         VouArView( DbrLockList() )

      CASE nLastKey == K_ALT_G                    // Global Locks
         MGR_ArrayBrowse( CacheGetAllLocks(), { { 1, 'Record Number', 'N', 10, 0,'@Z 9999999999' } }, ;
                                  'Global Locks on : '+alias(), {} )

      CASE nLastKey == K_ALT_I
         MGR_ShowProcesses( CacheGetLockInfo(), 'Lock Info for Record #'+NTRIM( RecNo() ) )

      CASE nLastKey ==  K_ALT_F                   // FILTER
         cFilter := trim( VouGetSome( 'Filter Expression', space( 50 ) ) )
         IF ! Empty( cFilter )
            SET FILTER TO  &cFilter
            oBrw:goTop()
            oBrw:RefreshAll()
            oBrw:ForceStable()
         ENDIF

      CASE nLastKey == K_ALT_R                    // CLEAR FILTER
         Set Filter TO
         oBrw:rowPos := 1
         oBrw:RefreshAll()
         oBrw:ForceStable()

      CASE nLastKey == K_ALT_INS                  // APPEND BLANK
         DbAppend()
         oBrw:RefreshAll()
         oBrw:ForceStable()

      CASE nLastKey == K_ALT_DEL                  // DELETE RECORD
         IF Alert( 'Delete Current Record ?', {'No','Yes'} ) == 2
            //CacheBeginTransaction()
            IF RLock()
               DbDelete()
               DbUnlock()
            ENDIF
            //CacheRollBackTransaction()
            //CacheEndTransaction()
            oBrw:RefreshAll()
            oBrw:ForceStable()
         ENDIF

      CASE nLastKey == K_ALT_K                    // UNLOCK SELECTED RECORD
         a_:={}
         v_:= DbrLockList()
         AEval( v_, {|e| AAdd( a_, str( e,10,0 ) ) } )
         IF ! Empty( a_ )
            nChoice := VouPopup( a_, 'Select a Record to Unlock' )
            IF nChoice > 0
               DbrUnlock( v_[ nChoice ] )
            ENDIF
         ENDIF

      CASE nLastKey == K_ALT_E                    //  SEEK LAST
         BrwSeek( oBrw, .T., .F., .f. )

      CASE nLastKey == K_ALT_Y                    //  SOFT SEEK
         BrwSeek( oBrw, .F., .T., .f. )

      CASE nLastKey == K_ALT_P                    // SET SCOPE
         BrwSetScope( oBrw )

      CASE nLastKey == K_ALT_W                    // CLEAR SCOPE
         IF IndexOrd() > 0
            OrdScope( 0,nil )
            OrdScope( 1,nil )
            oBrw:goTop()
            oBrw:RefreshAll()
            oBrw:ForceStable()
         ENDIF

      CASE nLastKey == K_ALT_X                    // Sum Average High Low
         SomeNumbers( oBrw )

      CASE nLastKey == K_ALT_T                    // STATISTICS
         SomeStats()

      CASE nLastKey == K_ALT_Q                    // Toggle LOCK info column
         BrwLockInfo( oBrw )

      CASE nLastKey == K_ALT_V
         BrwShowPerform()

      CASE nLastKey == K_ALT_Z
         nRec := VouGetSome( 'Skip how many records?', 0, , 0 )
         DbSkip( nRec )
         oBrw:RefreshAll()
         oBrw:ForceStable()

      CASE nLastKey == K_ALT_F1
         IF ( bBlock := SetKey( nLastKey ) ) <> NIL
            eval( bBlock )
         ENDIF

      CASE nLastKey == K_CTRL_LEFT
         BrwDecWidth( oBrw )

      CASE nLastKey == K_CTRL_RIGHT
         BrwIncWidth( oBrw )

      CASE nLastKey == K_CTRL_F1
         BrwFind( oBrw )

      CASE nLastKey == K_CTRL_F12
         xVar := Eval( oBrw:GetColumn( oBrw:ColPos ):Block )
         IF ValType( xVar ) == 'C'
            nFld := oBrw:GetColumn( oBrw:ColPos ):Cargo
            oBrw:GetColumn( oBrw:ColPos ):Block := {|| PassDecrypt( fieldget( nFld ) ) }
            oBrw:refreshAll()
         ENDIF

      CASE nLastKey == K_ENTER                    // EDIT
         BrwHandleEnter( oBrw )

      CASE nLastKey == HB_K_RESIZE
         oBrw:nBottom := iif( lGUI, MaxRow()-3, MaxRow()-1 )
         oBrw:nRight := iif( lGUI, MaxCol()-2, MaxCol() )
         oBrw:RefreshAll()
         oCrt:cargo:oHBar:setPosAndSize()
         DispBox( 0, 0, MaxRow(), MaxCol(), "         ", "N/W" )
         DispStat()
      ENDCASE
   ENDDO
   VouRestScrn( scr )
   VouSetVstk( ved_ )
   lIsLockInfo := .F.

   RETURN nil


FUNCTION BrwPosCursor( oBrw, nRec )

   oBrw:refreshAll()
   oBrw:forceStable()
   IF RecNo() != nRec
      oBrw:dehilite()
      oBrw:RowPos := 1
      oBrw:forceStable()
      Dbgoto( nRec )
      oBrw:refreshAll()
      oBrw:forceStable()
      oBrw:hilite()
   ENDIF

   RETURN nil


#if 0
STATIC FUNCTION GetPrevBlockFld( oBrw )
   RETURN oBrw:GetColumn( oBrw:ColPos ):Cargo
#endif


STATIC FUNCTION AddAField( oBrw, nFld, aStr, lInsert )
   LOCAL h, w, oCol

   DEFAULT aStr    TO DbStruct()
   DEFAULT lInsert TO .T.

   h := alltrim( FieldName( nFld ) )
   w := max( Len( FieldName( nFld ) ), aStr[ nFld,3 ] )
   IF aStr[ nFld,2 ] == 'D'
      w += 2
   ENDIF
   IF aStr[ nFld,2 ] == 'N'
      h := padl( h, w )
   ENDIF
   oCol := TBColumnNew( h, FieldBlock( FieldName( nFld ) ) )
   oCol:width   := w // aStr[ nFld,3 ]
   oCol:picture := BrwPicture( aStr[ nFld,2 ], aStr[ nFld,3 ], aStr[ nFld,4 ] )
   oCol:cargo   := nFld

   IF lInsert
      oBrw:InsColumn( oBrw:ColPos, oCol )
      oBrw:RefreshAll()
   ELSE
      oBrw:addColumn( oCol )
   ENDIF

   RETURN NIL


STATIC FUNCTION BrwAddColumn( oBrw )
   LOCAL aStr := DbStruct()
   LOCAL aMnu := {}
   LOCAL nFld

   AEval( aStr, {|e_| AAdd( aMnu, e_[ 1 ] ) } )

   IF ( nFld := VouPopup( aMnu, 'Select a Field to Insert' ) ) > 0
      AddAField( oBrw, nFld, aStr, .T. )
   ENDIF

   RETURN nil


STATIC FUNCTION BrwHandleEnter( oBrw )
   LOCAL v_, oCol, xVar, cPic, kRB, kCC, nRec
   LOCAL getlist := {}

   v_:= VouSetVstk()
   oCol := oBrw:GetColumn( oBrw:ColPos )
   xVar := Eval( oCol:Block )
   cPic := oCol:picture
   IF ValType( xVar ) == "C"
      IF col()+Len( xVar ) > min( oCol:width, oBrw:nRight )
         cPic := "@S"+ltrim(str(min(oBrw:nRight-col()+1,  oCol:width )))
      ENDIF
   ENDIF
   oBrw:dehilite()
   SetCursor( 1 )
   kRB := Setkey( K_RBUTTONDOWN, {|| PostCopiedText() } )
   kCC := Setkey( K_CTRL_C     , {|| CopyGetText() } )

   @ row(),col() GET xVar PICTURE cPic COLOR 'W+/R,N/GR*'
   READ

   Setkey( K_RBUTTONDOWN, kRB )
   Setkey( K_CTRL_C     , kCC )
   SetCursor( 0 )
   oBrw:hilite()

   IF RLock()
      nRec := recNo()
      eval( oBrw:GetColumn( oBrw:ColPos ):Block, xVar )
      DbCommit()
      DbUnlock()
      BrwPosCursor( oBrw, nRec )
      oBrw:Right()
   ELSE
      Alert( 'Some other process has this record locked!' )
   ENDIF
   VouSetVstk( v_ )

   RETURN NIL


STATIC FUNCTION BrwSetScope( oBrw )
   LOCAL cScope

   IF IndexOrd() > 0
      cScope := VouGetSome( 'Scope Top', EmptyVrb( &( IndexKey() ) ) )
      IF ValType( cScope ) == 'C'
         cScope := trim( cScope )
      ENDIF
      IF ! Empty( cScope )
         OrdScope( 0, cScope )
         cScope := VouGetSome( 'Scope Bottom', EmptyVrb( &( IndexKey() ) ) )
         IF ValType( cScope ) == 'C'
            cScope := trim( cScope )
         ENDIF
         IF ! Empty( cScope )
            OrdScope( 1, cScope )
         ENDIF
         oBrw:goTop()
         oBrw:RefreshAll()
         oBrw:ForceStable()
      ENDIF
   ENDIF

   RETURN NIL


STATIC FUNCTION BrwStructure( oBrw )
   LOCAL cText := "", aStr, n, kF12, cKey
   LOCAL mnu_:={}

   HB_SYMBOL_UNUSED( oBrw )

   cText += hb_eol()
   cText += replicate( "-", 40 ) + hb_eol()
   cText += padc( "Table: " + alias() + "  Date: " + dtoc( date() ) + "-" + time(), 40 ) + hb_eol()
   cText += replicate( "-", 40 ) + hb_eol()
   cText += hb_eol()

   aStr := DbStruct()
   AEval( aStr, {|e_,i,x| x := str( i,3,0 ) +' '+ pad( e_[ 1 ],10 ) +' '+ e_[ 2 ] + ;
                        ' '+ str( e_[ 3 ],3,0 ) +' '+ str( e_[ 4 ],2,0 ), ;
                        AAdd( mnu_,x ), ;
                        cText += padc( x, 40 ) + hb_eol() } )

   cText += hb_eol()
   cText += replicate( "-", 40 ) + hb_eol()
   cText += hb_eol()
   n := 0
   DO WHILE .T.
      n++
      cKey := OrdKey( n )
      IF Empty( cKey )
         EXIT
      ENDIF
      cText += pad( OrdName( n ), 12 ) + " " + cKey + hb_eol()
   ENDDO
   cText += hb_eol()
   cText += replicate( "-", 40 ) + hb_eol()
   cText += hb_eol()
   //
   kF12 := SetKey( K_F12, {|| Hb_GtInfo( HB_GTI_CLIPBOARDDATA, cText ) } )
   VouPopup( mnu_, alias() + " [ F12 Clipboard ]" )
   SetKey( K_F12, kF12 )

   RETURN NIL


STATIC FUNCTION BrwOrder( oBrw )
   LOCAL i, cKey, a_:={}

   FOR i := 1 TO 50
      IF ( cKey := indexkey( i ) ) == ''
         EXIT
      ENDIF
      AAdd( a_, OrdName( i ) + ' : ' + cKey )
   NEXT

   IF ! Empty( a_ )
      IF ( i := VouPopup( a_, 'Select an Index by Key !' /*, { 5,13,20,67 } */ ) ) > 0
         DbSetOrder( i )
         oBrw:RefreshAll()
         oBrw:ForceStable()

         DispStat()
      ENDIF
   ENDIF

   RETURN NIL


STATIC FUNCTION BrwDelColumn( oBrw )

   IF oBrw:ColCount > 1
      oBrw:DelColumn( oBrw:ColPos )
      oBrw:RefreshAll()
   ENDIF

   RETURN nil


STATIC FUNCTION BrwMoveLeft( oBrw )
   LOCAL save_col, col_to_move := oBrw:colPos

   IF col_to_move > 1
      save_col := oBrw:getColumn(col_to_move)
      oBrw:setcolumn(col_to_move, oBrw:getcolumn(col_to_move - 1))
      oBrw:setcolumn(col_to_move - 1, save_col)
      oBrw:left()
      oBrw:refreshAll()
   ENDIF

   RETURN col_to_move > 1


STATIC FUNCTION BrwMoveRight( b )
   LOCAL save_col, col_to_move := b:colPos

   IF col_to_move < b:colcount
      save_col := b:GetColumn(col_to_move)
      b:setColumn(col_to_move, b:getColumn(col_to_move + 1))
      b:setColumn(col_to_move + 1, save_col)
      b:Right()
      b:refreshall()
   ENDIF

   RETURN col_to_move < b:colcount


STATIC FUNCTION BrwDecWidth( oBrw )
   LOCAL oCol := oBrw:GetColumn( oBrw:ColPos )
   LOCAL cType := ValType( eval( oCol:Block ) )

   IF cType == 'C' .and. oCol:Width <> NIL
      IF oCol:Width > 3
         oCol:Width --
         oBrw:Configure()
         oBrw:ForceStable()
      ENDIF
   ENDIF

   RETURN nil


STATIC FUNCTION BrwIncWidth( oBrw )
   LOCAL oCol := oBrw:GetColumn( oBrw:ColPos )
   LOCAL cType := ValType( eval( oCol:Block ) )

   IF cType == 'C' .and. oCol:Width <> NIL
      oCol:Width++
      oBrw:Configure()
      oBrw:ForceStable()
   ENDIF

   RETURN nil


STATIC FUNCTION BrwSeek( oBrw, lSeekLast, lSoftSeek, lAsGet )
   LOCAL cSeek, nRec, scr, nFound

   THREAD STATIC ccSeek
   THREAD STATIC nLastOrd

   IF IndexOrd() <= 0
      BrwOrder( oBrw )
   ENDIF
   IF IndexOrd() <= 0
      Alert( 'No index active. Please set it via F3!' )
      RETURN nil
   ENDIF

   IF nLastOrd != IndexOrd()
      ccSeek := NIL
      nLastOrd := IndexOrd()
   ENDIF

   cSeek := &( indexKey() )
   IF ValType( cSeek ) == 'C' .and. !( lAsGet )
      IF Empty( ccSeek )
         ccSeek := ""
      ENDIF
      scr := VouWinSave( MaxRow()-1, 0, maxrow()-1, MaxCol() )
      uiDispOut( MaxRow() - 1, 0, PadC( IndexKey(), MaxCol() + 1 ), "W+/B*" )
      cSeek := VouGetString( IndexKey(), ccSeek )
      VouWinRest( scr )
      IF Len( cSeek ) == 0
         RETURN NIL
      ENDIF
      ccSeek := cSeek
   ELSE
      cSeek := VouGetSome( IndexKey(), cSeek )
   ENDIF

   IF lAsGet
      cSeek := trim( cSeek )
   ENDIF

   nRec := RecNo()
   dbseek( cSeek, lSoftSeek, lSeekLast )
   IF ! Found()
      Alert( 'Not Found <' + OrdName() + '><' + VouXtos( cSeek ) + '>' )

      IF ! lSoftSeek
         DbGoTo( nRec )
      ENDIF
   ELSE
      nFound := RecNo()
   ENDIF

   oBrw:RefreshAll()
   oBrw:ForceStable()

   IF ! Empty( nFound )
      IF ! ( RecNo() == nFound )
         // Tells that oBrw:rowPos is NOT synchronized WITH recno()
         // This only happens when record is up amd row is down
         DO WHILE !( RecNo() == nFound )
            oBrw:up()
            oBrw:ForceStable()
         ENDDO
      ENDIF
   ENDIF

   RETURN nil


STATIC FUNCTION BrwFind( oBrw )
   LOCAL cSeek, nFld, aStr, bBlock
   LOCAL lFound := .F.

   aStr   := DbStruct()
   nFld   := oBrw:GetColumn( oBrw:ColPos ):Cargo
   bBlock := oBrw:GetColumn( oBrw:ColPos ):Block
   cSeek  := VouGetSome( aStr[ nFld, 1 ], eval( bBlock ) )

   DO WHILE .T.
      oBrw:down()
      oBrw:ForceStable()
      IF oBrw:hitBottom
         EXIT
      ENDIF
      IF eval( bBlock ) == cSeek
         lFound := .T.
         EXIT
      ENDIF
      IF Inkey() == K_ESC
         EXIT
      ENDIF
      IF nextkey() == K_ESC
         EXIT
      ENDIF
   ENDDO

   IF ! lFound
      Alert( "Record could not been found!" )
   ENDIF

   RETURN nil


STATIC FUNCTION BrwShowPerform()
   LOCAL cRet

   THREAD STATIC nState := 0
   THREAD STATIC cMetrics := ''

   IF nState == 0
      cRet := CachePerfMon( 3 )  // Enable
      ? cRet
      IF cRet <> 'ALREADY'
         nState := 1
         cMetrics := CachePerfMon( 2 )
         ? cMetrics
      ENDIF
   ENDIF

   IF nState > 0
      cRet := CachePerfMon( 1 )
      ? cRet
      inkey(0)
   ENDIF

   RETURN nil


STATIC FUNCTION BrwPicture( cType, nLen, nDec )
   LOCAL cPic

   DO CASE
   CASE cType == 'N'
      cPic := '@Z '+if( nDec > 0, replicate( '9',nLen-nDec-1 )+'.'+replicate( '9',nDec ), ;
                                  replicate( '9',nLen ) )
   CASE cType == "C"
      cPic := "@ " + replicate( 'X', nLen )
   ENDCASE

   RETURN cPic

#if 0
STATIC FUNCTION BrwFullScreen( oBrw )
   HB_SYMBOL_UNUSED( oBrw )
   LOCAL aFontNew   := Wvt_GetFontInfo()
   LOCAL nScrWidth  := Wvt_GetScreenWidth()
   LOCAL nScrHeight := Wvt_GetScreenHeight() - 40
   LOCAL wvtRows    := int( nScrHeight / aFontNew[ 6 ] )
   LOCAL wvtCols    := int( nScrWidth  / aFontNew[ 7 ] ) - 1

   THREAD STATIC lInZoom := .F.

   IF lInZoom
      wvtRows  := 25
      wvtCols  := 80
   ELSE
      wvtRows  := max(  7, wvtRows )
      wvtCols  := max( 20, wvtCols )
   ENDIF

   lInZoom := !lInZoom

//   hb_gtInfo( HB_GTI_SCREENHEIGHT, wvtRows * aFontNew[ 6 ] )
//   hb_gtInfo( HB_GTI_SCREENWIDTH , wvtCols * aFontNew[ 7 ] )
   SetMode( wvtRows, wvtCols )
   DispBox( 0,0,MaxRow(),MaxCol(),'         ','N/W' )

   oBrw:nLeft   := 0
   oBrw:nTop    := 0
   oBrw:nRight  := MaxCol()
   oBrw:nBottom := MaxRow()-1

   oBrw:Configure()
   oBrw:RefreshAll()
   oBrw:forceStable()
//#else
//   Wvt_ShowWindow( SW_SHOWMAXIMIZED )
//#endif
   RETURN nil
#endif


STATIC FUNCTION DispStat( cStatus, lArrays )

   DEFAULT lArrays TO .F.

   IF lArrays
      DEFAULT cStatus TO '< Array Browser >'
      @ MaxRow(), 0 SAY PadC( cStatus, MaxCol()+1 ) COLOR 'W+/B'

   ELSE
      cStatus := "<" + Alias() + ">" + iif( Empty( IndexOrd() ), "", "<" + OrdName() + " : " + IndexKey() + ">" )

      @ MaxRow(), 0 SAY PadC( 'F2 OrdZ  F3 Ordr  F4 Goto  F5 Scrl  F6 Util  F7 Seek  F8 Frez  F9 Stru  F10 Lock', MaxCol()+1 ) COLOR 'W+/B'
      Hb_GtInfo( HB_GTI_WINTITLE, "[" + CacheGetConnectionInfo( DBCI_IP ) + "][" + str( CacheGetCurrentProcessID(),5 ) + "] " + cStatus )
   ENDIF

   RETURN NIL


STATIC FUNCTION Navigate( oBrw, nLastKey )
   LOCAL lDone := .T.

   DO CASE
   CASE nLastKey == K_UP
      oBrw:Up()

   CASE nLastKey == K_DOWN
      oBrw:Down()

   CASE nLastKey == K_PGUP
      oBrw:PageUp()

   CASE nLastKey == K_PGDN
      oBrw:PageDown()

   CASE nLastKey == K_CTRL_PGUP
      oBrw:GoTop()

   CASE nLastKey == K_CTRL_PGDN
      oBrw:GoBottom()

   CASE nLastKey == K_RIGHT
      oBrw:Right()

   CASE nLastKey == K_LEFT
      oBrw:left()

   CASE nLastKey == K_HOME
      oBrw:home()

   CASE nLastKey == K_END
      oBrw:end()

   CASE nLastKey == K_CTRL_HOME
      oBrw:PanHome()

   CASE nLastKey == K_CTRL_END
      oBrw:PanEnd()

   CASE nLastKey == K_MWBACKWARD
      oBrw:down()

   CASE nLastKey == K_MWFORWARD
      oBrw:up()

   CASE Vou_NavigateToCell( oBrw, nLastKey )

   OTHERWISE
      lDone := .F.

   ENDCASE

   RETURN lDone


FUNCTION Vou_NavigateToCell( oBrowse, nLastKey )
   LOCAL nCount, nHitWhere

   IF nLastKey != K_LBUTTONUP
      RETURN .F.
   ENDIF

   nHitWhere := oBrowse:HitTest( mrow(), mcol() )

   IF nHitWhere ==  -5121   // on a cell
      oBrowse:dehilite()
      oBrowse:refreshCurrent()
      oBrowse:forceStable()

      nCount := oBrowse:mRowPos - oBrowse:RowPos
      DispBegin()
      WHILE ( nCount < 0 )
         nCount++
         oBrowse:Up()
         oBrowse:ForceStable()
      ENDDO

      WHILE ( nCount > 0 )
         nCount --
         oBrowse:Down()
         oBrowse:ForceStable()
      ENDDO

      nCount := oBrowse:mColPos - oBrowse:ColPos
      WHILE ( nCount < 0 )
         nCount++
         oBrowse:Left()
      ENDDO

      WHILE ( nCount > 0 )
         nCount--
         oBrowse:Right()
      ENDDO
      DispEnd()
      RETURN .T.
   ENDIF

   RETURN .F.


STATIC FUNCTION CacheSkipper( nRequest )
   LOCAL nCount := 0

   IF ( nRequest > 0 )
      DO WHILE ( nCount < nRequest )
         skip 1
         IF eof()
            dbGobottom()
            EXIT
         ENDIF
         nCount++
      ENDDO

   ELSEIF ( nRequest < 0 )
      WHILE ( nCount > nRequest )
         skip -1
         IF bof()
            dbGotop()
            EXIT
         ENDIF
         nCount--
      END
   ENDIF

   RETURN (nCount)


STATIC FUNCTION BrwMove( oBrw, nMode )
   LOCAL n := 0
   LOCAL nStart := seconds()
   LOCAL n10

   DEFAULT nMode TO 1

   IF nMode == 3
      n := LastRec()
      DO WHILE .T.
         oBrw:GoBottom()
         oBrw:RefreshAll()
         oBrw:ForceStable()

         IF Inkey() == K_ESC
            EXIT
         ENDIF

         n10 := LastRec()-n
         HB_GtInfo( HB_GTI_WINTITLE, 'Records: '+ltrim( str( LastRec(),10,0 ) ) + ' Processed: ' + ;
                ltrim( str( n10,10,0 ) )+' / '+ltrim( str( seconds()-nStart ) )+' Seconds')
         inkey( 1 )
      ENDDO
   ELSEIF nMode == 1 .or. nMode == 2
      DO WHILE .T.
         n++
         oBrw:down()
         oBrw:ForceStable()
         IF oBrw:hitBottom
            IF nMode == 1
               oBrw:GoTop()
            ENDIF
            oBrw:RefreshAll()
            oBrw:ForceStable()
         ENDIF

         IF Inkey() == K_ESC
            EXIT
         ENDIF

         IF ( n%10000 == 0 )
            HB_GtInfo( HB_GTI_WINTITLE, ltrim( str( n,10,0 ) ) + ' = '+ ltrim( str( seconds()-nStart ) ) + '  10000 : '+ltrim( str( seconds()-n10 ) ) )
            n10 := seconds()
         ENDIF
      ENDDO
   ELSEIF nMode == 4
      DO WHILE .T.
         n++
         oBrw:down()
         oBrw:ForceStable()
         IF oBrw:hitBottom
            IF nMode == 1
               oBrw:GoTop()
            ENDIF
            oBrw:RefreshAll()
            oBrw:ForceStable()
         ENDIF

         IF Lastkey() == K_ESC .or. NextKey() == K_ESC
            EXIT
         ENDIF

         IF ( n%10000 == 0 )
            HB_GtInfo( HB_GTI_WINTITLE, ltrim( str( n,10,0 ) ) + ' = '+ ltrim( str( seconds()-nStart ) ) + '  10000 : '+ltrim( str( seconds()-n10 ) ) )
            n10 := seconds()
         ENDIF
      ENDDO
   ELSEIF nMode == 5
      DO WHILE .T.
         n++
         oBrw:up()
         oBrw:ForceStable()
         IF oBrw:hitTop
            IF nMode == 1
               oBrw:GoBottom()
            ENDIF
            oBrw:RefreshAll()
            oBrw:ForceStable()
         ENDIF

         IF Lastkey() == K_ESC .or. NextKey() == K_ESC
            EXIT
         ENDIF

         IF ( n%10000 == 0 )
            HB_GtInfo( HB_GTI_WINTITLE, ltrim( str( n,10,0 ) ) + ' = '+ ltrim( str( seconds()-nStart ) ) + '  10000 : '+ltrim( str( seconds()-n10 ) ) )
            n10 := seconds()
         ENDIF
      ENDDO
   ENDIF

   Keyboard( CHR( K_UP ) )

   RETURN nil


STATIC FUNCTION BrwLockInfo( oBrw )
   LOCAL oCol

   IF !lIsLockInfo
      aLockList  := DbrLockList()
      aLockListG := CacheGetAllLocks()

      oCol := TBColumnNew( 'LK', {|| iif( ascan( aLockListG, RecNo() ) > 0, chr( 251 ),' ' ) + ;
                                     iif( ascan( aLockList , RecNo() ) > 0, chr( 251 ),' ' ) } )
      oCol:width := 2
      oBrw:InsColumn( 1,oCol )
      oBrw:freeze++
   ELSE
      oBrw:DelColumn( 1 )
      oBrw:freeze--

   ENDIF
   lIsLockInfo := !lIsLockInfo

   oBrw:refreshAll()
   oBrw:forceStable()

   RETURN nil


STATIC FUNCTION BrwSaveView( oBrw )
   LOCAL aPos, cText := ""

   aPos := Wvg_GetWindowRect( Wvt_GetWindowHandle() )

   cText += hb_ntos( aPos[ 1 ]   ) + ";"
   cText += hb_ntos( aPos[ 2 ]   ) + ";"
   cText += hb_ntos( MaxRow()    ) + ";"
   cText += hb_ntos( MaxCol()    ) + ";"
   cText += hb_ntos( oBrw:rowPos ) + ";"
   cText += hb_ntos( oBrw:colPos ) + ";"
   cText += hb_ntos( indexOrd()  ) + ";"
   cText += hb_ntos( recNo()     ) + ";"

   SetINIValue( "View_" + alias(), cText )

   RETURN NIL


STATIC FUNCTION BrwHelp( oBrw )
   LOCAL v_:= {}, nChoice

   HB_SYMBOL_UNUSED( oBrw )

   AAdd( v_, 'F1      Help      This Screen'                                      )
   AAdd( v_, 'F2      NatOrd    Sets the index to 0 for natural record order'     )
   AAdd( v_, 'F3      Order     Set a new index order'                            )
   AAdd( v_, 'F4      Goto      Goto a specific record by number'                 )
   AAdd( v_, 'F5      Scroll    Auto scrolls the browser current-bottom-top'      )
   AAdd( v_, 'F6      Utilities More database options - filters, delete, appends' )
   AAdd( v_, 'F7      Seek      Search for a record by Current Index'             )
   AAdd( v_, 'F8      Freeze    Freezes leftmoost column'                         )
   AAdd( v_, 'F9      Struct    View Structure of the Table'                      )
   AAdd( v_, 'F10     Lock      Toggles locks status of currents record'          )
*  AAdd( v_, 'F11     Zoom      Toggles browser window full/normal screen'        )
   AAdd( v_, 'F12     Save      Save browser coordinates'                         )
   AAdd( v_, 'Sh+F8   UnFreeze  UnFreezes last freezed column'                    )
   AAdd( v_, 'ENTER   Edit      Edit current cell'                                )
   AAdd( v_, 'Sh+F5   MoveRight Moves current column right one position'          )
   AAdd( v_, 'Sh+F6   MoveLeft  Moves current column left on position'            )
   AAdd( v_, 'Alt+F5  InsColumn Insert column at current location'                )
   AAdd( v_, 'Alt+F6  DelColumn Deletes currently hilighted column'               )
   AAdd( v_, 'Ctrl+Left         Decrease column width'                            )
   AAdd( v_, 'ctrl+Right        Increases column width'                           )

   nChoice := VouPopup( v_, 'Help' )
   IF nChoice > 1
      wvt_Keyboard( {K_F1,K_F2,K_F3,K_F4,K_F5,K_F6,K_F7,K_F8,K_F9,;
                     K_F10,K_F11,K_SH_F8,K_ENTER, K_SH_F5,K_SH_F6,;
                     K_ALT_F5,K_ALT_F6,K_CTRL_LEFT,K_CTRL_RIGHT}[ nChoice ] )
   ENDIF

   RETURN Nil


STATIC FUNCTION BrwUtils( oBrw )
   LOCAL v_:={}, mnu_:={}
   LOCAL nChoice

   HB_SYMBOL_UNUSED( oBrw )

   AAdd( v_, { 'Seek Last                  Alt_E  ' , K_ALT_E   } )
   AAdd( v_, { 'Seek Soft                  Alt_Y  ' , K_ALT_Y   } )
   AAdd( v_, { 'Skip Records               ALT_Z  ' , K_ALT_Z   } )
   AAdd( v_, { 'Set a Filter               Alt_F  ' , K_ALT_F   } )
   AAdd( v_, { 'Clear Filter               Alt_R  ' , K_ALT_R   } )
   AAdd( v_, { 'Set Scope                  Alt_P  ' , K_ALT_P   } )
   AAdd( v_, { 'Clear Scope                Alt_W  ' , K_ALT_W   } )
   AAdd( v_, { 'Append a blank Record      Alt_INS' , K_ALT_INS } )
   AAdd( v_, { 'Delete Current Record      Alt_DEL' , K_ALT_DEL } )
   AAdd( v_, { 'Show Statistics            Alt_T  ' , K_ALT_T   } )
   AAdd( v_, { 'Performance Stats          Alt_V  ' , K_ALT_V   } )
   AAdd( v_, { ' '                                  , NIL       } )
   AAdd( v_, { 'Sum Average High Low       Alt_X  ' , K_ALT_X   } )
   AAdd( v_, { 'Find in field              Ctrl_F1' , K_CTRL_F1 } )

   AEval( v_, {|e_| AAdd( mnu_, e_[ 1 ] ) } )

   nChoice := VouPopup( mnu_, 'Select an Option' )

   IF nChoice > 0
      Wvt_Keyboard( v_[ nChoice,2 ] )
   ENDIF

   RETURN nil


STATIC FUNCTION BrwLocks( oBrw )
   LOCAL v_:={}, mnu_:={}
   LOCAL nChoice

   HB_SYMBOL_UNUSED( oBrw )

   AAdd( v_, { 'Lock Current Record        Alt_L  ' , K_ALT_L   } )
   AAdd( v_, { 'Unlock Current Record      Alt_U  ' , K_ALT_U   } )
   AAdd( v_, { 'Unlock a Selective Record  Alt_K  ' , K_ALT_K   } )
   AAdd( v_, { ' '                                  , NIL       } )
   AAdd( v_, { 'List of Locked Records     Alt_S  ' , K_ALT_S   } )
   AAdd( v_, { 'Lock Info                  Alt_I  ' , K_ALT_I   } )
   AAdd( v_, { ' '                                  , NIL       } )
   AAdd( v_, { 'List of Global Locks       Alt_G  ' , K_ALT_G   } )
   AAdd( v_, { ' '                                  , NIL       } )
   AAdd( v_, { 'Lock Info Column (Toggle)  Alt_Q  ' , K_ALT_Q   } )

   AEval( v_, {|e_| AAdd( mnu_, e_[ 1 ] ) } )

   nChoice := VouPopup( mnu_, 'Select an Option' )

   IF nChoice > 0
      Wvt_Keyboard( v_[ nChoice,2 ] )
   ENDIF

   RETURN nil


FUNCTION SomeStats()
   LOCAL aStats := {}

   AAdd( aStats, '   Generic'                                     )
   AAdd( aStats, 'Server Time     = '+ VouXtoS( CacheGetServerTime()  ) )
   AAdd( aStats, 'Server Date     = '+ VouXtoS( CacheGetServerDate()  ) )
   AAdd( aStats, 'Insert Lock Mode= '+ VouXtoS( CacheInsertLockMode() ) )
   AAdd( aStats, 'LastRec()       = '+ VouXtoS( LastRec()  )      )
   AAdd( aStats, 'OrdKeyNo()      = '+ VouXtoS( OrdKeyNo() )      )
   AAdd( aStats, 'OrdKeyCount()   = '+ VouXtoS( OrdKeyCount() )   )
   AAdd( aStats, '   Field Info'                                  )
   AAdd( aStats, 'FCount()        = '+ VouXtoS( FCount() )        )
   AAdd( aStats, 'DbFieldInfo()   = '+ VouXtoS( DbFieldInfo( 1,1 ) ) )
   AAdd( aStats, '   Index Info'                                  )
   AAdd( aStats, 'IndexKey()      = '+ VouXtoS( IndexKey() )      )
   AAdd( aStats, 'IndexOrd()      = '+ VouXtoS( IndexOrd() )      )
   AAdd( aStats, 'IndexExt()      = '+ VouXtoS( IndexExt() )      )
   AAdd( aStats, 'OrdKey()        = '+ VouXtoS( OrdKey() )        )

   VouPopup( aStats, 'Database Various Info!' )

   RETURN nil


STATIC FUNCTION MyBeforeKeys( oBrw, nLastKey, aBeforeKeys )
   LOCAL n
   LOCAL lRet := .F.

   IF aBeforeKeys <> nil
      n := ascan( aBeforeKeys, {|e_| e_[ 1 ] == nLastKey } )
      IF n > 0
         lRet := .T.
         Eval( aBeforeKeys[ n,2 ], oBrw, nLastKey )
      ENDIF
   ENDIF

   RETURN lRet


STATIC FUNCTION SomeNumbers( oBrw )
   LOCAL xVar, n, nNewVal, cMsg
   LOCAL nHigh   := 0
   LOCAL nLow    := 0
   LOCAL nRecs   := 0
   LOCAL nMode   := 0
   LOCAL nSum    := 0
   LOCAL oColumn := oBrw:GetColumn( oBrw:ColPos )

   xVar := Eval( oColumn:Block )

   oBrw:GoTop()
   oBrw:ForceStable()

   IF ValType( xVar ) == 'N'
      n     := Alert( 'Option', { 'Sum','Average','High','Low' } )
      nMode += n
   ENDIF

   IF .T.
      DO WHILE .T.
         IF nextkey() == 27
            inkey()
            EXIT
         ENDIF
         IF oBrw:hitbottom
            EXIT
         ENDIF
         nRecs++
         IF nMode > 0
            nNewVal := eval( oColumn:Block )
            nSum += nNewVal
            IF nMode > 2
               nHigh := max( nHigh, nNewVal )
               nLow  := min( nLow, nNewVal )
            ENDIF
         ENDIF
         oBrw:down()
         oBrw:ForceStable()
      ENDDO
   ENDIF

   #define ZTRIM( n )  ltrim( str( n,28,0 ) )

   cMsg := 'Rec:'+ ZTRIM( nRecs )
   IF     nMode == 1
      cMsg += '  Sum:'+NTRIM( nSum )
   ELSEIF nMode == 2
      cMsg += '  Avg:'+ZTRIM( nSum/nRecs )
   ELSEIF nMode == 3
      cMsg += '  High:'+NTRIM( nHigh )
   ELSEIF nMode == 4
      cMsg += '  Low:'+NTRIM( nLow )
   ENDIF

   Alert( cMsg )

   RETURN .T.


STATIC FUNCTION PassDecrypt( MyPass )
   LOCAL RmPass := ""
   LOCAL Rmi    := 1

   MyPass = Alltrim( MyPass )

   DO WHILE Rmi <= Len( MyPass )
     RmPass := RmPass + Chr( Asc( Substr( MyPass, Rmi, 1) ) - ( 2*Rmi+1+Rmi%3 ) )
     Rmi++
   ENDDO

   RETURN Alltrim( RmPass )


FUNCTION SetVisualBrowser( v_Yes )
   LOCAL lYes
   THREAD STATIC s_Yes := .F.
   lYes := s_Yes
   IF ValType( v_Yes ) == 'L'
      s_Yes := v_Yes
   ENDIF
   RETURN lYes

/*----------------------------------------------------------------------*/
//                              Array Browser
/*----------------------------------------------------------------------*/

FUNCTION MGR_ArrayBrowse( aData, aInfo, cTitle, aBeforeKeys, lThreaded )
#ifdef __MT__
   DEFAULT lThreaded TO .T.
#else
   DEFAULT lThreaded TO .F.
#endif

   IF lThreaded
      hb_ThreadStart( {|| MyBrowseArray_X( aData, aInfo, cTitle, aBeforeKeys, .T. ) } )
   ELSE
      MyBrowseArray_X( aData, aInfo, cTitle, aBeforeKeys, .F. )
   ENDIF
   RETURN NIL


FUNCTION MyBrowseArray_X( aData, aInfo, cTitle, aBeforeKeys, lThreaded )
   LOCAL oBrw, cType, lContinue, i, nLastKey, d_
   LOCAL org_

   IF ValType( aData ) <> "A"
      RETURN NIL
   ENDIF
   IF Len( aData ) == 0
      RETURN NIL
   ENDIF

   IF lThreaded
      hb_gtReload( 'WVG' )

      Hb_GtInfo( HB_GTI_FONTNAME  , 'Courier New' )
      Hb_GtInfo( HB_GTI_FONTSIZE  , 16 )
      Hb_GtInfo( HB_GTI_FONTWIDTH , 10 )
      Hb_GtInfo( HB_GTI_RESIZEMODE, HB_GTI_RESIZEMODE_ROWS )

      SetCursor( 0 )
      SetColor( 'N/W' )
      CLS
   ENDIF

   org_:= aclone( aData )

   DEFAULT aInfo  TO {}
   DEFAULT cTitle TO 'Array Browser'

   d_:= {}
   IF ValType( aData[ 1 ] ) <> 'A'
      AEval( aData, {|e| AAdd( d_, { e } ) } )
      aData := d_
   ENDIF

   asize( aInfo, Len( aData[ 1 ] ) )
   FOR i := 1 TO Len( aData[ 1 ] )
      cType := ValType( aData[ 1,i ] )

      DEFAULT aInfo[ i ] TO {}
      asize( aInfo[ i ], 6 )

      DEFAULT aInfo[ i,1 ] TO i
      DEFAULT aInfo[ i,2 ] TO 'C_'+NTRIM( i )
      DEFAULT aInfo[ i,3 ] TO cType
      DEFAULT aInfo[ i,4 ] TO iif( cType == 'N', 10, if( cType == 'D',8, ;
                                  iif( cType == 'L', 1, max( 8, Len( aData[ 1,i ] ) ) ) ) )
      DEFAULT aInfo[ i,5 ] TO iif( cType == 'N', 3, 0 )

      DEFAULT aInfo[ i,6 ] TO iif( cType == 'N', '@Z 999999.999', '' )
   NEXT

   oBrw := TBrowseNew( 0,0,MaxRow()-1,MaxCol() )
   oBrw:Cargo         := { aData, 1, aInfo }

   oBrw:SkipBlock     := {|x| ArraySkipper( x,oBrw ) }
   oBrw:GoTopBlock    := {|| oBrw:Cargo[ 2 ] := 1, oBrw:Cargo[ 2 ]  }
   oBrw:GoBottomBlock := {|| oBrw:Cargo[ 2 ] := Len( oBrw:Cargo[ 1 ] ), oBrw:Cargo[ 2 ] }

   oBrw:HeadSep       := Chr( 196 ) + Chr( 194 ) + Chr( 196 )
   oBrw:ColSep        := ' ' + Chr( 179 ) + ' '
   oBrw:ColorSpec     := 'N/W,W+/R,N/W,N/W'

   BuildArrayColumns( oBrw )

   while ( !oBrw:stabilize() ) ; end

   DispStat( cTitle,.T. )

   lContinue := .T.
   DO WHILE lContinue
      DO WHILE ( ( nLastKey := Inkey( ,INKEY_ALL + HB_INKEY_GTEVENT ) ) == 0 ) .and. !( oBrw:Stabilize() )
      ENDDO

      IF nLastKey == 0
         HB_GtInfo( HB_GTI_WINTITLE, '<'+ cTitle +'>  <'+ NTRIM( oBrw:Cargo[ 2 ] ) +'/'+ NTRIM( Len( oBrw:Cargo[ 1 ] ) )+'>' )

         DO WHILE ( nLastKey == 0 )
            nLastKey := Inkey( 0, INKEY_ALL + HB_INKEY_GTEVENT )
         ENDDO
      ENDIF

      DO CASE
      CASE Navigate( oBrw, nLastKey )

      CASE MyBeforeKeys( oBrw, nLastKey, aBeforeKeys )

      CASE nLastKey == K_F1
         DispArBrowseHelp()

      CASE nLastKey == K_F4
         PrintBrw( oBrw, cTitle )

      CASE nLastKey == K_F5
         ExportBrw( oBrw, aInfo )

      CASE nLastkey == K_F6
         SortBrw( oBrw, aData, .T. )

      CASE nLastkey == K_F7
         SortBrw( oBrw, aData, .F. )

      CASE nLastkey == K_SH_F6
         aData := aclone( org_ )
         oBrw:goTop()
         oBrw:refreshAll()
         oBrw:forceStable()

      CASE nLastkey == K_ESC
         lContinue := .F.

      CASE nLastKey == HB_K_RESIZE
         oBrw:nBottom := MaxRow()-1
         oBrw:nRight := MaxCol()
         oBrw:RefreshAll()
         DispStat( cTitle, .T. )

      ENDCASE
   ENDDO

   RETURN NIL


STATIC FUNCTION ArraySkipper( howmany, oBrw )
   LOCAL actual
   LOCAL aLen   := Len( oBrw:Cargo[ 1 ] )
   LOCAL curPos := oBrw:Cargo[ 2 ]

   IF howmany >= 0
      IF ( curpos + howmany ) > alen
         actual := alen - curpos
         curpos := alen
      ELSE
         actual := howmany
         curpos += howmany
      ENDIF
   ELSE
      IF ( curpos + howmany ) < 1
         actual := 1 - curpos
         curpos := 1
      ELSE
         actual := howmany
         curpos += howmany
      ENDIF
   ENDIF

   oBrw:Cargo[ 2 ] := curPos

   RETURN actual


STATIC FUNCTION BuildArrayColumns( oBrw )
   LOCAL i, h, w, oCol
   LOCAL aAttr := oBrw:Cargo[ 3 ]

   FOR i := 1 TO Len( aAttr )
      h := trim( aAttr[ i,2 ] )
      IF aAttr[ i,3 ] == 'M'
         w := 10
      ELSE
         w := aAttr[ i,4 ]
      ENDIF

      IF aAttr[ i,3 ] == 'N'
         h := space( w - Len( h ) ) + h
      ELSE
         h := pad( h,w )
      ENDIF

      oCol         := TbColumnNew( h )
      oCol:cargo   := aAttr[ i ]
      oCol:width   := w
      oCol:picture := aAttr[ i,6 ]

      oCol:Block   := GetArrayBlock( i,oBrw )

      oBrw:AddColumn( oCol )
   NEXT

   RETURN NIL


STATIC FUNCTION GetArrayBlock( nCol,oBrw )
   RETURN {|| oBrw:Cargo[ 1, oBrw:Cargo[ 2 ], oBrw:Cargo[ 3, nCol, 1 ] ] }


STATIC FUNCTION EmptyVrb( xVrb )

   SWITCH ValType( xVrb )
   CASE "N" ; RETURN 0
   CASE "C" ; RETURN space( Len( xVrb ) )
   CASE "L" ; RETURN .F.
   CASE "D" ; RETURN ctod( "" )
   CASE "M" ; RETURN ""
   ENDSWITCH

   RETURN ""


#define CLR_BLACK                                 RGB(   0,  0,  0 )
#define CLR_WHITE                                 RGB( 255,255,255 )
#define CLR_DARKRED                               RGB( 128,  0,  0 )
#define CLR_DARKBLUE                              RGB(   0,  0,128 )
#define CLR_DARKGREEN                             RGB(   0,128,  0 )
#define CLR_DARKMAGENTA                           RGB( 128,  0,128 )
#define CLR_BROWN                                 RGB( 128,128,  0 )
#define CLR_DARKGRAY                              RGB( 128,128,128 )
#define CLR_LIGHTGRAY                             RGB( 225,225,225 )
#define CLR_LIGHTESTGRAY                          RGB( 245,245,245 )
#define CLR_CYAN                                  RGB(   0,128,128 )
#define CLR_BLUE                                  RGB(   0,  0,255 )
#define CLR_RED                                   RGB( 255,  0,  0 )
#define CLR_GRAY                                  RGB( 192,192,192 )
#define CLR_GREEN                                 RGB(   0,255,  0 )
#define CLR_PINK                                  RGB( 255,  0,128 )
#define CLR_MAGENTA                               RGB( 255,  0,255 )
#define CLR_YELLOW                                RGB( 255,255,  0 )
#define CLR_LIGHTCYAN                             RGB(   0,255,255 )
#define CLR_LIGHTYELLOW                           RGB( 255,255,185 )
#define CLR_LIGHTESTYELLOW                        RGB( 255,255,225 )
#define CLR_LIGHTESTGREEN                         RGB( 240,255,255 )
#define CLR_LIGHTESTPINK                          RGB( 255,240,240 )


#define wGridVert                                 1  // 10
#define wHdFirstPg                                2
#define wHdNextPg                                 3
#define wColHdNxt                                 4  // 13
#define wInFirstPg                                5
#define wInNextPg                                 6
#define wPageInfo                                 7  // 16
#define wPgTotals                                 8  // 17
#define wRecurringOff                             9
#define wGrdHorzByRec                             10
#define wPgNumbers                                11
#define wSysDate                                  12  // 21
#define wSysTime                                  13
#define wSerialNos                                14
#define wGrpTotals                                15
#define wColHdFst                                 16  // 25
#define wCopies                                   17  // 29
#define wPgLen                                    18
#define wPgWid                                    19
#define wGapCols                                  20  // 32
#define wBlankRows                                21
#define wBlAfterRows                              22
#define wStartingPg                               23  // 35
#define wIntro1                                   24  // 36
#define wIntro2                                   25  // 37
#define wWaterMark                                26  // 48
#define wPrinter                                  27  // 49
#define wPaper                                    28  // 50
#define wPreview                                  29  // 51
#define wGrdHorzByGrp                             30  // 52
#define wGrayScale                                31  // 53
#define wQuality                                  32  // 54,55,56
#define wOrient                                   33  // 57,58
#define wGrid                                     34  // 59
#define wSaveSettings                             35  // 27
#define wGrdTotals                                36
#define wSummary                                  37
#define wCPI                                      38

#define wCTGrpTotals                              39  // 141
#define wCBGrpTotals                              40
#define wCTPgeTotals                              41
#define wCBPgeTotals                              42
#define wCTGrdTotals                              43
#define wCBGrdTotals                              44  // 146
#define wCTGrpTotals2                             45  // 147
#define wCBGrpTotals2                             46  // 148
#define wCTGrpTotals3                             47  // 149
#define wCBGrpTotals3                             48  // 150

#define wWrap                                     49  // 71
#define wGrpTotals2                               50  // 72
#define wGrpTotals3                               51  // 73

#define wTopMargin                                52  // 81
#define wLeftMargin                               53  // 82
#define wBottomMargin                             54  // 83
#define wRightMargin                              55  // 84

#define wCancelled                                56

#define wCTTitle                                  57  // 151
#define wCTIntro                                  58  // 152
#define wCTInfo                                   59  // 153
#define wCTGrid                                   60  // 154


FUNCTION CreateVouch32( oServer )
   LOCAL cServer  := 'Vouch32.Vouch.1'
   LOCAL cDllName := 'Vouch32x.dll'
   LOCAL lOk      := .T.
   LOCAL cDll

   cDll := hb_dirBase() + cDllName
   IF ! hb_fileExists( cDll )
      Alert( cDll + " does not exists !" )
   ENDIF

   TRY
      oServer := CreateObject( cServer )
   CATCH
      IF WAPI_ShellExecute( NIL, "OPEN", "REGSVR32.EXE", cDll, NIL, 1 ) == 1
         hb_idleSleep( 5 )
         TRY
            oServer := CreateObject( cServer )
         CATCH
            Alert( "ERROR! " + cServer + " not avialable." )
            lOk := .F.
         END
      ENDIF
   END
   RETURN lOk


FUNCTION PrintBrw( oBrw, cTitle )
   LOCAL oVouch32, oRpt, i, oCol, oColBrw, nColumns, dat_
   LOCAL aOptions := {}
   LOCAL nLPI     := 6

   DEFAULT cTitle TO "Browsed Data"

   IF ! CreateVouch32( @oVouch32 )
      Return nil
   ENDIF
   oRpt := oVouch32:Vouch32ReportServer()
   nColumns := oBrw:ColCount

   aOptions := oRpt:DefaultProperties( aOptions )
   AddParameters( oRpt, @aOptions )

   oRpt:cReportTitle := cTitle
   oRpt:AddIntro( cTitle )
   oRpt:nPointSize   := iif( nLPI == 6, 12, iif( nLPI == 8, 9, iif( nLPI == 12, 6, 12 ) ) )

   FOR i := 1 TO nColumns
      oColBrw       := oBrw:GetColumn( i )

      oCol          := oVouch32:Vouch32ReportColumn()

      oCol:Heading  := oColBrw:heading
      oCol:Type     := ValType( eval( oColBrw:block ) )
      oCol:Width    := oColBrw:width
      oCol:Decimals := 0
      oCol:picture  := oColBrw:picture

      oRpt:AddColumn( oCol )
   NEXT

   oRpt:Create()

   IF ! oRpt:aOptions[ wCancelled ]
      IF oRpt:StartReport()

         oBrw:GoTop()
         oBrw:ForceStable()

         DO WHILE .T.
            dat_:={}
            FOR i := 1 TO nColumns
               AAdd( dat_, eval( oBrw:GetColumn( i ):block ) )
            NEXT

            oRpt:Data( dat_ )

            oBrw:Down()
            oBrw:ForceStable()

            IF LastKey() == K_ESC .or. oBrw:hitBottom
               EXIT
            ENDIF
         ENDDO

         oRpt:EndReport()
      ENDIF
   ENDIF

   RETURN NIL


STATIC FUNCTION AddParameters( oRpt, aOptions )

   oRpt:AddTitle( 'Curacao' )
   oRpt:AddTitle( '1605 W. Olympic Blvd #600' )

   aOptions := oRpt:DefaultProperties( aOptions )

   aOptions[ wCancelled    ] := .F.
   aOptions[ wCPI          ] := 6                   // 10 CPI
   aOptions[ wCTTitle      ] := CLR_DARKGREEN
   aOptions[ wCTIntro      ] := CLR_DARKRED
   aOptions[ wCTInfo       ] := CLR_BLUE
   aOptions[ wRecurringOff ] := .T.

   aOptions := oRpt:FetchProperties( aOptions )

   oRpt:nSerialWidth := 5

   oRpt:SetRowColors( { CLR_LIGHTESTGREEN, NIL,CLR_LIGHTESTGRAY,NIL,;
                        CLR_LIGHTESTYELLOW,NIL,CLR_LIGHTESTPINK,NIL } )
   RETURN NIL


STATIC FUNCTION ExportBrw( oBrw, aInfo )
   LOCAL cExportTo := trim( VouGetSome( "Filename .dbf/.csv", space( 50 ) ) )

   IF Empty( cExportTo )
      RETURN NIL
   ENDIF
   ExportAs( oBrw, aInfo, cExportTo )

   RETURN NIL


STATIC FUNCTION ExportAs( oBrw, aAttr, cExportTo )
   LOCAL aStr, d_, nArea, cFType, csv_, s, nCols

   cFType := Upper( Right( cExportTo, 4 ) )

   IF cFType  == ".DBF"
      aStr := {}
      AEval( aAttr, {|e_| AAdd( aStr, { e_[ 2 ], e_[ 3 ], e_[ 4 ], e_[ 5 ] } ) } )
      dbCreate( cExportTo, aStr, "DBFCDX" )
      IF hb_fileExists( cExportTo )
         nArea := select()
         USE ( cExportTo ) EXCLUSIVE ALIAS "SOURCE" NEW
         IF ! Neterr()

            oBrw:goTop()
            oBrw:forceStable()
            d_:= array( Len( aStr ) )
            DO WHILE .T.
               AEval( d_, {|e,i| e:=e, d_[ i ] := eval( oBrw:getColumn( i ):block ) } )
               DbAppend()
               AEval( d_, {|e,i| fieldput( i, e ) } )

               oBrw:down()
               oBrw:forceStable()
               IF oBrw:hitBottom .or. lastkey() == 27
                  EXIT
               ENDIF
            ENDDO

            DbCloseArea()
            Select( nArea )
            Alert( "Results are saved in " + cExportTo )
         ENDIF
      ELSE
         Alert( "Table could not been created " + cExportTo )
      ENDIF

   ELSEIF cFType  == ".CSV"
      IF .T.
         IF .T.
            csv_:={}

            oBrw:goTop()
            oBrw:forceStable()
            nCols := oBrw:colCount
            d_:= array( nCols )
            DO WHILE .T.
               s := ""
               AEval( d_, {|e,j| e:=e, s += X2Csv( eval( oBrw:getColumn( j ):block ), aAttr[ j,3 ] ) + iif( j == nCols, '', ',' ) } )
               AAdd( csv_, s )

               oBrw:down()
               oBrw:forceStable()
               IF oBrw:hitBottom .or. lastkey() == 27
                  EXIT
               ENDIF
            ENDDO

            IF ! Empty( csv_ )
               s := ''
               AEval( aAttr, {|e_| s += e_[ 2 ]+',' } )
               s += CRLF
               AEval( csv_, {|e| s += e + CRLF } )
               MemoWrit( cExportTo, s )
               Alert( "Data is exported to " + cExportTo )
            ELSE
               Alert( "No data is available under these parameters!" )
            ENDIF
         ENDIF
      ENDIF
   ELSE
      Alert( "You have not provided a proper .dbf/.csv file name !" )
   ENDIF
   RETURN NIL


STATIC FUNCTION X2Csv( x, cTyp )
   LOCAL xVar := ''

   DO CASE
   CASE cTyp == 'C'
      IF at( '"', x ) > 0 .OR. at( ',', x ) > 0
         xVar := '"' + StrTran( x, '"', '""' ) + '"'
      ELSEIF IsDigit( left( x,1 ) )
         RETURN '="'+ x +'"'
      ELSE
         RETURN x
      ENDIF
   CASE cTyp == 'N'
      xVar := ltrim( str( x ) )
   CASE cTyp == 'D'
      xVar := dtoc( x )
   CASE cTyp == 'L'
      xVar := if( x, 'Yes','No' )
   ENDCASE

   RETURN xVar


STATIC FUNCTION DispArBrowseHelp()
   LOCAL v_:={}, mnu_:={}
   LOCAL nChoice

   AAdd( v_, { 'Help Screen            F1  ' , K_F1   } )
   AAdd( v_, { 'Print                  F4  ' , K_F4   } )
   AAdd( v_, { "Export Data            F5  " , K_F5   } )
   AAdd( v_, { "Sort Data Ascending    F6  " , K_F6   } )
   AAdd( v_, { "Sort Data Decending    F7  " , K_F7   } )
   AAdd( v_, { "Original Data       Sh+F6  " , K_SH_F6} )

   AEval( v_, {|e_| AAdd( mnu_, e_[ 1 ] ) } )

   nChoice := VouPopup( mnu_, 'Options for Data Browser' )

   IF nChoice > 0
      Wvt_Keyboard( v_[ nChoice,2 ] )
   ENDIF

   RETURN nil


STATIC FUNCTION SortBrw( oBrw, aData, lAsc )
   LOCAL nCol := oBrw:colPos
   IF lAsc
      asort( aData, , , {|e_,f_| e_[ nCol ] < f_[ nCol ] } )
   ELSE
      asort( aData, , , {|e_,f_| e_[ nCol ] > f_[ nCol ] } )
   ENDIF
   oBrw:goTop()
   oBrw:refreshAll()
   oBrw:forceStable()

   RETURN NIL


STATIC FUNCTION BrwSetThisOrder( oBrw, nOrder )

   DbSetOrder( nOrder )
   oBrw:refreshAll()
   oBrw:forceStable()
   DispStat()

   RETURN NIL


STATIC FUNCTION BrwDbGoto( oBrw, nRec )

   DbGoto( nRec )
   oBrw:RefreshAll()
   oBrw:ForceStable()
   DispStat()

   RETURN NIL


STATIC FUNCTION BrwInsertColumn( oBrw, cHeading )
   LOCAL i, lFound := .F.

   cHeading := Upper( Trim( cHeading ) )

   FOR i := 1 TO oBrw:colCount
      IF oBrw:getColumn( i ):heading == cHeading
         lFound := .T.
         EXIT
      ENDIF
   NEXT
   IF lFound
      AddAField( oBrw, i )
      Wvt_Keyboard( HB_K_RESIZE )
   ENDIF

   RETURN NIL


STATIC FUNCTION BrwShowColumn( oBrw, cHeading )
   LOCAL i, j, nCur

   cHeading := Upper( Trim( cHeading ) )

   nCur := oBrw:colPos
   FOR i := 1 TO oBrw:colCount
      IF  oBrw:getColumn( i ):heading() == Right( cHeading, Len( oBrw:getColumn( i ):heading() ) )
         EXIT
      ENDIF
   NEXT
   IF i < nCur
      FOR j := nCur - 1 TO i STEP -1
         oBrw:left()
      NEXT
   ELSEIF i > nCur
      FOR j := nCur + 1 TO i
         oBrw:Right()
      NEXT
   ENDIF
   oBrw:refreshCurrent()
   oBrw:forceStable()

   RETURN NIL


STATIC FUNCTION BrwToggleLeftPanel( oBrw, oCrt )
   LOCAL oCargo := oCrt:cargo

   IF oCargo:lHidden
      oCargo:oTrFlds:show()
      oCargo:oLbIdx:show()
      oCargo:oSNvg:show()
      oCargo:oSIns:show()
      oCargo:oSGo:show()
      oCargo:oEdNvg:show()
      oCargo:oEdIns:show()
      oCargo:oEdGo:show()
   ELSE
      oCargo:oTrFlds:hide()
      oCargo:oLbIdx:hide()
      oCargo:oSNvg:hide()
      oCargo:oSIns:hide()
      oCargo:oSGo:hide()
      oCargo:oEdNvg:hide()
      oCargo:oEdIns:hide()
      oCargo:oEdGo:hide()
   ENDIF

   oCargo:lHidden := ! oCargo:lHidden
   oBrw:nLeft := iif( oCargo:lHidden, 2, 20 )
   oBrw:configure()
   Wvt_keyboard( HB_K_RESIZE )

   RETURN NIL


STATIC FUNCTION BrwBuildComponents( oCrt, oBrw, aStr )
   LOCAL oXbp, i, cKey, a_:={}
   LOCAL oCargo := oCrt:cargo

   BrwBuildToolbar( oCrt, oBrw )

   SetMode( MaxRow()+1, MaxCol()+1 )  /* Neccessary because adding menu has reduced the overall size of window */

   oXbp := WvgScrollBar():new( oCrt, , { {|| -( oBrw:nBottom+1 ) }, {|| -( oBrw:nLeft ) } }, ;
                                              { -1, {|| -( oBrw:nRight - oBrw:nLeft + 1 ) } } )
   oXbp:range := { 1, oBrw:colCount }
   oXbp:type  := WVGSCROLL_HORIZONTAL
   oXbp:create()
   oXbp:scroll := {|mp1| oBrw:colPos := mp1[ 1 ], oBrw:refreshCurrent(), oBrw:forceStable() }
   //
   oCargo:oHBar := oXbp

   oXbp := WvgStatic():new( oCrt )
   oXbp:type    := WVGSTATIC_TYPE_TEXT
   oXbp:options := WVGSTATIC_TEXT_LEFT
   oXbp:caption := "Navigate:"
   oXbp:create( , , { -3, -2 }, { -1, -5 } )
   oXbp:setColorFG( "W+" )
   oXbp:setColorBG( "B" )
   //
   oCargo:oSNvg := oXbp
   //
   oXbp := WvgSLE():new( oCrt )
   oXbp:bufferLength := 10
   oXbp:create( , , { -3, -8 }, { -1, -10 } )
   oXbp:setColorFG( "B" )
   oXbp:setColorBG( "W+" )
   oXbp:returnPressed := {|m1,m2,o| m1 := m2, BrwShowColumn( oBrw, o:getData() ) }
   oXbp:tooltipText := "Type in a field name and press ENTER"
   //
   oCargo:oEdNvg := oXbp

   oXbp := WvgStatic():new( oCrt )
   oXbp:type    := WVGSTATIC_TYPE_TEXT
   oXbp:options := WVGSTATIC_TEXT_LEFT
   oXbp:caption := "Insert:"
   oXbp:create( , , { -4, -2 }, { -1, -5 } )
   oXbp:setColorFG( "W+" )
   oXbp:setColorBG( "B" )
   //
   oCargo:oSIns := oXbp
   //
   oXbp := WvgSLE():new( oCrt )
   oXbp:bufferLength := 10
   oXbp:create( , , { -4, -8 }, { -1, -10 } )
   oXbp:setColorFG( "B" )
   oXbp:setColorBG( "W+" )
   oXbp:returnPressed := {|m1,m2,o| m1 := m2, BrwInsertColumn( oBrw, o:getData() ) }
   oXbp:tooltipText := "Type-in a field name and press ENTER"
   //
   oCargo:oEdIns := oXbp

   oXbp := WvgStatic():new( oCrt )
   oXbp:type    := WVGSTATIC_TYPE_TEXT
   oXbp:options := WVGSTATIC_TEXT_LEFT
   oXbp:caption := "Goto:"
   oXbp:create( , , { -5, -2 }, { -1, -5 } )
   oXbp:setColorFG( "W+" )
   oXbp:setColorBG( "B" )
   //
   oCargo:oSGo := oXbp
   //
   oXbp := WvgSLE():new( oCrt )
   oXbp:bufferLength := 10
   oXbp:create( , , { -5, -8 }, { -1, -10 } )
   oXbp:setColorFG( "B" )
   oXbp:setColorBG( "W+" )
   oXbp:returnPressed := {|m1,m2,o| m1 := m2, BrwDbGoto( oBrw, val( o:getData() ) ) }
   oXbp:tooltipText := "Type-in record number and press ENTER"
   //
   oCargo:oEdGo := oXbp

   /* Indexes Listbox */
   IF ! Empty( alias() )
      FOR i := 1 TO 50
         IF ( cKey := indexkey( i ) ) == ''
            EXIT
         ENDIF
         AAdd( a_, OrdName( i ) + ' : ' + cKey )
      NEXT
   ENDIF
   //
   oXbp := WvgListBox():new( oCrt )
   oXbp:create( , , { -7, -2 }, { -5.1, -16 }, , .T. )
   oXbp:setColorFG( "B" )
   oXbp:setColorBG( "W+" )
   //
   oXbp:addItem( "Natural Order" )
   IF ! Empty( a_ )
      FOR i := 1 TO Len( a_ )
         oXbp:addItem( a_[ i ] )
      NEXT
   ENDIF
   oXbp:tooltipText := "Click to change order"
   IF ! Empty( a_ )
      oXbp:itemMarked := {|m1,m2,o| m1 := m1, m2 := m2, BrwSetThisOrder( oBrw, ascan( a_, o:getCurItem() ) ) }
   ENDIF
   //
   oCargo:oLbIdx := oXbp

   oCargo:oTrFlds := BrwBuildTree( oCrt, oBrw, aStr )

   // Some GUI elements
   Wvg_SetPaint( "Browser", 101, {|| Wvt_DrawBoxRecessed( 3, iif( oCargo:lHidden, 2, 20 ), MaxRow()-2, MaxCol()-2 ) } )
   //
   WvtSetPaint( Wvg_GetPaint( "Browser" ) )

   oCrt:refresh()

   RETURN NIL


STATIC FUNCTION BrwBuildTree( oCrt, oBrw, aStr )
   LOCAL oTree, oItem1, i, oItem2
   LOCAL cRoot := iif( Empty( alias() ), "Data", alias() )

   oTree := WvgTreeView():new( oCrt )
   oTree:hasLines   := .T.
   oTree:hasButtons := .T.
   oTree:alwaysShowSelection := .T.
   oTree:create( , , { -13, -2 }, { {|| -( MaxRow()-1-13 ) }, -16 } )
   oTree:setColorFG( "B" )
   oTree:setColorBG( "W+" )

   oItem1 := oTree:rootItem:addItem( cRoot )

   IF hb_isArray( aStr )
      FOR i := 1 TO Len( aStr )
         oItem2 := oItem1:addItem( StrZero( i, iif( Len( aStr ) > 99, 3, 2 ) ) + " " + aStr[ i, 1 ] )
         oItem2:addItem( "Type: " + aStr[ i,2 ] )
         oItem2:addItem( "Len: "  + hb_ntos( aStr[ i,3 ] ) )
         oItem2:addItem( "Dec: "  + hb_ntos( aStr[ i,4 ] ) )
      NEXT
   ELSE
      FOR i := 1 TO oBrw:colCount
         oItem1:addItem( oBrw:getColumn( i ):heading )
      NEXT
   ENDIF
   oTree:showExpanded( .T., 1 )

   oTree:tooltipText := cRoot + ":" + " Click on a field name to bring in focus"
   oTree:itemMarked := {|oItem| iif( oItem <> NIL, BrwShowColumn( oBrw, oItem:caption ), NIL ) }

   oCrt:setFocus()

   RETURN oTree


STATIC FUNCTION BrwBuildToolBar( oCrt, oBrw )
   LOCAL oTBar, nRGB := RGB( 255,255,255 )

   oTBar := WvgToolBar():new( oCrt, , { -0.1, -3 }, { -1, {|| -( MaxCol() + 1 ) } } )

   oTBar:style        := WVGTOOLBAR_STYLE_FLAT

   oTBar:buttonWidth  := 18
   oTBar:buttonHeight := 14

   oTBar:imageWidth   := 16
   oTBar:imageHeight  := 16

   oTBar:showToolTips := .T.

   oTBar:create()

   oTBar:addItem()
   oTBar:addItem( "Toggle Left Panel"  , "ON_OFF"           , , , , , , nRGB )
   oTBar:addItem()
   oTBar:addItem( "Top"                , "UP_FIRST"         , , , , , , nRGB )
   oTBar:addItem( "Up"                 , "UP"               , , , , , , nRGB )
   oTBar:addItem( "Down"               , "DOWN"             , , , , , , nRGB )
   oTBar:addItem( "Bottom"             , "DOWN_LAST"        , , , , , , nRGB )
   oTBar:addItem()
   oTBar:addItem( "Lock"               , "LOCK"             , , , , , , nRGB )
   oTBar:addItem( "Unlock"             , "UNLOCK"           , , , , , , nRGB )
   oTBar:addItem()
   oTBar:addItem( "Add New Record"     , "ADD"              , , , , , , nRGB )
   oTBar:addItem( "Delete Record"      , "DELETE"           , , , , , , nRGB )
   oTBar:addItem()
   oTBar:addItem( "Print GDI"          , "PRINT"            , , , , , , nRGB )
   oTBar:addItem( "Save View"          , "SAVE"             , , , , , , nRGB )
   oTBar:addItem()
   oTBar:addItem( "Column: Left-most"  , "LEFT_MOST"        , , , , , , nRGB )
   oTBar:addItem( "Column: Left"       , "LEFT"             , , , , , , nRGB )
   oTBar:addItem( "Column: Right"      , "RIGHT"            , , , , , , nRGB )
   oTBar:addItem( "Column: Right-most" , "RIGHT_MOST"       , , , , , , nRGB )
   oTBar:addItem( "Column: Freeze"     , "COLUMN_FREEZE"    , , , , , , nRGB )
   oTBar:addItem( "Column: Unfreeze"   , "COLUMN_UNFREEZE"  , , , , , , nRGB )
   oTBar:addItem( "Column: Move Right" , "COLUMN_RIGHT"     , , , , , , nRGB )
   oTBar:addItem( "Column: Move Left"  , "COLUMN_LEFT"      , , , , , , nRGB )
   oTBar:addItem( "Column: Add"        , "COLUMN_ADD"       , , , , , , nRGB )
   oTBar:addItem( "Column: Delete"     , "COLUMN_DELETE"    , , , , , , nRGB )
   oTBar:addItem( "Column: +Width"     , "INCREASE_WIDTH"   , , , , , , nRGB )
   oTBar:addItem( "Column: -Width"     , "DECREASE_WIDTH"   , , , , , , nRGB )
   oTBar:addItem()

   oTBar:buttonClick := {|oBtn| BrwExecTBarAction( oBtn, oBrw, oCrt ) }

   oCrt:cargo:oTBar := oTBar

   RETURN NIL


STATIC FUNCTION BrwExecTBarAction( oBtn, oBrw, oCrt )

   HB_SYMBOL_UNUSED( oBrw )

   oCrt:setFocus()

   SWITCH oBtn:caption

   CASE "Toggle Left Panel"  ; BrwToggleLeftPanel( oBrw, oCrt )  ; EXIT

   CASE "Down"               ; Wvt_Keyboard( K_DOWN      ) ; EXIT
   CASE "Up"                 ; Wvt_Keyboard( K_UP        ) ; EXIT
   CASE "Bottom"             ; Wvt_Keyboard( K_CTRL_PGDN ) ; EXIT
   CASE "Top"                ; Wvt_Keyboard( K_CTRL_PGUP ) ; EXIT
   CASE "Page-down"          ; Wvt_Keyboard( K_PGDN      ) ; EXIT
   CASE "Page-up"            ; Wvt_Keyboard( K_PGUP      ) ; EXIT

   CASE "Lock"               ; Wvt_Keyboard( K_ALT_L     ) ; EXIT
   CASE "Unlock"             ; Wvt_Keyboard( K_ALT_U     ) ; EXIT

   CASE "Print GDI"          ; PrintBrw( oBrw )            ; EXIT
   CASE "Save View"          ; Wvt_Keyboard( K_F12       ) ; EXIT

   CASE "Add New Record"     ; Wvt_Keyboard( K_ALT_INS   ) ; EXIT
   CASE "Delete Record"      ; Wvt_Keyboard( K_ALT_DEL   ) ; EXIT

   CASE "Column: Left-most"  ; Wvt_Keyboard( K_CTRL_HOME ) ; EXIT
   CASE "Column: Right"      ; Wvt_Keyboard( K_RIGHT     ) ; EXIT
   CASE "Column: Left"       ; Wvt_Keyboard( K_LEFT      ) ; EXIT
   CASE "Column: Right-most" ; Wvt_Keyboard( K_CTRL_END  ) ; EXIT
   CASE "Column: Freeze"     ; Wvt_Keyboard( K_F8        ) ; EXIT
   CASE "Column: Unfreeze"   ; Wvt_Keyboard( K_SH_F8     ) ; EXIT
   CASE "Column: Move Right" ; Wvt_Keyboard( K_SH_F5     ) ; EXIT
   CASE "Column: Move Left"  ; Wvt_Keyboard( K_SH_F6     ) ; EXIT
   CASE "Column: Add"        ; Wvt_Keyboard( K_ALT_F5    ) ; EXIT
   CASE "Column: Delete"     ; Wvt_Keyboard( K_ALT_F6    ) ; EXIT
   CASE "Column: +Width"     ; Wvt_Keyboard( K_CTRL_RIGHT ) ; EXIT
   CASE "Column: -Width"     ; Wvt_Keyboard( K_CTRL_LEFT  ) ; EXIT

   ENDSWITCH

   RETURN NIL


FUNCTION WVT_Paint()
   WvtPaintObjects()
   RETURN NIL

//--------------------------------------------------------------------//
//                          CLASS CrtObjects
//--------------------------------------------------------------------//

#include "hbclass.ch"

CLASS CrtObjects

   DATA oCrt
   DATA lHidden     INIT .F.
   DATA oTrFlds, oLbIdx, oSNvg, oSIns, oSGo, oEdNvg, oEdIns, oEdGo, oHBar, oTBar

   METHOD new( oCrt )

   ENDCLASS


METHOD CrtObjects:new( oCrt )
   ::oCrt := oCrt
   RETURN Self

