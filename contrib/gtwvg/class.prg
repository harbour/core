/*
 * Wvt*Classes
 *
 * Copyright 2007-2012 Pritpal Bedi <bedipritpal@hotmail.com>
 * Based On:
 * Video subsystem for Windows using GUI windows instead of Console
 *     Copyright 2003 Peter Rees <peter@rees.co.nz>
 *                    Rees Software and Systems Ltd
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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
#include "hbgtinfo.ch"
#include "inkey.ch"
#include "setcurs.ch"

#include "wvtwin.ch"

#define K_LBUTTONPRESSED        1021
#define K_RBUTTONPRESSED        1022
#define K_MBUTTONPRESSED        1023

#define K_SBLINEUP              1051
#define K_SBLINEDOWN            1052
#define K_SBPAGEUP              1053
#define K_SBPAGEDOWN            1054

#define K_SBLINELEFT            1055
#define K_SBLINERIGHT           1056
#define K_SBPAGELEFT            1057
#define K_SBPAGERIGHT           1058

#define K_SBTHUMBTRACKVERT      1059
#define K_SBTHUMBTRACKHORZ      1060

#define OBJ_CHILD_OBJ             1
#define OBJ_CHILD_EVENTS          2
#define OBJ_CHILD_DATABLOCK       3
#define OBJ_CHILD_REFRESHBLOCK    4

CREATE CLASS WvtDialog

   /* To hold previous settings */
   VAR    nOldRows
   VAR    nOldCols
   VAR    aOldFont
   VAR    cOldTitle
   VAR    cOldColor
   VAR    nOldCursor
   VAR    aPalette
   VAR    cScreen
   VAR    aWvtScreen
   VAR    aOldPnt
   VAR    oldTooltipActive
   VAR    oldTooltipWidth
   VAR    oldTooltipBkColor
   VAR    oldTooltipTextColor
   VAR    oldMenuHandle
   VAR    oldMenuBlock
   VAR    lGui

   /* Dialog parameters */
   VAR    nRows
   VAR    nCols
   VAR    cFont
   VAR    nFontHeight
   VAR    nFontWidth
   VAR    nFontBold
   VAR    nFontQuality
   VAR    cTitle
   VAR    cColor

   /* Objects handling */
   VAR    aObjects                                INIT {}
   VAR    oCurObj
   VAR    oLastObj
   VAR    oObjOver
   VAR    oLastOver
   VAR    nCurObj                                 INIT 1
   VAR    nLastObj                                INIT 0
   VAR    nObjOver                                INIT 0
   VAR    nLastOver                               INIT -1
   VAR    nUseObj
   VAR    oMenu
   VAR    aDialogKeys                             INIT {}
   VAR    cDialogID                               INIT ""

   /* Tooltip Management */
   VAR    nTooltipWidth
   VAR    nTooltipBkColor
   VAR    nTooltipTextColor

   /* Miscellaneous */
   VAR    ClassName                               INIT "WVTDIALOG"
   VAR    cPaintBlockID
   VAR    nPaintID                                INIT 1
   VAR    nObjID                                  INIT 5000
   VAR    nKey
   VAR    hFonts                                  INIT {}
   VAR    lEventHandled
   VAR    lTabStops                               INIT .F.
   VAR    bOnCreate

   ACCESS nObjects                                INLINE Len( ::aObjects )

   METHOD New( nRows, nCols, cTitle, cFont, nFontHeight, nFontWidth, nFontBold, nFontQuality )
   METHOD create()
   METHOD Destroy()
   METHOD Event()
   METHOD Execute()
   METHOD Inkey()
   METHOD MouseOver()
   METHOD Update()
   METHOD CreateObjects()
   METHOD Eval( bBlock, p1, p2, p3, p4, p5 )
   METHOD ActivateMenu()

   METHOD AddObject( oObject )                    INLINE AAdd( ::aObjects, oObject )
   METHOD MaxRow()                                INLINE ::nRows - 1
   METHOD MaxCol()                                INLINE ::nCols - 1
   METHOD OnTimer()                               INLINE AEval( ::aObjects, {| o | o:OnTimer() } )

ENDCLASS

METHOD WvtDialog:New( nRows, nCols, cTitle, cFont, nFontHeight, nFontWidth, nFontBold, nFontQuality )

   LOCAL fnt_ := wvt_GetFontInfo()

   __defaultNIL( @nRows        , 25 )
   __defaultNIL( @nCols        , 80 )
   __defaultNIL( @cTitle       , wvt_GetTitle() )
   __defaultNIL( @cFont        , fnt_[ 1 ] )
   __defaultNIL( @nFontHeight  , fnt_[ 2 ] )
   __defaultNIL( @nFontWidth   , fnt_[ 3 ] )
   __defaultNIL( @nFontBold    , fnt_[ 4 ] )
   __defaultNIL( @nFontQuality , fnt_[ 5 ] )

   IF Empty( cFont )
      cFont := fnt_[ 1 ]
   ENDIF
   IF Empty( nFontHeight )
      nFontHeight := fnt_[ 2 ]
   ENDIF
   IF Empty( nFontWidth )
      nFontWidth := fnt_[ 3 ]
   ENDIF

   ::nOldRows            := MaxRow() + 1
   ::nOldCols            := MaxCol() + 1
   ::aOldFont            := wvt_GetFontInfo()
   ::cOldTitle           := wvt_GetTitle()
   ::cOldColor           := SetColor()
   ::nOldCursor          := SetCursor()
   ::aPalette            := wvt_GetPalette()

   ::oldMenuHandle       := wvt_GetMenu()
   ::oldMenuBlock        := SetKey( wvt_SetMenuKeyEvent() )

   ::oldTooltipWidth     := wvt_GetToolTipWidth()
   ::oldTooltipBkColor   := wvt_GetToolTipBkColor()
   ::oldTooltipTextColor := wvt_GetToolTipTextColor()

   ::nRows               := nRows
   ::nCols               := nCols
   ::cTitle              := cTitle
   ::cFont               := cFont
   ::nFontHeight         := nFontHeight
   ::nFontWidth          := nFontWidth
   ::nFontBold           := nFontBold
   ::nFontQuality        := nFontQuality

   ::cPaintBlockID       := StrZero( hb_Random( 99999998 ), 8 )
   ::nObjOver            := 0
   ::nKey                := 0
   ::cColor              := "N/W"
   ::nUseObj             := 0
   ::lGui                := wvt_SetGUI( .F. )

   RETURN Self

METHOD WvtDialog:Create()

   LOCAL aPalette, i, j

   ::oldToolTipActive := wvt_SetToolTipActive( .T. )
   IF ::nTooltipWidth != NIL
      wvt_SetToolTipWidth( ::nTooltipWidth )
   ENDIF
   IF ::nTooltipBkColor != NIL
      wvt_SetToolTipBkColor( ::nTooltipBkColor )
   ENDIF
   IF ::nTooltipTextColor != NIL
      wvt_SetToolTipTextColor( ::nTooltipTextColor )
   ENDIF

   aPalette      := wvt_GetPalette()
   aPalette[ 9 ] := RGB( 175, 175, 175 )
   wvt_SetPalette( aPalette )

   ::cScreen     := SaveScreen( 0, 0, MaxRow(), MaxCol() )
   ::aWvtScreen  := wvt_SaveScreen( 0, 0, MaxRow(), MaxCol() )
   ::aOldPnt     := WvtSetPaint( {} )

   SetMode( ::nRows, ::nCols )
   DO WHILE .T.
      IF wvt_SetFont( ::cFont, ::nFontHeight, ::nFontWidth, ::nFontBold, ::nFontQuality )
         EXIT
      ENDIF
      ::nFontHeight--
   ENDDO
#if 0
   wvt_SetFont( ::cFont, ::nFontHeight, ::nFontWidth, ::nFontBold, ::nFontQuality )
#endif
   SetMode( ::nRows, ::nCols )

   wvt_SetTitle( ::cTitle )

   SetColor( ::cColor )
   CLS
   ::Eval( ::bOnCreate )

   ::CreateObjects()

   IF Len( ::aObjects ) > 0
      ::oCurObj := ::aObjects[ 1 ]
   ENDIF

   FOR i := 1 TO Len( ::aObjects )
      IF ! Empty( ::aObjects[ i ]:aPaint )
         FOR j := 1 TO Len( ::aObjects[ i ]:aPaint )
            wvg_SetPaint( ::cPaintBlockID, ::nPaintID++, ;
               ::aObjects[ i ]:aPaint[ j ][ 1 ], ::aObjects[ i ]:aPaint[ j ][ 2 ] )
         NEXT
      ENDIF
   NEXT
   WvtSetPaint( wvg_GetPaint( ::cPaintBlockID ) )

   IF AScan( ::aObjects, {| o | o:lTabStop } ) > 0
      ::lTabStops := .T.
   ENDIF

   ::Update()

   IF HB_ISOBJECT( ::oMenu )
      wvt_SetMenu( ::oMenu:hMenu )
      wvt_DrawMenuBar()
      SetKey( wvt_SetMenuKeyEvent(), {|| ::ActivateMenu( ::oMenu ) } )
   ENDIF

   RETURN Self

METHOD PROCEDURE WvtDialog:Destroy()

   IF HB_ISOBJECT( ::oMenu )
      ::oMenu:Destroy()
   ENDIF

   AEval( ::aObjects, {| o | o:destroy() } )

   wvt_SetToolTip( 0, 0, 0, 0, "" )
   wvt_SetToolTipActive( ::oldToolTipActive )
   wvt_SetToolTipWidth( ::oldTooltipWidth )
   wvt_SetToolTipBkColor( ::oldTooltipBkColor )
   wvt_SetToolTipTextColor( ::oldTooltipTextColor )

   /* Here set mode is before setting the font */
   SetMode( ::nOldRows, ::nOldCols )
   wvt_SetFont( ::aOldFont[ 1 ], ::aOldFont[ 2 ], ::aOldFont[ 3 ], ::aOldFont[ 4 ], ::aOldFont[ 5 ] )
   wvt_SetTitle( ::cOldTitle )
   wvt_SetPalette( ::aPalette )
   wvt_SetPointer( WVT_IDC_ARROW )
   wvt_SetMousePos( MRow(), MCol() )

   SetColor( ::cOldColor )
   SetCursor( ::nOldCursor )

   IF ::oldMenuHandle != NIL .AND. ::oldMenuHandle != 0
      wvt_SetMenu( ::oldMenuHandle )
   ENDIF
   SetKey( wvt_SetMenuKeyEvent(), ::oldMenuBlock )
   RestScreen( 0, 0, MaxRow(), MaxCol(), ::cScreen )
   wvt_RestScreen( 0, 0, MaxRow(), MaxCol(), ::aWvtScreen )
   wvg_PurgePaint( ::cPaintBlockID )
   WvtSetPaint( ::aOldPnt )
   wvt_SetGUI( ::lGui )

   RETURN

METHOD WvtDialog:Event()

   LOCAL nKey

   IF ( nKey := Inkey( 0.1, INKEY_ALL + HB_INKEY_GTEVENT ) ) == 0
      IF wvt_IsLButtonPressed()
         nKey := K_LBUTTONPRESSED
      ENDIF
   ENDIF

   RETURN nKey

METHOD WvtDialog:Execute()

   IF ::nObjects == 0
      DO WHILE .T.
         IF Inkey( 0.1, hb_bitOr( INKEY_ALL, HB_INKEY_GTEVENT ) ) == K_ESC
            EXIT
         ENDIF
      ENDDO
   ELSE
      DO WHILE ::Inkey() != K_ESC
      ENDDO
   ENDIF

   RETURN ::nKey

METHOD WvtDialog:Inkey()

   LOCAL n, oObj, nID, i

   ::lEventHandled := .F.
   ::nUseObj       := 0

   ::nKey := ::Event()
   ::OnTimer()

   IF ::nKey != 0
      IF ::nKey == K_ESC .OR. ::nKey == K_CTRL_ENTER
         RETURN K_ESC
      ENDIF

      DO CASE

      CASE ::nKey == K_TAB
         IF ::lTabStops
            DO WHILE .T.
               ::nCurObj++
               IF ::nCurObj > ::nObjects
                  ::nCurObj := 1
               ENDIF
               IF ::aObjects[ ::nCurObj ]:lTabStop
                  EXIT
               ENDIF
            ENDDO
         ENDIF

         ::lEventHandled := .T.

      CASE ::nKey == K_SH_TAB
         IF ::lTabStops
            DO WHILE .T.
               ::nCurObj--
               IF ::nCurObj < 1
                  ::nCurObj := ::nObjects
               ENDIF
               IF ::aObjects[ ::nCurObj ]:lTabStop
                  EXIT
               ENDIF
            ENDDO
         ENDIF

         ::lEventHandled := .T.

      CASE ::nKey == K_MOUSEMOVE .OR. ::nKey == K_MMLEFTDOWN
         ::MouseOver()
         IF ::nObjOver == 0
            wvt_SetPointer( WVT_IDC_ARROW )
         ELSEIF ::oObjOver:nPointer != NIL .AND. ::oObjOver:lActive
            wvt_SetPointer( ::oObjOver:nPointer )
         ELSE
            wvt_SetPointer( WVT_IDC_ARROW )
         ENDIF
         ::lEventHandled := .T.

      ENDCASE

      IF ::nKey == K_LBUTTONDOWN    .OR. ;
         ::nKey == K_LBUTTONUP      .OR. ;
         ::nKey == K_LDBLCLK        .OR. ;
         ::nKey == K_MMLEFTDOWN     .OR. ;
         ::nKey == K_LBUTTONPRESSED .OR. ;
         ::nKey == K_RBUTTONDOWN

         ::MouseOver()

         IF ::nObjOver > 0
            IF ::aObjects[ ::nObjOver ]:nType == DLG_OBJ_BUTTON     .OR. ;
               ::aObjects[ ::nObjOver ]:nType == DLG_OBJ_TOOLBAR    .OR. ;
               ::aObjects[ ::nObjOver ]:nType == DLG_OBJ_PUSHBUTTON .OR. ;
               ::aObjects[ ::nObjOver ]:nType == DLG_OBJ_SCROLLBAR

               oObj := ::aObjects[ ::nObjOver ]
               IF oObj:oParent:className() == "WVTBROWSE"
                  nID := oObj:oParent:nID
                  IF ( n := AScan( ::aObjects, {| o | o:nID == nID } ) ) > 0
                     ::nCurObj := n
                  ENDIF
               ENDIF
            ELSE
               ::nCurObj := ::nObjOver
            ENDIF
            ::nUseObj := ::nObjOver
         ELSE
            ::lEventHandled := .T.
         ENDIF
      ENDIF

      IF ::nLastOver != ::nObjOver
         IF ::nLastOver > 0
            ::aObjects[ ::nLastOver ]:HoverOff()
         ENDIF

         ::nLastOver := ::nObjOver

         IF ::nObjOver > 0
            ::oObjOver:HoverOn()
         ENDIF

         IF ::nObjOver == 0
            wvt_SetToolTip( 0, 0, 0, 0, "" )
         ELSEIF ::oObjOver:lActive
            ::oObjOver:SetTooltip()
         ELSE
            wvt_SetToolTip( 0, 0, 0, 0, "" )
         ENDIF
      ENDIF

      IF ::nCurObj != ::nLastObj
         IF ::nLastObj == 0
            ::aObjects[ ::nCurObj  ]:Hilite()
         ELSE
            ::aObjects[ ::nLastObj ]:DeHilite()
            ::aObjects[ ::nCurObj  ]:Hilite()
         ENDIF

         ::nLastObj := ::nCurObj
         ::oCurObj  := ::aObjects[ ::nCurObj ]
         ::oLastObj := ::aObjects[ ::nCurObj ]

         IF ::oCurObj:nType == DLG_OBJ_BROWSE
            dbSelectArea( ::oCurObj:cAlias )
         ENDIF

         ::Eval( ::oCurObj:bOnFocus, ::oCurObj )
      ENDIF

      IF ::nKey == K_LBUTTONDOWN
         IF ::nUseObj > 0
            IF !( ::lEventHandled := ::aObjects[ ::nUseObj ]:LeftDown() )
               ::lEventHandled := ::Eval( ::aObjects[ ::nUseObj ]:bOnLeftDown )
               IF ::aObjects[ ::nUseObj ]:className() == "WVTBROWSE"
                  ::lEventHandled := .F.
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      IF ::nKey == K_LBUTTONUP
         IF ::nUseObj > 0
            IF !( ::lEventHandled := ::aObjects[ ::nUseObj ]:LeftUp() )
               ::lEventHandled := ::Eval( ::aObjects[ ::nUseObj ]:bOnLeftUp )
            ENDIF
         ENDIF
      ENDIF

      IF ::nKey == K_MMLEFTDOWN
         IF ::nUseObj > 0
            IF !( ::lEventHandled := ::aObjects[ ::nUseObj ]:MMLeftDown() )
               ::lEventHandled := ::Eval( ::aObjects[ ::nUseObj ]:bOnMMLeftDown )
            ENDIF
         ENDIF
      ENDIF

      IF ::nKey == K_LBUTTONPRESSED
         IF ::nUseObj > 0
            IF !( ::lEventHandled := ::aObjects[ ::nUseObj ]:LeftPressed() )
               ::lEventHandled := ::Eval( ::aObjects[ ::nUseObj ]:bOnLeftPressed )
            ENDIF
         ENDIF
      ENDIF

      IF ::nKey == K_LDBLCLK
         IF ::nUseObj > 0
            ::lEventHandled := ::Eval( ::aObjects[ ::nUseObj ]:bOnSelect )
         ENDIF
      ENDIF

      IF ::nKey == K_RBUTTONDOWN .AND. ::nUseObj > 0
         ::lEventHandled := ::aObjects[ ::nUseObj ]:ShowPopup()
      ENDIF

      IF ! ::lEventHandled
         IF ::nCurObj > 0
            IF ! Empty( ::aDialogKeys )
               IF ( n := AScan( ::aDialogKeys, {| e_ | e_[ 1 ] == ::nKey } ) ) > 0
                  Eval( ::aDialogKeys[ n ][ 2 ], Self, ::oCurObj )
               ENDIF
            ENDIF

            ::lEventHandled := ::oCurObj:HandleEvent( ::nKey )

            IF ::lEventHandled
               IF ::oCurObj:nChildren > 0
                  FOR i := 1 to ::oCurObj:nChildren
                     IF AScan( ::oCurObj:aChildren[ i ][ OBJ_CHILD_EVENTS ], ::nKey ) > 0
                        ::oCurObj:NotifyChild( i, ::nKey, ::oCurObj )
                     ENDIF
                  NEXT
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      IF ! ::lEventHandled
         IF HB_ISBLOCK( SetKey( ::nKey ) )
            Eval( SetKey( ::nKey ) )
         ENDIF
      ENDIF
   ENDIF

   RETURN ::nKey

METHOD WvtDialog:MouseOver()

   LOCAL mRow := MRow()
   LOCAL mCol := MCol()
   LOCAL nObj

   nObj := AScan( ::aObjects, ;
      {| o | o:nType != DLG_OBJ_STATIC               .AND. ;
      o:nType != DLG_OBJ_TOOLBAR              .AND. ;
      mRow >= o:nTop  .AND. mRow <= o:nBottom .AND. ;
      mCol >= o:nLeft .AND. mCol <= o:nRight      } )

   ::nObjOver := nObj
   ::oObjOver := iif( nObj > 0, ::aObjects[ nObj ], NIL )
   IF nObj > 0
      ::aObjects[ nObj ]:nmRow := mRow
      ::aObjects[ nObj ]:nmCol := mCol
   ENDIF

   RETURN Self

METHOD WvtDialog:Update()

   wvt_InvalidateRect( 0, 0, ::MaxRow(), ::MaxCol() )

   RETURN Self

METHOD WvtDialog:CreateObjects()

   LOCAL i, nObjs

   nObjs := Len( ::aObjects )

   FOR i := 1 TO nObjs
      SWITCH ::aObjects[ i ]:nType

      CASE DLG_OBJ_BROWSE
         ::aObjects[ i ]:Create()
         EXIT
      CASE DLG_OBJ_STATUSBAR
         ::aObjects[ i ]:Create()
         EXIT
      CASE DLG_OBJ_LABEL
         ::aObjects[ i ]:Create()
         EXIT
      CASE DLG_OBJ_TOOLBAR
         ::aObjects[ i ]:Create()
         EXIT
      CASE DLG_OBJ_BUTTON
         ::aObjects[ i ]:Create()
         EXIT
      CASE DLG_OBJ_PUSHBUTTON
         ::aObjects[ i ]:Create()
         EXIT
      CASE DLG_OBJ_IMAGE
         ::aObjects[ i ]:Create()
         EXIT
      CASE DLG_OBJ_STATIC
         ::aObjects[ i ]:Create()
         EXIT
#if 0
      CASE DLG_OBJ_SCROLLBAR
         ::aObjects[ i ]:Create()
         EXIT
#endif
      CASE DLG_OBJ_GETS
         ::aObjects[ i ]:Create()
         EXIT
      CASE DLG_OBJ_BANNER
         ::aObjects[ i ]:Create()
         EXIT
      CASE DLG_OBJ_TEXTBOX
         ::aObjects[ i ]:Create()
         EXIT
      CASE DLG_OBJ_PROGRESSBAR
         ::aObjects[ i ]:Create()
         EXIT
      ENDSWITCH
   NEXT

   RETURN Self

METHOD WvtDialog:Eval( bBlock, p1, p2, p3, p4, p5 )

   LOCAL lRet

   IF ( lRet := HB_ISBLOCK( bBlock ) )
      Eval( bBlock, p1, p2, p3, p4, p5 )
   ENDIF

   RETURN lRet

METHOD WvtDialog:ActivateMenu()

   LOCAL nMenu := wvt_GetLastMenuEvent()
   LOCAL aMenuItem

   IF ! Empty( nMenu )
      IF HB_ISOBJECT( ::oMenu )
         IF ! Empty( aMenuItem := ::oMenu:FindMenuItemById( nMenu ) )
            IF HB_ISBLOCK( aMenuItem[ WVT_MENU_ACTION ] )
               Eval( aMenuItem[ WVT_MENU_ACTION ] )
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   RETURN Self

/*
 * Class WvtObject
 *
 * Must never be used directly. It is parent class FOR all other objects!
 */
CREATE CLASS WvtObject

   VAR    oParent
   VAR    nType
   VAR    nId

   VAR    nTop
   VAR    nLeft
   VAR    nBottom
   VAR    nRight
   VAR    aPxlTLBR                                INIT {}

   VAR    aObjects                                INIT {}
   VAR    aParent                                 INIT {}
   VAR    aChildren                               INIT {}
   VAR    aPaint                                  INIT {}
   VAR    bPaint
   VAR    ClassName                               INIT ""

   VAR    nObjID                                  INIT 900000
   VAR    nPointer
   VAR    cargo
   VAR    xSettings
   VAR    cText
   VAR    cToolTip
   VAR    lActive                                 INIT .T.
   VAR    lAnimate                                INIT .F.
   VAR    lTabStop                                INIT .T.
   VAR    hFont

   VAR    aPopup                                  INIT {}
   VAR    hPopup                                  INIT NIL
   VAR    nPopupItemID                            INIT 700000

   VAR    nMRow                                   INIT 0
   VAR    nMCol                                   INIT 0
   VAR    cColorHilite                            INIT "W+/B*"
   VAR    cColorDehilite                          INIT "W/N*"

   VAR    nTextColor
   VAR    nBackColor
   VAR    nBackMode                               INIT 0 /* OPAQUE 1-TRANSPARENT */
   VAR    nTextColorHoverOn
   VAR    nTextColorHoverOff
   VAR    nBackColorHoverOn
   VAR    nBackColorHoverOff
   VAR    cFont
   VAR    nFontHeight
   VAR    nFontWidth
   VAR    nFontWeight
   VAR    nFontQuality
   VAR    nCharSet
   VAR    lItalic
   VAR    lUnderline
   VAR    lStrikeOut
   VAR    nAlignHorz
   VAR    nAlignVert
   VAR    nAngle

   ACCESS ToolTip                                 INLINE iif( ::cTooltip == NIL, "", ::cTooltip )
   ASSIGN ToolTip( cTip )                         INLINE ::cToolTip := cTip

   VAR    bHandleEvent
   VAR    bOnCreate                               INIT {|| NIL }
   VAR    bOnSelect                               INIT {|| NIL }
   VAR    bOnFocus                                INIT {|| NIL }
   VAR    bOnRefresh                              INIT {|| NIL }
   VAR    bOnLeftUp                               INIT {|| NIL }
   VAR    bOnLeftDown                             INIT {|| .F. }
   VAR    bOnMMLeftDown                           INIT {|| NIL }
   VAR    bOnLeftPressed                          INIT {|| NIL }
   VAR    bTooltip                                INIT {|| NIL }
   VAR    bSaveSettings                           INIT {|| NIL }
   VAR    bRestSettings                           INIT {|| NIL }
   VAR    bOnHilite                               INIT {|| NIL }
   VAR    bOnDeHilite                             INIT {|| NIL }

   ACCESS nChildren                               INLINE Len( ::aChildren )
   VAR    nIndexOrder

   METHOD New( oParent, nType, nID, nTop, nLeft, nBottom, nRight )
   METHOD create()
   METHOD Destroy()
   METHOD CreatePopup()
   METHOD ShowPopup()

   METHOD SetToolTip()                            INLINE wvt_SetToolTip( ::nTop, ::nLeft, ::nBottom, ::nRight, ::Tooltip )
   METHOD Refresh()                               INLINE wvt_InvalidateRect( ::nTop, ::nLeft, ::nTop, ::nLeft )
   METHOD Eval( bBlock )                          INLINE iif( HB_ISEVALITEM( bBlock ), Eval( bBlock, Self ), NIL )
   METHOD AddChild( aChild )                      INLINE AAdd( ::aChildren, aChild )
   METHOD AddParent( aParent )                    INLINE AAdd( ::aParent, aParent )

   METHOD PaintBlock()                            INLINE NIL
   METHOD Hilite()                                INLINE NIL
   METHOD DeHilite()                              INLINE NIL
   METHOD HandleEvent()                           INLINE .F.
   METHOD LeftDown()                              INLINE .F.
   METHOD LeftUp()                                INLINE .F.
   METHOD MMLeftDown()                            INLINE .F.
   METHOD LeftPressed()                           INLINE .F.
   METHOD HoverOn()                               INLINE NIL
   METHOD HoverOff()                              INLINE NIL
   METHOD OnTimer()                               INLINE NIL
   METHOD SaveSettings()                          INLINE NIL
   METHOD RestSettings()                          INLINE NIL
   METHOD Activate()                              INLINE NIL
   METHOD DeActivate()                            INLINE NIL
   METHOD NotifyChild( /* nChild */ )             INLINE NIL

ENDCLASS

METHOD WvtObject:New( oParent, nType, nID, nTop, nLeft, nBottom, nRight )

   IF ! HB_ISNUMERIC( nID )
      nID := ++::nObjID
   ENDIF

   ::oParent   :=  oParent
   ::nType     :=  nType
   ::nId       :=  nID
   ::nTop      :=  nTop
   ::nLeft     :=  nLeft
   ::nBottom   :=  nBottom
   ::nRight    :=  nRight

   SWITCH nType

   CASE DLG_OBJ_BROWSE
      ::ClassName := "WVTBROWSE"
      EXIT

   CASE DLG_OBJ_STATIC
      ::ClassName := "WVTSTATIC"
      ::lTabStop  := .F.
      EXIT

   CASE DLG_OBJ_GETS
      ::ClassName := "WVTGETS"
      EXIT

   CASE DLG_OBJ_IMAGE
      ::ClassName := "WVTIMAGE"
      ::lTabStop  := .F.
      EXIT

   CASE DLG_OBJ_PUSHBUTTON
      ::ClassName := "WVTPUSHBUTTON"
      EXIT

   CASE DLG_OBJ_BUTTON
      ::ClassName := "WVTBUTTON"
      ::lTabStop  := .F.
      EXIT

   CASE DLG_OBJ_TOOLBAR
      ::ClassName := "WVTTOOLBAR"
      ::lTabStop  := .F.
      EXIT

   CASE DLG_OBJ_LABEL
      ::ClassName := "WVTLABEL"
      ::lTabStop  := .F.
      EXIT

   CASE DLG_OBJ_SCROLLBAR
      ::ClassName := "WVTSCROLLBAR"
      ::lTabStop  := .F.
      EXIT

   CASE DLG_OBJ_STATUSBAR
      ::ClassName := "WVTSTATUSBAR"
      ::lTabStop  := .F.
      EXIT

   CASE DLG_OBJ_BANNER
      ::ClassName := "WVTBANNER"
      ::lTabStop  := .F.
      EXIT

   CASE DLG_OBJ_TEXTBOX
      ::ClassName := "WVTTEXTBOX"
      ::lTabStop  := .F.
      EXIT

   CASE DLG_OBJ_PROGRESSBAR
      ::ClassName := "WVTPROGRESSBAR"
      ::lTabStop  := .F.
      EXIT

   ENDSWITCH

   RETURN Self

METHOD WvtObject:Create()

   ::Eval( ::bOnCreate )
   ::CreatePopup()

   RETURN Self

METHOD WvtObject:Destroy()

   IF ::hFont != NIL
      wvg_DeleteObject( ::hFont )
      ::hFont := NIL
   ENDIF

   IF ::hPopup != NIL
      wvt_DestroyMenu( ::hPopup )
      ::hPopup := NIL
   ENDIF

   RETURN NIL

METHOD WvtObject:CreatePopup()

   LOCAL i, nID

   IF ! Empty( ::aPopup ) .AND. ::hPopup == NIL
      ::hPopup := wvt_CreatePopupMenu()

      FOR i := 1 TO Len( ::aPopup )

         ASize( ::aPopup[ i ], 3 )
         nID := ::nPopupItemID++
         ::aPopup[ i ][ 3 ] := nID

         wvt_AppendMenu( ::hPopup, MF_ENABLED + MF_STRING, nID, ::aPopup[ i ][ 1 ] )
      NEXT
   ENDIF

   RETURN Self

METHOD WvtObject:ShowPopup()

   LOCAL lRet := .F., nRet, n, aPos

   IF ::hPopup != NIL
      aPos := wvt_GetCursorPos()

      nRet := wvt_TrackPopupMenu( ::hPopup, TPM_CENTERALIGN + TPM_RETURNCMD, ;
         aPos[ 1 ], aPos[ 2 ], 0, wvt_GetWindowHandle() )
      IF nRet > 0
         IF ( n := AScan( ::aPopup, {| e_ | e_[ 3 ] == nRet } ) ) > 0
            lRet := .T.

            IF HB_ISBLOCK( ::aPopup[ n ][ 2 ] )
               Eval( ::aPopup[ n ][ 2 ] )
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   RETURN lRet

/* Class WvtBrowse */
CREATE CLASS WvtBrowse INHERIT WvtObject

   VAR    cAlias
   VAR    oBrw
   VAR    lHSBar                                  INIT .T.
   VAR    lVSBar                                  INIT .T.
   VAR    oHBar
   VAR    oVBar
   VAR    bTotalRecords
   VAR    bCurrentRecord
   VAR    bTotalColumns
   VAR    bCurrentColumn

   ACCESS cDesc                                   INLINE iif( ::cText == NIL, "", ::cText )
   ASSIGN cDesc( cText )                          INLINE ::cText := cText

   METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight )
   METHOD create()
   METHOD PaintBlock( nPaintObj )
   METHOD Hilite()
   METHOD DeHilite()
   METHOD HandleEvent( nKey )
   METHOD Refresh()
   METHOD SetVBar()
   METHOD SetHBar()
   METHOD SetTooltip()
   METHOD SaveSettings()
   METHOD RestSettings()
   METHOD NotifyChild( nIndex, nKey, oCurObj )

ENDCLASS

METHOD WvtBrowse:New( oParent, nID, nTop, nLeft, nBottom, nRight )

   ::Super:New( oParent, DLG_OBJ_BROWSE, nID, nTop, nLeft, nBottom, nRight )

   RETURN Self

METHOD WvtBrowse:Create()

   dbSelectArea( ::cAlias )
#if 0
   ::nTop    := ::oBrw:nTop - 2
   ::nLeft   := ::oBrw:nLeft - 2
   ::nBottom := iif( ::lHSBar, ::oBrw:nBottom, ::oBrw:nBottom + 1 )
   ::nRight  := iif( ::lVSBar, ::oBrw:nRight, ::oBrw:nRight + 2 )
#else
   ::nTop    := ::oBrw:nTop
   ::nLeft   := ::oBrw:nLeft
   ::nBottom := ::oBrw:nBottom
   ::nRight  := ::oBrw:nRight
#endif
   ::PaintBlock( 1 )
   ::PaintBlock( 2 )
   ::PaintBlock( 3 )
   ::PaintBlock( 4 )

   ::Super:Create()

   __defaultNIL( @::bTotalRecords, {|| ( ::cAlias )->( ordKeyCount() ) } )
   __defaultNIL( @::bCurrentRecord, {|| ( ::cAlias )->( ordKeyNo()    ) } )
   ::SetVBar()

   __defaultNIL( @::bTotalColumns, {|| ::oBrw:ColCount } )
   __defaultNIL( @::bCurrentColumn, {|| ::oBrw:ColPos   } )
   ::SetHBar()

   ::oBrw:ForceStable()
   ::DeHilite()

   RETURN Self

METHOD WvtBrowse:SetVBar()

   IF ::lVSBar
      ::oVBar := WvtScrollBar():New( Self, 999991, ;
         ::oBrw:nTop, ::oBrw:nRight + 1, ::oBrw:nBottom, ::oBrw:nRight + 2 )
      ::oVBar:nBarType   := WVT_SCROLLBAR_VERT
      ::oVBar:bTotal     := ::bTotalRecords
      ::oVBar:bCurrent   := ::bCurrentRecord
      ::oVBar:aPxlBtnTop := { -2, 2, 0, 0 }
      ::oVBar:aPxlBtnBtm := {  0, 2, 2, 0 }
      ::oVBar:aPxlScroll := {  0, 2, 0, 0 }
      ::oVBar:Create()

      AAdd( ::aPaint, { ::oVBar:bBtnLeftTop, ;
         { WVT_BLOCK_BUTTON, ::oVBar:nBtn1Top, ::oVBar:nBtn1Left, ;
         ::oVBar:nBtn1Bottom, ::oVBar:nBtn1Right } } )

      AAdd( ::aPaint, { ::oVBar:bBtnRightBottom, ;
         { WVT_BLOCK_BUTTON, ::oVBar:nBtn2Top, ::oVBar:nBtn2Left, ;
         ::oVBar:nBtn2Bottom, ::oVBar:nBtn2Right } } )

      AAdd( ::aPaint, { ::oVBar:bBtnScroll, ;
         { WVT_BLOCK_BUTTON, ::oVBar:nSTop, ::oVBar:nSLeft, ;
         ::oVBar:nSBottom, ::oVBar:nSRight } } )

      ::oParent:AddObject( ::oVBar )
   ENDIF

   RETURN Self

METHOD WvtBrowse:SetHBar()

   IF ::lHSBar
      ::oHBar := WvtScrollBar():New( Self, 999990, ;
         ::oBrw:nBottom + 1, ::oBrw:nLeft, ::oBrw:nBottom + 1, ::oBrw:nRight )
      ::oHBar:nBarType   := 2
      ::oHBar:bTotal     := ::bTotalColumns
      ::oHBar:bCurrent   := ::bCurrentColumn
      ::oHBar:aPxlBtnLft := { 2, -2, 0, 0 }
      ::oHBar:aPxlBtnRgt := { 2, 0, 0, 2 }
      ::oHBar:aPxlScroll := { 2, 0, 0, 0 }
      ::oHBar:Create()

      AAdd( ::aPaint, { ::oHBar:bBtnLeftTop, ;
         { WVT_BLOCK_BUTTON, ::oHBar:nBtn1Top, ::oHBar:nBtn1Left, ;
         ::oHBar:nBtn1Bottom, ::oHBar:nBtn1Right } } )
      AAdd( ::aPaint, { ::oHBar:bBtnRightBottom, ;
         { WVT_BLOCK_BUTTON, ::oHBar:nBtn2Top, ::oHBar:nBtn2Left, ;
         ::oHBar:nBtn2Bottom, ::oHBar:nBtn2Right } } )
      AAdd( ::aPaint, { ::oHBar:bBtnScroll, ;
         { WVT_BLOCK_BUTTON, ::oHBar:nSTop, ::oHBar:nSLeft, ;
         ::oHBar:nSBottom, ::oHBar:nSRight } } )

      ::oParent:AddObject( ::oHBar )
   ENDIF

   RETURN Self

METHOD WvtBrowse:Refresh()

   LOCAL nWorkArea := Select()

   IF HB_ISBLOCK( ::bOnRefresh )
      Eval( ::bOnRefresh, Self )
   ELSE
      Select( ::cAlias )

      ::oBrw:RefreshAll()
      ::oBrw:ForceStable()

      Select( nWorkArea )
   ENDIF

   RETURN Self

METHOD WvtBrowse:HandleEvent( nKey )

   LOCAL lRet := .F.

   IF HB_ISBLOCK( ::bHandleEvent )
      lRet := Eval( ::bHandleEvent, Self, ::oParent:cPaintBlockID, ::oBrw, nKey )
   ENDIF

   RETURN lRet

METHOD WvtBrowse:NotifyChild( nIndex, nKey, oCurObj )

   LOCAL xData, i

   IF nIndex > 0 .AND. nIndex <= Len( ::aChildren )
      IF HB_ISBLOCK( ::aChildren[ nIndex ][ OBJ_CHILD_DATABLOCK ] )
         xData := Eval( ::aChildren[ nIndex ][ OBJ_CHILD_DATABLOCK ] )
      ENDIF

      Eval( ::aChildren[ nIndex ][ OBJ_CHILD_REFRESHBLOCK ], ;
         ::aChildren[ nIndex ][ OBJ_CHILD_OBJ ], ;
         ::aChildren[ nIndex ][ OBJ_CHILD_OBJ ]:oParent:cPaintBlockID, ;
         ::aChildren[ nIndex ][ OBJ_CHILD_OBJ ]:oBrw, ;
         nKey, ;
         xData )

      IF ::aChildren[ nIndex ][ OBJ_CHILD_OBJ ]:nChildren > 0
         /* Pretend IF focus is current on this object */
         Eval( ::aChildren[ nIndex ][ OBJ_CHILD_OBJ ]:bOnFocus, ::aChildren[ nIndex ][ OBJ_CHILD_OBJ ] )

         FOR i := 1 to ::aChildren[ nIndex ][ OBJ_CHILD_OBJ ]:nChildren
            ::aChildren[ nIndex ][ OBJ_CHILD_OBJ ]:NotifyChild( i, nKey, ::aChildren[ nIndex ][ OBJ_CHILD_OBJ ] )
         NEXT

         /* Restore previous environments */
         Eval( oCurObj:bOnFocus, oCurObj )
      ENDIF
   ENDIF

   RETURN Self

METHOD WvtBrowse:Hilite()

   LOCAL b := ::oBrw

   hb_DispOutAt( b:nTop - 2, b:nLeft - 2, PadR( " " + ::cDesc, b:nRight - b:nLeft + 5 ), ::cColorHilite )

   RETURN Self

METHOD WvtBrowse:DeHilite()

   LOCAL b := ::oBrw

   hb_DispOutAt( b:nTop - 2, b:nLeft - 2, PadR( " " + ::cDesc, b:nRight - b:nLeft + 5 ), ::cColorDeHilite )

   RETURN Self

METHOD WvtBrowse:SetTooltip()

   LOCAL cTip, nArea

   IF HB_ISBLOCK( ::bTooltip )
      ::SaveSettings()
      nArea := Select( ::cAlias )

      Select( ::cAlias )

      cTip := Eval( ::bTooltip )

      Select( nArea )

      ::RestSettings()
   ENDIF

   IF cTip != NIL
      ::Tooltip := cTip
   ENDIF

   wvt_SetToolTip( ::nTop, ::nLeft, ::nBottom, ::nRight, ::Tooltip )

   RETURN Self

METHOD WvtBrowse:SaveSettings()

   IF HB_ISBLOCK( ::bSaveSettings )
      ::xSettings := Eval( ::bSaveSettings, Self )
   ENDIF

   RETURN Self

METHOD WvtBrowse:RestSettings()

   IF ::xSettings != NIL .AND. HB_ISBLOCK( ::bRestSettings )
      Eval( ::bRestSettings, Self )
   ENDIF

   RETURN Self

METHOD WvtBrowse:PaintBlock( nPaintObj )

   LOCAL bBlock, b := ::oBrw

   SWITCH nPaintObj

   CASE 1
      bBlock := {|| wvt_DrawBoxRaised( b:nTop - 2, b:nLeft - 2, b:nBottom + 1, b:nRight + 2 ) }
      AAdd( ::aPaint, { bBlock, { WVT_BLOCK_BOX, b:nTop - 3, b:nLeft - 3, b:nBottom + 2, b:nRight + 3 } } )
      EXIT

   CASE 2
      bBlock := {|| wvt_DrawBoxRecessed( b:nTop, b:nLeft, b:nBottom, b:nRight ) }
      AAdd( ::aPaint, { bBlock, { WVT_BLOCK_BOX, b:nTop - 1, b:nLeft - 1, b:nBottom + 1, b:nRight + 1 } } )
      EXIT

   CASE 3
      bBlock := {|| wvt_DrawGridHorz( b:nTop + 3, b:nLeft, b:nRight, b:nBottom - b:nTop - 2 ) }
      AAdd( ::aPaint, { bBlock, { WVT_BLOCK_GRID_H, b:nTop + 4, b:nLeft + 1, b:nBottom - 1, b:nRight - 1 } } )
      EXIT

   CASE 4
      bBlock := {|| wvt_DrawGridVert( b:nTop, b:nBottom, b:aColumnsSep, Len( b:aColumnsSep ) ) }
      AAdd( ::aPaint, { bBlock, { WVT_BLOCK_GRID_V, b:nTop + 1, b:nLeft + 1, b:nBottom - 1, b:nRight - 1, b } } )
      EXIT

   ENDSWITCH

   RETURN Self

/* WvtStatusBar */
CREATE CLASS WvtStatusBar INHERIT WvtObject

   VAR    aPanels
   VAR    cColor

   METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight )
   METHOD create()
   METHOD SetPanels( aPanels )
   METHOD SetText( nPanel, cText, cColor )
   METHOD SetIcon( nPanel, cIconFile )
   METHOD Update( nPanel, cText, cColor )
   METHOD PaintBlock()
   METHOD Refresh()

ENDCLASS

METHOD WvtStatusBar:New( oParent, nID, nTop, nLeft, nBottom, nRight )

   __defaultNIL( @nTop, oParent:MaxRow() )
   __defaultNIL( @nLeft, 0 )
   __defaultNIL( @nBottom, oParent:MaxRow() )
   __defaultNIL( @nRight, oParent:MaxCol() )

   ::Super:New( oParent, DLG_OBJ_STATUSBAR, nID, nTop, nLeft, nBottom, nRight )

   ::cColor  := "N/W"

   RETURN Self

METHOD WvtStatusBar:Create()

   ::Refresh()
   ::PaintBlock( DLG_OBJ_STATUSBAR, Self )

   ::Super:Create()

   RETURN Self

METHOD WvtStatusBar:PaintBlock()

   LOCAL a_ := {}, nPanels

   AEval( ::aPanels, {| o | AAdd( a_, o:nTop ), AAdd( a_, o:nLeft ), ;
      AAdd( a_, o:nBottom ), AAdd( a_, o:nRight ) } )

   a_[ Len( a_ ) ]++
   nPanels := Len( ::aPanels )

   ::bPaint  := {|| wvt_DrawStatusBar( nPanels, a_ ) }
   AAdd( ::aPaint, { ::bPaint, ;
      { WVT_BLOCK_STATUSBAR, ::nTop, ::nLeft, ::nBottom, ::nRight } } )

   RETURN Self

METHOD WvtStatusBar:SetPanels( aPanels )

   LOCAL i, oPanel, nID
   LOCAL nLastCol := ::oParent:MaxCol()

   nID := 200000

   ::aPanels := {}

   oPanel := WvtPanel():New( ::oParent, ++nID, ::nTop, 0 )

   AAdd( ::aPanels, oPanel )

   IF aPanels != NIL
      FOR i := 1 TO Len( aPanels )
         IF ::oParent:MaxCol() > aPanels[ i ]
            oPanel := WvtPanel():New( ::oParent, ++nID, ::nTop, aPanels[ i ] )
            AAdd( ::aPanels, oPanel )
         ENDIF
      NEXT
   ENDIF

   ATail( ::aPanels ):nRight := nLastCol

   FOR i := Len( ::aPanels ) - 1 TO 1 STEP -1
      oPanel        := ::aPanels[ i ]
      oPanel:nRight := ::aPanels[ i + 1 ]:nLeft
      oPanel:cColor := ::cColor
   NEXT

   RETURN Self

METHOD WvtStatusBar:Update( nPanel, cText, cColor )

   LOCAL oPanel

   IF nPanel > 0 .AND. nPanel <= Len( ::aPanels )
      oPanel        := ::aPanels[ nPanel ]
      oPanel:Text   := cText
      oPanel:cColor := iif( cColor == NIL, "N/W", cColor )
      oPanel:Refresh()
   ENDIF

   RETURN Self

METHOD WvtStatusBar:SetText( nPanel, cText, cColor )

   LOCAL oPanel

   __defaultNIL( @cColor, ::cColor )

   IF nPanel > 0 .AND. nPanel <= Len( ::aPanels )
      oPanel        := ::aPanels[ nPanel ]
      oPanel:Text   := cText
      oPanel:cColor := cColor
   ENDIF

   RETURN Self

METHOD WvtStatusBar:SetIcon( nPanel, cIconFile )

   IF nPanel > 0 .AND. nPanel <= Len( ::aPanels )
      ::aPanels[ nPanel ]:cIconFile := cIconFile
   ENDIF

   RETURN Self

METHOD WvtStatusBar:Refresh()

   LOCAL i

   FOR i := 1 TO Len( ::aPanels )
      ::aPanels[ i ]:Refresh()
   NEXT

   RETURN NIL

/* Class WvtPanel */
CREATE CLASS WvtPanel INHERIT WvtObject

   VAR    cColor
   VAR    cTxt
   VAR    cIconFile

   ACCESS TEXT                                    INLINE ::cTxt
   ASSIGN TEXT( cText )                           INLINE ::cTxt := PadR( cText, ::nRight - ::nLeft - 2 )

   METHOD New( oParent, nId, nTop, nLeft )
   METHOD Refresh()

ENDCLASS

METHOD WvtPanel:New( oParent, nId, nTop, nLeft )

   ::Super:New( oParent, DLG_OBJ_PANEL, nId, nTop, nLeft, nTop )

   RETURN Self

METHOD WvtPanel:Refresh()

   IF ::Text != NIL
      hb_DispOutAt( ::nTop, ::nLeft + 1, ::Text, ::cColor )
   ENDIF

   RETURN Self

/* Class WvtLabel */
CREATE CLASS WvtLabel INHERIT WvtObject

   ACCESS TEXT                                    INLINE iif( ::cText == NIL, "", ::cText )
   ASSIGN TEXT( cTxt )                            INLINE ::cText := iif( cTxt == NIL, "", cTxt )

   METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight )
   METHOD create( lConfg )
   METHOD Configure()
   METHOD Refresh()
   METHOD HoverOn()
   METHOD HoverOff()
   METHOD SetText( ctxt )
   METHOD SetTextColor( nRGB )
   METHOD SetBackColor( nRGB )

ENDCLASS

METHOD WvtLabel:New( oParent, nID, nTop, nLeft, nBottom, nRight )

   ::Super:New( oParent, DLG_OBJ_LABEL, nID, nTop, nLeft, nBottom, nRight )

   RETURN Self

METHOD WvtLabel:Create( lConfg )

   __defaultNIL( @lConfg, .F. )

   __defaultNIL( @::nBottom, ::nTop )
   __defaultNIL( @::nRight, ::nLeft + Len( ::Text ) )
   __defaultNIL( @::nTextColor, RGB( 0, 0, 0 ) )

   ::nTextColorHoverOff := ::nTextColor
   ::nBackColorHoverOff := ::nBackColor

   ::hFont := wvt_CreateFont( ::cFont, ::nFontHeight, ::nFontWidth, ::nFontWeight, ::lItalic, ;
      ::lUnderline, ::lStrikeout, ::nCharSet, ::nFontQuality, ::nAngle )
   IF ::hFont != 0
      IF ! lConfg
         ::bPaint := {|| wvt_DrawLabelObj( ::nTop, ::nLeft, ::nBottom, ::nRight, ;
            ::Text, ::nAlignHorz, ::nAlignVert, ::nTextColor, ::nBackColor, ::hFont ) }
         AAdd( ::aPaint, { ::bPaint, { WVT_BLOCK_LABEL, ::nTop, ::nLeft, ::nBottom, ::nRight } } )
      ENDIF
   ENDIF

   ::Super:Create()

   RETURN Self

METHOD WvtLabel:Refresh()

   Eval( ::bPaint )

   RETURN Self

METHOD WvtLabel:SetText( cTxt )

   IF HB_ISSTRING( cTxt )
      ::Text := cTxt
      ::Refresh()
   ENDIF

   RETURN Self

METHOD WvtLabel:SetTextColor( nRGB )

   IF HB_ISNUMERIC( nRGB )
      ::nTextColor := nRGB
      ::nTextColorHoverOff := nRGB
      ::Refresh()
   ENDIF

   RETURN Self

METHOD WvtLabel:SetBackColor( nRGB )

   IF HB_ISNUMERIC( nRGB )
      ::nBackColor := nRGB
      ::nBackColorHoverOff := nRGB
      ::Refresh()
   ENDIF

   RETURN Self

METHOD WvtLabel:Configure()

   ::nTextColorHoverOff := ::nTextColor
   ::nBackColorHoverOff := ::nBackColor

   IF ::hFont != 0
      wvg_DeleteObject( ::hFont )
   ENDIF

   ::hFont := wvt_CreateFont( ::cFont, ::nFontHeight, ::nFontWidth, ::nFontWeight, ::lItalic, ;
      ::lUnderline, ::lStrikeout, ::nCharSet, ::nFontQuality, ::nAngle )

   RETURN Self

METHOD WvtLabel:HoverOn()

   LOCAL lOn := .F.

   IF ::nTextColorHoverOn != NIL
      lOn := .T.
      ::nTextColor := ::nTextColorHoverOn
   ENDIF
   IF ::nBackColorHoverOn != NIL
      lOn := .T.
      ::nBackColor := ::nBackColorHoverOn
   ENDIF

   IF lOn
      ::Refresh()
   ENDIF

   RETURN Self

METHOD WvtLabel:HoverOff()

   LOCAL lOn := .F.

   IF ::nTextColorHoverOn != NIL
      lOn := .T.
      ::nTextColor := ::nTextColorHoverOff
   ENDIF
   IF ::nBackColorHoverOn != NIL
      lOn := .T.
      ::nBackColor := ::nBackColorHoverOff
   ENDIF

   IF lOn
      ::Refresh()
   ENDIF

   RETURN Self

/* Class WvtToolBar */
CREATE CLASS WvtToolBar INHERIT WvtObject

   VAR    nPaintID
   VAR    aObjects                                INIT {}
   VAR    lHidden                                 INIT .F.
   VAR    nCurButton                              INIT 0
   VAR    lActive
   VAR    lFloating
   VAR    wScreen
   VAR    cScreen
   VAR    nBtnLeft                                INIT 0
   VAR    nRGBSep                                 INIT RGB( 150, 150, 150 )

   ACCESS nButtons                                INLINE Len( ::aButtons )

   METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight )
   METHOD create()
   METHOD Refresh()
   METHOD AddButton( cFileImage, bBlock, cTooltip )
   METHOD PaintToolBar()
   METHOD HoverOn()
   METHOD HoverOff()

ENDCLASS

METHOD WvtToolBar:New( oParent, nID, nTop, nLeft, nBottom, nRight )

   nTop    := 0
   nLeft   := 0
   __defaultNIL( @nBottom, 1 )
   nRight  := oParent:MaxCol()

   ::Super:New( oParent, DLG_OBJ_TOOLBAR, nID, nTop, nLeft, nBottom, nRight )

   ::lActive   := .T.
   ::lFloating := .F.
   ::nPaintID  := ::oParent:nPaintID++

   RETURN Self

METHOD WvtToolBar:Create()

   IF ::lFloating
      ::lActive := .F.
      ::lHidden := .T.
   ENDIF

   AEval( ::aObjects, {| o | o:lActive := ::lActive } )

   ::bPaint := {|| ::PaintToolBar() }
   AAdd( ::aPaint, { ::bPaint, ;
      { WVT_BLOCK_TOOLBAR, ::nTop, ::nLeft, ::nBottom, ::nRight } } )

   ::Super:Create()

   RETURN Self

METHOD WvtToolBar:Refresh()

   IF ::lFloating
      hb_DispBox( ::nTop, ::nLeft, ::nBottom, ::nRight, "         ", "n/w" )
   ELSE
      wvt_InvalidateRect( ::nTop, ::nLeft, ::nTop, ::nLeft )
   ENDIF

   RETURN Self

METHOD WvtToolBar:PaintToolBar()

   IF ::lActive
      wvt_DrawLine( ::nTop, ::nLeft, ::nBottom, ::nRight, 0, 1, 2, , , ::nRGBSep )
   ENDIF

   RETURN Self

METHOD WvtToolBar:AddButton( cFileImage, bBlock, cTooltip )

   LOCAL oObj, nCol

   nCol := ( ::nBottom - ::nTop + 1 ) * 2

   oObj := WvtToolButton():New( Self )

   oObj:lActive    := ::lActive
   oObj:nTop       := ::nTop
   oObj:nLeft      := ::nBtnLeft + 1
   oObj:nBottom    := ::nBottom

   IF HB_ISSTRING( cFileImage )
      oObj:nBtnType   := TLB_BUTTON_TYPE_IMAGE
      oObj:nRight     := oObj:nLeft + nCol - 1
      oObj:cFileImage := cFileImage
      oObj:bOnLeftUp  := bBlock
      oObj:Tooltip    := cTooltip
   ELSE
      oObj:nBtnType   := TLB_BUTTON_TYPE_SEPARATOR
      oObj:nRight     := oObj:nLeft
   ENDIF

   AAdd( ::aObjects, oObj )

   ::nBtnLeft         := oObj:nRight + 1
   ::nCurButton++

   ::oParent:AddObject( oObj )

   RETURN Self

METHOD WvtToolBar:HoverOn()

   IF ::lFloating .AND. ::lHidden
      ::lHidden   := .F.
      ::lActive   := .T.
#if 0
      ::cScreen   := SaveScreen( ::nTop, ::nLeft, ::nBottom, ::nRight )
      ::wScreen   := wvt_SaveScreen( ::nTop, ::nLeft, ::nBottom, ::nRight )
#endif
      AEval( ::aObjects, {| o | o:lActive := ::lActive } )

      ::Refresh()
   ENDIF

   RETURN Self

METHOD WvtToolBar:HoverOff()

   IF ::lFloating .AND. ! ::lHidden
      ::lHidden := .T.
      ::lActive := .F.
      AEval( ::aObjects, {| o | o:lActive := ::lActive } )
#if 0
      RestScreen( ::nTop, ::nLeft, ::nBottom, ::nRight, ::cScreen )
      wvt_RestScreen( ::nTop, ::nLeft, ::nBottom, ::nRight, ::wScreen, .F. )
#endif
      ::Refresh()
   ENDIF

   RETURN Self

/* Class WvtToolButton */
CREATE CLASS WvtToolButton INHERIT WvtObject

   VAR    cFileImage
   VAR    nCurState             INIT 0
   VAR    nBtnType              INIT TLB_BUTTON_TYPE_IMAGE
   VAR    aPxlOffSet            INIT { 0, -1, -3, 1 }

   METHOD New( oParent )
   METHOD create()
   METHOD Refresh()
   METHOD LeftDown()
   METHOD LeftUp()
   METHOD HoverOn()
   METHOD HoverOff()
   METHOD PaintButton()

ENDCLASS

METHOD WvtToolButton:New( oParent )

   ::Super:New( oParent, DLG_OBJ_BUTTON )

   RETURN Self

METHOD WvtToolButton:Create()

   ::bPaint := {|| ::PaintButton() }
   AAdd( ::aPaint, { ::bPaint, ;
      { WVT_BLOCK_BUTTON, ::nTop, ::nLeft, ::nBottom, ::nRight } } )

   ::Super:Create()

   RETURN Self

METHOD WvtToolButton:Refresh()

   IF ::lActive
      Eval( ::bPaint )
   ENDIF

   RETURN Self

METHOD WvtToolButton:PaintButton()

   IF ::lActive
      IF ::nBtnType == TLB_BUTTON_TYPE_IMAGE
         wvt_DrawImage( ::nTop, ::nLeft, ::nBottom, ::nRight, ::cFileImage, { 4, 4, -6, -4 } )
      ELSE
         wvt_DrawLine( ::nTop, ::nLeft, ::nBottom, ::nRight, 1, 1, , , , ::oParent:nRGBSep )
      ENDIF
   ENDIF

   RETURN Self

METHOD WvtToolButton:LeftDown()

   LOCAL lRet := .F.

   IF ::lActive .AND. ::nBtnType == TLB_BUTTON_TYPE_IMAGE
      wvt_DrawToolButtonState( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet, 2 )
      lRet := .T.
   ENDIF

   RETURN lRet

METHOD WvtToolButton:LeftUp()

   LOCAL lRet := .F.

   IF ::lActive .AND. ::nBtnType == TLB_BUTTON_TYPE_IMAGE
      wvt_DrawToolButtonState( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet, 1 )
      Eval( ::bOnLeftUp )
      lRet := .T.
   ENDIF

   RETURN lRet

METHOD WvtToolButton:HoverOn()

   ::oParent:HoverOn()

   IF ::lActive .AND. ::nBtnType == TLB_BUTTON_TYPE_IMAGE
      wvt_DrawToolButtonState( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet, 1 )
   ENDIF

   RETURN Self

METHOD WvtToolButton:HoverOff()

   ::oParent:HoverOff()

   IF ::lActive .AND. ::nBtnType == TLB_BUTTON_TYPE_IMAGE
      wvt_DrawToolButtonState( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet, 0 )
   ENDIF

   RETURN Self

/* Class WvtImage */
CREATE CLASS WvtImage INHERIT WvtObject

   VAR    cImageFile

   ACCESS cImage                                  INLINE ::cImageFile
   ASSIGN cImage( cImg )                          INLINE ::cImageFile := cImg

   METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight )
   METHOD create()
   METHOD SetImage( cImage )

ENDCLASS

METHOD WvtImage:New( oParent, nID, nTop, nLeft, nBottom, nRight )

   ::Super:New( oParent, DLG_OBJ_IMAGE, nId, nTop, nLeft, nBottom, nRight )

   RETURN Self

METHOD WvtImage:Create()

   ::bPaint := {|| iif( hb_FileExists( ::cImage ), ;
      wvt_DrawImage( ::nTop, ::nLeft, ::nBottom, ::nRight, ::cImage ), "" ) }

   AAdd( ::aPaint, { ::bPaint, ;
      { WVT_BLOCK_IMAGE, ::nTop, ::nLeft, ::nBottom, ::nRight } } )

   ::Super:Create()

   RETURN Self

METHOD WvtImage:SetImage( cImage )

   IF cImage != NIL .AND. hb_FileExists( cImage )
      ::cImageFile := cImage
      ::Refresh()
   ENDIF

   RETURN Self

/* Class WvtStatic */
CREATE CLASS WvtStatic INHERIT WvtObject

   VAR    nStatic
   VAR    nOrient
   VAR    nFormat
   VAR    nAlign
   VAR    nStyle
   VAR    nThick
   VAR    nColor

   VAR    nfTop
   VAR    nfLeft
   VAR    nfBottom
   VAR    nfRight

   VAR    nHorzVert                               INIT 0
   VAR    aRGBb
   VAR    aRGBe

   VAR    aPxlOffSet                              INIT {}

   METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight )
   METHOD create()
   METHOD Refresh()
   METHOD HoverOn()
   METHOD HoverOff()

ENDCLASS

METHOD WvtStatic:New( oParent, nID, nTop, nLeft, nBottom, nRight )

   ::Super:New( oParent, DLG_OBJ_STATIC, nID, nTop, nLeft, nBottom, nRight )

   RETURN Self

METHOD WvtStatic:Create()

   LOCAL lInside := .F.

   SWITCH ::nStatic

   CASE WVT_STATIC_LINE
      lInside := .T.
      ::bPaint  := {|| wvt_DrawLine( ::nTop, ::nLeft, ::nBottom, ::nRight, ;
         ::nOrient, ::nFormat, ::nAlign, ::nStyle, ::nThick, ::nColor ) }
      EXIT

   CASE WVT_STATIC_BOXRAISED
      ::bPaint := {|| wvt_DrawBoxRaised( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      EXIT

   CASE WVT_STATIC_BOXRECESSED
      ::bPaint := {|| wvt_DrawBoxRecessed( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      EXIT

   CASE WVT_STATIC_BOXGROUP
      ::bPaint := {|| wvt_DrawBoxGroup( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      EXIT

   CASE WVT_STATIC_BOXGROUPRAISED
      ::bPaint := {|| wvt_DrawBoxGroupRaised( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      EXIT

   CASE WVT_STATIC_OUTLINE
      ::bPaint := {|| wvt_DrawOutline( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      EXIT

   CASE WVT_STATIC_RECTANGLE
      lInside := .T.
      ::bPaint := {|| wvt_DrawRectangle( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      EXIT

   CASE WVT_STATIC_ROUNDRECT
      lInside := .T.
      ::bPaint := {|| wvt_DrawRoundRect( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      EXIT

   CASE WVT_STATIC_FOCUSRECT
      lInside := .T.
      ::bPaint := {|| wvt_DrawFocusRect( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      EXIT

   CASE WVT_STATIC_ELLIPSE
      lInside := .T.
      ::bPaint := {|| wvt_DrawEllipse( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      EXIT

   CASE WVT_STATIC_SHADEDRECT
      lInside := .T.
      ::bPaint := {|| wvt_DrawShadedRect( ::nTop, ::nLeft, ::nBottom, ::nRight, ;
         ::aPxlOffSet, ::nHorzVert, ::aRGBb, ::aRGBe ) }
      EXIT

   ENDSWITCH

   IF lInside
      ::nfTop    := ::nTop
      ::nfLeft   := ::nLeft
      ::nfBottom := ::nBottom
      ::nfRight  := ::nRight
   ELSE
      ::nfTop    := ::nTop    - 1
      ::nfLeft   := ::nLeft   - 1
      ::nfBottom := ::nBottom + 1
      ::nfRight  := ::nRight  + 1
   ENDIF

   AAdd( ::aPaint, { ::bPaint, ;
      { WVT_BLOCK_STATIC, ::nfTop, ::nfLeft, ::nfBottom, ::nfRight } } )

   ::Super:Create()

   RETURN Self

METHOD WvtStatic:HoverOn()
   RETURN Self

METHOD WvtStatic:HoverOff()
   RETURN Self

METHOD WvtStatic:Refresh()

   Eval( ::bPaint )

   RETURN Self

/* Class WvtPushButton */
CREATE CLASS WvtPushButton INHERIT WvtObject

   VAR    cCaption
   VAR    cFileImage

   ACCESS block                                   INLINE ::bOnLeftUp
   ASSIGN block( bBlock )                         INLINE ::bOnLeftUp := bBlock

   METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight )
   METHOD create()
   METHOD LeftDown()
   METHOD LeftUp()
   METHOD PaintButton()

ENDCLASS

METHOD WvtPushButton:New( oParent, nID, nTop, nLeft, nBottom, nRight )

   ::Super:New( oParent, DLG_OBJ_PUSHBUTTON, nID, nTop, nLeft, nBottom, nRight )

   RETURN Self

METHOD WvtPushButton:Create()

   ::bPaint := {|| ::PaintButton() }

   AAdd( ::aPaint, { ::bPaint, ;
      { WVT_BLOCK_BUTTON, ::nTop, ::nLeft, ::nBottom, ::nRight } } )

   ::Super:Create()

   RETURN Self

METHOD WvtPushButton:PaintButton()

   IF ::cCaption == NIL
      wvt_DrawImage( ::nTop, ::nLeft, ::nBottom, ::nRight, ::cFileImage, { 4, 4, -4, -4 } )
   ELSE
      wvt_DrawButton( ::nTop, ::nLeft, ::nBottom, ::nRight, ::cCaption, , 4 )
   ENDIF
   wvt_DrawToolButtonState( ::nTop, ::nLeft, ::nBottom, ::nRight, { 0, 0, 0, 0 }, 1 )

   RETURN Self

METHOD WvtPushButton:LeftDown()

   wvt_DrawToolButtonState( ::nTop, ::nLeft, ::nBottom, ::nRight, { 0, 0, 0, 0 }, 2 )

   RETURN .T.

METHOD WvtPushButton:LeftUp()

   wvt_DrawToolButtonState( ::nTop, ::nLeft, ::nBottom, ::nRight, { 0, 0, 0, 0 }, 1 )
   ::Eval( ::bOnLeftUp )

   RETURN .T.

/* Class WvtGets */
CREATE CLASS WvtGets INHERIT WvtObject

   VAR    aGetList                                INIT {}
   VAR    nLastGet                                INIT 1
   VAR    nCurGet                                 INIT 1
   VAR    GetList                                 INIT {}
   VAR    cDesc                                   INIT ""

   METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight )
   METHOD create()
   METHOD KillFocus()
   METHOD SetFocus()
   METHOD HandleEvent( nKey )
   METHOD AddGets( nRow, nCol, xVar, cPic, cColor, bValid, bWhen )
   METHOD PaintBlock( nIndex )
   METHOD READ()
   METHOD Hilite()
   METHOD DeHilite()
   METHOD GetData()
   METHOD SetData()

ENDCLASS

METHOD WvtGets:New( oParent, nID, nTop, nLeft, nBottom, nRight )

   ::Super:New( oParent, DLG_OBJ_GETS, nID, nTop, nLeft, nBottom, nRight )

   RETURN Self

METHOD WvtGets:Create()

   LOCAL i
   LOCAL nCurRow := Row()
   LOCAL nCurCol := Col()

   FOR i := 1 TO Len( ::aGetList )

      __defaultNIL( @::aGetList[ i ][ 7 ], "N/W*,N/W*,,,N/GR*" )
      __defaultNIL( @::aGetList[ i ][ 5 ], {|| .T. } )
      __defaultNIL( @::aGetList[ i ][ 6 ], {|| .T. } )

      AAdd( ::GetList, Get():New( ::aGetList[ i ][ 1 ], ::aGetList[ i ][ 2 ], {| v | iif( PCount() == 0, ::aGetList[ i ][ 3 ], ::aGetList[ i ][ 3 ] := v ) }, "::aGetList[ i ][ 3 ]", ::aGetList[ i ][ 7 ] ) )

      ::GetList[ i ]:Display()
      ::PaintBlock( i )
   NEXT
   SetPos( nCurRow, nCurCol )

   ::Super:Create()
   ::Dehilite()

   RETURN Self

METHOD WvtGets:PaintBlock( nIndex )

   LOCAL nLen, bPaint

   nLen   := Len( Transform( ::aGetList[ nIndex ][ 3 ], ::aGetList[ nIndex ][ 4 ] ) )

   bPaint := {|| wvt_DrawBoxGet( ::aGetList[ nIndex ][ 1 ], ::aGetList[ nIndex ][ 2 ], nLen ) }

   AAdd( ::aPaint, { bPaint, ;
      { WVT_BLOCK_GETS, ::aGetList[ nIndex ][ 1 ] - 1, ::aGetList[ nIndex ][ 2 ] - 1, ;
      ::aGetList[ nIndex ][ 1 ] - 1,  ::aGetList[ nIndex ][ 2 ] + nLen } } )

   RETURN Self

METHOD WvtGets:SetFocus()

   RETURN Self

METHOD WvtGets:KillFocus()

   RETURN Self

METHOD WvtGets:AddGets( nRow, nCol, xVar, cPic, cColor, bValid, bWhen )

   AAdd( ::aGetList, { nRow, nCol, xVar, cPic, bValid, bWhen, cColor } )

   RETURN Self

METHOD WvtGets:HandleEvent( nKey )

   LOCAL lRet := .F.

   DO CASE
   CASE nKey == K_LDBLCLK
      ::Read()
      lRet := .T.
   ENDCASE

   RETURN lRet

METHOD WvtGets:Read()

   ReadModal( ::GetList, ::nCurGet )

   RETURN Self

METHOD WvtGets:GetData()
   RETURN NIL

METHOD WvtGets:SetData( /* aData */ )
   RETURN Self

METHOD WvtGets:Hilite()

   hb_DispOutAt( ::nTop, ::nLeft, PadR( " " + ::cDesc, ::nRight - ::nLeft + 1 ), ::cColorHilite )

   RETURN Self

METHOD WvtGets:DeHilite()

   hb_DispOutAt( ::nTop, ::nLeft, PadR( " " + ::cDesc, ::nRight - ::nLeft + 1 ), ::cColorDeHilite )

   RETURN Self

/* Class WvtScrollBar */
CREATE CLASS WvtScrollBar INHERIT WvtObject

   VAR    nBarType                                INIT WVT_SCROLLBAR_VERT

   VAR    nTotal                                  INIT 100
   VAR    nCurrent                                INIT 1
   VAR    nThumbPos                               INIT 0
   VAR    nBlockNo                                INIT 1

   VAR    nSTop
   VAR    nSLeft
   VAR    nSBottom
   VAR    nSRight

   VAR    nBtn1Top
   VAR    nBtn1Left
   VAR    nBtn1Bottom
   VAR    nBtn1Right

   VAR    nBtn2Top
   VAR    nBtn2Left
   VAR    nBtn2Bottom
   VAR    nBtn2Right
   VAR    bBtnLeftTop
   VAR    bBtnLeftTopDep
   VAR    bBtnRightBottom
   VAR    bBtnRightBottomDep
   VAR    bBtnScroll
   VAR    bTotal
   VAR    bCurrent
   VAR    lHidden                                 INIT .T.

   VAR    aPxlBtnTop                              INIT { 0, 0, 0, 0 }
   VAR    aPxlBtnLft                              INIT { 0, 0, 0, 0 }
   VAR    aPxlBtnBtm                              INIT { 0, 0, 0, 0 }
   VAR    aPxlBtnRgt                              INIT { 0, 0, 0, 0 }
   VAR    aPxlScroll                              INIT { 0, 0, 0, 0 }

   VAR    lLeftDown                               INIT .F.
   VAR    lOnThumb                                INIT .F.
   VAR    lAnchored                               INIT .F.
   VAR    lOnLeftDown                             INIT .F.

   VAR    nScrollUnits                            INIT 0

   METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight )
   METHOD create()
   METHOD Configure( nTop, nLeft, nBottom, nRight )
   METHOD Refresh()
   METHOD HandleEvent( nKey )
   METHOD SetPos( nTotal, nCurrent )
   METHOD GetPos()
   METHOD ThumbPos()
   METHOD SetTooltip()

ENDCLASS

METHOD wvtScrollbar:New( oParent, nID, nTop, nLeft, nBottom, nRight )

   ::Super:New( oParent, DLG_OBJ_SCROLLBAR, nID, nTop, nLeft, nBottom, nRight )

   RETURN Self

METHOD wvtScrollbar:Create()

   IF ::nTop == NIL .OR. ::nLeft == NIL
      RETURN NIL
   ENDIF

   IF ::nBarType == WVT_SCROLLBAR_VERT
      __defaultNIL( @::nBottom, ::nTop + 5 )
      __defaultNIL( @::nRight, ::nLeft + 1 )

      ::nRight       := ::nLeft + 1
      ::nBottom      := Max( 7, ::nBottom )

      ::nBtn1Top     := ::nTop
      ::nBtn1Left    := ::nLeft
      ::nBtn1Bottom  := ::nTop
      ::nBtn1Right   := ::nRight

      ::nBtn2Top     := ::nBottom
      ::nBtn2Left    := ::nLeft
      ::nBtn2Bottom  := ::nBottom
      ::nBtn2Right   := ::nRight

      ::nSTop        := ::nTop + 1
      ::nSLeft       := ::nLeft
      ::nSBottom     := ::nBottom - 1
      ::nSRight      := ::nRight

      ::nScrollUnits := ::nSBottom - ::nSTop + 1

      ::nTotal       := Eval( ::bTotal   )
      ::nCurrent     := Eval( ::bCurrent )
      ::ThumbPos()

      ::bBtnLeftTop := ;
         {|| wvt_DrawScrollButton( ::nBtn1Top, ::nBtn1Left, ::nBtn1Bottom, ::nBtn1Right, ::aPxlBtnTop, 1 ) }
      ::bBtnRightBottom := ;
         {|| wvt_DrawScrollButton( ::nBtn2Top, ::nBtn2Left, ::nBtn2Bottom, ::nBtn2Right, ::aPxlBtnBtm, 3 ) }
      ::bBtnScroll := ;
         {|| wvt_DrawScrollThumbVert( ::nSTop, ::nSLeft, ::nSBottom, ::nSRight, ::aPxlScroll, ;
         ::nThumbPos ) }
      ::bBtnLeftTopDep := ;
         {|| wvt_DrawScrollButton( ::nBtn1Top, ::nBtn1Left, ::nBtn1Bottom, ::nBtn1Right, ::aPxlBtnTop, 1, .T. ) }
      ::bBtnRightBottomDep := ;
         {|| wvt_DrawScrollButton( ::nBtn2Top, ::nBtn2Left, ::nBtn2Bottom, ::nBtn2Right, ::aPxlBtnBtm, 3, .T. ) }

   ELSE
      __defaultNIL( @::nBottom, ::nTop )
      __defaultNIL( @::nRight, ::nLeft + 11 )

      ::nBottom      := ::nTop
      ::nRight       := Max( 11, ::nRight )

      ::nBtn1Top     := ::nTop
      ::nBtn1Left    := ::nLeft
      ::nBtn1Bottom  := ::nBottom
      ::nBtn1Right   := ::nLeft + 1

      ::nBtn2Top     := ::nTop
      ::nBtn2Left    := ::nRight - 1
      ::nBtn2Bottom  := ::nBottom
      ::nBtn2Right   := ::nRight

      ::nSTop        := ::nTop
      ::nSLeft       := ::nLeft + 2
      ::nSBottom     := ::nBottom
      ::nSRight      := ::nRight - 2

      ::nScrollUnits := ::nSRight - ::nSLeft + 1

      ::nTotal       := Eval( ::bTotal   )
      ::nCurrent     := Eval( ::bCurrent )

      ::ThumbPos()

      ::bBtnLeftTop := ;
         {|| wvt_DrawScrollButton( ::nBtn1Top, ::nBtn1Left, ::nBtn1Bottom, ::nBtn1Right, ::aPxlBtnLft, 2 ) }
      ::bBtnRightBottom := ;
         {|| wvt_DrawScrollButton( ::nBtn2Top, ::nBtn2Left, ::nBtn2Bottom, ::nBtn2Right, ::aPxlBtnRgt, 4 ) }
      ::bBtnScroll := ;
         {|| wvt_DrawScrollThumbHorz( ::nSTop, ::nSLeft, ::nSBottom, ::nSRight, ::aPxlScroll, ::nThumbPos ) }
      ::bBtnLeftTopDep := ;
         {|| wvt_DrawScrollButton( ::nBtn1Top, ::nBtn1Left, ::nBtn1Bottom, ::nBtn1Right, ::aPxlBtnLft, 2, .T. ) }
      ::bBtnRightBottomDep := ;
         {|| wvt_DrawScrollButton( ::nBtn2Top, ::nBtn2Left, ::nBtn2Bottom, ::nBtn2Right, ::aPxlBtnRgt, 4, .T. ) }

   ENDIF

   ::bOnLeftUp      := {|| ::HandleEvent( K_LBUTTONUP      ) }
   ::bOnLeftDown    := {|| ::HandleEvent( K_LBUTTONDOWN    ), .F. }
   ::bOnMMLeftDown  := {|| ::HandleEvent( K_MMLEFTDOWN     ) }
   ::bOnLeftPressed := {|| ::HandleEvent( K_LBUTTONPRESSED ) }

   Eval( ::bBtnLeftTop     )
   Eval( ::bBtnRightBottom )

   ::Super:Create()

   RETURN Self

METHOD wvtScrollbar:Configure( nTop, nLeft, nBottom, nRight )

   ::nTop     := nTop
   ::nLeft    := nLeft
   ::nBottom  := nBottom
   ::nRight   := nRight

   IF ::nBarType == WVT_SCROLLBAR_VERT
      ::nRight       := ::nLeft + 1
      ::nBottom      := Max( 7, ::nBottom )

      ::nBtn1Top     := ::nTop
      ::nBtn1Left    := ::nLeft
      ::nBtn1Bottom  := ::nTop
      ::nBtn1Right   := ::nRight

      ::nBtn2Top     := ::nBottom
      ::nBtn2Left    := ::nLeft
      ::nBtn2Bottom  := ::nBottom
      ::nBtn2Right   := ::nRight

      ::nSTop        := ::nTop + 1
      ::nSLeft       := ::nLeft
      ::nSBottom     := ::nBottom - 1
      ::nSRight      := ::nRight

      ::nScrollUnits := ::nSBottom - ::nSTop + 1

      ::nTotal       := Eval( ::bTotal   )
      ::nCurrent     := Eval( ::bCurrent )
      ::ThumbPos()
   ELSE
      ::nBottom      := ::nTop
      ::nRight       := Max( 11, ::nRight )

      ::nBtn1Top     := ::nTop
      ::nBtn1Left    := ::nLeft
      ::nBtn1Bottom  := ::nBottom
      ::nBtn1Right   := ::nLeft + 1

      ::nBtn2Top     := ::nTop
      ::nBtn2Left    := ::nRight - 1
      ::nBtn2Bottom  := ::nBottom
      ::nBtn2Right   := ::nRight

      ::nSTop        := ::nTop
      ::nSLeft       := ::nLeft + 2
      ::nSBottom     := ::nBottom
      ::nSRight      := ::nRight - 2

      ::nScrollUnits := ::nSRight - ::nSLeft + 1

      ::nTotal       := Eval( ::bTotal   )
      ::nCurrent     := Eval( ::bCurrent )

      ::ThumbPos()
   ENDIF

   ::Refresh()

   RETURN Self

METHOD wvtScrollbar:Refresh()

   Eval( ::bBtnScroll )

   RETURN Self

METHOD wvtScrollbar:SetPos( nTotal, nCurrent )

   __defaultNIL( @nTotal, Eval( ::bTotal   ) )
   __defaultNIL( @nCurrent, Eval( ::bCurrent ) )

   ::nTotal   := nTotal
   ::nCurrent := nCurrent

   ::ThumbPos()
   ::Refresh()

   RETURN Self

METHOD wvtScrollbar:ThumbPos()

   LOCAL nNewPos, nRecPerUnit, nCurUnit

   IF ::nBarType == WVT_SCROLLBAR_VERT
      nRecPerUnit := ::nTotal / ::nScrollUnits
      nCurUnit    := Int( ::nCurrent / nRecPerUnit )

      DO CASE
      CASE ::nCurrent == 1
         nCurUnit := 0
      CASE ::nCurrent == ::nTotal
         nCurUnit := ::nScrollUnits
      ENDCASE
      nNewPos     := ::nSTop + nCurUnit

      DO CASE
      CASE nNewPos < ::nSTop
         nNewPos  := ::nSTop
      CASE nNewPos > ::nSBottom
         nNewPos  := ::nSBottom
      ENDCASE

   ELSE
      IF ::nTotal < ::nScrollUnits
         nCurUnit := ::nCurrent * Int( ::nScrollUnits / ::nTotal )
      ELSE
         nRecPerUnit := ::nTotal / ::nScrollUnits
         nCurUnit    := Int( ::nCurrent / nRecPerUnit )
      ENDIF

      DO CASE
      CASE ::nCurrent == 1
         nCurUnit := 0
      CASE ::nCurrent == ::nTotal
         nCurUnit := ::nScrollUnits
      ENDCASE

      nNewPos := ::nSLeft + nCurUnit

      DO CASE
      CASE nNewPos < ::nSLeft
         nNewPos := ::nSLeft
      CASE nNewPos > ::nSRight - 1
         nNewPos := ::nSRight - 1
      ENDCASE

   ENDIF

   ::nThumbPos := nNewPos

   RETURN Self

METHOD wvtScrollbar:GetPos()
   RETURN ::nCurrent

METHOD wvtScrollbar:SetTooltip()

   ::Tooltip := hb_ntos( Int( ::nCurrent ) ) + " / " + hb_ntos( Int( ::nTotal ) )

   wvt_SetToolTip( ::nTop, ::nLeft, ::nBottom, ::nRight, ::Tooltip )

   RETURN Self

METHOD wvtScrollbar:HandleEvent( nKey )

   LOCAL nmRow, nmCol, nOff
   LOCAL lHit  := .F.
   LOCAL mKeys_ := { K_LBUTTONDOWN, K_LBUTTONUP, K_MMLEFTDOWN, K_LBUTTONPRESSED }

   IF AScan( mKeys_, nKey ) == 0
      RETURN .F.
   ENDIF

   nmRow := MRow()
   nmCol := MCol()

   DO CASE
   CASE ::nBarType == WVT_SCROLLBAR_VERT
      lHit := .T.

      DO CASE
      CASE ::lAnchored .AND. nKey == K_MMLEFTDOWN
         IF nmRow != ::nThumbPos
            nOff := ::nThumbPos - nmRow
            IF nOff > 0
               ::nThumbPos := Max( ::nTop + 1, nmRow )
            ELSE
               ::nThumbPos := Min( ::nBottom - 1, nmRow )
            ENDIF
            ::nCurrent := ( ::nTotal * ( ::nThumbPos - ::nTop ) / ::nScrollUnits )

            IF ::nCurrent > ::nTotal
               ::nCurrent := ::nTotal
            ENDIF
            IF ::nCurrent < 1
               ::nCurrent := 1
            ENDIF

            ::SetPos( ::nTotal, ::nCurrent )

            ::SetTooltip()
            wvt_Keyboard( K_SBTHUMBTRACKVERT )
         ELSE
            lHit := .F.
         ENDIF

      CASE ::lAnchored .AND. nKey == K_LBUTTONUP
         ::lAnchored := .F.

      OTHERWISE
         lHit := .F.

         IF nmCol >= ::nLeft .AND. nmCol <= ::nRight
            lHit := .T.

            DO CASE
            CASE nmRow == ::nThumbPos .AND. nKey == K_LBUTTONDOWN
               ::lAnchored := .T.

            CASE nKey == K_LBUTTONUP
               IF ( lHit := ::lOnLeftDown )
                  DO CASE
                  CASE nmRow == ::nTop
                     Eval( ::bBtnLeftTop )
                  CASE nmRow == ::nBottom
                     Eval( ::bBtnRightBottom )
                  CASE nmRow < ::nThumbPos .AND. nmRow > ::nTop
                  CASE nmRow > ::nThumbPos .AND. nmRow < ::nBottom
                  OTHERWISE
                     lHit := .F.
                  ENDCASE
                  IF lHit
                     ::lOnLeftDown := .F.
                  ENDIF
               ENDIF

            CASE nKey == K_LBUTTONPRESSED
               IF ( lHit := ::lOnLeftDown )
                  DO CASE
                  CASE nmRow == ::nTop
                     wvt_Keyboard( K_SBLINEUP   )
                  CASE nmRow == ::nBottom
                     wvt_Keyboard( K_SBLINEDOWN )
                  CASE nmRow < ::nThumbPos .AND. nmRow > ::nTop
                     wvt_Keyboard( K_SBPAGEUP )
                  CASE nmRow > ::nThumbPos .AND. nmRow < ::nBottom
                     wvt_Keyboard( K_SBPAGEDOWN )
                  OTHERWISE
                     lHit := .F.
                  ENDCASE
               ENDIF

            CASE nKey == K_LBUTTONDOWN
               DO CASE
               CASE nmRow == ::nTop
                  Eval( ::bBtnLeftTopDep )
                  wvt_Keyboard( K_SBLINEUP )
               CASE nmRow == ::nBottom
                  Eval( ::bBtnRightBottomDep )
                  wvt_Keyboard( K_SBLINEDOWN )
               CASE nmRow < ::nThumbPos .AND. nmRow > ::nTop
                  wvt_Keyboard( K_SBPAGEUP   )
               CASE nmRow > ::nThumbPos .AND. nmRow < ::nBottom
                  wvt_Keyboard( K_SBPAGEDOWN )
               OTHERWISE
                  lHit := .F.
               ENDCASE
               IF lHit
                  ::lOnLeftDown := .T.
               ENDIF
            ENDCASE
         ENDIF

      ENDCASE

   CASE ::nBarType == WVT_SCROLLBAR_HORZ
      DO CASE
      CASE ::lAnchored .AND. nKey == K_MMLEFTDOWN
         IF ( lHit := ( nmCol < ::nThumbPos .OR. nmCol > ::nThumbPos + 1 ) )

            nOff := ::nThumbPos - nmCol
            IF nOff > 0
               ::nThumbPos := Max( ::nLeft + 2, nmCol )
            ELSE
               ::nThumbPos := Min( ::nRight - 2, nmCol )
            ENDIF

            ::nCurrent := ( ::nTotal * ( ::nThumbPos - ::nLeft + 1 ) / ::nScrollUnits )

            IF ::nCurrent > ::nTotal
               ::nCurrent := ::nTotal
            ENDIF
            IF ::nCurrent < 1
               ::nCurrent := 1
            ENDIF

            ::SetPos( ::nTotal, ::nCurrent )

            wvt_Keyboard( K_SBTHUMBTRACKHORZ )
         ENDIF

      CASE ::lAnchored .AND. nKey == K_LBUTTONUP
         ::lAnchored := .F.
         lHit := .T.

      OTHERWISE

         IF ( lHit := nmRow == ::nTop .AND. nmCol >= ::nLeft .AND. nmCol <= ::nRight )

            DO CASE
            CASE nKey == K_LBUTTONDOWN .AND. nmCol >= ::nThumbPos .AND. nmCol <= ::nThumbPos + 1
               ::lAnchored := .T.

            CASE nKey == K_LBUTTONUP

               IF ( lHit := ::lOnLeftDown )
                  DO CASE
                  CASE nmCol >= ::nLeft    .AND. nmCol <= ::nLeft + 1
                     Eval( ::bBtnLeftTop )
                  CASE nmCol >= ::nRight - 1 .AND. nmCol <= ::nRight
                     Eval( ::bBtnRightBottom )
                  CASE nmCol <  ::nThumbPos
                  CASE nmCol >  ::nThumbPos + 1
                  OTHERWISE
                     lHit := .F.
                  ENDCASE
                  IF lHit
                     ::lOnLeftDown := .F.
                  ENDIF
               ENDIF

            CASE nKey == K_LBUTTONPRESSED
               IF ( lHit := ::lOnLeftDown )
                  DO CASE
                  CASE nmCol == ::nLeft  .OR. nmCol == ::nLeft + 1
                     wvt_Keyboard( K_SBLINELEFT )
                  CASE nmCol == ::nRight .OR. nmCol == ::nRight - 1
                     wvt_Keyboard( K_SBLINERIGHT )
                  CASE nmCol < ::nThumbPos
                     wvt_Keyboard( K_SBPAGELEFT )
                  CASE nmCol > ::nThumbPos + 1
                     wvt_Keyboard( K_SBPAGERIGHT )
                  OTHERWISE
                     lHit := .F.
                  ENDCASE
               ENDIF

            CASE nKey == K_LBUTTONDOWN
               DO CASE
               CASE nmCol == ::nLeft  .OR. nmCol == ::nLeft + 1
                  Eval( ::bBtnLeftTopDep )
                  wvt_Keyboard( K_SBLINELEFT )
               CASE nmCol == ::nRight .OR. nmCol == ::nRight - 1
                  Eval( ::bBtnRightBottomDep )
                  wvt_Keyboard( K_SBLINERIGHT )
               CASE nmCol < ::nThumbPos
                  wvt_Keyboard( K_SBPAGELEFT )
               CASE nmCol > ::nThumbPos + 1
                  wvt_Keyboard( K_SBPAGERIGHT )
               OTHERWISE
                  lHit := .F.
               ENDCASE
               IF lHit
                  ::lOnLeftDown := .T.
               ENDIF
            ENDCASE
         ENDIF
      ENDCASE
   ENDCASE

   RETURN lHit

/* CLASS WvtBanner */
CREATE CLASS WvtBanner INHERIT WvtObject

   VAR    nTimeDelay                              INIT 0.5    /* One-half Second */
   VAR    nDirection                              INIT 0      /* LEFT 1-RIGHT */
   VAR    nCharToSkip                             INIT 1
   VAR    cText                                   INIT ""
   VAR    cDispText                               INIT ""
   VAR    nTextLen                                INIT 0
   VAR    nTextIndex                              INIT 0

   VAR    oLabel

   VAR    nAlignVert                              INIT 2     /* Center */

   VAR    nCurSeconds                             INIT 0
   VAR    nCurAlign

   METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight )
   METHOD create()
   METHOD Configure()
   METHOD Refresh()
   METHOD HoverOn()
   METHOD HoverOff()
   METHOD OnTimer()
   METHOD SetText( cText )
   METHOD Destroy()

ENDCLASS

METHOD WvtBanner:New( oParent, nID, nTop, nLeft, nBottom, nRight )

   ::Super:New( oParent, DLG_OBJ_BANNER, nID, nTop, nLeft, nBottom, nRight )

   RETURN Self

METHOD WvtBanner:Create()

   ::cDispText := ::cText

   ::oLabel := WvtLabel():New( ::oParent, , ::nTop, ::nLeft, ::nBottom, ::nRight )
   ::oLabel:Text              := ::cDispText
   ::oLabel:cFont             := ::cFont
   ::oLabel:nFontHeight       := ::nFontHeight
   ::oLabel:nFontWidth        := ::nFontWidth
   ::oLabel:nFontWeight       := ::nFontWeight
   ::oLabel:nFontQuality      := ::nFontQuality
   ::oLabel:lItalic           := ::lItalic
   ::oLabel:lStrikeout        := ::lStrikeout
   ::oLabel:lUnderline        := ::lUnderline
   ::oLabel:nAlignVert        := ::nAlignVert
   ::oLabel:nAlignHorz        := iif( ::nDirection == 0, 0, 1 )
   ::oLabel:nTextColor        := ::nTextColor
   ::oLabel:nBackColor        := ::nBackColor
   ::oLabel:nTextColorHoverOn := ::nTextColorHoverOn
   ::oLabel:nBackColorHoverOn := ::nBackColorHoverOn

   ::oLabel:Create()

   ::nCurSeconds := Seconds()
   ::nTextLen    := Len( ::cText )
   ::nTextIndex  := iif( ::nDirection == 0, 1, ::nTextLen )
   ::nCurAlign   := ::nDirection

   ::Super:Create()

   RETURN Self

METHOD WvtBanner:Destroy()

   wvg_DeleteObject( ::oLabel:hFont )

   RETURN NIL

METHOD WvtBanner:Configure()
   RETURN Self

METHOD WvtBanner:OnTimer()

   ::Refresh()

   RETURN Self

METHOD WvtBanner:SetText( cText )

   IF cText != NIL
      ::cText := cText
      ::Refresh()
   ENDIF

   RETURN Self

METHOD WvtBanner:Refresh()

   LOCAL nNewTime

   IF Abs( ( nNewTime := Seconds() ) - ::nCurSeconds ) >= ::nTimeDelay
      ::nCurSeconds := nNewTime

      IF ::nDirection == 0
         ::nTextIndex++
         IF ::nTextIndex > ::nTextLen
            ::nTextIndex := 1
            ::nCurAlign  := iif( ::nCurAlign == 0, 1, 0 )
         ENDIF

         IF ::nCurAlign == 0   /* Left */
            ::cDispText := SubStr( ::cText, ::nTextIndex )
         ELSE                  /* Right */
            ::cDispText := SubStr( ::cText, 1, ::nTextIndex )
         ENDIF
      ELSE
         ::nTextIndex--
         IF ::nTextIndex < 0
            ::nTextIndex := ::nTextLen
            ::nCurAlign := iif( ::nCurAlign == 0, 1, 0 )
         ENDIF

         IF ::nCurAlign == 0   /* Left */
            ::cDispText := SubStr( ::cText, ::nTextIndex )
         ELSE                  /* Right */
            ::cDispText := SubStr( ::cText, 1, ::nTextIndex )
         ENDIF
      ENDIF

      ::oLabel:nAlignHorz := ::nCurAlign
      ::oLabel:SetText( ::cDispText )
      ::oLabel:Refresh()
   ENDIF

   RETURN Self

METHOD WvtBanner:HoverOn()

   ::oLabel:HoverOn()

   RETURN Self

METHOD WvtBanner:HoverOff()

   ::oLabel:HoverOff()

   RETURN Self

/* Class WvtTextBox */
CREATE CLASS WvtTextBox INHERIT WvtObject

   VAR    cText                                   INIT ""

   METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight )
   METHOD create()
   METHOD Configure()
   METHOD Refresh()
   METHOD SetText( cText )
   METHOD HoverOn()
   METHOD HoverOff()

ENDCLASS

METHOD WvtTextBox:New( oParent, nID, nTop, nLeft, nBottom, nRight )

   ::Super:New( oParent, DLG_OBJ_TEXTBOX, nID, nTop, nLeft, nBottom, nRight )

   RETURN Self

METHOD WvtTextBox:Create()

   ::nTextColorHoverOff := ::nTextColor

   ::hFont := wvt_CreateFont( ::cFont, ::nFontHeight, ::nFontWidth, ;
      ::nFontWeight, ::lItalic, ::lUnderline, ::lStrikeout, ;
      ::nCharSet, ::nFontQuality, 0 )

   IF ::hFont != 0
      ::bPaint := {|| wvt_DrawTextBox( ::nTop, ::nLeft, ::nBottom, ::nRight, ;
         ::aPxlTLBR, ::cText, ::nAlignHorz, ::nAlignVert, ;
         ::nTextColor, ::nBackColor, ::nBackMode, ::hFont ) }

      AAdd( ::aPaint, { ::bPaint, { WVT_BLOCK_LABEL, ::nTop, ::nLeft, ::nBottom, ::nRight } } )
   ENDIF

   ::Super:Create()

   RETURN Self

METHOD WvtTextBox:Refresh()

   Eval( ::bPaint )

   RETURN Self

METHOD WvtTextBox:Configure()
   RETURN Self

METHOD WvtTextBox:SetText( cText )

   IF cText != NIL
      ::cText := cText
      ::Refresh()
   ENDIF

   RETURN Self

METHOD WvtTextBox:HoverOn( /* cText */ )

   IF ::nTextColorHoverOn != NIL
      ::nTextColor := ::nTextColorHoverOn
      ::Refresh()
   ENDIF

   RETURN Self

METHOD WvtTextBox:HoverOff( /* cText */ )

   IF ::nTextColorHoverOn != NIL
      ::nTextColor := ::nTextColorHoverOff
      ::Refresh()
   ENDIF

   RETURN Self

/* Class WvtProgressBar */
CREATE CLASS WvtProgressBar INHERIT WvtObject

   VAR    cImage
   VAR    nDirection                              INIT 0      /* 0-Left-Right,Top-Bottom  1-Right-Left,Bottom-Top */
   VAR    nStyle                                  INIT 0
   VAR    lVertical                               INIT .F.
   VAR    lActive                                 INIT .F.

   VAR    nBarColor                               INIT RGB( 0, 0, 128 )
   VAR    nCurrent                                INIT 0
   VAR    nTotal                                  INIT 1
   VAR    nPercent                                INIT 0
   VAR    cBackColor                              INIT "W/W"

   VAR    cScreen

   METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight )
   METHOD create()
   METHOD display( nCurrent, nTotal )
   METHOD Activate()
   METHOD DeActivate()

ENDCLASS

METHOD WvtProgressBar:New( oParent, nID, nTop, nLeft, nBottom, nRight )

   ::Super:New( oParent, DLG_OBJ_PROGRESSBAR, nID, nTop, nLeft, nBottom, nRight )

   RETURN Self

METHOD WvtProgressBar:Create()

   __defaultNIL( @::nTop, 0 )
   __defaultNIL( @::nLeft, 0 )
   __defaultNIL( @::nBottom, iif( ::lVertical, ::nTop + 9, ::nTop ) )
   __defaultNIL( @::nRight, iif( ::lVertical, ::nLeft + 1, ::nLeft + 19 ) )
   __defaultNIL( @::nTextColor, RGB( 255, 255, 255 ) )
   __defaultNIL( @::nBackColor, RGB( 198, 198, 198 ) )

   ::bPaint := {|| ::Display() }
   AAdd( ::aPaint, { ::bPaint, { WVT_BLOCK_LABEL, ::nTop, ::nLeft, ::nBottom, ::nRight } } )

   ::Super:Create()

   RETURN Self

METHOD WvtProgressBar:Display( nCurrent, nTotal )

   IF ! ::lActive
      RETURN Self
   ENDIF

   __defaultNIL( @nCurrent, ::nCurrent )
   __defaultNIL( @nTotal, ::nTotal )

   ::nCurrent := nCurrent
   ::nTotal   := nTotal

   IF ::nCurrent > ::nTotal
      ::nCurrent := ::nTotal
   ENDIF

   ::nPercent := Int( ::nCurrent / ::nTotal * 100 )

   wvt_DrawProgressBar( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlTLBR, ::nPercent, ;
      ::nBackColor, ::nBarColor, ::cImage, ::lVertical, ::nDirection )

   RETURN Self

METHOD WvtProgressBar:Activate()

   ::cScreen := SaveScreen( ::nTop, ::nLeft, ::nBottom, ::nRight )
   hb_DispBox( ::nTop, ::nLeft, ::nBottom, ::nRight, "         ", ::cBackColor )
   ::lActive := .T.

   RETURN Self

METHOD WvtProgressBar:DeActivate()

   ::lActive  := .F.
   ::nCurrent := 0
   ::nTotal   := 1
   RestScreen( ::nTop, ::nLeft, ::nBottom, ::nRight, ::cScreen )
   ::cScreen := NIL

   RETURN Self

/* Class WvtMenu [Peter Rees] */
CREATE CLASS wvtMenu

   METHOD create( cCaption )
   METHOD AddItem( cCaption, bAction )
   METHOD DelAllItems()
   METHOD DelItem( nItemNum )
   METHOD EnableItem( nItemNum )
   METHOD DisableItem( nItemNum )
   METHOD NumItems()
   METHOD Destroy()
   METHOD GetItem( nItemNum )
   METHOD FindMenuItemById( nId )
   METHOD DrawMenuBar()

   CLASS VAR MenuItemId                            INIT 1

   VAR    aItems
   VAR    hMenu
   VAR    Caption
   VAR    IdNumber

ENDCLASS

METHOD wvtMenu:Create( cCaption )

   ::aItems := {}

   IF Empty( ::hMenu := wvt_CreateMenu() )
#if 0
      Throw( ErrorNew( "wvtMenu", 1000, "wvtMenu:Init()", "Create Menu Error", { cCaption, cCaption } ) )
#endif
   ENDIF
   ::Caption := iif( cCaption == NIL, "", cCaption )

   RETURN Self

METHOD wvtMenu:Destroy()

   IF ! Empty( ::hMenu )
      ::DelAllItems()

      IF ! wvt_DestroyMenu( ::hMenu )
#if 0
         Throw( ErrorNew( "wvtMenu", 1000, "wvtMenu:Destroy()", "Destroy menu FAILED", {} ) )
#endif
      ENDIF
      ::hMenu := 0
   ENDIF

   RETURN .T.

METHOD wvtMenu:AddItem( cCaption, bAction )

   LOCAL lResult := .F., aItem

   IF ! Empty( ::hMenu ) .AND. ( ! Empty( cCaption ) .OR. ! Empty( bAction ) )
      IF HB_ISOBJECT( bAction )
         cCaption := iif( ! Empty( cCaption ), cCaption, bAction:Caption )
         aItem := { MF_POPUP, bAction:hMenu, cCaption, bAction }   /* bAction is a wvtMenu object reference */
      ELSEIF HB_ISBLOCK( bAction )
         aItem := { MF_STRING, ::MenuItemId++, cCaption, bAction } /* bAction is a code block to execute */
      ELSEIF Left( cCaption, 1 ) == "-"
         aItem := { MF_SEPARATOR, 0, 0, NIL }
      ELSE
#if 0
         Throw( ErrorNew( "wvtMenu", 3101, "wvtMenu:AddItem()", "Argument Error", { cCaption, bAction } ) )
#endif
      ENDIF

      IF ! wvt_AppendMenu( ::hMenu, aItem[ WVT_MENU_TYPE ], aItem[ WVT_MENU_IDENTIFIER ], aItem[ WVT_MENU_CAPTION ] )
#if 0
         Throw( ErrorNew( "wvtMenu", 1000, "wvtMenu:AddItem()", "Add menu item", { cCaption, bAction } ) )
#endif
      ENDIF

      AAdd( ::aItems, aItem )

      lResult := .T.
   ENDIF

   RETURN lResult

METHOD wvtMenu:DelAllItems()

   LOCAL lResult := .T., nItems

   nItems := ::NumItems()
   DO WHILE nItems > 0 .AND. lResult
      lResult := ::DelItem( nItems )
      nItems--
   ENDDO

   RETURN lResult

METHOD wvtMenu:DelItem( nItemNum )

   LOCAL lResult := .F.

   IF nItemNum > 0 .AND. nItemNum <= ::NumItems()
      IF ::aItems[ nItemNum ][ WVT_MENU_TYPE ] == MF_POPUP
         ::aItems[ nItemNum ][ WVT_MENU_MENUOBJ ]:Destroy()
      ENDIF

      IF ( lResult := wvt_DeleteMenu( ::hMenu, nItemNum - 1, MF_BYPOSITION ) ) /* Remember ZERO base */
         hb_ADel( ::aItems, nItemNum, .T. )
      ELSE
#if 0
         Throw( ErrorNew( "wvtMenu", 1000, "wvtMenu:DelItem()", "Delete menu item FAILED", { nItemNum } ) )
#endif
      ENDIF
   ENDIF

   RETURN lResult

METHOD wvtMenu:EnableItem( nItemNum )

   LOCAL nPrevious := -1

   IF ! Empty( ::hMenu ) .AND. ! Empty( nItemNum )
      nPrevious := wvt_EnableMenuItem( ::hMenu, nItemNum - 1, MF_BYPOSITION + MF_ENABLED )
   ENDIF

   RETURN nPrevious

METHOD wvtMenu:DisableItem( nItemNum )

   LOCAL nPrevious := -1

   IF ! Empty( ::hMenu ) .AND. ! Empty( nItemNum )
      nPrevious := wvt_EnableMenuItem( ::hMenu, nItemNum - 1, MF_BYPOSITION + MF_GRAYED )
   ENDIF

   RETURN nPrevious

METHOD wvtMenu:NumItems()
   RETURN Len( ::aItems )

METHOD wvtMenu:GetItem( nItemNum )

   LOCAL nItems := ::NumItems(), aResult := NIL

   IF nItemNum > 0 .AND. nItemNum <= nItems
      aResult := ::aItems[ nItemNum ]
   ENDIF

   RETURN aResult

METHOD wvtMenu:FindMenuItemById( nId )

   LOCAL x, aResult := {}

   IF ! Empty( nId )
      x := ::NumItems()
      DO WHILE x > 0 .AND. Empty( aResult )
         IF ::aItems[ x ][ WVT_MENU_TYPE ] == MF_POPUP
            aResult := ::aItems[ x ][ WVT_MENU_MENUOBJ ]:FindMenuItemById( nId )
         ELSEIF ::aItems[ x ][ WVT_MENU_IDENTIFIER ] == nId
            aResult := ::aItems[ x ]
         ENDIF
         x--
      ENDDO
   ENDIF

   RETURN aResult

METHOD PROCEDURE wvtMenu:DrawMenuBar()

   wvt_DrawMenuBar()

   RETURN

/* Class WvtConsole */
CREATE CLASS WvtConsole INHERIT WvtObject

   METHOD New( oParent )
   METHOD Say( nRow, nCol, xExp, cColor )
   METHOD Box( nRow, nCol, n2Row, n2Col, cBoxChars, cColor )

ENDCLASS

METHOD WvtConsole:New( oParent )

   ::Super:New( oParent, DLG_OBJ_CONSOLE, , -1, -1, -1, -1 )

   RETURN Self

METHOD WvtConsole:Say( nRow, nCol, xExp, cColor )

   LOCAL nCRow, nCCol, nCursor

   IF nRow >= 0 .AND. nCol >= 0 .AND. xExp != NIL
      nCursor := SetCursor( SC_NONE )
      nCRow   := Row()
      nCCol   := Col()
      hb_DispOutAt( nRow, nCol, xExp, cColor )
      SetPos( nCRow, nCCol )
      SetCursor( nCursor )
   ENDIF

   RETURN Self

METHOD WvtConsole:Box( nRow, nCol, n2Row, n2Col, cBoxChars, cColor )

   LOCAL nCRow, nCCol, nCursor

   IF nRow >= 0 .AND. nCol >= 0
      nCursor := SetCursor( SC_NONE )
      nCRow   := Row()
      nCCol   := Col()
      hb_DispBox( nRow, nCol, n2Row, n2Col, cBoxChars, cColor )
      SetPos( nCRow, nCCol )
      SetCursor( nCursor )
   ENDIF

   RETURN Self

/* TBrowseWvg From TBrowse */
#define _TBCI_COLOBJECT       1   /* column object                          */
#define _TBCI_COLWIDTH        2   /* width of the column                    */
#define _TBCI_COLPOS          3   /* column position on screen              */
#define _TBCI_CELLWIDTH       4   /* width of the cell                      */
#define _TBCI_CELLPOS         5   /* cell position in column                */
#define _TBCI_COLSEP          6   /* column separator                       */
#define _TBCI_SEPWIDTH        7   /* width of the separator                 */
#define _TBCI_HEADING         8   /* column heading                         */
#define _TBCI_FOOTING         9   /* column footing                         */
#define _TBCI_HEADSEP         10  /* heading separator                      */
#define _TBCI_FOOTSEP         11  /* footing separator                      */
#define _TBCI_DEFCOLOR        12  /* default color                          */
#define _TBCI_FROZENSPACE     13  /* space after frozen columns             */
#define _TBCI_LASTSPACE       14  /* space after last visible column        */
#define _TBCI_SIZE            14  /* size of array with TBrowse column data */

CREATE CLASS TBrowseWvg INHERIT TBrowse

   VAR    aColumnsSep                             INIT {}

   METHOD SetVisible()

ENDCLASS

METHOD TBrowseWvg:SetVisible()

   LOCAL lFirst, aCol, nColPos

   ::Super:SetVisible()
   ::aColumnsSep := {}

   lFirst := .T.
   FOR EACH aCol IN ::aColData
      IF aCol[ _TBCI_COLPOS ] != NIL
         IF lFirst
            lFirst := .F.

         ELSE
            nColPos := aCol[ _TBCI_COLPOS ]

            IF aCol[ _TBCI_SEPWIDTH ] > 0
               nColPos += Int( aCol[ _TBCI_SEPWIDTH ] / 2 )
            ENDIF

            AAdd( ::aColumnsSep, nColPos )
         ENDIF
      ENDIF
   NEXT

   RETURN Self
