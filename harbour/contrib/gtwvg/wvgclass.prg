/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * GUI Components
 * Copyright 2007 Pritpal Bedi <pritpal@vouchcac.com>
 * Based On:
 *
 * Video subsystem for Windows using GUI windows instead of Console
 *     Copyright 2003 Peter Rees <peter@rees.co.nz>
 *                    Rees Software & Systems Ltd
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option )
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.   If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/ ).
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
 * not apply to the code that you add in this way.   To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                            Wvt*Classes
 *                               2004
 *                Pritpal Bedi <pritpal@vouchcac.com>
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include                 "hbclass.ch"
#include                   "inkey.ch"
#include                  "common.ch"
#include                 "setcurs.ch"

#include                  "wvtwin.ch"

/*----------------------------------------------------------------------*/

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

/*----------------------------------------------------------------------*/

#define OBJ_CHILD_OBJ             1
#define OBJ_CHILD_EVENTS          2
#define OBJ_CHILD_DATABLOCK       3
#define OBJ_CHILD_REFRESHBLOCK    4

/*----------------------------------------------------------------------*/
/*
 *                         Class WvtDialog
 */
/*----------------------------------------------------------------------*/

CLASS wvtDialog

   /*  To hold previous settings  */
   DATA   nOldRows
   DATA   nOldCols
   DATA   aOldFont
   DATA   cOldTitle
   DATA   cOldColor
   DATA   nOldCursor
   DATA   aPalette
   DATA   cScreen
   DATA   aWvtScreen
   DATA   aOldPnt
   DATA   oldTooltipActive
   DATA   oldTooltipWidth
   DATA   oldTooltipBkColor
   DATA   oldTooltipTextColor
   DATA   oldMenuHandle
   DATA   oldMenuBlock
   DATA   lGui

   /*  Dialog Parameters  */
   DATA   nRows
   DATA   nCols
   DATA   cFont
   DATA   nFontHeight
   DATA   nFontWidth
   DATA   nFontBold
   DATA   nFontQuality
   DATA   cTitle
   DATA   cColor

   /*  Objects handelling  */
   DATA   aObjects                                INIT {}
   DATA   oCurObj
   DATA   oLastObj
   DATA   oObjOver
   DATA   oLastOver
   DATA   nCurObj                                 INIT 1
   DATA   nLastObj                                INIT 0
   DATA   nObjOver                                INIT 0
   DATA   nLastOver                               INIT -1
   DATA   nUseObj
   DATA   oMenu
   DATA   aDialogKeys                             INIT {}
   DATA   cDialogID                               INIT ""

   /*  Tooltip Management  */
   DATA   nTooltipWidth
   DATA   nTooltipBkColor
   DATA   nTooltipTextColor

   /*  Miscellaneous  */
   DATA   ClassName                               INIT "WVTDIALOG"
   DATA   cPaintBlockID
   DATA   nPaintID                                INIT 1
   DATA   nObjID                                  INIT 5000
   DATA   nKey
   DATA   hFonts                                  INIT {}
   DATA   lEventHandled
   DATA   lTabStops                               INIT .f.
   DATA   bOnCreate

   ACCESS nObjects                                INLINE len( ::aObjects )

   METHOD New( nRows, nCols, cTitle, cFont, nFontHeight, nFontWidth,nFontBold,nFontQuality )
   METHOD Create()
   METHOD Destroy()
   METHOD Event()
   METHOD Execute()
   METHOD Inkey()
   METHOD MouseOver()
   METHOD Update()
   METHOD CreateObjects()
   METHOD Eval( bBlock, p1,p2,p3,p4,p5 )
   METHOD ActivateMenu()

   METHOD AddObject( oObject )                    INLINE aadd( ::aObjects, oObject )
   METHOD MaxRow()                                INLINE ::nRows - 1
   METHOD MaxCol()                                INLINE ::nCols - 1
   METHOD OnTimer()                               INLINE aeval( ::aObjects, {|o| o:OnTimer() } )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD wvtDialog:New( nRows, nCols, cTitle, cFont, nFontHeight, nFontWidth,nFontBold,nFontQuality )
   LOCAL fnt_:= Wvt_GetFontInfo()

   DEFAULT nRows         TO 25
   DEFAULT nCols         TO 80
   DEFAULT cTitle        TO Wvt_GetTitle()
   DEFAULT cFont         TO fnt_[ 1 ]
   DEFAULT nFontHeight   TO fnt_[ 2 ]
   DEFAULT nFontWidth    TO fnt_[ 3 ]
   DEFAULT nFontBold     TO fnt_[ 4 ]
   DEFAULT nFontQuality  TO fnt_[ 5 ]

   IF empty( cFont )
      cFont := fnt_[ 1 ]
   ENDIF
   IF empty( nFontHeight )
      nFontHeight := fnt_[ 2 ]
   ENDIF
   IF empty( nFontWidth )
      nFontWidth := fnt_[ 3 ]
   ENDIF

   ::nOldRows            := MaxRow()+1
   ::nOldCols            := MaxCol()+1
   ::aOldFont            := Wvt_GetFontInfo()
   ::cOldTitle           := Wvt_GetTitle()
   ::cOldColor           := SetColor()
   ::nOldCursor          := SetCursor()
   ::aPalette            := Wvt_GetPalette()

   ::oldMenuHandle       := Wvt_GetMenu()
   ::oldMenuBlock        := SetKey( Wvt_SetMenuKeyEvent() )

   ::oldTooltipWidth     := Wvt_GetTooltipWidth()
   ::oldTooltipBkColor   := Wvt_GetTooltipBkColor()
   ::oldTooltipTextColor := Wvt_GetTooltipTextColor()

   ::nRows               := nRows
   ::nCols               := nCols
   ::cTitle              := cTitle
   ::cFont               := cFont
   ::nFontHeight         := nFontHeight
   ::nFontWidth          := nFontWidth
   ::nFontBold           := nFontBold
   ::nFontQuality        := nFontQuality

   ::cPaintBlockID       := strzero( Hb_Random( 99999998 ),8 )
   ::nObjOver            := 0
   ::nKey                := 0
   ::cColor              := "N/W"
   ::nUseObj             := 0
   ::lGui                := Wvt_SetGui( .f. )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD wvtDialog:Create()
   LOCAL aPalette, i, j

   ::oldToolTipActive := Wvt_SetToolTipActive( .t. )
   IF ::nTooltipWidth != nil
      Wvt_setTooltipWidth( ::nTooltipWidth )
   ENDIF
   IF ::nTooltipBkColor != nil
      Wvt_SetTooltipBkColor( ::nTooltipBkColor )
   ENDIF
   IF ::nTooltipTextColor != nil
      Wvt_SetTooltipTextColor( ::nTooltipTextColor )
   ENDIF

   aPalette      := Wvt_GetPalette()
   aPalette[ 9 ] := RGB( 175,175,175 )
   Wvt_SetPalette( aPalette )

   ::cScreen     := SaveScreen( 0, 0, maxrow(), maxcol() )
   ::aWvtScreen  := Wvt_SaveScreen( 0, 0, maxrow(), maxcol() )
   ::aOldPnt     := WvtSetPaint( {} )

   SetMode( ::nRows, ::nCols )
   do while .t.
      IF Wvt_SetFont( ::cFont, ::nFontHeight, ::nFontWidth, ::nFontBold, ::nFontQuality )
         EXIT
      ENDIF
      ::nFontHeight--
   enddo
   /* Wvt_SetFont( ::cFont, ::nFontHeight, ::nFontWidth, ::nFontBold, ::nFontQuality ) */
   SetMode( ::nRows, ::nCols )

   Wvt_SetTitle( ::cTitle )

   SetColor( ::cColor )
   CLS
   ::Eval( ::bOnCreate )

   ::CreateObjects()

   IF len( ::aObjects ) > 0
      ::oCurObj := ::aObjects[ 1 ]
   ENDIF

   FOR i := 1 to len( ::aObjects )
      IF !empty( ::aObjects[ i ]:aPaint )
         FOR j := 1 to len( ::aObjects[ i ]:aPaint )
            wvg_SetPaint( ::cPaintBlockID, ::nPaintID++, ;
                ::aObjects[ i ]:aPaint[ j,1 ], ::aObjects[ i ]:aPaint[ j,2 ] )
         next
      ENDIF
   next
   WvtSetPaint( wvg_GetPaint( ::cPaintBlockID ) )

   IF ascan( ::aObjects, {|o| o:lTabStop } ) > 0
      ::lTabStops := .t.
   ENDIF

   ::Update()

   IF HB_ISOBJECT( ::oMenu )
      Wvt_SetMenu( ::oMenu:hMenu )
      Wvt_DrawMenuBar()
      SetKey( Wvt_SetMenuKeyEvent(), {|| ::ActivateMenu( ::oMenu ) } )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD wvtDialog:Destroy()

   IF HB_ISOBJECT( ::oMenu )
      ::oMenu:Destroy()
   ENDIF

   aeval( ::aObjects, {|o| o:destroy() } )

   Wvt_SetTooltip( 0,0,0,0,"" )
   Wvt_SetTooltipActive( ::oldToolTipActive )
   Wvt_setTooltipWidth( ::oldTooltipWidth )
   Wvt_SetTooltipBkColor( ::oldTooltipBkColor )
   Wvt_SetTooltipTextColor( ::oldTooltipTextColor )

   /*  Here set mode is before setting the font  */
   SetMode( ::nOldRows, ::nOldCols )
   Wvt_SetFont( ::aOldFont[ 1 ], ::aOldFont[ 2 ], ::aOldFont[ 3 ], ::aOldFont[ 4 ], ::aOldFont[ 5 ] )
   Wvt_SetTitle( ::cOldTitle )
   Wvt_SetPalette( ::aPalette )
   Wvt_SetPointer( WVT_IDC_ARROW )
   Wvt_SetMousePos( MRow(), MCol() )

   SetColor( ::cOldColor )
   SetCursor( ::nOldCursor )

   IF ::oldMenuHandle != nil .and. ::oldMenuHandle != 0
      Wvt_SetMenu( ::oldMenuHandle )
   ENDIF
   SetKey( Wvt_SetMenuKeyEvent(), ::oldMenuBlock )
   RestScreen( 0, 0, maxrow(), maxcol(), ::cScreen )
   Wvt_RestScreen( 0, 0 ,maxrow(), maxcol(), ::aWvtScreen )
   wvg_PurgePaint( ::cPaintBlockID )
   WvtSetPaint( ::aOldPnt )
   Wvt_SetGui( ::lGui )


   RETURN nil

/*----------------------------------------------------------------------*/

METHOD wvtDialog:Event()
   LOCAL  nKey

   IF ( nKey := inkey( 0.1, INKEY_ALL + HB_INKEY_GTEVENT ) ) == 0
      IF Wvt_IsLButtonPressed()
         nKey := K_LBUTTONPRESSED
      ENDIF
   ENDIF

   RETURN nKey

/*----------------------------------------------------------------------*/

METHOD wvtDialog:Execute()

   IF ::nObjects == 0
      DO WHILE .t.
         IF inkey( 0.1, INKEY_ALL + HB_INKEY_GTEVENT ) == K_ESC
            EXIT
         ENDIF
      ENDDO
   ELSE
      DO WHILE ( ::Inkey() != K_ESC )
      ENDDO
   ENDIF

   RETURN ::nKey

/*----------------------------------------------------------------------*/

METHOD wvtDialog:Inkey()
   LOCAL  n, oObj, nID, i

   ::lEventHandled := .f.
   ::nUseObj       := 0

   ::nKey := ::Event()
   ::OnTimer()

   IF ::nKey != 0
      IF ::nKey == K_ESC .or. ::nKey == K_CTRL_ENTER
         RETURN K_ESC
      ENDIF

      DO CASE

      CASE ::nKey == K_TAB
         IF ::lTabStops
            DO WHILE .t.
               ::nCurObj++
               IF ::nCurObj > ::nObjects
                  ::nCurObj := 1
               ENDIF
               IF ::aObjects[ ::nCurObj ]:lTabStop
                  EXIT
               ENDIF
            ENDDO
         ENDIF

         ::lEventHandled := .t.

      CASE ::nKey == K_SH_TAB
         IF ::lTabStops
            DO WHILE .t.
               ::nCurObj--
               IF ::nCurObj < 1
                  ::nCurObj := ::nObjects
               ENDIF
               IF ::aObjects[ ::nCurObj ]:lTabStop
                  EXIT
               ENDIF
            ENDDO
         ENDIF

         ::lEventHandled := .t.

      CASE ::nKey == K_MOUSEMOVE .or. ::nKey == K_MMLEFTDOWN
         ::MouseOver()
         IF ::nObjOver == 0
            Wvt_SetPointer( WVT_IDC_ARROW )
         ELSEIF ::oObjOver:nPointer != nil .and. ::oObjOver:lActive
            Wvt_SetPointer( ::oObjOver:nPointer )
         ELSE
            Wvt_SetPointer( WVT_IDC_ARROW )
         ENDIF
         ::lEventHandled := .t.

      ENDCASE

      IF    ::nKey == K_LBUTTONDOWN     .or. ;
            ::nKey == K_LBUTTONUP       .or. ;
            ::nKey == K_LDBLCLK         .or. ;
            ::nKey == K_MMLEFTDOWN      .or. ;
            ::nKey == K_LBUTTONPRESSED  .or. ;
            ::nKey == K_RBUTTONDOWN

         ::MouseOver()

         IF ::nObjOver > 0
            IF    ::aObjects[ ::nObjOver ]:nType == DLG_OBJ_BUTTON     .or. ;
                  ::aObjects[ ::nObjOver ]:nType == DLG_OBJ_TOOLBAR    .or. ;
                  ::aObjects[ ::nObjOver ]:nType == DLG_OBJ_PUSHBUTTON .or. ;
                  ::aObjects[ ::nObjOver ]:nType == DLG_OBJ_SCROLLBAR

               oObj := ::aObjects[ ::nObjOver ]
               IF oObj:oParent:ClassName == "WVTBROWSE"
                  nID := oObj:oParent:nID
                  IF ( n := ascan( ::aObjects, {|o| o:nID == nID } ) ) > 0
                     ::nCurObj := n
                  ENDIF
               ENDIF
            ELSE
               ::nCurObj := ::nObjOver
            ENDIF
            ::nUseObj := ::nObjOver

         ELSE
            ::lEventHandled := .t.

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
            Wvt_SetTooltip( 0,0,0,0,"" )

         ELSEIF ::oObjOver:lActive
            ::oObjOver:SetTooltip()

         ELSE
            Wvt_SetTooltip( 0,0,0,0,"" )

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
            Select( ::oCurObj:cAlias )

         ENDIF

         ::Eval( ::oCurObj:bOnFocus, ::oCurObj )
      ENDIF

      IF ::nKey == K_LBUTTONDOWN
         IF ::nUseObj > 0
            IF !( ::lEventHandled := ::aObjects[ ::nUseObj ]:LeftDown() )
               ::lEventHandled := ::Eval( ::aObjects[ ::nUseObj ]:bOnLeftDown )
               IF ::aObjects[ ::nUseObj ]:className == "WVTBROWSE"
                  ::lEventHandled := .f.
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

      IF ::nKey == K_RBUTTONDOWN .and. ::nUseObj > 0
         ::lEventHandled := ::aObjects[ ::nUseObj ]:ShowPopup()
      ENDIF

      IF !( ::lEventHandled )
         IF ::nCurObj > 0
            IF !empty( ::aDialogKeys )
               IF ( n := ascan( ::aDialogKeys, {|e_| e_[ 1 ] == ::nKey } ) ) > 0
                  Eval( ::aDialogKeys[ n, 2 ], self, ::oCurObj )
               ENDIF
            ENDIF

            ::lEventHandled := ::oCurObj:HandleEvent( ::nKey )

            IF ( ::lEventHandled )
               IF ::oCurObj:nChildren > 0
                  FOR i := 1 to ::oCurObj:nChildren
                     IF ascan( ::oCurObj:aChildren[ i, OBJ_CHILD_EVENTS ],::nKey ) > 0
                        ::oCurObj:NotifyChild( i, ::nKey, ::oCurObj )
                     ENDIF
                  next
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      IF !( ::lEventHandled )
         IF HB_ISBLOCK( SetKey( ::nKey ) )
            Eval( SetKey( ::nKey ) )
         ENDIF
      ENDIF
   ENDIF

   RETURN ::nKey

/*----------------------------------------------------------------------*/

METHOD wvtDialog:MouseOver()
   LOCAL mRow := MRow()
   LOCAL mCol := MCol()
   LOCAL nObj

   nObj := ascan( ::aObjects, ;
                    {|o| o:nType != DLG_OBJ_STATIC               .AND. ;
                         o:nType != DLG_OBJ_TOOLBAR              .AND. ;
                         mRow >= o:nTop  .AND. mRow <= o:nBottom .AND. ;
                         mCol >= o:nLeft .AND. mCol <= o:nRight      } )

   ::nObjOver := nObj
   ::oObjOver := iif( nObj > 0, ::aObjects[ nObj ], nil )
   IF nObj > 0
      ::aObjects[ nObj ]:nmRow := mRow
      ::aObjects[ nObj ]:nmCol := mCol

   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD wvtDialog:Update()

   Wvt_InvalidateRect( 0, 0, ::maxrow(), ::maxcol() )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD wvtDialog:CreateObjects()
   LOCAL i, nObjs

   nObjs := len( ::aObjects )

   FOR i := 1 to nObjs
      switch ::aObjects[ i ]:nType

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
   /*
      CASE DLG_OBJ_SCROLLBAR
         ::aObjects[ i ]:Create()
         EXIT
   */
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
      end
   next

   RETURN self

/*----------------------------------------------------------------------*/

METHOD wvtDialog:Eval( bBlock, p1,p2,p3,p4,p5 )
   LOCAL lRet

   IF ( lRet := HB_ISBLOCK( bBlock ) )
      eval( bBlock, p1,p2,p3,p4,p5 )
   ENDIF

   RETURN lRet

/*----------------------------------------------------------------------*/

METHOD wvtDialog:ActivateMenu()
   LOCAL nMenu:= Wvt_GetLastMenuEvent()
   LOCAL aMenuItem

   IF !EMPTY( nMenu )
      IF HB_ISOBJECT( ::oMenu )
         IF !EMPTY( aMenuItem := ::oMenu:FindMenuItemById( nMenu ) )
            IF HB_ISBLOCK( aMenuItem[ WVT_MENU_ACTION ] )
               EVAL( aMenuItem[ WVT_MENU_ACTION ] )
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                         Class WvtObject
 *
 * Must never be used directly. It is parent class FOR all other objects!
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

CLASS WvtObject

   DATA   oParent
   DATA   nType
   DATA   nId

   DATA   nTop
   DATA   nLeft
   DATA   nBottom
   DATA   nRight
   DATA   aPxlTLBR                                INIT {}

   DATA   aObjects                                INIT {}
   DATA   aParent                                 INIT {}
   DATA   aChildren                               INIT {}
   DATA   aPaint                                  INIT {}
   DATA   bPaint
   DATA   ClassName                               INIT ""

   DATA   nObjID                                  INIT 900000
   DATA   nPointer
   DATA   cargo
   DATA   xSettings
   DATA   cText
   DATA   cToolTip
   DATA   lActive                                 INIT .t.
   DATA   lAnimate                                INIT .f.
   DATA   lTabStop                                INIT .t.
   DATA   hFont

   DATA   aPopup                                  INIT {}
   DATA   hPopup                                  INIT nil
   DATA   nPopupItemID                            INIT 700000

   DATA   nMRow                                   INIT 0
   DATA   nMCol                                   INIT 0
   DATA   cColorHilite                            INIT "W+/B*"
   DATA   cColorDehilite                          INIT "W/N*"

   DATA   nTextColor
   DATA   nBackColor
   DATA   nBackMode                               INIT 0 /* OPAQUE 1-TRANSPARENT */
   DATA   nTextColorHoverOn
   DATA   nTextColorHoverOff
   DATA   nBackColorHoverOn
   DATA   nBackColorHoverOff
   DATA   cFont
   DATA   nFontHeight
   DATA   nFontWidth
   DATA   nFontWeight
   DATA   nFontQuality
   DATA   nCharSet
   DATA   lItalic
   DATA   lUnderline
   DATA   lStrikeOut
   DATA   nAlignHorz
   DATA   nAlignVert
   DATA   nAngle

   ACCESS ToolTip                                 INLINE iif( ::cTooltip == nil, "", ::cTooltip )
   ASSIGN ToolTip( cTip )                         INLINE ::cToolTip := cTip

   DATA   bHandleEvent
   DATA   bOnCreate                               INIT   {|| NIL }
   DATA   bOnSelect                               INIT   {|| NIL }
   DATA   bOnFocus                                INIT   {|| NIL }
   DATA   bOnRefresh                              INIT   {|| NIL }
   DATA   bOnLeftUp                               INIT   {|| NIL }
   DATA   bOnLeftDown                             INIT   {|| .f. }
   DATA   bOnMMLeftDown                           INIT   {|| NIL }
   DATA   bOnLeftPressed                          INIT   {|| NIL }
   DATA   bTooltip                                INIT   {|| NIL }
   DATA   bSaveSettings                           INIT   {|| NIL }
   DATA   bRestSettings                           INIT   {|| NIL }
   DATA   bOnHilite                               INIT   {|| NIL }
   DATA   bOnDeHilite                             INIT   {|| NIL }

   ACCESS nChildren                               INLINE len( ::aChildren )
   DATA   nIndexOrder

   METHOD New( oParent, nType, nID, nTop, nLeft, nBottom, nRight )
   METHOD Create()
   METHOD Destroy()
   METHOD CreatePopup()
   METHOD ShowPopup()

   METHOD SetToolTip()                            INLINE Wvt_SetToolTip( ::nTop, ::nLeft, ::nBottom, ::nRight, ::Tooltip )
   METHOD Refresh()                               INLINE Wvt_InvalidateRect( ::nTop, ::nLeft, ::nTop, ::nLeft )
   METHOD Eval( bBlock )                          INLINE iif( HB_ISBLOCK( bBlock ), Eval( bBlock, self ), nil )
   METHOD AddChild( aChild )                      INLINE aadd( ::aChildren, aChild )
   METHOD AddParent( aParent )                    INLINE aadd( ::aParent, aParent )

   METHOD PaintBlock()                            INLINE nil
   METHOD Hilite()                                INLINE nil
   METHOD DeHilite()                              INLINE nil
   METHOD HandleEvent()                           INLINE .f.
   METHOD LeftDown()                              INLINE .f.
   METHOD LeftUp()                                INLINE .f.
   METHOD MMLeftDown()                            INLINE .f.
   METHOD LeftPressed()                           INLINE .f.
   METHOD HoverOn()                               INLINE nil
   METHOD HoverOff()                              INLINE nil
   METHOD OnTimer()                               INLINE nil
   METHOD SaveSettings()                          INLINE nil
   METHOD RestSettings()                          INLINE nil
   METHOD Activate()                              INLINE nil
   METHOD DeActivate()                            INLINE nil
   METHOD NotifyChild( /*nChild*/ )               INLINE nil

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD WvtObject:New( oParent, nType, nID, nTop, nLeft, nBottom, nRight )

   DEFAULT nID TO ++::nObjID

   ::oParent   :=  oParent
   ::nType     :=  nType
   ::nId       :=  nID
   ::nTop      :=  nTop
   ::nLeft     :=  nLeft
   ::nBottom   :=  nBottom
   ::nRight    :=  nRight

   switch nType

   CASE DLG_OBJ_BROWSE
      ::ClassName := "WVTBROWSE"
      EXIT

   CASE DLG_OBJ_STATIC
      ::ClassName := "WVTSTATIC"
      ::lTabStop  := .f.
      EXIT

   CASE DLG_OBJ_GETS
      ::ClassName := "WVTGETS"
      EXIT

   CASE DLG_OBJ_IMAGE
      ::ClassName := "WVTIMAGE"
      ::lTabStop  := .f.
      EXIT

   CASE DLG_OBJ_PUSHBUTTON
      ::ClassName := "WVTPUSHBUTTON"
      EXIT

   CASE DLG_OBJ_BUTTON
      ::ClassName := "WVTBUTTON"
      ::lTabStop  := .f.
      EXIT

   CASE DLG_OBJ_TOOLBAR
      ::ClassName := "WVTTOOLBAR"
      ::lTabStop  := .f.
      EXIT

   CASE DLG_OBJ_LABEL
      ::ClassName := "WVTLABEL"
      ::lTabStop  := .f.
      EXIT

   CASE DLG_OBJ_SCROLLBAR
      ::ClassName := "WVTSCROLLBAR"
      ::lTabStop  := .f.
      EXIT

   CASE DLG_OBJ_STATUSBAR
      ::ClassName := "WVTSTATUSBAR"
      ::lTabStop  := .f.
      EXIT

   CASE DLG_OBJ_BANNER
      ::ClassName := "WVTBANNER"
      ::lTabStop  := .f.
      EXIT

   CASE DLG_OBJ_TEXTBOX
      ::ClassName := "WVTTEXTBOX"
      ::lTabStop  := .f.
      EXIT

   CASE DLG_OBJ_PROGRESSBAR
      ::ClassName := "WVTPROGRESSBAR"
      ::lTabStop  := .f.
      EXIT

   end

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtObject:Create()

   ::Eval( ::bOnCreate )
   ::CreatePopup()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtObject:Destroy()

   IF ::hFont != nil
      WVG_DeleteObject( ::hFont )
      ::hFont := nil
   ENDIF

   IF ::hPopup != nil
      Wvt_DestroyMenu( ::hPopup )
      ::hPopup := nil
   ENDIF

   RETURN Nil

/*----------------------------------------------------------------------*/

METHOD WvtObject:CreatePopup()
   LOCAL i, nID

   IF !empty( ::aPopup ) .and. ::hPopup == nil
      ::hPopup := Wvt_CreatePopupMenu()

      FOR i := 1 to len( ::aPopup )

         aSize( ::aPopup[ i ],3 )
         nID := ::nPopupItemID++
         ::aPopup[ i,3 ] := nID

         Wvt_AppendMenu( ::hPopup, MF_ENABLED + MF_STRING, nID, ::aPopup[ i,1 ] )
      next
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtObject:ShowPopup()
   LOCAL lRet := .f., nRet, n, aPos

   IF ::hPopup != nil
      aPos := Wvt_GetCursorPos()

      nRet := Wvt_TrackPopupMenu( ::hPopup, TPM_CENTERALIGN +TPM_RETURNCMD, ;
                             aPos[ 1 ], aPos[ 2 ], 0, Wvt_GetWindowHandle() )
      IF nRet > 0
         IF ( n := ascan( ::aPopup, {|e_| e_[ 3 ] == nRet } ) ) > 0
            lRet := .t.

            IF HB_ISBLOCK( ::aPopup[ n,2 ] )
               Eval( ::aPopup[ n,2 ] )
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   RETURN lRet

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                         Class WvtBrowse
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

CLASS WvtBrowse FROM WvtObject

   DATA   cAlias
   DATA   oBrw
   DATA   lHSBar                                  INIT .t.
   DATA   lVSBar                                  INIT .t.
   DATA   oHBar
   DATA   oVBar
   DATA   bTotalRecords
   DATA   bCurrentRecord
   DATA   bTotalColumns
   DATA   bCurrentColumn

   ACCESS cDesc                                   INLINE iif( ::cText == nil, "", ::cText )
   ASSIGN cDesc( cText )                          INLINE ::cText := cText

   METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight )
   METHOD Create()
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

/*----------------------------------------------------------------------*/

METHOD WvtBrowse:New( oParent, nID, nTop, nLeft, nBottom, nRight )

   ::Super:New( oParent, DLG_OBJ_BROWSE, nID, nTop, nLeft, nBottom, nRight )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtBrowse:Create()

   Select( ::cAlias )
#if 0
   ::nTop    := ::oBrw:nTop-2
   ::nLeft   := ::oBrw:nLeft-2
   ::nBottom := iif( ::lHSBar, ::oBrw:nBottom, ::oBrw:nBottom+1 )
   ::nRight  := iif( ::lVSBar, ::oBrw:nRight , ::oBrw:nRight+2  )
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

   DEFAULT ::bTotalRecords  TO {|| ( ::cAlias )->( OrdKeyCount() ) }
   DEFAULT ::bCurrentRecord TO {|| ( ::cAlias )->( OrdKeyNo()    ) }
   ::SetVBar()

   DEFAULT ::bTotalColumns  TO {|| ::oBrw:ColCount }
   DEFAULT ::bCurrentColumn TO {|| ::oBrw:ColPos   }
   ::SetHBar()

   ::oBrw:ForceStable()
   ::DeHilite()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtBrowse:SetVBar()

   IF ::lVSBar
      ::oVBar := WvtScrollBar():New( self, 999991, ;
                 ::oBrw:nTop, ::oBrw:nRight+1, ::oBrw:nBottom, ::oBrw:nRight+2 )
      ::oVBar:nBarType   := WVT_SCROLLBAR_VERT
      ::oVBar:bTotal     := ::bTotalRecords
      ::oVBar:bCurrent   := ::bCurrentRecord
      ::oVBar:aPxlBtnTop := { -2,2,0,0 }
      ::oVBar:aPxlBtnBtm := {  0,2,2,0 }
      ::oVBar:aPxlScroll := {  0,2,0,0 }
      ::oVBar:Create()

      aadd( ::aPaint, { ::oVBar:bBtnLeftTop, ;
          { WVT_BLOCK_BUTTON, ::oVBar:nBtn1Top, ::oVBar:nBtn1Left, ;
                ::oVBar:nBtn1Bottom, ::oVBar:nBtn1Right } } )

      aadd( ::aPaint, { ::oVBar:bBtnRightBottom, ;
          { WVT_BLOCK_BUTTON, ::oVBar:nBtn2Top, ::oVBar:nBtn2Left, ;
                ::oVBar:nBtn2Bottom, ::oVBar:nBtn2Right } } )

      aadd( ::aPaint, { ::oVBar:bBtnScroll, ;
          { WVT_BLOCK_BUTTON, ::oVBar:nSTop, ::oVBar:nSLeft, ;
                ::oVBar:nSBottom, ::oVBar:nSRight } } )

      ::oParent:AddObject( ::oVBar )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtBrowse:SetHBar()

   IF ::lHSBar
      ::oHBar := WvtScrollBar():New( self, 999990, ;
                 ::oBrw:nBottom+1, ::oBrw:nLeft, ::oBrw:nBottom+1, ::oBrw:nRight )
      ::oHBar:nBarType   := 2
      ::oHBar:bTotal     := ::bTotalColumns
      ::oHBar:bCurrent   := ::bCurrentColumn
      ::oHBar:aPxlBtnLft := { 2,-2,0,0 }
      ::oHBar:aPxlBtnRgt := { 2, 0,0,2 }
      ::oHBar:aPxlScroll := { 2, 0,0,0 }
      ::oHBar:Create()

      aadd( ::aPaint, { ::oHBar:bBtnLeftTop, ;
          { WVT_BLOCK_BUTTON, ::oHBar:nBtn1Top, ::oHBar:nBtn1Left, ;
                       ::oHBar:nBtn1Bottom, ::oHBar:nBtn1Right } } )
      aadd( ::aPaint, { ::oHBar:bBtnRightBottom, ;
          { WVT_BLOCK_BUTTON, ::oHBar:nBtn2Top, ::oHBar:nBtn2Left, ;
                       ::oHBar:nBtn2Bottom, ::oHBar:nBtn2Right } } )
      aadd( ::aPaint, { ::oHBar:bBtnScroll, ;
          { WVT_BLOCK_BUTTON, ::oHBar:nSTop, ::oHBar:nSLeft, ;
                       ::oHBar:nSBottom, ::oHBar:nSRight } } )

      ::oParent:AddObject( ::oHBar )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtBrowse:Refresh()
   LOCAL nWorkArea := Select()

   IF HB_ISBLOCK( ::bOnRefresh )
      eval( ::bOnRefresh, self )
   ELSE
      Select( ::cAlias )

      ::oBrw:RefreshAll()
      ::oBrw:ForceStable()

      Select( nWorkArea )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtBrowse:HandleEvent( nKey )
   LOCAL lRet := .f.

   IF valtype( ::bHandleEvent ) == "B"
      lRet := eval( ::bHandleEvent, self, ::oParent:cPaintBlockID, ::oBrw, nKey )
   ENDIF

   RETURN lRet

/*----------------------------------------------------------------------*/

METHOD WvtBrowse:NotifyChild( nIndex, nKey, oCurObj )
   LOCAL xData, i

   IF nIndex > 0 .and. nIndex <= len( ::aChildren )
      IF valtype( ::aChildren[ nIndex, OBJ_CHILD_DATABLOCK ] ) == "B"
         xData := eval( ::aChildren[ nIndex, OBJ_CHILD_DATABLOCK ] )
      ENDIF

      eval( ::aChildren[ nIndex, OBJ_CHILD_REFRESHBLOCK ], ;
            ::aChildren[ nIndex, OBJ_CHILD_OBJ ],;
            ::aChildren[ nIndex, OBJ_CHILD_OBJ ]:oParent:cPaintBlockID, ;
            ::aChildren[ nIndex, OBJ_CHILD_OBJ ]:oBrw, ;
            nKey, ;
            xData )

      IF ::aChildren[ nIndex, OBJ_CHILD_OBJ ]:nChildren > 0
         /* Pretend IF focus is current on this object */
         Eval( ::aChildren[ nIndex, OBJ_CHILD_OBJ ]:bOnFocus, ::aChildren[ nIndex, OBJ_CHILD_OBJ ] )

         FOR i := 1 to ::aChildren[ nIndex, OBJ_CHILD_OBJ ]:nChildren
            ::aChildren[ nIndex, OBJ_CHILD_OBJ ]:NotifyChild( i, nKey, ::aChildren[ nIndex, OBJ_CHILD_OBJ ] )
         next

         /* Restore previous environments */
         Eval( oCurObj:bOnFocus, oCurObj )
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtBrowse:Hilite()
   LOCAL b := ::oBrw

   DispOutAt( b:nTop-2, b:nLeft-2, pad( " "+::cDesc, b:nRight-b:nLeft+5 ), ::cColorHilite )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtBrowse:DeHilite()
   LOCAL b := ::oBrw

   DispOutAt( b:nTop-2, b:nLeft-2, pad( " "+::cDesc, b:nRight-b:nLeft+5 ), ::cColorDeHilite )

   RETURN Self

/*----------------------------------------------------------------------*/

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

   IF cTip != nil
      ::Tooltip := cTip
   ENDIF

   Wvt_SetTooltip( ::nTop, ::nLeft, ::nBottom, ::nRight, ::Tooltip )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtBrowse:SaveSettings()

   IF HB_ISBLOCK( ::bSaveSettings )
      ::xSettings := Eval( ::bSaveSettings, self )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtBrowse:RestSettings()

   IF ::xSettings != nil .and. HB_ISBLOCK( ::bRestSettings )
      Eval( ::bRestSettings, self )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtBrowse:PaintBlock( nPaintObj )
   LOCAL bBlock, b := ::oBrw

   switch nPaintObj

   CASE 1
      bBlock := {|| Wvt_DrawBoxRaised( b:nTop-2,b:nLeft-2,b:nBottom+1,b:nRight+2 ) }
      aadd( ::aPaint, { bBlock, { WVT_BLOCK_BOX, b:nTop-3,b:nLeft-3,b:nBottom+2,b:nRight+3 } } )
      EXIT

   CASE 2
      bBlock := {|| Wvt_DrawBoxRecessed( b:nTop,b:nLeft,b:nBottom,b:nRight ) }
      aadd( ::aPaint, { bBlock, { WVT_BLOCK_BOX, b:nTop-1,b:nLeft-1,b:nBottom+1,b:nRight+1 } } )
      EXIT

   CASE 3
      bBlock := {|| Wvt_DrawGridHorz( b:nTop+3, b:nLeft, b:nRight, b:nBottom - b:nTop - 2 ) }
      aadd( ::aPaint, { bBlock, { WVT_BLOCK_GRID_H, b:nTop+4, b:nLeft+1, b:nBottom-1, b:nRight-1 } } )
      EXIT

   CASE 4
      bBlock := {|| Wvt_DrawGridVert( b:nTop, b:nBottom, b:aColumnsSep, len( b:aColumnsSep ) ) }
      aadd( ::aPaint, { bBlock, { WVT_BLOCK_GRID_V, b:nTop+1, b:nLeft+1, b:nBottom-1, b:nRight-1, b } } )
      EXIT

   end

   RETURN Self

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                            WvtStatusBar
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

CLASS WvtStatusBar FROM WvtObject

   DATA   aPanels
   DATA   cColor

   METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight )
   METHOD Create()
   METHOD SetPanels( aPanels )
   METHOD SetText( nPanel, cText, cColor )
   METHOD SetIcon( nPanel, cIconFile )
   METHOD Update( nPanel, cText, cColor )
   METHOD PaintBlock()
   METHOD Refresh()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD WvtStatusBar:New( oParent, nID, nTop, nLeft, nBottom, nRight )

   DEFAULT nTop    TO oParent:MaxRow()
   DEFAULT nLeft   TO 0
   DEFAULT nBottom TO oParent:MaxRow()
   DEFAULT nRight  TO oParent:MaxCol()

   ::Super:New( oParent, DLG_OBJ_STATUSBAR, nID, nTop, nLeft, nBottom, nRight )

   ::cColor  := "N/W"

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtStatusBar:Create()

   ::Refresh()
   ::PaintBlock( DLG_OBJ_STATUSBAR, self )

   ::Super:Create()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtStatusBar:PaintBlock()
   LOCAL a_:= {}, nPanels

   aeval( ::aPanels, {|o| aadd( a_,o:nTop )   , aadd( a_,o:nLeft ), ;
                          aadd( a_,o:nBottom ), aadd( a_,o:nRight ) } )

   a_[ len( a_ ) ]++
   nPanels := len( ::aPanels )

   ::bPaint  := {|| Wvt_DrawStatusBar( nPanels, a_ ) }
   aadd( ::aPaint, { ::bPaint,;
            { WVT_BLOCK_STATUSBAR, ::nTop, ::nLeft, ::nBottom, ::nRight } } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtStatusBar:SetPanels( aPanels )
   LOCAL i, oPanel, nID
   LOCAL nLastCol := ::oParent:MaxCol()

   nID := 200000

   ::aPanels := {}

   oPanel := WvtPanel():New( ::oParent, ++nID, ::nTop, 0 )

   aadd( ::aPanels, oPanel )

   IF aPanels != nil
      FOR i := 1 to len( aPanels )
         IF ::oParent:MaxCol() > aPanels[ i ]
            oPanel := WvtPanel():New( ::oParent, ++nID, ::nTop, aPanels[ i ] )
            aadd( ::aPanels, oPanel )
         ENDIF
      next
   ENDIF

   atail( ::aPanels ):nRight := nLastCol

   FOR i := len( ::aPanels ) - 1 TO 1 STEP -1
      oPanel        := ::aPanels[ i ]
      oPanel:nRight := ::aPanels[ i+1 ]:nLeft
      oPanel:cColor := ::cColor
   next

   RETURN self

/*----------------------------------------------------------------------*/

METHOD WvtStatusBar:Update( nPanel, cText, cColor )
   LOCAL oPanel

   IF nPanel > 0 .and. nPanel <= len( ::aPanels )
      oPanel        := ::aPanels[ nPanel ]
      oPanel:Text   := cText
      oPanel:cColor := iif( cColor == nil, "N/W", cColor )
      oPanel:Refresh()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtStatusBar:SetText( nPanel, cText, cColor )
   LOCAL oPanel

   DEFAULT cColor TO ::cColor

   IF nPanel > 0 .and. nPanel <= len( ::aPanels )
      oPanel        := ::aPanels[ nPanel ]
      oPanel:Text   := cText
      oPanel:cColor := cColor
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtStatusBar:SetIcon( nPanel, cIconFile )

   IF nPanel > 0 .and. nPanel <= len( ::aPanels )
      ::aPanels[ nPanel ]:cIconFile := cIconFile
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtStatusBar:Refresh()
   LOCAL i

   FOR i := 1 to len( ::aPanels )
      ::aPanels[ i ]:Refresh()
   next

   RETURN nil

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                         Class WvtPanel
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

CLASS WvtPanel FROM WvtObject

   DATA   cColor
   DATA   cTxt
   DATA   cIconFile

   ACCESS Text                                    INLINE ::cTxt
   ASSIGN Text( cText )                           INLINE ::cTxt := pad( cText, ::nRight - ::nLeft-2 )

   METHOD New( oParent, nId, nTop, nLeft )
   METHOD Refresh()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD WvtPanel:New( oParent, nId, nTop, nLeft )

   ::Super:New( oParent, DLG_OBJ_PANEL, nId, nTop, nLeft, nTop )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtPanel:Refresh()

   IF ::Text != nil
      DispOutAt( ::nTop, ::nLeft+1, ::Text, ::cColor )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                         Class WvtLabel
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

CLASS WvtLabel FROM WvtObject

   ACCESS Text                                    INLINE iif( ::cText == nil, "", ::cText )
   ASSIGN Text( cTxt )                            INLINE ::cText := iif( cTxt == nil, "", cTxt )

   METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight )
   METHOD Create( lConfg )
   METHOD Configure()
   METHOD Refresh()
   METHOD HoverOn()
   METHOD HoverOff()
   METHOD SetText( ctxt )
   METHOD SetTextColor( nRGB )
   METHOD SetBackColor( nRGB )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD WvtLabel:New( oParent, nID, nTop, nLeft, nBottom, nRight )

   ::Super:New( oParent, DLG_OBJ_LABEL, nID, nTop, nLeft, nBottom, nRight )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtLabel:Create( lConfg )

   DEFAULT lConfg       TO .f.

   DEFAULT ::nBottom    TO ::nTop
   DEFAULT ::nRight     TO ::nLeft + len( ::Text )
   DEFAULT ::nTextColor TO RGB( 0,0,0 )

   ::nTextColorHoverOff := ::nTextColor
   ::nBackColorHoverOff := ::nBackColor

   ::hFont := Wvt_CreateFont( ::cFont, ::nFontHeight, ::nFontWidth, ::nFontWeight, ::lItalic,;
                              ::lUnderline, ::lStrikeout, ::nCharSet, ::nFontQuality, ::nAngle )
   IF ::hFont != 0
      IF !( lConfg )
         ::bPaint := {|| Wvt_DrawLabelObj( ::nTop, ::nLeft, ::nBottom, ::nRight,;
                       ::Text, ::nAlignHorz, ::nAlignVert, ::nTextColor, ::nBackColor, ::hFont ) }
         aadd( ::aPaint, { ::bPaint, { WVT_BLOCK_LABEL, ::nTop, ::nLeft, ::nBottom, ::nRight } } )
      ENDIF
   ENDIF

   ::Super:Create()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtLabel:Refresh()

   Eval( ::bPaint )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtLabel:SetText( cTxt )

   IF valtype( cTxt ) == "C"
      ::Text := cTxt
      ::Refresh()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtLabel:SetTextColor( nRGB )

   IF valtype( nRGB ) == "N"
      ::nTextColor := nRGB
      ::nTextColorHoverOff := nRGB
      ::Refresh()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtLabel:SetBackColor( nRGB )

   IF valtype( nRGB ) == "N"
      ::nBackColor := nRGB
      ::nBackColorHoverOff := nRGB
      ::Refresh()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtLabel:Configure()

   ::nTextColorHoverOff := ::nTextColor
   ::nBackColorHoverOff := ::nBackColor

   IF ::hFont != 0
      WVG_DeleteObject( ::hFont )
   ENDIF

   ::hFont := Wvt_CreateFont( ::cFont, ::nFontHeight, ::nFontWidth, ::nFontWeight, ::lItalic,;
                              ::lUnderline, ::lStrikeout, ::nCharSet, ::nFontQuality, ::nAngle )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtLabel:HoverOn()
   LOCAL lOn := .f.

   IF ::nTextColorHoverOn != nil
      lOn := .t.
      ::nTextColor := ::nTextColorHoverOn
   ENDIF
   IF ::nBackColorHoverOn != nil
      lOn := .t.
      ::nBackColor := ::nBackColorHoverOn
   ENDIF

   IF lOn
      ::Refresh()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtLabel:HoverOff()
   LOCAL lOn := .f.

   IF ::nTextColorHoverOn != nil
      lOn := .t.
      ::nTextColor := ::nTextColorHoverOff
   ENDIF
   IF ::nBackColorHoverOn != nil
      lOn := .t.
      ::nBackColor := ::nBackColorHoverOff
   ENDIF

   IF lOn
      ::Refresh()
   ENDIF

   Return Self

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                       Class WvtToolBar
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

CLASS WvtToolBar FROM WvtObject

   DATA   nPaintID
   DATA   aObjects                                INIT {}
   DATA   lHidden                                 INIT .f.
   DATA   nCurButton                              INIT 0
   DATA   lActive
   DATA   lFloating
   DATA   wScreen
   DATA   cScreen
   DATA   nBtnLeft                                INIT 0
   DATA   nRGBSep                                 INIT RGB( 150,150,150 )

   ACCESS nButtons                                INLINE len( ::aButtons )

   METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight )
   METHOD Create()
   METHOD Refresh()
   METHOD AddButton( cFileImage, bBlock, cTooltip )
   METHOD PaintToolBar()
   METHOD HoverOn()
   METHOD HoverOff()

ENDCLASS

/*----------------------------------------------------------------------*/

METHOD WvtToolBar:New( oParent, nID, nTop, nLeft, nBottom, nRight )

   nTop    := 0
   nLeft   := 0
   DEFAULT nBottom TO 1
   nRight  := oParent:MaxCol()

   ::Super:New( oParent, DLG_OBJ_TOOLBAR, nID, nTop, nLeft, nBottom, nRight )

   ::lActive   := .t.
   ::lFloating := .F.
   ::nPaintID  := ::oParent:nPaintID++

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtToolBar:Create()

   IF ::lFloating
      ::lActive := .f.
      ::lHidden := .t.
   ENDIF

   aeval( ::aObjects, {|o| o:lActive := ::lActive } )

   ::bPaint := {|| ::PaintToolBar() }
   aadd( ::aPaint, { ::bPaint,;
            { WVT_BLOCK_TOOLBAR, ::nTop, ::nLeft, ::nBottom, ::nRight } } )

   ::Super:Create()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtToolBar:Refresh()

   IF ::lFloating
      DispBox( ::nTop, ::nLeft, ::nBottom, ::nRight, "         ", "n/w" )
   ELSE
      Wvt_InvalidateRect( ::nTop, ::nLeft, ::nTop, ::nLeft )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtToolBar:PaintToolBar()

   IF ( ::lActive )
      Wvt_DrawLine( ::nTop, ::nLeft, ::nBottom, ::nRight, 0, 1, 2, , , ::nRGBSep )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtToolBar:AddButton( cFileImage, bBlock, cTooltip )
   LOCAL oObj, nCol

   nCol := ( ::nBottom-::nTop+1 ) * 2

   oObj := WvtToolButton():New( self )

   oObj:lActive    := ::lActive
   oObj:nTop       := ::nTop
   oObj:nLeft      := ::nBtnLeft + 1
   oObj:nBottom    := ::nBottom

   IF valtype( cFileImage ) == "C"
      oObj:nBtnType   := TLB_BUTTON_TYPE_IMAGE
      oObj:nRight     := oObj:nLeft + nCol - 1
      oObj:cFileImage := cFileImage
      oObj:bOnLeftUp  := bBlock
      oObj:Tooltip    := cTooltip
   ELSE
      oObj:nBtnType   := TLB_BUTTON_TYPE_SEPARATOR
      oObj:nRight     := oObj:nLeft
   ENDIF

   aadd( ::aObjects, oObj )

   ::nBtnLeft         := oObj:nRight + 1
   ::nCurButton++

   ::oParent:AddObject( oObj )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtToolBar:HoverOn()

   IF ::lFloating .and. ::lHidden
      ::lHidden   := .f.
      ::lActive   := .t.
      #IF 0
      ::cScreen   := SaveScreen( ::nTop, ::nLeft, ::nBottom, ::nRight )
      ::wScreen   := Wvt_SaveScreen( ::nTop, ::nLeft, ::nBottom, ::nRight )
      #ENDIF
      aeval( ::aObjects, {|o| o:lActive := ::lActive } )

      ::Refresh()
   ENDIF

   RETURN self

/*----------------------------------------------------------------------*/

METHOD WvtToolBar:HoverOff()

   IF ::lFloating .and. !( ::lHidden )
      ::lHidden := .t.
      ::lActive := .f.
      aeval( ::aObjects, {|o| o:lActive := ::lActive } )
      #IF 0
      RestScreen( ::nTop, ::nLeft, ::nBottom, ::nRight, ::cScreen )
      Wvt_RestScreen( ::nTop, ::nLeft, ::nBottom, ::nRight, ::wScreen, .f. )
      #ENDIF
      ::Refresh()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                     Class WvtToolButton
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

CLASS WvtToolButton FROM WvtObject

   DATA   cFileImage
   DATA   nCurState             INIT 0
   DATA   nBtnType              INIT TLB_BUTTON_TYPE_IMAGE
   DATA   aPxlOffSet            INIT { 0, -1, -3, 1 }

   METHOD New( oParent )
   METHOD Create()
   METHOD Refresh()
   METHOD LeftDown()
   METHOD LeftUp()
   METHOD HoverOn()
   METHOD HoverOff()
   METHOD PaintButton()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD WvtToolButton:New( oParent )

   ::Super:New( oParent, DLG_OBJ_BUTTON )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtToolButton:Create()

   ::bPaint := {|| ::PaintButton() }
   aadd( ::aPaint, { ::bPaint,;
               { WVT_BLOCK_BUTTON, ::nTop, ::nLeft, ::nBottom, ::nRight }} )

   ::Super:Create()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtToolButton:Refresh()

   IF ::lActive
      Eval( ::bPaint )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtToolButton:PaintButton()

   IF ::lActive
      IF ::nBtnType == TLB_BUTTON_TYPE_IMAGE
         Wvt_DrawImage( ::nTop, ::nLeft, ::nBottom, ::nRight, ::cFileImage, {4,4,-6,-4} )
      ELSE
         Wvt_DrawLine( ::nTop, ::nLeft, ::nBottom, ::nRight, 1, 1, , , , ::oParent:nRGBSep )
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtToolButton:LeftDown()
   LOCAL lRet := .f.

   IF ::lActive .and. ::nBtnType == TLB_BUTTON_TYPE_IMAGE
      Wvt_DrawToolButtonState( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet, 2 )
      lRet := .t.
   ENDIF

   RETURN lRet

/*----------------------------------------------------------------------*/

METHOD WvtToolButton:LeftUp()
   LOCAL lRet := .f.

   IF ::lActive .and. ::nBtnType == TLB_BUTTON_TYPE_IMAGE
      Wvt_DrawToolButtonState( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet, 1 )
      Eval( ::bOnLeftUp )
      lRet := .t.
   ENDIF

   RETURN lRet

/*----------------------------------------------------------------------*/

METHOD WvtToolButton:HoverOn()

   ::oParent:HoverOn()

   IF ::lActive .and. ::nBtnType == TLB_BUTTON_TYPE_IMAGE
      Wvt_DrawToolButtonState( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet, 1 )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtToolButton:HoverOff()

   ::oParent:HoverOff()

   IF ::lActive .and. ::nBtnType == TLB_BUTTON_TYPE_IMAGE
      Wvt_DrawToolButtonState( ::nTop, ::nLeft, ::nBottom, ::nRight,::aPxlOffSet, 0 )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                          Class WvtImage
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

CLASS WvtImage FROM WvtObject

   DATA   cImageFile

   ACCESS cImage                                  INLINE ::cImageFile
   ASSIGN cImage( cImg )                          INLINE ::cImageFile := cImg

   METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight )
   METHOD Create()
   METHOD SetImage( cImage )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD WvtImage:New( oParent, nID, nTop, nLeft, nBottom, nRight )

   ::Super:New( oParent, DLG_OBJ_IMAGE, nId, nTop, nLeft, nBottom, nRight )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtImage:Create()

   ::bPaint := {|| iif( file( ::cImage ), ;
        Wvt_DrawImage( ::nTop, ::nLeft, ::nBottom, ::nRight, ::cImage ),"" ) }

   aadd( ::aPaint, { ::bPaint,;
               { WVT_BLOCK_IMAGE, ::nTop, ::nLeft, ::nBottom, ::nRight } } )

   ::Super:Create()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtImage:SetImage( cImage )

   IF cImage != nil .and. file( cImage )
      ::cImageFile := cImage
      ::Refresh()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                          Class WvtStatic
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

CLASS WvtStatic FROM WvtObject

   DATA   nStatic
   DATA   nOrient
   DATA   nFormat
   DATA   nAlign
   DATA   nStyle
   DATA   nThick
   DATA   nColor

   DATA   nfTop
   DATA   nfLeft
   DATA   nfBottom
   DATA   nfRight

   DATA   nHorzVert                               INIT 0
   DATA   aRGBb
   DATA   aRGBe

   DATA   aPxlOffSet                              INIT {}

   METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight )
   METHOD Create()
   METHOD Refresh()
   METHOD HoverOn()
   METHOD HoverOff()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD WvtStatic:New( oParent, nID, nTop, nLeft, nBottom, nRight )

   ::Super:New( oParent, DLG_OBJ_STATIC, nID, nTop, nLeft, nBottom, nRight )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtStatic:Create()
   LOCAL lInside := .f.

   SWITCH ::nStatic

   CASE WVT_STATIC_LINE
      lInside := .t.
      ::bPaint  := {|| Wvt_DrawLine( ::nTop, ::nLeft, ::nBottom, ::nRight, ;
               ::nOrient, ::nFormat, ::nAlign, ::nStyle, ::nThick, ::nColor ) }
      EXIT

   CASE WVT_STATIC_BOXRAISED
      ::bPaint := {|| Wvt_DrawBoxRaised( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      EXIT

   CASE WVT_STATIC_BOXRECESSED
      ::bPaint := {|| Wvt_DrawBoxRecessed( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      EXIT

   CASE WVT_STATIC_BOXGROUP
      ::bPaint := {|| Wvt_DrawBoxGroup( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      EXIT

   CASE WVT_STATIC_BOXGROUPRAISED
      ::bPaint := {|| Wvt_DrawBoxGroupRaised( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      EXIT

   CASE WVT_STATIC_OUTLINE
      ::bPaint := {|| Wvt_DrawOutline( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      EXIT

   CASE WVT_STATIC_RECTANGLE
      lInside := .t.
      ::bPaint := {|| Wvt_DrawRectangle( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      EXIT

   CASE WVT_STATIC_ROUNDRECT
      lInside := .t.
      ::bPaint := {|| Wvt_DrawRoundRect( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      EXIT

   CASE WVT_STATIC_FOCUSRECT
      lInside := .t.
      ::bPaint := {|| Wvt_DrawFocusRect( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      EXIT

   CASE WVT_STATIC_ELLIPSE
      lInside := .t.
      ::bPaint := {|| Wvt_DrawEllipse( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      EXIT

   CASE WVT_STATIC_SHADEDRECT
      lInside := .t.
      ::bPaint := {|| Wvt_DrawShadedRect( ::nTop, ::nLeft, ::nBottom, ::nRight, ;
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

   aadd( ::aPaint, { ::bPaint,;
             { WVT_BLOCK_STATIC, ::nfTop, ::nfLeft, ::nfBottom, ::nfRight }} )

   ::Super:Create()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtStatic:HoverOn()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtStatic:HoverOff()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtStatic:Refresh()

   Eval( ::bPaint )

   RETURN Self

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                      Class WvtPushButton
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

CLASS WvtPushButton FROM WvtObject

   DATA   cCaption
   DATA   cFileImage

   ACCESS block                                   INLINE ::bOnLeftUp
   ASSIGN block( bBlock )                         INLINE ::bOnLeftUp := bBlock

   METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight )
   METHOD Create()
   METHOD LeftDown()
   METHOD LeftUp()
   METHOD PaintButton()

ENDCLASS

/*----------------------------------------------------------------------*/

METHOD WvtPushButton:New( oParent, nID, nTop, nLeft, nBottom, nRight )

   ::Super:New( oParent, DLG_OBJ_PUSHBUTTON, nID, nTop, nLeft, nBottom, nRight )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtPushButton:Create()

   ::bPaint := {|| ::PaintButton() }

   aadd( ::aPaint, { ::bPaint,;
               { WVT_BLOCK_BUTTON, ::nTop, ::nLeft, ::nBottom, ::nRight } } )

   ::Super:Create()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtPushButton:PaintButton()

   IF ::cCaption == nil
      Wvt_DrawImage( ::nTop, ::nLeft, ::nBottom, ::nRight, ::cFileImage, { 4, 4,-4, -4 } )
   ELSE
      Wvt_DrawButton( ::nTop, ::nLeft, ::nBottom, ::nRight, ::cCaption, , 4 )
   ENDIF
   Wvt_DrawToolButtonState( ::nTop, ::nLeft, ::nBottom, ::nRight, {0,0,0,0}, 1 )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtPushButton:LeftDown()

   Wvt_DrawToolButtonState( ::nTop, ::nLeft, ::nBottom, ::nRight,{0,0,0,0} , 2 )

   RETURN .t.

/*----------------------------------------------------------------------*/

METHOD WvtPushButton:LeftUp()

   Wvt_DrawToolButtonState( ::nTop, ::nLeft, ::nBottom, ::nRight, {0,0,0,0}, 1 )
   ::Eval( ::bOnLeftUp )

   RETURN .t.

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                           Class WvtGets
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

CLASS WvtGets FROM WvtObject

   DATA   aGetList                                INIT  {}
   DATA   nLastGet                                INIT  1
   DATA   nCurGet                                 INIT  1
   DATA   GetList                                 INIT  {}
   DATA   cDesc                                   INIT  ""

   METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight )
   METHOD Create()
   METHOD KillFocus()
   METHOD SetFocus()
   METHOD HandleEvent( nKey )
   METHOD AddGets( nRow, nCol, xVar, cPic, cColor, bValid, bWhen )
   METHOD PaintBlock( nIndex )
   METHOD Read()
   METHOD Hilite()
   METHOD DeHilite()
   METHOD GetData()
   METHOD SetData()

ENDCLASS

/*----------------------------------------------------------------------*/

METHOD WvtGets:New( oParent, nID, nTop, nLeft, nBottom, nRight )

   ::Super:New( oParent, DLG_OBJ_GETS, nID, nTop, nLeft, nBottom, nRight )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtGets:Create()
   LOCAL i, GetList
   LOCAL nCurRow := row()
   LOCAL nCurCol := Col()

   FOR i := 1 to len( ::aGetList )
      GetList := {}

      DEFAULT ::aGetList[ i,7 ] TO "N/W*,N/W*,,,N/GR*"
      DEFAULT ::aGetList[ i,5 ] TO {|| .T. }
      DEFAULT ::aGetList[ i,6 ] TO {|| .T. }

      @ ::aGetList[ i,1 ], ::aGetList[ i,2 ] GET ::aGetList[ i,3 ] PICTURE ::aGetList[ i,4 ] COLOR ::aGetList[ i,7 ]

      aadd( ::GetList, GetList[ 1 ] )

      ::GetList[ i ]:Display()
      ::PaintBlock( i )
   next
   SetPos( nCurRow, nCurCol )

   ::Super:Create()
   ::Dehilite()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtGets:PaintBlock( nIndex )
   LOCAL nLen, bPaint

   nLen   := len( Transform( ::aGetList[ nIndex,3 ], ::aGetList[ nIndex,4 ] ) )

   bPaint := {|| Wvt_DrawBoxGet( ::aGetList[ nIndex,1 ], ::aGetList[ nIndex,2 ], nLen ) }

   aadd( ::aPaint, { bPaint,;
               { WVT_BLOCK_GETS, ::aGetList[ nIndex,1 ]-1, ::aGetList[ nIndex,2 ]-1, ;
                     ::aGetList[ nIndex,1 ]-1,  ::aGetList[ nIndex,2 ]+nLen } } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtGets:SetFocus()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtGets:KillFocus()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtGets:AddGets( nRow, nCol, xVar, cPic, cColor, bValid, bWhen )

   aadd( ::aGetList, { nRow, nCol, xVar, cPic, bValid, bWhen, cColor } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtGets:HandleEvent( nKey )
   LOCAL lRet := .f.

   DO CASE
   CASE nKey == K_LDBLCLK
      ::Read()
      lRet := .t.
   ENDCASE

   RETURN lRet

/*----------------------------------------------------------------------*/

METHOD WvtGets:Read()

   ReadModal( ::GetList, ::nCurGet )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtGets:GetData()
   LOCAL aData := NIL

   RETURN aData

/*----------------------------------------------------------------------*/

METHOD WvtGets:SetData( /*aData*/ )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtGets:Hilite()

   DispOutAt( ::nTop, ::nLeft, pad( " "+::cDesc, ::nRight-::nLeft+1 ), ::cColorHilite )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtGets:DeHilite()

   DispOutAt( ::nTop, ::nLeft, pad( " "+::cDesc, ::nRight-::nLeft+1 ), ::cColorDeHilite )

   RETURN Self

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                       Class WvtScrollBar
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

CLASS WvtScrollBar FROM WvtObject

   DATA   nBarType                                INIT WVT_SCROLLBAR_VERT

   DATA   nTotal                                  INIT 100
   DATA   nCurrent                                INIT 1
   DATA   nThumbPos                               INIT 0
   DATA   nBlockNo                                INIT 1

   DATA   nSTop
   DATA   nSLeft
   DATA   nSBottom
   DATA   nSRight

   DATA   nBtn1Top
   DATA   nBtn1Left
   DATA   nBtn1Bottom
   DATA   nBtn1Right

   DATA   nBtn2Top
   DATA   nBtn2Left
   DATA   nBtn2Bottom
   DATA   nBtn2Right
   DATA   bBtnLeftTop
   DATA   bBtnLeftTopDep
   DATA   bBtnRightBottom
   DATA   bBtnRightBottomDep
   DATA   bBtnScroll
   DATA   bTotal
   DATA   bCurrent
   DATA   lHidden                                 INIT .t.

   DATA   aPxlBtnTop                              INIT {0,0,0,0}
   DATA   aPxlBtnLft                              INIT {0,0,0,0}
   DATA   aPxlBtnBtm                              INIT {0,0,0,0}
   DATA   aPxlBtnRgt                              INIT {0,0,0,0}
   DATA   aPxlScroll                              INIT {0,0,0,0}

   DATA   lLeftDown                               INIT .f.
   DATA   lOnThumb                                INIT .f.
   DATA   lAnchored                               INIT .f.
   DATA   lOnLeftDown                             INIT .f.

   DATA   nScrollUnits                            INIT 0

   METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight )
   METHOD Create()
   METHOD Configure( nTop, nLeft, nBottom, nRight )
   METHOD Refresh()
   METHOD HandleEvent( nKey )
   METHOD SetPos( nTotal, nCurrent )
   METHOD GetPos()
   METHOD ThumbPos()
   METHOD SetTooltip()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD wvtScrollbar:New( oParent, nID, nTop, nLeft, nBottom, nRight )

   ::Super:New( oParent, DLG_OBJ_SCROLLBAR, nID, nTop, nLeft, nBottom, nRight )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD wvtScrollbar:Create()

   IF ::nTop == nil .or. ::nLeft == nil
      return nil
   ENDIF

   IF ::nBarType == WVT_SCROLLBAR_VERT
      DEFAULT ::nBottom TO ::nTop + 5
      DEFAULT ::nRight  TO ::nLeft + 1

      ::nRight       := ::nLeft + 1
      ::nBottom      := max( 7, ::nBottom )

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
           {|| Wvt_DrawScrollButton( ::nBtn1Top,::nBtn1Left,::nBtn1Bottom,::nBtn1Right,::aPxlBtnTop,1 ) }
      ::bBtnRightBottom := ;
           {|| Wvt_DrawScrollButton( ::nBtn2Top,::nBtn2Left,::nBtn2Bottom,::nBtn2Right,::aPxlBtnBtm,3 ) }
      ::bBtnScroll := ;
           {|| Wvt_DrawScrollThumbVert( ::nSTop ,::nSLeft  ,::nSBottom,::nSRight,::aPxlScroll,;
                                                  ::nThumbPos ) }
      ::bBtnLeftTopDep := ;
           {|| Wvt_DrawScrollButton( ::nBtn1Top,::nBtn1Left,::nBtn1Bottom,::nBtn1Right,::aPxlBtnTop,1,.t. ) }
      ::bBtnRightBottomDep := ;
           {|| Wvt_DrawScrollButton( ::nBtn2Top,::nBtn2Left,::nBtn2Bottom,::nBtn2Right,::aPxlBtnBtm,3,.t. ) }

   ELSE
      DEFAULT ::nBottom TO ::nTop
      DEFAULT ::nRight  TO ::nLeft + 11

      ::nBottom      := ::nTop
      ::nRight       := max( 11, ::nRight )

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
           {|| Wvt_DrawScrollButton( ::nBtn1Top,::nBtn1Left,::nBtn1Bottom,::nBtn1Right,::aPxlBtnLft,2 ) }
      ::bBtnRightBottom := ;
           {|| Wvt_DrawScrollButton( ::nBtn2Top,::nBtn2Left,::nBtn2Bottom,::nBtn2Right,::aPxlBtnRgt,4 ) }
      ::bBtnScroll := ;
           {|| Wvt_DrawScrollThumbHorz( ::nSTop,::nSLeft,::nSBottom,::nSRight, ::aPxlScroll,::nThumbPos ) }
      ::bBtnLeftTopDep := ;
           {|| Wvt_DrawScrollButton( ::nBtn1Top,::nBtn1Left,::nBtn1Bottom,::nBtn1Right,::aPxlBtnLft,2,.t. ) }
      ::bBtnRightBottomDep := ;
           {|| Wvt_DrawScrollButton( ::nBtn2Top,::nBtn2Left,::nBtn2Bottom,::nBtn2Right,::aPxlBtnRgt,4,.t. ) }

   ENDIF

   ::bOnLeftUp      := {|| ::HandleEvent( K_LBUTTONUP      ) }
   ::bOnLeftDown    := {|| ::HandleEvent( K_LBUTTONDOWN    ), .f. }
   ::bOnMMLeftDown  := {|| ::HandleEvent( K_MMLEFTDOWN     ) }
   ::bOnLeftPressed := {|| ::HandleEvent( K_LBUTTONPRESSED ) }

   Eval( ::bBtnLeftTop     )
   Eval( ::bBtnRightBottom )

   ::Super:Create()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD wvtScrollbar:Configure( nTop, nLeft, nBottom, nRight )

   ::nTop     := nTop
   ::nLeft    := nLeft
   ::nBottom  := nBottom
   ::nRight   := nRight

   IF ::nBarType == WVT_SCROLLBAR_VERT
      ::nRight       := ::nLeft + 1
      ::nBottom      := max( 7, ::nBottom )

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
      ::nRight       := max( 11, ::nRight )

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

/*----------------------------------------------------------------------*/

METHOD wvtScrollbar:Refresh()

   Eval( ::bBtnScroll )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD wvtScrollbar:SetPos( nTotal, nCurrent )

   DEFAULT nTotal   TO Eval( ::bTotal   )
   DEFAULT nCurrent TO Eval( ::bCurrent )

   ::nTotal   := nTotal
   ::nCurrent := nCurrent

   ::ThumbPos()
   ::Refresh()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD wvtScrollbar:ThumbPos()
   LOCAL  nNewPos, nRecPerUnit, nCurUnit

   IF ::nBarType == WVT_SCROLLBAR_VERT
      nRecPerUnit := ::nTotal / ::nScrollUnits
      nCurUnit    := int( ::nCurrent / nRecPerUnit )

      IF ::nCurrent == 1
         nCurUnit := 0
      ELSEIF ::nCurrent == ::nTotal
         nCurUnit := ::nScrollUnits
      ENDIF
      nNewPos     := ::nSTop + nCurUnit

      IF nNewPos < ::nSTop
         nNewPos  := ::nSTop
      ELSEIF nNewPos > ::nSBottom
         nNewPos  := ::nSBottom
      ENDIF

   ELSE
      IF ::nTotal < ::nScrollUnits
         nCurUnit := ::nCurrent * int( ::nScrollUnits / ::nTotal )
      ELSE
         nRecPerUnit := ::nTotal / ::nScrollUnits
         nCurUnit    := int( ::nCurrent / nRecPerUnit )
      ENDIF

      IF ::nCurrent == 1
         nCurUnit := 0
      ELSEIF ::nCurrent == ::nTotal
         nCurUnit := ::nScrollUnits
      ENDIF

      nNewPos := ::nSLeft + nCurUnit

      IF nNewPos < ::nSLeft
         nNewPos := ::nSLeft
      ELSEIF nNewPos > ::nSRight - 1
         nNewPos := ::nSRight-1
      ENDIF

   ENDIF

   ::nThumbPos := nNewPos

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD wvtScrollbar:GetPos()

   RETURN ::nCurrent

/*----------------------------------------------------------------------*/

METHOD wvtScrollbar:SetTooltip()

   ::Tooltip := ltrim( str( ::nCurrent,12,0 ) ) + " / " + ;
                ltrim( str( ::nTotal  ,12,0 ) )

   Wvt_SetToolTip( ::nTop, ::nLeft, ::nBottom, ::nRight, ::Tooltip )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD wvtScrollbar:HandleEvent( nKey )
   LOCAL nmRow, nmCol, nOff
   LOCAL lHit  := .F.
   LOCAL mKeys_:={ K_LBUTTONDOWN, K_LBUTTONUP, K_MMLEFTDOWN, K_LBUTTONPRESSED }

   IF ascan( mKeys_, nKey ) == 0
      RETURN .f.
   ENDIF

   nmRow := MRow()
   nmCol := MCol()

   DO CASE
   CASE ::nBarType == WVT_SCROLLBAR_VERT
      lHit := .t.

      DO CASE
      CASE ::lAnchored .and. nKey == K_MMLEFTDOWN
         IF nmRow != ::nThumbPos
            nOff := ::nThumbPos - nmRow
            IF nOff > 0
               ::nThumbPos := max( ::nTop+1, nmRow )
            ELSE
               ::nThumbPos := min( ::nBottom-1, nmRow )
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
            Wvt_Keyboard( K_SBTHUMBTRACKVERT )
         ELSE
            lHit := .f.
         ENDIF

      CASE ::lAnchored .and. nKey == K_LBUTTONUP
         ::lAnchored := .f.

      OTHERWISE
         lHit := .f.

         IF nmCol >= ::nLeft .and. nmCol <= ::nRight
            lHit := .t.

            DO CASE
            CASE nmRow == ::nThumbPos .and. nKey == K_LBUTTONDOWN
               ::lAnchored := .t.

            CASE nKey == K_LBUTTONUP
               IF ( lHit := ::lOnLeftDown )
                  DO CASE
                  CASE nmRow == ::nTop
                     Eval( ::bBtnLeftTop )
                  CASE nmRow == ::nBottom
                     Eval( ::bBtnRightBottom )
                  CASE nmRow < ::nThumbPos .and. nmRow > ::nTop
                  CASE nmRow > ::nThumbPos .and. nmRow < ::nBottom
                  OTHERWISE
                     lHit := .f.
                  ENDCASE
                  IF lHit
                     ::lOnLeftDown := .f.
                  ENDIF
               ENDIF

            CASE nKey == K_LBUTTONPRESSED
               IF ( lHit := ::lOnLeftDown )
                  DO CASE
                  CASE nmRow == ::nTop
                     Wvt_Keyboard( K_SBLINEUP   )
                  CASE nmRow == ::nBottom
                     Wvt_Keyboard( K_SBLINEDOWN )
                  CASE nmRow < ::nThumbPos .and. nmRow > ::nTop
                     Wvt_Keyboard( K_SBPAGEUP )
                  CASE nmRow > ::nThumbPos .and. nmRow < ::nBottom
                     Wvt_Keyboard( K_SBPAGEDOWN )
                  OTHERWISE
                     lHit := .f.
                  ENDCASE
               ENDIF

            CASE nKey == K_LBUTTONDOWN
               DO CASE
               CASE nmRow == ::nTop
                  Eval( ::bBtnLeftTopDep )
                  Wvt_Keyboard( K_SBLINEUP )
               CASE nmRow == ::nBottom
                  Eval( ::bBtnRightBottomDep )
                  Wvt_Keyboard( K_SBLINEDOWN )
               CASE nmRow < ::nThumbPos .and. nmRow > ::nTop
                  Wvt_Keyboard( K_SBPAGEUP   )
               CASE nmRow > ::nThumbPos .and. nmRow < ::nBottom
                  Wvt_Keyboard( K_SBPAGEDOWN )
               OTHERWISE
                  lHit := .f.
               ENDCASE
               IF lHit
                  ::lOnLeftDown := .t.
               ENDIF
            ENDCASE
         ENDIF

      ENDCASE

   CASE ::nBarType == WVT_SCROLLBAR_HORZ
      DO CASE
      CASE ::lAnchored .and. nKey == K_MMLEFTDOWN
         IF ( lHit := ( nmCol < ::nThumbPos .or. nmCol > ::nThumbPos+1 ) )

            nOff := ::nThumbPos - nmCol
            IF nOff > 0
               ::nThumbPos := max( ::nLeft+2, nmCol )
            ELSE
               ::nThumbPos := min( ::nRight-2, nmCol )
            ENDIF

            ::nCurrent := ( ::nTotal * ( ::nThumbPos - ::nLeft+1 ) / ::nScrollUnits )

            IF ::nCurrent > ::nTotal
               ::nCurrent := ::nTotal
            ENDIF
            IF ::nCurrent < 1
               ::nCurrent := 1
            ENDIF

            ::SetPos( ::nTotal, ::nCurrent )

            Wvt_Keyboard( K_SBTHUMBTRACKHORZ )
         ENDIF

      CASE ::lAnchored .and. nKey == K_LBUTTONUP
         ::lAnchored := .f.
         lHit := .t.

      OTHERWISE

         IF ( lHit := nmRow == ::nTop .and. nmCol >= ::nLeft .and. nmCol <= ::nRight )

            DO CASE
            CASE nKey == K_LBUTTONDOWN .and. nmCol >= ::nThumbPos .and. nmCol <= ::nThumbPos+1
               ::lAnchored := .t.

            CASE nKey == K_LBUTTONUP

               IF ( lHit := ::lOnLeftDown )
                  DO CASE
                  CASE nmCol >= ::nLeft    .and. nmCol <= ::nLeft+1
                     Eval( ::bBtnLeftTop )
                  CASE nmCol >= ::nRight-1 .and. nmCol <= ::nRight
                     Eval( ::bBtnRightBottom )
                  CASE nmCol <  ::nThumbPos
                  CASE nmCol >  ::nThumbPos+1
                  OTHERWISE
                     lHit := .f.
                  ENDCASE
                  IF lHit
                     ::lOnLeftDown := .f.
                  ENDIF
               ENDIF

            CASE nKey == K_LBUTTONPRESSED
               IF ( lHit := ::lOnLeftDown )
                  DO CASE
                  CASE nmCol == ::nLeft  .or. nmCol == ::nLeft+1
                     Wvt_Keyboard( K_SBLINELEFT )
                  CASE nmCol == ::nRight .or. nmCol == ::nRight-1
                     Wvt_Keyboard( K_SBLINERIGHT )
                  CASE nmCol < ::nThumbPos
                     Wvt_Keyboard( K_SBPAGELEFT )
                  CASE nmCol > ::nThumbPos+1
                     Wvt_Keyboard( K_SBPAGERIGHT )
                  OTHERWISE
                     lHit := .f.
                  ENDCASE
               ENDIF

            CASE nKey == K_LBUTTONDOWN
               DO CASE
               CASE nmCol == ::nLeft  .or. nmCol == ::nLeft+1
                  Eval( ::bBtnLeftTopDep )
                  Wvt_Keyboard( K_SBLINELEFT )
               CASE nmCol == ::nRight .or. nmCol == ::nRight-1
                  Eval( ::bBtnRightBottomDep )
                  Wvt_Keyboard( K_SBLINERIGHT )
               CASE nmCol < ::nThumbPos
                  Wvt_Keyboard( K_SBPAGELEFT )
               CASE nmCol > ::nThumbPos+1
                  Wvt_Keyboard( K_SBPAGERIGHT )
               OTHERWISE
                  lHit := .f.
               ENDCASE
               IF lHit
                  ::lOnLeftDown := .t.
               ENDIF
            ENDCASE
         ENDIF
      ENDCASE
   ENDCASE

   RETURN lHit

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                        CLASS WvtBanner
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

CLASS WvtBanner FROM WvtObject

   DATA   nTimeDelay                              INIT 0.5    /* One-half Second */
   DATA   nDirection                              INIT 0      /* LEFT 1-RIGHT */
   DATA   nCharToSkip                             INIT 1
   DATA   cText                                   INIT ""
   DATA   cDispText                               INIT ""
   DATA   nTextLen                                INIT 0
   DATA   nTextIndex                              INIT 0

   DATA   oLabel

   DATA   nAlignVert                              INIT 2     /* Center */

   DATA   nCurSeconds                             INIT 0
   DATA   nCurAlign

   METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight )
   METHOD Create()
   METHOD Configure()
   METHOD Refresh()
   METHOD HoverOn()
   METHOD HoverOff()
   METHOD OnTimer()
   METHOD SetText( cText )
   METHOD Destroy()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD WvtBanner:New( oParent, nID, nTop, nLeft, nBottom, nRight )

   ::Super:New( oParent, DLG_OBJ_BANNER, nID, nTop, nLeft, nBottom, nRight )

   RETURN Self

/*----------------------------------------------------------------------*/

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
   ::nTextLen    := len( ::cText )
   ::nTextIndex  := iif( ::nDirection == 0, 1, ::nTextLen )
   ::nCurAlign   := ::nDirection

   ::Super:Create()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtBanner:Destroy()

   WVG_DeleteObject( ::oLabel:hFont )

   RETURN nil

/*----------------------------------------------------------------------*/

METHOD WvtBanner:Configure()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtBanner:OnTimer()

   ::Refresh()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtBanner:SetText( cText )

   IF cText != nil
      ::cText := cText
      ::Refresh()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtBanner:Refresh()
   LOCAL nNewTime

   IF abs( ( nNewTime := Seconds() ) - ::nCurSeconds ) >= ::nTimeDelay
      ::nCurSeconds := nNewTime

      IF ::nDirection == 0
         ::nTextIndex++
         IF ::nTextIndex > ::nTextLen
            ::nTextIndex := 1
            ::nCurAlign  := iif( ::nCurAlign == 0, 1, 0 )
         ENDIF

         IF ::nCurAlign == 0   /* Left  */
            ::cDispText := substr( ::cText,::nTextIndex )
         ELSE                  /* Right */
            ::cDispText := substr( ::cText, 1, ::nTextIndex )
         ENDIF
      ELSE
         ::nTextIndex--
         IF ::nTextIndex < 0
            ::nTextIndex := ::nTextLen
            ::nCurAlign := iif( ::nCurAlign == 0, 1, 0 )
         ENDIF

         IF ::nCurAlign == 0   /* Left  */
            ::cDispText := substr( ::cText,::nTextIndex )
         ELSE                  /* Right */
            ::cDispText := substr( ::cText, 1, ::nTextIndex )
         ENDIF
      ENDIF

      ::oLabel:nAlignHorz := ::nCurAlign
      ::oLabel:SetText( ::cDispText )
      ::oLabel:Refresh()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtBanner:HoverOn()

   ::oLabel:HoverOn()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtBanner:HoverOff()

   ::oLabel:HoverOff()

   RETURN Self

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                        Class WvtTextBox
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

CLASS WvtTextBox FROM WvtObject

   DATA   cText                                   INIT ""

   METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight )
   METHOD Create()
   METHOD Configure()
   METHOD Refresh()
   METHOD SetText( cText )
   METHOD HoverOn()
   METHOD HoverOff()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD WvtTextBox:New( oParent, nID, nTop, nLeft, nBottom, nRight )

   ::Super:New( oParent, DLG_OBJ_TEXTBOX, nID, nTop, nLeft, nBottom, nRight )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtTextBox:Create()

   ::nTextColorHoverOff := ::nTextColor

   ::hFont := Wvt_CreateFont( ::cFont, ::nFontHeight, ::nFontWidth, ;
                  ::nFontWeight, ::lItalic, ::lUnderline, ::lStrikeout, ;
                  ::nCharSet, ::nFontQuality, 0 )

   IF ::hFont != 0
      ::bPaint := {|| Wvt_DrawTextBox( ::nTop, ::nLeft, ::nBottom, ::nRight, ;
            ::aPxlTLBR, ::cText, ::nAlignHorz, ::nAlignVert, ;
            ::nTextColor, ::nBackColor, ::nBackMode, ::hFont ) }

      aadd( ::aPaint, { ::bPaint, { WVT_BLOCK_LABEL, ::nTop, ::nLeft, ::nBottom, ::nRight } } )
   ENDIF

   ::Super:Create()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtTextBox:Refresh()

   Eval( ::bPaint )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtTextBox:Configure()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtTextBox:SetText( cText )

   IF cText != nil
      ::cText := cText
      ::Refresh()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtTextBox:HoverOn( /*cText*/ )

   IF ::nTextColorHoverOn != nil
      ::nTextColor := ::nTextColorHoverOn
      ::Refresh()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtTextBox:HoverOff( /*cText*/ )

   IF ::nTextColorHoverOn != nil
      ::nTextColor := ::nTextColorHoverOff
      ::Refresh()
   ENDIF

RETURN Self

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                       Class WvtProgressBar
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

CLASS WvtProgressBar FROM WvtObject

   DATA   cImage
   DATA   nDirection                              INIT 0      /* 0-Left-Right,Top-Bottom  1-Right-Left,Bottom-Top */
   DATA   nStyle                                  INIT 0
   DATA   lVertical                               INIT .f.
   DATA   lActive                                 INIT .f.

   DATA   nBarColor                               INIT RGB( 0,0,128 )
   DATA   nCurrent                                INIT 0
   DATA   nTotal                                  INIT 1
   DATA   nPercent                                INIT 0
   DATA   cBackColor                              INIT "W/W"

   DATA   cScreen

   METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight )
   METHOD Create()
   METHOD Display( nCurrent, nTotal )
   METHOD Activate()
   METHOD DeActivate()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD WvtProgressBar:New( oParent, nID, nTop, nLeft, nBottom, nRight )

   ::Super:New( oParent, DLG_OBJ_PROGRESSBAR, nID, nTop, nLeft, nBottom, nRight )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtProgressBar:Create()

   DEFAULT ::nTop       TO 0
   DEFAULT ::nLeft      TO 0
   DEFAULT ::nBottom    TO iif( ::lVertical, ::nTop + 9, ::nTop )
   DEFAULT ::nRight     TO iif( ::lVertical, ::nLeft + 1, ::nLeft + 19 )
   DEFAULT ::nTextColor TO RGB( 255,255,255 )
   DEFAULT ::nBackColor TO RGB( 198,198,198 )

   ::bPaint := {|| ::Display() }
   aadd( ::aPaint, { ::bPaint, { WVT_BLOCK_LABEL, ::nTop, ::nLeft, ::nBottom, ::nRight } } )

   ::Super:Create()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtProgressBar:Display( nCurrent, nTotal )

   IF !( ::lActive )
      return Self
   ENDIF

   DEFAULT nCurrent TO ::nCurrent
   DEFAULT nTotal   TO ::nTotal

   ::nCurrent := nCurrent
   ::nTotal   := nTotal

   IF ::nCurrent > ::nTotal
      ::nCurrent := ::nTotal
   ENDIF

   ::nPercent := int( ::nCurrent / ::nTotal * 100 )

   Wvt_DrawProgressBar( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlTLBR, ::nPercent, ;
                        ::nBackColor, ::nBarColor, ::cImage, ::lVertical, ::nDirection )
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtProgressBar:Activate()

   ::cScreen := SaveScreen( ::nTop, ::nLeft, ::nBottom, ::nRight )
   DispBox( ::nTop, ::nLeft, ::nBottom, ::nRight, "         ", ::cBackColor )
   ::lActive := .t.

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtProgressBar:DeActivate()

   ::lActive  := .f.
   ::nCurrent := 0
   ::nTotal   := 1
   RestScreen( ::nTop, ::nLeft, ::nBottom, ::nRight, ::cScreen )
   ::cScreen := nil

   RETURN Self

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                           Class WvtMenu
 *                            Peter Rees
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

CLASS wvtMenu

   METHOD Create( cCaption )
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

   CLASSVAR MenuItemId                            INIT 1

   VAR    aItems
   VAR    hMenu
   VAR    Caption
   VAR    IdNumber

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD wvtMenu:Create( cCaption )
   ::aItems := {}

   IF EMPTY( ::hMenu:= Wvt_CreateMenu() )
      #IF 0
      Throw( ErrorNew( "wvtMenu", 1000, "wvtMenu:Init()", "Create Menu Error", { cCaption, cCaption },"wvt.prg" ) )
      #ENDIF
   ENDIF
   ::Caption:= iif( cCaption == NIL, "", cCaption )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD wvtMenu:Destroy()

   IF !EMPTY( ::hMenu )
      ::DelAllItems()

      IF !Wvt_DestroyMenu( ::hMenu )
         #IF 0
         Throw( ErrorNew( "wvtMenu", 1000, "wvtMenu:Destroy()", "Destroy menu FAILED", {},"wvt.prg" ) )
         #ENDIF
      ENDIF
      ::hMenu:= 0
   ENDIF

   RETURN .T.

/*----------------------------------------------------------------------*/

METHOD wvtMenu:AddItem( cCaption, bAction )
   LOCAL lResult:= .F., aItem

   IF !EMPTY( ::hMenu ) .AND. ( !EMPTY( cCaption ) .OR. !EMPTY( bAction ) )
      IF HB_ISOBJECT( bAction )
         cCaption:= iif(!EMPTY(cCaption),cCaption,bAction:Caption)
         aItem:= {MF_POPUP,bAction:hMenu,cCaption,bAction}   /* bAction is a wvtMenu object reference */
      ELSEIF HB_ISBLOCK(bAction)
         aItem:= {MF_STRING,::MenuItemId++,cCaption,bAction} /* bAction is a code block to execute */
      ELSEIF left( cCaption, 1 )=="-"
         aItem:= {MF_SEPARATOR,0,0,NIL}
      ELSE
         #IF 0
         Throw( ErrorNew( "wvtMenu", 3101, "wvtMenu:AddItem()", "Argument Error", { cCaption, bAction },"wvt.prg" ) )
         #ENDIF
      ENDIF

      IF !Wvt_AppendMenu(::hMenu, aItem[WVT_MENU_TYPE],aItem[WVT_MENU_IDENTIFIER],aItem[WVT_MENU_CAPTION])
         #IF 0
         Throw( ErrorNew( "wvtMenu", 1000, "wvtMenu:AddItem()", "Add menu item", { cCaption, bAction },"wvt.prg" ) )
         #ENDIF
      ENDIF

      AADD(::aItems, aItem)
      lResult:= .T.
   ENDIF

   RETURN lResult

/*----------------------------------------------------------------------*/

METHOD wvtMenu:DelAllItems()
   LOCAL lResult:= .T.,  nItems

   nItems := ::NumItems()
   DO WHILE nItems>0 .AND. lResult
      lResult := ::DelItem( nItems )
      nItems--
   ENDDO

   RETURN lResult

/*----------------------------------------------------------------------*/

METHOD wvtMenu:DelItem( nItemNum )
   LOCAL lResult:= .F.

   IF nItemNum > 0 .AND. nItemNum <= ::NumItems()
      IF ::aItems[ nItemNum,WVT_MENU_TYPE ]== MF_POPUP
         ::aItems[ nItemNum,WVT_MENU_MENUOBJ ]:Destroy()
      ENDIF

      IF ( lResult:= Wvt_DeleteMenu(::hMenu, nItemNum-1,MF_BYPOSITION)) /* Remember ZERO base */
         ADEL( ::aItems, nItemNum )
         ASIZE( ::aItems, LEN( ::aItems ) - 1 )
      ELSE
         #IF 0
         Throw( ErrorNew( "wvtMenu", 1000, "wvtMenu:DelItem()", "Delete menu item FAILED", { nItemNum },"wvt.prg" ) )
         #ENDIF
      ENDIF
   ENDIF

   RETURN lResult

/*----------------------------------------------------------------------*/

METHOD wvtMenu:EnableItem( nItemNum )
   LOCAL nPrevious:= -1

   IF !EMPTY( ::hMenu ) .AND. !EMPTY( nItemNum )
      nPrevious:= Wvt_EnableMenuItem( ::hMenu, nItemNum-1, MF_BYPOSITION + MF_ENABLED )
   ENDIF

   RETURN nPrevious

/*----------------------------------------------------------------------*/

METHOD wvtMenu:DisableItem( nItemNum )
   LOCAL nPrevious:= -1

   IF !EMPTY( ::hMenu ) .AND. !EMPTY( nItemNum )
      nPrevious:= Wvt_EnableMenuItem( ::hMenu, nItemNum-1, MF_BYPOSITION + MF_GRAYED )
   ENDIF

   RETURN nPrevious

/*----------------------------------------------------------------------*/

METHOD wvtMenu:NumItems()

   RETURN LEN( ::aItems )

/*----------------------------------------------------------------------*/

METHOD wvtMenu:GetItem( nItemNum )
   LOCAL nItems := ::NumItems(), aResult:= NIL

   IF nItemNum > 0 .AND. nItemNum <= nItems
      aResult:= ::aItems[ nItemNum ]
   ENDIF

   RETURN aResult

/*----------------------------------------------------------------------*/

METHOD wvtMenu:FindMenuItemById( nId )
   LOCAL x, aResult:= {}

   IF !EMPTY( nId )
      x:= ::NumItems()
      DO WHILE x > 0 .AND. EMPTY( aResult )
         IF ::aItems[ x,WVT_MENU_TYPE ] == MF_POPUP
            aResult:= ::aItems[ x,WVT_MENU_MENUOBJ ]:FindMenuItemById( nId )
         ELSEIF ::aItems[ x,WVT_MENU_IDENTIFIER ] == nId
            aResult := ::aItems[ x ]
         ENDIF
         x--
      ENDDO
   ENDIF

   RETURN aResult

/*----------------------------------------------------------------------*/

METHOD wvtMenu:DrawMenuBar()

   Wvt_DrawMenuBar()

   RETURN NIL

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                         Class WvtConsole
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

CLASS WvtConsole FROM WvtObject

   METHOD New( oParent )
   METHOD Say( nRow, nCol, xExp, cColor )
   METHOD Box( nRow, nCol, n2Row, n2Col, cBoxChars, cColor )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD WvtConsole:New( oParent )

   ::Super:New( oParent, DLG_OBJ_CONSOLE, , -1, -1, -1, -1 )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtConsole:Say( nRow, nCol, xExp, cColor )
   LOCAL nCRow, nCCol, nCursor

   IF nRow >=0 .and. nCol >= 0 .and. xExp != nil
      nCursor := SetCursor( SC_NONE )
      nCRow   := Row()
      nCCol   := Col()
      DispOutAt( nRow, nCol, xExp, cColor )
      SetPos( nCRow, nCCol )
      SetCursor( nCursor )
   ENDIF

RETURN Self

/*----------------------------------------------------------------------*/

METHOD WvtConsole:Box( nRow, nCol, n2Row, n2Col, cBoxChars, cColor )

   LOCAL nCRow, nCCol, nCursor

   IF nRow >=0 .and. nCol >= 0
      nCursor := SetCursor( SC_NONE )
      nCRow   := Row()
      nCCol   := Col()
      DispBox( nRow, nCol, n2Row, n2Col, cBoxChars, cColor )
      SetPos( nCRow, nCCol )
      SetCursor( nCursor )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                      TBrowseWVG From TBrowse
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#define _TBCI_COLOBJECT       1   /* column object                             */
#define _TBCI_COLWIDTH        2   /* width of the column                       */
#define _TBCI_COLPOS          3   /* column position on screen                 */
#define _TBCI_CELLWIDTH       4   /* width of the cell                         */
#define _TBCI_CELLPOS         5   /* cell position in column                   */
#define _TBCI_COLSEP          6   /* column separator                          */
#define _TBCI_SEPWIDTH        7   /* width of the separator                    */
#define _TBCI_HEADING         8   /* column heading                            */
#define _TBCI_FOOTING         9   /* column footing                            */
#define _TBCI_HEADSEP        10   /* heading separator                         */
#define _TBCI_FOOTSEP        11   /* footing separator                         */
#define _TBCI_DEFCOLOR       12   /* default color                             */
#define _TBCI_FROZENSPACE    13   /* space after frozen columns                */
#define _TBCI_LASTSPACE      14   /* space after last visible column           */
#define _TBCI_SIZE           14   /* size of array with TBrowse column data    */

/*----------------------------------------------------------------------*/

CLASS TBrowseWVG FROM TBrowse

   DATA   aColumnsSep                             INIT {}

   METHOD SetVisible()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD TBrowseWVG:SetVisible()
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
               nColPos += Int( aCol[ _TBCI_SEPWIDTH ]/2 )
            ENDIF

            aadd( ::aColumnsSep, nColPos )
         ENDIF
      ENDIF
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/
