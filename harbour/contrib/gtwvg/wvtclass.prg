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
 * Video subsystem for Win32 using GUI windows instead of Console
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
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                            Wvt*Classes
//                               2004
//                Pritpal Bedi <pritpal@vouchcac.com>
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

#include                 'hbclass.ch'
#include                   'inkey.ch'
#include                  'common.ch'
#include                 'setcurs.ch'

#include                  'wvtwin.ch'
//#include                     'xhb.ch'

//-------------------------------------------------------------------//

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

//-------------------------------------------------------------------//

#define OBJ_CHILD_OBJ             1
#define OBJ_CHILD_EVENTS          2
#define OBJ_CHILD_DATABLOCK       3
#define OBJ_CHILD_REFRESHBLOCK    4

//-------------------------------------------------------------------//
//
//                         Class WvtDialog
//
//-------------------------------------------------------------------//

CLASS wvtDialog

   //  To hold previous settings
   //
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

   //  Dialog Parameters
   //
   DATA   nRows
   DATA   nCols
   DATA   cFont
   DATA   nFontHeight
   DATA   nFontWidth
   DATA   nFontBold
   DATA   nFontQuality
   DATA   cTitle
   DATA   cColor

   //  Objects handelling
   //
   DATA   aObjects              INIT {}
   DATA   oCurObj
   DATA   oLastObj
   DATA   oObjOver
   DATA   oLastOver
   DATA   nCurObj               INIT 1
   DATA   nLastObj              INIT 0
   DATA   nObjOver              INIT 0
   DATA   nLastOver             INIT -1
   DATA   nUseObj
   DATA   oMenu
   DATA   aDialogKeys           INIT {}
   DATA   cDialogID             INIT ''

   //  Tooltip Management
   //
   DATA   nTooltipWidth
   DATA   nTooltipBkColor
   DATA   nTooltipTextColor

   //  Miscellaneous
   //
   DATA   ClassName             INIT 'WVTDIALOG'
   DATA   cPaintBlockID
   DATA   nPaintID              INIT 1
   DATA   nObjID                INIT 5000
   DATA   nKey
   DATA   hFonts                INIT {}
   DATA   lEventHandled
   DATA   lTabStops             INIT .f.

   ACCESS nObjects              INLINE len( ::aObjects )

   DATA   bOnCreate

   METHOD New()
   METHOD Create()
   METHOD Destroy()
   METHOD AddObject( oObject )  INLINE aadd( ::aObjects, oObject )
   METHOD Execute()
   METHOD MouseOver()
   METHOD CreateObjects()
   METHOD MaxRow()              INLINE ::nRows - 1
   METHOD MaxCol()              INLINE ::nCols - 1
   METHOD Eval()
   METHOD Update()
   METHOD OnTimer()             INLINE aeval( ::aObjects, {|o| o:OnTimer() } )
   METHOD Event()
   METHOD Inkey()
   METHOD ActivateMenu()

ENDCLASS

//-------------------------------------------------------------------//

METHOD New( nRows, nCols, cTitle, cFont, nFontHeight, nFontWidth,nFontBold,nFontQuality ) CLASS wvtDialog
   LOCAL fnt_:= Wvt_GetFontInfo()

   DEFAULT nRows         TO 25
   DEFAULT nCols         TO 80
   DEFAULT cTitle        TO Wvt_GetTitle()
   DEFAULT cFont         TO fnt_[ 1 ]
   DEFAULT nFontHeight   TO fnt_[ 2 ]
   DEFAULT nFontWidth    TO fnt_[ 3 ]
   DEFAULT nFontBold     TO fnt_[ 4 ]
   DEFAULT nFontQuality  TO fnt_[ 5 ]

   if empty( cFont )
      cFont := fnt_[ 1 ]
   endif
   if empty( nFontHeight )
      nFontHeight := fnt_[ 2 ]
   endif
   if empty( nFontWidth )
      nFontWidth := fnt_[ 3 ]
   endif

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
   ::cColor              := 'N/W'
   ::nUseObj             := 0
   ::lGui                := Wvt_SetGui( .f. )

   RETURN Self

//-------------------------------------------------------------------//

METHOD Create() CLASS wvtDialog
   LOCAL aPalette, n, i, j  // , cScr, cQry

   ::oldToolTipActive := Wvt_SetToolTipActive( .t. )
   if ::nTooltipWidth <> nil
      Wvt_setTooltipWidth( ::nTooltipWidth )
   endif
   if ::nTooltipBkColor <> nil
      Wvt_SetTooltipBkColor( ::nTooltipBkColor )
   endif
   if ::nTooltipTextColor <> nil
      Wvt_SetTooltipTextColor( ::nTooltipTextColor )
   endif

   aPalette      := Wvt_GetPalette()
   aPalette[ 9 ] := RGB( 175,175,175 )
   Wvt_SetPalette( aPalette )

   ::cScreen     := SaveScreen( 0, 0, maxrow(), maxcol() )
   ::aWvtScreen  := Wvt_SaveScreen( 0, 0, maxrow(), maxcol() )
   ::aOldPnt     := WvtSetPaint( {} )

   SetMode( ::nRows, ::nCols )
   do while .t.
      if Wvt_SetFont( ::cFont, ::nFontHeight, ::nFontWidth, ::nFontBold, ::nFontQuality )
         exit
      endif
      ::nFontHeight--
   enddo
   //Wvt_SetFont( ::cFont, ::nFontHeight, ::nFontWidth, ::nFontBold, ::nFontQuality )
   SetMode( ::nRows, ::nCols )

   Wvt_SetTitle( ::cTitle )

   SetColor( ::cColor )
   CLS
   ::Eval( ::bOnCreate )

   // Actually Create the Objects
   ::CreateObjects()

   if len( ::aObjects ) > 0
      ::oCurObj := ::aObjects[ 1 ]
   endif

   for i := 1 to len( ::aObjects )
      if !empty( ::aObjects[ i ]:aPaint )
         for j := 1 to len( ::aObjects[ i ]:aPaint )
            SetPaint( ::cPaintBlockID, ::nPaintID++, ;
                ::aObjects[ i ]:aPaint[ j,1 ], ::aObjects[ i ]:aPaint[ j,2 ] )
         next
      endif
   next
   WvtSetPaint( GetPaint( ::cPaintBlockID ) )

   if ( n := ascan( ::aObjects, {|o| o:lTabStop } ) ) > 0
      ::lTabStops := .t.
   endif

   ::Update()

   if HB_ISOBJECT( ::oMenu )
      Wvt_SetMenu( ::oMenu:hMenu )
      Wvt_DrawMenuBar()
      SetKey( Wvt_SetMenuKeyEvent(), {|| ::ActivateMenu( ::oMenu ) } )
   endif

   RETURN Self

//-------------------------------------------------------------------//

METHOD Destroy() CLASS wvtDialog

   if HB_ISOBJECT( ::oMenu )
      ::oMenu:Destroy()
   endif

   aeval( ::aObjects, {|o| o:destroy() } )

   Wvt_SetTooltip( 0,0,0,0,'' )
   Wvt_SetTooltipActive( ::oldToolTipActive )
   Wvt_setTooltipWidth( ::oldTooltipWidth )
   Wvt_SetTooltipBkColor( ::oldTooltipBkColor )
   Wvt_SetTooltipTextColor( ::oldTooltipTextColor )

   //  Here set mode is before setting the font
   //
   SetMode( ::nOldRows, ::nOldCols )
   Wvt_SetFont( ::aOldFont[ 1 ], ::aOldFont[ 2 ], ::aOldFont[ 3 ], ::aOldFont[ 4 ], ::aOldFont[ 5 ] )
   Wvt_SetTitle( ::cOldTitle )
   Wvt_SetPalette( ::aPalette )
   Wvt_SetPointer( WVT_IDC_ARROW )
   Wvt_SetMousePos( MRow(), MCol() )

   SetColor( ::cOldColor )
   SetCursor( ::nOldCursor )

   if ::oldMenuHandle <> nil .and. ::oldMenuHandle <> 0
      Wvt_SetMenu( ::oldMenuHandle )
   endif
   SetKey( Wvt_SetMenuKeyEvent(), ::oldMenuBlock )
   RestScreen( 0, 0, maxrow(), maxcol(), ::cScreen )
   Wvt_RestScreen( 0, 0 ,maxrow(), maxcol(), ::aWvtScreen )
   PurgePaint( ::cPaintBlockID )
   WvtSetPaint( ::aOldPnt )
   Wvt_SetGui( ::lGui )


   RETURN nil

//-------------------------------------------------------------------//

METHOD Event() CLASS wvtDialog
   LOCAL  nKey

   if ( nKey := inkey( 0.1, INKEY_ALL ) ) == 0
      if Wvt_IsLButtonPressed()

         nKey := K_LBUTTONPRESSED

      endif
   endif

   RETURN ( nKey )

//-------------------------------------------------------------------//

METHOD Execute() CLASS wvtDialog

   if ::nObjects == 0
      do while .t.
         if inkey( 0.1 ) == K_ESC
            exit
         endif
      enddo
   else
      do while ( ::Inkey() <> K_ESC )
      enddo
   endif

   RETURN ::nKey

//-------------------------------------------------------------------//

METHOD Inkey() CLASS wvtDialog
   LOCAL  n, oObj, nID, i

   ::lEventHandled := .f.
   ::nUseObj       := 0

   ::nKey := ::Event()
   ::OnTimer()

   if ::nKey <> 0
      if ::nKey == K_ESC .or. ::nKey == K_CTRL_ENTER
         return K_ESC
      endif

      do case

      case ::nKey == K_TAB
         if ::lTabStops
            do while .t.
               ::nCurObj++
               if ::nCurObj > ::nObjects
                  ::nCurObj := 1
               endif
               if ::aObjects[ ::nCurObj ]:lTabStop
                  exit
               endif
            enddo
         endif

         ::lEventHandled := .t.

      case ::nKey == K_SH_TAB
         if ::lTabStops
            do while .t.
               ::nCurObj--
               if ::nCurObj < 1
                  ::nCurObj := ::nObjects
               endif
               if ::aObjects[ ::nCurObj ]:lTabStop
                  exit
               endif
            enddo
         endif

         ::lEventHandled := .t.

      case ::nKey == K_MOUSEMOVE .or. ::nKey == K_MMLEFTDOWN
         ::MouseOver()
         if ::nObjOver == 0
            Wvt_SetPointer( WVT_IDC_ARROW )
         elseif ::oObjOver:nPointer <> nil .and. ::oObjOver:lActive
            Wvt_SetPointer( ::oObjOver:nPointer )
         else
            Wvt_SetPointer( WVT_IDC_ARROW )
         endif
         ::lEventHandled := .t.

      endcase

      if    ::nKey == K_LBUTTONDOWN     .or. ;
            ::nKey == K_LBUTTONUP       .or. ;
            ::nKey == K_LDBLCLK         .or. ;
            ::nKey == K_MMLEFTDOWN      .or. ;
            ::nKey == K_LBUTTONPRESSED  .or. ;
            ::nKey == K_RBUTTONDOWN

         ::MouseOver()

         if ::nObjOver > 0
            if    ::aObjects[ ::nObjOver ]:nType == DLG_OBJ_BUTTON     .or. ;
                  ::aObjects[ ::nObjOver ]:nType == DLG_OBJ_TOOLBAR    .or. ;
                  ::aObjects[ ::nObjOver ]:nType == DLG_OBJ_PUSHBUTTON .or. ;
                  ::aObjects[ ::nObjOver ]:nType == DLG_OBJ_SCROLLBAR

               oObj := ::aObjects[ ::nObjOver ]
               if oObj:oParent:ClassName == 'WVTBROWSE'
                  nID := oObj:oParent:nID
                  if ( n := ascan( ::aObjects, {|o| o:nID == nID } ) ) > 0
                     ::nCurObj := n
                  endif
               endif
            else
               ::nCurObj := ::nObjOver
            endif
            ::nUseObj := ::nObjOver

         else
            ::lEventHandled := .t.

         endif
      endif

      if ::nLastOver <> ::nObjOver
         if ::nLastOver > 0
            ::aObjects[ ::nLastOver ]:HoverOff()
         endif

         ::nLastOver := ::nObjOver

         if ::nObjOver > 0
            ::oObjOver:HoverOn()
         endif

         if ::nObjOver == 0
            Wvt_SetTooltip( 0,0,0,0,'' )

         elseif ::oObjOver:lActive
            ::oObjOver:SetTooltip()

         else
            Wvt_SetTooltip( 0,0,0,0,'' )

         endif
      endif

      if ::nCurObj <> ::nLastObj
         if ::nLastObj == 0
            ::aObjects[ ::nCurObj  ]:Hilite()

         else
            ::aObjects[ ::nLastObj ]:DeHilite()
            ::aObjects[ ::nCurObj  ]:Hilite()

         endif

         ::nLastObj := ::nCurObj
         ::oCurObj  := ::aObjects[ ::nCurObj ]
         ::oLastObj := ::aObjects[ ::nCurObj ]

         if ::oCurObj:nType == DLG_OBJ_BROWSE
            Select( ::oCurObj:cAlias )

         endif

         ::Eval( ::oCurObj:bOnFocus, ::oCurObj )
      endif

      if ::nKey == K_LBUTTONDOWN
         if ::nUseObj > 0
            if !( ::lEventHandled := ::aObjects[ ::nUseObj ]:LeftDown() )
               ::lEventHandled := ::Eval( ::aObjects[ ::nUseObj ]:bOnLeftDown )

            endif
         endif
      endif

      if ::nKey == K_LBUTTONUP
         if ::nUseObj > 0
            if !( ::lEventHandled := ::aObjects[ ::nUseObj ]:LeftUp() )
               ::lEventHandled := ::Eval( ::aObjects[ ::nUseObj ]:bOnLeftUp )

            endif
         endif
      endif

      if ::nKey == K_MMLEFTDOWN
         if ::nUseObj > 0
            if !( ::lEventHandled := ::aObjects[ ::nUseObj ]:MMLeftDown() )
               ::lEventHandled := ::Eval( ::aObjects[ ::nUseObj ]:bOnMMLeftDown )

            endif
         endif
      endif

      if ::nKey == K_LBUTTONPRESSED
         if ::nUseObj > 0
            if !( ::lEventHandled := ::aObjects[ ::nUseObj ]:LeftPressed() )
               ::lEventHandled := ::Eval( ::aObjects[ ::nUseObj ]:bOnLeftPressed )

            endif
         endif
      endif

      if ::nKey == K_LDBLCLK
         if ::nUseObj > 0
            ::lEventHandled := ::Eval( ::aObjects[ ::nUseObj ]:bOnSelect )

         endif
      endif

      if ::nKey == K_RBUTTONDOWN .and. ::nUseObj > 0
         ::lEventHandled := ::aObjects[ ::nUseObj ]:ShowPopup()
      endif

      if !( ::lEventHandled )
         if ::nCurObj > 0
            if !empty( ::aDialogKeys )
               if ( n := ascan( ::aDialogKeys, {|e_| e_[ 1 ] == ::nKey } ) ) > 0
                  Eval( ::aDialogKeys[ n, 2 ], self, ::oCurObj )
               endif
            endif

            ::lEventHandled := ::oCurObj:HandleEvent( ::nKey )

            if ( ::lEventHandled )
               if ::oCurObj:nChildren > 0
                  for i := 1 to ::oCurObj:nChildren
                     //if ( ::nKey IN ::oCurObj:aChildren[ i, OBJ_CHILD_EVENTS ] )
                     if ascan( ::oCurObj:aChildren[ i, OBJ_CHILD_EVENTS ],::nKey ) > 0
                        ::oCurObj:NotifyChild( i, ::nKey, ::oCurObj )
                     endif
                  next
               endif
            endif
         endif
      endif

      if !( ::lEventHandled )
         if ISBLOCK( SetKey( ::nKey ) )
            Eval( SetKey( ::nKey ) )
         endif
      endif
   endif

   RETURN ( ::nKey )

//-------------------------------------------------------------------//

METHOD MouseOver()  CLASS wvtDialog
   LOCAL mRow := MRow()
   LOCAL mCol := MCol()
   LOCAL nObj

   nObj := ascan( ::aObjects, ;
                    {|o| o:nType <> DLG_OBJ_STATIC               .and. ;
                         o:nType <> DLG_OBJ_TOOLBAR              .and. ;
                         mRow >= o:nTop  .and. mRow <= o:nBottom .and. ;
                         mCol >= o:nLeft .and. mCol <= o:nRight      } )

   ::nObjOver := nObj
   ::oObjOver := if( nObj > 0, ::aObjects[ nObj ], nil )
   if nObj > 0
      ::aObjects[ nObj ]:nmRow := mRow
      ::aObjects[ nObj ]:nmCol := mCol

   endif

   RETURN Self

//-------------------------------------------------------------------//

METHOD Update() CLASS wvtDialog

   Wvt_InvalidateRect( 0, 0, ::maxrow(), ::maxcol() )

   RETURN Self

//-------------------------------------------------------------------//

METHOD CreateObjects() CLASS wvtDialog
   LOCAL i, nObjs

   nObjs := len( ::aObjects )

   for i := 1 to nObjs
      switch ::aObjects[ i ]:nType

      case DLG_OBJ_BROWSE
         ::aObjects[ i ]:Create()
         exit
      case DLG_OBJ_STATUSBAR
         ::aObjects[ i ]:Create()
         exit
      case DLG_OBJ_LABEL
         ::aObjects[ i ]:Create()
         exit
      case DLG_OBJ_TOOLBAR
         ::aObjects[ i ]:Create()
         exit
      case DLG_OBJ_BUTTON
         ::aObjects[ i ]:Create()
         exit
      case DLG_OBJ_PUSHBUTTON
         ::aObjects[ i ]:Create()
         exit
      case DLG_OBJ_IMAGE
         ::aObjects[ i ]:Create()
         exit
      case DLG_OBJ_STATIC
         ::aObjects[ i ]:Create()
         exit
   /*
      case DLG_OBJ_SCROLLBAR
         ::aObjects[ i ]:Create()
         exit
   */
      case DLG_OBJ_GETS
         ::aObjects[ i ]:Create()
         exit
      case DLG_OBJ_BANNER
         ::aObjects[ i ]:Create()
         exit
      case DLG_OBJ_TEXTBOX
         ::aObjects[ i ]:Create()
         exit
      case DLG_OBJ_PROGRESSBAR
         ::aObjects[ i ]:Create()
         exit
      end
   next

   RETURN self

//-------------------------------------------------------------------//

METHOD Eval( bBlock, p1,p2,p3,p4,p5 ) CLASS wvtDialog
   LOCAL lRet

   if ( lRet := ISBLOCK( bBlock ) )
      eval( bBlock, p1,p2,p3,p4,p5 )
   endif

   RETURN lRet

//-------------------------------------------------------------------//

METHOD ActivateMenu() CLASS WvtDialog
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

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                         Class WvtObject
//
// Must never be used directly. It is parent class for all other objects!
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

CLASS WvtObject

   DATA   oParent
   DATA   nType
   DATA   nId

   DATA   nTop
   DATA   nLeft
   DATA   nBottom
   DATA   nRight
   DATA   aPxlTLBR              INIT {}

   DATA   aObjects              INIT {}
   DATA   aParent               INIT {}
   DATA   aChildren             INIT {}
   DATA   aPaint                INIT {}
   DATA   bPaint
   DATA   ClassName             INIT ''

   DATA   nObjID                INIT 900000
   DATA   nPointer
   DATA   cargo
   DATA   xSettings
   DATA   cText
   DATA   cToolTip
   DATA   lActive               INIT .t.
   DATA   lAnimate              INIT .f.
   DATA   lTabStop              INIT .t.
   DATA   hFont

   DATA   aPopup                INIT {}
   DATA   hPopup                INIT nil
   DATA   nPopupItemID          INIT 700000

   DATA   nMRow                 INIT 0
   DATA   nMCol                 INIT 0
   DATA   cColorHilite          INIT 'W+/B*'
   DATA   cColorDehilite        INIT 'W/N*'

   DATA   nTextColor
   DATA   nBackColor
   DATA   nBackMode             INIT 0 // OPAQUE 1-TRANSPARENT
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

   ACCESS ToolTip               INLINE if( ::cTooltip == nil, '', ::cTooltip )
   ASSIGN ToolTip( cTip )       INLINE ::cToolTip := cTip

   DATA   bHandleEvent
   DATA   bOnCreate
   DATA   bOnSelect
   DATA   bOnFocus
   DATA   bOnRefresh
   DATA   bOnLeftUp
   DATA   bOnLeftDown
   DATA   bOnMMLeftDown
   DATA   bOnLeftPressed
   DATA   bTooltip
   DATA   bSaveSettings
   DATA   bRestSettings
   DATA   bOnHilite
   DATA   bOnDeHilite

   ACCESS nChildren             INLINE len( ::aChildren )
   DATA   nIndexOrder

   METHOD New()
   METHOD Create()
   METHOD Destroy()

   //  To Avoid any runtime errors if a message is send to an object
   //  which does not implement these methods
   //
   METHOD PaintBlock()          INLINE nil
   METHOD Hilite()              INLINE nil
   METHOD DeHilite()            INLINE nil
   METHOD HandleEvent()         INLINE .f.

   METHOD LeftDown()            INLINE .f.
   METHOD LeftUp()              INLINE .f.
   METHOD MMLeftDown            INLINE .f.
   METHOD LeftPressed           INLINE .f.

   METHOD HoverOn()             INLINE nil
   METHOD HoverOff()            INLINE nil

   METHOD OnTimer()             INLINE nil

   METHOD SaveSettings()        INLINE nil
   METHOD RestSettings()        INLINE nil

   METHOD SetToolTip()          INLINE ;
            Wvt_SetToolTip( ::nTop, ::nLeft, ::nBottom, ::nRight, ::Tooltip )

   METHOD Refresh()             INLINE ;
            Wvt_InvalidateRect( ::nTop, ::nLeft, ::nTop, ::nLeft )

   METHOD Eval( bBlock )        INLINE ;
            if( ISBLOCK( bBlock ), Eval( bBlock, self ), nil )

   METHOD CreatePopup()
   METHOD ShowPopup()
   METHOD Activate()            INLINE nil
   METHOD DeActivate()          INLINE nil

   METHOD AddChild( aChild )    INLINE aadd( ::aChildren, aChild )
   METHOD AddParent( aParent )  INLINE aadd( ::aParent, aParent )

   METHOD NotifyChild( /*nChild*/ ) INLINE nil

   ENDCLASS

//-------------------------------------------------------------------//

METHOD New( oParent, nType, nID, nTop, nLeft, nBottom, nRight ) CLASS WvtObject

   DEFAULT nID TO ++::nObjID

   ::oParent   :=  oParent
   ::nType     :=  nType
   ::nId       :=  nID
   ::nTop      :=  nTop
   ::nLeft     :=  nLeft
   ::nBottom   :=  nBottom
   ::nRight    :=  nRight

   switch nType

   case DLG_OBJ_BROWSE
      ::ClassName := 'WVTBROWSE'
      exit

   case DLG_OBJ_STATIC
      ::ClassName := 'WVTSTATIC'
      ::lTabStop  := .f.
      exit

   case DLG_OBJ_GETS
      ::ClassName := 'WVTGETS'
      exit

   case DLG_OBJ_IMAGE
      ::ClassName := 'WVTIMAGE'
      ::lTabStop  := .f.
      exit

   case DLG_OBJ_PUSHBUTTON
      ::ClassName := 'WVTPUSHBUTTON'
      exit

   case DLG_OBJ_BUTTON
      ::ClassName := 'WVTBUTTON'
      ::lTabStop  := .f.
      exit

   case DLG_OBJ_TOOLBAR
      ::ClassName := 'WVTTOOLBAR'
      ::lTabStop  := .f.
      exit

   case DLG_OBJ_LABEL
      ::ClassName := 'WVTLABEL'
      ::lTabStop  := .f.
      exit

   case DLG_OBJ_SCROLLBAR
      ::ClassName := 'WVTSCROLLBAR'
      ::lTabStop  := .f.
      exit

   case DLG_OBJ_STATUSBAR
      ::ClassName := 'WVTSTATUSBAR'
      ::lTabStop  := .f.
      exit

   case DLG_OBJ_BANNER
      ::ClassName := 'WVTBANNER'
      ::lTabStop  := .f.
      exit

   case DLG_OBJ_TEXTBOX
      ::ClassName := 'WVTTEXTBOX'
      ::lTabStop  := .f.
      exit

   case DLG_OBJ_PROGRESSBAR
      ::ClassName := 'WVTPROGRESSBAR'
      ::lTabStop  := .f.
      exit

   end

   RETURN Self

//-------------------------------------------------------------------//

METHOD Create() CLASS WvtObject

   ::Eval( ::bOnCreate )
   ::CreatePopup()

   RETURN Self

//-------------------------------------------------------------------//

METHOD Destroy() CLASS WvtObject

   if ::hFont <> nil
      Win_DeleteObject( ::hFont )
      ::hFont := nil
   endif

   if ::hPopup <> nil
      Wvt_DestroyMenu( ::hPopup )
      ::hPopup := nil
   endif

   RETURN Nil

//-------------------------------------------------------------------//

METHOD CreatePopup() CLASS WvtObject
   LOCAL i, nID

   if !empty( ::aPopup ) .and. ::hPopup == nil
      ::hPopup := Wvt_CreatePopupMenu()

      for i := 1 to len( ::aPopup )

         aSize( ::aPopup[ i ],3 )
         nID := ::nPopupItemID++
         ::aPopup[ i,3 ] := nID

         Wvt_AppendMenu( ::hPopup, MF_ENABLED + MF_STRING, nID, ::aPopup[ i,1 ] )
      next
   endif

   RETURN Self

//-------------------------------------------------------------------//

METHOD ShowPopup() CLASS WvtObject
   LOCAL lRet := .f., nRet, n, aPos

   if ::hPopup <> nil
      aPos := Wvt_GetCursorPos()

      nRet := Wvt_TrackPopupMenu( ::hPopup, TPM_CENTERALIGN +TPM_RETURNCMD, ;
                             aPos[ 1 ], aPos[ 2 ], 0, Wvt_GetWindowHandle() )
      if nRet > 0
         if ( n := ascan( ::aPopup, {|e_| e_[ 3 ] == nRet } ) ) > 0
            lRet := .t.

            if ISBLOCK( ::aPopup[ n,2 ] )
               Eval( ::aPopup[ n,2 ] )
            endif
         endif
      endif
   endif

   RETURN lRet

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                         Class WvtBrowse
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

CLASS WvtBrowse FROM WvtObject

   DATA   cAlias
   DATA   oBrw
   DATA   lHSBar                INIT .t.
   DATA   lVSBar                INIT .t.
   DATA   oHBar
   DATA   oVBar
   DATA   bTotalRecords
   DATA   bCurrentRecord
   DATA   bTotalColumns
   DATA   bCurrentColumn

   ACCESS cDesc                 INLINE if( ::cText == nil, '', ::cText )
   ASSIGN cDesc( cText )        INLINE ::cText := cText

   METHOD New()
   METHOD Create()
   METHOD PaintBlock()
   METHOD Hilite()
   METHOD DeHilite()
   METHOD HandleEvent()
   METHOD Refresh()
   METHOD SetVBar()
   METHOD SetHBar()
   METHOD SetTooltip()
   METHOD SaveSettings()
   METHOD RestSettings()
   METHOD NotifyChild()

ENDCLASS

//-------------------------------------------------------------------//

METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight ) CLASS WvtBrowse

   ::Super:New( oParent, DLG_OBJ_BROWSE, nID, nTop, nLeft, nBottom, nRight )

   RETURN Self

//-------------------------------------------------------------------//

METHOD Create() CLASS WvtBrowse

   Select( ::cAlias )

   ::nTop    := ::oBrw:nTop-2
   ::nLeft   := ::oBrw:nLeft-2
   ::nBottom := if( ::lHSBar, ::oBrw:nBottom, ::oBrw:nBottom+1 )
   ::nRight  := if( ::lVSBar, ::oBrw:nRight , ::oBrw:nRight+2  )

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

//-------------------------------------------------------------------//

METHOD SetVBar() CLASS WvtBrowse

   if ::lVSBar
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
   endif

   RETURN Self

//-------------------------------------------------------------------//

METHOD SetHBar() CLASS WvtBrowse

   if ::lHSBar
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
   endif

   RETURN Self

//-------------------------------------------------------------------//

METHOD Refresh() CLASS WvtBrowse
   LOCAL nWorkArea := Select()

   if ISBLOCK( ::bOnRefresh )
      eval( ::bOnRefresh, self )
   else
      Select( ::cAlias )

      ::oBrw:RefreshAll()
      ::oBrw:ForceStable()

      Select( nWorkArea )
   endif

   RETURN Self

//-------------------------------------------------------------------//

METHOD HandleEvent( nKey ) CLASS WvtBrowse
   Local lRet := .f.

   if valtype( ::bHandleEvent ) == 'B'
      lRet := eval( ::bHandleEvent, self, ::oParent:cPaintBlockID, ::oBrw, nKey )
   endif

   RETURN lRet

//-------------------------------------------------------------------//

METHOD NotifyChild( nIndex, nKey, oCurObj ) CLASS WvtBrowse
   Local xData, i

   if nIndex > 0 .and. nIndex <= len( ::aChildren )
      if valtype( ::aChildren[ nIndex, OBJ_CHILD_DATABLOCK ] ) == 'B'
         xData := eval( ::aChildren[ nIndex, OBJ_CHILD_DATABLOCK ] )
      endif

      eval( ::aChildren[ nIndex, OBJ_CHILD_REFRESHBLOCK ], ;
            ::aChildren[ nIndex, OBJ_CHILD_OBJ ],;
            ::aChildren[ nIndex, OBJ_CHILD_OBJ ]:oParent:cPaintBlockID, ;
            ::aChildren[ nIndex, OBJ_CHILD_OBJ ]:oBrw, ;
            nKey, ;
            xData )

      if ::aChildren[ nIndex, OBJ_CHILD_OBJ ]:nChildren > 0
         // Pretend if focus is current on this object
         //
         Eval( ::aChildren[ nIndex, OBJ_CHILD_OBJ ]:bOnFocus, ::aChildren[ nIndex, OBJ_CHILD_OBJ ] )

         for i := 1 to ::aChildren[ nIndex, OBJ_CHILD_OBJ ]:nChildren
            ::aChildren[ nIndex, OBJ_CHILD_OBJ ]:NotifyChild( i, nKey, ::aChildren[ nIndex, OBJ_CHILD_OBJ ] )
         next

         // Restore previous environments
         Eval( oCurObj:bOnFocus, oCurObj )
      endif
   endif

   RETURN Self

//----------------------------------------------------------------------//

METHOD Hilite() CLASS WvtBrowse
   LOCAL b := ::oBrw

   DispOutAt( b:nTop-2, b:nLeft-2, pad( ' '+::cDesc, b:nRight-b:nLeft+5 ), ::cColorHilite )

   RETURN Self

//-------------------------------------------------------------------//

METHOD DeHilite() CLASS WvtBrowse
   LOCAL b := ::oBrw

   DispOutAt( b:nTop-2, b:nLeft-2, pad( ' '+::cDesc, b:nRight-b:nLeft+5 ), ::cColorDeHilite )

   RETURN Self

//-------------------------------------------------------------------//

METHOD SetTooltip() CLASS WvtBrowse
   LOCAL cTip, nArea

   if ISBLOCK( ::bTooltip )
      ::SaveSettings()
      nArea := Select( ::cAlias )

      Select( ::cAlias )

      cTip := Eval( ::bTooltip )

      Select( nArea )

      ::RestSettings()
   endif

   if cTip <> nil
      ::Tooltip := cTip
   endif

   Wvt_SetTooltip( ::nTop, ::nLeft, ::nBottom, ::nRight, ::Tooltip )

   RETURN Self

//-------------------------------------------------------------------//

METHOD SaveSettings CLASS WvtBrowse

   if ISBLOCK( ::bSaveSettings )
      ::xSettings := Eval( ::bSaveSettings, self )
   endif

   RETURN Self

//-------------------------------------------------------------------//

METHOD RestSettings() CLASS WvtBrowse

   if ::xSettings <> nil .and. ISBLOCK( ::bRestSettings )
      Eval( ::bRestSettings, self )
   endif

   RETURN Self

//-------------------------------------------------------------------//

METHOD PaintBlock( nPaintObj ) CLASS WvtBrowse
   LOCAL bBlock, b := ::oBrw

   switch nPaintObj

   case 1
      bBlock := {|| Wvt_DrawBoxRaised( b:nTop-2,b:nLeft-2,b:nBottom+1,b:nRight+2 ) }
      aadd( ::aPaint, { bBlock, { WVT_BLOCK_BOX, b:nTop-3,b:nLeft-3,b:nBottom+2,b:nRight+3 } } )
      exit

   case 2
      bBlock := {|| Wvt_DrawBoxRecessed( b:nTop,b:nLeft,b:nBottom,b:nRight ) }
      aadd( ::aPaint, { bBlock, { WVT_BLOCK_BOX, b:nTop-1,b:nLeft-1,b:nBottom+1,b:nRight+1 } } )
      exit

   case 3
      bBlock := {|| Wvt_DrawGridHorz( b:nTop+3, b:nLeft, b:nRight, b:nBottom - b:nTop - 2 ) }
      aadd( ::aPaint, { bBlock, { WVT_BLOCK_GRID_H, b:nTop+4, b:nLeft+1, b:nBottom-1, b:nRight-1 } } )
      exit

   case 4
      bBlock := {|| Wvt_DrawGridVert( b:nTop, b:nBottom, b:aColumnsSep, len( b:aColumnsSep ) ) }
      aadd( ::aPaint, { bBlock, { WVT_BLOCK_GRID_V, b:nTop+1, b:nLeft+1, b:nBottom-1, b:nRight-1, b } } )
      exit

   end

   RETURN Self

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                            WvtStatusBar
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

CLASS WvtStatusBar FROM WvtObject

   DATA aPanels
   DATA cColor

   METHOD New()
   METHOD Create()
   METHOD SetPanels()
   METHOD SetText()
   METHOD SetIcon()
   METHOD Refresh()
   METHOD Update( nPanel, cText )
   METHOD PaintBlock()

ENDCLASS

//-------------------------------------------------------------------//

METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight ) CLASS WvtStatusBar

   DEFAULT nTop    TO oParent:MaxRow()
   DEFAULT nLeft   TO 0
   DEFAULT nBottom TO oParent:MaxRow()
   DEFAULT nRight  TO oParent:MaxCol()

   ::Super:New( oParent, DLG_OBJ_STATUSBAR, nID, nTop, nLeft, nBottom, nRight )

   ::cColor  := 'N/W'

   RETURN Self

//-------------------------------------------------------------------//

METHOD Create() CLASS WvtStatusBar

   ::Refresh()
   ::PaintBlock( DLG_OBJ_STATUSBAR, self )

   ::Super:Create()

   RETURN Self

//-------------------------------------------------------------------//

METHOD PaintBlock() CLASS WvtStatusBar
   LOCAL a_:= {}, nPanels

   aeval( ::aPanels, {|o| aadd( a_,o:nTop )   , aadd( a_,o:nLeft ), ;
                          aadd( a_,o:nBottom ), aadd( a_,o:nRight ) } )

   a_[ len( a_ ) ]++
   nPanels := len( ::aPanels )

   ::bPaint  := {|| Wvt_DrawStatusBar( nPanels, a_ ) }
   aadd( ::aPaint, { ::bPaint,;
            { WVT_BLOCK_STATUSBAR, ::nTop, ::nLeft, ::nBottom, ::nRight } } )

   RETURN Self

//-------------------------------------------------------------------//

METHOD SetPanels( aPanels )  CLASS WvtStatusBar
   LOCAL i, oPanel, nID
   LOCAL nLastCol := ::oParent:MaxCol()

   nID := 200000

   ::aPanels := {}

   oPanel := WvtPanel():New( ::oParent, ++nID, ::nTop, 0 )

   aadd( ::aPanels, oPanel )

   if aPanels <> nil
      for i := 1 to len( aPanels )
         if ::oParent:MaxCol() > aPanels[ i ]
            oPanel := WvtPanel():New( ::oParent, ++nID, ::nTop, aPanels[ i ] )
            aadd( ::aPanels, oPanel )
         endif
      next
   endif

   atail( ::aPanels ):nRight := nLastCol

   for i := len( ::aPanels ) - 1 TO 1 STEP -1
      oPanel        := ::aPanels[ i ]
      oPanel:nRight := ::aPanels[ i+1 ]:nLeft
      oPanel:cColor := ::cColor
   next

   RETURN self

//-------------------------------------------------------------------//

METHOD Update( nPanel, cText, cColor )
   LOCAL oPanel

   if nPanel > 0 .and. nPanel <= len( ::aPanels )
      oPanel        := ::aPanels[ nPanel ]
      oPanel:Text   := cText
      oPanel:cColor := if( cColor == nil, 'N/W', cColor )
      oPanel:Refresh()
   endif

   RETURN Self

//-------------------------------------------------------------------//

METHOD SetText( nPanel, cText, cColor )  CLASS WvtStatusBar
   LOCAL oPanel

   DEFAULT cColor TO ::cColor

   if nPanel > 0 .and. nPanel <= len( ::aPanels )
      oPanel        := ::aPanels[ nPanel ]
      oPanel:Text   := cText
      oPanel:cColor := cColor
   endif

   RETURN Self

//-------------------------------------------------------------------//

METHOD SetIcon( nPanel, cIconFile ) CLASS WvtStatusBar

   if nPanel > 0 .and. nPanel <= len( ::aPanels )
      ::aPanels[ nPanel ]:cIconFile := cIconFile
   endif

   RETURN Self

//-------------------------------------------------------------------//

METHOD Refresh() CLASS WvtStatusBar
   LOCAL i

   for i := 1 to len( ::aPanels )
      ::aPanels[ i ]:Refresh()
   next

   RETURN nil

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                         Class WvtPanel
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

CLASS WvtPanel FROM WvtObject

   DATA cColor
   DATA cTxt
   DATA cIconFile

   ACCESS Text                  INLINE ::cTxt
   ASSIGN Text( cText )         INLINE ::cTxt := pad( cText, ::nRight - ::nLeft-2 )

   METHOD New()
   METHOD Refresh()

ENDCLASS

//-------------------------------------------------------------------//

METHOD New( oParent, nId, nTop, nLeft ) CLASS WvtPanel

   ::Super:New( oParent, DLG_OBJ_PANEL, nId, nTop, nLeft, nTop )

   RETURN Self

//-------------------------------------------------------------------//

METHOD Refresh() CLASS WvtPanel

   if ::Text <> nil
      DispOutAt( ::nTop, ::nLeft+1, ::Text, ::cColor )
   endif

   RETURN Self

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                         Class WvtLabel
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

CLASS WvtLabel FROM WvtObject

   ACCESS Text                  INLINE if( ::cText == nil, '', ::cText )
   ASSIGN Text( cTxt )          INLINE ::cText := if( cTxt == nil, '', cTxt )

   METHOD New()
   METHOD Create()
   METHOD Configure()
   METHOD Refresh()
   METHOD HoverOn()
   METHOD HoverOff()
   METHOD SetText()
   METHOD SetTextColor()
   METHOD SetBackColor()

ENDCLASS

//-------------------------------------------------------------------//

METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight ) CLASS WvtLabel

   ::Super:New( oParent, DLG_OBJ_LABEL, nID, nTop, nLeft, nBottom, nRight )

   RETURN Self

//-------------------------------------------------------------------//

METHOD Create( lConfg ) CLASS WvtLabel

   DEFAULT lConfg       TO .f.

   DEFAULT ::nBottom    TO ::nTop
   DEFAULT ::nRight     TO ::nLeft + len( ::Text )
   DEFAULT ::nTextColor TO RGB( 0,0,0 )

   ::nTextColorHoverOff := ::nTextColor
   ::nBackColorHoverOff := ::nBackColor

   ::hFont := Wvt_CreateFont( ::cFont, ::nFontHeight, ::nFontWidth, ::nFontWeight, ::lItalic,;
                              ::lUnderline, ::lStrikeout, ::nCharSet, ::nFontQuality, ::nAngle )
   if ::hFont <> 0
      if !( lConfg )
         ::bPaint := {|| Wvt_DrawLabelObj( ::nTop, ::nLeft, ::nBottom, ::nRight,;
                       ::Text, ::nAlignHorz, ::nAlignVert, ::nTextColor, ::nBackColor, ::hFont ) }
         aadd( ::aPaint, { ::bPaint, { WVT_BLOCK_LABEL, ::nTop, ::nLeft, ::nBottom, ::nRight } } )
      endif
   endif

   ::Super:Create()

   RETURN Self

//-------------------------------------------------------------------//

METHOD Refresh() CLASS WvtLabel

Eval( ::bPaint )

RETURN Self

//-------------------------------------------------------------------//

METHOD SetText( cTxt ) CLASS WvtLabel

   if valtype( cTxt ) == 'C'
      ::Text := cTxt
      ::Refresh()
   endif

   RETURN Self

//-------------------------------------------------------------------//

METHOD SetTextColor( nRGB ) CLASS WvtLabel

   if valtype( nRGB ) == 'N'
      ::nTextColor := nRGB
      ::nTextColorHoverOff := nRGB
      ::Refresh()
   endif

   RETURN Self

//-------------------------------------------------------------------//

METHOD SetBackColor( nRGB ) CLASS WvtLabel

   if valtype( nRGB ) == 'N'
      ::nBackColor := nRGB
      ::nBackColorHoverOff := nRGB
      ::Refresh()
   endif

   RETURN Self

//-------------------------------------------------------------------//

METHOD Configure() CLASS WvtLabel

   ::nTextColorHoverOff := ::nTextColor
   ::nBackColorHoverOff := ::nBackColor

   if ::hFont <> 0
      Win_DeleteObject( ::hFont )
   endif

   ::hFont := Wvt_CreateFont( ::cFont, ::nFontHeight, ::nFontWidth, ::nFontWeight, ::lItalic,;
                              ::lUnderline, ::lStrikeout, ::nCharSet, ::nFontQuality, ::nAngle )

   RETURN Self

//-------------------------------------------------------------------//

METHOD HoverOn() CLASS WvtLabel
   LOCAL lOn := .f.

   if ::nTextColorHoverOn <> nil
      lOn := .t.
      ::nTextColor := ::nTextColorHoverOn
   endif
   if ::nBackColorHoverOn <> nil
      lOn := .t.
      ::nBackColor := ::nBackColorHoverOn
   endif

   if lOn
      ::Refresh()
   endif

   RETURN Self

//-------------------------------------------------------------------//

METHOD HoverOff() CLASS WvtLabel
   LOCAL lOn := .f.

   if ::nTextColorHoverOn <> nil
      lOn := .t.
      ::nTextColor := ::nTextColorHoverOff
   endif
   if ::nBackColorHoverOn <> nil
      lOn := .t.
      ::nBackColor := ::nBackColorHoverOff
   endif

   if lOn
      ::Refresh()
   endif

   Return Self

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                       Class WvtToolBar
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

CLASS WvtToolBar FROM WvtObject

   DATA   nPaintID
   DATA   aObjects              INIT {}
   DATA   lHidden               INIT .f.
   DATA   nCurButton            INIT 0
   DATA   lActive
   DATA   lFloating
   DATA   wScreen
   DATA   cScreen
   DATA   nBtnLeft              INIT 0
   DATA   nRGBSep               INIT RGB( 150,150,150 )

   ACCESS nButtons              INLINE len( ::aButtons )

   METHOD New()
   METHOD Create()
   METHOD Refresh()
   METHOD AddButton()
   METHOD PaintToolBar()
   METHOD HoverOn()
   METHOD HoverOff()

ENDCLASS

//-------------------------------------------------------------------//

METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight ) CLASS WvtToolBar

   nTop    := 0
   nLeft   := 0
   DEFAULT nBottom TO 1
   nRight  := oParent:MaxCol()

   ::Super:New( oParent, DLG_OBJ_TOOLBAR, nID, nTop, nLeft, nBottom, nRight )

   ::lActive   := .t.
   ::lFloating := .F.
   ::nPaintID  := ::oParent:nPaintID++

   RETURN Self

//-------------------------------------------------------------------//

METHOD Create() CLASS WvtToolBar

   if ::lFloating
      ::lActive := .f.
      ::lHidden := .t.
   endif

   aeval( ::aObjects, {|o| o:lActive := ::lActive } )

   ::bPaint := {|| ::PaintToolBar() }
   aadd( ::aPaint, { ::bPaint,;
            { WVT_BLOCK_TOOLBAR, ::nTop, ::nLeft, ::nBottom, ::nRight } } )

   ::Super:Create()

   RETURN Self

//-------------------------------------------------------------------//

METHOD Refresh() CLASS WvtToolBar

   if ::lFloating
      DispBox( ::nTop, ::nLeft, ::nBottom, ::nRight, '         ', 'n/w' )
   else
      Wvt_InvalidateRect( ::nTop, ::nLeft, ::nTop, ::nLeft )
   endif

   RETURN Self

//-------------------------------------------------------------------//

METHOD PaintToolBar() CLASS WvtToolBar

   if ( ::lActive )
      Wvt_DrawLine( ::nTop, ::nLeft, ::nBottom, ::nRight, 0, 1, 2, , , ::nRGBSep )
   endif

   RETURN Self

//-------------------------------------------------------------------//

METHOD AddButton( cFileImage, bBlock, cTooltip ) CLASS WvtToolBar
   LOCAL oObj, nCol

   nCol := ( ::nBottom-::nTop+1 ) * 2

   oObj := WvtToolButton():New( self )

   oObj:lActive    := ::lActive
   oObj:nTop       := ::nTop
   oObj:nLeft      := ::nBtnLeft + 1
   oObj:nBottom    := ::nBottom

   if valtype( cFileImage ) == 'C'
      oObj:nBtnType   := TLB_BUTTON_TYPE_IMAGE
      oObj:nRight     := oObj:nLeft + nCol - 1
      oObj:cFileImage := cFileImage
      oObj:bOnLeftUp  := bBlock
      oObj:Tooltip    := cTooltip
   else
      oObj:nBtnType   := TLB_BUTTON_TYPE_SEPARATOR
      oObj:nRight     := oObj:nLeft
   endif

   aadd( ::aObjects, oObj )

   ::nBtnLeft         := oObj:nRight + 1
   ::nCurButton++

   ::oParent:AddObject( oObj )

   RETURN Self

//-------------------------------------------------------------------//

METHOD HoverOn()

   if ::lFloating .and. ::lHidden
      ::lHidden   := .f.
      ::lActive   := .t.
      // ::cScreen   := SaveScreen( ::nTop, ::nLeft, ::nBottom, ::nRight )
      // ::wScreen   := Wvt_SaveScreen( ::nTop, ::nLeft, ::nBottom, ::nRight )

      aeval( ::aObjects, {|o| o:lActive := ::lActive } )

      ::Refresh()
   endif

   RETURN self

//-------------------------------------------------------------------//

METHOD HoverOff()

   if ::lFloating .and. !( ::lHidden )
      ::lHidden := .t.
      ::lActive := .f.
      aeval( ::aObjects, {|o| o:lActive := ::lActive } )
      // RestScreen( ::nTop, ::nLeft, ::nBottom, ::nRight, ::cScreen )
      // Wvt_RestScreen( ::nTop, ::nLeft, ::nBottom, ::nRight, ::wScreen, .f. )
      ::Refresh()
   endif

   RETURN Self

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                     Class WvtToolButton
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

CLASS WvtToolButton FROM WvtObject

   DATA   cFileImage
   DATA   nCurState             INIT 0
   DATA   nBtnType              INIT TLB_BUTTON_TYPE_IMAGE
   DATA   aPxlOffSet            INIT { 0, -1, -3, 1 }

   METHOD New()
   METHOD Create()
   METHOD Refresh()
   METHOD LeftDown()
   METHOD LeftUp()
   METHOD HoverOn()
   METHOD HoverOff()
   METHOD PaintButton()

   ENDCLASS

//-------------------------------------------------------------------//

METHOD New( oParent ) CLASS WvtToolButton

   ::Super:New( oParent, DLG_OBJ_BUTTON )

   RETURN Self

//-------------------------------------------------------------------//

METHOD Create() CLASS WvtToolButton

   ::bPaint := {|| ::PaintButton() }
   aadd( ::aPaint, { ::bPaint,;
               { WVT_BLOCK_BUTTON, ::nTop, ::nLeft, ::nBottom, ::nRight }} )

   ::Super:Create()

   RETURN Self

//-------------------------------------------------------------------//

METHOD Refresh() CLASS WvtToolButton

   if ::lActive
      Eval( ::bPaint )
   endif

   RETURN Self

//-------------------------------------------------------------------//

METHOD PaintButton() CLASS WvtToolButton

   if ::lActive
      if ::nBtnType == TLB_BUTTON_TYPE_IMAGE
         Wvt_DrawImage( ::nTop, ::nLeft, ::nBottom, ::nRight, ::cFileImage, {4,4,-6,-4} )
      else
         Wvt_DrawLine( ::nTop, ::nLeft, ::nBottom, ::nRight, 1, 1, , , , ::oParent:nRGBSep )
      endif
   endif

   RETURN Self

//-------------------------------------------------------------------//

METHOD LeftDown() CLASS WvtToolButton
   LOCAL lRet := .f.

   if ::lActive .and. ::nBtnType == TLB_BUTTON_TYPE_IMAGE
      Wvt_DrawToolButtonState( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet, 2 )
      lRet := .t.
   endif

   RETURN lRet

//-------------------------------------------------------------------//

METHOD LeftUp() CLASS WvtToolButton
   LOCAL lRet := .f.

   if ::lActive .and. ::nBtnType == TLB_BUTTON_TYPE_IMAGE
      Wvt_DrawToolButtonState( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet, 1 )
      Eval( ::bOnLeftUp )
      lRet := .t.
   endif

   RETURN lRet

//-------------------------------------------------------------------//

METHOD HoverOn() CLASS WvtToolButton

   ::oParent:HoverOn()

   if ::lActive .and. ::nBtnType == TLB_BUTTON_TYPE_IMAGE
      Wvt_DrawToolButtonState( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet, 1 )
   endif

   RETURN Self

//-------------------------------------------------------------------//

METHOD HoverOff() CLASS WvtToolButton

   ::oParent:HoverOff()

   if ::lActive .and. ::nBtnType == TLB_BUTTON_TYPE_IMAGE
      Wvt_DrawToolButtonState( ::nTop, ::nLeft, ::nBottom, ::nRight,::aPxlOffSet, 0 )
   endif

   RETURN Self

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                          Class WvtImage
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

CLASS WvtImage FROM WvtObject

   DATA   cImageFile

   ACCESS cImage                INLINE ::cImageFile
   ASSIGN cImage( cImg )        INLINE ::cImageFile := cImg

   METHOD New()
   METHOD Create()
   METHOD SetImage()

   ENDCLASS

//-------------------------------------------------------------------//

METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight ) CLASS WvtImage

   ::Super:New( oParent, DLG_OBJ_IMAGE, nId, nTop, nLeft, nBottom, nRight )

   RETURN Self

//-------------------------------------------------------------------//

METHOD Create() CLASS WvtImage

   ::bPaint := {|| if( file( ::cImage ), ;
        Wvt_DrawImage( ::nTop, ::nLeft, ::nBottom, ::nRight, ::cImage ),'' ) }

   aadd( ::aPaint, { ::bPaint,;
               { WVT_BLOCK_IMAGE, ::nTop, ::nLeft, ::nBottom, ::nRight } } )

   ::Super:Create()

   RETURN Self

//-------------------------------------------------------------------//

METHOD SetImage( cImage ) CLASS WvtImage

   if cImage <> nil .and. file( cImage )
      ::cImageFile := cImage
      ::Refresh()
   endif

   RETURN Self

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                          Class WvtStatic
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

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

   DATA   nHorzVert             INIT 0
   DATA   aRGBb
   DATA   aRGBe

   DATA   aPxlOffSet            INIT {}

   METHOD New()
   METHOD Create()
   METHOD Refresh()
   METHOD HoverOn()
   METHOD HoverOff()

   ENDCLASS

//-------------------------------------------------------------------//

METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight ) CLASS WvtStatic

   ::Super:New( oParent, DLG_OBJ_STATIC, nID, nTop, nLeft, nBottom, nRight )

   RETURN Self

//-------------------------------------------------------------------//

METHOD Create() CLASS WvtStatic
   LOCAL lInside := .f.

   switch ::nStatic

   case WVT_STATIC_LINE
      lInside := .t.
      ::bPaint  := {|| Wvt_DrawLine( ::nTop, ::nLeft, ::nBottom, ::nRight, ;
               ::nOrient, ::nFormat, ::nAlign, ::nStyle, ::nThick, ::nColor ) }
      exit

   case WVT_STATIC_BOXRAISED
      ::bPaint := {|| Wvt_DrawBoxRaised( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      exit

   case WVT_STATIC_BOXRECESSED
      ::bPaint := {|| Wvt_DrawBoxRecessed( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      exit

   case WVT_STATIC_BOXGROUP
      ::bPaint := {|| Wvt_DrawBoxGroup( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      exit

   case WVT_STATIC_BOXGROUPRAISED
      ::bPaint := {|| Wvt_DrawBoxGroupRaised( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      exit

   case WVT_STATIC_OUTLINE
      ::bPaint := {|| Wvt_DrawOutline( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      exit

   case WVT_STATIC_RECTANGLE
      lInside := .t.
      ::bPaint := {|| Wvt_DrawRectangle( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      exit

   case WVT_STATIC_ROUNDRECT
      lInside := .t.
      ::bPaint := {|| Wvt_DrawRoundRect( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      exit

   case WVT_STATIC_FOCUSRECT
      lInside := .t.
      ::bPaint := {|| Wvt_DrawFocusRect( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      exit

   case WVT_STATIC_ELLIPSE
      lInside := .t.
      ::bPaint := {|| Wvt_DrawEllipse( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlOffSet ) }
      exit

   case WVT_STATIC_SHADEDRECT
      lInside := .t.
      ::bPaint := {|| Wvt_DrawShadedRect( ::nTop, ::nLeft, ::nBottom, ::nRight, ;
                                ::aPxlOffSet, ::nHorzVert, ::aRGBb, ::aRGBe ) }
      exit

   end

   if lInside
      ::nfTop    := ::nTop
      ::nfLeft   := ::nLeft
      ::nfBottom := ::nBottom
      ::nfRight  := ::nRight
   else
      ::nfTop    := ::nTop    - 1
      ::nfLeft   := ::nLeft   - 1
      ::nfBottom := ::nBottom + 1
      ::nfRight  := ::nRight  + 1
   endif

   aadd( ::aPaint, { ::bPaint,;
             { WVT_BLOCK_STATIC, ::nfTop, ::nfLeft, ::nfBottom, ::nfRight }} )

   ::Super:Create()

   RETURN Self

//-------------------------------------------------------------------//

METHOD HoverOn()

   RETURN Self

//-------------------------------------------------------------------//

METHOD HoverOff()

   RETURN Self

//-------------------------------------------------------------------//

METHOD Refresh() CLASS WvtStatic

   Eval( ::bPaint )

   RETURN Self

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                      Class WvtPushButton
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

CLASS WvtPushButton FROM WvtObject

   DATA   cCaption
   DATA   cFileImage

   ACCESS block                 INLINE ::bOnLeftUp
   ASSIGN block( bBlock )       INLINE ::bOnLeftUp := bBlock

   METHOD New()
   METHOD Create()
   METHOD LeftDown()
   METHOD LeftUp()
   METHOD PaintButton()

ENDCLASS

//-------------------------------------------------------------------//

METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight ) CLASS WvtPushButton

   ::Super:New( oParent, DLG_OBJ_PUSHBUTTON, nID, nTop, nLeft, nBottom, nRight )

   RETURN Self

//-------------------------------------------------------------------//

METHOD Create() CLASS WvtPushButton

   ::bPaint := {|| ::PaintButton() }

   aadd( ::aPaint, { ::bPaint,;
               { WVT_BLOCK_BUTTON, ::nTop, ::nLeft, ::nBottom, ::nRight } } )

   ::Super:Create()

   RETURN Self

//-------------------------------------------------------------------//

METHOD PaintButton() CLASS WvtPushButton

   if ::cCaption == nil
      Wvt_DrawImage( ::nTop, ::nLeft, ::nBottom, ::nRight, ::cFileImage, { 4, 4,-4, -4 } )
   else
      Wvt_DrawButton( ::nTop, ::nLeft, ::nBottom, ::nRight, ::cCaption, , 4 )
   endif
   Wvt_DrawToolButtonState( ::nTop, ::nLeft, ::nBottom, ::nRight, {0,0,0,0}, 1 )

   RETURN Self

//-------------------------------------------------------------------//

METHOD LeftDown() CLASS WvtPushButton

   Wvt_DrawToolButtonState( ::nTop, ::nLeft, ::nBottom, ::nRight,{0,0,0,0} , 2 )

   RETURN .t.

//-------------------------------------------------------------------//

METHOD LeftUp() CLASS WvtPushButton

   Wvt_DrawToolButtonState( ::nTop, ::nLeft, ::nBottom, ::nRight, {0,0,0,0}, 1 )
   ::Eval( ::bOnLeftUp )

   RETURN .t.

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                           Class WvtGets
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

CLASS WvtGets FROM WvtObject

   DATA   aGetList              INIT  {}
   DATA   nLastGet              INIT  1
   DATA   nCurGet               INIT  1
   DATA   GetList               INIT  {}
   DATA   cDesc                 INIT  ''

   METHOD New()
   METHOD Create()
   METHOD KillFocus()
   METHOD SetFocus()
   METHOD HandleEvent()
   METHOD AddGets()
   METHOD PaintBlock()
   METHOD Read()
   METHOD Hilite()
   METHOD DeHilite()
   METHOD GetData()
   METHOD SetData()

ENDCLASS

//-------------------------------------------------------------------//

METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight ) CLASS WvtGets

   ::Super:New( oParent, DLG_OBJ_GETS, nID, nTop, nLeft, nBottom, nRight )

   RETURN Self

//-------------------------------------------------------------------//

METHOD Create() CLASS WvtGets
   LOCAL i, GetList
   LOCAL nCurRow := row()
   LOCAL nCurCol := Col()

   for i := 1 to len( ::aGetList )
      GetList := {}

      DEFAULT ::aGetList[ i,7 ] TO 'N/W*,N/W*,,,N/GR*'
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

//-------------------------------------------------------------------//

METHOD PaintBlock( nIndex ) CLASS WvtGets
   LOCAL nLen, bPaint

   nLen   := len( Transform( ::aGetList[ nIndex,3 ], ::aGetList[ nIndex,4 ] ) )

   bPaint := {|| Wvt_DrawBoxGet( ::aGetList[ nIndex,1 ], ::aGetList[ nIndex,2 ], nLen ) }

   aadd( ::aPaint, { bPaint,;
               { WVT_BLOCK_GETS, ::aGetList[ nIndex,1 ]-1, ::aGetList[ nIndex,2 ]-1, ;
                     ::aGetList[ nIndex,1 ]-1,  ::aGetList[ nIndex,2 ]+nLen } } )

   RETURN Self

//-------------------------------------------------------------------//

METHOD SetFocus() CLASS WvtGets

   RETURN Self

//-------------------------------------------------------------------//

METHOD KillFocus() CLASS WvtGets

   RETURN Self

//-------------------------------------------------------------------//

METHOD AddGets( nRow, nCol, xVar, cPic, cColor, bValid, bWhen ) CLASS WvtGets

   aadd( ::aGetList, { nRow, nCol, xVar, cPic, bValid, bWhen, cColor } )

   RETURN Self

//-------------------------------------------------------------------//

METHOD HandleEvent( nKey ) CLASS WvtGets
   Local lRet := .f.

   do case
   case nKey == K_LDBLCLK
      ::Read()
      lRet := .t.
   endcase

   RETURN lRet

//-------------------------------------------------------------------//

METHOD Read() CLASS WvtGets

   ReadModal( ::GetList, ::nCurGet )

   RETURN Self

//-------------------------------------------------------------------//

METHOD GetData() CLASS WvtGets
   LOCAL aData

   RETURN aData

//-------------------------------------------------------------------//

METHOD SetData( /*aData*/ )

   RETURN Self

//-------------------------------------------------------------------//

METHOD Hilite() CLASS WvtGets

   DispOutAt( ::nTop, ::nLeft, pad( ' '+::cDesc, ::nRight-::nLeft+1 ), ::cColorHilite )

   RETURN Self

//-------------------------------------------------------------------//

METHOD DeHilite() CLASS WvtGets

   DispOutAt( ::nTop, ::nLeft, pad( ' '+::cDesc, ::nRight-::nLeft+1 ), ::cColorDeHilite )

   RETURN Self

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                       Class WvtScrollBar
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

CLASS WvtScrollBar FROM WvtObject

   DATA   nBarType              INIT WVT_SCROLLBAR_VERT

   DATA   nTotal                INIT 100
   DATA   nCurrent              INIT 1
   DATA   nThumbPos             INIT 0
   DATA   nBlockNo              INIT 1

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

   DATA   lHidden               INIT .t.

   DATA   aPxlBtnTop            INIT {0,0,0,0}
   DATA   aPxlBtnLft            INIT {0,0,0,0}
   DATA   aPxlBtnBtm            INIT {0,0,0,0}
   DATA   aPxlBtnRgt            INIT {0,0,0,0}
   DATA   aPxlScroll            INIT {0,0,0,0}

   DATA   lLeftDown             INIT .f.
   DATA   lOnThumb              INIT .f.
   DATA   lAnchored             INIT .f.
   DATA   lOnLeftDown           INIT .f.

   DATA   nScrollUnits          INIT 0

   METHOD New()
   METHOD Create()
   METHOD Configure()
   METHOD Refresh()
   METHOD HandleEvent()
   METHOD SetPos()
   METHOD GetPos()
   METHOD ThumbPos()
   METHOD SetTooltip()

ENDCLASS

//-------------------------------------------------------------------//

METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight ) CLASS WvtScrollBar

   ::Super:New( oParent, DLG_OBJ_SCROLLBAR, nID, nTop, nLeft, nBottom, nRight )

   RETURN Self

//-------------------------------------------------------------------//

METHOD Create() CLASS WvtScrollBar

   if ::nTop == nil .or. ::nLeft == nil
      return nil
   endif

   if ::nBarType == WVT_SCROLLBAR_VERT
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

   else
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
           {|| Wvt_DrawScrollThumbHorz( ::nSTop,::nSLeft,::nSBottom,::nSRight,;
                                            ::aPxlScroll,::nThumbPos ) }
      ::bBtnLeftTopDep := ;
           {|| Wvt_DrawScrollButton( ::nBtn1Top,::nBtn1Left,::nBtn1Bottom,::nBtn1Right,::aPxlBtnLft,2,.t. ) }
      ::bBtnRightBottomDep := ;
           {|| Wvt_DrawScrollButton( ::nBtn2Top,::nBtn2Left,::nBtn2Bottom,::nBtn2Right,::aPxlBtnRgt,4,.t. ) }

   endif

   ::bOnLeftUp      := {|| ::HandleEvent( K_LBUTTONUP      ) }
   ::bOnLeftDown    := {|| ::HandleEvent( K_LBUTTONDOWN    ) }
   ::bOnMMLeftDown  := {|| ::HandleEvent( K_MMLEFTDOWN     ) }
   ::bOnLeftPressed := {|| ::HandleEvent( K_LBUTTONPRESSED ) }

   Eval( ::bBtnLeftTop     )
   Eval( ::bBtnRightBottom )

   ::Super:Create()

   RETURN Self

//-------------------------------------------------------------------//

METHOD Configure( nTop, nLeft, nBottom, nRight ) CLASS WvtScrollBar

   ::nTop     := nTop
   ::nLeft    := nLeft
   ::nBottom  := nBottom
   ::nRight   := nRight

   ::Create()
   ::Refresh()

   RETURN Self

//-------------------------------------------------------------------//

METHOD Refresh() CLASS WvtScrollBar

   Eval( ::bBtnScroll )

   RETURN Self

//-------------------------------------------------------------------//

METHOD SetPos( nTotal, nCurrent ) CLASS WvtScrollBar

   DEFAULT nTotal   TO Eval( ::bTotal   )
   DEFAULT nCurrent TO Eval( ::bCurrent )

   ::nTotal   := nTotal
   ::nCurrent := nCurrent

   ::ThumbPos()
   ::Refresh()

   RETURN Self

//-------------------------------------------------------------------//

METHOD ThumbPos() CLASS WvtScrollBar
   LOCAL  nNewPos     := ::nThumbPos
   LOCAL  nRecPerUnit, nCurUnit

   if ::nBarType == WVT_SCROLLBAR_VERT
      nRecPerUnit := ::nTotal / ::nScrollUnits
      nCurUnit    := int( ::nCurrent / nRecPerUnit )

      if ::nCurrent == 1
         nCurUnit := 0
      elseif ::nCurrent == ::nTotal
         nCurUnit := ::nScrollUnits
      endif
      nNewPos     := ::nSTop + nCurUnit

      if nNewPos < ::nSTop
         nNewPos := ::nSTop
      elseif nNewPos > ::nSBottom
         nNewPos := ::nSBottom
      endif

   else
      if ::nTotal < ::nScrollUnits
         nCurUnit := ::nCurrent * int( ::nScrollUnits / ::nTotal )
      else
         nRecPerUnit := ::nTotal / ::nScrollUnits
         nCurUnit    := int( ::nCurrent / nRecPerUnit )
      endif

      if ::nCurrent == 1
         nCurUnit := 0
      elseif ::nCurrent == ::nTotal
         nCurUnit := ::nScrollUnits
      endif

        nNewPos := ::nSLeft + nCurUnit

        if nNewPos < ::nSLeft
           nNewPos := ::nSLeft
      elseif nNewPos > ::nSRight - 1
           nNewPos := ::nSRight-1
      endif

   endif

   ::nThumbPos := nNewPos

   RETURN Self

//-------------------------------------------------------------------//

METHOD GetPos() CLASS WvtScrollBar

   RETURN ::nCurrent

//-------------------------------------------------------------------//

METHOD SetTooltip() CLASS WvtScrollBar

   ::Tooltip := ltrim( str( ::nCurrent,12,0 ) ) + ' / ' + ;
                ltrim( str( ::nTotal  ,12,0 ) )

   Wvt_SetToolTip( ::nTop, ::nLeft, ::nBottom, ::nRight, ::Tooltip )

   RETURN Self

//-------------------------------------------------------------------//

METHOD HandleEvent( nKey ) CLASS WvtScrollBar
   LOCAL nmRow, nmCol, nOff
   LOCAL lHit  := .F.
   LOCAL mKeys_:={ K_LBUTTONDOWN, K_LBUTTONUP, K_MMLEFTDOWN, K_LBUTTONPRESSED }

   if ascan( mKeys_, nKey ) == 0
      return .f.
   endif

   nmRow := MRow()
   nmCol := MCol()

   do case
   case ::nBarType == WVT_SCROLLBAR_VERT
      lHit := .t.

      do case
      case ::lAnchored .and. nKey == K_MMLEFTDOWN
         if nmRow <> ::nThumbPos
            nOff := ::nThumbPos - nmRow
            if nOff > 0
               ::nThumbPos := max( ::nTop+1, nmRow )
            else
               ::nThumbPos := min( ::nBottom-1, nmRow )
            endif
            ::nCurrent := ( ::nTotal * ( ::nThumbPos - ::nTop ) / ::nScrollUnits )

            if ::nCurrent > ::nTotal
               ::nCurrent := ::nTotal
            endif
            if ::nCurrent < 1
               ::nCurrent := 1
            endif

            ::SetPos( ::nTotal, ::nCurrent )

            ::SetTooltip()
            Wvt_Keyboard( K_SBTHUMBTRACKVERT )
         else
            lHit := .f.
         endif

      case ::lAnchored .and. nKey == K_LBUTTONUP
         ::lAnchored := .f.

      otherwise
         lHit := .f.

         if nmCol >= ::nLeft .and. nmCol <= ::nRight
            lHit := .t.

            do case
            case nmRow == ::nThumbPos .and. nKey == K_LBUTTONDOWN
               ::lAnchored := .t.

            case nKey == K_LBUTTONUP
               if ( lHit := ::lOnLeftDown )
                  do case
                  case nmRow == ::nTop
                     Eval( ::bBtnLeftTop )
                  case nmRow == ::nBottom
                     Eval( ::bBtnRightBottom )
                  case nmRow < ::nThumbPos .and. nmRow > ::nTop
                  case nmRow > ::nThumbPos .and. nmRow < ::nBottom
                  otherwise
                     lHit := .f.
                  endcase
                  if lHit
                     ::lOnLeftDown := .f.
                  endif
               endif

            case nKey == K_LBUTTONPRESSED
               if ( lHit := ::lOnLeftDown )
                  do case
                  case nmRow == ::nTop
                     Wvt_Keyboard( K_SBLINEUP   )
                  case nmRow == ::nBottom
                     Wvt_Keyboard( K_SBLINEDOWN )
                  case nmRow < ::nThumbPos .and. nmRow > ::nTop
                     Wvt_Keyboard( K_SBPAGEUP )
                  case nmRow > ::nThumbPos .and. nmRow < ::nBottom
                     Wvt_Keyboard( K_SBPAGEDOWN )
                  otherwise
                     lHit := .f.
                  endcase
               endif

            case nKey == K_LBUTTONDOWN
               do case
               case nmRow == ::nTop
                  Eval( ::bBtnLeftTopDep )
                  Wvt_Keyboard( K_SBLINEUP )
               case nmRow == ::nBottom
                  Eval( ::bBtnRightBottomDep )
                  Wvt_Keyboard( K_SBLINEDOWN )
               case nmRow < ::nThumbPos .and. nmRow > ::nTop
                  Wvt_Keyboard( K_SBPAGEUP   )
               case nmRow > ::nThumbPos .and. nmRow < ::nBottom
                  Wvt_Keyboard( K_SBPAGEDOWN )
               otherwise
                  lHit := .f.
               endcase
               if lHit
                  ::lOnLeftDown := .t.
               endif
            endcase
         endif

      endcase

   case ::nBarType == WVT_SCROLLBAR_HORZ
      do case
      case ::lAnchored .and. nKey == K_MMLEFTDOWN
         if ( lHit := ( nmCol < ::nThumbPos .or. nmCol > ::nThumbPos+1 ) )

            nOff := ::nThumbPos - nmCol
            if nOff > 0
               ::nThumbPos := max( ::nLeft+2, nmCol )
            else
               ::nThumbPos := min( ::nRight-2, nmCol )
            endif

            ::nCurrent := ( ::nTotal * ( ::nThumbPos - ::nLeft+1 ) / ::nScrollUnits )

            if ::nCurrent > ::nTotal
               ::nCurrent := ::nTotal
            endif
            if ::nCurrent < 1
               ::nCurrent := 1
            endif

            ::SetPos( ::nTotal, ::nCurrent )

            Wvt_Keyboard( K_SBTHUMBTRACKHORZ )
         endif

      case ::lAnchored .and. nKey == K_LBUTTONUP
         ::lAnchored := .f.
         lHit := .t.

      otherwise

         if ( lHit := nmRow == ::nTop .and. nmCol >= ::nLeft .and. nmCol <= ::nRight )

            do case
            case nKey == K_LBUTTONDOWN .and. nmCol >= ::nThumbPos .and. nmCol <= ::nThumbPos+1
               ::lAnchored := .t.

            case nKey == K_LBUTTONUP

               if ( lHit := ::lOnLeftDown )
                  do case
                  case nmCol >= ::nLeft    .and. nmCol <= ::nLeft+1
                     Eval( ::bBtnLeftTop )
                  case nmCol >= ::nRight-1 .and. nmCol <= ::nRight
                     Eval( ::bBtnRightBottom )
                  case nmCol <  ::nThumbPos
                  case nmCol >  ::nThumbPos+1
                  otherwise
                     lHit := .f.
                  endcase
                  if lHit
                     ::lOnLeftDown := .f.
                  endif
               endif

            case nKey == K_LBUTTONPRESSED
               if ( lHit := ::lOnLeftDown )
                  do case
                  case nmCol == ::nLeft  .or. nmCol == ::nLeft+1
                     Wvt_Keyboard( K_SBLINELEFT )
                  case nmCol == ::nRight .or. nmCol == ::nRight-1
                     Wvt_Keyboard( K_SBLINERIGHT )
                  case nmCol < ::nThumbPos
                     Wvt_Keyboard( K_SBPAGELEFT )
                  case nmCol > ::nThumbPos+1
                     Wvt_Keyboard( K_SBPAGERIGHT )
                  otherwise
                     lHit := .f.
                  endcase
               endif

            case nKey == K_LBUTTONDOWN
               do case
               case nmCol == ::nLeft  .or. nmCol == ::nLeft+1
                  Eval( ::bBtnLeftTopDep )
                  Wvt_Keyboard( K_SBLINELEFT )
               case nmCol == ::nRight .or. nmCol == ::nRight-1
                  Eval( ::bBtnRightBottomDep )
                  Wvt_Keyboard( K_SBLINERIGHT )
               case nmCol < ::nThumbPos
                  Wvt_Keyboard( K_SBPAGELEFT )
               case nmCol > ::nThumbPos+1
                  Wvt_Keyboard( K_SBPAGERIGHT )
               otherwise
                  lHit := .f.
               endcase
               if lHit
                  ::lOnLeftDown := .t.
               endif
            endcase
         endif
      endcase
   endcase

   RETURN lHit

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                        CLASS WvtBanner
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

CLASS WvtBanner FROM WvtObject

   DATA   nTimeDelay            INIT 0.5    // One-half Second
   DATA   nDirection            INIT 0      // LEFT 1-RIGHT
   DATA   nCharToSkip           INIT 1
   DATA   cText                 INIT ''
   DATA   cDispText             INIT ''
   DATA   nTextLen              INIT 0
   DATA   nTextIndex            INIT 0

   DATA   oLabel

   DATA   nAlignVert            INIT 2     // Center

   DATA   nCurSeconds           INIT 0
   DATA   nCurAlign

   METHOD New()
   METHOD Create()
   METHOD Configure()
   METHOD Refresh()
   METHOD HoverOn()
   METHOD HoverOff()
   METHOD OnTimer()
   METHOD SetText()
   METHOD Destroy()

ENDCLASS

//-------------------------------------------------------------------//

METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight ) CLASS WvtBanner

   ::Super:New( oParent, DLG_OBJ_BANNER, nID, nTop, nLeft, nBottom, nRight )

   RETURN Self

//-------------------------------------------------------------------//

METHOD Create() CLASS WvtBanner

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
   ::oLabel:nAlignHorz        := if( ::nDirection == 0, 0, 1 )
   ::oLabel:nTextColor        := ::nTextColor
   ::oLabel:nBackColor        := ::nBackColor
   ::oLabel:nTextColorHoverOn := ::nTextColorHoverOn
   ::oLabel:nBackColorHoverOn := ::nBackColorHoverOn

   ::oLabel:Create()

   ::nCurSeconds := Seconds()
   ::nTextLen    := len( ::cText )
   ::nTextIndex  := if( ::nDirection == 0, 1, ::nTextLen )
   ::nCurAlign   := ::nDirection

   ::Super:Create()

   RETURN Self

//-------------------------------------------------------------------//

METHOD Destroy() CLASS WvtBanner

   Win_DeleteObject( ::oLabel:hFont )

   RETURN nil

//-------------------------------------------------------------------//

METHOD Configure() CLASS WvtBanner

   RETURN Self

//-------------------------------------------------------------------//

METHOD OnTimer() CLASS WvtBanner

   ::Refresh()

   RETURN Self

//-------------------------------------------------------------------//

METHOD SetText( cText ) CLASS WvtBanner

   if cText <> nil
      ::cText := cText
      ::Refresh()
   endif

   RETURN Self

//-------------------------------------------------------------------//

METHOD Refresh() CLASS WvtBanner
   LOCAL nNewTime

   if abs( ( nNewTime := Seconds() ) - ::nCurSeconds ) >= ::nTimeDelay
      ::nCurSeconds := nNewTime

      if ::nDirection == 0
         ::nTextIndex++
         if ::nTextIndex > ::nTextLen
            ::nTextIndex := 1
            ::nCurAlign  := if( ::nCurAlign == 0, 1, 0 )
         endif

         if ::nCurAlign == 0   // Left
            ::cDispText := substr( ::cText,::nTextIndex )
         else                  // Right
            ::cDispText := substr( ::cText, 1, ::nTextIndex )
         endif
      else
         ::nTextIndex--
         if ::nTextIndex < 0
            ::nTextIndex := ::nTextLen
            ::nCurAlign := if( ::nCurAlign == 0, 1, 0 )
         endif

         if ::nCurAlign == 0   // Left
            ::cDispText := substr( ::cText,::nTextIndex )
         else                  // Right
            ::cDispText := substr( ::cText, 1, ::nTextIndex )
         endif
      endif

      ::oLabel:nAlignHorz := ::nCurAlign
      ::oLabel:SetText( ::cDispText )
      ::oLabel:Refresh()
   endif

   RETURN Self

//-------------------------------------------------------------------//

METHOD HoverOn() CLASS WvtBanner

   ::oLabel:HoverOn()

   RETURN Self

//-------------------------------------------------------------------//

METHOD HoverOff() CLASS WvtBanner

   ::oLabel:HoverOff()

   RETURN Self

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                        Class WvtTextBox
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

CLASS WvtTextBox FROM WvtObject

   DATA   cText                 INIT ''

   METHOD New()
   METHOD Create()
   METHOD Configure()
   METHOD Refresh()
   METHOD SetText()
   METHOD HoverOn()
   METHOD HoverOff()

ENDCLASS

//-------------------------------------------------------------------//

METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight ) CLASS WvtTextBox

   ::Super:New( oParent, DLG_OBJ_TEXTBOX, nID, nTop, nLeft, nBottom, nRight )

   RETURN Self

//-------------------------------------------------------------------//

METHOD Create() CLASS WvtTextBox

   ::nTextColorHoverOff := ::nTextColor

   ::hFont := Wvt_CreateFont( ::cFont, ::nFontHeight, ::nFontWidth, ;
                  ::nFontWeight, ::lItalic, ::lUnderline, ::lStrikeout, ;
                  ::nCharSet, ::nFontQuality, 0 )

   if ::hFont <> 0
      ::bPaint := {|| Wvt_DrawTextBox( ::nTop, ::nLeft, ::nBottom, ::nRight, ;
            ::aPxlTLBR, ::cText, ::nAlignHorz, ::nAlignVert, ;
            ::nTextColor, ::nBackColor, ::nBackMode, ::hFont ) }

      aadd( ::aPaint, { ::bPaint, { WVT_BLOCK_LABEL, ::nTop, ::nLeft, ::nBottom, ::nRight } } )
   endif

   ::Super:Create()

   RETURN Self

//-------------------------------------------------------------------//

METHOD Refresh() CLASS WvtTextBox

   Eval( ::bPaint )

   RETURN Self

//-------------------------------------------------------------------//

METHOD Configure() CLASS WvtTextBox

   RETURN Self

//-------------------------------------------------------------------//

METHOD SetText( cText ) CLASS WvtTextBox

   if cText <> nil
      ::cText := cText
      ::Refresh()
   endif

   RETURN Self

//-------------------------------------------------------------------//

METHOD HoverOn( /*cText*/ ) CLASS WvtTextBox

   if ::nTextColorHoverOn <> nil
      ::nTextColor := ::nTextColorHoverOn
      ::Refresh()
   endif

   RETURN Self

//-------------------------------------------------------------------//

METHOD HoverOff( /*cText*/ ) CLASS WvtTextBox

   if ::nTextColorHoverOn <> nil
      ::nTextColor := ::nTextColorHoverOff
      ::Refresh()
   endif

RETURN Self

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                       Class WvtProgressBar
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

CLASS WvtProgressBar FROM WvtObject

   DATA   cImage
   DATA   nDirection            INIT 0      // 0-Left-Right,Top-Bottom  1-Right-Left,Bottom-Top
   DATA   nStyle                INIT 0
   DATA   lVertical             INIT .f.
   DATA   lActive               INIT .f.

   DATA   nBarColor             INIT RGB( 0,0,128 )
   DATA   nCurrent              INIT 0
   DATA   nTotal                INIT 1
   DATA   nPercent              INIT 0
   DATA   cBackColor            INIT 'W/W'

   DATA   cScreen

   METHOD New()
   METHOD Create()
   METHOD Display()
   METHOD Activate()
   METHOD DeActivate()

ENDCLASS

//-------------------------------------------------------------------//

METHOD New( oParent, nID, nTop, nLeft, nBottom, nRight ) CLASS WvtProgressBar

   ::Super:New( oParent, DLG_OBJ_PROGRESSBAR, nID, nTop, nLeft, nBottom, nRight )

   RETURN Self

//-------------------------------------------------------------------//

METHOD Create() CLASS WvtProgressBar

   DEFAULT ::nTop       TO 0
   DEFAULT ::nLeft      TO 0
   DEFAULT ::nBottom    TO if( ::lVertical, ::nTop + 9, ::nTop )
   DEFAULT ::nRight     TO if( ::lVertical, ::nLeft + 1, ::nLeft + 19 )
   DEFAULT ::nTextColor TO RGB( 255,255,255 )
   DEFAULT ::nBackColor TO RGB( 198,198,198 )

   ::bPaint := {|| ::Display() }
   aadd( ::aPaint, { ::bPaint, { WVT_BLOCK_LABEL, ::nTop, ::nLeft, ::nBottom, ::nRight } } )

   ::Super:Create()

   RETURN Self

//-------------------------------------------------------------------//

METHOD Display( nCurrent, nTotal ) CLASS WvtProgressBar

   if !( ::lActive )
      return Self
   endif

   DEFAULT nCurrent TO ::nCurrent
   DEFAULT nTotal   TO ::nTotal

   ::nCurrent := nCurrent
   ::nTotal   := nTotal

   if ::nCurrent > ::nTotal
      ::nCurrent := ::nTotal
   endif

   ::nPercent := int( ::nCurrent / ::nTotal * 100 )

   Wvt_DrawProgressBar( ::nTop, ::nLeft, ::nBottom, ::nRight, ::aPxlTLBR, ::nPercent, ;
                        ::nBackColor, ::nBarColor, ::cImage, ::lVertical, ::nDirection )
   RETURN Self

//-------------------------------------------------------------------//

METHOD Activate() CLASS WvtProgressBar

   ::cScreen := SaveScreen( ::nTop, ::nLeft, ::nBottom, ::nRight )
   DispBox( ::nTop, ::nLeft, ::nBottom, ::nRight, '         ', ::cBackColor )
   ::lActive := .t.

   RETURN Self

//-------------------------------------------------------------------//

METHOD DeActivate() CLASS WvtProgressBar

   ::lActive  := .f.
   ::nCurrent := 0
   ::nTotal   := 1
   RestScreen( ::nTop, ::nLeft, ::nBottom, ::nRight, ::cScreen )
   ::cScreen := nil

   RETURN Self

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                           Class WvtMenu
//                            Peter Rees
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

CLASS wvtMenu

   METHOD Create( cCaption )
   METHOD AddItem( cCaption, bAction )
   METHOD DelAllItems()
   METHOD DelItem( nItem )
   METHOD EnableItem( nItemNum )
   METHOD DisableItem( nItemNum )
   METHOD NumItems()
   METHOD Destroy()
   METHOD GetItem( nItemNum )
   METHOD FindMenuItemById( nId )
   METHOD DrawMenuBar()

   CLASSVAR MenuItemId          INIT 1

   VAR    aItems
   VAR    hMenu
   VAR    Caption
   VAR    IdNumber

ENDCLASS

//-------------------------------------------------------------------//

METHOD Create( cCaption ) CLASS wvtMenu
   ::aItems := {}

   IF EMPTY( ::hMenu:= Wvt_CreateMenu() )
      //Throw( ErrorNew( "wvtMenu", 1000, "wvtMenu:Init()", "Create Menu Error", { cCaption, cCaption },"WVT.PRG" ) )
   ENDIF
   ::Caption:= IIF( cCaption == NIL, "", cCaption )

   RETURN(Self)

//-------------------------------------------------------------------//

METHOD Destroy() CLASS wvtMenu

   IF !EMPTY( ::hMenu )
      ::DelAllItems()

      IF !Wvt_DestroyMenu( ::hMenu )
         //Throw( ErrorNew( "wvtMenu", 1000, "wvtMenu:Destroy()", "Destroy menu FAILED", {},"WVT.PRG" ) )
      ENDIF
      ::hMenu:= 0
   ENDIF

   RETURN( .T. )

//-------------------------------------------------------------------//

METHOD AddItem(cCaption, bAction) CLASS wvtMenu
   LOCAL lResult:= .F., aItem

   IF !EMPTY( ::hMenu ) .AND. ( !EMPTY( cCaption ) .OR. !EMPTY( bAction ) )
      IF HB_ISOBJECT( bAction )
         cCaption:= IIF(!EMPTY(cCaption),cCaption,bAction:Caption)
         aItem:= {MF_POPUP,bAction:hMenu,cCaption,bAction} // bAction is a wvtMenu object reference
      ELSEIF HB_ISBLOCK(bAction)
         aItem:= {MF_STRING,::MenuItemId++,cCaption,bAction} // bAction is a code block to execute
      ELSEIF left( cCaption, 1 )=="-"
         aItem:= {MF_SEPARATOR,0,0,NIL}
      ELSE
         //Throw( ErrorNew( "wvtMenu", 3101, "wvtMenu:AddItem()", "Argument Error", { cCaption, bAction },"WVT.PRG" ) )
      ENDIF

      IF !Wvt_AppendMenu(::hMenu, aItem[WVT_MENU_TYPE],aItem[WVT_MENU_IDENTIFIER],aItem[WVT_MENU_CAPTION])
         //Throw( ErrorNew( "wvtMenu", 1000, "wvtMenu:AddItem()", "Add menu item", { cCaption, bAction },"WVT.PRG" ) )
      ENDIF

      AADD(::aItems, aItem)
      lResult:= .T.
   ENDIF

   RETURN( lResult )

//-------------------------------------------------------------------//

METHOD DelAllItems() CLASS wvtMenu
   LOCAL lResult:= .T.,  nItems

   nItems := ::NumItems()
   DO WHILE nItems>0 .AND. lResult
      lResult := ::DelItem( nItems )
      nItems--
   ENDDO

   RETURN ( lResult )

//-------------------------------------------------------------------//

METHOD DelItem( nItemNum ) CLASS wvtMenu
   LOCAL lResult:= .F.

   IF nItemNum > 0 .AND. nItemNum <= ::NumItems()
      IF ::aItems[ nItemNum,WVT_MENU_TYPE ]== MF_POPUP
         ::aItems[ nItemNum,WVT_MENU_MENUOBJ ]:Destroy()
      ENDIF

      IF ( lResult:= Wvt_DeleteMenu(::hMenu, nItemNum-1,MF_BYPOSITION)) // Remember ZERO base
         ADEL( ::aItems, nItemNum )
         ASIZE( ::aItems, LEN( ::aItems ) - 1 )
      ELSE
         //Throw( ErrorNew( "wvtMenu", 1000, "wvtMenu:DelItem()", "Delete menu item FAILED", { nItemNum },"WVT.PRG" ) )
      ENDIF
   ENDIF

   RETURN(lResult)

//-------------------------------------------------------------------//

METHOD EnableItem( nItemNum ) CLASS wvtMenu
   LOCAL nPrevious:= -1

   IF !EMPTY( ::hMenu ) && !EMPTY( nItemNum )
      nPrevious:= Wvt_EnableMenuItem( ::hMenu, nItemNum-1, MF_BYPOSITION + MF_ENABLED )
   ENDIF

   RETURN ( nPrevious )

//-------------------------------------------------------------------//

METHOD DisableItem( nItemNum ) CLASS wvtMenu
   LOCAL nPrevious:= -1

   IF !EMPTY( ::hMenu ) && !EMPTY( nItemNum )
      nPrevious:= Wvt_EnableMenuItem( ::hMenu, nItemNum-1, MF_BYPOSITION + MF_GRAYED )
   ENDIF

   RETURN ( nPrevious )

//-------------------------------------------------------------------//

METHOD NumItems() CLASS wvtMenu

   RETURN ( LEN( ::aItems ) )

//-------------------------------------------------------------------//

METHOD GetItem( nItemNum ) CLASS wvtMenu
   LOCAL nItems := ::NumItems(), aResult:= NIL

   IF nItemNum > 0 .AND. nItemNum <= nItems
      aResult:= ::aItems[ nItemNum ]
   ENDIF

   RETURN ( aResult )

//-------------------------------------------------------------------//

METHOD FindMenuItemById( nId ) CLASS wvtMenu
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

   RETURN ( aResult )

//-------------------------------------------------------------------//

METHOD DrawMenuBar() CLASS wvtMenu

   Wvt_DrawMenuBar()

   RETURN ( NIL )

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                         Class WvtConsole
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

CLASS WvtConsole FROM WvtObject

   METHOD New()
   METHOD Say()
   METHOD Box()

ENDCLASS

//-------------------------------------------------------------------//

METHOD New( oParent ) CLASS WvtConsole

   ::Super:New( oParent, DLG_OBJ_CONSOLE, , -1, -1, -1, -1 )

   RETURN Self

//-------------------------------------------------------------------//

METHOD Say( nRow, nCol, xExp, cColor ) CLASS WvtConsole
   LOCAL nCRow, nCCol, nCursor

   if nRow >=0 .and. nCol >= 0 .and. xExp <> nil
      nCursor := SetCursor( SC_NONE )
      nCRow   := Row()
      nCCol   := Col()
      DispOutAt( nRow, nCol, xExp, cColor )
      SetPos( nCRow, nCCol )
      SetCursor( nCursor )
   endif

RETURN Self

//-------------------------------------------------------------------//

METHOD Box( nRow, nCol, n2Row, n2Col, cBoxChars, cColor ) CLASS WvtConsole

   LOCAL nCRow, nCCol, nCursor

   if nRow >=0 .and. nCol >= 0
      nCursor := SetCursor( SC_NONE )
      nCRow   := Row()
      nCCol   := Col()
      DispBox( nRow, nCol, n2Row, n2Col, cBoxChars, cColor )
      SetPos( nCRow, nCCol )
      SetCursor( nCursor )
   endif

   RETURN Self

//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//
//                      TBrowseWVG From TBrowse
//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

#define _TBCI_COLOBJECT       1   // column object
#define _TBCI_COLWIDTH        2   // width of the column
#define _TBCI_COLPOS          3   // column position on screen
#define _TBCI_CELLWIDTH       4   // width of the cell
#define _TBCI_CELLPOS         5   // cell position in column
#define _TBCI_COLSEP          6   // column separator
#define _TBCI_SEPWIDTH        7   // width of the separator
#define _TBCI_HEADING         8   // column heading
#define _TBCI_FOOTING         9   // column footing
#define _TBCI_HEADSEP        10   // heading separator
#define _TBCI_FOOTSEP        11   // footing separator
#define _TBCI_DEFCOLOR       12   // default color
#define _TBCI_FROZENSPACE    13   // space after frozen columns
#define _TBCI_LASTSPACE      14   // space after last visible column
#define _TBCI_SIZE           14   // size of array with TBrowse column data

//-------------------------------------------------------------------//

CLASS TBrowseWVG FROM TBrowse

   DATA   aColumnsSep               INIT {}

   METHOD SetVisible()

   ENDCLASS

//-------------------------------------------------------------------//

METHOD SetVisible() CLASS TBrowseWVG
   Local lFirst, aCol, nColPos

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

   Return Self
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//
//-------------------------------------------------------------------//

