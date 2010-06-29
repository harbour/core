/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                            Harbour-Qt IDE
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                               27Jun2010
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "common.ch"
#include "hbclass.ch"
#include "hbqt.ch"
#include "hbide.ch"
#include "xbp.ch"

/*----------------------------------------------------------------------*/

#define  BRW_TYPE_DBF                             1
#define  BRW_TYPE_ARRAY                           2

/*----------------------------------------------------------------------*/

CLASS IdeBrowseManager INHERIT IdeObject

   DATA   oStack
   DATA   oPanel
   DATA   oMain
   DATA   qLayout
   DATA   qVSplitter
   DATA   qToolBar
   DATA   qLayMain

   DATA   aToolBtns                               INIT  {}
   DATA   aItems                                  INIT  {}

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()
   METHOD execEvent( cEvent, p )
   METHOD addTable( cFileDBF, cAlias, nRow, nCol )
   METHOD addArray( aData, aAttr, nRow, nCol )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeBrowseManager:new( oIde )

   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowseManager:destroy()
   #if 0
   LOCAL oBrw

   // Close tables and clear variables
   FOR EACH oBrw IN ::aItems
      Select( oBrw:cAlias )
      DbCloseArea()
   NEXT
   ::aItems := {}
   #endif
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowseManager:create( oIde )
   LOCAL oDock, qBtn

   DEFAULT oIde TO ::oIde
   ::oIde := oIde

   oDock := ::oIde:oEM:oQScintillaDock:oWidget

   ::oPanel := QWidget():new()
   oDock:setWidget( ::oPanel )

   ::qLayout := QVBoxLayout():new() ; ::oPanel:setLayout( ::qLayout )
   ::qLayout:setContentsMargins( 0,0,0,0 )
   ::qLayout:setSpacing( 2 )

   ::qToolbar := QToolbar():new()
   ::qLayout:addWidget( ::qToolbar )
   ::qToolbar:setIconSize( QSize():new( 16,16 ) )

   ::oStack := QStackedWidget():new()
   ::qLayout:addWidget( ::oStack )

   ::oMain := QWidget():new()
   ::oStack:addWidget( ::oMain )

   ::qLayMain := QHBoxLayout():new(); ::oMain:setLayout( ::qLayMain )
   ::qLayMain:setContentsMargins( 0,0,0,0 )
   ::qLayMain:setSpacing( 2 )

   ::qVSplitter := QSplitter():new()
   ::qVSplitter:setOrientation( Qt_Vertical )
   ::qLayMain:addWidget( ::qVSplitter )

   qBtn := QToolButton():new()
   qBtn:setTooltip( "Open a Table" )
   qBtn:setAutoRaise( .t. )
   qBtn:setIcon( hbide_image( "open" ) )
   ::connect( qBtn, "clicked()", {|| ::execEvent( "buttonOpen_clicked" ) } )
   aadd( ::aToolBtns, qBtn )

   ::qToolBar:addWidget( qBtn )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowseManager:execEvent( cEvent, p )
   LOCAL cTable, cPath

   HB_SYMBOL_UNUSED( p )

   SWITCH cEvent
   CASE "buttonOpen_clicked"
      IF !empty( cTable := hbide_fetchAFile( ::oIde:oDlg, "Select a Table", { { "Database File", "*.dbf" } }, ::oIde:cWrkFolderLast ) )
         hb_fNameSplit( cTable, @cPath )
         ::oIde:cWrkFolderLast := cPath
         ::addTable( cTable )
      ENDIF

      EXIT
   ENDSWITCH

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowseManager:addTable( cFileDBF, cAlias, nRow, nCol )
   LOCAL oBrw

   HB_SYMBOL_UNUSED( nRow     )
   HB_SYMBOL_UNUSED( nCol     )

   oBrw := IdeBrowse():new()
   oBrw:cTable := cFileDBF
   oBrw:cAlias := cAlias
   oBrw:create()

   ::qVSplitter:addWidget( oBrw:oWnd:oWidget )

   aadd( ::aItems, oBrw )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowseManager:addArray( aData, aAttr, nRow, nCol )

   HB_SYMBOL_UNUSED( aData )
   HB_SYMBOL_UNUSED( aAttr )
   HB_SYMBOL_UNUSED( nRow  )
   HB_SYMBOL_UNUSED( nCol  )

   RETURN Self

/*----------------------------------------------------------------------*/
//
//                            Class IdeBrowse
//
/*----------------------------------------------------------------------*/

CLASS IdeBrowse INHERIT IdeObject

   DATA   oWnd
   DATA   oBrw
   DATA   qLayout
   DATA   qForm
   DATA   qFLayout
   DATA   qSplitter
   DATA   aForm                                   INIT  {}

   DATA   nType                                   INIT  BRW_TYPE_DBF
   DATA   cAlias                                  INIT  ""
   DATA   cTable                                  INIT  ""
   DATA   aData                                   INIT  {}
   DATA   aStruct                                 INIT  {}
   DATA   aAttr                                   INIT  {}
   DATA   nIndex                                  INIT  0
   DATA   cDriver                                 INIT  "DBFCDX"
   DATA   cIndex                                  INIT  ""
   DATA   nOrder                                  INIT  0
   DATA   nArea                                   INIT  0
   DATA   nCursorType                             INIT  XBPBRW_CURSOR_CELL

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()
   METHOD execEvent( cEvent, p, p1 )
   METHOD buildBrowser()
   METHOD buildColumns()
   METHOD dataLink( nField )
   METHOD getPP( aStruct )

   METHOD skipBlock( nHowMany )
   METHOD goTop()
   METHOD goBottom()
   METHOD goTo( nRec )
   METHOD recNo()
   METHOD lastRec()
   METHOD next()
   METHOD previous()
   METHOD activated()
   METHOD buildForm()
   METHOD populateForm()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:new( oIde )

   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:create( oIde )
   LOCAL xVrb, cT
   LOCAL lMissing := .t.

   DEFAULT oIde TO ::oIde
   ::oIde := oIde

   IF !empty( ::aData )
      ::nType := BRW_TYPE_ARRAY
   ENDIF

   IF ::nType == BRW_TYPE_DBF
      IF !empty( ::cAlias ) .AND. empty( ::cTable )
         IF select( ::cAlias ) > 0
            lMissing := .f.
         ENDIF
      ENDIF

      IF lMissing .AND. !empty( ::cTable )
         IF empty( ::cAlias )
            USE ( ::cTable ) SHARED NEW VIA ( ::cDriver )
         ELSE
            USE ( ::cTable ) SHARED NEW ALIAS ( ::cAlias ) VIA ( ::cDriver )
         ENDIF
         IF NetErr()
            MsgBox( ::cTable, "Could not been opened!" )
            RETURN Self
         ENDIF
         IF empty( ::cAlias )
            ::cAlias := alias()
         ENDIF
      ENDIF

      ::aStruct := DbStruct()
   ELSE
      FOR EACH xVrb IN ::aData[ 1 ]
         cT := valtype( xVrb )
         aadd( ::aStruct, "Fld_" + hb_ntos( xVrb:__enumIndex() ) )
         aadd( ::aStruct, cT )
         IF cT == "N"
            aadd( ::aStruct, 12 )
            aadd( ::aStruct,  2 )
         ELSEIF cT == "D"
            aadd( ::aStruct,  8 )
            aadd( ::aStruct,  0 )
         ELSEIF cT == "L"
            aadd( ::aStruct,  1 )
            aadd( ::aStruct,  0 )
         ELSE
            aadd( ::aStruct, len( xVrb ) )
            aadd( ::aStruct,  0 )
         ENDIF
      NEXT
   ENDIF

   ::buildBrowser()
   ::buildColumns()
   ::buildForm()

   ::oBrw:configure()
   ::oBrw:forceStable()

   ::oBrw:navigate := {|mp1,mp2| ::execEvent( "browse_navigate", mp1, mp2 ) }

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:execEvent( cEvent, p, p1 )

   HB_SYMBOL_UNUSED( p  )
   HB_SYMBOL_UNUSED( p1 )

   SWITCH cEvent
   CASE "browse_navigate"
      ::populateForm()
      EXIT

   ENDSWITCH

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:activated()

HB_TRACE( HB_TR_ALWAYS, "ACTIVATED" )
   ::oQScintillaDock:oWidget:setWindowTitle( ::cTable )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:buildBrowser()
   LOCAL qLayout, oWnd, oXbpBrowse

   oWnd := XbpWindow():new()
   oWnd:oWidget := QWidget():new()

   qLayout := QHBoxLayout():new()
   oWnd:oWidget:setLayout( qLayout )
   qLayout:setContentsMargins( 0,0,0,0 )
   qLayout:setSpacing( 2 )

   ::qSplitter := QSplitter():new()
   ::qSplitter:setOrientation( Qt_Horizontal )

   qLayout:addWidget( ::qSplitter )

   /* Browse View */
   oXbpBrowse := XbpBrowse():new():create( oWnd, , { 0,0 }, oWnd:currentSize() )
   oXbpBrowse:setFontCompoundName( "10.Courier" )

   ::qSplitter:addWidget( oXbpBrowse:oWidget )

   //qLayout:addWidget( oXbpBrowse:oWidget )

   oXbpBrowse:cursorMode    := ::nCursorType

   oXbpBrowse:skipBlock     := {|n| ::skipBlock( n ) }
   oXbpBrowse:goTopBlock    := {| | ::goTop()        }
   oXbpBrowse:goBottomBlock := {| | ::goBottom()     }
   //
   oXbpBrowse:firstPosBlock := {| | 1                }
   oXbpBrowse:lastPosBlock  := {| | ::lastRec()      }

   oXbpBrowse:posBlock      := {| | ::recNo()        }
   oXbpBrowse:goPosBlock    := {|n| ::goto( n )      }
   oXbpBrowse:phyPosBlock   := {| | ::recNo()        }

   oXbpBrowse:setInputFocus := {|| ::activated()     }

   /* Form View */
   ::qForm := QWidget():new()
   ::qFLayout := QFormLayout():new()
   ::qForm:setLayout( ::qFLayout )

   ::qSplitter:addWidget( ::qForm )

   ::qLayout := qLayout
   ::oWnd    := oWnd
   ::oBrw    := oXbpBrowse

   RETURN Self

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_xtosForForm( xVrb )
   LOCAL cType := valtype( xVrb )

   DO CASE
   CASE cType == "N" ; RETURN ltrim( str( xVrb ) )
   CASE cType == "L" ; RETURN iif( xVrb, "YES", "NO" )
   CASE cType == "D" ; RETURN dtos( xVrb )
   CASE cType == "C" ; RETURN trim( xVrb )
   ENDCASE

   RETURN ""

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:populateForm()
   LOCAL a_, oCol

   IF ::nType == BRW_TYPE_DBF
      FOR EACH a_ IN ::aForm
         oCol := ::oBrw:getColumn( a_:__enumIndex() )
         ::aForm[ a_:__enumIndex(), 2 ]:setText( hbide_xtosForForm( eval( oCol:block ) ) )
      NEXT
   ELSE

   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:buildForm()
   LOCAL a_, qLbl, qEdit

   IF ::nType == BRW_TYPE_DBF
      FOR EACH a_ IN ::aStruct
         qLbl := QLabel():new()
         qLbl:setText( a_[ 1 ] )
         qEdit := QLineEdit():new()
         ::qFLayout:addRow( qLbl, qEdit )
         aadd( ::aForm, { qLbl, qEdit } )
      NEXT
   ELSE

   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:dataLink( nField )
   LOCAL bBlock

   IF ::nType == BRW_TYPE_DBF
      bBlock := {|| ( ::cAlias )->( fieldget( nField ) ) }
   ELSE
      bBlock := {|| ::aData[ ::nIndex, nField ] }
   ENDIF

   RETURN bBlock

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:buildColumns()
   LOCAL oXbpColumn, aPresParam, a_

   IF ::nType == BRW_TYPE_DBF
      FOR EACH a_ IN ::aStruct
         aPresParam := ::getPP( a_ )

         oXbpColumn          := XbpColumn():new()
         oXbpColumn:dataLink := ::dataLink( a_:__enumIndex() )
         oXbpColumn:create( , , , , aPresParam )

         ::oBrw:addColumn( oXbpColumn )
      NEXT
   ELSE
      FOR EACH a_ IN ::aStruct
         ::getPP( a_, a_:__enumIndex() )

         oXbpColumn          := XbpColumn():new()
         oXbpColumn:dataLink := ::dataLink( a_:__enumIndex() )
         oXbpColumn:create( , , , , aPresParam )

         ::oBrw:addColumn( oXbpColumn )
      NEXT
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:getPP( aStruct )
   LOCAL aPresParam := {}

   aadd( aPresParam, { XBP_PP_COL_HA_CAPTION      , aStruct[ 1 ]  } )
   aadd( aPresParam, { XBP_PP_COL_DA_ROWHEIGHT    , 20            } )

   RETURN aPresParam

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:destroy()
   IF !empty( ::oWnd )
      ::oWnd:destroy()
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:skipBlock( nHowMany )
   LOCAL nRecs, nCurPos
   LOCAL nSkipped := 0

   IF ::nType == BRW_TYPE_DBF
      IF nHowMany == 0
         ( ::cAlias )->( DBSkip( 0 ) )

      ELSEIF nHowMany > 0
         DO WHILE nSkipped != nHowMany .AND. ::next()
            nSkipped++
         ENDDO
      ELSE
         DO WHILE nSkipped != nHowMany .AND. ::previous()
            nSkipped--
         ENDDO
      ENDIF

   ELSE
      nRecs    := len( ::aData )
      nCurPos  := ::nIndex

      IF nHowMany >= 0
         IF ( nCurpos + nHowMany ) > nRecs
            nSkipped := nRecs - nCurpos
            ::nIndex := nRecs
         ELSE
            nSkipped := nHowMany
            ::nIndex += nHowMany
         ENDIF

      ELSE
         IF ( nCurpos + nHowMany ) < 1
            nSkipped := 1 - nCurpos
            ::nIndex := 1
         ELSE
            nSkipped := nHowMany
            ::nIndex += nHowMany
         ENDIF

      ENDIF

   ENDIF

   RETURN nSkipped

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:goTop()

   IF ::nType == BRW_TYPE_DBF
      ( ::cAlias )->( DbGotop() )
   ELSE
      ::nIndex := 1
   ENDIF
   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:goBottom()

   IF ::nType == BRW_TYPE_DBF
      ( ::cAlias )->( DbGoBottom() )
   ELSE
      ::nIndex := len( ::aData )
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:recNo()

   IF ::nType == BRW_TYPE_DBF
      RETURN ( ::cAlias )->( RecNo() )
   ELSE
      RETURN ::nIndex
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:lastRec()

   IF ::nType == BRW_TYPE_DBF
      RETURN ( ::cAlias )->( LastRec() )
   ELSE
      RETURN len( ::aData )
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:goTo( nRec )

   IF ::nType == BRW_TYPE_DBF
      ( ::cAlias )->( DbGoto( nRec ) )
   ELSE
      ::nIndex := nRec
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:next()
   LOCAL nSaveRecNum := recno()
   LOCAL lMoved := .T.

   IF ( ::cAlias )->( Eof() )
      lMoved := .F.
   ELSE
      ( ::cAlias )->( DBSkip( 1 ) )
      IF Eof()
         lMoved := .F.
         ( ::cAlias )->( DBGoTo( nSaveRecNum ) )
      ENDIF
   ENDIF

   RETURN lMoved

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:previous()
   LOCAL nSaveRecNum := Recno()
   LOCAL lMoved := .T.

   ( ::cAlias )->( DBSkip( -1 ) )

   IF Bof()
      ( ::cAlias )->( DBGoTo( nSaveRecNum ) )
      lMoved := .F.
   ENDIF

   RETURN lMoved

/*----------------------------------------------------------------------*/
