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

STATIC FUNCTION hbide_getNewAlias( cTable )
   LOCAL cFile

   STATIC n := 0
   n++
   hb_fNameSplit( cTable, , @cFile )

   RETURN cFile + "_" + hb_ntos( n )

/*----------------------------------------------------------------------*/

FUNCTION hbide_browseSome( oIde )
   LOCAL cTable, cPath

   IF !empty( cTable := hbide_fetchAFile( oIde:oDlg, "Select a Table", { { "Database File", "*.dbf" } }, oIde:cWrkFolderLast ) )
      hb_fNameSplit( cTable, @cPath )
      oIde:cWrkFolderLast := cPath
      oIde:oBM:addTable( cTable )
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

CLASS IdeBrowseManager INHERIT IdeObject

   DATA   aItems                                  INIT  {}
   DATA   oPanel
   DATA   qLayout

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()
   METHOD addTable( cFileDBF, cAlias, nRow, nCol )
   METHOD addArray( aData, aAttr, nRow, nCol )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeBrowseManager:new( oIde )

   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowseManager:create( oIde )

   DEFAULT oIde TO ::oIde
   ::oIde := oIde

   ::oPanel := QWidget():new()

   ::oIde:oEM:oQScintillaDock:oWidget:setWidget( ::oPanel )

   ::qLayout := QVBoxLayout():new()
   ::oPanel:setLayout( ::qLayout )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowseManager:destroy()
   LOCAL oBrw

   // Close tables and clear variables
   FOR EACH oBrw IN ::aItems
      Select( oBrw:cAlias )
      DbCloseArea()
   NEXT
   ::aItems := {}

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowseManager:addArray( aData, aAttr, nRow, nCol )

   HB_SYMBOL_UNUSED( aData )
   HB_SYMBOL_UNUSED( aAttr )
   HB_SYMBOL_UNUSED( nRow  )
   HB_SYMBOL_UNUSED( nCol  )

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

   ::qLayout:addWidget( oBrw:oWnd:oWidget )

   aadd( ::aItems, oBrw )

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

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:buildBrowser()
   LOCAL qLayout, oWnd, oXbpBrowse

   oWnd := XbpWindow():new()
   oWnd:oWidget := QWidget():new()
   //oWnd:oWidget:resize( 600,300 )
   //oWnd:oWidget:setWindowFlags( Qt_Sheet )

   qLayout := QVBoxLayout():new()
   oWnd:oWidget:setLayout( qLayout )

   oXbpBrowse := XbpBrowse():new():create( oWnd, , { 0,0 }, oWnd:currentSize() )
   oXbpBrowse:setFontCompoundName( "10.Courier" )

   qLayout:addWidget( oXbpBrowse:oWidget )

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

   ::qLayout := qLayout
   ::oWnd    := oWnd
   ::oBrw    := oXbpBrowse

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:dataLink( nField )
   LOCAL bBlock

   IF ::nType == BRW_TYPE_DBF
      bBlock := {|| (::cAlias)->( fieldget( nField ) ) }
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
         DBSkip( 0 )

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
      DbGotop()
   ELSE
      ::nIndex := 1
   ENDIF
   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:goBottom()

   IF ::nType == BRW_TYPE_DBF
      DbGoBottom()
   ELSE
      ::nIndex := len( ::aData )
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:recNo()

   IF ::nType == BRW_TYPE_DBF
      RETURN RecNo()
   ELSE
      RETURN ::nIndex
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:lastRec()

   IF ::nType == BRW_TYPE_DBF
      RETURN LastRec()
   ELSE
      RETURN len( ::aData )
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:goTo( nRec )

   IF ::nType == BRW_TYPE_DBF
      DbGoto( nRec )
   ELSE
      ::nIndex := nRec
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:next()
   LOCAL nSaveRecNum := recno()
   LOCAL lMoved := .T.

   IF Eof()
      lMoved := .F.
   ELSE
      DBSkip( 1 )
      IF Eof()
         lMoved := .F.
         DBGoTo( nSaveRecNum )
      ENDIF
   ENDIF

   RETURN lMoved

/*----------------------------------------------------------------------*/

METHOD IdeBrowse:previous()
   LOCAL nSaveRecNum := Recno()
   LOCAL lMoved := .T.

   DBSkip( -1 )

   IF Bof()
      DBGoTo( nSaveRecNum )
      lMoved := .F.
   ENDIF

   RETURN lMoved

/*----------------------------------------------------------------------*/
