/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2010 Pritpal Bedi <pritpal@vouchcac.com>
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
 *                               06Mar2010
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbide.ch"
#include "common.ch"
#include "hbclass.ch"
#include "xbp.ch"
#include "hbqtgui.ch"

/*----------------------------------------------------------------------*/

#define FLIST_NAME                                1
#define FLIST_TYPE                                2
#define FLIST_LINE                                3
#define FLIST_SOURCE                              4
#define FLIST_NAME_1                              5
#define FLIST_TYPE_1                              6
#define FLIST_SYNTAX                              7

/*----------------------------------------------------------------------*/

CLASS IdeFunctions INHERIT IdeObject

   DATA   isNotSetYet                             INIT .t.
   DATA   aHdr                                    INIT {}
   DATA   aItems                                  INIT {}
   DATA   aTags                                   INIT { { "", {} } }
   DATA   aList                                   INIT {}
   DATA   inAction                                INIT .f.
   DATA   nPNm                                    INIT 25
   DATA   nPPr                                    INIT 15
   DATA   nPSr                                    INIT 50
   DATA   aProjList                               INIT {}

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()
   METHOD clear( lHdrAlso )
   METHOD show()
   METHOD tagProject( cProjectTitle )
   METHOD populateTable()
   METHOD consolidateList()
   METHOD buildHeader()
   METHOD execEvent( nMode, p )
   METHOD openFunction( lCheckDuplicates )
   METHOD jumpToFunction( cWord )
   METHOD positionToFunction( cWord, lShowTip )
   METHOD buildTags()
   METHOD loadTags( aProjects )
   METHOD listProjects()
   METHOD clearProjects()
   METHOD getMarkedProjects()
   METHOD enableControls( lEnable )
   METHOD getFunctionPrototypes()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeFunctions:new( oIde )
   ::oIde := oIde
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFunctions:create( oIde )

   DEFAULT oIde TO ::oIde
   ::oIde := oIde

   ::oUI := hbide_getUI( "funclist" )

   ::buildHeader()

   ::oUI:q_editFunction :connect( "textChanged(QString)"        , {|p| ::execEvent( "editFunc_textChanged", p   ) } )
   ::oUI:q_editFunction :connect( "returnPressed()"             , {| | ::execEvent( "editFunc_returnPressed"    ) } )
   ::oUI:q_buttonMark   :connect( "clicked()"                   , {| | ::execEvent( "buttonMark_clicked"        ) } )
   ::oUI:q_buttonLoad   :connect( "clicked()"                   , {| | ::execEvent( "buttonLoad_clicked"        ) } )
   ::oUI:q_buttonTag    :connect( "clicked()"                   , {| | ::execEvent( "buttonTag_clicked"         ) } )
   ::oUI:q_buttonClose  :connect( "clicked()"                   , {| | ::execEvent( "buttonClose_clicked"       ) } )
   ::oUI:q_tableFuncList:connect( "itemSelectionChanged()"      , {| | ::execEvent( "tableFuncList_itemSelectionChanged" ) } )
   ::oUI:q_tableFuncList:connect( "itemDoubleClicked(QTableWidgetItem*)", {|p| ::execEvent( "tableFuncList_itemDoubleClicked", p ) } )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFunctions:execEvent( nMode, p )
   LOCAL n, nLen

   IF ::lQuitting
      RETURN Self
   ENDIF

   DO CASE
   CASE nMode == "editFunc_textChanged"
      p    := upper( p )
      nLen := Len( p )
      IF ( n := ascan( ::aList, {|e_| left( e_[ 1 ], nLen ) == p } ) ) > 0
         ::oUI:q_tableFuncList:setCurrentItem( ::aItems[ n ] )
      ENDIF

   CASE nMode == "editFunc_returnPressed"
      ::openFunction( .f. )

   CASE nMode == "tableFuncList_itemDoubleClicked"
      ::openFunction( .f. )

   CASE nMode == "buttonMark_clicked"
      ::oUI:q_listProjects:show()
      ::listProjects()

   CASE nMode == "buttonLoad_clicked"
      ::oUI:q_listProjects:hide()
      ::loadTags()

   CASE nMode == "buttonTag_clicked"
      ::oUI:q_listProjects:hide()
      ::buildTags()
      ::oEM:updateCompleter()

   CASE nMode == "buttonClose_clicked"
      ::oFunctionsDock:hide()

   CASE nMode == "tableFuncList_itemSelectionChanged"
      n := ::oUI:q_tableFuncList:currentRow()
      IF n >= 0
         ::oUI:q_editSyntax:setText( ::aList[ n + 1, 2 ] )
      ENDIF
   ENDCASE

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFunctions:buildHeader()
   LOCAL oTbl, qItm, cHdr, qFnt, qHdr
   LOCAL cDH := " "

   oTbl := ::oUI:q_tableFuncList

   qFnt := QFont( "Courier New" )
   oTbl:setFont( qFnt )

   oTbl:verticalHeader():hide()

   qHdr := oTbl:horizontalHeader()
   qHdr:setStretchLastSection( .t. )

   oTbl:setColumnCount( 1 )

   cHdr := pad( "Name", ::nPNm ) + cDH + "Typ " + cDH + "  Line" + cDH + ;
                            pad( "Project", ::nPPr ) + cDH + pad( "Source", ::nPSr )

   qItm := QTableWidgetItem()
   qItm:setText( cHdr )
   qItm:setFont( qFnt )
   qItm:setTextAlignment( Qt_AlignLeft )
   aadd( ::aHdr, qItm )
   oTbl:setHorizontalHeaderItem( 0, qItm )
   oTbl:setColumnWidth( 0, 800 )

   oTbl:setShowGrid( .f. )

   oTbl:setAlternatingRowColors( .t. )

   ::oUI:q_listProjects:hide()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFunctions:destroy()
   LOCAL qitm

   IF !empty( ::oUI )
      ::clearProjects()

      FOR EACH qItm IN ::aHdr
         qItm := NIL
      NEXT
      ::aHdr := {}

      ::clear( .t. )

      ::oUI:destroy()
   ENDIF

   ::isNotSetYet  := NIL
   ::aHdr         := NIL
   ::aItems       := NIL
   ::aTags        := NIL
   ::aList        := NIL
   ::inAction     := NIL
   ::nPNm         := NIL
   ::nPPr         := NIL
   ::nPSr         := NIL
   ::aProjList    := NIL

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFunctions:clear( lHdrAlso )
   LOCAL qItm

   IF lHdrAlso
      FOR EACH qItm IN ::aHdr
         qItm := NIL
      NEXT
      ::aHdr := {}
   ENDIF

   FOR EACH qItm IN ::aItems
      qItm := NIL
   NEXT
   ::aItems := {}

   IF lHdrAlso
      ::oUI:q_tableFuncList:clear()
   ELSE
      ::oUI:q_tableFuncList:clearContents()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFunctions:show()

   IF ::isNotSetYet
      ::isNotSetYet := .f.

      ::oFunctionsDock:oWidget:setWidget( ::oUI:oWidget )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFunctions:positionToFunction( cWord, lShowTip )
   LOCAL nLen, p, n, cProto := ""

   IF !empty( ::aList )
      p    := upper( cWord )
      nLen := Len( p )
      IF ( n := ascan( ::aList, {|e_| left( e_[ 1 ], nLen ) == p } ) ) > 0
         ::oUI:q_editFunction:setText( cWord )
         ::oUI:q_tableFuncList:setCurrentItem( ::aItems[ n ] )

         cProto := ::aList[ n, 2 ]

         IF lShowTip
            // TODO: where

         ENDIF
      ENDIF
   ENDIF

   RETURN cProto

/*----------------------------------------------------------------------*/

METHOD IdeFunctions:jumpToFunction( cWord )
   LOCAL nLen, lOpened := .f., p, n

   IF !empty( ::aList )
      p    := upper( cWord )
      nLen := Len( p )
      IF ( n := ascan( ::aList, {|e_| left( e_[ 1 ], nLen ) == p } ) ) > 0
         ::oUI:q_editFunction:setText( cWord )
         ::oUI:q_tableFuncList:setCurrentItem( ::aItems[ n ] )
         lOpened := ::openFunction( .t. )
      ENDIF
   ENDIF

   RETURN lOpened

/*----------------------------------------------------------------------*/

METHOD IdeFunctions:openFunction( lCheckDuplicates )
   LOCAL n, cFunc, cSource, oEdit, lFound, cProto
   LOCAL lOpened := .f.

   IF ( n := ::oUI:q_tableFuncList:currentRow() ) >= 0
      n++
      cFunc := ::aList[ n, 1 ]
      IF lCheckDuplicates .AND. n < Len( ::aList ) .AND. ::aList[ n + 1, 1 ] == cFunc
         ::oFunctionsDock:show()
         ::oUI:q_tableFuncList:setFocus()
         RETURN lOpened
      ENDIF

      cProto  := ::aList[ n, 2 ]
      cSource := alltrim( substr( ::aList[ n, 3 ], 53 ) )
      ::oSM:editSource( cSource, , , , , , .f. )

      IF !empty( oEdit := ::oEM:getEditCurrent() )
         IF !( lFound := oEdit:find( cProto, QTextDocument_FindCaseSensitively ) )
            lFound := oEdit:find( cProto, QTextDocument_FindBackward + QTextDocument_FindCaseSensitively )
         ENDIF
         IF lFound
            oEdit:centerCursor()
            lOpened := .t.
         ELSE
            HB_TRACE( HB_TR_DEBUG, "IdeFunctions:openFunction()", "It should not happen." )
         ENDIF
      ENDIF
   ENDIF

   RETURN lOpened

/*----------------------------------------------------------------------*/

METHOD IdeFunctions:clearProjects()
   LOCAL qItm

   IF !empty( ::aProjList )
      FOR EACH qItm IN ::aProjList
         qItm := NIL
      NEXT
   ENDIF
   ::aProjList := {}
   ::oUI:q_listProjects:clear()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFunctions:listProjects()
   LOCAL s, qItm, oLst := ::oUI:q_listProjects

   ::clearProjects()

   FOR EACH s IN ::oPM:getProjectsTitleList()
      qItm := QListWidgetItem()
      qItm:setText( s )
      qItm:setCheckState( Qt_Unchecked )
      //oLst:addItem_1( qItm )
      oLst:addItem( qItm )
      aadd( ::aProjList, qItm )
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFunctions:getMarkedProjects()
   LOCAL qItm, a_:= {}

   FOR EACH qItm IN ::aProjList
      IF qItm:checkState() == 2
         aadd( a_, qItm:text() )
      ENDIF
   NEXT

   RETURN a_

/*----------------------------------------------------------------------*/

METHOD IdeFunctions:enableControls( lEnable )

   ::inAction := ! lEnable

   ::oUI:q_buttonMark:setEnabled( lEnable )
   ::oUI:q_buttonLoad:setEnabled( lEnable )
   ::oUI:q_buttonTag:setEnabled( lEnable )

   ::oUI:q_editFunction:setEnabled( lEnable )

   ::showApplicationCursor( iif( lEnable, NIL, Qt_BusyCursor ) )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFunctions:loadTags( aProjects )
   LOCAL cProjectTitle, cProjFile, cTagFile, aTags, n, a_
   LOCAL lPopulate := .f.
   LOCAL qApp := QApplication()

   DEFAULT aProjects TO ::getMarkedProjects()

   IF empty( aProjects )
      RETURN Self
   ENDIF
   a_:= aProjects

   IF !( ::inAction )
      ::enableControls( .f. )

      FOR EACH cProjectTitle IN a_
         cProjFile := ::oPM:getProjectFileNameFromTitle( cProjectTitle )
         IF ! empty( cProjFile ) .AND. hb_fileExists( cProjFile )
            cTagFile := hb_FNameExtSet( cProjFile, ".tag" )
            IF hb_fileExists( cTagFile )
               lPopulate := .t.

               aTags := hb_deserialize( hb_memoRead( cTagFile ) )

               IF ( n := ascan( ::aTags, {|e_| e_[ 1 ] == cProjectTitle } ) ) == 0
                  aadd( ::aTags, { cProjectTitle, aTags } )
               ELSE
                  ::aTags[ n, 2 ] := aTags
               ENDIF
            ENDIF
         ENDIF

         qApp:processEvents()
         IF ::lQuitting
            EXIT
         ENDIF
      NEXT

      IF lPopulate
         ::consolidateList()
         ::populateTable()
      ENDIF

      ::enableControls( .t. )
   ENDIF

   ::clearProjects()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFunctions:buildTags()
   LOCAL cProjectTitle
   LOCAL a_:= ::getMarkedProjects()

   IF !empty( a_ )
      FOR EACH cProjectTitle IN a_
         ::tagProject( cProjectTitle )
      NEXT
      ::oIde:oINI:aTaggedProjects := a_
      ::clearProjects()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFunctions:tagProject( cProjectTitle )
   LOCAL aSumData := ""
   LOCAL cComments, aSummary, cPath, cSource, cExt, aTags, aText, aFuncList, aLines
   LOCAL cProjFile, cRoot, aCTags, aSources, cSrc, a_, n
   LOCAL qApp := QApplication()

   IF !( ::inAction )
      ::enableControls( .f. )

      cProjFile := ::oPM:getProjectFileNameFromTitle( cProjectTitle )
      aSources  := ::oPM:getSourcesByProjectTitle( cProjectTitle )
      cRoot     := ::oPM:getProjectPathFromTitle( cProjectTitle )

      FOR EACH cSource IN aSources
         aSources[ cSource:__enumIndex() ] := hbide_syncProjPath( cRoot, cSource )
      NEXT

      aCTags := {}

      FOR EACH cSrc IN aSources
         aFuncList := {}
         aLines    := {}

         HB_FNameSplit( cSrc, @cPath, @cSource, @cExt )

         IF upper( cExt ) $ ".PRG.CPP"
            IF !empty( aText := hbide_readSource( cSrc ) )
               aSumData  := {}
               cComments := CheckComments( aText )
               aSummary  := Summarize( aText, cComments, @aSumData , iif( upper( cExt ) == ".PRG", 9, 1 ) )
               aTags     := UpdateTags( cSrc, aSummary, aSumData, @aFuncList, @aLines, aText )

               IF !empty( aTags )
                  aeval( aTags, {|e_| aadd( aCTags, { e_[1],e_[2],e_[3],e_[4],e_[7] } ) } )
               ENDIF
            ENDIF
         ENDIF

         qApp:processEvents()
         IF ::lQuitting
            EXIT
         ENDIF
      NEXT

      FOR EACH a_ IN aCTags
         a_[ 5 ] := iif( left( a_[ 5 ], 1 ) == ":", substr( a_[ 5 ], 2 ), a_[ 5 ] )
      NEXT

      IF ( n := ascan( ::aTags, {|e_| e_[ 1 ] == cProjectTitle } ) ) == 0
         aadd( ::aTags, { cProjectTitle, aCTags } )
      ELSE
         ::aTags[ n, 2 ] := aCTags
      ENDIF

      hb_memowrit( hb_FNameExtSet( cProjFile, ".tag" ), hb_serialize( aCTags ) )

      ::consolidateList()
      ::populateTable()

      ::enableControls( .t. )
   ENDIF

   RETURN cProjFile

//----------------------------------------------------------------------//

METHOD IdeFunctions:consolidateList()
   LOCAL s, a_, b_, cProjectTitle
   LOCAL cDL := " "

   ::aList := {}

   FOR EACH b_ IN ::aTags
      IF !empty( cProjectTitle := b_[ 1 ] )
         FOR EACH a_ IN b_[ 2 ]
            s := pad( a_[ 1 ], ::nPNm ) + ;
                 cDL + ;
                 hbide_abbrFuncType( a_[ 2 ] ) + ;
                 cDL + ;
                 padl( ltrim( str( a_[ 3 ] ) ), 6 ) + ;
                 cDL + ;
                 pad( cProjectTitle, ::nPPr ) + ;
                 cDL + ;
                 pad( a_[ 4 ], ::nPSr )

            aadd( ::aList, { a_[ 1 ], a_[ 5 ], s } )
         NEXT
      ENDIF
   NEXT

   IF !empty( ::aList )
      asort( ::aList, , , {|e_,f_| e_[ 1 ] < f_[ 1 ] } )
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFunctions:populateTable()
   LOCAL oTbl, qItm, a_, n
   LOCAL qApp := QApplication()

   ::clear( .t. )
   ::buildHeader()

   oTbl := ::oUI:q_tableFuncList
   oTbl:setRowCount( Len( ::aList ) )

   n := 0
   FOR EACH a_ IN ::aList
      qItm := QTableWidgetItem()

      qItm:setText( a_[ 3 ] )
      qItm:setTooltip( a_[ 2 ] )
      oTbl:setItem( n, 0, qItm )
      oTbl:setRowHeight( n, 16 )

      qApp:processEvents()
      IF ::lQuitting
         EXIT
      ENDIF

      aadd( ::aItems, qItm )
      n++
      ::oUI:q_labelEntries:setText( "Entries: " + hb_ntos( n ) )
   NEXT

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeFunctions:getFunctionPrototypes()
   LOCAL aProto := {}, a_

   FOR EACH a_ IN ::aList
      aadd( aProto, alltrim( a_[ 2 ] ) )
   NEXT

   RETURN aProto

/*----------------------------------------------------------------------*/

STATIC FUNCTION hbide_abbrFuncType( cFunc )
   LOCAL cAbbr := ""

   IF "STATIC" $ cFunc
      cAbbr += "S"
   ENDIF
   IF "FUNC" $ cFunc
      cAbbr += "F"
   ENDIF
   IF "PROC" $ cFunc
      cAbbr += "P"
   ENDIF
   IF "HB_" $ cFunc
      cAbbr += ":C"
   ENDIF
   IF "CLASS" $ cFunc
      cAbbr += "C"
   ENDIF
   IF "METHOD" $ cFunc
      cAbbr += "M"
      IF ":" $ cFunc
         cAbbr += ":D"
      ENDIF
   ENDIF

   RETURN padc( cAbbr, 3 )

/*----------------------------------------------------------------------*/
