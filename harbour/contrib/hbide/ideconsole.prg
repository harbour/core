/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2011 Pritpal Bedi <bedipritpal@hotmail.com>
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
 *                               25Aug2011
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "common.ch"
#include "hbclass.ch"
#include "hbqtgui.ch"
#include "hbide.ch"
#include "xbp.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"
#include "achoice.ch"
#include "box.ch"

/*----------------------------------------------------------------------*/

THREAD STATIC s_vid_stk := ""

/*----------------------------------------------------------------------*/

#define THE_FILL                                  chr( 177 )
#define CHR_PIPE                                  chr( 124 )
#define B_SLIM                                    chr( 219 ) + chr( 223 ) + chr( 219 ) + chr( 219 ) + chr( 219 ) + chr( 220 ) + chr( 219 ) + chr( 219 )

#define OBJ_TYPE                                  1     //   N   1

#define OBJ_ROW                                   2     //   N   3
#define OBJ_COL                                   3     //   N   3
#define OBJ_TO_ROW                                4     //   N   3
#define OBJ_TO_COL                                5     //   N   3

#define OBJ_TEXT                                  6     //   C  15
#define OBJ_EQN                                   6     //   C 240
#define OBJ_NAME                                  6     //   C 240

#define OBJ_F_TYPE                                7     //   C   1
#define OBJ_F_LEN                                 8     //   N   3
#define OBJ_F_DEC                                 9     //   N   2
#define OBJ_F_PIC                                 10    //   C  20
#define OBJ_COLOR                                 11    //   C   7

#define OBJ_WHEN                                  12    //   C   7
#define OBJ_BORDER                                12    //   C   7
#define OBJ_BOX_SHAPE                             12    //   C  --

#define OBJ_VALID                                 13    //   C   7
#define OBJ_PATTERN                               13    //   C  10

#define OBJ_ID                                    14    //   C  15
#define OBJ_SEC_ROW                               15    //   N   3
#define OBJ_OBJ_UNIQUE                            16    //   N   4
#define OBJ_MDL_F_TYPE                            17    //   C  10
//
#define OBJ_INIT_VRBLS                            17


#define OBJ_REFRESH_ALL                           1
#define OBJ_REFRESH_LINE                          2
#define OBJ_REFRESH_NIL                           0

#define OBJ_O_BOX                                 1
#define OBJ_O_LINE                                2
#define OBJ_O_TEXT                                3
#define OBJ_O_FIELD                               4
#define OBJ_O_EXP                                 5
#define OBJ_O_BMP                                 6

#define OBJ_MODE_SELECT                           1
#define OBJ_MODE_MOVE                             2
#define OBJ_MODE_IDLE                             0

#translate B_MSG ;
      [ <msg,...>               ] ;
      [ AT <r1> [, <c1> ]       ] ;
      [ TO <r2> [, <c2> ]       ] ;
      [ WIDTH <w>               ] ;
      [ DEPTH <d>               ] ;
      [ COLOR <clr>             ] ;
      [ CHOOSE <ch,...>         ] ;
      [ CHOOSECOLOR <chClr>     ] ;
      [ CHCOLOR <chClr>         ] ;
      [ INTO <ret>              ] ;
      [ WAIT <wait>             ] ;
      [ <rest:RESTORE,REST>     ] ;
      [ <paste:PASTE>           ] ;
      [ <shadow:SHADOW>         ] ;
      [ TRIGGER <trg>           ] ;
      [ INITIAL <init>          ] ;
      [ SELECTABLES <sel>       ] ;
      [ ABORT <abr>             ] ;
      [ <selections:SELECTIONS> ] ;
      [ <leftright:LEFTRIGHT>   ] ;
      [ <cent:CENTER,CENTRE>    ] ;
      [ TAGGED <tag_>           ] ;
      [ <num:NUMERIC>           ] ;
      [ HELP <hlp>              ] ;
      [ EXECUTE <ex_>           ] ;
      [ NUMBERED <num_>         ] ;
      [ <lNoXpp:NOXPP>          ] ;
      [ WINDOW <oWin>           ] ;
      [ ICON <cIcon>            ] ;
      [ WVT <lWvt>              ] ;
      [ ALIGN <nAlign>          ] ;
   => ;
      [<ret> := ] VouchMsgBox (<r1>, <c1>, <r2>, <c2>, <w>, <d>, ;
         {<msg>}, <clr>, {<ch>}, <chClr>, <wait>,  <.rest.>, ;
         <.paste.>, <.shadow.>, <trg>, <init>, <sel>, <abr>, ;
         <.selections.>, <.leftright.>, <.cent.>, <tag_>,<.num.>,;
         <hlp>,<ex_>,<num_>,<.lNoXpp.>,<oWin>,<cIcon>,<lWvt>,<nAlign> )


#xtranslate B_GETS ;
            HEADERS <hed> VALUES <val> ;
            [ SELECTABLES <sel> ] ;
            [ AT <r1> [, <c1> ] ] ;
            [ TO <r2> [, <c2> ] ] ;
            [ TITLE   <ttl>     ] ;
            [ INTO    <ret>     ] ;
            [ WHEN    <whn>     ] ;
            [ VALID   <vld>     ] ;
            [ PICTURE <pic>     ] ;
            [ HELP    <hlp>     ] ;
            [ ORDER   <ord>     ] ;
            => ;
   [<ret> := ] VouchGetArray(<hed>, <val>, <sel>, <r1>, <c1>, <r2>, <c2>, ;
                        <ttl>, <whn>, <vld>, <pic>, <hlp>, <ord> )


#define COMPILE( cStr )    &( "{|v,w,x| " + cStr + " }" )

#define CHECKMARK                                 chr( 251 )

/*----------------------------------------------------------------------*/

CLASS IdeConsole INHERIT IdeObject

   DATA   oXbpDock
   DATA   oMDI
   DATA   oMDIArea
   DATA   nOffX
   DATA   nOffY

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD show()
   METHOD resizeByRowCols( nRows, nCols )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeConsole:new( oIde )

   DEFAULT oIde    TO ::oIde

   ::oIde    := oIde


   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeConsole:create( oIde )

   DEFAULT oIde TO ::oIde

   ::oIde := oIde

   RETURN self

/*----------------------------------------------------------------------*/

METHOD IdeConsole:show()
   LOCAL qRect, qRect1

   IF empty( ::oUI )
      ::oMdiArea := QMdiArea():new()
      ::oCuiEdDock:oWidget:setWidget( ::oMdiArea )

      ::oUI := XbpCrt():new( , , { 10,10 }, { 600,480 }, , .t. )
      ::oUI:title       := "My First CRT"
      ::oUI:toolTiptext := "Really My First XbpCRT()"
      ::oUI:lModal      := .f.
      ::oUI:create()
      ::oUI:show()

      hb_gtInfo( HB_GTI_RESIZEMODE, HB_GTI_RESIZEMODE_ROWS )
      SetColor( "N/W" )
      ? " "
      hb_gtInfo( HB_GTI_FONTSIZE, 12 )
      SetMode( 28,80 )
      hb_setDispCP( "EN" )

      qRect   := ::oUI:oWidget:frameGeometry()
      qRect1  := ::oUI:oWidget:geometry()
      ::nOffX := qRect:width()  - qRect1:width()
      ::nOffY := qRect:height() - qRect1:height()

      ::oMdi  := ::oMdiArea:addSubWindow( ::oUI:oWidget )

      ::oMdi:show()
      ::resizeByRowCols( 28, 80 )

      // ::oCuiEdDock:oWidget:setMinimumWidth( 80 * hb_gtInfo( HB_GTI_FONTWIDTH ) + ::nOffX )

      BuildScreen()

      ::oMdiArea:removeSubWindow( ::oUI:oWidget /*::oMDI*/ )
      ::oMDI := NIL
      ::oUI:destroy()

      ::oUI := NIL

      ::oCuiEdDock:oWidget:hide()
   ENDIF

   RETURN Self

/*------------------------------------------------------------------------*/

METHOD IdeConsole:resizeByRowCols( nRows, nCols )

   RETURN ::oMdi:resize( nCols * hb_gtInfo( HB_GTI_FONTWIDTH ) + ::nOffX, nRows * hb_gtInfo( HB_GTI_FONTSIZE ) + ::nOffY )

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#define N_TRIM( n )                               ltrim( str( n, 10, 0 ) )

/*----------------------------------------------------------------------*/

CLASS hbCUIEditor

   DATA cSource                                   INIT ""
   DATA cScreen                                   INIT ""
   DATA lEdited                                   INIT .f.

   DATA obj_                                      INIT {}
   DATA scn_
   DATA rpt_                                      INIT { { "", 0, "" } }

   DATA aUndo                                     INIT {}
   DATA aRedo                                     INIT {}

   DATA sectors_                                  INIT {}
   DATA nDesign                                   INIT 1
   DATA nTop                                      INIT 1
   DATA nLeft                                     INIT 0
   DATA nBottom                                   INIT maxrow() - 2
   DATA nRight                                    INIT maxcol()
   DATA nMode                                     INIT 0
   DATA nRowCur                                   INIT 0
   DATA nColCur                                   INIT 0
   DATA nRowRep                                   INIT 1
   DATA nColRep                                   INIT 1
   DATA nRowDis                                   INIT -1
   DATA nColDis                                   INIT -1
   DATA nRowMenu                                  INIT 0
   DATA nRowRuler                                 INIT 0
   DATA nRowStatus                                INIT maxrow() - 1
   DATA nColStatus                                INIT 0
   DATA nColsMax                                  INIT 400
   DATA nRowsMax                                  INIT 200
   DATA nRowPrev                                  INIT 1
   DATA nColPrev                                  INIT 1
   DATA cClrStatus                                INIT "W+/BG"
   DATA cClrText                                  INIT "W+/B"
   DATA cClrHilite                                INIT "GR+/BG"
   DATA cClrWindow                                INIT "W+/BG"
   DATA cClrRuler                                 INIT "N/W"
   DATA cClrOverall                               INIT "N/W"
   DATA cClrPrev                                  INIT "B/W"
   DATA cClrSelect                                INIT "GR+/N"
   DATA nObjHilite                                INIT 0
   DATA nObjSelected                              INIT 0
   DATA cRuler                                    INIT ""
   DATA cDrawFill                                 INIT chr( 176 )
   DATA aObjId                                    INIT { 'Bitmap','Line','Text','Field','Expression','BitMap' }
   DATA xRefresh                                  INIT OBJ_REFRESH_ALL
   DATA nObjCopied                                INIT 0
   DATA aProperty                                 INIT {}
   DATA lGraphics                                 INIT .f.
   DATA aTextBlock                                INIT {}
   DATA aFields                                   INIT {}
   DATA nLastKey                                  INIT 0

   METHOD new( cSource, cScreen )
   METHOD create( cSource, cScreen )
   METHOD destroy()

   METHOD operate()

   METHOD scrDisplay()
   METHOD scrMove()
   METHOD scrMoveLine()
   METHOD scrDispSelected()
   METHOD scrDispGhost( gst_ )
   METHOD scrStatus()

   METHOD scrMouse()
   METHOD scrToMouse( nmRow, nmCol )
   METHOD scrOrdObj()
   METHOD scrMovRgt()
   METHOD scrMovLft()
   METHOD scrMovUp()
   METHOD scrMovDn()
   METHOD scrChkObj()
   METHOD scrUpdObjRC()
   METHOD scrRepCol()
   METHOD scrAddLine()
   METHOD scrDelLine()
   METHOD scrIsBoxIn()
   METHOD scrObjCopy()
   METHOD scrObjPas()
   METHOD scrObjDel( nObj )
   METHOD scrOnLastCol( nObj )
   METHOD scrOnFirstCol( nObj, type_ )
   METHOD scrGetChar( nRow, nCol )
   METHOD scrTextBlock()
   METHOD scrTextMove( nMode )
   METHOD scrTextPost( gst_, nMode )
   METHOD scrTextDel()

   METHOD scrLoad( lAsk )
   METHOD scrSave( lAsk )

   METHOD scrAddBox( nObj )
   METHOD scrAddFld( nObj )
   METHOD scrAddTxt( nMode )
   METHOD scrMsg( msg )
   METHOD scrInKey( key_ )
   METHOD scrConfig()
   METHOD scrReConfig()
   METHOD scrObjBlank()
   METHOD scrVrbBlank( nType )
   METHOD scrObj2Vv( o_ )
   METHOD scrVv2Obj( v_, o_ )

   METHOD objType( nObj )
   METHOD objIsBox( nObj )
   METHOD objIsFld( nObj )
   METHOD objIsTxt( nObj )
   METHOD scrIsTxt()

   METHOD scrVrbHeaders( nType )
   METHOD scrGetProperty( nObj )
   METHOD scrUpdateSource( prg_ )
   METHOD scrBuildSource( prg_, nIndent )
   METHOD scrBuildFunction( prg_ )
   METHOD scrBuildFromBuffer( cBuffer, cScreen )
   METHOD scrOrdGets()
   METHOD scrUpdateUndo()
   METHOD scrUndo()
   METHOD scrPreview()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD hbCUIEditor:new( cSource, cScreen )

   DEFAULT cSource TO ::cSource
   DEFAULT cScreen TO ::cScreen

   ::cSource := cSource
   ::cScreen := cScreen

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD hbCUIEditor:create( cSource, cScreen )

   DEFAULT cSource TO ::cSource
   DEFAULT cScreen TO ::cScreen

   ::cSource := cSource
   ::cScreen := cScreen

   ::scrLoad( .f. )
   ::scrConfig()
   ::operate()

   RETURN SELF

/*----------------------------------------------------------------------*/

METHOD hbCUIEditor:destroy()
   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD hbCUIEditor:scrLoad( lAsk )
   LOCAL cBuffer, n, n1, nSel, aMatches, aMatch, cSource
   LOCAL scr_:={}

   IF ::lEdited
      IF alert( "Screen has been edited, save ?", { "Yes","No" } ) == 1
         ::scrSave( .f. )
      ENDIF
   ENDIF

   IF lAsk
      cSource := alltrim( VouchGetSome( "Source (.PRG)", pad( ::cSource, 50 ) ) )
      IF empty( cSource )
         RETURN NIL
      ENDIF
      IF ! ( cSource == ::cSource )
         ::obj_:={}
         ::cSource := cSource
      ENDIF
   ENDIF
   IF empty( ::cSource )
      RETURN Self
   ENDIF

   IF lAsk .OR. empty( ::cScreen )
      cBuffer := hb_memoread( ::cSource )
      aMatches := hb_regExAll( "HB_SCREEN_BEGINS", cBuffer, .f., .f., 0, 1, .f. )
      IF ! empty( aMatches )
         FOR EACH aMatch IN aMatches
            IF ( n := hb_at( "<", cBuffer, aMatch[ 2 ] ) ) > 0
               IF ( n1 := hb_at( ">", cBuffer, aMatch[ 2 ] ) ) > 0
                  aadd( scr_, substr( cBuffer, n + 1, n1 - n - 1 ) )
               ENDIF
            ENDIF
         NEXT
      ENDIF
      IF ! empty( scr_ )
         B_MSG "Select a Screen" CHOOSE scr_ RESTORE SHADOW CENTER INTO nSel
         IF nSel > 0
            ::cScreen := scr_[ nSel ]
         ENDIF
      ENDIF
   ENDIF

   IF ! empty( cBuffer)
      ::obj_:= {}
      ::scrBuildFromBuffer( cBuffer, ::cScreen )
      ::xRefresh := OBJ_REFRESH_ALL
      ::lEdited := .f.
      ::aUndo := {}
   ENDIF

   ::cScreen := iif( empty( ::cScreen ), "", ::cScreen )

   RETURN Self

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrBuildFromBuffer( cBuffer, cScreen )
   LOCAL cTokenB := "/* HB_SCREEN_BEGINS <" + cScreen + "> */"
   LOCAL cTokenE := "/* HB_SCREEN_ENDS <" + cScreen + "> */"
   LOCAL cCode, aCode, cLine, aAttr, aMatch, aMatches, nStart, nEnd, nLen, s, n, o_, a_

   IF ( nStart := at( cTokenB, cBuffer ) ) > 0
      IF ( nEnd := at( cTokenE, cBuffer ) ) > 0
         cCode := substr( cBuffer, nStart + len( cTokenB ), nEnd - nStart - 1 - len( cTokenB ) )
         aCode := hb_aTokens( strtran( cCode, chr( 13 ), chr( 10 ) ), chr( 10 ) )
         IF ! empty( aCode )
            aAttr := {}
            FOR EACH cLine IN aCode
               cLine := alltrim( cLine )
               IF ! empty( cLine )
                  IF left( cLine, 3 ) == "///"
                     aAttr := hb_aTokens( substr( cLine, 5 ), " " )
                     aSize( aAttr, 6 )
                     DEFAULT aAttr[ 6 ] TO ""
                     aAttr[ 1 ] := val( aAttr[ 1 ] )
                     aAttr[ 2 ] := val( aAttr[ 2 ] )
                     aAttr[ 3 ] :=      aAttr[ 3 ]
                     aAttr[ 4 ] := val( aAttr[ 4 ] )
                     aAttr[ 5 ] := val( aAttr[ 5 ] )
                     aAttr[ 6 ] := strtran( aAttr[ 6 ], "~", " " )

                  ELSE
                     SWITCH aAttr[ 2 ]

                     CASE OBJ_O_BOX
                        aMatches := hb_regExAll( "^@|\bBOX\b|\bCOLOR\b", cLine, .f., .f., 0, 1, .f. )
                        IF ! empty( nLen := len( aMatches ) )
                           o_:= ::scrObjBlank()
                           //
                           o_[ OBJ_TYPE       ] := OBJ_O_BOX
                           o_[ OBJ_F_LEN      ] := 9
                           o_[ OBJ_MDL_F_TYPE ] := 62

                           FOR EACH aMatch IN aMatches
                              SWITCH alltrim( upper( aMatch[ 1 ] ) )
                              CASE "@"
                                 n := aMatches[ aMatch:__enumIndex() + 1, 2 ]
                                 s := alltrim( substr( cLine, aMatch[ 3 ] + 1, n - 1 - aMatch[ 3 ] ) )
                                 s := strtran( s, " ,", "," )
                                 s := strtran( s, ", ", "," )
                                 a_:= hb_aTokens( s, "," )

                                 o_[ OBJ_ROW    ] := val( alltrim( a_[ 1 ] ) ) + 1
                                 o_[ OBJ_COL    ] := val( alltrim( a_[ 2 ] ) ) + 1
                                 o_[ OBJ_TO_ROW ] := val( alltrim( a_[ 3 ] ) ) + 1
                                 o_[ OBJ_TO_COL ] := val( alltrim( a_[ 4 ] ) ) + 1

                                 EXIT
                              CASE "BOX"
                                 IF nLen > 2
                                    n := aMatches[ aMatch:__enumIndex() + 1, 2 ]
                                    s := alltrim( substr( cLine, aMatch[ 3 ] + 1, n - 1 - aMatch[ 3 ] ) )
                                 ELSE
                                    s := substr( cLine, aMatch[ 3 ] + 1 )
                                 ENDIF
                                 s := alltrim( s )
                                 s := strtran( s, '"', "" )
                                 o_[ OBJ_BOX_SHAPE ] := substr( s, 1, 8 )
                                 o_[ OBJ_PATTERN   ] := iif( len( s ) == 9, "FILLED", "CLEAR" )

                                 EXIT
                              CASE "COLOR"
                                 o_[ OBJ_COLOR ] := alltrim( substr( cLine, aMatch[ 3 ] + 1 ) )

                                 EXIT
                              ENDSWITCH
                           NEXT
                           aadd( ::obj_, o_ )
                        ENDIF
                        EXIT

                     CASE OBJ_O_TEXT
                        aMatches := hb_regExAll( "^@|\bSAY\b|\bCOLOR\b", cLine, .f., .f., 0, 1, .f. )
                        IF ! empty( nLen := len( aMatches ) )
                           o_:= ::scrObjBlank()
                           //
                           o_[ OBJ_TYPE ] := OBJ_O_TEXT
                           o_[ OBJ_F_TYPE ] := "C"

                           FOR EACH aMatch IN aMatches
                              SWITCH alltrim( upper( aMatch[ 1 ] ) )
                              CASE "@"
                                 n := aMatches[ aMatch:__enumIndex() + 1, 2 ]
                                 s := alltrim( substr( cLine, aMatch[ 3 ] + 1, n - 1 - aMatch[ 3 ] ) )
                                 s := strtran( s, ", ", "," )
                                 a_:= hb_aTokens( s, "," )

                                 o_[ OBJ_ROW    ] := val( alltrim( a_[ 1 ] ) ) + 1
                                 o_[ OBJ_COL    ] := val( alltrim( a_[ 2 ] ) ) + 1

                                 EXIT
                              CASE "SAY"
                                 IF nLen > 2
                                    n := aMatches[ aMatch:__enumIndex() + 1, 2 ]
                                    s := alltrim( substr( cLine, aMatch[ 3 ] + 1, n - 1 - aMatch[ 3 ] ) )
                                 ELSE
                                    s := substr( cLine, aMatch[ 3 ] + 1 )
                                 ENDIF
                                 IF ! empty( aAttr[ 6 ] )
                                    o_[ OBJ_TEXT   ] := aAttr[ 6 ]
                                    o_[ OBJ_ID     ] := s
                                    s := aAttr[ 6 ]
                                 ELSE
                                    s := alltrim( s )
                                    s := substr( s, 2, len( s ) - 2 )
                                    o_[ OBJ_TEXT   ] := s
                                 ENDIF
                                 o_[ OBJ_TO_ROW ] := o_[ OBJ_ROW ]
                                 o_[ OBJ_TO_COL ] := o_[ OBJ_COL ] + len( s ) - 1
                                 o_[ OBJ_F_LEN  ] := len( s )

                                 EXIT
                              CASE "COLOR"
                                 o_[ OBJ_COLOR  ] := alltrim( substr( cLine, aMatch[ 3 ] + 1 ) )

                              ENDSWITCH
                           NEXT

                           aadd( ::obj_, o_ )
                        ENDIF
                        EXIT

                     CASE OBJ_O_FIELD
                        aMatches := hb_regExAll( "^@|\bGET\b|\bPICTURE\b|\bCOLOR\b|\bWHEN\b|\bVALID\b", cLine, .f., .f., 0, 1, .f. )
                        IF ! empty( nLen := len( aMatches ) )
                           o_:= ::scrObjBlank()
                           //
                           o_[ OBJ_TYPE ] := OBJ_O_FIELD

                           FOR EACH aMatch IN aMatches
                              SWITCH alltrim( upper( aMatch[ 1 ] ) )

                              CASE "@"
                                 n := aMatches[ aMatch:__enumIndex() + 1, 2 ]
                                 s := alltrim( substr( cLine, aMatch[ 3 ] + 1, n - 1 - aMatch[ 3 ] ) )
                                 s := strtran( s, ", ", "," )
                                 a_:= hb_aTokens( s, "," )

                                 o_[ OBJ_ROW    ] := val( alltrim( a_[ 1 ] ) ) + 1
                                 o_[ OBJ_COL    ] := val( alltrim( a_[ 2 ] ) ) + 1
                                 o_[ OBJ_TO_ROW ] := o_[ OBJ_ROW ]
                                 o_[ OBJ_TO_COL ] := o_[ OBJ_COL ] + aAttr[ 4 ] - 1
                                 o_[ OBJ_F_TYPE ] := aAttr[ 3 ]
                                 o_[ OBJ_F_LEN  ] := aAttr[ 4 ]
                                 o_[ OBJ_F_DEC  ] := aAttr[ 5 ]

                                 EXIT

                              CASE "GET"
                                 IF nLen > 2
                                    n := aMatches[ aMatch:__enumIndex() + 1, 2 ]
                                    s := alltrim( substr( cLine, aMatch[ 3 ] + 1, n - 1 - aMatch[ 3 ] ) )
                                 ELSE
                                    s := substr( cLine, aMatch[ 3 ] + 1 )
                                 ENDIF
                                 s := alltrim( s )
                                 o_[ OBJ_ID  ] := s
                                 o_[ OBJ_EQN ] := padc( s, aAttr[ 4 ] )
                                 EXIT

                              CASE "PICTURE"
                                 IF aMatch:__enumIndex() < len( aMatches )
                                    n := aMatches[ aMatch:__enumIndex() + 1, 2 ]
                                    o_[ OBJ_F_PIC ] := alltrim( substr( cLine, aMatch[ 3 ] + 1, n - 1 - aMatch[ 3 ] ) )
                                 ELSE
                                    o_[ OBJ_F_PIC ] := alltrim( substr( cLine, aMatch[ 3 ] + 1 ) )
                                 ENDIF
                                 EXIT

                              CASE "COLOR"
                                 IF aMatch:__enumIndex() < len( aMatches )
                                    n := aMatches[ aMatch:__enumIndex() + 1, 2 ]
                                    o_[ OBJ_COLOR ] := alltrim( substr( cLine, aMatch[ 3 ] + 1, n - 1 - aMatch[ 3 ] ) )
                                 ELSE
                                    o_[ OBJ_COLOR ] := alltrim( substr( cLine, aMatch[ 3 ] + 1 ) )
                                 ENDIF
                                 EXIT

                              CASE "WHEN"
                                 IF aMatch:__enumIndex() < len( aMatches )
                                    n := aMatches[ aMatch:__enumIndex() + 1, 2 ]
                                    o_[ OBJ_WHEN ] := alltrim( substr( cLine, aMatch[ 3 ] + 1, n - 1 - aMatch[ 3 ] ) )
                                 ELSE
                                    o_[ OBJ_WHEN ] := alltrim( substr( cLine, aMatch[ 3 ] + 1 ) )
                                 ENDIF
                                 EXIT

                              CASE "VALID"
                                 IF aMatch:__enumIndex() < len( aMatches )
                                    n := aMatches[ aMatch:__enumIndex() + 1, 2 ]
                                    o_[ OBJ_VALID ] := alltrim( substr( cLine, aMatch[ 3 ] + 1, n - 1 - aMatch[ 3 ] ) )
                                 ELSE
                                    o_[ OBJ_VALID ] := alltrim( substr( cLine, aMatch[ 3 ] + 1 ) )
                                 ENDIF
                                 EXIT

                              ENDSWITCH

                           NEXT

                           aadd( ::obj_, o_ )
                        ENDIF
                        EXIT

                     ENDSWITCH
                     aAttr := {}
                  ENDIF
               ENDIF
            NEXT
         ENDIF
      ENDIF
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD hbCUIEditor:scrSave( lAsk )
   LOCAL s, o_, cSource, cScreen, nLenSay, nLenPic, nLenClr, nLenWhn, nLenVld, nLenGet
   local prg_:={}

   DEFAULT lAsk TO .f.

   IF empty( ::cSource ) .OR. lAsk
      cSource := trim( VouchGetSome( "Source (.PRG) File", pad( ::cSource, 40 ) ) )
      IF empty( cSource )
         RETURN NIL
      ENDIF
      ::cSource := trim( cSource )
   ENDIF

   IF empty( ::cScreen ) .OR. lAsk .OR. ::cScreen == "Untitled"
      cScreen := alltrim( VouchGetSome( "Screen Identity?", pad( ::cScreen, 13 ) ) )
      IF empty( cScreen )
         cScreen := dtos( date() ) + left( time(), 5 )
      ENDIF
      ::cScreen := cScreen
   ENDIF

   ::scrOrdObj()

   nLenSay := nLenPic := nLenClr := nLenWhn := nLenVld := nLenGet := 0

   aeval( ::obj_, {|e_| iif( e_[ OBJ_TYPE ] == OBJ_O_FIELD, nLenGet := max( len( e_[ OBJ_ID   ] ), nLenGet ), ;
                        iif( e_[ OBJ_TYPE ] == OBJ_O_TEXT , nLenSay := max( len( e_[ OBJ_TEXT ] ), nLenSay ), NIL ) ) } )
   nLenSay := iif( empty( nLenSay ), 0, nLenSay + 2 )

   aeval( ::obj_, {|e_| nLenClr := max( len( e_[ OBJ_COLOR ] ), nLenClr ) } )
   aeval( ::obj_, {|e_| nLenPic := max( len( e_[ OBJ_F_PIC ] ), nLenPic ) } )
   aeval( ::obj_, {|e_| nLenWhn := max( len( e_[ OBJ_WHEN  ] ), nLenWhn ) } )
   aeval( ::obj_, {|e_| nLenVld := max( len( e_[ OBJ_VALID ] ), nLenVld ) } )

   aadd( prg_, "/* HB_SCREEN_BEGINS <" + ::cScreen + "> */" )
   aadd( prg_, " " )
   FOR EACH o_ IN ::obj_
      IF ! empty( o_[ OBJ_TYPE ] )

         aadd( prg_, "/// " + hb_ntos( o_:__enumIndex() ) + " " + hb_ntos( o_[ OBJ_TYPE ] ) + " " + ;
                         o_[ OBJ_F_TYPE ] + " " + N_TRIM( o_[ OBJ_F_LEN ] ) + " " + N_TRIM( o_[ OBJ_F_DEC ] ) + ;
                         iif( o_[ OBJ_TYPE ] == OBJ_O_TEXT, " " + strtran( o_[ OBJ_ID ], " ", "~" ), "" ) )

         s := pad( "@ " + N_TRIM( o_[ OBJ_ROW ] - 1 ) + ", " + N_TRIM( o_[ OBJ_COL ] - 1 ), 10 ) + " "

         SWITCH o_[ OBJ_TYPE ]

         CASE OBJ_O_FIELD
            s += "GET " + pad( o_[ OBJ_ID ], nLenGet ) + " "
            IF !empty( o_[ OBJ_F_PIC ] )
               s += "PICTURE " + pad( o_[ OBJ_F_PIC ], nLenPic ) + " "
            ELSE
               IF nLenPic > 0
                  s += space( 8 + nLenPic + 1 )
               ENDIF
            ENDIF
            IF !empty( o_[ OBJ_COLOR ] )
               s += "COLOR "   + pad( o_[ OBJ_COLOR ], nLenClr ) + " "
            ELSE
               IF nLenClr > 0
                  s += space( 6 + nLenClr + 1 )
               ENDIF
            ENDIF
            IF !empty( o_[ OBJ_WHEN ] )
               s += "WHEN "    + pad( o_[ OBJ_WHEN  ], nLenWhn ) + " "
            ELSE
               IF nLenWhn > 0
                  s += space( 5 + nLenWhn + 1 )
               ENDIF
            ENDIF
            IF !empty( o_[ OBJ_VALID ] )
               s += "VALID "   + pad( o_[ OBJ_VALID ], nLenVld ) + " "
            ELSE
               s += space( 6 + nLenVld )
            ENDIF
            EXIT

         CASE OBJ_O_BOX
            s += ", " + N_TRIM( o_[ OBJ_TO_ROW ] - 1 ) + ", " + N_TRIM( o_[ OBJ_TO_COL ] - 1 ) + " BOX " + ;
                        '"' + o_[ OBJ_BOX_SHAPE ] + iif( o_[ OBJ_PATTERN ] == "CLEAR", "", " " ) + '"' + " "
            IF ! empty( o_[ OBJ_COLOR ] )
               s += "COLOR " + o_[ OBJ_COLOR ]
            ENDIF
            EXIT

         CASE OBJ_O_TEXT
            IF ! empty( o_[ OBJ_ID ] )
               s += "SAY " + o_[ OBJ_ID ] + " "
            ELSE
               s += "SAY " + pad( '"' + o_[ OBJ_TEXT ] + '"', nLenSay ) + " "
            ENDIF
            IF ! empty( o_[ OBJ_COLOR ] )
               s += "COLOR " + o_[ OBJ_COLOR ]
            ENDIF
            EXIT

         ENDSWITCH

         aadd( prg_, s )
      ENDIF
   NEXT
   aadd( prg_, " " )
   aadd( prg_, "/* HB_SCREEN_ENDS <" + ::cScreen + "> */" )

   IF !empty( prg_ )
      ::scrUpdateSource( prg_ )
   ENDIF

   RETURN Self

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrBuildFunction( prg_ )
   LOCAL s

   s := hb_eol()
   s += "FUNCTION hbcui_" + lower( strtran( ::cScreen, " ", "_" ) ) + "()" + hb_eol()
   s += hb_eol()
   s += ::scrBuildSource( prg_, 3 ) + hb_eol()
   s += hb_eol()
   s += "   RETURN NIL"
   s += hb_eol()

   RETURN s

/*----------------------------------------------------------------------*/

METHOD hbCUIEditor:scrBuildSource( prg_, nIndent )
   LOCAL cP, s := ""

   cP := space( nIndent )
   aeval( prg_, {|e| s += cP + e + hb_eol() } )
   s := substr( s, 1, len( s ) - len( hb_eol() ) )

   RETURN s

/*----------------------------------------------------------------------*/

METHOD hbCUIEditor:scrUpdateSource( prg_ )
   LOCAL cBuffer, cTokenB, cTokenE, nStart, nEnd, s, nIndent, cTmp

   IF hb_fileExists( ::cSource )
      cBuffer := hb_memoread( ::cSource )
      cTokenB := "/* HB_SCREEN_BEGINS <" + ::cScreen + "> */"
      cTokenE := "/* HB_SCREEN_ENDS <" + ::cScreen + "> */"

      IF ( nStart := at( cTokenB, cBuffer ) ) > 0
         nEnd := at( cTokenE, cBuffer )
         IF nEnd == 0
            nEnd := nStart + len( cTokenB )
         ELSE
            nEnd += len( cTokenE )
         ENDIF
         cTmp := substr( cBuffer, 1, nStart - 1 )
         nIndent := nStart - hb_rat( hb_eol(), cTmp ) - len( hb_eol() )

         s := ::scrBuildSource( prg_, nIndent )
         s := substr( cBuffer, 1, nStart - nIndent - 1 ) + s + substr( cBuffer, nEnd )

      ELSE
         s := ::scrBuildFunction( prg_ )
         s := cBuffer + hb_eol() + s

      ENDIF

   ELSE
      s := ::scrBuildFunction( prg_)

   ENDIF

   hb_memowrit( ::cSource, s )
   ::lEdited := .f.

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD hbCUIEditor:scrConfig()
   LOCAL s, n

   ::sectors_       := {}
   ::nDesign        := 1

   ::nTop           := 1
   ::nLeft          := 0
   ::nBottom        := maxrow()-2
   ::nRight         := maxcol()

   ::nMode          := 0
   ::nRowCur        := ::nTop
   ::nColCur        := ::nLeft
   ::nRowRep        := 1
   ::nColRep        := 1
   ::nRowDis        := ::nTop  - 1
   ::nColDis        := ::nLeft - 1

   ::nRowMenu       := 0
   ::nRowRuler      := 0
   ::nRowStatus     := maxrow() - 1
   ::nColStatus     := 0

   ::nColsMax       := 400
   ::nRowPrev       := ::nTop
   ::nColPrev       := ::nLeft
   ::nRowsMax       := 200

   ::cClrStatus     := "W+/BG"
   ::cClrText       := 'W+/B'
   ::cClrHilite     := 'GR+/BG'
   ::cClrWindow     := 'W+/BG'
   ::cClrRuler      := "N/W"
   ::cClrOverall    := "N/W"
   ::cClrPrev       := 'B/W'
   ::cClrSelect     := 'GR+/N'

   ::nObjHilite     := 0
   ::nObjSelected   := 0

   s := '.'
   FOR n := 1 TO 40
      s += '.......' + strtran( str( n,3 ), ' ', '.' )
   NEXT
   ::cRuler         := s

   ::cDrawFill      := replicate( chr( 176 ), 9 )
   ::aObjId         := { 'Bitmap','Line','Text','Field','Expression','BitMap' }
   ::xRefresh       := OBJ_REFRESH_ALL
   ::nObjCopied     := 0
   ::aProperty      := {}
   ::lGraphics      := .f.
   ::aTextBlock     := {}
   ::aFields        := {}
   ::nLastKey       := 0

   RETURN Self

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrReConfig()

   ::nMode          := 0

   ::nTop           := 1
   ::nLeft          := 0
   ::nBottom        := maxrow() - 2
   ::nRight         := maxcol()

   ::nRowCur        := ::nTop
   ::nColCur        := ::nLeft
   ::nRowRep        := 1
   ::nColRep        := 1
   ::nRowDis        := ::nTop - 1
   ::nColDis        := ::nLeft - 1

   ::nRowMenu       := 0
   ::nRowRuler      := 0
   ::nRowStatus     := maxrow() - 1
   ::nColStatus     := 0

   ::nRowPrev       := ::nTop
   ::nColPrev       := ::nLeft
   ::nColsMax       := 400
   ::nRowsMax       := 200

   RETURN NIL

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrUpdateUndo()

   aadd( ::aUndo, aclone( ::obj_ ) )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD hbCUIEditor:scrUndo()
   LOCAL nLast

   IF ! empty( nLast := len( ::aUndo ) )
      ::obj_:= aclone( ::aUndo[ nLast ] )
      hb_adel( ::aUndo, nLast, .t. )
      ::xRefresh := OBJ_REFRESH_ALL
   ENDIF

   RETURN Self

//----------------------------------------------------------------------//

METHOD hbCUIEditor:operate()
   LOCAL nObj, nToCol, i, nOff
   LOCAL grf_:= { 43,45,46,48,49,50,51,52,53,54,55,56,57 }

   readinsert( .t. )

   ::scrDisplay()
   ::scrMove()
   ::scrStatus()

   keyboard( chr( K_UP ) )

   DO WHILE .t.
      ::nRowPrev := ::nRowCur
      ::nColPrev := ::nColCur
      ::xRefresh := OBJ_REFRESH_NIL

      setCursor( .t. )
      setCursor( iif( readInsert(), 2, 1 ) )

      DO WHILE .t.
         ::nLastKey := inkey( 0, INKEY_ALL + HB_INKEY_GTEVENT )
         IF ::nLastKey <> 0 .OR. ::nLastKey <> K_MOUSEMOVE
            EXIT
         ENDIF
      ENDDO

      DO CASE
      CASE ::lGraphics .AND. ascan( grf_,::nLastKey ) > 0
         //processkey()
      CASE ::scrMouse()
#IF 0
      CASE ::nLastKey == K_ALT_F6
         graphChar()
         ::lGraphics := ! ::lGraphics
         ::xRefresh  := OBJ_REFRESH_ALL
#ENDIF
      /*  Save Report */
      CASE ::nLastKey == K_ESC
         IF alert( "Do you want to exit ?", { "Yes","No" } ) == 1
            IF ::lEdited
               IF alert( "Screen has been edited, do you want to save ?", { "Yes","No" } ) == 1
                  ::scrSave()
               ENDIF
            ENDIF
            EXIT
         ENDIF
      CASE ::nLastKey == K_CTRL_ENTER
         IF ::lEdited
            ::scrSave()
         ENDIF
         EXIT
      CASE ::nLastKey == K_ALT_P
         ::scrPreview()
      CASE ::nLastKey == K_ALT_L
         ::scrLoad( .t. )
      CASE ::nLastKey == K_ALT_S
         ::scrSave( .t. )
      CASE ::nLastKey == K_ALT_G
         ::scrOrdGets()
      CASE ::nLastKey == K_ALT_Z
         ::scrUndo()

      CASE ::nLastKey == K_RIGHT
         ::scrMovRgt()
      CASE ::nLastKey == K_LEFT
         ::scrMovLft()
      CASE ::nLastKey == K_UP
         ::scrMovUp()
      CASE ::nLastKey == K_DOWN
         ::scrMovDn()
      CASE ::nLastKey == K_MWBACKWARD
         ::scrMovDn()
      CASE ::nLastKey == K_MWFORWARD
         ::scrMovUp()
      CASE ::nLastKey == K_HOME
         ::nColRep := 1
         ::nColCur := ::nLeft
         ::nColDis := ::nLeft - 1
         ::xRefresh := OBJ_REFRESH_ALL
      CASE ::nLastKey == K_END
         nToCol := 0
         ascan( ::obj_, {|e_| iif( e_[ OBJ_ROW ] == ::nRowRep, nToCol := max( nToCol, e_[ OBJ_TO_COL ] ), NIL ) } )
         IF nToCol > 0
            IF nToCol < ::nColRep
               nOff := ::nColRep - nToCol - 1
               FOR i := 1 TO nOff
                  ::scrMovLft()
               NEXT
            ELSEIF nToCol > ::nColRep
               FOR i := ::nColRep TO nToCol
                  ::scrMovRgt()
               NEXT
            ENDIF
         ENDIF
         ::xRefresh := OBJ_REFRESH_ALL
      CASE ::nLastKey == K_PGUP
         nOff := min( ::nRowRep - 1, ::nBottom - ::nTop + 1 )
         FOR i := 1 TO nOff
            ::scrMovUp()
         NEXT
         ::xRefresh := OBJ_REFRESH_ALL
      CASE ::nLastKey == K_PGDN
         nOff := min( ::nRowsMax - ::nRowRep, ::nBottom - ::nTop + 1 )
         FOR i := 1 TO nOff
            ::scrMovDn()
         NEXT
         ::xRefresh := OBJ_REFRESH_ALL

      CASE ::nLastKey == K_INS
         readInsert( !readInsert() )
         setcursor( iif( readInsert(), 2, 1 ) )

      CASE ::nLastKey == K_ENTER
         IF ::nMode == OBJ_MODE_SELECT .AND. ::nObjSelected > 0
            ::nMode         := OBJ_MODE_IDLE
            ::xRefresh      := OBJ_REFRESH_LINE
            ::nObjSelected  := 0
            ::scrMsg()
         ENDIF

      CASE VouchInRange( ::nLastKey, K_SPACE, 254 ) .AND. ::nMode <> OBJ_MODE_SELECT
         ::scrAddTxt( 1 )

      CASE ::nLastKey == K_F1                           //  Help
         help()
      CASE ::nLastKey == K_F7                           //  Copy
         ::scrObjCopy()
      CASE ::nLastKey == K_F8                           //  Paste
         ::scrObjPas()
      CASE ::nLastKey == K_F9                           //  Box
         ::scrAddBox()
      CASE ::nLastKey == K_F10                          //  Fields
         ::scrAddFld()

      CASE ::nLastKey == K_DEL
         IF ::nMode == OBJ_MODE_SELECT .AND. ::nObjSelected > 0
            ::scrObjDel( ::nObjSelected )
            ::nMode := 0
            ::xRefresh     := OBJ_REFRESH_ALL
            ::scrMsg()
         ELSE
            IF ! empty( ::aTextBlock )
               ::scrTextDel()
               ::scrOrdObj()
               ::nMode        := 0
               ::nObjSelected := 0
               ::nObjHilite   := 0
               ::xRefresh     := OBJ_REFRESH_ALL
            ELSEIF ::scrIsTxt()
               ::scrAddTxt( 2 )
            ELSEIF ::nObjHilite > 0
               ::scrObjDel( ::nObjHilite )
               ::nMode        := 0
               ::nObjHilite   := 0
               ::xRefresh     := OBJ_REFRESH_ALL
            ENDIF
         ENDIF
      CASE ::nLastKey == K_BS
         IF ::nMode <> OBJ_MODE_SELECT
            IF ::scrMovLft()
               IF ::scrIsTxt()
                  ::scrAddTxt( 3 )
               ENDIF
            ENDIF
         ENDIF

      CASE ::nLastKey == K_ALT_N
         ::scrAddLine()
         ::lEdited := .t.

      CASE ::nLastKey == K_ALT_O
         ::scrDelLine()
         ::lEdited := .t.

      CASE ::nLastKey == K_CTRL_F6    //  Selection of Block
         ::scrTextBlock()
      CASE ::nLastKey == K_CTRL_F7    //  Move, Copy
         ::scrTextMove( 1 )
      CASE ::nLastKey == K_CTRL_F8    //  Move, Cut AND Paste
         ::scrTextMove( 0 )

      CASE ::nLastKey == HB_K_RESIZE
         ::scrReConfig()
         ::scrDisplay()
         ::scrMove()
         ::scrStatus()

      ENDCASE

      IF ::nMode == OBJ_MODE_SELECT
         ::xRefresh := iif( ::xRefresh == OBJ_REFRESH_NIL, OBJ_REFRESH_LINE, ::xRefresh )
         ::scrUpdObjRC()
      ENDIF

      //  Check on which OBJECT cursor is placed
      //
      nObj := ::scrChkObj()

      IF nObj > 0 .and. ::nLastKey == K_F4
         ::scrGetProperty( nObj )
      ENDIF

      IF nObj > 0 .AND. ::nMode <> OBJ_MODE_SELECT
         ::xRefresh   := iif( ::xRefresh == OBJ_REFRESH_NIL, OBJ_REFRESH_LINE, ::xRefresh )
         ::nObjHilite := nObj
         ::scrOnFirstCol( nObj, { OBJ_O_FIELD, OBJ_O_EXP } )

      ELSEIF ! empty( ::nObjHilite )
         ::xRefresh    := iif( ::xRefresh == OBJ_REFRESH_NIL, OBJ_REFRESH_LINE, ::xRefresh )
         ::nObjHilite := 0

      ENDIF

      IF nObj > 0 .AND. ::nLastKey == K_F5
         SWITCH ::obj_[ nObj, OBJ_TYPE ]
         CASE OBJ_O_FIELD
            ::scrAddFld( nObj ) ; EXIT
         CASE OBJ_O_TEXT
            ::scrGetProperty( nObj ); EXIT
         CASE OBJ_O_BOX
            ::scrAddBox( nObj ) ; EXIT
         ENDSWITCH
      ENDIF

      //  Is the OBJECT selected
      IF nObj > 0 .AND. ::nLastKey == K_F6 .AND. ::objIsBox( nObj )
         ::scrUpdateUndo()

         ::nMode         := OBJ_MODE_SELECT
         ::nObjSelected := nObj
         ::scrOnFirstCol( nObj, { OBJ_O_BOX } )
         ::scrMsg( "BOX is selected: Arrow-keys to move, ENTER to finished, DEL to delete" )
         ::lEdited := .t.

      ELSEIF nObj > 0 .AND. ::nLastKey == K_F6 .AND. ! ::objIsBox( nObj )
         ::scrUpdateUndo()

         ::nMode        := OBJ_MODE_SELECT
         ::nObjSelected := nObj
         ::scrOnFirstCol( nObj, { OBJ_O_TEXT } )
         ::scrMsg( iif( ::objIsTxt( nObj ), "TEXT", "FIELD" ) + " is selected: Arrow-keys to move, ENTER to finished, DEL to delete" )
         ::lEdited := .t.

      ENDIF

      IF     ::xRefresh == OBJ_REFRESH_ALL
         ::scrMove()
      ELSEIF ::xRefresh == OBJ_REFRESH_LINE
         IF ::scrIsBoxIn()
            ::scrMove()
         ELSE
            ::scrMoveLine()
         ENDIF
      ENDIF

      ::scrStatus()

      IF ::lGraphics
         //grfRest()
      ENDIF
   ENDDO

   ::scrOrdObj()

   RETURN Self

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrDisplay()

   dispbegin()
   setcursor( 0 )
   setColor( ::cClrOverall )
   cls

   @ ::nRowRuler, ::nLeft SAY substr( ::cRuler, 1, ::nRight - ::nLeft + 1 ) COLOR ::cClrRuler

   ::scrMsg()

   setcolor( ::cClrWindow )
   setCursor( 2 )
   dispend()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD hbCUIEditor:scrMove()
   LOCAL i
   LOCAL crs := setCursor( 0 )
   LOCAL nOff, cText, nRow, nCol, cColor

   dispBegin()

   dispBox( ::nTop      ,;
            ::nLeft     ,;
            ::nBottom   ,;
            ::nRight    ,;
            ::cDrawFill ,;
            ::cClrPrev   )

   FOR i := 1 TO len( ::obj_ )
      IF ::obj_[ i,OBJ_ROW ] + ::nRowDis <= ::nBottom .AND. ;
         ::obj_[ i,OBJ_COL ] + ::nColDis <= ::nRight

         nOff := ::obj_[ i,OBJ_COL ] + ::nColDis
         nRow := ::obj_[ i,OBJ_ROW ] + ::nRowDis
         nCol := ::obj_[ i,OBJ_COL ] + ::nColDis

         IF nOff < 0
            nCol := 0
         ENDIF

         IF ::objIsBox( i )
            cColor := iif( ::nObjSelected == i, ::cClrSelect,;
                      iif( ::nObjHilite   == i, ::cClrHilite,;
                                           'W/B' ) )
            DO CASE
            CASE VouchInArray( ::obj_[ i,OBJ_MDL_F_TYPE ], { 61,62,63,67,68 } )
               dispBox( ::obj_[ i,OBJ_ROW    ] + ::nRowDis,;
                        ::obj_[ i,OBJ_COL    ] + ::nColDis,;
                        ::obj_[ i,OBJ_TO_ROW ] + ::nRowDis,;
                        ::obj_[ i,OBJ_TO_COL ] + ::nColDis,;
                        substr( ::obj_[ i, OBJ_BOX_SHAPE ], 1, 8 ),;
                        cColor )

            CASE VouchInArray( ::obj_[ i, OBJ_MDL_F_TYPE ], { 64,65 } )    //  Line
               @  ::obj_[ i, OBJ_ROW    ] + ::nRowDis,;
                  ::obj_[ i, OBJ_COL    ] + ::nColDis ;
               TO ::obj_[ i, OBJ_TO_ROW ] + ::nRowDis,;
                  ::obj_[ i, OBJ_TO_COL ] + ::nColDis ;
               COLOR cColor

            ENDCASE
         ENDIF

         IF ::objIsFld( i )
            cText  := ::obj_[ i,OBJ_TEXT ]
            cColor := iif( ::nObjSelected == i, ::cClrSelect,;
                      iif( ::nObjHilite   == i, ::nObjHilite,;
                                           'W+/W' ) )
            IF nOff < 0
               cText := substr( ::obj_[ i,OBJ_TEXT ], abs( nOff ) + 1 )
            ENDIF
            @ nRow, nCol SAY cText COLOR cColor
         ENDIF

         IF ::objIsTxt( i )
            cText  := ::obj_[ i,OBJ_EQN ]
            cColor := iif( ::nObjSelected == i, ::cClrSelect,;
                         iif( empty( ::obj_[ i, OBJ_COLOR ] ), ::cClrText,;
                                           'W/B' ) )
            IF nOff < 0
               cText := substr( ::obj_[ i, OBJ_EQN ], abs( nOff ) + 1 )
            ENDIF

            @ nRow, nCol SAY cText COLOR cColor
         ENDIF

      ELSEIF ( ::obj_[ i, OBJ_ROW ] + ::nRowDis > ::nBottom )

      ENDIF
   NEXT

   ::ScrDispSelected()
   dispEnd()
   setcursor( crs )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD hbCUIEditor:scrMoveLine()
   LOCAL i,crs, nRow, nCol, cText, nOff, cColor

   crs := setCursor( 0 )

   IF ::nRowPrev == ::nRowCur
      dispbegin()
      dispBox( ::nRowCur   ,;
               ::nLeft     ,;
               ::nRowCur   ,;
               ::nRight    ,;
               ::cDrawFill ,;
               ::cClrPrev   )

      FOR i := 1 TO len( ::obj_ )
         nOff := ::obj_[ i, OBJ_COL ] + ::nColDis
         nRow := ::obj_[ i, OBJ_ROW ] + ::nRowDis
         nCol := nOff

         IF ::objIsBox( i )
            DO CASE
            CASE VouchInArray( ::obj_[ i, OBJ_MDL_F_TYPE ], {64,65} )    //  Lines V.H
               @  ::obj_[ i, OBJ_ROW    ] + ::nRowDis,;
                  ::obj_[ i, OBJ_COL    ] + ::nColDis ;
               TO ::obj_[ i, OBJ_TO_ROW ] + ::nRowDis,;
                  ::obj_[ i, OBJ_TO_COL ] + ::nColDis ;
               COLOR iif( ::nObjHilite == i, ::nObjHilite,;
                                          'W/B' )
            ENDCASE
         ENDIF

         IF ::obj_[ i, OBJ_ROW ] == ::nRowRep
            IF ::objIsFld( i )
               cText := ::obj_[ i,OBJ_TEXT ]
               cColor := iif( ::nObjSelected == i, ::cClrSelect,;
                         iif( ::nObjHilite   == i, ::nObjHilite,;
                                           'W+/W' ) )
               @ nRow, nCol SAY cText COLOR cColor
            ENDIF

            IF ::objIsTxt( i )
               cText  := ::obj_[ i, OBJ_EQN ]
               cColor := iif( ::nObjSelected == i, ::cClrSelect,;
                         iif( empty( ::obj_[ i, OBJ_COLOR ] ), ::cClrText,;
                                            'W/B' ) )
               @ nRow, nCol SAY cText COLOR cColor
            ENDIF
         ENDIF
      NEXT

      ::scrDispSelected()

      dispEnd()
   ELSE
      ::scrMove()

   ENDIF

   setCursor( crs )
   RETURN NIL

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrDispSelected()
   LOCAL i,j,nCol,nRow

   IF ! empty( ::aTextBlock )
      DispBegin()

      FOR i := ::aTextBlock[ 1 ] TO ::aTextBlock[ 3 ]
         IF ( nRow := i + ::nRowDis ) <= ::nBottom
            FOR j := ::aTextBlock[ 2 ] TO ::aTextBlock[ 4 ]
               IF ( nCol := j + ::nColDis ) <= ::nRight
                  @ nRow, nCol SAY ::scrGetChar( i, j ) COLOR 'GR+/R'
               ENDIF
            NEXT
         ENDIF
      NEXT

      DispEnd()
   ENDIF
   RETURN NIL

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrDispGhost( gst_ )
   LOCAL i,j,nRow,nCol

   DispBegin()
   FOR i := gst_[ 1 ] TO gst_[ 3 ]
      IF ( nRow := i + ::nRowDis ) <= ::nBottom
         FOR j := gst_[ 2 ] TO gst_[ 4 ]
            IF ( nCol := j + ::nColDis ) <= ::nRight
               @ nRow, nCol SAY THE_FILL COLOR 'GR+/R'
            ENDIF
         NEXT
      ENDIF
   NEXT
   DispEnd()

   RETURN NIL

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrStatus()
   LOCAL s, typ_, objId, cS

   cS := iif( len( ::cSource ) <= 20, ::cSource, substr( ::cSource, 1, 3 ) + ".." + right( ::cSource, 15 ) )

   dispbegin()
   s := pad( cS, 20 ) + CHR_PIPE

   s += pad( ::cScreen, 13 ) +;
             CHR_PIPE+;
             ' R:'+;
             str( ::nRowRep - 1, 3 ) +;
             ' C:'+;
             str( ::nColRep - 1, 3 ) +;
             CHR_PIPE +;
             iif( readInsert(), 'Ins', '   ' ) +;
             CHR_PIPE

   objId := ''
   IF ::nObjHilite > 0
      objId := ::aObjId[ ::obj_[ ::nObjHilite, OBJ_TYPE ] ]
      IF ::objIsBox( ::nObjHilite )
         typ_:= { 'Bitmap', 'Frame', 'Ellipse', 'Line (H)', 'Line (V)', 'Grid', 'BarCode', 'Text Box' }
         objId := typ_[ ::obj_[ ::nObjHilite, OBJ_MDL_F_TYPE ] - 60 ]
      ENDIF

   ELSEIF ::nObjSelected > 0
      objId := ::aObjId[ ::obj_[ ::nObjSelected, OBJ_TYPE ] ]
      IF ::objIsBox( ::nObjSelected )
         typ_:= {'Bitmap','Frame','Ellipse','Line (H)','Line (V)','Grid','BarCode','Text Box'}
         objId := typ_[ ::obj_[ ::nObjSelected, OBJ_MDL_F_TYPE ] - 60 ]
      ENDIF
   ENDIF

   s += pad( trim( objId ), 10 ) + CHR_PIPE
   s += "U:" + hb_ntos( len( ::aUndo ) )

   @ ::nRowStatus, ::nColStatus SAY pad( s, maxcol() + 1 ) COLOR ::cClrStatus

   /* Ruler */
   s := substr( ::cRuler, max( 1, ::nColRep - ::nColCur + ::nLeft ), ::nRight - ::nLeft + 1 )
   DispBox( ::nTop - 1, 0, ::nTop - 1, maxcol(), '         ', ::cClrOverall )
   @ ::nRowRuler, ::nLeft SAY s COLOR ::cClrRuler
   @ ::nRowRuler, ::nColCur SAY substr( s, ::nColCur - ::nLeft + 1, 1 ) COLOR 'GR+/BG'

   DevPos( ::nRowCur, ::nColCur )

   ::nRowPrev := ::nRowCur
   ::nColPrev := ::nColCur

   dispend()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD hbCUIEditor:scrMouse()
   LOCAL nmRow, nmCol
   LOCAL nEvent := ::nLastKey
   LOCAL aEvents_:= { K_LBUTTONUP, K_LBUTTONDOWN, K_MMLEFTDOWN }

   STATIC nLastCol  := 0
   STATIC nLastRow  := 0
   STATIC lAnchored := .f.
   STATIC nCursor

   IF ! VouchInArray( ::nLastKey, aEvents_ )
      RETURN .f.
   ENDIF

   nmRow := mRow()
   nmCol := mCol()

   IF nmRow < ::nTop .OR. nmRow > ::nBottom .OR. nmCol < ::nLeft .OR. nmCol > ::nRight
      RETURN .f.
   ENDIF

   ::scrToMouse( nmRow, nmCol )

   IF nEvent == K_LDBLCLK

   ELSEIF nEvent == K_MMLEFTDOWN /*K_LBUTTONDOWN */ .AND. ! lAnchored
      IF ::scrChkObj() > 0 .AND. ::nMode <> OBJ_MODE_SELECT
         nCursor    := SetCursor( 0 )
         lAnchored  := .t.
         ::nLastKey := K_F6
      ENDIF

   ELSEIF nEvent == K_MMLEFTDOWN .AND. lAnchored

   ELSEIF nEvent == K_LBUTTONUP  .AND. lAnchored
      SetCursor( nCursor )
      lAnchored := .f.
      __keyboard( chr( K_ENTER ) )

   ELSEIF nEvent == K_LBUTTONUP

   ENDIF

   RETURN .t.

/*----------------------------------------------------------------------*/

METHOD hbCUIEditor:scrToMouse( nmRow, nmCol )
   LOCAL nRowOff, nColOff

   nRowOff := nmRow - ::nRowCur
   IF nRowOff <> 0
      ::nRowCur += nRowOff
      ::nRowRep += nRowOff
   ENDIF

   nColOff := nmCol - ::nColCur
   IF nColOff <> 0
      ::nColCur += nColOff
      ::nColRep += nColOff
   ENDIF

   RETURN NIL

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrOrdGets()
   LOCAL a_:={}, d_:={}, t_, n_, h_, n

   t_:={}
   FOR EACH a_ IN ::obj_
      IF a_[ OBJ_TYPE ] == OBJ_O_BOX
         aadd( t_, a_ )
      ENDIF
   NEXT
   IF ! empty( t_ )
      asort( t_, , , {|e_,f_| e_[ OBJ_ROW ] < f_[ OBJ_ROW ] } )
      aeval( t_, {|e_| aadd( d_, e_ ) } )
   ENDIF

   t_:={}
   FOR EACH a_ IN ::obj_
      IF a_[ OBJ_TYPE ] == OBJ_O_TEXT
         aadd( t_, a_ )
      ENDIF
   NEXT
   IF ! empty( t_ )
      asort( t_, , , {|e_,f_| e_[ OBJ_ROW ] < f_[ OBJ_ROW ] } )
      aeval( t_, {|e_| aadd( d_, e_ ) } )
   ENDIF

   // GETS are TO be appended as ordered by the developer
   t_:={}
   FOR EACH a_ IN ::obj_
      IF a_[ OBJ_TYPE ] == OBJ_O_FIELD
         aadd( t_, a_ )
      ENDIF
   NEXT
   IF ! empty( t_ )
      n_:= array( len( t_ ) )
      h_:= array( len( t_ ) )
      aeval( t_, {|e_,i| e_:= e_, n_[ i ] := i } )
      aeval( t_, {|e_,i| h_[ i ] := e_[ OBJ_ID ] } )

      ::scrMsg( "ENTER: Next Number if Blank, 0 if Numbered    CTRL_ENTER: When Done" )
      B_MSG "Order GETS" CHOOSE h_ RESTORE SHADOW CENTER INTO n_ SELECTIONS NUMERIC // NUMBERED n_
      ::scrMsg()

      IF len( n_ ) != len( t_ )
         alert( "Must ORDER every field !" )
         RETURN Self
      ENDIF
      FOR EACH n IN n_
         aadd( d_, t_[ n ] )
      NEXT
   ENDIF

   ::obj_:= d_

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD hbCUIEditor:scrOrdObj()
   LOCAL a_:={}, d_:={}, t_

   t_:={}
   FOR EACH a_ IN ::obj_
      IF a_[ OBJ_TYPE ] == OBJ_O_BOX
         aadd( t_, a_ )
      ENDIF
   NEXT
   IF ! empty( t_ )
      asort( t_, , , {|e_,f_| e_[ OBJ_ROW ] < f_[ OBJ_ROW ] } )
      aeval( t_, {|e_| aadd( d_, e_ ) } )
   ENDIF

   t_:={}
   FOR EACH a_ IN ::obj_
      IF a_[ OBJ_TYPE ] == OBJ_O_TEXT
         aadd( t_, a_ )
      ENDIF
   NEXT
   IF ! empty( t_ )
      asort( t_, , , {|e_,f_| e_[ OBJ_ROW ] < f_[ OBJ_ROW ] } )
      aeval( t_, {|e_| aadd( d_, e_ ) } )
   ENDIF

   // GETS are TO be appended as ordered by the developer
   FOR EACH a_ IN ::obj_
      IF a_[ OBJ_TYPE ] == OBJ_O_FIELD
         aadd( d_, a_ )
      ENDIF
   NEXT

   ::obj_:= d_

   RETURN Self

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrMovRgt()
   LOCAL lMoved := .t.

   ::nColCur++
   IF ::nColCur > ::nRight
      IF ::nColsMax > ::nColRep
         ::nColDis--
         ::nColCur--
         ::nColRep++
         ::xRefresh := OBJ_REFRESH_ALL
      ELSE
         lMoved := .f.
         tone( 100,1 )
         ::nColCur--
      ENDIF
   ELSE
      ::nColRep++
   ENDIF
   RETURN lMoved

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrMovLft()
   LOCAL lMoved := .t.
   ::nColCur--
   IF ::nColCur < ::nLeft
      IF ::nColRep > 1
         ::nColDis++
         ::nColCur++
         ::nColRep--
         ::xRefresh := OBJ_REFRESH_ALL
      ELSE
         lMoved := .f.
         tone(200,1)
         ::nColCur++
      ENDIF
   ELSE
      ::nColRep--
   ENDIF
   RETURN lMoved

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrMovUp()
   LOCAL lMoved := .t.

   ::nRowCur--
   IF ::nRowCur < ::nTop
      ::nRowCur := ::nTop
      IF ::nRowRep > 1
         ::nRowDis++
         ::nRowRep--
         ::xRefresh := OBJ_REFRESH_ALL
      ELSE
         lMoved := .f.
         tone(300,1)
      ENDIF
   ELSE
      ::nRowRep--
   ENDIF
   RETURN lMoved

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrMovDn()
   LOCAL lMoved := .t.

   ::nRowCur++
   IF ::nRowCur  > ::nBottom
      ::nRowCur := ::nBottom
      IF ::nRowRep < ::nRowsMax
         ::nRowDis--
         ::nRowRep++
         ::xRefresh := OBJ_REFRESH_ALL
      ELSE
         lMoved := .f.
         tone( 300,1 )
      ENDIF
   ELSE
      ::nRowRep++
   ENDIF
   RETURN lMoved

//----------------------------------------------------------------------//

METHOD hbCUIEditor:objType( nObj )
   RETURN ::obj_[ nObj, OBJ_TYPE ]

/*----------------------------------------------------------------------*/

METHOD hbCUIEditor:objIsTxt( nObj )
   RETURN ::obj_[ nObj, OBJ_TYPE ] == OBJ_O_TEXT

/*----------------------------------------------------------------------*/

METHOD hbCUIEditor:objIsBox( nObj )
   RETURN ::obj_[ nObj, OBJ_TYPE ] == OBJ_O_BOX

/*----------------------------------------------------------------------*/

METHOD hbCUIEditor:objIsFld( nObj )
   RETURN ::obj_[ nObj, OBJ_TYPE ] == OBJ_O_FIELD

/*----------------------------------------------------------------------*/

METHOD hbCUIEditor:scrIsTxt()
   LOCAL e_

   FOR EACH e_ IN ::obj_
      IF e_[ OBJ_TYPE ] == OBJ_O_TEXT
         IF ::nRowRep == e_[ OBJ_ROW ] .AND. ( ::nColRep >= e_[ OBJ_COL ] .AND. ::nColRep <= e_[ OBJ_TO_COL ] )
            RETURN .t.
         ENDIF
      ENDIF
   NEXT

   RETURN .f.

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrChkObj()
   LOCAL a_:={}, e_

   FOR EACH e_ IN ::obj_
      IF e_[ OBJ_TYPE ] == OBJ_O_BOX
         IF     ( ::nRowRep == e_[ OBJ_ROW ] .OR.  ::nRowRep == e_[ OBJ_TO_ROW ] ) .AND. ;
                ( ::nColRep >= e_[ OBJ_COL ] .AND. ::nColRep <= e_[ OBJ_TO_COL ] )
            aadd( a_, e_:__enumIndex() )
         ELSEIF ( ::nColRep == e_[ OBJ_COL ] .OR.  ::nColRep == e_[ OBJ_TO_COL ] ) .AND. ;
                ( ::nRowRep >= e_[ OBJ_ROW ] .AND. ::nRowRep <= e_[ OBJ_TO_ROW ] )
            aadd( a_, e_:__enumIndex() )
         ENDIF
      ELSE
         IF ::nRowRep == e_[ OBJ_ROW ] .AND. ( ::nColRep >= e_[ OBJ_COL ] .AND. ::nColRep <= e_[ OBJ_TO_COL ] )
            aadd( a_, e_:__enumIndex() )
         ENDIF
      ENDIF
   NEXT

   IF !empty( a_ )
      IF len( a_ ) >= 2
         RETURN a_[ 2 ]
      ELSE
         RETURN a_[ 1 ]
      ENDIF
   ENDIF

   RETURN 0

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrUpdObjRC()
   LOCAL nW, nH
   LOCAL nObj := ::nObjSelected

   IF nObj > 0
      nH := ::obj_[ nObj, OBJ_TO_ROW ] - ::obj_[ nObj, OBJ_ROW ]
      nW := ::obj_[ nObj, OBJ_TO_COL ] - ::obj_[ nObj, OBJ_COL ]

      ::obj_[ nObj, OBJ_ROW ] := ::nRowRep
      ::obj_[ nObj, OBJ_COL ] := ::nColRep

      IF ::objIsBox( nObj )
         ::obj_[ nObj, OBJ_TO_ROW ] := ::obj_[ nObj, OBJ_ROW ] + nH
         ::obj_[ nObj, OBJ_TO_COL ] := ::obj_[ nObj, OBJ_COL ] + nW
      ELSE
         ::obj_[ nObj, OBJ_TO_ROW ] := ::nRowRep
         ::obj_[ nObj, OBJ_TO_COL ] := ::nColRep + len( ::obj_[ nObj, iif( ::objIsTxt( nObj ), OBJ_EQN, OBJ_TEXT ) ] ) - 1
      ENDIF
   ENDIF
   RETURN NIL

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrRepCol()
   LOCAL oCol := ::nColsMax, nCol

   nCol := VouchGetSome( 'Number of Columns?', oCol )

   IF !empty( nCol )
      nCol := max( 10, nCol )
      ::nColsMax             := nCol
      ::nRight               := min( maxcol(), ::nLeft + nCol - 1 )
      ::xRefresh             := OBJ_REFRESH_ALL
   ENDIF

   RETURN NIL

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrAddLine()

   ::scrUpdateUndo()

   aeval( ::obj_, {|e_,i| iif( e_[ OBJ_ROW ] >= ::nRowRep, ::obj_[ i, OBJ_TO_ROW ]++, NIL ) } )
   aeval( ::obj_, {|e_,i| iif( e_[ OBJ_ROW ] >= ::nRowRep, ::obj_[ i, OBJ_ROW    ]++, NIL ) } )

   ::xRefresh := OBJ_REFRESH_ALL

   RETURN NIL

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrDelLine()
   LOCAL nRow := ::nRowRep
   LOCAL n, isLast

   ::scrUpdateUndo()

   isLast := ( nRow == ::nRowsMax )

   IF ::nRowsMax < ( ::nBottom - ::nTop + 1 )
      ::nBottom := max( ::nTop, min( ::nBottom - 1, maxrow() - 2 ) )
   ENDIF

   DO WHILE .t.
      IF ( n := ascan( ::obj_, {|e_| e_[ OBJ_ROW ] == nRow } ) ) == 0
         EXIT
      ENDIF
      VouchAShrink( ::obj_, n )
   ENDDO

   aeval( ::obj_, {|e_,i| iif( e_[ OBJ_ROW ] > nRow, ::obj_[ i, OBJ_TO_ROW ]--, NIL ) } )
   aeval( ::obj_, {|e_,i| iif( e_[ OBJ_ROW ] > nRow, ::obj_[ i, OBJ_ROW    ]--, NIL ) } )

   IF isLast
      ::nRowRep--
      ::nRowCur--
   ENDIF

   ::xRefresh := OBJ_REFRESH_ALL

   RETURN NIL

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrIsBoxIn()
   RETURN ascan( ::obj_,{|e_| VouchInRange( ::nRowRep, e_[ OBJ_ROW ], e_[ OBJ_TO_ROW ] );
                                     .AND. ;
                            ( e_[ OBJ_TYPE ] == OBJ_O_BOX ) } )    >    0

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrObjCopy()

   IF ::nMode == OBJ_MODE_SELECT
      ::nObjCopied := ::nObjSelected
   ELSEIF ::nObjHilite > 0
      ::nObjCopied := ::nObjHilite
   ENDIF
   RETURN NIL

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrObjPas()       //  Paste Copied OBJECT
   LOCAL nObj, o_, oldRow, oldCol, oldRow2, oldcol2

   IF ( nObj := ::nObjCopied ) > 0 .AND. ::nObjSelected == 0
      ::scrUpdateUndo()

      o_:= aclone( ::obj_[ nObj ] )

      oldRow  := o_[ OBJ_ROW    ] ; oldCol  := o_[ OBJ_COL    ]
      oldRow2 := o_[ OBJ_TO_ROW ] ; oldCol2 := o_[ OBJ_TO_COL ]

      o_[ OBJ_ROW ]         := ::nRowRep
      o_[ OBJ_COL ]         := ::nColRep
      IF o_[ OBJ_TYPE   ]   == OBJ_O_FIELD
         o_[ OBJ_TO_ROW ]   := ::nRowRep
         o_[ OBJ_TO_COL ]   := ::nColRep + len( o_[ OBJ_TEXT ] ) - 1
      ELSEIF o_[ OBJ_TYPE ] == OBJ_O_BOX
         o_[ OBJ_TO_ROW ]   := ::nRowRep + ( oldRow2 - oldRow )
         o_[ OBJ_TO_COL ]   := ::nColRep + ( oldCol2 - oldCol )
      ELSEIF o_[ OBJ_TYPE ] == OBJ_O_TEXT
         o_[ OBJ_TO_ROW]    := ::nRowRep
         o_[ OBJ_TO_COL ]   := ::nColRep + ( oldCol2 - oldCol )
      ENDIF

      aadd( ::obj_, o_ )

      ::scrOrdObj()
      ::nObjSelected  := 0
      ::xRefresh      := OBJ_REFRESH_LINE
      ::nMode         := 0
      ::nObjCopied    := 0
   ENDIF
   RETURN NIL

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrObjDel( nObj )

   ::scrUpdateUndo()

   VouchAShrink( ::obj_, nObj )
   ::nObjSelected := 0
   ::xRefresh     := OBJ_REFRESH_LINE

   RETURN Self

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrOnLastCol( nObj )
   LOCAL nOff, i

   IF ::objIsBox( nObj )
      nOff := ::obj_[ nObj, OBJ_TO_COL ] - ::nColRep
      FOR i := 1 TO nOff
         ::scrMovRgt()
      NEXT

      nOff := ::obj_[ nObj, OBJ_TO_ROW ] - ::nRowRep
      FOR i := 1 TO nOff
         ::scrMovDn()
      NEXT
      ::scrMove()
      ::scrStatus()
      ::nRowPrev := ::nRowCur
      ::nColPrev := ::nColCur
   ENDIF

   RETURN NIL

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrOnFirstCol( nObj, type_ )
   LOCAL nCur, nOff

   IF ::nColRep <> ::obj_[ nObj,OBJ_COL ]
      IF VouchInArray( ::obj_[ nObj, OBJ_TYPE ], type_ )
         IF ::objIsBox( nObj )
            nCur := ::nColCur
            nOff := ::nColRep - ::obj_[ nObj, OBJ_COL ]
            ::nColCur := max( ::nLeft, ::nColCur - nOff )
            ::nColRep := ::obj_[ nObj, OBJ_COL]
            IF nOff > nCur - ::nLeft
               ::xRefresh := OBJ_REFRESH_ALL
               ::nColDis += nOff - ( nCur - ::nLeft )
            ENDIF

            nCur := ::nRowCur
            nOff := ::nRowRep - ::obj_[ nObj, OBJ_ROW ]
            ::nRowCur := max( ::nTop, ::nRowCur - nOff )
            ::nRowRep := ::obj_[ nObj,OBJ_ROW ]
            IF nOff > nCur - ::nTop
               ::xRefresh := OBJ_REFRESH_ALL
               ::nRowDis += nOff - ( nCur - ::nTop )
            ENDIF

         ELSE
            IF ::nLastKey == K_RIGHT
               nCur := ::nColCur
               nOff := ::obj_[ nObj, OBJ_TO_COL ] - ::nColRep + 1 //  NEXT Col TO OBJECT
               IF ::nColRep + nOff > ::nColsMax
                  ::nColsMax := ::nColRep + nOff
               ENDIF
               ::nColCur := min( ::nRight, ::nColCur + nOff )
               ::nColRep := ::obj_[ nObj,OBJ_TO_COL ] + 1
               IF nOff > ::nRight - nCur
                  ::xRefresh := OBJ_REFRESH_ALL
                  ::nColDis -= nOff - ( ::nRight - nCur )
               ENDIF
               ::nObjHilite := 0
            ELSE
               nCur := ::nColCur
               nOff := ::nColRep - ::obj_[ nObj,OBJ_COL ]
               ::nColCur := max( ::nLeft, ::nColCur - nOff )
               ::nColRep := ::obj_[ nObj,OBJ_COL ]
               IF nOff > nCur - ::nLeft
                  ::xRefresh := OBJ_REFRESH_ALL
                  ::nColDis += nOff - ( nCur - ::nLeft )
               ENDIF
           ENDIF
        ENDIF
      ENDIF
   ENDIF

   RETURN NIL

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrGetChar( nRow, nCol )
   LOCAL s := THE_FILL,n

   //  Locate Text
   n := ascan( ::obj_,{|e_| e_[ OBJ_ROW ] == nRow .AND. ;
                     VouchInRange( nCol, e_[ OBJ_COL ], e_[ OBJ_TO_COL ] ) } )
   IF n == 0   //  Locate Box
      n := ascan( ::obj_,{|e_| VouchInRange( nRow, e_[ OBJ_ROW ], e_[ OBJ_TO_ROW ] ) .AND. ;
                                   VouchInRange( nCol, e_[ OBJ_COL ], e_[ OBJ_TO_COL ] ) } )
   ENDIF

   IF n > 0
      IF     ::objIsTxt( n )
         s := substr( ::obj_[ n, OBJ_TEXT ], nCol - ::obj_[ n, OBJ_COL ] + 1, 1 )

      ELSEIF ::objIsFld( n )
         s := substr( ::obj_[ n, OBJ_ID ], nCol - ::obj_[ n, OBJ_COL ] + 1, 1 )

      ELSEIF ::objIsBox( n )
         IF     nRow == ::obj_[ n, OBJ_ROW   ]
            IF     nCol == ::obj_[ n, OBJ_COL       ]
               s := substr(::obj_[ n, OBJ_BOX_SHAPE ], 1, 1 )
            ELSEIF nCol == ::obj_[ n, OBJ_TO_COL    ]
               s := substr(::obj_[ n, OBJ_BOX_SHAPE ], 3, 1 )
            ELSE
               s := substr(::obj_[ n, OBJ_BOX_SHAPE ], 2, 1 )
            ENDIF
         ELSEIF nRow == ::obj_[ n, OBJ_TO_ROW ]
            IF     nCol == ::obj_[ n, OBJ_COL       ]
               s := substr(::obj_[ n, OBJ_BOX_SHAPE ], 7, 1 )
            ELSEIF nCol == ::obj_[ n, OBJ_TO_COL    ]
               s := substr(::obj_[ n, OBJ_BOX_SHAPE ], 5, 1 )
            ELSE
               s := substr(::obj_[ n, OBJ_BOX_SHAPE ], 6, 1 )
            ENDIF
         ELSE
            IF     nCol == ::obj_[ n, OBJ_COL       ]
               s := substr(::obj_[ n, OBJ_BOX_SHAPE ], 8, 1 )   //  4.8 are Same
            ELSEIF nCol == ::obj_[ n, OBJ_TO_COL    ]
               s := substr(::obj_[ n, OBJ_BOX_SHAPE ], 4, 1 )
            ELSE
               s := substr(::obj_[ n, OBJ_BOX_SHAPE ], 9, 1 )
               s := iif( empty( s ), THE_FILL, s )
            ENDIF
         ENDIF
      ENDIF
   ENDIF
   RETURN s

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrTextBlock()
   LOCAL n, nKey
   LOCAL key_:= { K_RIGHT,K_LEFT,K_UP,K_DOWN,K_ENTER }

   ::aTextBlock := { ::nRowRep, ::nColRep, ::nRowRep, ::nColRep }

   ::scrMsg( 'Use <Arrow Keys> TO Select Text Block, <Enter> TO Finish' )
   ::scrMove()
   ::scrStatus()

   DO WHILE .t.
      nKey := ::scrInkey( key_ )

      DO CASE
      CASE nKey == key_[ 1 ]
         IF ::scrMovRgt()
            ::aTextBlock[ 4 ]++
         ENDIF
      CASE nKey == key_[ 2 ]
         IF ::scrMovLft()
            ::aTextBlock[ 4 ]--
         ENDIF
      CASE nKey == key_[ 3 ]
         IF ::scrMovUp()
            ::aTextBlock[ 3 ]--
         ENDIF
      CASE nKey == key_[ 4 ]
         IF ::scrMovDn()
            ::aTextBlock[ 3 ]++
         ENDIF
      CASE nKey == key_[ 5 ]
         EXIT
      ENDCASE

      IF ::aTextBlock[ 3 ] < ::aTextBlock[ 1 ]
         n := ::aTextBlock[ 1 ]
         ::aTextBlock[ 1 ] := ::aTextBlock[ 3 ]
         ::aTextBlock[ 3 ] := n
      ENDIF

      IF ::aTextBlock[ 4 ] < ::aTextBlock[ 2 ]
         n := ::aTextBlock[ 2 ]
         ::aTextBlock[ 2 ] := ::aTextBlock[ 4 ]
         ::aTextBlock[ 4 ] := n
      ENDIF

      ::scrMove()
      ::scrStatus()
   ENDDO
   ::scrMsg()

   RETURN NIL

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrTextMove( nMode )
   LOCAL gst_, nKey
   LOCAL crs := setCursor(0)
   LOCAL key_:= { K_RIGHT, K_LEFT, K_UP, K_DOWN, K_ENTER }

   DEFAULT nMode TO 0   //  0.Paste   1.Copy

   IF ! empty( ::aTextBlock )
      //  CREATE a ghost movement block
      ::scrMsg('Use Arrow Keys TO Move Selected Block')
      //  Check FOR current cursor position
      gst_:= { ::nRowRep , ::nColRep,;
               ::nRowRep + ::aTextBlock[ 3 ] - ::aTextBlock[ 1 ],;
               ::nColRep + ::aTextBlock[ 4 ] - ::aTextBlock[ 2 ] }
      DO WHILE .t.
         ::scrMove()
         ::scrDispGhost( gst_ )
         ::scrStatus()

         nKey := ::scrInkey( key_ )
         DO CASE
         CASE nKey == key_[1]
            IF ::scrMovRgt()
               gst_[2]++ ; gst_[4]++
            ENDIF
         CASE nKey == key_[2]
            IF ::scrMovLft()
               gst_[2]-- ; gst_[4]--
            ENDIF
         CASE nKey == key_[3]
            IF ::scrMovUp()
               gst_[1]-- ; gst_[3]--
            ENDIF
         CASE nKey == key_[4]
            IF ::scrMovDn()
               gst_[1]++ ; gst_[3]++
            ENDIF
         CASE nKey == key_[5]
            EXIT
         ENDCASE
      ENDDO

      ::scrTextPost( gst_, nMode )

      ::scrOrdObj()
      ::scrMove()
      ::scrStatus()

      ::scrMsg()
   ENDIF
   setCursor(crs)

   RETURN Self

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrTextPost( gst_, nMode )
   LOCAL n,i,s,s1,s2,s3,n1,nWid,nCol,nn
   LOCAL del_:={0},ins_:={},d_:={},ddd_
   LOCAL old_:= aclone( ::aTextBlock )

   ::scrUpdateUndo()

   FOR i := gst_[ 1 ] TO gst_[ 3 ]
      n := -1
      DO WHILE .t.
         n := ascan( ::obj_, {|e_| e_[ OBJ_ROW ] == i ;
                                       .AND. ;
                        ( VouchInRange( e_[ OBJ_COL    ], gst_[ 2 ], gst_[ 4 ] );
                                       .OR. ;
                          VouchInRange( e_[ OBJ_TO_COL ], gst_[ 2 ], gst_[ 4 ] ) ) ;
                                       .AND.;
                                   ! VouchInArray( n, del_ ) } )
         IF n > 0
            IF ::objIsTxt( n )
               aadd( del_, n )

               s1    := '' ; s3 := ''
               s     := ::obj_[ n, OBJ_EQN ]
               nCol  := ::obj_[ n, OBJ_COL ]

               IF gst_[2] <= ::obj_[ n, OBJ_COL ] .AND. gst_[ 4 ] >= ::obj_[ n, OBJ_TO_COL ]
                  //  Only deletion of OBJECT
                  //  s2 := s
               ELSEIF gst_[2] >=  nCol
                  s1 := substr( s, 1, gst_[ 2 ] - nCol )
                  //  s2 := substr(s,gst_[2]-nCol+1,gst_[4]-nCol+1)
                  s3 := substr( s, gst_[ 4 ] - nCol + 2 )
               ELSEIF gst_[ 2 ] <   nCol
                  s1 := substr( s, 1, gst_[ 2 ] - nCol )
                  //  s2 := substr(s,gst_[2]-nCol+1,gst_[4]-nCol+1)
                  s3 := substr( s, gst_[ 4 ] - nCol + 2 )
               ENDIF

               IF len( s1 ) > 0
                  aadd( ins_, ::scrObjBlank() ) ; n1 := len( ins_ )

                  ins_[ n1, OBJ_TYPE    ] := OBJ_O_TEXT
                  ins_[ n1, OBJ_ROW     ] := ::obj_[ n, OBJ_ROW ]
                  ins_[ n1, OBJ_COL     ] := ::obj_[ n, OBJ_COL ]
                  ins_[ n1, OBJ_EQN     ] := s1
                  ins_[ n1, OBJ_TO_ROW  ] := ::obj_[ n, OBJ_ROW     ]
                  ins_[ n1, OBJ_TO_COL  ] := ins_[ n1, OBJ_COL ] + len( s1 ) - 1
               ENDIF

               IF len( s3 ) > 0
                  aadd( ins_, ::scrObjBlank() )
                  n1 := len( ins_ )

                  ins_[ n1, OBJ_TYPE    ] := OBJ_O_TEXT
                  ins_[ n1, OBJ_ROW     ] := ::obj_[n, OBJ_ROW]
                  ins_[ n1, OBJ_COL     ] := gst_[ 4 ] + 1
                  ins_[ n1, OBJ_EQN     ] := s3
                  ins_[ n1, OBJ_TO_ROW  ] := ::obj_[ n, OBJ_ROW     ]
                  ins_[ n1, OBJ_TO_COL  ] := ins_[ n1, OBJ_COL ] + len( s3 ) - 1
               ENDIF

            ELSEIF ::objIsFld( n )
               aadd( del_, n )

            ELSEIF ::objIsBox( n )

            ENDIF
         ELSE
            EXIT
         ENDIF
      ENDDO
   NEXT

   ddd_:= del_ ; del_:={0} ; nn := 0

   FOR i := old_[1] TO old_[3]    //  Rows
      n := -1

      DO WHILE .t.
         n := ascan(::obj_,{|e_| e_[ OBJ_ROW ] == i;
                                      .AND. ;
                     ( VouchInRange(e_[ OBJ_COL     ], old_[ 2 ], old_[ 4 ] );
                                       .OR. ;
                       VouchInRange( e_[ OBJ_TO_COL ], old_[ 2 ], old_[ 4 ] ) ) ;
                                      .AND. ;
                                  ! VouchInArray( n, del_ ) } )
         IF n > 0
            IF     ::objIsTxt( n )
               aadd( del_, n )

               //  TO be retained as it is
               s1    := '' ; s2 := '' ; s3 := ''
               s     := ::obj_[ n,OBJ_EQN]
               nCol  := ::obj_[ n,OBJ_COL]

               IF old_[ 2 ] <= ::obj_[ n, OBJ_COL ] .AND. old_[ 4 ] >= ::obj_[ n, OBJ_TO_COL ]
                  s2 := s   //  Insert WITH moved coordinates
               ELSEIF  old_[ 2 ] >= ::obj_[ n, OBJ_COL ]
                  s1 := substr( s, 1, old_[ 2 ] - nCol )
                  s2 := substr( s, old_[ 2 ] - nCol + 1, old_[ 4 ] - old_[ 2 ] + 1 )
                  s3 := substr( s, old_[ 4 ] - nCol + 2 )
               ELSEIF old_[ 2 ] < nCol
                  s1 := substr( s, 1, old_[ 2 ] - nCol )
                  s2 := substr( s, old_[ 2 ] - nCol + 1, old_[ 4 ] - old_[ 2 ] + 1 )
                  s3 := substr( s, old_[ 4 ] - nCol + 2 )
               ENDIF

               IF nMode == 0
                  IF len( s1 ) > 0
                     aadd( ins_, ::scrObjBlank() ) ; n1 := len( ins_ )

                     ins_[ n1, OBJ_TYPE    ] := OBJ_O_TEXT
                     ins_[ n1, OBJ_ROW     ] := ::obj_[ n, OBJ_ROW ]
                     ins_[ n1, OBJ_COL     ] := ::obj_[ n, OBJ_COL ]
                     ins_[ n1, OBJ_EQN     ] := s1
                     ins_[ n1, OBJ_TO_ROW  ] := ::obj_[ n, OBJ_ROW ]
                     ins_[ n1, OBJ_TO_COL  ] := ins_[ n1,OBJ_COL   ] + len( s1 ) - 1
                  ENDIF
                  IF len(s3) > 0
                     aadd( ins_, ::scrObjBlank() ) ; n1 := len( ins_ )

                     ins_[ n1, OBJ_TYPE    ] := OBJ_O_TEXT
                     ins_[ n1, OBJ_ROW     ] := ::obj_[ n, OBJ_ROW ]
                     ins_[ n1, OBJ_COL     ] := old_[ 4 ] + 1
                     ins_[ n1, OBJ_EQN     ] := s3
                     ins_[ n1, OBJ_TO_ROW  ] := ::obj_[ n, OBJ_ROW ]
                     ins_[ n1, OBJ_TO_COL  ] := ins_[ n1,OBJ_COL   ] + len( s3 ) - 1
                  ENDIF
               ENDIF

               IF len(s2) > 0
                  aadd( ins_, aclone( ::obj_[ n ] ) ) ;  n1 := len( ins_ )

                  ins_[ n1, OBJ_ROW    ] := gst_[ 1 ] + nn
                  ins_[ n1, OBJ_COL    ] := gst_[ 2 ]+ iif( old_[ 2 ] - ::obj_[ n, OBJ_COL ] >= 0, 0, abs( old_[ 2 ] - ::obj_[ n, OBJ_COL ] ) )
                  ins_[ n1, OBJ_TO_ROW ] := ins_[ n1, OBJ_ROW ]
                  ins_[ n1, OBJ_TO_COL ] := ins_[ n1, OBJ_COL ] + len( s2 ) - 1
                  ins_[ n1, OBJ_EQN    ] := s2
               ENDIF

            ELSEIF ::objIsFld( n )
               IF nMode == 0
                  aadd( del_, n )
               ENDIF

               //  Same OBJECT is TO be inserted IN moved block
               aadd( ins_, aclone( ::obj_[ n ] ) ) ; n1 := len( ins_ )
               nWid := ::obj_[ n, OBJ_TO_COL ] - ::obj_[ n, OBJ_COL ]

               ins_[ n1, OBJ_ROW    ] := gst_[ 1 ] + nn
               ins_[ n1, OBJ_COL    ] := gst_[ 2 ] + old_[ 2 ] - ::obj_[ n, OBJ_COL ]
               ins_[ n1, OBJ_TO_ROW ] := ins_[ n1, OBJ_ROW ]
               ins_[ n1, OBJ_TO_COL ] := ins_[ n1, OBJ_COL ] + nWid
            ENDIF
         ELSE
            EXIT
         ENDIF
      ENDDO
      nn++
   NEXT

   IF nMode <> 0
      del_:={}
   ENDIF
   aeval( ddd_,{|e| aadd( del_, e ) } )

   IF !empty( del_ )
      FOR i := 1 TO len( ::obj_)
         IF ascan( del_, i ) == 0
            aadd( d_, ::obj_[ i ] )
         ENDIF
      NEXT
      ::obj_:= aclone( d_ )
   ENDIF

   aeval( ins_, {|e_| aadd( ::obj_, e_ ) } )

   ::aTextBlock := {}

   RETURN Self

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrTextDel()
   LOCAL i,n,n1,s,s1,s3,nCol
   LOCAL ins_:={},del_:={},d_:={},old_:={}

   ::scrUpdateUndo()

   old_:= ::aTextBlock
   FOR i := old_[1] TO old_[3]    //  Rows
      n := -1

      DO WHILE .t.
         n := ascan( ::obj_,{|e_| e_[ OBJ_ROW ] == i;
                                      .AND. ;
                     ( VouchInRange( e_[ OBJ_COL    ], old_[ 2 ], old_[ 4 ] );
                                       .OR. ;
                       VouchInRange( e_[ OBJ_TO_COL ], old_[ 2 ], old_[ 4 ] ) ) ;
                                      .AND. ;
                                  ! VouchInArray( n, del_ ) })
         IF n > 0
            IF ::objIsTxt( n )
               aadd( del_, n )

               //  TO be retained as it is
               s1    := '' ; s3 := ''
               s     := ::obj_[ n, OBJ_EQN ]
               nCol  := ::obj_[ n, OBJ_COL ]

               IF old_[ 2 ] <= ::obj_[ n, OBJ_COL ] .AND. old_[ 4 ] >= ::obj_[ n, OBJ_TO_COL ]
                  //  s2 := s   //  Insert WITH moved coordinates
               ELSEIF  old_[ 2 ] >= ::obj_[ n, OBJ_COL ]
                  s1 := substr( s, 1, old_[ 2 ] - nCol )
                  //  s2 := substr(s,old_[2]-nCol+1,old_[4]-old_[2]+1)
                  s3 := substr( s, old_[ 4 ] - nCol + 2 )
               ELSEIF old_[ 2 ] < nCol
                  s1 := substr( s, 1, old_[ 2 ] - nCol )
                  //  s2 := substr(s,old_[2]-nCol+1,old_[4]-old_[2]+1)
                  s3 := substr( s, old_[ 4 ] - nCol + 2 )
               ENDIF

               IF len( s1 ) > 0
                  aadd( ins_, ::scrObjBlank() ) ; n1 := len( ins_ )

                  ins_[ n1, OBJ_TYPE    ] := OBJ_O_TEXT
                  ins_[ n1, OBJ_ROW     ] := ::obj_[ n,OBJ_ROW ]
                  ins_[ n1, OBJ_COL     ] := ::obj_[ n,OBJ_COL ]
                  ins_[ n1, OBJ_EQN     ] := s1
                  ins_[ n1, OBJ_TO_ROW  ] := ::obj_[ n, OBJ_ROW     ]
                  ins_[ n1, OBJ_TO_COL  ] := ins_[ n1, OBJ_COL ] + len( s1 ) - 1
               ENDIF
               IF len( s3 ) > 0
                  aadd( ins_, ::scrObjBlank() ) ; n1 := len( ins_ )

                  ins_[ n1, OBJ_TYPE    ] := OBJ_O_TEXT
                  ins_[ n1, OBJ_ROW     ] := ::obj_[ n, OBJ_ROW ]
                  ins_[ n1, OBJ_COL     ] := old_[ 4 ] + 1
                  ins_[ n1, OBJ_EQN     ] := s3
                  ins_[ n1, OBJ_TO_ROW  ] := ::obj_[ n, OBJ_ROW     ]
                  ins_[ n1, OBJ_TO_COL  ] := ins_[ n1, OBJ_COL ] + len( s3 ) - 1
               ENDIF

            ELSEIF ::objIsFld( n )
               aadd(del_,n)

            ENDIF
         ELSE
            EXIT
         ENDIF
      ENDDO
   NEXT

   IF !empty(del_)
      FOR i := 1 TO len( ::obj_ )
         IF ascan(del_,i) == 0
            aadd(d_,::obj_[i])
         ENDIF
      NEXT
      ::obj_:= aclone(d_)
      IF empty( ::obj_ )
         aadd( ::obj_,::scrObjBlank())
      ENDIF
   ENDIF

   aeval( ins_,{|e_| aadd( ::obj_,e_ ) } )
   ::aTextBlock := {}

   RETURN Self

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrAddTxt( nMode )
   LOCAL txt_:={}, n, lClub, i
   LOCAL n1,s1,s2,nTxt,nDel
   LOCAL nRepCol := ::nColRep, nRepRow := ::nRowRep
   LOCAL nKey    := ::nLastKey

   ::scrUpdateUndo()

   //  nMode   1.Add   2.Del   3.BS

   //  Scan obj_ FOR Text Objects Related WITH Current Report Row
   aeval( ::obj_,{|e_| iif( e_[ OBJ_TYPE ] == OBJ_O_TEXT .AND. e_[ OBJ_ROW ] == nRepRow, aadd( txt_,e_ ),'' ) } )
   IF nMode == 1      //  New Character
      IF empty( txt_ ) .OR. ascan( txt_, {|e_| VouchInRange( nRepCol, e_[ OBJ_COL ], e_[ OBJ_TO_COL ] ) } ) == 0
         aadd( txt_, ::scrObjBlank() )
         nTxt := len( txt_ )
         txt_[ nTxt, OBJ_TYPE    ]  := OBJ_O_TEXT
         txt_[ nTxt, OBJ_F_TYPE  ]  := 'C'
         txt_[ nTxt, OBJ_F_LEN   ]  := 1
         txt_[ nTxt, OBJ_ROW     ]  := ::nRowRep
         txt_[ nTxt, OBJ_COL     ]  := ::nColRep
         txt_[ nTxt, OBJ_EQN     ]  := ''
         txt_[ nTxt, OBJ_TO_ROW  ]  := ::nRowRep
         txt_[ nTxt, OBJ_TO_COL  ]  := ::nColRep
      ENDIF
   ENDIF

   nTxt := ascan( txt_,{|e_| VouchInRange( nRepCol, e_[ OBJ_COL ], e_[ OBJ_TO_COL ] ) } )

   IF     nMode == 1
      txt_[ nTxt, OBJ_EQN ] := substr( txt_[ nTxt, OBJ_EQN ], 1, ::nColRep - txt_[ nTxt, OBJ_COL] ) + ;
                                 chr( nKey ) + ;
           substr( txt_[ nTxt, OBJ_EQN ], ::nColRep - txt_[ nTxt, OBJ_COL ] + iif( ReadInsert(), 1, 2 ) )

      txt_[ nTxt, OBJ_TO_COL ] := txt_[ nTxt, OBJ_COL ] + len( txt_[ nTxt, OBJ_EQN ] ) - 1

   ELSEIF nMode == 2  .OR. nMode == 3 //  Delete
      IF readInsert()
         txt_[nTxt,OBJ_EQN] := substr( txt_[ nTxt, OBJ_EQN ], 1,;
                        ::nColRep - txt_[ nTxt, OBJ_COL ] ) + ;
           substr( txt_[ nTxt, OBJ_EQN ], ::nColRep - txt_[ nTxt, OBJ_COL ] + 2 )
         txt_[ nTxt, OBJ_TO_COL ] := txt_[ nTxt, OBJ_COL ] + len( txt_[ nTxt, OBJ_EQN ] ) - 1
      ELSE             //  Divide it IN two objects
         s1   := substr( txt_[ nTxt, OBJ_EQN ], 1, ::nColRep - txt_[ nTxt, OBJ_COL ] )
         s2   := substr( txt_[ nTxt, OBJ_EQN ], ::nColRep - txt_[ nTxt, OBJ_COL ] + 2 )
         nDel := 0
         IF len( s1 ) > 0
            txt_[ nTxt, OBJ_EQN     ] := s1
            txt_[ nTxt, OBJ_TO_COL  ] := txt_[ nTxt, OBJ_COL ] + len( s1 ) - 1
         ELSE
            nDel := nTxt
         ENDIF

         IF len( s2 ) > 0
            IF nDel == 0
               aadd( txt_, aclone( txt_[ nTxt ] ) )
               n1 := len( txt_ )
            ELSE
               n1 := nDel
            ENDIF
            txt_[ n1, OBJ_TYPE    ] := OBJ_O_TEXT
            txt_[ n1, OBJ_F_TYPE  ] := 'C'
            txt_[ n1, OBJ_F_LEN   ] := len( s2 )
            txt_[ n1, OBJ_ROW     ] := ::nRowRep
            txt_[ n1, OBJ_COL     ] := ::nColRep+1
            txt_[ n1, OBJ_EQN     ] := s2
            txt_[ n1, OBJ_TO_ROW  ] := ::nRowRep
            txt_[ n1, OBJ_TO_COL  ] := txt_[ n1, OBJ_COL ] + len( s2 ) - 1
         ENDIF
         IF len( s1 ) == 0 .AND. len( s2 ) == 0
            VouchAShrink( txt_, nTxt )
         ENDIF
      ENDIF
   ENDIF

   IF !empty( txt_ )
      DO WHILE .t.
         IF ( n := ascan( txt_, {|e_| e_[ OBJ_TO_COL ] < e_[ OBJ_COL ] } ) ) > 0
            VouchAShrink( txt_, n )
         ELSE
            EXIT
         ENDIF
      ENDDO
      #if 0
      IF empty( txt_ )
         aadd( txt_, ::scrObjBlank() )
      ENDIF
      #endif
      //  CLUB DIFFERENT TEXT OBJECTS IF THESE ARE ADJACENT
      asort( txt_ , , , {|e_,f_| e_[ OBJ_COL ] < f_[ OBJ_COL ] } )

      DO WHILE .t.
         lClub := .f.
         FOR i := 2 TO len( txt_ )
            IF txt_[ i    , OBJ_COL    ] == txt_[ i - 1, OBJ_TO_COL ] + 1
               txt_[ i - 1, OBJ_EQN    ] += txt_[ i, OBJ_EQN ]    //  Club both
               txt_[ i - 1, OBJ_TO_COL ] := txt_[ i - 1, OBJ_COL ] + len( txt_[ i - 1, OBJ_EQN ] ) - 1
               txt_[ i - 1, OBJ_F_LEN  ] := len( txt_[ i - 1, OBJ_EQN ] )
               VouchAShrink( txt_,i )
               lClub   := .t.
            ENDIF
         NEXT
         IF ! lClub
            EXIT
         ENDIF
      ENDDO
   ENDIF

   DO WHILE .t.
      IF( n := ascan( ::obj_, {|e_| e_[ OBJ_TYPE ] == OBJ_O_TEXT .AND. e_[ OBJ_ROW ] == ::nRowRep } ) ) > 0
         VouchAShrink( ::obj_,n )
      ELSE
         EXIT
      ENDIF
   ENDDO

   aeval( txt_, {|e_| iif( e_[ OBJ_ROW ] > 0, aadd( ::obj_, e_ ), '' ) } )

   DO WHILE .t.
      IF( n := ascan( ::obj_,{|e_| e_[ OBJ_TO_COL ] < e_[ OBJ_COL ] } ) ) > 0
         VouchAShrink( ::obj_, n )
      ELSE
         EXIT
      ENDIF
   ENDDO

   IF nMode == 1
      keyboard( chr( K_RIGHT ) )
   ENDIF

   ::xRefresh := OBJ_REFRESH_LINE
   ::lEdited := .t.

   ::scrOrdObj()

   RETURN NIL

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrMsg( msg )
   LOCAL row := row(), col := col()

   @ maxrow(),0 SAY padc( " ", maxcol()+1 ) COLOR "W+/W"
   IF empty( msg )
      msg := "F1:Help F4:Prop F5:Edit F6:Select F7:Copy F8:Paste F9:Box F10:Field"
   ENDIF
   msg := " " + msg + " "
   @ maxrow(), ( maxcol()+1-len( msg ) )/2 SAY msg COLOR "W+/RB"

   setPos( row,col )
   RETURN NIL

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrInkey( key_ )
   LOCAL nKey

   DO WHILE .t.
      nKey := inkey( 0 )
      IF ascan( key_, nKey ) > 0
         EXIT
      ENDIF
   ENDDO

   RETURN nKey

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrObjBlank()
   LOCAL o_:= array( OBJ_INIT_VRBLS )

   o_[ OBJ_TYPE       ] := 0
   o_[ OBJ_ROW        ] := 0
   o_[ OBJ_COL        ] := 0
   o_[ OBJ_TO_ROW     ] := 0
   o_[ OBJ_TO_COL     ] := 0
   o_[ OBJ_TEXT       ] := ""
   o_[ OBJ_F_TYPE     ] := "."
   o_[ OBJ_F_LEN      ] := 0
   o_[ OBJ_F_DEC      ] := 0
   o_[ OBJ_F_PIC      ] := ""
   o_[ OBJ_COLOR      ] := ""
   o_[ OBJ_WHEN       ] := ""
   o_[ OBJ_VALID      ] := ""
   o_[ OBJ_ID         ] := ""
   o_[ OBJ_SEC_ROW    ] := 0
   o_[ OBJ_OBJ_UNIQUE ] := 0
   o_[ OBJ_MDL_F_TYPE ] := 0

   RETURN o_

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrVrbBlank( nType )
   LOCAL v_:= {}
   LOCAL nW := 200

   SWITCH nType
   CASE OBJ_O_FIELD
      aadd( v_, space( nW ) )
      aadd( v_, 'C'         )
      aadd( v_, 25          )
      aadd( v_, 0           )
      aadd( v_, space( nW ) )
      aadd( v_, space( nW ) )
      aadd( v_, space( nW ) )
      aadd( v_, space( nW ) )
      EXIT
   CASE OBJ_O_BOX
      aadd( v_, space( nW ) )
      aadd( v_, B_SINGLE    )
      aadd( v_, "CLEAR"     )
      EXIT
   CASE OBJ_O_TEXT
      aadd( v_, space( nW ) )
      aadd( v_, space( nW ) )
      EXIT
   ENDSWITCH

   RETURN v_

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrObj2Vv( o_ )
   LOCAL v_:={}, nW := 200

   SWITCH o_[ OBJ_TYPE ]

   CASE OBJ_O_FIELD
      aadd( v_, pad( o_[ OBJ_ID      ], nW ) )
      aadd( v_,      o_[ OBJ_F_TYPE  ]       )
      aadd( v_,      o_[ OBJ_F_LEN   ]       )
      aadd( v_,      o_[ OBJ_F_DEC   ]       )
      aadd( v_, pad( o_[ OBJ_F_PIC   ], nW ) )
      aadd( v_, pad( o_[ OBJ_COLOR   ], nW ) )
      aadd( v_, pad( o_[ OBJ_WHEN    ], nW ) )
      aadd( v_, pad( o_[ OBJ_VALID   ], nW ) )
      EXIT
   CASE OBJ_O_BOX
      aadd( v_, pad( o_[ OBJ_COLOR   ], nW ) )
      aadd( v_, o_[ OBJ_BORDER  ] )
      aadd( v_, o_[ OBJ_PATTERN ] )
      EXIT
   CASE OBJ_O_TEXT
      aadd( v_, pad( o_[ OBJ_ID      ], nW ) )
      aadd( v_, pad( o_[ OBJ_COLOR   ], nW ) )
      EXIT
   ENDSWITCH

   RETURN v_

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrVrbHeaders( nType )
   LOCAL h_:= {}

   SWITCH nType
   CASE OBJ_O_FIELD
      aadd( h_, ' Expression' )
      aadd( h_, ' Type      ' )
      aadd( h_, ' Width     ' )
      aadd( h_, ' Decimals  ' )
      aadd( h_, ' Picture   ' )
      aadd( h_, ' Color     ' )
      aadd( h_, ' When      ' )
      aadd( h_, ' Valid     ' )
      EXIT
   CASE OBJ_O_BOX
      aadd( h_, ' Color     ' )
      aadd( h_, ' Border    ' )
      aadd( h_, ' Pattern   ' )
      EXIT
   CASE OBJ_O_TEXT
      aadd( h_, ' Expression' )
      aadd( h_, ' Color     ' )
      EXIT
   ENDSWITCH

   RETURN h_

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrVv2Obj( v_, o_ )

   SWITCH o_[ OBJ_TYPE ]

   CASE OBJ_O_FIELD
      o_[ OBJ_ID      ] := trim( v_[ 1 ] )
      o_[ OBJ_F_TYPE  ] :=       v_[ 2 ]
      o_[ OBJ_F_LEN   ] :=       v_[ 3 ]
      o_[ OBJ_F_DEC   ] :=       v_[ 4 ]
      o_[ OBJ_F_PIC   ] := trim( v_[ 5 ] )
      o_[ OBJ_COLOR   ] := trim( v_[ 6 ] )
      o_[ OBJ_WHEN    ] := trim( v_[ 7 ] )
      o_[ OBJ_VALID   ] := trim( v_[ 8 ] )
      EXIT
   CASE OBJ_O_BOX
      o_[ OBJ_COLOR   ] := trim( v_[ 1 ] )
      o_[ OBJ_BORDER  ] := v_[ 2 ]
      o_[ OBJ_PATTERN ] := v_[ 3 ]
      EXIT
   CASE OBJ_O_TEXT
      o_[ OBJ_ID      ] := trim( v_[ 1 ] )
      o_[ OBJ_COLOR   ] := trim( v_[ 2 ] )
      EXIT
   ENDSWITCH

   RETURN Self

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrAddBox( nObj )
   LOCAL o_, nKey, nnObj := nObj

   ::scrUpdateUndo()

   DEFAULT nObj TO 0

   IF empty( nObj )
      o_:= ::scrObjBlank()

      o_[ OBJ_TYPE       ] := OBJ_O_BOX
      o_[ OBJ_ROW        ] := ::nRowRep
      o_[ OBJ_COL        ] := ::nColRep
      o_[ OBJ_TO_ROW     ] := ::nRowRep
      o_[ OBJ_TO_COL     ] := ::nColRep
      o_[ OBJ_F_LEN      ] := 9
      o_[ OBJ_MDL_F_TYPE ] := 62

      o_[ OBJ_BOX_SHAPE  ] := B_SINGLE
      o_[ OBJ_PATTERN    ] := "CLEAR"

      aadd( ::obj_, o_ )
      nObj := len( ::obj_ )
   ENDIF

   IF ! empty( nnObj )
      ::scrOnLastCol( nnObj )
   ENDIF
   ::scrMsg( 'Draw Box with <Arrow Keys>. Finish with <Enter>' )

   DO WHILE .t.
      nKey := inkey( 0 )
      DO CASE
      CASE nKey == K_RIGHT
         IF ::scrMovRgt()
            ::obj_[ nObj,OBJ_TO_COL ]++
         ENDIF
      CASE nKey == K_LEFT
         IF ::scrMovLft()
            ::obj_[ nObj,OBJ_TO_COL ]--
         ENDIF
      CASE nKey == K_DOWN
         IF ::scrMovDn()
            ::obj_[ nObj,OBJ_TO_ROW ]++
         ENDIF
      CASE nKey == K_UP
         IF ::scrMovUp()
            ::obj_[ nObj,OBJ_TO_ROW ]--
         ENDIF
      CASE nKey == K_ENTER
         EXIT
      ENDCASE
      ::scrMove()
      ::scrStatus()
   ENDDO

   ::scrOrdObj()
   ::scrMsg()
   ::xRefresh := OBJ_REFRESH_ALL
   ::lEdited := .t.

   RETURN NIL

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrAddFld( nObj )
   LOCAL h_, w_, o_, v_

   DEFAULT nObj TO 0

   v_:= iif( nObj > 0, ::scrObj2Vv( ::obj_[ nObj ] ), ::scrVrbBlank( OBJ_O_FIELD ) )
   h_:= ::scrVrbHeaders( OBJ_O_FIELD )

   w_:= afill( array( len( h_ ) ), {|| .T. } )

   w_[ 2 ] := {| | VouchMenuM( 'MN_TYFLD' ) }
   w_[ 3 ] := {|v| v := oAchGet( 2 ), iif( v == 'D', !oCPut( 8 ), iif( v == 'L', !oCPut( 1 ), .t. ) ) }
   w_[ 4 ] := {|v| v := oAchGet( 2 ), iif( v <> 'N', !oCPut( 0 ), .t. ) }

   ::scrMsg( "ENTER: Starts Editing Current Selection.  CTRL_ENTER: When Done." )
   B_GETS HEADERS h_ VALUES v_ TITLE 'Configure Field' WHEN w_ INTO v_
   ::scrMsg()

   v_:= v_[ 1 ]
   v_[ 1 ] := alltrim( trim( v_[ 1 ] ) )
   IF empty( v_[ 1 ] )
      RETURN NIL
   ENDIF

   IF lastkey() <> K_ESC
      ::scrUpdateUndo()

      IF nObj == 0
         o_:= ::scrObjBlank()
      ELSE
         o_:= ::obj_[ nObj ]
      ENDIF
      o_[ OBJ_TYPE    ] := OBJ_O_FIELD

      ::scrVv2Obj( v_, o_ )

      o_[ OBJ_ROW     ] := iif( nObj == 0, ::nRowRep, o_[ OBJ_ROW    ] )
      o_[ OBJ_COL     ] := iif( nObj == 0, ::nColRep, o_[ OBJ_COL    ] )
      o_[ OBJ_TEXT    ] := padc( alltrim( v_[ 1 ] ), v_[ 3 ] )
      o_[ OBJ_TO_ROW  ] := iif( nObj == 0, ::nRowRep, o_[ OBJ_TO_ROW ] )
      o_[ OBJ_TO_COL  ] := iif( nObj == 0, ::nColRep, o_[ OBJ_COL    ] ) + v_[ 3 ] - 1

      IF nObj == 0
         aadd( ::obj_, o_ )
         nObj := len( ::obj_ )
      ELSE
         ::obj_[ nObj ] := o_
      ENDIF

      ::nObjSelected := 0
      ::xRefresh     := OBJ_REFRESH_LINE
      ::nMode        := 0
      ::lEdited      := .t.
   ENDIF

   IF nObj > 0
      ::scrOrdObj()
   ENDIF

   RETURN Self

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrGetProperty( nObj )
   LOCAL o_, v_, w_, h_

   o_:= ::obj_[ nObj ]

   SWITCH ::objType( nObj )

   CASE OBJ_O_BOX
      ::scrUpdateUndo()

      v_:= iif( nObj > 0, ::scrObj2Vv( ::obj_[ nObj ] ), ::scrVrbBlank( OBJ_O_BOX ) )
      h_:= ::scrVrbHeaders( OBJ_O_BOX )
      w_:= afill( array( len( h_ ) ), {|| .T. } )

      w_[ 2 ] := {| | VouchMenuM( 'MN_BOX'  ) }
      w_[ 3 ] := {| | VouchMenuM( 'MN_FILL' ) }

      ::scrMsg( "ENTER: Starts Editing Current Selection.  CTRL_ENTER: When Done." )
      B_GETS HEADERS h_ VALUES v_ TITLE 'Configure Field' WHEN w_ INTO v_
      ::scrMsg()

      ::scrVv2Obj( v_[ 1 ], o_ )
      EXIT

   CASE OBJ_O_TEXT
      ::scrUpdateUndo()

      v_:= iif( nObj > 0, ::scrObj2Vv( ::obj_[ nObj ] ), ::scrVrbBlank( OBJ_O_TEXT ) )
      h_:= ::scrVrbHeaders( OBJ_O_TEXT )
      w_:= afill( array( len( h_ ) ), {|| .T. } )

      ::scrMsg( "ENTER: Starts Editing Current Selection.  CTRL_ENTER: When Done." )
      B_GETS HEADERS h_ VALUES v_ TITLE 'Configure Field' WHEN w_ INTO v_
      ::scrMsg()

      ::scrVv2Obj( v_[ 1 ], o_ )
      EXIT

   CASE OBJ_O_FIELD
      ::scrAddFld( nObj )
      EXIT

   ENDSWITCH

   ::lEdited := .t.

   RETURN SELF

/*----------------------------------------------------------------------*/

METHOD hbCUIEditor:scrPreview()
   LOCAL nRows, nCols, a_, cColor, aScr, cPic
   LOCAL nMaxRows := 0
   LOCAL nMaxCols := 0
   LOCAL getlist  := {}
   LOCAL g_       := {}

   aeval( ::obj_, {|e_| nMaxRows := max( e_[ OBJ_TO_ROW ], nMaxRows ), nMaxCols := max( e_[ OBJ_TO_COL ], nMaxCols ) } )

   IF nMaxRows > 25
      nMaxRows++
   ELSE
      nMaxRows := 25
   ENDIF
   IF nMaxCols > 80
      nMaxCols++
   ELSE
      nMaxCols := 80
   ENDIF

   nRows := maxrow()
   nCols := maxcol()
   aScr  := VouchWndSave( 0,0,maxrow(),maxcol() )
   vstk_push()

   ::scrOrdObj()
   aeval( ::obj_, {|e_| iif( e_[ OBJ_TYPE ] == OBJ_O_FIELD, aadd( g_, VouchVrbBlank( e_ ) ), aadd( g_, NIL ) ) } )

   SetColor( "W/B" )
   SetCursor( 0 )
   SetMode( nMaxRows, nMaxCols )
   CLS

   FOR EACH a_ IN ::obj_
      cColor := VouchGetColor( a_[ OBJ_TYPE ], a_[ OBJ_COLOR ] )

      SWITCH a_[ OBJ_TYPE ]
      CASE OBJ_O_BOX
         DispBox( a_[ OBJ_ROW ]-1, a_[ OBJ_COL ]-1, a_[ OBJ_TO_ROW ]-1, a_[ OBJ_TO_COL ]-1, a_[ OBJ_BOX_SHAPE ] + iif( a_[ OBJ_PATTERN ] == "CLEAR", "", " " ), cColor )
         EXIT
      CASE OBJ_O_TEXT
         @ a_[ OBJ_ROW ]-1, a_[ OBJ_COL ]-1 SAY a_[ OBJ_TEXT ] COLOR cColor
         EXIT
      CASE OBJ_O_FIELD
         cPic := VouchGetPic( a_[ OBJ_F_TYPE ], a_[ OBJ_F_PIC ], a_[ OBJ_F_LEN ], a_[ OBJ_F_DEC ] )
         IF ! empty( cPic )
            @ a_[ OBJ_ROW ]-1, a_[ OBJ_COL ]-1 GET g_[ a_:__enumIndex() ] COLOR cColor PICTURE cPic
         ELSE
            @ a_[ OBJ_ROW ]-1, a_[ OBJ_COL ]-1 GET g_[ a_:__enumIndex() ] COLOR cColor
         ENDIF
         EXIT
      ENDSWITCH
   NEXT

   IF len( getlist ) > 0
      READ
   ELSE
      DO WHILE inkey() != K_ESC; ENDDO
   ENDIF

   SetMode( nRows + 1, nCols + 1 )
   VouchWndRest( aScr )
   vstk_pop()

   RETURN Self

/*----------------------------------------------------------------------*/

STATIC FUNCTION VouchGetPic( cType, cPic, nLen, nDec )
   LOCAL cP := ""

   IF cType == "N"
      cP := iif( nDec > 0, replicate( "9", nLen - nDec - 1 ) + "." + replicate( "9", nDec ), replicate( "9", nLen ) )
   ELSE
      IF left( cPic,1 ) == '"' .and. right( cPic,1 ) == '"'
         cP := substr( cPic, 2, len( cPic ) - 2 )
      ELSEIF left( cPic,1 ) == "'" .and. right( cPic,1 ) == "'"
         cP := substr( cPic, 2, len( cPic ) - 2 )
      ENDIF
   ENDIF

   RETURN cP

/*----------------------------------------------------------------------*/

STATIC FUNCTION VouchVrbBlank( o_ )

   SWITCH o_[ OBJ_F_TYPE ]
   CASE "C"
      RETURN space( o_[ OBJ_F_LEN ] )
   CASE "M"
      RETURN space( 10 )
   CASE "N"
      RETURN 0
   CASE "D"
      RETURN ctod( "" )
   CASE "L"
      RETURN .f.
   ENDSWITCH

   RETURN ""

/*----------------------------------------------------------------------*/

STATIC FUNCTION VouchGetColor( cType, cColor )

   IF left( cColor,1 ) == '"' .and. right( cColor,1 ) == '"'
      RETURN cColor
   ELSEIF left( cColor,1 ) == "'" .and. right( cColor,1 ) == "'"
      RETURN cColor
   ELSE
      IF cType == OBJ_O_BOX
         RETURN "W/B"
      ELSEIF cType == OBJ_O_TEXT
         RETURN "W/B"
      ELSEIF cType == OBJ_O_FIELD
         RETURN "N/W,GR+/BG"
      ENDIF
   ENDIF

   RETURN "W/B"

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#define CGO_POS                                   1
#define CGO_ROW                                   2
#define CGO_LSEL                                  3
#define CGO_LNUM                                  4
#define CGO_CH_                                   5
#define CGO_SEL_                                  6
#define CGO_EXE_                                  7
#define CGO_SCROL                                 8
#define CGO_LENSCR                                9


#define LEN_COL_STR                               20
#define LEN_VID_STK_ENTRY                         LEN_COL_STR + 3

/*----------------------------------------------------------------------*/

FUNCTION VouchInRange( v, r1, r2 )
   RETURN ( v >= r1 .AND. v <= r2 )

/*----------------------------------------------------------------------*/

FUNCTION pad_max( a_,lNum,max )
   LOCAL i := 1
   DEFAULT lNum TO .f.
   IF max == NIL
      max := 0
      aeval( a_, {|x| max := max( max,len( x ) )} )
   ENDIF
   aeval( a_, {|x| a_[ i ] := iif( lNum, str( i,3 ) + '  ', '' ) + pad( x,max ), i++ } )
   RETURN a_

/*----------------------------------------------------------------------*/

FUNCTION VouchInArray( v,a_ )
   RETURN( ascan( a_,{|e| e = v } ) > 0 )

//----------------------------------------------------------------------//

FUNCTION VouchAShrink( a_,n )
   IF n > 0
      adel( a_,n )
      asize( a_,len( a_ )-1 )
   ENDIF
   RETURN a_

//----------------------------------------------------------------------//

FUNCTION setGetAch( v_ )
   LOCAL lCrt
   THREAD STATIC sCrt := {}
   lCrt := sCrt
   IF hb_isArray( v_ )
      sCrt := v_
   ENDIF
   RETURN lCrt

/*----------------------------------------------------------------------*/

FUNCTION VouchWndSave( t, l, b, r )
   LOCAL wnd_,crs

   crs := mSetCursor( .f. )
   DEFAULT t TO 0, ;
           l TO 0, ;
           b TO maxrow(), ;
           r TO maxcol()

   wnd_:= { t, l, b, r, saveScreen( t,l,b,r ) }

   mSetCursor( crs )

   RETURN wnd_

//----------------------------------------------------------------------//

FUNCTION VouchWndRest( wnd_ )
   LOCAL crs, bError

   bError := errorblock( {|oErr| Break( oErr ) } )
   BEGIN SEQUENCE
      crs := mSetCursor( .f. )
      RestScreen( wnd_[1], wnd_[2], wnd_[3], wnd_[4], wnd_[5] )
      mSetCursor( crs )
   END
   errorblock( bError )

   RETURN NIL

//----------------------------------------------------------------------//

FUNCTION Wvt()
   RETURN .F.

/*----------------------------------------------------------------------*/

FUNCTION VouchGetArray( h_,vv_, sel_, nTop, nLft, nBtm, nRgt, title, bWhen_, bValid_, pic_, hlp, ord_ )
   LOCAL i, scr , nSel, maxL, mLen, nDiff, clr
   LOCAL nLenVrb, clr1, s, cTyp
   LOCAL nLastKey,cgo_, aScrol_,nLenMnu,pmt_:={}

   HB_SYMBOL_UNUSED( hlp )
   HB_SYMBOL_UNUSED( ord_ )

   IF h_== NIL .OR. valtype(h_)<>'A' .OR. vv_== NIL .OR. valtype(vv_)<>'A'
      RETURN {vv_, 0}
   ENDIF

   nLenVrb := 0
   aeval( vv_, {|e| cTyp := valtype( e ), nLenVrb := max( ;
                     iif( cTyp == 'C', len( e ), ;
                         iif( cTyp == 'N', 15, iif( cTyp == 'D', 8, 3 ) ) ), nLenVrb ) } )

   IF bWhen_ == NIL
      bWhen_:= afill( array( len( vv_) ), {|| .t. } )
      FOR i := 1 TO len( vv_ )
         s := h_[ i ]
         IF valtype( vv_[ i ] ) == 'L'
            bWhen_[ i ] := {|| VouchYN( s, oGet() ), .f. }
         ENDIF
      NEXT
   ENDIF

   IF bValid_ == NIL
      bValid_:= afill( array( len( vv_ ) ), {|| .t. } )
   ENDIF

   pmt_:={}
   aeval(h_,{|e,i| aadd( pmt_, e + " {"+xtos(vv_[i])+ "}" ) })

   //  decide maximum length of the largest prompt
   mLen := 0
   aeval( pmt_, {|x| mLen := max( mLen, len( x ) ) } )
   mLen := max( len( h_[ 1 ] ) + 2 + nLenVrb, mLen ) + 2

   IF nTop == NIL
      nTop := int( ( maxrow() - min( 3 + len( h_ ), maxrow() - 3 ) ) / 2 )
   ENDIF
   IF nBtm == NIL
      nBtm := min( nTop + len( h_ ) + 3, maxrow() - 3 )
   ENDIF

   IF nLft == NIL
      nLft := max( 4,int( ( maxcol() - min( 2+mLen, maxcol()-8 ) ) / 2 ) )
   ENDIF
   IF nRgt == NIL
      nRgt := nLft + mLen
      IF nRgt > maxcol() - 4
         nDiff := nRgt - (maxcol()-4)
         IF nLft - nDiff < 0
            nLft := 4
            nRgt := maxcol()-4
         ELSE
            nLft  := nLft - nDiff
            nRgt := nLft + mLen
         ENDIF
      ENDIF
   ENDIF

   IF title == NIL .OR. empty( title )
      title = "Untitled"
   ELSE
      title := alltrim( title )
   ENDIF
   title := padc( title, nRgt - nLft )
   title := { title, replicate( chr( 196 ), len( title ) + 2 ) }
   maxL  := len( h_[ 1 ] )
   sel_  := iif( sel_ == NIL, .t., sel_ )

   vstk_push()
   setcursor(0)

   scr := VouchWndSave( max( 0, nTop -1 ), max( 0, nLft-1 ), nBtm + 1, nRgt + 2, .f. )

   B_MSG title AT nTop, nLft TO nBtm, nRgt SHADOW
   clr := "W+/BG"
   setcolor( clr + "," + "+GR/B" + ",,," + "N" + substr( clr, at( "/", clr ) ) )
   aScrol_ := ScrolBarNew( nTop + 2, nRgt, nBtm, "gr+/b" )

   nLenMnu := len( pmt_ )
   clr1    := setColor()

   ScrolBarDisplay( aScrol_ )
   ScrolBarUpdate( aScrol_, 1, nLenMnu, .t. )

   cgo_:= { 1, 0, .f., .f., pmt_, sel_,/*exe_*/, aScrol_, nLenMnu }

   SetGetAch( vv_ )

   IF pic_ == NIL
      pic_:= array( len( vv_ ) )
      FOR i := 1 TO len( vv_ )
         cTyp := valtype( vv_[ i ] )
         pic_[ i ] := iif( cTyp == "C", "@S" + hb_ntos( nLenVrb ) + "K ", iif( cTyp == "N", "@Z 99999999.999", iif( cTyp == "L", "Y", "@ " ) ) )
      NEXT
   ENDIF

   DO WHILE .t.
      setColor( clr1 )

      pmt_:= {}
      aeval( h_, {|e,i| aadd( pmt_, e + " {" + xtos( vv_[ i ] ) + "}" ) } )
      cgo_[ CGO_CH_ ] := pmt_

      clear typeahead
      nSel := VouchAChoice( nTop + 3, nLft + 1, nBtm - 1, nRgt - 1, ;
                            cgo_[ CGO_CH_ ], cgo_[ CGO_SEL_ ], "VouchFunc1", ;
                            cgo_[ CGO_POS ], cgo_[ CGO_ROW ],/* oWin */, @nLastKey, cgo_ )

      IF  nLastKey == K_ENTER
         vv_[ nSel ]  := VouchGetChoice( vv_[ nSel ], nTop + cgo_[ CGO_ROW ] + 3, ;
                           nLft + maxL + 1, nRgt - 1, bWhen_[ nSel ], ;
                           bValid_[ nSel ], pic_[ nSel ] )

      ELSEIF nLastKey == K_F10
         EXIT
      ELSEIF nLastKey == K_ESC
         EXIT
      ELSEIF nLastKey == K_CTRL_ENTER
         EXIT
      ELSEIF nLastKey == K_CTRL_END
         EXIT
      ENDIF
   ENDDO

   vstk_pop()
   VouchWndRest(scr)

   RETURN{ vv_, nSel }

/*----------------------------------------------------------------------*/

FUNCTION VouchFunc1( mode, nElem, nRow, nKey, cgo_ )
   LOCAL ret := AC_CONT

   IF nKey <> 0
      ScrolBarUpdate( cgo_[CGO_SCROL], nElem, cgo_[CGO_LENSCR], .t. )
   ENDIF

   cgo_[CGO_POS] := nElem
   cgo_[CGO_ROW] := nRow

   DO CASE
   CASE mode == AC_IDLE
   CASE mode == AC_HITTOP
      //NannyBoo
   CASE mode == AC_HITBOTTOM
      //Charge
   CASE mode == AC_NOITEM
      ret := AC_ABORT
   OTHERWISE
      DO CASE
      CASE nKey == K_CTRL_END
         ret := AC_SELECT
      CASE nKey == K_ENTER
         ret := AC_SELECT
      CASE nKey == K_CTRL_ENTER
         ret := AC_SELECT
      CASE nKey == K_F10
         ret := AC_SELECT
      CASE nKey == K_ESC
         ret := AC_ABORT
      CASE nKey > 31 .AND. nKey < 123
         cgo_[CGO_POS] := scan_ff( cgo_[CGO_POS], cgo_[CGO_CH_], chr( nKey ), 3 )
         RETURN AC_ABORT
      ENDCASE
   ENDCASE

   RETURN ret

//----------------------------------------------------------------------//

STATIC FUNCTION scan_ff( elem, a_, c /*, nFrom */ )
   LOCAL na, nlen

   c := lower( substr( c,1,1 ) )
   nLen := len( c )
   IF( na := ascan( a_,{|e| lower( substr( ltrim( e ),1,nLen ) ) == c }, min( elem+1, len( a_ ) ) ) ) == 0
      na := ascan( a_,{|e| lower( substr( ltrim( e ),1,nlen ) ) == c },1,elem-1 )
   ENDIF

   RETURN iif( na == 0, elem, na )

//----------------------------------------------------------------------//

STATIC FUNCTION VouchGetChoice( vrb, row, col, e_col, whn, vld, pic )
   LOCAL scr, maxL, n_vrb, dec, r, c, r1, c1, crs, clr
   LOCAL type := valtype( vrb )
   LOCAL getlist := {}

   IF type == "N"
      n_vrb := str( vrb )
      dec   := at( ".", n_vrb )
      IF pic == NIL
         IF dec > 0
            pic := replicate( "9", maxL -( maxL - dec ) - 1 ) + "." + replicate( "9", maxL - dec )
         ELSE
            pic := replicate( "9", maxL )
         ENDIF
      ENDIF
   ELSEIF type == "D"
      pic := ""
   ELSEIF type == "L"
      pic := "Y"
   ELSEIF type == "C"
      maxL := len( vrb )
      pic  := "@K"
      IF ( maxL + col ) > e_col
         pic += "S" + ltrim( str( e_col - col ) )
      ENDIF
   ENDIF

   r  := row
   c  := col
   r1 := r
   c1 := e_col

   clr := SetColor()
   scr := VouchWndSave( r-1, c-1, r1, c1 )

   @ r, c clear TO r1, c1

   crs := setcursor( 1 )
   @ r, c+1 get vrb when whn() valid vld() picture pic
   atail( getlist ):cargo := { whn,vld }
   read

   setcursor( crs )
   SetColor( clr )
   VouchWndRest( scr )

   RETURN vrb

//----------------------------------------------------------------------//

STATIC FUNCTION ScrolBarUpdate()
   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION ScrolBarDisplay()
   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION ScrolBarNew()
   RETURN NIL

/*----------------------------------------------------------------------*/

STATIC FUNCTION whn()
   RETURN eval( getActive():cargo[1] )

//----------------------------------------------------------------------//

STATIC FUNCTION vld()
   RETURN eval( getActive():cargo[2] )

//----------------------------------------------------------------------//

FUNCTION oAchGet( n )
   RETURN setGetAch()[n]

//----------------------------------------------------------------------//

STATIC FUNCTION oAchPut( n,v )
   setGetAch()[n] := v
   RETURN .t.

//----------------------------------------------------------------------//

FUNCTION oCPut( v )
   getactive():varPut( v )
   RETURN .t.

//----------------------------------------------------------------------//

STATIC FUNCTION oGet()
   RETURN getActive():varGet()

//----------------------------------------------------------------------//

FUNCTION GetCrtCargoSlots()
   RETURN { .f.,.f.,.f.,.f.,.f.,.f.,.f.,.f.,.f.,.f. }

/*----------------------------------------------------------------------*/

FUNCTION xtos( x )
   LOCAL type := valtype( x )
   DO CASE
   CASE type == 'C'
      RETURN alltrim( x )
   CASE type == 'D'
      RETURN dtoc( x )
   CASE type == 'L'
      RETURN iif( x, 'Y', 'N' )
   CASE type == 'N'
      RETURN ltrim( str( x ) )
   ENDCASE
   RETURN ""

//----------------------------------------------------------------------//

FUNCTION VouchRgb( nR, nG, nB )
   RETURN ( nR +( nG * 256 ) +( nB * 256 * 256 ) )

//---------------------------------------------------------------------//

FUNCTION VouchYN( msg, nInit )
   LOCAL g := getactive(), sel

   msg  := iif( msg==NIL,'',msg )
   nInit := iif( nInit==NIL,1,iif( valtype( nInit )=='N',nInit,iif( nInit,1,2 ) ) )

   B_MSG msg CHOOSE 'Yes','No ' TRIGGER {1,1} INITIAL nInit ;
   RESTORE SHADOW AT g:row - 3, g:col INTO sel

   IF g <> NIL
      g:varPut( iif( sel == 1, .t., .f. ) )
   ENDIF

   RETURN sel == 1

//----------------------------------------------------------------------//

FUNCTION VouchMenuMM( mnu_,nInit,msg,lExact,aSel )
   LOCAL n, i, t, m_:={}

   DEFAULT nInit  TO getActive():varGet()
   DEFAULT msg    TO 'Select an Option'
   DEFAULT lExact TO .f.
   DEFAULT aSel   TO {}

   aSel := asize( aSel, len( mnu_ ) )
   FOR i := 1 TO len( mnu_ )
      DEFAULT aSel[ i ] TO .t.
   NEXT

   aeval( mnu_,{|e_| aadd( m_,e_[ 1 ] ) } )

   IF( t := valtype( nInit ) == 'C' )
      //nInit := iif( lExact, nInit, trim( nInit ) )
   ENDIF

   n := max( 1, ascan( mnu_, {|e_| ;
      iif( t, iif( lExact, nInit, trim( nInit ) ) $ e_[ 2 ], nInit = e_[ 2 ] )  } ) )

   B_MSG msg CHOOSE m_ INITIAL n SELECTABLES aSel RESTORE SHADOW AT row()-3,col() WVT .T. INTO n
   n := max( 1,n )

   getActive():varPut( iif( t,pad( mnu_[n,2],len( nInit ) ),mnu_[n,2] ) )

   RETURN .f.

//----------------------------------------------------------------------//

FUNCTION VouchMenuM( id,nInit,msg )
   LOCAL n, m_:={},t, mnu_

   DEFAULT msg   TO 'Select'
   DEFAULT nInit TO getActive():varGet()

   mnu_:={}
   DO CASE
   CASE id == "MN_TYFLD"
      aadd( mnu_, { "Character", "C" } )
      aadd( mnu_, { "Numeric"  , "N" } )
      aadd( mnu_, { "Date"     , "D" } )
      aadd( mnu_, { "Logical"  , "L" } )

   CASE id == "MN_BOX"
      aadd( mnu_, { "B_SINGLE"        , B_SINGLE        } )
      aadd( mnu_, { "B_DOUBLE"        , B_DOUBLE        } )
      aadd( mnu_, { "B_SINGLE_DOUBLE" , B_SINGLE_DOUBLE } )
      aadd( mnu_, { "B_DOUBLE_SINGLE" , B_DOUBLE_SINGLE } )

   CASE id == "MN_FILL"
      aadd( mnu_, { "Clear" , "CLEAR"  } )
      aadd( mnu_, { "Filled", "FILLED" } )

   ENDCASE

   aeval( mnu_,{|e_| aadd( m_,e_[ 1 ] ) } )
   t := valtype( nInit ) == 'C'
   n := max( 1, ascan( mnu_, {|e_| iif( t, trim( nInit ) $ e_[ 2 ], nInit == e_[ 2 ] ) } ) )

   B_MSG msg CHOOSE m_ INITIAL n INTO n RESTORE SHADOW AT row()-3,col() WVT .T.
   n := max( 1,n )

   getActive():varPut( mnu_[n,2] )

   RETURN .f.   //  Note, because the FUNCTION is used IN when clause

//----------------------------------------------------------------------//

FUNCTION vstk_push()
   s_vid_stk := chr( set( _SET_CURSOR ) ) + ;
                        chr( row() ) + chr( col() ) + ;
                        pad( setcolor(), LEN_COL_STR ) + ;
                        s_vid_stk
   RETURN NIL

//----------------------------------------------------------------------//

FUNCTION vstk_pop()
   IF len( s_vid_stk ) > 0
      setcursor( asc( substr( s_vid_stk, 1, 1 ) ) )
      //@ asc( substr( s_vid_stk, 2, 1 ) ), asc( substr( s_vid_stk, 3, 1 ) ) SAY ""
      devpos( asc( substr( s_vid_stk, 2, 1 ) ), asc( substr( s_vid_stk, 3, 1 ) ) )
      setcolor( substr( s_vid_stk, 4, LEN_COL_STR ) )
      s_vid_stk := substr( s_vid_stk, LEN_VID_STK_ENTRY + 1 )
   ENDIF
   RETURN NIL

//----------------------------------------------------------------------//

FUNCTION VouchMsgBox(r1, c1, r2, c2, width, depth, msg_, msgClr, ;
      ch_, chClr, wait, restore, paste, shadow, trg_, sel, lSelect_, abr, ;
      lSlctns, lLeftRight, center, tagged_,lNumeric,help,exe_,num_,;
      lNoXpp, oWin, cIcon, lWvt, nAlign )

   LOCAL msgLen := 0, chLen := 0, maxLen, pmtWidth, xRet:= NIL
   LOCAL boxWide, boxDeep, oldCur, oldClr, oldScr, oldR, oldC, tBoxDeep
   LOCAL i, oGet, oVal, gap, mCrs,n,nLastKey, cr1
   LOCAL nLenScrol, nMsg, clr, aScrolbar
   LOCAL nSlctns_:={}, dd_:={}, cgo_:={}

   HB_SYMBOL_UNUSED( trg_  )
   HB_SYMBOL_UNUSED( help  )
   HB_SYMBOL_UNUSED( cIcon )
   HB_SYMBOL_UNUSED( lWvt  )

   DEFAULT lNoXpp     TO .f.
   DEFAULT nAlign     TO 2    // only FOR wvt . center

   DEFAULT ch_        TO {}
   DEFAULT restore    TO .f.

   DEFAULT r1         TO row()
   DEFAULT c1         TO col()
   DEFAULT msg_       TO {}
   DEFAULT ch_        TO {}
   DEFAULT lSelect_   TO {}
   DEFAULT msgClr     TO 1 //"W+/BG"
   DEFAULT chClr      TO 3 //"+W/B"
   DEFAULT restore    TO .f.
   DEFAULT paste      TO .f.
   DEFAULT shadow     TO .f.
   DEFAULT abr        TO .f.
   DEFAULT lSlctns    TO .f.
   DEFAULT lLeftRight TO .f.
   DEFAULT center     TO .f.
   DEFAULT tagged_    TO {}
   DEFAULT lNumeric   TO .f.
   DEFAULT num_       TO {}

   oGet := iif( paste, getactive(), oGet )

   IF ( len( msg_) > 0) .AND. (valtype (msg_[1]) == "A" )
      msg_ := aclone( msg_[ 1 ] )
   ENDIF
   IF ( len( msg_ ) > 0 ) .AND. ( msg_[1] == NIL )
      msg_:= {}
   ENDIF
   IF ( len( ch_ ) > 0 ) .AND. ( valtype( ch_[ 1 ] ) = "A" )
      ch_:= aclone( ch_[ 1 ] )
   ENDIF
   IF len( msg_ ) == 0 .AND. len( ch_ ) == 0
      RETURN .f.
   ENDIF

   IF lSlctns
      IF lNumeric
         IF empty(num_)
            FOR i := 1 TO len (ch_)
               ch_[i] := '    '+ch_[i]
            NEXT
         ELSE
            FOR i := 1 TO len (ch_)
               IF (n := ascan(num_,i))==0
                  ch_[ i ] := '    '+ch_[i]
               ELSE
                  ch_[ i ] := pad( hb_ntos( n ), 4 ) + ch_[ i ]
               ENDIF
            NEXT
         ENDIF
      ELSE
         FOR i := 1 TO len( ch_ )
            ch_[ i ] := iif( empty( tagged_ ),'  ', iif( tagged_[ i ], CHECKMARK + ' ', '  ' ) ) + ch_[ i ]
         NEXT
      ENDIF
   ENDIF

   aeval( msg_, {|s| msgLen := max( msgLen, len( s ) ) } )
   aeval( ch_,  {|s| chLen  := max( chLen,  len( s ) ) } )
   maxlen := max( msgLen, chLen )
   aeval( ch_, {|s,i| s := s, ch_[ i ] := pad( ch_[ i ], maxLen ) } )

   IF empty( lSelect_ )
      lSelect_:= {}
      aeval( ch_,  {|s| aadd( lSelect_, iif( empty( s ), .f., .t. ) ) } )
   ELSE
      aeval( ch_, {|s,i| lSelect_[ i ] := iif( empty( s ), .f., lSelect_[ i ] ) } )
   ENDIF
   IF ascan( lSelect_, {|e| e } ) == 0
      IF len(ch_) > 0
         RETURN 0
      ENDIF
   ENDIF

   nMsg := Len( msg_ )
//   nOff := iif( nMsg == 1, 0,  1 )

   boxDeep  := iif( len( msg_ )=0,0,len( msg_ )+1 ) + iif( len( ch_  )=0,0,len( ch_  )+1 )
   tBoxDeep := boxDeep
   boxDeep  := min( boxDeep, maxrow() - r1 )
   boxWide  := max( msgLen, chLen ) + 3

   DEFAULT r2 TO r1 + iif( depth = NIL, boxDeep, depth )
   DEFAULT c2 TO c1 + iif( width = NIL, boxWide, width )

   IF center
      r1 := int( ( maxrow() - tBoxDeep ) / 2 )
      r1 := iif( r1 <= 0,1,r1 )
      r2 := r1 + tBoxDeep
      IF r2 > maxrow() - 1
         r2 := maxrow() - 1
      ENDIF

      c1 := int( ( maxcol() - boxWide ) / 2 )
      c1 := iif( c1 < 0, 3, c1 )
      c2 := c1 + boxWide
      IF c2 > maxcol()-3
         c2 := maxcol()-3
      ENDIF
   ELSE
      IF r2 <= r1
         r2 := maxrow()
      ENDIF
      IF c2 <= c1
         c2 := maxcol()
      ENDIF
      IF r2 > maxrow()
         gap := (r2 - maxrow () )
         r2 := r2 - gap
         r1 := r1 - gap
      ENDIF
      IF c2 > maxcol() - 4
         gap := ( c2 - maxcol() + 4 )
         c2  := c2 - gap
         c1  := c1 - gap
      ENDIF
   ENDIF

   IF shadow
      IF r2 == maxrow()
         r2 := r2 - 1
      ENDIF
      IF c2 == maxcol ()
         c2 := c2 - 2
         c1 := c1 - 2
      ENDIF
   ENDIF

   IF restore
      IF shadow
         oldScr := VouchWndSave( max( 0,r1-2 ), max( 0,c1-2 ), r2 + 1 , c2 + 3 )
      ELSE
         oldScr := VouchWndSave( r1, c1, r2, c2 )
      ENDIF
   ENDIF

   IF sel == NIL .OR. sel < 1 .OR. sel > len( ch_ )
      sel := 1
   ENDIF

   vstk_push()

   oldR   := row()
   oldC   := col()
   oldCur := setcursor( 0 )
   oldClr := SetColor( "W+/BG" )
   mCrs   := mSetCursor( .f. )

   dispbox( r1, c1, r2, c2, B_SLIM + " " )
   IF shadow
      VouchShadow( r1, c1, r2, c2 )
   ENDIF

   FOR i = 1 TO min( len( msg_ ), r2 - ( r1 + 1 ) )
      devpos( r1 + i, c1 + 2 )
      devout( pad( msg_[ i ], c2 - ( c1 + 3 ) ) )
   NEXT i
   mSetCursor( mCrs )

   clr := "W+/BG"
   IF len( ch_ ) > 0
      IF nMsg > 0
         mCrs := mSetCursor(.f.)
         devpos( r1 + 1 + nMsg, c1 + 1 )
         devout( replicate( chr( 196 ), c2 - ( c1 + 1 ) ) )
         mSetCursor( mCrs )
         cr1 := r1 + nMsg + 2
      ELSE
         cr1 := r1 + 1
      ENDIF

      setcolor( clr + "," + "+W/B" + ",,," + "N" + substr( clr, at( "/", clr ) ) )
      aScrolBar := ScrolBarNew( cr1 - 1, c2, r2 /*, colorGet( C_SCROLL ) )*/ )

      nLenScrol := len( ch_ )
      pmtWidth  := c2 - c1 - 3
      aeval( ch_, {|e,i| ch_[ i ] := pad( e, pmtWidth ) } )

      ScrolBarDisplay( aScrolBar )
      ScrolBarUpdate( aScrolBar, sel, nLenScrol, .t. )

      cgo_:= { sel, 0, lSlctns, lNumeric, ch_, lSelect_, exe_, aScrolbar, nLenScrol }

      DO WHILE .t.
         sel := VouchAChoice( cr1, c1 + 2, r2 - 1, c1 + (c2 - c1) - 2, ;
                              cgo_[CGO_CH_], cgo_[CGO_SEL_], "VouchFunc2", ;
                              cgo_[CGO_POS], cgo_[CGO_ROW], oWin, ;
                              @nLastKey, @cgo_ )
         IF !lSlctns
            EXIT
         ELSE
            IF nLastKey == K_ESC ;
                           .OR. nLastKey == K_CTRL_ENTER ;
                           .OR. nLastKey == K_ALT_F7
               EXIT
            ENDIF
         ENDIF
      ENDDO

   ELSEIF valtype (wait) = "N"
      sel := inkey (wait)
   ENDIF

   IF paste
      IF valtype( oGet:varGet() ) == "C"
         oVal := oGet:varGet()
         oGet:varPut( pad( ch_[ iif( sel = 0,1,sel ) ], len( oVal ) ) )
         oGet:display()
      ENDIF
   ENDIF

   IF restore
      VouchWndRest( oldScr )
      oldscr := NIL
   ENDIF

   IF lSlctns
      IF !lNumeric
         FOR i = 1 TO len( cgo_[CGO_CH_] )
            IF substr( cgo_[CGO_CH_,i], 1, 1) == CHECKMARK
               aadd( nSlctns_,i )
            ENDIF
         NEXT
      ELSE
         FOR i := 1 TO len(cgo_[CGO_CH_])
            IF val( left( cgo_[CGO_CH_,i],4 ) )>0
               aadd( dd_,{val( left( cgo_[CGO_CH_,i],4 ) ),i} )
            ENDIF
         NEXT
         IF !empty(dd_)
            asort(dd_,,,{|e_,f_| e_[1]<f_[1] })
            aeval(dd_,{|e_| aadd(nSlctns_,e_[2]) })
         ENDIF
      ENDIF
   ENDIF

   setcursor( oldCur )
   devpos( oldR,oldC )

   vstk_pop()
   setcolor( oldClr )

   RETURN iif( lSlctns, nSlctns_, sel )

//----------------------------------------------------------------------//

FUNCTION VouchFunc2( nMode, nElem, nRel, nKey, cgo_ )
   LOCAL n, i, nn, s

   IF nKey <> 0 .AND. nKey <> K_MOUSEMOVE
      ScrolBarUpdate( cgo_[ CGO_SCROL ], nElem, cgo_[ CGO_LENSCR ], .t. )
      IF cgo_[ CGO_EXE_ ] <> NIL
         eval( cgo_[ CGO_EXE_,nElem ] )
      ENDIF
   ENDIF

   cgo_[CGO_POS] := nElem
   cgo_[CGO_ROW] := nRel

   DO CASE
   CASE nKey == K_F1
      //  help()
      RETURN AC_CONT
   CASE nmode = AC_IDLE
      RETURN AC_CONT
   CASE nmode = AC_HITTOP
      KEYBOARD CHR( K_CTRL_PGDN )
      RETURN AC_CONT
   CASE nmode = AC_HITBOTTOM
      KEYBOARD CHR( K_CTRL_PGUP )
      RETURN AC_CONT
   CASE nmode = AC_EXCEPT
      DO CASE
      CASE nKey == K_F1
         //  help()
         RETURN AC_CONT
      CASE nKey = K_ESC
         RETURN AC_ABORT
      CASE nKey == K_F9      // TAG ALL
         IF cgo_[CGO_LSEL]
            IF cgo_[CGO_LNUM]
               FOR i := 1 TO len( cgo_[CGO_CH_] )
                  IF cgo_[CGO_SEL_]
                     cgo_[CGO_CH_,i] := chr( 251 ) + substr( cgo_[ CGO_CH_,i ], 2 )
                  ENDIF
               NEXT
               RETURN AC_ABORT
            ELSE
               RETURN AC_CONT
            ENDIF
         ELSE
            RETURN AC_CONT
         ENDIF
      CASE nKey == K_F10      // UnTAG ALL
         IF cgo_[CGO_LSEL]
            FOR i := 1 TO len( cgo_[CGO_CH_] )
               cgo_[CGO_CH_,i] := " "+substr( cgo_[CGO_CH_,i],2 )
            NEXT
            RETURN AC_ABORT
         ELSE
            RETURN AC_CONT
         ENDIF

      CASE nKey = K_ENTER
         IF cgo_[CGO_LSEL]
            IF !cgo_[CGO_LNUM]
               cgo_[CGO_CH_,cgo_[CGO_POS]] := iif( substr( cgo_[CGO_CH_,cgo_[CGO_POS]],1,1 )==CHECKMARK, ;
                             " ",CHECKMARK )+substr( cgo_[CGO_CH_,cgo_[CGO_POS]],2 )
               cgo_[CGO_POS] := min( cgo_[CGO_POS]+1,len( cgo_[CGO_CH_] ) )
               RETURN AC_ABORT
            ELSE
               IF( n:=val( substr( cgo_[CGO_CH_,cgo_[CGO_POS]],1,4 ) ) )>0
                  cgo_[CGO_CH_,cgo_[CGO_POS]] := "    "+substr( cgo_[CGO_CH_,cgo_[CGO_POS]],5 )
                  cgo_[CGO_POS] := min( cgo_[CGO_POS]+1,len( cgo_[CGO_CH_] ) )
                  FOR i := 1 TO len( cgo_[CGO_CH_] )
                     IF( nn := val( left( cgo_[CGO_CH_,i],4 ) ) )>0
                        IF nn > n
                           nn := nn - 1
                           s := iif( nn > 0,pad( hb_ntos( nn ),4 ),"    " )
                           cgo_[CGO_CH_,i] := s + substr( cgo_[CGO_CH_,i],5 )
                        ENDIF
                     ENDIF
                  NEXT
               ELSE
                  nn := 0
                  n  := 0
                  aeval( cgo_[CGO_CH_], {|e| n := val( left( e,4 ) ), nn := iif( n>nn,n,nn ) } )
                  cgo_[CGO_CH_,cgo_[CGO_POS]] := pad( hb_ntos( nn+1 ),4 ) + substr( cgo_[CGO_CH_,cgo_[CGO_POS]],5 )
                  cgo_[CGO_POS] := min( cgo_[CGO_POS]+1, len( cgo_[CGO_CH_] ) )
               ENDIF
               RETURN AC_ABORT
            ENDIF
         ELSE
            RETURN AC_SELECT
         ENDIF

      CASE nKey = K_CTRL_ENTER
         RETURN AC_SELECT
      CASE nKey = HB_K_RESIZE
         RETURN AC_CONT
      OTHERWISE
         IF cgo_[CGO_LSEL]
            cgo_[CGO_POS] := scan_f( cgo_[CGO_POS], cgo_[CGO_CH_], nKey, iif( !cgo_[CGO_LNUM],3,5 ) )
            RETURN AC_ABORT
         ELSE
            RETURN AC_GOTO
         ENDIF
      ENDCASE
   CASE nmode = AC_NOITEM
      RETURN AC_ABORT
   OTHERWISE
      RETURN AC_GOTO
   ENDCASE

   RETURN AC_CONT

//----------------------------------------------------------------------//

STATIC FUNCTION scan_f( elem, a_, key, nFrom )
   LOCAL n := elem, na, c

   c := lower( chr( key ) )
   na := ascan( a_, {|e| lower( substr( e, nFrom, 1 ) ) == c }, min( elem + 1, len( a_ ) ) )
   IF na == 0
      na := ascan( a_,{|e| lower( substr( e, nFrom, 1 ) ) == c },1,elem-1 )
   ENDIF
   IF na <> 0
      n := na
   ENDIF
   RETURN n

//----------------------------------------------------------------------//

#define BLACK                                     0
#define WHITE                                     7
#define DK_GRAY                                   8

#define ATTR_CONV( FORE, BACK )                   (BACK)*16+(FORE)
#define COL_SHADOW_ATTRIBUTE                      ATTR_CONV( DK_GRAY, BLACK )

//----------------------------------------------------------------------//

STATIC FUNCTION VouchShadow( t, l, b, r )
   IF r < maxcol() - 1 .AND. b < maxrow()
      sha_attr( b + 1, l + 1, b + 1, r + 1, COL_SHADOW_ATTRIBUTE )
      sha_attr( t + 1, r + 1, b + 1, r + 2, COL_SHADOW_ATTRIBUTE )
   ENDIF
   RETURN NIL

//----------------------------------------------------------------------//

STATIC FUNCTION sha_attr( t, l, b, r, new_attr )
   LOCAL old_scr_area, new_scr_area, i

   old_scr_area := savescreen( t, l, b, r )
   new_scr_area = ""

   FOR i = 1 TO len( old_scr_area ) STEP 2
      new_scr_area := new_scr_area + substr( old_scr_area, i, 1 ) + chr( new_attr )
   NEXT

   restscreen( t, l, b, r, new_scr_area )
   RETURN NIL

//----------------------------------------------------------------------//

FUNCTION VouchGetSome( msg, vrb, pass, pic, set_, wh, vl, nLastKey )
   LOCAL screen, l, nMaxLen, nLenMsg, nLenVrb, clr, r
   LOCAL t       := maxrow()-7
   LOCAL b       := maxrow()-3
   LOCAL GetList := {}
   LOCAL dType   := valtype( vrb )

   HB_SYMBOL_UNUSED( set_ )
   HB_SYMBOL_UNUSED( nLastKey )

   DEFAULT msg  TO 'Please Enter Required Value'
   DEFAULT wh   TO {|| .t. }
   DEFAULT vl   TO {|| .t. }
   DEFAULT pass TO .f.
   DEFAULT pic  TO iif( dType == 'Y', 'Y', '@K ' )

   clr := SetColor()

   nMaxLen := maxcol() - 7
   nLenMsg := len( msg )

   DO CASE
   CASE dType == 'D' ; nLenVrb := 8
   CASE dType == 'N' ; nLenVrb := 17
   CASE dType == 'C' ; nLenVrb := len( vrb )
   CASE dType == 'L' ; nLenVrb := 1
   ENDCASE

   IF nLenMsg + nLenVrb > nMaxLen   //  Only when vrb type c will be asked
      nLenVrb := nMaxLen - nLenMsg - 7
      pic     := substr( pic, 1, 1 ) + 'S' + hb_ntos( nLenVrb ) + substr( pic, 2 )
   ENDIF

   pic := iif( dType == 'N', '@Z 99999999999999.99', pic )
   l   := ( ( maxcol() + 1 - ( nLenMsg + nLenVrb + 7 ) ) / 2 )
   r   := l + nLenMsg + nLenVrb + 6

   SetColor( 'W+/RB,GR+/BG,,,W+/BG' )
   vstk_push()
   screen := VouchWndSave( t-1, l-4, b+2, r+3 )

   dispbox( t, l, b, r, B_SLIM + " " )
   VouchShadow( t, l, b, r )

   @ t+2, l+3 SAY msg GET vrb PICTURE pic  WHEN eval(wh) VALID eval(vl)
   setCursor(1)
   read

   VouchWndRest( screen )
   vstk_pop()
   SetColor( clr )

   RETURN vrb

/*----------------------------------------------------------------------*/

FUNCTION help( cToken )
   LOCAL nKey, nRows, nCols, aScr, lSetMode

   nRows := maxrow()
   nCols := maxcol()
   aScr  := VouchWndSave( 0, 0, maxrow(), maxcol() )
   lSetMode := nRows <> 27 .or. nCols <> 79
   Vstk_push()

   IF lSetMode
      SetMode( 28,80 )
   ENDIF

   SetCursor( 0 )
   SetColor( "W/B" )

   DispBegin()
   CLS
   DispHelp( cToken )
   DispEnd()

   DO WHILE .t.
      nKey := inkey()
      IF     nKey == K_ESC
         SetHelpStr( "" )
         EXIT
      ELSEIF nKey == 49
         SetHelpStr( "Keys" )
         __keyboard( chr( K_F1 ) )
         EXIT
      ELSEIF nKey == 50
         SetHelpStr( "General-1" )
         __keyboard( chr( K_F1 ) )
         EXIT
      ELSEIF nKey == 51
         SetHelpStr( "General-2" )
         __keyboard( chr( K_F1 ) )
         EXIT
      ELSEIF nKey == 52
         SetHelpStr( "General-3" )
         __keyboard( chr( K_F1 ) )
         EXIT
      ELSEIF nKey == 53
         SetHelpStr( "About" )
         __keyboard( chr( K_F1 ) )
         EXIT
      ENDIF
   ENDDO

   IF lSetMode
      SetMode( nRows + 1, nCols + 1 )
   ENDIF
   VouchWndRest( aScr )
   Vstk_pop()

   RETURN NIL

/*----------------------------------------------------------------------*/

FUNCTION SetHelpStr( cStr )
   LOCAL o_str
   STATIC s_str := ""

   o_str := s_str
   IF hb_isChar( cStr )
      s_str := cStr
   ENDIF

   RETURN o_str

/*----------------------------------------------------------------------*/

STATIC FUNCTION DispHelp( cToken )

   DEFAULT cToken TO SetHelpStr()
   IF empty( cToken )
      cToken := "KEYS"
   ENDIF

   SWITCH Upper( cToken )

   CASE "KEYS"
      /* HB_SCREEN_BEGINS <Keys> */

      /// 1 3 C 76 0
      @ 1, 2     SAY "                                     Keys                                   " COLOR "N/W*"
      /// 2 3 C 12 0
      @ 3, 10    SAY "This screen."
      /// 3 3 C 7 0
      @ 3, 2     SAY "F1     "                                                                      COLOR "GR+/B"
      /// 4 3 C 7 0
      @ 4, 2     SAY "F4     "                                                                      COLOR "GR+/B"
      /// 5 3 C 59 0
      @ 4, 10    SAY "Properties of current object in a selectable/editable list."
      /// 6 3 C 64 0
      @ 5, 10    SAY "Edit current object: Text-no action, Box-resize action, Field-F4"
      /// 7 3 C 7 0
      @ 5, 2     SAY "F5     "                                                                      COLOR "GR+/B"
      /// 8 3 C 7 0
      @ 6, 2     SAY "F6     "                                                                      COLOR "GR+/B"
      /// 9 3 C 40 0
      @ 6, 10    SAY "Selects current object (Box/Field/Text)."
      /// 10 3 C 7 0
      @ 7, 2     SAY "F7     "                                                                      COLOR "GR+/B"
      /// 11 3 C 39 0
      @ 7, 10    SAY "Copies current object (Box/Field/Text)."
      /// 12 3 C 48 0
      @ 8, 10    SAY "Pastes copied object at current cursor position."
      /// 13 3 C 7 0
      @ 8, 2     SAY "F8     "                                                                      COLOR "GR+/B"
      /// 14 3 C 7 0
      @ 9, 2     SAY "F9     "                                                                      COLOR "GR+/B"
      /// 15 3 C 32 0
      @ 9, 10    SAY "Starts to define new box object."
      /// 16 3 C 25 0
      @ 10, 10   SAY "Defines a new GET object."
      /// 17 3 C 7 0
      @ 10, 2    SAY "F10    "                                                                      COLOR "GR+/B"
      /// 18 3 C 63 0
      @ 12, 10   SAY "Deletes current object (Box/Field) or current character (Text)."
      /// 19 3 C 7 0
      @ 12, 2    SAY "Del    "                                                                      COLOR "GR+/B"
      /// 20 3 C 36 0
      @ 13, 10   SAY "Cursor is positioned at column zero."
      /// 21 3 C 7 0
      @ 13, 2    SAY "Home   "                                                                      COLOR "GR+/B"
      /// 22 3 C 63 0
      @ 14, 10   SAY "Cursor is positioned at the next to last column of last object."
      /// 23 3 C 7 0
      @ 14, 2    SAY "End    "                                                                      COLOR "GR+/B"
      /// 24 3 C 54 0
      @ 16, 10   SAY "Inserts blank row, all objects are moved down one row."
      /// 25 3 C 7 0
      @ 16, 2    SAY "Alt_N  "                                                                      COLOR "GR+/B"
      /// 26 3 C 66 0
      @ 17, 10   SAY "Deletes objects on current row, next objects are moved up one row."
      /// 27 3 C 7 0
      @ 17, 2    SAY "Alt_O  "                                                                      COLOR "GR+/B"
      /// 28 3 C 7 0
      @ 18, 2    SAY "Alt_G  "                                                                      COLOR "GR+/B"
      /// 29 3 C 53 0
      @ 18, 10   SAY "Re-order GETS. This is different than creation order."
      /// 30 3 C 7 0
      @ 20, 2    SAY "Ctrl_F6"                                                                      COLOR "GR+/B"
      /// 31 3 C 23 0
      @ 20, 10   SAY "Begins block selection."
      /// 32 3 C 36 0
      @ 21, 10   SAY "Copy selected block at new location."
      /// 33 3 C 7 0
      @ 21, 2    SAY "Ctrl_F7"                                                                      COLOR "GR+/B"
      /// 34 3 C 7 0
      @ 22, 63   SAY "Alt_Z  "                                                                      COLOR "GR+/B"
      /// 35 3 C 45 0
      @ 22, 10   SAY "Cut and paste selected block at new location."
      /// 36 3 C 4 0
      @ 22, 71   SAY "Undo"
      /// 37 3 C 7 0
      @ 22, 2    SAY "Ctrl_F8"                                                                      COLOR "GR+/B"
      /// 38 3 C 7 0
      @ 24, 63   SAY "Alt_P  "                                                                      COLOR "GR+/B"
      /// 39 3 C 7 0
      @ 24, 71   SAY "Preview"
      /// 40 3 C 7 0
      @ 24, 33   SAY "Alt_L  "                                                                      COLOR "GR+/B"
      /// 41 3 C 20 0
      @ 24, 41   SAY "Load another screen."
      /// 42 3 C 7 0
      @ 24, 2    SAY "Alt_S  "                                                                      COLOR "GR+/B"
      /// 43 3 C 21 0
      @ 24, 10   SAY "Save designed screen."
      /// 44 3 C 76 0
      @ 26, 2    SAY " ESC-Designer  1-Keys  2-General  3-ListedInputs  4-BlockSelection  5-About " COLOR "N/W*"

      /* HB_SCREEN_ENDS <Keys> */
      EXIT

   CASE "GENERAL-1"
      /* HB_SCREEN_BEGINS <General-1> */

      /// 1 3 C 76 0
      @ 1, 2     SAY "                                   General                                  " COLOR "N/W*"
      /// 2 3 C 76 0
      @ 3, 2     SAY "hbCuiEd is a fixed-coordinated, character based screen designer which allows"
      /// 3 3 C 76 0
      @ 4, 2     SAY "to arrange Harbour's GT oriented objects in visual interaction and saves the"
      /// 4 3 C 76 0
      @ 5, 2     SAY "results as Harbour source code (with some meta info) directly into the .PRG "
      /// 5 3 C 73 0
      @ 6, 2     SAY "file ready to be compiled and linked. Thus generated forms can be edited "
      /// 6 3 C 76 0
      @ 7, 2     SAY "either directly in the source file or through this tool which allows two-way"
      /// 7 3 C 56 0
      @ 8, 2     SAY "communication leading to highest degree of productivity."
      /// 8 3 C 76 0
      @ 10, 2    SAY "One source file can contain n number of screens, anywhere in the source, at "
      /// 9 3 C 74 0
      @ 11, 2    SAY "any indentation. The only requirement is to place following lines where a "
      /// 10 3 C 17 0
      @ 12, 2    SAY "screen is needed:"
      /// 11 3 C 35 0
      @ 13, 22   SAY "/* HB_SCREEN_BEGINS <ScreenName> */"                                          COLOR "GR+/B"
      /// 12 3 C 33 0
      @ 14, 22   SAY "/* HB_SCREEN_ENDS <ScreenName> */"                                            COLOR "GR+/B"
      /// 13 3 C 76 0
      @ 15, 2    SAY "<ScreenName> should be unique 13 characters long string across given source."
      /// 14 3 C 74 0
      @ 16, 2    SAY "This is to be done manually. Once you place above lines into source file, "
      /// 15 3 C 75 0
      @ 17, 2    SAY "just supply that source to load a screen. All screens defined therein will "
      /// 16 3 C 39 0
      @ 18, 2    SAY "be presented to be selected and edited."
      /// 17 3 C 76 0
      @ 20, 2    SAY "The designer implements SAYs with/without expression, GETs with all clauses,"
      /// 18 3 C 56 0
      @ 21, 2    SAY "BOXes with all flavours, special characters (TOBE Done)."
      /// 19 3 C 76 0
      @ 26, 2    SAY " ESC-Designer  1-Keys  2-General  3-ListedInputs  4-BlockSelection  5-About " COLOR "N/W*"

      /* HB_SCREEN_ENDS <General-1> */
      EXIT

   CASE "GENERAL-2"
      /* HB_SCREEN_BEGINS <General-2> */

      /// 1 3 C 76 0
      @ 1, 2     SAY "                               Selective Input                              " COLOR "N/W*"
      /// 2 3 C 76 0
      @ 26, 2    SAY " ESC-Designer  1-Keys  2-General  3-ListedInputs  4-BlockSelection  5-About " COLOR "N/W*"

      /* HB_SCREEN_ENDS <General-2> */
      EXIT

   CASE "GENERAL-3"
      /* HB_SCREEN_BEGINS <General-3> */

      /// 1 3 C 76 0
      @ 1, 2     SAY "                               Block Selection                              " COLOR "N/W*"
      /// 2 3 C 76 0
      @ 26, 2    SAY " ESC-Designer  1-Keys  2-General  3-ListedInputs  4-BlockSelection  5-About " COLOR "N/W*"

      /* HB_SCREEN_ENDS <General-3> */
      EXIT

   CASE "ABOUT"
      /* HB_SCREEN_BEGINS <About> */

      /// 1 3 C 76 0
      @ 1, 2     SAY "                                    About                                   " COLOR "N/W*"
      /// 2 3 C 1 0
      @ 4, 40    SAY "*"                                                                            COLOR "W+/B"
      /// 3 3 C 35 0
      @ 7, 23    SAY "Harbour Screen Designer ( hbCuiEd )"                                          COLOR "GR+/B"
      /// 4 3 C 13 0
      @ 9, 34    SAY "Developed by"
      /// 5 3 C 40 0
      @ 10, 20   SAY "Pritpal Bedi ( bedipritpal@hotmail.com )"
      /// 6 3 C 14 0
      @ 14, 33   SAY "Copyright 2011"
      /// 7 3 C 12 0
      @ 15, 34   SAY "Pritpal Bedi"                                                                 COLOR "W+/B"
      /// 8 3 C 23 0
      @ 16, 29   SAY "www.harbour-project.org"
      /// 9 3 C 29 0
      @ 20, 26   SAY "Visit the project website at:"
      /// 10 3 C 31 0
      @ 21, 25   SAY "http://www.harbour-project.org/"                                              COLOR "GR+/B"
      /// 11 3 C 1 0
      @ 23, 40   SAY "*"                                                                            COLOR "W+/B"
      /// 12 3 C 76 0
      @ 26, 2    SAY " ESC-Designer  1-Keys  2-General  3-ListedInputs  4-BlockSelection  5-About " COLOR "N/W*"

      /* HB_SCREEN_ENDS <About> */
      EXIT

   ENDSWITCH

   RETURN NIL

/*----------------------------------------------------------------------*/

FUNCTION hbcui_test()
   LOCAL getlist := {}

   /* HB_SCREEN_BEGINS <test> */

   /// 1 1 . 9 0
   @ 1, 2     , 12, 36 BOX "+-+¦+-+¦"
   /// 2 3 C 5 0
   @ 3, 4     SAY "Name "
   /// 3 3 C 15 0
   @ 5, 4     SAY "Salary "
   /// 4 3 C 14 0
   @ 7, 4     SAY "Date of Birth "
   /// 5 4 C 25 0
   @ 3, 10    GET cName   PICTURE "@K! "
   /// 6 4 N 12 2
   @ 5, 23    GET nSalary
   /// 7 4 D 8 0
   @ 7, 27    GET dBirth

   /* HB_SCREEN_ENDS <test> */

   RETURN NIL

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#define INRANGE( xLo, xVal, xHi )  ( ( xVal >= xLo ) .AND. ( xVal <= xHi ) )
#define BETWEEN( xLo, xVal, xHi )  min( max( xLo, xVal ), xHi )

/*----------------------------------------------------------------------*/

FUNCTION VouchAChoice( nTop, nLft, nBtm, nRgt, acItems, xSelect, cUserFunc, nPos, nHiLytRow, oWin, nLastKey, cargo_ )
   LOCAL nChoice, oChoice
   LOCAL crs := SetCursor( 0 )

   oChoice := AChoiceNew():New( nTop, nLft, nBtm, nRgt, acItems, xSelect, ;
                                cUserFunc, ;
                                nPos, nHiLytRow, oWin, nLastKey, cargo_   )
   oChoice:Exe()
   nChoice  := oChoice:nPos
   nLastKey := oChoice:nKey
   oChoice:Destroy()

   SetCursor( crs )

   RETURN ( nChoice )

/*----------------------------------------------------------------------*/

CREATE CLASS AChoiceNew

   VAR nTop, nLeft, nBottom, nRight
   VAR acItems
   VAR xSelect
   VAR cUserFunc
   VAR nPos
   VAR nHiLiteRow
   VAR oWin
   VAR cargo_
   VAR nNumCols
   VAR nNumRows
   VAR acCopy
   VAR alSelect
   VAR nNewPos
   VAR lFinished
   VAR nKey
   VAR nMode
   VAR nAtTop
   VAR nAtBtm
   VAR nItems
   VAR bScan
   VAR lUserFunc
   VAR nUserFunc
   VAR bUserFunc
   VAR cLoClr
   VAR cHiClr
   VAR cUnClr
   VAR nFrstItem
   VAR nLastItem
   VAR bAction
   VAR mrc_

   METHOD init
   METHOD Destroy
   METHOD DispPageNew
   METHOD DispLineNew
   METHOD Up
   METHOD Down
   METHOD PageUp
   METHOD PageDown
   METHOD GoTop
   METHOD GoBottom
   METHOD Top
   METHOD Bottom
   METHOD GoTo
   METHOD Exe
   METHOD DeHilite
   METHOD HiLite
   METHOD DispAtNew

ENDCLASS

//----------------------------------------------------------------------//

METHOD AChoiceNew:Destroy()

   RETURN NIL

//----------------------------------------------------------------------//

METHOD AChoiceNew:init( nTop, nLft, nBtm, nRgt, acItems, xSelect, ;
                        cUserFunc, nPos, nHiLiteRow, oWin, nLastKey, cargo_ )
   LOCAL nCntr

   HB_SYMBOL_UNUSED( nLastKey )

   DEFAULT nTop       TO 0              // The topmost row of the window
   DEFAULT nLft       TO 0              // The leftmost column of the window
   DEFAULT nBtm       TO maxrow() + 1   // The bottommost row of the windows
   DEFAULT nRgt       TO maxcol() + 1   // The rightmost column of the window
   DEFAULT acItems    TO {}             // The items FROM which TO choose
   DEFAULT xSelect    TO .T.            // Array OR logical, what is selectable
   DEFAULT cUserFunc  TO NIL            // Optional FUNCTION FOR key exceptions
   DEFAULT nPos       TO 1              // The number of the selected item
   DEFAULT nHiLiteRow TO 0              // The row TO be highlighted

   ::nTop        := nTop
   ::nLeft       := nLft
   ::nBottom     := nBtm
   ::nRight      := nRgt
   ::acItems     := acItems
   ::xSelect     := xSelect
   ::cUserFunc   := cUserFunc
   ::nPos        := nPos
   ::nHiLiteRow  := nHiLiteRow
   ::oWin        := oWin
   ::cargo_      := cargo_

   ::nNumCols    := 0                    // Number of columns IN the window
   ::nNumRows    := 0                    // Number of rows IN the window
   ::acCopy      := {}                   // A padded copy of the items
   ::alSelect    := {}                   // Select permission
   ::nNewPos     := 0                    // The NEXT item TO be selected
   ::lFinished   := .F.                  // Is processing finished?
   ::nKey        := 0                    // The keystroke TO be processed
   ::nMode       := AC_IDLE              // The current operating mode
   ::nAtTop      := 1                    // The number of the item at the top
   ::nAtBtm      := 1                    // The number of the item at the bottom
   ::nItems      := 0                    // The number of items
   ::bScan       := { | cX | IF( left( cX, 1 ) == upper( chr( ::nKey ) ), .T., .F. ) }
   ::lUserFunc   := ( !empty( ::cUserFunc ) )
   ::nUserFunc   := 0                    // RETURN value FROM user FUNCTION
   ::bUserFunc   := { || AC_ABORT }      // Block form of user FUNCTION
   ::cLoClr      := Before( ",", setcolor() )
   ::cHiClr      := Before( ",", After( ",", setcolor() ) )
   ::cUnClr      := After( ",", After( ",", After( ",", After( ",", setcolor() ) ) ) )
   ::nFrstItem   := 0
   ::nLastItem   := 0
   ::bAction     := NIL
   ::mrc_        := {}

   IF ::lUserFunc
      ::bUserFunc := &( "{|nM,nP,nH,nK,aC|" + ::cUserFunc + "(nM,nP,nH,nK,aC)}" )
   ENDIF

   IF empty( ::cHiClr )
      ::cHiClr := After( "/", ::cLoClr ) + "/" + Before( "/", ::cLoClr )
   ENDIF

   IF empty( ::cUnClr )
      ::cUnClr := ::cLoClr
   ENDIF

   ::nNumCols := ::nRight - ::nLeft + 1
   ::nNumRows := ::nBottom - ::nTop + 1

   aeval( ::acItems, { | x | IF( valtype( x ) == "C", aadd( ::acCopy, padr( x, ::nNumCols ) ), .F. ) } )
   ::nItems := len( ::acCopy )

   ::alSelect := array( ::nItems )

   IF valtype( ::xSelect ) == "A"
      afill( ::alSelect, .T. )
      FOR nCntr := 1 TO len( ::xSelect )
         IF nCntr <= ::nItems
            IF valtype( ::xSelect[ nCntr ] ) == "C"
               IF empty( ::xSelect[ nCntr ] )
                  ::lFinished := .T.
                  ::nPos      := 0
               ELSE
                  ::alSelect[ nCntr ] := &( ::xSelect[ nCntr ] )
               ENDIF
            ELSE
               ::alSelect[ nCntr ] := ::xSelect[ nCntr ]
            ENDIF
         ELSE
            nCntr := len( ::xSelect ) + 1
         ENDIF
      NEXT
   ELSE
      afill( ::alSelect, ::xSelect )
   ENDIF

   RETURN SELF

//----------------------------------------------------------------------//

METHOD AChoiceNew:Exe()

   IF !( ::lFinished )
      ::nFrstItem := ascan( ::alSelect, .T. )  // First valid item

      IF ::nFrstItem == 0
         ::nLastItem := 0
         ::nPos      := 0
         ::nMode     := AC_NOITEM
      ELSE
         ::nMode     := AC_IDLE
         ::nLastItem := ::nItems
         DO WHILE ( !( ::alSelect[ ::nLastItem ] ) )
            ::nLastItem--
         ENDDO
      ENDIF

      // Ensure hilighted item can be selected
      ::nPos := BETWEEN( ::nFrstItem, ::nPos, ::nLastItem )
      ::nNewPos := ::nPos
      IF !( ::alSelect[ ::nNewPos ] )
         IF ::nNewPos == ::nLastItem
            ::nNewPos := ::nFrstItem
         ENDIF
         DO WHILE ( !( ::alSelect[ ::nNewPos ] ) )
            ::nNewPos++
         ENDDO
      ENDIF
      ::nPos := ::nNewPos

      // Force hilighted row TO be valid
      //
      ::nHiLiteRow := BETWEEN( 0, ::nHiLiteRow, ::nNumRows - 1 )

      // Force the topmost item TO be a valid index of the array
      //
      ::nAtTop := BETWEEN( 1, max( 1, ::nPos - ::nHiLiteRow ), ::nItems )

      // Ensure as much of the selection area as possible is covered
      //
      IF ( ::nAtTop + ::nNumRows - 1 ) > ::nItems
         ::nAtTop := max( 1, ::nItems - ::nNumrows + 1 )
      ENDIF

      ::DispPageNew()
   ENDIF

   DO WHILE ( !::lFinished )

      IF ::nMode != AC_GOTO .AND. ::nMode != AC_NOITEM
         ::nKey  := inkey( , INKEY_ALL + HB_INKEY_GTEVENT )
         ::nMode := AC_IDLE
         ::mrc_  := { 0, 0, mRow(), mCol(), 0, LastKey(), .f. }
      ENDIF

   #ifdef __WVT__
      IF nLastPos <> ::nPos
         Wvt_DrawFocusRect( ::nTop + ( ::nPos - ::nAtTop ), ::nLeft, ;
                            ::nTop + ( ::nPos - ::nAtTop ), ::nRight )
         nLastPos := ::nPos
      ENDIF
   #ENDIF

      DO CASE
      CASE ( ::bAction := SetKey( ::nKey ) ) != NIL
         eval( ::bAction, ProcName( 1 ), ProcLine( 1 ), '' )

      CASE ::nKey == K_MOUSEMOVE
         ::nPos := ::DispAtNew()

      CASE ::nKey == K_MWFORWARD
         ::Up()

      CASE ::nKey == K_MWBACKWARD
         ::Down()

      CASE ::nKey == K_LDBLCLK
         ::nPos := ::DispAtNew()
         ::nMode  := AC_SELECT

      CASE ::nKey == K_LBUTTONDOWN
         IF ::mrc_[ 3 ] >= ::nTop  .AND. ::mrc_[ 3 ] <= ::nBottom .AND. ;
            ::mrc_[ 4 ] >= ::nLeft .AND. ::mrc_[ 4 ] <= ::nRight
            keyboard( chr( K_ENTER ) )
         ENDIF

      CASE ( ( ::nKey == K_ESC ) .OR. ( ::nMode == AC_NOITEM ) ) .AND. ( !::lUserFunc )
         ::nMode     := AC_ABORT
         ::nPos      := 0
         ::lFinished := .T.

      CASE ::nKey == K_UP
         ::Up()

      CASE ::nKey == K_DOWN
         ::Down()

      CASE ::nKey == K_PGUP
         ::PageUp()

      CASE ::nKey == K_PGDN
         ::PageDown()

      CASE ::nKey == K_HOME
         ::Top()

      CASE ::nKey == K_END
         ::Bottom()

      CASE ( ::nKey == K_CTRL_HOME .OR. ::nKey == K_CTRL_PGUP )
         ::GoTop()

      CASE ( ::nKey == K_CTRL_END .OR. ::nKey == K_CTRL_PGDN )
         ::GoBottom()

      CASE ( ::nKey == K_ENTER ) .AND. ( !::lUserFunc )
         ::nMode     := AC_SELECT
         ::lFinished := .T.

      CASE ( ::nKey == K_RIGHT ) .AND. ( !::lUserFunc )
         ::nPos      := 0
         ::lFinished := .T.

      CASE ( ::nKey == K_LEFT ) .AND. ( !::lUserFunc )
         ::nPos      := 0
         ::lFinished := .T.

      CASE INRANGE( 32, ::nKey, 255 ) .AND. ( ( !::lUserFunc ) .OR. ( ::nMode == AC_GOTO ) )
         ::GoTo()
         ::nMode := AC_IDLE

      CASE ::nMode == AC_GOTO
         ::nMode := AC_IDLE

      OTHERWISE
         IF ::nKey == 0
            ::nMode := AC_IDLE
         ELSE
            ::nMode := AC_EXCEPT
         ENDIF

      ENDCASE

      IF ::lUserFunc
         ::nUserFunc := eval( ::bUserFunc, ::nMode, ::nPos, ;
                              ::nPos - ::nAtTop, ::nKey, ::cargo_ )
         DO CASE
         CASE ::nUserFunc == AC_ABORT
            ::lFinished := .T.
            ::nPos      := 0

         CASE ::nUserFunc == AC_SELECT
            ::lFinished := .T.

         CASE ::nUserFunc == AC_CONT

         CASE ::nUserFunc == AC_GOTO
            ::nMode := AC_GOTO

         ENDCASE
      ENDIF
   ENDDO

   RETURN SELF

//----------------------------------------------------------------------//

METHOD AChoiceNew:DispPageNew()
   LOCAL nCntr
   LOCAL nRow := row()
   LOCAL nCol := col()
   LOCAL nRowPos, nPos

   DispBegin()

   FOR nCntr := 1 TO ::nNumRows
      nRowPos := ::nTop   + nCntr - 1
      nPos    := ::nAtTop + nCntr - 1

      IF INRANGE( 1, nPos, ::nItems )
         ::DispLineNew( nPos, nRowPos, nPos == ::nPos )
      ELSE
         DispOutAt( nRowPos, ::nLeft, space( len( ::acCopy[ 1 ] ) ), ::cLoClr, ::oWin )
      ENDIF
   NEXT

   DispEnd()

   SetPos( nRow,nCol )

   RETURN SELF

//----------------------------------------------------------------------//

METHOD AChoiceNew:DispLineNew( nPos, nRow, lHiLite )

   DispOutAt( nRow, ::nLeft, ::acCopy[ nPos ],;
                IF( ::alSelect[ nPos ], ;
                  IF( lHiLite, ::cHiClr, ::cLoClr ), ::cUnClr ), ::oWin )

   RETURN SELF

//----------------------------------------------------------------------//

METHOD AChoiceNew:DeHilite()

   ::DispLineNew( ::nPos, ::nTop + ( ::nPos - ::nAtTop ), .F. )

   RETURN SELF

//----------------------------------------------------------------------//

METHOD AChoiceNew:HiLite()

   ::DispLineNew( ::nPos, ::nTop + ( ::nPos - ::nAtTop ), .T. )

   RETURN SELF

//----------------------------------------------------------------------//

METHOD AChoiceNew:Up()
   LOCAL nScroll

   IF ::nPos == ::nFrstItem
      ::nMode := AC_HITTOP
      IF ::nAtTop > max( 1, ::nPos - ::nNumRows + 1 )
         ::nAtTop := max( 1, ::nPos - ::nNumRows + 1 )
         ::DispPageNew()
      ENDIF
   ELSE
      ::nNewPos := ::nPos - 1
      DO WHILE !( ::alSelect[ ::nNewPos ] )
         ::nNewPos--
      ENDDO

      IF INRANGE( ::nAtTop, ::nNewPos, ::nAtTop + ::nNumRows - 1 )
         ::DeHilite()
         ::nPos := ::nNewPos
         ::HiLite()
      ELSE
         DispBegin()

         ::DeHilite()

         nScroll := max( -::nNumRows, ( ::nNewPos - ( ::nAtTop + ::nNumRows - 1 ) ) )
         Scroll( ::nTop, ::nLeft, ::nBottom, ::nRight, nScroll )

         ::nAtTop := ::nNewPos
         ::nPos   := max( ::nPos, ::nAtTop + ::nNumRows - 1 )

         DO WHILE ( ::nPos > ::nNewPos )
            ::DispLineNew( ::nPos, ::nTop + ( ::nPos - ::nAtTop ), .F. )
            ::nPos--
         ENDDO

         ::HiLite()

         Dispend()
      ENDIF
   ENDIF

   RETURN SELF

//----------------------------------------------------------------------//

METHOD AChoiceNew:Down()
   LOCAL nScroll

   IF ::nPos == ::nLastItem
      ::nMode := AC_HITBOTTOM
      IF ::nAtTop < min( ::nPos, ::nItems - ::nNumRows + 1 )
         ::nAtTop := min( ::nPos, ::nItems - ::nNumRows + 1 )
         ::DispPageNew()
      ENDIF
   ELSE
      ::nNewPos := ::nPos + 1
      DO WHILE !( ::alSelect[ ::nNewPos ] )
         ::nNewPos++
      ENDDO

      IF INRANGE( ::nAtTop, ::nNewPos, ::nAtTop + ::nNumRows - 1 )
         ::DeHilite()
         ::nPos := ::nNewPos
         ::HiLite()
      ELSE
         Dispbegin()

         ::DeHilite()

         nScroll := min( ::nNumRows, ( ::nNewPos - ( ::nAtTop + ::nNumRows - 1 ) ) )
         scroll( ::nTop, ::nLeft, ::nBottom, ::nRight, nScroll )

         ::nAtTop := ::nNewPos - ::nNumRows + 1
         ::nPos   := max( ::nPos, ::nAtTop )
         DO WHILE ( ::nPos < ::nNewPos )
            ::DispLineNew( ::nPos, ::nTop + ( ::nPos - ::nAtTop ), .F. )
            ::nPos ++
         ENDDO

         ::Hilite()

         Dispend()
      ENDIF
   ENDIF

   RETURN SELF

//----------------------------------------------------------------------//

METHOD AChoiceNew:PageUp()

   IF ::nPos == ::nFrstItem
      ::nMode := AC_HITTOP
      IF ::nAtTop > max( 1, ::nPos - ::nNumRows + 1 )
         ::nAtTop := max( 1, ::nPos - ::nNumRows + 1 )
         ::DispPageNew()
      ENDIF
   ELSE
      IF INRANGE( ::nAtTop, ::nFrstItem, ::nAtTop + ::nNumRows - 1 )
         ::nPos   := ::nFrstItem
         ::nAtTop := max( ::nPos - ::nNumRows + 1, 1 )
         ::DispPageNew()
      ELSE
         IF ( ::nPos - ::nNumRows + 1 ) < ::nFrstItem
            ::nPos   := ::nFrstItem
            ::nAtTop := ::nFrstItem
         ELSE
            ::nPos   := max( ::nFrstItem, ::nPos - ::nNumRows + 1 )
            ::nAtTop := max( 1, ::nAtTop - ::nNumRows + 1 )
            DO WHILE ( ::nPos > ::nFrstItem ) .AND. ( !( ::alSelect[ ::nPos ] ) )
               ::nPos--
               ::nAtTop--
            ENDDO
            ::nAtTop := max( 1, ::nAtTop )
         ENDIF
         ::DispPageNew()
      ENDIF
   ENDIF

   RETURN SELF

//----------------------------------------------------------------------//

METHOD AChoiceNew:PageDown()
   LOCAL nGap

   IF ::nPos == ::nLastItem
      ::nMode := AC_HITBOTTOM
      IF ::nAtTop < min( ::nPos, max( 1, ::nItems - ::nNumRows + 1 ) )
         ::nAtTop := min( ::nPos, max( 1, ::nItems - ::nNumRows + 1 ) )
         ::DispPageNew()
      ENDIF
   ELSE
      IF INRANGE( ::nAtTop, ::nLastItem, ::nAtTop + ::nNumRows - 1 )
         ::DeHilite()
         ::nPos := ::nLastItem
         ::Hilite()
      ELSE
         nGap := ::nPos - ::nAtTop
         ::nPos := min( ::nLastItem, ::nPos + ::nNumRows - 1 )
         IF ( ::nPos + ::nNumRows - 1 ) > ::nLastItem
            ::nAtTop := ::nLastItem - ::nNumRows + 1
            ::nPos   := min( ::nLastItem, ::nAtTop + nGap )
         ELSE
            ::nAtTop := ::nPos - nGap
         ENDIF

         DO WHILE ( ::nPos < ::nLastItem ) .AND. !( ::alSelect[ ::nPos ] )
            ::nPos++
            ::nAtTop++
         ENDDO

         DO WHILE ( ::nAtTop + ::nNumRows - 1 ) > ::nItems
            ::nAtTop--
         ENDDO
         ::DispPageNew()
      ENDIF
   ENDIF

   RETURN SELF

//----------------------------------------------------------------------//

METHOD AChoiceNew:Top()

   IF ::nPos == ::nFrstItem
      IF ::nAtTop == max( 1, ::nPos - ::nNumRows + 1 )
         ::nMode := AC_HITTOP
      ELSE
         ::nAtTop := max( 1, ::nPos - ::nNumRows + 1 )
         ::DispPageNew()
      ENDIF
   ELSE
      ::nNewPos := ::nAtTop
      DO WHILE !( ::alSelect[ ::nNewPos ] )
         ::nNewPos++
      ENDDO
      IF ::nNewPos != ::nPos
         ::DeHilite()
         ::nPos := ::nNewPos
         ::HiLite()
      ENDIF
   ENDIF

   RETURN SELF

//----------------------------------------------------------------------//

METHOD AChoiceNew:Bottom()

   IF ::nPos == ::nLastItem
      IF ::nAtTop == min( ::nPos, ::nItems - ::nNumRows + 1 )
         ::nMode := AC_HITBOTTOM
      ELSE
         ::nAtTop := min( ::nPos, ::nItems - ::nNumRows + 1 )
         ::DispPageNew()
      ENDIF
   ELSE
      ::nNewPos := ::nAtTop + ::nNumRows - 1
      DO WHILE !( ::alSelect[ ::nNewPos ] )
         ::nNewPos--
      ENDDO
      IF ::nNewPos != ::nPos
         ::DeHilite()
         ::nPos := ::nNewPos
         ::HiLite()
      ENDIF
   ENDIF

   RETURN SELF

//----------------------------------------------------------------------//

METHOD AChoiceNew:GoTop()

   IF ::nPos == ::nFrstItem
      IF ::nAtTop == max( 1, ::nPos - ::nNumRows + 1 )
         ::nMode := AC_HITTOP
      ELSE
         ::nAtTop := max( 1, ::nPos - ::nNumRows + 1 )
         ::DispPageNew()
      ENDIF
   ELSE
      ::nPos   := ::nFrstItem
      ::nAtTop := ::nPos
      ::DispPageNew()
   ENDIF

   RETURN SELF

//----------------------------------------------------------------------//

METHOD AChoiceNew:GoBottom()

   IF ::nPos == ::nLastItem
      IF ::nAtTop == min( ::nLastItem, ::nItems - ::nNumRows + 1 )
         ::nMode := AC_HITBOTTOM
      ELSE
         ::nAtTop := min( ::nLastItem, ::nItems - ::nNumRows + 1 )
         ::DispPageNew()
      ENDIF
   ELSE
      IF INRANGE( ::nAtTop, ::nLastItem, ::nAtTop + ::nNumRows - 1 )
         ::DeHilite()
         ::nPos := ::nLastItem
         ::HiLite()
      ELSE
         ::nPos   := ::nLastItem
         ::nAtTop := max( 1, ::nPos - ::nNumRows + 1 )
         ::DispPageNew()
      ENDIF
   ENDIF

   RETURN SELF

//----------------------------------------------------------------------//

METHOD AChoiceNew:GoTo()

   ::nNewPos := ascan( ::acCopy, ::bScan, ::nPos + 1 )
   DO WHILE INRANGE( ::nPos, ::nNewPos, ::nLastItem ) .AND. !( ::alSelect[ ::nNewPos ] )
      ::nNewPos := ascan( ::acCopy, ::bScan, ::nNewPos + 1 )
   ENDDO

   IF ::nNewPos == 0
      ::nNewPos := ascan( ::acCopy, ::bScan )
      DO WHILE INRANGE( 1, ::nNewPos, ::nLastItem ) .AND. !( ::alSelect[ ::nNewPos ] )
         ::nNewPos := ascan( ::acCopy, ::bScan, ::nNewPos + 1 )
      ENDDO
   ENDIF

   IF INRANGE( ::nFrstItem, ::nNewPos, ::nLastItem ) .AND. ::alSelect[ ::nNewPos ]
      IF INRANGE( ::nAtTop, ::nNewPos, ::nAtTop + ::nNumRows - 1 )
         ::DeHilite()
         ::nPos := ::nNewPos
         ::HiLite()
      ELSE
         ::nPos   := ::nNewPos
         ::nAtTop := BETWEEN( 1, ::nPos - ::nNumRows + 1, ::nItems )
         ::DispPageNew()
      ENDIF
   ENDIF

   RETURN SELF

//----------------------------------------------------------------------//

METHOD AChoiceNew:DispAtNew()
   LOCAL nNewPos

   IF ::mrc_[ 3 ] >= ::nTop .AND. ::mrc_[ 3 ] <= ::nTop + ::nNumRows - 1 ;
                                 .AND. ;
            ::mrc_[ 4 ] >= ::nLeft .AND. ::mrc_[ 4 ] <= ::nRight

      IF ( nNewPos := ::nAtTop + ( ::mrc_[ 3 ] - ::nTop ) ) <> ::nPos
         IF ::alSelect[ nNewPos ]
            ::DeHilite()
            ::nPos    := nNewPos
            ::nNewPos := ::nPos
            ::HiLite()
         ENDIF
      ENDIF
   ENDIF

   RETURN ::nPos

//----------------------------------------------------------------------//

STATIC FUNCTION Before( cDelim, cValue )
   LOCAL cRetVal := cValue

   IF cDelim $ cValue
      cRetVal := left( cValue, at( cDelim, cValue ) - 1 )
   ENDIF

   RETURN ( cRetVal )

//----------------------------------------------------------------------//

STATIC FUNCTION After( cDelim, cValue )
   LOCAL cRetVal := ''

   IF cDelim $ cValue
      cRetVal := substr( cValue, at( cDelim, cValue ) + 1 )
   ENDIF

   RETURN ( cRetVal )

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

STATIC FUNCTION BuildScreen()
   LOCAL oCUI

   SET SCOREBOARD OFF
   CLS

   hb_gtInfo( HB_GTI_WINTITLE  , "Harbour CUI Forms Designer" )
   hb_gtInfo( HB_GTI_RESIZEMODE, HB_GTI_RESIZEMODE_ROWS )

   oCUI := hbCUIEditor():new():create()
   oCUI:destroy()

   RETURN NIL

/*----------------------------------------------------------------------*/

FUNCTION hbide_setExitCuiEd( lYes )
   LOCAL l_lYes
   STATIC s_lYes := .f.
   l_lYes := s_lYes
   IF hb_isLogical( lYes )
      s_lYes := lYes
   ENDIF
   RETURN l_lYes

/*----------------------------------------------------------------------*/
