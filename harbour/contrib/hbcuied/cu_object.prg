/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * CUI Forms Editor
 *
 * Copyright 2011 Pritpal Bedi <bedipritpal@hotmail.com>
 * http://harbour-project.org
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
 *                       Harbour CUI Editor Source
 *
 *                             Pritpal Bedi
 *                               13Aug2011
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbcuied.ch"
#include "common.ch"
#include "inkey.ch"
#include "box.ch"
#include "hbclass.ch"

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
   DATA cDrawFill                                 INIT "°°°°°°°°°" //"±±±±±±±±±"
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
   METHOD scrObject()
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
   METHOD scrProperty()
   METHOD scrMsg( msg )
   METHOD scrInKey( key_ )
   METHOD scrConfig()
   METHOD scrReConfig()
   METHOD scrSectors()
   METHOD scrAddPrp( sct_ )
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

   ::cDrawFill      := "°°°°°°°°°" // 176   "±±±±±±±±±"  177
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

METHOD hbCUIEditor:scrSectors()

   aadd( ::sectors_, { 1, "Screen", "R    ", 100, "W+/BG", "", .f., .f. } )

   RETURN 100

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrAddPrp( sct_ )

   aadd( ::sectors_, { sct_[1], sct_[2], sct_[3], sct_[4], sct_[5], sct_[6], sct_[7], sct_[8] } )

   RETURN NIL

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
         IF ::nLastKey <> K_MOUSEMOVE
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
      CASE ::nLastKey == K_ALT_L
         ::scrLoad( .t. )
      CASE ::nLastKey == K_ALT_S
         ::scrSave( .t. )

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
         //  scrMovPgUp(scn_)
         ::nRowRep := 1
         ::nRowCur := ::nTop
         ::nRowDis := ::nTop - 1
         ::xRefresh := OBJ_REFRESH_ALL
      CASE ::nLastKey == K_PGDN
         //  ::nRowRep := ::nRowsMax
         //  ::nRowCur := ::nBottom

      CASE ::nLastKey == K_INS
         readInsert( !readInsert() )
         setcursor( iif( readInsert(), 2, 1 ) )

      CASE ::nLastKey == K_ENTER
         IF ::nMode == OBJ_MODE_SELECT .AND. ::nObjSelected > 0
            ::obj_[ ::nObjSelected, OBJ_SECTION ] := 1
            ::nColsMax      := max( ::nColsMax, ::obj_[ ::nObjSelected, OBJ_TO_COL ] + 1 )
            ::nMode         := OBJ_MODE_IDLE
            ::xRefresh      := OBJ_REFRESH_LINE
            ::nObjSelected  := 0
            ::scrMsg()
         ENDIF

      CASE VouchInRange( ::nLastKey, K_SPACE, 254 ) .AND. ::nMode <> OBJ_MODE_SELECT
         ::scrAddTxt( 1 )

      CASE ::nLastKey == K_F1                           //  Help
         help()
      CASE ::nLastKey == K_F3                           //  OBJECT
         ::scrObject()
      CASE ::nLastKey == K_F7                           //  Copy
         ::scrObjCopy()
      CASE ::nLastKey == K_F8                           //  Paste
         ::scrObjPas()
      CASE ::nLastKey == K_F9                           //  Box
         ::scrAddBox()
      CASE ::nLastKey == K_F10                          //  Fields
         ::scrAddFld()

      CASE ::nLastKey == K_DEL
         IF ! empty( ::aTextBlock )
            ::scrTextDel()
            ::scrOrdObj()
            ::nMode         := 0
            ::nObjSelected := 0
            ::nObjHilite   := 0
            ::xRefresh      := OBJ_REFRESH_ALL
         ELSEIF ::scrIsTxt()
            ::scrAddTxt( 2 )
         ELSEIF ::nMode == OBJ_MODE_SELECT
            ::scrObjDel( ::nObjSelected )
            ::nMode         := 0
            ::nObjSelected := 0
         ELSEIF ::nObjHilite > 0
            ::scrObjDel( ::nObjHilite )
            ::nMode         := 0
            ::nObjSelected := 0
            ::nObjHilite   := 0
            ::xRefresh      := OBJ_REFRESH_ALL
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

      CASE ::nLastKey == K_ALT_W
         //::scrRepCol()
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
         ::nMode         := OBJ_MODE_SELECT
         ::nObjSelected := nObj
         ::scrOnFirstCol( nObj, { OBJ_O_BOX } )
         ::scrMsg( "Box is Selected. Use Arrow Keys to Move, Enter to Finish !" )
         ::lEdited := .t.

      ELSEIF nObj > 0 .AND. ::nLastKey == K_F6 .AND. ! ::objIsBox( nObj )
         ::nMode        := OBJ_MODE_SELECT
         ::nObjSelected := nObj
         ::scrOnFirstCol( nObj, { OBJ_O_TEXT } )
         ::scrMsg( "OBJECT is Selected. Use Arrow Keys to Move, Enter to Finished" )
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
                                       'W/B' /* obj_[i,OBJ_COLOR] */ ) )
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
                                      'W+/W' /* obj_[i,OBJ_COLOR] */ ) )
            IF nOff < 0
               cText := substr( ::obj_[ i,OBJ_TEXT ], abs( nOff ) + 1 )
            ENDIF
            @ nRow, nCol SAY cText COLOR cColor
         ENDIF

         IF ::objIsTxt( i )
            cText  := ::obj_[ i,OBJ_EQN ]
            cColor := iif( ::nObjSelected == i, ::cClrSelect,;
                         iif( empty( ::obj_[ i, OBJ_COLOR ] ), ::cClrText,;
                                           'W/B' /* obj_[i,OBJ_COLOR] */) )
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
      dispBox( ::nRowCur  ,;
               ::nLeft     ,;
               ::nRowCur  ,;
               ::nRight    ,;
               ::cDrawFill,;
               ::cClrPrev  )

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
                              'W/B' /* obj_[i,OBJ_COLOR] */ )
            ENDCASE
         ENDIF

         IF ::obj_[ i, OBJ_ROW ] == ::nRowRep
            IF ::objIsFld( i )
               cText := ::obj_[ i,OBJ_TEXT ]
               cColor := iif( ::nObjSelected == i, ::cClrSelect,;
                         iif( ::nObjHilite   == i, ::nObjHilite,;
                                           'W+/W' /* obj_[i,OBJ_COLOR] */ ))
               @ nRow, nCol SAY cText COLOR cColor
            ENDIF

            IF ::objIsTxt( i )
               cText  := ::obj_[ i, OBJ_EQN ]
               cColor := iif( ::nObjSelected == i, ::cClrSelect,;
                         iif( empty( ::obj_[ i, OBJ_COLOR ] ), ::cClrText,;
                                      'W/B' /* obj_[i,OBJ_COLOR] */))
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
   s := pad( cS, 20 ) + ' ³ '

   s += pad( ::cScreen, 13 ) +;
             ' ³ '+;
             ' R:'+;
             str( ::nRowRep - 1, 3 ) +;
             ' C:'+;
             str( ::nColRep - 1, 3 ) +;
             ' ³ ' +;
             iif( readInsert(), 'Ins', '   ' ) +;
             ' ³ '

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

   s += pad( trim( objId ), 10 ) + ' ³ '

   @ ::nRowStatus, ::nColStatus SAY pad( s, maxcol() + 1 ) COLOR ::cClrStatus

   /* Ruler */
   s := substr( ::cRuler, max( 1, ::nColRep - ::nColCur + ::nLeft ), ::nRight - ::nLeft + 1 )
   DispBox( ::nTop - 1, 0, ::nTop - 1, maxcol(), '         ', ::cClrOverall )
   @ ::nRowRuler, ::nLeft SAY s COLOR ::cClrRuler
   @ ::nRowRuler, ::nColCur SAY substr( s, ::nColCur - ::nLeft + 1, 1 ) COLOR 'GR+/BG'

   @ ::nRowCur, ::nColCur SAY ""

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

   IF nmRow < ::nTop .OR. nmRow > ::nBottom .OR. ;
                   nmCol < ::nLeft .OR. nmCol > ::nRight
      RETURN .f.
   ENDIF

   ::scrToMouse( nmRow, nmCol )

   IF nEvent == K_LDBLCLK

   ELSEIF nEvent == K_MMLEFTDOWN /*K_LBUTTONDOWN */ .AND. !( lAnchored )
      IF ::scrChkObj() > 0 .AND. ::nMode <> OBJ_MODE_SELECT
         nCursor := SetCursor( 0 )

         lAnchored := .t.
         ::nLastKey := K_F6
//         Wvt_SetMousePos( ::nRowCur, ::nColCur )
      ENDIF

   ELSEIF nEvent == K_MMLEFTDOWN .AND. lAnchored

   ELSEIF nEvent == K_LBUTTONUP  .AND. lAnchored
//      Wvt_SetMousePos( ::nRowCur, ::nColCur )
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

METHOD hbCUIEditor:scrOrdObj()
   LOCAL a_:={}, d_:={}

   FOR EACH a_ IN ::obj_
      IF a_[ OBJ_TYPE ] == OBJ_O_BOX
         aadd( d_, a_ )
      ENDIF
   NEXT
   FOR EACH a_ IN ::obj_
      IF a_[ OBJ_TYPE ] == OBJ_O_TEXT
         aadd( d_, a_ )
      ENDIF
   NEXT
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
   LOCAL e_

   FOR EACH e_ IN ::obj_
      IF e_[ OBJ_TYPE ] == OBJ_O_BOX
         IF     ( ::nRowRep == e_[ OBJ_ROW ] .OR.  ::nRowRep == e_[ OBJ_TO_ROW ] ) .AND. ;
                ( ::nColRep >= e_[ OBJ_COL ] .AND. ::nColRep <= e_[ OBJ_TO_COL ] )
            RETURN e_:__enumIndex()
         ELSEIF ( ::nColRep == e_[ OBJ_COL ] .OR.  ::nColRep == e_[ OBJ_TO_COL ] ) .AND. ;
                ( ::nRowRep >= e_[ OBJ_ROW ] .AND. ::nRowRep <= e_[ OBJ_TO_ROW ] )
            RETURN e_:__enumIndex()
         ENDIF
      ELSE
         IF ::nRowRep == e_[ OBJ_ROW ] .AND. ( ::nColRep >= e_[ OBJ_COL ] .AND. ::nColRep <= e_[ OBJ_TO_COL ] )
            RETURN e_:__enumIndex()
         ENDIF
      ENDIF
   NEXT

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

   aeval( ::obj_, {|e_,i| iif( e_[ OBJ_ROW ] >= ::nRowRep, ::obj_[ i, OBJ_TO_ROW ]++, NIL ) } )
   aeval( ::obj_, {|e_,i| iif( e_[ OBJ_ROW ] >= ::nRowRep, ::obj_[ i, OBJ_ROW    ]++, NIL ) } )

   ::xRefresh := OBJ_REFRESH_ALL

   RETURN NIL

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrDelLine()
   LOCAL nRow := ::nRowRep
   LOCAL n, isLast

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
      o_[ OBJ_SECTION ]     := 1

      aadd( ::obj_, o_ )

      ::scrOrdObj()
      ::nObjSelected  := 0
      ::xRefresh       := OBJ_REFRESH_LINE
      ::nMode          := 0
      ::nObjCopied    := 0
   ENDIF
   RETURN NIL

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrObjDel( nObj )
   LOCAL n
   LOCAL nUnique := ::obj_[ nObj, OBJ_OBJ_UNIQUE ]

   VouchAShrink( ::obj_, nObj )
   IF empty( ::obj_ )
      aadd( ::obj_, ::scrObjBlank( ))
   ENDIF
   ::nObjSelected := 0
   ::xRefresh      := OBJ_REFRESH_LINE

   IF nUnique > 0
      IF ( n := ascan( ::aFields, {|e_| e_[1] == nUnique } ) ) > 0
         VouchAShrink( ::aFields, n )
      ENDIF
   ENDIF
   RETURN Self

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrObject()
   LOCAL nObj
   LOCAL mnu_:={'Field                        Alt_F'  ,;
                'Boxes                        Alt_B'  ,;
                ' '                                   ,;
                'Columns Width                Alt_W'  ,;
                'Graphic Characters           Alt_F6' ,;
                '  '                                  ,;
                'Copy OBJECT                  Alt_C'  ,;
                'Paste OBJECT                 Alt_V'  ,;
                'Selection of Block           Ctrl_F6',;
                'Copy Selection               Ctrl_F7',;
                'Cut & Paste Selection        Ctrl_F8',;
                '  '                                  ,;
                'Matrix                       Alt_M'   }

   LOCAL sel_:= {.t.,.t.,;
                 .f.,;
                 .t.,.t.,.t.,.t.,;
                 .f.,;
                 .t.,.t.,.t.,.t.,.t.,;
                 .f.,;
                 .t. }

   B_MSG CHOOSE mnu_ RESTORE SHADOW CENTER INTO nObj SELECTABLES sel_

   @ ::nRowCur, ::nColCur SAY ''

   DO CASE
   CASE nObj == 1                              //  Field
      ::scrAddFld()
   CASE nObj == 2                              //  Box
      ::scrAddBox()
   CASE nObj == 3                              //  Blank

   CASE nObj == 4                              //  Columns
      ::scrRepCol()
   CASE nObj == 5                              //  Graphcs
      //graphChar()
      ::lGraphics := ! ::lGraphics
   CASE nObj == 6                              //  Blank

   CASE nObj == 7                              //  Copy
      ::scrObjCopy()
   CASE nObj ==81                              //  Paste
      ::scrObjPas()
   CASE nObj == 9                              //  Block Selection
      ::scrTextBlock()
   CASE nObj == 10                             //  Copy Selectin
      ::scrTextMove( 1 )
   CASE nObj == 11                             //  Copy & Cut Selection
      ::scrTextMove( 0 )
   CASE nObj == 12                             //  Blank

   CASE nObj == 13                             //  Matrix
   ENDCASE

   RETURN nObj

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
      IF empty( ::obj_ )
         aadd( ::obj_, ::scrObjBlank() )
      ENDIF
   ENDIF

   aeval( ins_, {|e_| aadd( ::obj_, e_ ) } )

   ::aTextBlock := {}

   RETURN Self

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrTextDel()
   LOCAL i,n,n1,s,s1,s3,nCol
   LOCAL ins_:={},del_:={},d_:={},old_:={}

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
   LOCAL lOrder  := .f.
   LOCAL nKey    := ::nLastKey

   //  nMode   1.Add   2.Del   3.BS

   //  Scan obj_ FOR Text Objects Related WITH Current Report Row
   aeval( ::obj_,{|e_| iif( e_[ OBJ_TYPE ] == OBJ_O_TEXT .AND. e_[ OBJ_ROW ] == nRepRow, aadd( txt_,e_ ),'' ) } )
   IF nMode == 1      //  New Character
      IF empty( txt_ ) .OR. ascan( txt_, {|e_| VouchInRange( nRepCol, e_[ OBJ_COL ], e_[ OBJ_TO_COL ] ) } ) == 0
         aadd( txt_, ::scrObjBlank() ) ; lOrder := .t.
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
               lOrder := .t.
               n1                := len( txt_ )
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
            IF empty( txt_ )
               aadd( txt_, ::scrObjBlank() )
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   IF !empty( txt_ )
      DO WHILE .t.
         IF( n := ascan( txt_, {|e_| e_[ OBJ_TO_COL ] < e_[ OBJ_COL ] } ) ) > 0
            VouchAShrink( txt_, n )
         ELSE
            EXIT
         ENDIF
      ENDDO
      IF empty( txt_ )
         aadd( txt_, ::scrObjBlank() )
      ENDIF
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
      IF( n := ascan( ::obj_, {|e_| e_[OBJ_TYPE] == OBJ_O_TEXT .AND. e_[OBJ_ROW ] == ::nRowRep } ) ) > 0
         VouchAShrink( ::obj_,n )
         IF empty( ::obj_ )
            aadd( ::obj_, ::scrObjBlank() )
         ENDIF
      ELSE
         EXIT
      ENDIF
   ENDDO

   aeval( txt_, {|e_| iif( e_[ OBJ_ROW ] > 0, aadd( ::obj_, e_ ), '' ) } )   //  Now attach txt_

   DO WHILE .t.
      IF( n := ascan( ::obj_,{|e_| e_[ OBJ_TO_COL ] < e_[ OBJ_COL ] } ) ) > 0
         VouchAShrink( ::obj_, n )
         IF empty( ::obj_ )
            aadd( ::obj_, ::scrObjBlank() )
         ENDIF
      ELSE
         EXIT
      ENDIF
   ENDDO

   IF lOrder
      //  scrOrdObj( obj_ )
   ENDIF
   IF     nMode == 1
      keyboard( chr( K_RIGHT ) )
   ENDIF

   ::xRefresh := OBJ_REFRESH_LINE

   ::lEdited := .t.

   RETURN NIL

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrProperty()

   ::aProperty := {}

   RETURN NIL

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrMsg( msg )
   LOCAL row := row(), col := col()

   @ maxrow(),0 SAY padc( " ", maxcol()+1 ) COLOR "W+/W"
   IF empty( msg )
      msg := "F1:Help F4:Prop F5:Edit F6:Select F7:Copy F8:Paste F9:Box F10:Field"
   ENDIF
   msg := " " + msg + " "
   @ maxrow(),( maxcol()+1-len( msg ) )/2 SAY msg COLOR "W+/B"

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
   o_[ OBJ_SECTION    ] := 1
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
   ::scrMsg( 'Draw Frame WITH <Arrow Keys>. Finish WITH <Enter>' )

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

   B_GETS HEADERS h_ VALUES v_ TITLE 'Configure Field' WHEN w_ INTO v_

   v_:= v_[ 1 ]
   v_[ 1 ] := alltrim( trim( v_[ 1 ] ) )
   IF empty( v_[ 1 ] )
      RETURN NIL
   ENDIF

   IF lastkey() <> K_ESC
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
      o_[ OBJ_SECTION ] := 1

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
      v_:= iif( nObj > 0, ::scrObj2Vv( ::obj_[ nObj ] ), ::scrVrbBlank( OBJ_O_BOX ) )
      h_:= ::scrVrbHeaders( OBJ_O_BOX )
      w_:= afill( array( len( h_ ) ), {|| .T. } )

      w_[ 2 ] := {| | VouchMenuM( 'MN_BOX'  ) }
      w_[ 3 ] := {| | VouchMenuM( 'MN_FILL' ) }

      B_GETS HEADERS h_ VALUES v_ TITLE 'Configure Field' WHEN w_ INTO v_
      v_:= v_[ 1 ]

      ::scrVv2Obj( v_, o_ )
      EXIT

   CASE OBJ_O_TEXT
      v_:= iif( nObj > 0, ::scrObj2Vv( ::obj_[ nObj ] ), ::scrVrbBlank( OBJ_O_TEXT ) )
      h_:= ::scrVrbHeaders( OBJ_O_TEXT )
      w_:= afill( array( len( h_ ) ), {|| .T. } )

      B_GETS HEADERS h_ VALUES v_ TITLE 'Configure Field' WHEN w_ INTO v_
      v_:= v_[ 1 ]

      ::scrVv2Obj( v_, o_ )
      EXIT

   CASE OBJ_O_FIELD
      ::scrAddFld( nObj )
      EXIT

   ENDSWITCH

   ::lEdited := .t.

   RETURN SELF

/*----------------------------------------------------------------------*/

