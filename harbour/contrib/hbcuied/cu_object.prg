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
#include "hbclass.ch"

//----------------------------------------------------------------------//

CLASS CUIScreen

   DATA COBJECT                                   INIT ""
   DATA CRPT                                      INIT "Untitled"
   DATA NWHERE                                    INIT 1
   DATA NMODE                                     INIT 1
   DATA LMSG                                      INIT .F.

   DATA SECTORS_                                  INIT {}
   DATA DESIGN                                    INIT 1
   DATA FOR_ROWS                                  INIT { { 0, "" } }
   DATA TOP                                       INIT 1
   DATA LEFT                                      INIT 0
   DATA BOTTOM                                    INIT maxrow() - 2
   DATA RIGHT                                     INIT maxcol()
   DATA MODE                                      INIT 0
   DATA ROW_CUR                                   INIT 0
   DATA COL_CUR                                   INIT 0
   DATA ROW_REP                                   INIT 1
   DATA COL_REP                                   INIT 1
   DATA ROW_DIS                                   INIT -1
   DATA COL_DIS                                   INIT -1
   DATA ROW_MENU                                  INIT 0
   DATA ROW_RULER                                 INIT 0
   DATA STATUS_ROW                                INIT maxrow() - 1
   DATA STATUS_COL                                INIT 0
   DATA STATUS_COL_TO                             INIT maxcol()
   DATA COL_MAX                                   INIT 400
   DATA ROW_PREV                                  INIT 1
   DATA COL_PREV                                  INIT 1
   DATA REP_LINES                                 INIT 200
   DATA CLR_STATUS                                INIT "W+/BG"
   DATA CLR_TEXT                                  INIT "W+/B"
   DATA CLR_BOX                                   INIT "W/B"
   DATA CLR_FIELD                                 INIT 'W+/W'
   DATA CLR_HILITE                                INIT "GR+/BG"
   DATA CLR_WINDOW                                INIT "W+/BG"
   DATA CLR_RULER                                 INIT "N/W"
   DATA CLR_MENU                                  INIT "W+/B"
   DATA CLR_OVERALL                               INIT "N/W"
   DATA CLR_PREV                                  INIT "B/W"
   DATA CLR_SELECT                                INIT "GR+/N"
   DATA OBJ_HILITE                                INIT 0
   DATA OBJ_SELECTED                              INIT 0
   DATA RULER                                     INIT ""
   DATA DRAW_FILL                                 INIT "±±±±±±±±±"
   DATA OBJ_ID_                                   INIT { 'Bitmap','Line','Text','Field','Expression','BitMap' }
   DATA REFRESH                                   INIT OBJ_REFRESH_ALL
   DATA OBJ_COPIED                                INIT 0
   DATA BOX_SHAPE                                 INIT "ÚÄ¿³ÙÄÀ³"
   DATA CHR_PREV                                  INIT ""
   DATA DESIGN_ID                                 INIT "Module"
   DATA FILE                                      INIT "Untitled"
   DATA PROPERTY                                  INIT {}
   DATA GRAPHICS                                  INIT .f.
   DATA TEXT_BLOCK_                               INIT {}
   DATA FIELDS                                    INIT {}
   DATA LASTKEY                                   INIT 0

   METHOD new()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD CUIScreen:new()

   RETURN Self

/*----------------------------------------------------------------------*/

CLASS CUIObject

   ENDCLASS

/*----------------------------------------------------------------------*/

CLASS hbCUIEditor

   DATA cSource                                   INIT ""
   DATA cScreen                                   INIT ""
   
   DATA obj_                                      INIT {}
   DATA scn_
   DATA rpt_                                      INIT { { "", 0, "" } }

   DATA nCurObj

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
   METHOD scrMovPgUp()
   METHOD scrIsObjTxt()
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
   METHOD scrTxtProp( nObj )
   METHOD scrOnLastCol( nObj )
   METHOD scrOnFirstCol( nObj, type_ )
   METHOD scrGetChar( nRow, nCol )
   METHOD scrTextBlock()
   METHOD scrTextMove( nMode )
   METHOD scrTextPost( gst_, nMode )
   METHOD scrTextDel()
   
   METHOD scrLoad( cSource, cScreen )
   METHOD scrSave()
   
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
   METHOD scrVvBlank()
   METHOD scrVvSelAble()
   METHOD scrObj2Vv( o_ )
   METHOD scrVv2Obj( v_, o_ )

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

   ::scn_:= CUIScreen():new() 
   
   ::scrLoad( ::cSource, ::cScreen )

   ::scrConfig()

   ::operate()

   RETURN SELF
   
/*----------------------------------------------------------------------*/
      
METHOD hbCUIEditor:destroy()
   RETURN NIL 
   
/*----------------------------------------------------------------------*/
    
METHOD hbCUIEditor:scrLoad( cSource, cScreen )

   IF empty( cSource ) .OR. ! hb_fileExists( cSource )
      aadd( ::obj_, ::scrObjBlank() )
      RETURN SELF
   ENDIF    

   ::cSource       := cSource
   ::cScreen       := iif( empty( cScreen ), 'Untitled', cScreen )
      
   ::scn_:FILE     := ::cScreen
   ::scn_:COBJECT  := ::cSource
   ::scn_:CRPT     := ::cScreen
   ::scn_:NWHERE   := 1
   ::scn_:NMODE    := DGN_MODULE
   ::scn_:LMSG     := .f.

   RETURN Self 

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrSave()

   IF empty( ::cSource )
      ::cSource := VouchGetSome( "Source (.PRG) File", space( 40 ) )
      IF empty( ::cSource )
         RETURN NIL    
      ENDIF    
   ENDIF 
   
   IF empty( ::cScreen )
      ::cScreen := VouchGetSome( "Screen Identity?", space( 13 ) )
      IF empty( ::cScreen )
         ::cScreen := dtos( date() ) + left( time(), 5 )
      ENDIF       
   ENDIF 
         
   ::scn_:FILE     := ::cScreen
   ::scn_:COBJECT  := ::cSource
   
   #if 0
   LOCAL rpt_:={}
   aeval( ::obj_, {|e_| iif( e_[ OBJ_ROW ] == 0, NIL, aadd( rpt_, { '', 0, scrObj2str( e_ ) } ) ) } )
   
   IF ! empty( ::scn_:PROPERTY )
      aadd(rpt_, { '', 51, prpMdl2Str( ::scn_:PROPERTY ) } )
   ENDIF
   #endif
   #if 0
   IF !empty( ::scn_:FIELDS )
      FOR i := 1 TO len( ::scn_:FIELDS )
         aadd( rpt_,{ '', ::scn_:FIELDS[ i,1 ], prpFld2Str( ::scn_:FIELDS[ i ] ) } )
      NEXT
   ENDIF
   #endif

   RETURN Self

//----------------------------------------------------------------------//

METHOD hbCUIEditor:operate()
   LOCAL nObj 
   LOCAL grf_:= { 43,45,46,48,49,50,51,52,53,54,55,56,57 }

   readinsert( .t. )

   ::scrDisplay()
   ::scrMove()
   ::scrStatus()
   
   keyboard( chr( K_UP ) )

   DO WHILE .t.
      ::scn_:ROW_PREV := ::scn_:ROW_CUR 
      ::scn_:COL_PREV := ::scn_:COL_CUR
      ::scn_:REFRESH  := OBJ_REFRESH_NIL

      setCursor( .t. )
      setCursor( iif( readInsert(), 2, 1 ) )

      DO WHILE .t.
         ::scn_:LASTKEY := inkey( 0, INKEY_ALL + HB_INKEY_GTEVENT )
         IF ::scn_:LASTKEY <> K_MOUSEMOVE
            EXIT
         ENDIF
      ENDDO

      DO CASE
      CASE ::scn_:GRAPHICS .AND. ascan( grf_,::scn_:LASTKEY ) > 0
         //processkey()
      CASE ::scrMouse()
#IF 0
      CASE ::scn_:LASTKEY == K_ALT_F6
         graphChar()
         ::scn_:GRAPHICS := ! ::scn_:GRAPHICS
         ::scn_:REFRESH  := OBJ_REFRESH_ALL
#ENDIF
      /*  Save Report */
      CASE ::scn_:LASTKEY == K_ESC
         IF alert( "Do you want to exit ?", { "Yes","No" } ) == 1
            EXIT
         ENDIF    
      CASE ::scn_:LASTKEY == K_CTRL_ENTER
         IF alert( "Do you want TO save screen ?", { "Yes","No" } ) == 1
            ::scrSave()
         ENDIF    
         EXIT
         
      CASE ::scn_:LASTKEY == K_ALT_S
         ::scrSave()

      CASE ::scn_:LASTKEY == K_RIGHT
         ::scrMovRgt()
      CASE ::scn_:LASTKEY == K_LEFT
         ::scrMovLft()
      CASE ::scn_:LASTKEY == K_UP
         ::scrMovUp()
      CASE ::scn_:LASTKEY == K_DOWN
         ::scrMovDn()
      CASE ::scn_:LASTKEY == K_MWBACKWARD
         ::scrMovDn()
      CASE ::scn_:LASTKEY == K_MWFORWARD
         ::scrMovUp()
      CASE ::scn_:LASTKEY == K_HOME
         ::scn_:COL_REP := 1
         ::scn_:COL_CUR := ::scn_:LEFT
         ::scn_:COL_DIS := ::scn_:LEFT - 1
         ::scn_:REFRESH := OBJ_REFRESH_ALL
      CASE ::scn_:LASTKEY == K_END
         ::scn_:COL_REP := ::scn_:COL_MAX
         ::scn_:COL_CUR := ::scn_:RIGHT
         ::scn_:COL_DIS := ( ::scn_:LEFT - 1 ) - ( ::scn_:COL_REP - ( ::scn_:RIGHT - ::scn_:LEFT + 1 ) )
         ::scn_:REFRESH := OBJ_REFRESH_ALL
      CASE ::scn_:LASTKEY == K_PGUP
         //  scrMovPgUp(scn_)
         ::scn_:ROW_REP := 1
         ::scn_:ROW_CUR := ::scn_:TOP
         ::scn_:ROW_DIS := ::scn_:TOP - 1
         ::scn_:REFRESH := OBJ_REFRESH_ALL
      CASE ::scn_:LASTKEY == K_PGDN
         //  ::scn_:ROW_REP := ::scn_:REP_LINES
         //  ::scn_:ROW_CUR := ::scn_:BOTTOM

      CASE ::scn_:LASTKEY == K_INS
         readInsert( !readInsert() )
         setcursor( iif( readInsert(), 2, 1 ) )

      CASE ::scn_:LASTKEY == K_ENTER
         IF ::scn_:MODE == OBJ_MODE_SELECT .AND. ::scn_:OBJ_SELECTED > 0
            ::obj_[ ::scn_:OBJ_SELECTED, OBJ_SECTION ] := 1
            ::scn_:COL_MAX      := max( ::scn_:COL_MAX, ::obj_[ ::scn_:OBJ_SELECTED, OBJ_TO_COL ] + 1 )
            ::scn_:MODE         := OBJ_MODE_IDLE
            ::scn_:REFRESH      := OBJ_REFRESH_LINE
            ::scn_:OBJ_SELECTED := 0
            ::scrMsg()
         ENDIF

      CASE VouchInRange( ::scn_:LASTKEY, K_SPACE, 254 ) .AND. ::scn_:MODE <> OBJ_MODE_SELECT
         ::scrAddTxt( 1 )

      CASE ::scn_:LASTKEY == K_F1                           //  Help
         help( 'NWREPORT' )
      CASE ::scn_:LASTKEY == K_F3                           //  OBJECT
         ::scrObject()
      CASE ::scn_:LASTKEY == K_F4                           //  Properties
         ::scrProperty()
      CASE ::scn_:LASTKEY == K_F7                           //  Copy
         ::scrObjCopy()
      CASE ::scn_:LASTKEY == K_F8                           //  Paste
         ::scrObjPas()
      CASE ::scn_:LASTKEY == K_F9                           //  Box
         ::scrAddBox()
      CASE ::scn_:LASTKEY == K_F10                          //  Fields
         ::scrAddFld()
      CASE ::scn_:LASTKEY == K_DEL
         IF ! empty( ::scn_:TEXT_BLOCK_ )
            ::scrTextDel()
            ::scrOrdObj()
            ::scn_:MODE         := 0
            ::scn_:OBJ_SELECTED := 0
            ::scn_:OBJ_HILITE   := 0
            ::scn_:REFRESH      := OBJ_REFRESH_ALL
         ELSEIF ::scrIsObjTxt()
            ::scrAddTxt( 2 )
         ELSEIF ::scn_:MODE == OBJ_MODE_SELECT
            ::scrObjDel( ::scn_:OBJ_SELECTED )
            ::scn_:MODE         := 0
            ::scn_:OBJ_SELECTED := 0
         ELSEIF ::scn_:OBJ_HILITE > 0
            ::scrObjDel( ::scn_:OBJ_HILITE )
            ::scn_:MODE         := 0
            ::scn_:OBJ_SELECTED := 0
            ::scn_:OBJ_HILITE   := 0
            ::scn_:REFRESH      := OBJ_REFRESH_ALL
         ENDIF

      CASE ::scn_:LASTKEY == K_BS
         IF ::scn_:MODE <> OBJ_MODE_SELECT
            IF ::scrMovLft()
               IF ::scrIsObjTxt()
                  ::scrAddTxt( 3 )
               ENDIF
            ENDIF
         ENDIF

      CASE ::scn_:LASTKEY == K_ALT_N
         ::scrAddLine()
      CASE ::scn_:LASTKEY == K_ALT_O
         ::scrDelLine()
      CASE ::scn_:LASTKEY == K_ALT_W
         ::scrRepCol()
      CASE ::scn_:LASTKEY == K_CTRL_F6    //  Selection of Block
         ::scrTextBlock()
      CASE ::scn_:LASTKEY == K_CTRL_F7    //  Move, Copy
         ::scrTextMove( 1 )
      CASE ::scn_:LASTKEY == K_CTRL_F8    //  Move, Cut AND Paste
         ::scrTextMove( 0 )
      CASE ::scn_:LASTKEY == HB_K_RESIZE
         ::scrReConfig()
         ::scrDisplay()
         ::scrMove()
         ::scrStatus()

         
      ENDCASE

      IF ::scn_:MODE    == OBJ_MODE_SELECT
         ::scn_:REFRESH := iif( ::scn_:REFRESH == OBJ_REFRESH_NIL, OBJ_REFRESH_LINE, ::scn_:REFRESH )
         ::scrUpdObjRC()
      ENDIF

      //  Check on which OBJECT cursor is placed
      //
      nObj := ::scrChkObj()

      IF nObj > 0 .AND. ::scn_:MODE <> OBJ_MODE_SELECT
         ::scn_:REFRESH    := iif( ::scn_:REFRESH == OBJ_REFRESH_NIL, OBJ_REFRESH_LINE, ::scn_:REFRESH )
         ::scn_:OBJ_HILITE := nObj
         ::scrOnFirstCol( nObj, { OBJ_O_FIELD, OBJ_O_EXP } )

      ELSEIF ! empty( ::scn_:OBJ_HILITE )
         ::scn_:REFRESH    := iif( ::scn_:REFRESH == OBJ_REFRESH_NIL, OBJ_REFRESH_LINE, ::scn_:REFRESH )
         ::scn_:OBJ_HILITE := 0

      ENDIF

      IF nObj > 0 .AND. ::scn_:LASTKEY == K_F5 
         SWITCH ::obj_[ nObj, OBJ_TYPE ]
         CASE OBJ_O_FIELD
            ::scrAddFld( nObj ) ; EXIT 
         CASE OBJ_O_TEXT
            ::scrTxtProp( nObj ); EXIT 
         CASE OBJ_O_BOX
            ::scrAddBox( nObj ) ; EXIT 
         ENDSWITCH 
      ENDIF

      //  Is the OBJECT selected
      IF nObj > 0 .AND. ::scn_:LASTKEY == K_F6 .AND. ::obj_[ nObj,OBJ_TYPE ] == OBJ_O_BOX
         ::scn_:MODE         := OBJ_MODE_SELECT
         ::scn_:OBJ_SELECTED := nObj
         ::scrOnFirstCol( nObj, { OBJ_O_BOX } )
         ::scrMsg( "Box is Selected. Use Arrow Keys to Move, Enter to Finish !" )

      ELSEIF nObj > 0 .AND. ::scn_:LASTKEY == K_F6 .AND. ! ( ::obj_[ nObj,OBJ_TYPE ] == OBJ_O_BOX )
         ::scn_:MODE         := OBJ_MODE_SELECT
         ::scn_:OBJ_SELECTED := nObj
         ::scrOnFirstCol( nObj, { OBJ_O_TEXT } )
         ::scrMsg( "OBJECT is Selected. Use Arrow Keys to Move, Enter to Finished" )

      ENDIF

      IF     ::scn_:REFRESH == OBJ_REFRESH_ALL
         ::scrMove()
      ELSEIF ::scn_:REFRESH == OBJ_REFRESH_LINE
         IF ::scrIsBoxIn()
            ::scrMove()
         ELSE
            ::scrMoveLine()
         ENDIF
      ENDIF

      ::scrStatus()

      IF ::scn_:GRAPHICS
         //grfRest()
      ENDIF
   ENDDO

   ::scrOrdObj()

   RETURN Self 

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrDisplay()

   dispbegin()
   setcursor(0)
   setColor( ::scn_:CLR_OVERALL )
   cls

   @ ::scn_:ROW_RULER, ::scn_:LEFT ;
   SAY substr( ::scn_:RULER, 1, ::scn_:RIGHT - ::scn_:LEFT + 1 ) ;
   COLOR ::scn_:CLR_RULER

   ::scrMsg()

   setcolor( ::scn_:CLR_WINDOW )
   setCursor(2)
   dispend()

   RETURN Self 
   
/*----------------------------------------------------------------------*/
   
METHOD hbCUIEditor:scrMove()
   LOCAL i
   LOCAL crs := setCursor( 0 )
   LOCAL nOff, cText, nRow, nCol, cColor

   dispBegin()

   dispBox(::scn_:TOP      ,;
           ::scn_:LEFT     ,;
           ::scn_:BOTTOM   ,;
           ::scn_:RIGHT    ,;
           ::scn_:DRAW_FILL,;
           ::scn_:CLR_PREV  )

   FOR i := 1 TO len( ::obj_ )
      IF ::obj_[ i,OBJ_ROW ] + ::scn_:ROW_DIS <= ::scn_:BOTTOM .AND. ;
         ::obj_[ i,OBJ_COL ] + ::scn_:COL_DIS <= ::scn_:RIGHT 

         nOff := ::obj_[ i,OBJ_COL ] + ::scn_:COL_DIS
         nRow := ::obj_[ i,OBJ_ROW ] + ::scn_:ROW_DIS
         nCol := ::obj_[ i,OBJ_COL ] + ::scn_:COL_DIS

         IF nOff < 0
            nCol := 0
         ENDIF

         IF ::obj_[ i,OBJ_TYPE ] == OBJ_O_BOX
            cColor := iif( ::scn_:OBJ_SELECTED == i, ::scn_:CLR_SELECT,;
                      iif( ::scn_:OBJ_HILITE   == i, ::scn_:CLR_HILITE,;
                                       'W/B' /* obj_[i,OBJ_COLOR] */ ) )
            DO CASE
            CASE VouchInArray( ::obj_[ i,OBJ_MDL_F_TYPE ], { 61,62,63,67,68 } )
               dispBox( ::obj_[ i,OBJ_ROW    ] + ::scn_:ROW_DIS,;
                        ::obj_[ i,OBJ_COL    ] + ::scn_:COL_DIS,;
                        ::obj_[ i,OBJ_TO_ROW ] + ::scn_:ROW_DIS,;
                        ::obj_[ i,OBJ_TO_COL ] + ::scn_:COL_DIS,;
                        substr( ::obj_[ i, OBJ_BOX_SHAPE ], 1, 8 ),;
                        cColor )

            CASE VouchInArray( ::obj_[ i, OBJ_MDL_F_TYPE ], { 64,65 } )    //  Line
               @  ::obj_[ i, OBJ_ROW    ] + ::scn_:ROW_DIS,;
                  ::obj_[ i, OBJ_COL    ] + ::scn_:COL_DIS ;
               TO ::obj_[ i, OBJ_TO_ROW ] + ::scn_:ROW_DIS,;
                  ::obj_[ i, OBJ_TO_COL ] + ::scn_:COL_DIS ;
               COLOR cColor

            ENDCASE
         ENDIF

         IF ::obj_[ i, OBJ_TYPE ] == OBJ_O_FIELD
            cText  := ::obj_[ i,OBJ_TEXT ]
            cColor := iif( ::scn_:OBJ_SELECTED == i, ::scn_:CLR_SELECT,;
                      iif( ::scn_:OBJ_HILITE   == i, ::scn_:CLR_HILITE,;
                                      'W+/W' /* obj_[i,OBJ_COLOR] */ ) )
            IF nOff < 0
               cText := substr( ::obj_[ i,OBJ_TEXT ], abs( nOff ) + 1 )
            ENDIF
            @ nRow, nCol SAY cText COLOR cColor
         ENDIF

         IF ::obj_[ i, OBJ_TYPE ] == OBJ_O_TEXT
            cText  := ::obj_[ i,OBJ_EQN ]
            cColor := iif( ::scn_:OBJ_SELECTED == i, ::scn_:CLR_SELECT,;
                         iif( empty( ::obj_[ i, OBJ_COLOR ] ), ::scn_:CLR_TEXT,;
                                           'W/B' /* obj_[i,OBJ_COLOR] */) )
            IF nOff < 0
               cText := substr( ::obj_[ i, OBJ_EQN ], abs( nOff ) + 1 )
            ENDIF

            @ nRow, nCol SAY cText COLOR cColor
         ENDIF
         
      ELSEIF ( ::obj_[ i, OBJ_ROW ] + ::scn_:ROW_DIS > ::scn_:BOTTOM )

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

   IF ::scn_:ROW_PREV == ::scn_:ROW_CUR
      dispbegin()
      dispBox( ::scn_:ROW_CUR  ,;
               ::scn_:LEFT     ,;
               ::scn_:ROW_CUR  ,;
               ::scn_:RIGHT    ,;
               ::scn_:DRAW_FILL,;
               ::scn_:CLR_PREV  )

      FOR i := 1 TO len( ::obj_ )
         nOff := ::obj_[ i, OBJ_COL ] + ::scn_:COL_DIS
         nRow := ::obj_[ i, OBJ_ROW ] + ::scn_:ROW_DIS
         nCol := nOff

         IF ::obj_[ i, OBJ_TYPE ] == OBJ_O_BOX
            DO CASE
            CASE VouchInArray( ::obj_[ i, OBJ_MDL_F_TYPE ], {64,65} )    //  Lines V.H
               @  ::obj_[ i, OBJ_ROW    ] + ::scn_:ROW_DIS,;
                  ::obj_[ i, OBJ_COL    ] + ::scn_:COL_DIS ;
               TO ::obj_[ i, OBJ_TO_ROW ] + ::scn_:ROW_DIS,;
                  ::obj_[ i, OBJ_TO_COL ] + ::scn_:COL_DIS ;
               COLOR iif( ::scn_:OBJ_HILITE == i, ::scn_:CLR_HILITE,;
                              'W/B' /* obj_[i,OBJ_COLOR] */ )
            ENDCASE
         ENDIF

         IF ::obj_[ i, OBJ_ROW ] == ::scn_:ROW_REP
            IF ::obj_[ i, OBJ_TYPE ] == OBJ_O_FIELD
               cText := ::obj_[ i,OBJ_TEXT ]
               cColor := iif( ::scn_:OBJ_SELECTED == i, ::scn_:CLR_SELECT,;
                         iif( ::scn_:OBJ_HILITE   == i, ::scn_:CLR_HILITE,;
                                           'W+/W' /* obj_[i,OBJ_COLOR] */ ))
               @ nRow, nCol SAY cText COLOR cColor
            ENDIF

            IF ::obj_[ i, OBJ_TYPE ] == OBJ_O_TEXT
               cText  := ::obj_[ i, OBJ_EQN ]
               cColor := iif( ::scn_:OBJ_SELECTED == i, ::scn_:CLR_SELECT,;
                         iif( empty( ::obj_[ i, OBJ_COLOR ] ), ::scn_:CLR_TEXT,;
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

   IF ! empty( ::scn_:TEXT_BLOCK_ )
      DispBegin()

      FOR i := ::scn_:TEXT_BLOCK_[ 1 ] TO ::scn_:TEXT_BLOCK_[ 3 ]
         IF ( nRow := i + ::scn_:ROW_DIS ) <= ::scn_:BOTTOM
            FOR j := ::scn_:TEXT_BLOCK_[ 2 ] TO ::scn_:TEXT_BLOCK_[ 4 ]
               IF ( nCol := j + ::scn_:COL_DIS ) <= ::scn_:RIGHT
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
      IF ( nRow := i + ::scn_:ROW_DIS ) <= ::scn_:BOTTOM
         FOR j := gst_[ 2 ] TO gst_[ 4 ]
            IF ( nCol := j + ::scn_:COL_DIS ) <= ::scn_:RIGHT
               @ nRow, nCol SAY THE_FILL COLOR 'GR+/R'
            ENDIF
         NEXT
      ENDIF
   NEXT
   DispEnd()

   RETURN NIL

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrStatus()
   LOCAL s, typ_, objId, cClr := 'GR+/BG'

   dispbegin()
   s := pad( ::scn_:DESIGN_ID, 8 )+ ' ³ '

   s += pad( ::scn_:FILE, 12 )+;
             ' ³ '+;
             ' R:'+;
             str( ::scn_:ROW_REP - 1, 3 )+;
             ' C:'+;
             str( ::scn_:COL_REP - 1, 3 )+;
             ' ³ ' +;
             iif( readInsert(), 'Ins ', '    ')+;
             ' ³ '

   objId := ''
   IF ::scn_:OBJ_HILITE > 0
      objId := ::scn_:OBJ_ID_[ ::obj_[ ::scn_:OBJ_HILITE, OBJ_TYPE ] ]
      IF ::obj_[ ::scn_:OBJ_HILITE, OBJ_TYPE ] == OBJ_O_BOX
         typ_:= { 'Bitmap', 'Frame', 'Ellipse', 'Line (H)', 'Line (V)', 'Grid', 'BarCode', 'Text Box' }
         objId := typ_[ ::obj_[ ::scn_:OBJ_HILITE, OBJ_MDL_F_TYPE ] - 60 ]
      ENDIF
      
   ELSEIF ::scn_:OBJ_SELECTED > 0
      objId := ::scn_:OBJ_ID_[ ::obj_[ ::scn_:OBJ_SELECTED, OBJ_TYPE ] ]
      IF ::obj_[ ::scn_:OBJ_SELECTED, OBJ_TYPE ] == OBJ_O_BOX
         typ_:= {'Bitmap','Frame','Ellipse','Line (H)','Line (V)','Grid','BarCode','Text Box'}
         objId := typ_[ ::obj_[ ::scn_:OBJ_SELECTED, OBJ_MDL_F_TYPE ] - 60 ]
      ENDIF
   ENDIF

   s += pad( trim( objId ), 10 ) + ' ³ '
   
   @ ::scn_:STATUS_ROW, ::scn_:STATUS_COL ;
   SAY pad( s, ::scn_:STATUS_COL_TO - ::scn_:STATUS_COL + 1 ) ;
   COLOR ::scn_:CLR_STATUS

   /* Ruler */
   s := substr( ::scn_:RULER, max( 1, ::scn_:COL_REP - ::scn_:COL_CUR + ::scn_:LEFT ), ::scn_:RIGHT - ::scn_:LEFT + 1 )
   DispBox( ::scn_:TOP - 1, 0, ::scn_:TOP - 1, maxcol(), '         ', ::scn_:CLR_OVERALL )
   @ ::scn_:ROW_RULER, ::scn_:LEFT SAY s COLOR ::scn_:CLR_RULER
   @ ::scn_:ROW_RULER, ::scn_:COL_CUR SAY substr( s, ::scn_:COL_CUR - ::scn_:LEFT + 1, 1 ) COLOR cClr

   @ ::scn_:ROW_CUR, ::scn_:COL_CUR SAY ""

   //  Now update current cursor position
   ::scn_:ROW_PREV := ::scn_:ROW_CUR
   ::scn_:COL_PREV := ::scn_:COL_CUR

   dispend()

   RETURN Self 
   
/*----------------------------------------------------------------------*/

METHOD hbCUIEditor:scrMouse()
   LOCAL nmRow, nmCol
   LOCAL nEvent := ::scn_:LASTKEY
   LOCAL aEvents_:= { K_LBUTTONUP, K_LBUTTONDOWN, K_MMLEFTDOWN }

   STATIC nLastCol  := 0
   STATIC nLastRow  := 0
   STATIC lAnchored := .f.
   STATIC nCursor

   IF ! VouchInArray( ::scn_:LASTKEY, aEvents_ ) 
      RETURN .f.
   ENDIF

   nmRow := mRow()
   nmCol := mCol()

   IF nmRow < ::scn_:TOP .OR. nmRow > ::scn_:BOTTOM .OR. ;
                   nmCol < ::scn_:LEFT .OR. nmCol > ::scn_:RIGHT
      RETURN .f.
   ENDIF

   ::scrToMouse( nmRow, nmCol )

   IF nEvent == K_LDBLCLK

   ELSEIF nEvent == K_MMLEFTDOWN /*K_LBUTTONDOWN */ .AND. !( lAnchored )
      IF ::scrChkObj() > 0 .AND. ::scn_:MODE <> OBJ_MODE_SELECT
         nCursor := SetCursor( 0 )

         lAnchored := .t.
         ::scn_:LASTKEY := K_F6
//         Wvt_SetMousePos( ::scn_:ROW_CUR, ::scn_:COL_CUR )
      ENDIF

   ELSEIF nEvent == K_MMLEFTDOWN .AND. lAnchored

   ELSEIF nEvent == K_LBUTTONUP  .AND. lAnchored
//      Wvt_SetMousePos( ::scn_:ROW_CUR, ::scn_:COL_CUR )
      SetCursor( nCursor )
      lAnchored := .f.
      __keyboard( chr( K_ENTER ) )

   ELSEIF nEvent == K_LBUTTONUP

   ENDIF

   RETURN .t.
   
/*----------------------------------------------------------------------*/
   
METHOD hbCUIEditor:scrToMouse( nmRow, nmCol )
   LOCAL nRowOff, nColOff

   nRowOff := nmRow - ::scn_:ROW_CUR
   IF nRowOff <> 0
      ::scn_:ROW_CUR += nRowOff
      ::scn_:ROW_REP += nRowOff
   ENDIF

   nColOff := nmCol - ::scn_:COL_CUR
   IF nColOff <> 0
      ::scn_:COL_CUR += nColOff
      ::scn_:COL_REP += nColOff
   ENDIF

   RETURN NIL

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrOrdObj()

   //  Objects are ordered as per their type
   asort( ::obj_, , , {|e_,f_| e_[ OBJ_TYPE ] < f_[ OBJ_TYPE ] } )

   RETURN Self

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrMovRgt()
   LOCAL lMoved := .t.

   ::scn_:COL_CUR++
   IF ::scn_:COL_CUR > ::scn_:RIGHT
      IF ::scn_:COL_MAX > ::scn_:COL_REP
         ::scn_:COL_DIS--
         ::scn_:COL_CUR--
         ::scn_:COL_REP++
         ::scn_:REFRESH := OBJ_REFRESH_ALL
      ELSE
         lMoved := .f.
         tone( 100,1 )
         ::scn_:COL_CUR--
      ENDIF
   ELSE
      ::scn_:COL_REP++
   ENDIF
   RETURN lMoved

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrMovLft()
   LOCAL lMoved := .t.
   ::scn_:COL_CUR--
   IF ::scn_:COL_CUR < ::scn_:LEFT
      IF ::scn_:COL_REP > 1
         ::scn_:COL_DIS++
         ::scn_:COL_CUR++
         ::scn_:COL_REP--
         ::scn_:REFRESH := OBJ_REFRESH_ALL
      ELSE
         lMoved := .f.
         tone(200,1)
         ::scn_:COL_CUR++
      ENDIF
   ELSE
      ::scn_:COL_REP--
   ENDIF
   RETURN lMoved

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrMovUp()
   LOCAL lMoved := .t.

   ::scn_:ROW_CUR--
   IF ::scn_:ROW_CUR < ::scn_:TOP
      ::scn_:ROW_CUR := ::scn_:TOP
      IF ::scn_:ROW_REP > 1
         ::scn_:ROW_DIS++
         ::scn_:ROW_REP--
         ::scn_:REFRESH := OBJ_REFRESH_ALL
      ELSE
         lMoved := .f.
         tone(300,1)
      ENDIF
   ELSE
      ::scn_:ROW_REP--
   ENDIF
   RETURN lMoved

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrMovDn()
   LOCAL lMoved := .t.

   ::scn_:ROW_CUR++
   IF ::scn_:ROW_CUR  > ::scn_:BOTTOM
      ::scn_:ROW_CUR := ::scn_:BOTTOM
      IF ::scn_:ROW_REP < ::scn_:REP_LINES
         ::scn_:ROW_DIS--
         ::scn_:ROW_REP++
         ::scn_:REFRESH := OBJ_REFRESH_ALL
      ELSE
         lMoved := .f.
         tone( 300,1 )
      ENDIF
   ELSE
      ::scn_:ROW_REP++
   ENDIF
   RETURN lMoved

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrMovPgUp()
   LOCAL lMoved := .f.

   IF ::scn_:ROW_CUR == ::scn_:TOP
      IF ::scn_:ROW_REP > 1
         ::scn_:ROW_CUR := ::scn_:TOP
         ::scn_:ROW_REP := 1
         ::scn_:ROW_DIS := ::scn_:TOP - 1
         lMoved := .t.
         ::scn_:REFRESH := OBJ_REFRESH_ALL
      ENDIF
   ELSE                    //  IF ::scn_:ROW_CUR == ::scn_:ROW_BOTTOM]
      ::scn_:ROW_CUR := ::scn_:TOP
      ::scn_:ROW_REP := ::scn_:ROW_REP - ( ::scn_:ROW_CUR - ::scn_:TOP )
      ::scn_:ROW_DIS := ::scn_:ROW_DIS - ( ::scn_:ROW_CUR - ::scn_:TOP )
      lMoved := .t.
   ENDIF

   RETURN lMoved

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrIsObjTxt()
   RETURN ascan( ::obj_, {|e_| e_ [OBJ_TYPE ] == OBJ_O_TEXT;
         .AND. ;
         VouchInRange( ::scn_:ROW_REP, e_[ OBJ_ROW ], e_[ OBJ_TO_ROW ] ) ;
         .AND. ;
         VouchInRange( ::scn_:COL_REP, e_[ OBJ_COL ], e_[ OBJ_TO_COL ] ) } ) > 0

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrChkObj()
   LOCAL n
   n := ascan( ::obj_, {|e_| iif( e_[ OBJ_TYPE ] == OBJ_O_BOX, .f.,;
         VouchInRange( ::scn_:ROW_REP, e_[ OBJ_ROW ], e_[OBJ_TO_ROW ] ) ;
         .AND. ;
         VouchInRange( ::scn_:COL_REP, e_[ OBJ_COL ], e_[ OBJ_TO_COL ] ) ) } )
   IF empty( n )  //  No OBJECT other than box, check box,BMP
      n := ascan( ::obj_,{|e_| ;
         VouchInRange( ::scn_:ROW_REP, e_[ OBJ_ROW ], e_[ OBJ_TO_ROW ] ) ;
         .AND. ;
         VouchInRange( ::scn_:COL_REP, e_[ OBJ_COL ], e_[ OBJ_TO_COL ] ) } )
   ENDIF
   RETURN n

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrUpdObjRC()
   LOCAL nW, nH
   LOCAL nObj := ::scn_:OBJ_SELECTED

   IF nObj > 0
      nH := ::obj_[ nObj,OBJ_TO_ROW ] - ::obj_[ nObj,OBJ_ROW ]
      nW := ::obj_[ nObj,OBJ_TO_COL ] - ::obj_[ nObj,OBJ_COL ]

      ::obj_[ nObj,OBJ_ROW    ] := ::scn_:ROW_REP
      ::obj_[ nObj,OBJ_COL    ] := ::scn_:COL_REP

      IF ::obj_[ nObj,OBJ_TYPE ] == OBJ_O_BOX
         ::obj_[ nObj,OBJ_TO_ROW ] := ::obj_[ nObj,OBJ_ROW ] + nH
         ::obj_[ nObj,OBJ_TO_COL ] := ::obj_[ nObj,OBJ_COL ] + nW
      ELSE
         ::obj_[ nObj,OBJ_TO_ROW ] := ::scn_:ROW_REP
         ::obj_[ nObj,OBJ_TO_COL ] := ::scn_:COL_REP + ;
                  len( ::obj_[ nObj, iif( ::obj_[ nObj,OBJ_TYPE ] == OBJ_O_TEXT, OBJ_EQN, OBJ_TEXT ) ] ) - 1
      ENDIF
   ENDIF
   RETURN NIL

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrRepCol()
   LOCAL oCol := ::scn_:COL_MAX, nCol

   nCol := VouchGetSome( 'Number of Columns?', oCol )

   IF !empty( nCol )
      nCol := max( 10,nCol )
      ::scn_:COL_MAX             := nCol
      ::scn_:RIGHT               := min( maxCol(), ::scn_:LEFT + nCol - 1 )
      ::scn_:REFRESH             := OBJ_REFRESH_ALL
//      ::scn_:PROPERTY[ REP_COLS ]:= nCol
   ENDIF

   RETURN NIL

//----------------------------------------------------------------------//
/*
   This is the routine FROM where row based equations can be implemented
*/
METHOD hbCUIEditor:scrAddLine()
   LOCAL nRow := ::scn_:ROW_REP, nSct

   ::scn_:REFRESH := OBJ_REFRESH_ALL
   ::scn_:BOTTOM  := min( ::scn_:BOTTOM + 1, maxrow() - 3 )

   nSct := 1

   ::scn_:SECTORS_[ nSct, SCT_ROWS ]++
   ::scn_:REP_LINES++

   aeval( ::obj_, {|e_,i| iif( e_[ OBJ_ROW ] >= nRow, ::obj_[ i, OBJ_TO_ROW ] += 1, '' ) } )
   aeval( ::obj_, {|e_,i| iif( e_[ OBJ_ROW ] >= nRow, ::obj_[ i, OBJ_ROW    ] += 1, '' ) } )

   ::scn_:REFRESH := OBJ_REFRESH_ALL

   RETURN NIL

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrDelLine()
   LOCAL nRow := ::scn_:ROW_REP
   LOCAL nSct, n, isLast

   isLast := nRow == ::scn_:REP_LINES

   nSct   := 1
   IF ::scn_:SECTORS_[ nSct, SCT_ROWS ] == 1    //  A Single Row Must remain IN one group
      RETURN NIL
   ENDIF

   ::scn_:SECTORS_[ nSct, SCT_ROWS ]--
   ::scn_:REP_LINES--

   IF ::scn_:REP_LINES < ( ::scn_:BOTTOM - ::scn_:TOP + 1 )
      ::scn_:BOTTOM := max( ::scn_:TOP, min( ::scn_:BOTTOM - 1, maxrow() - 3 ) )
   ENDIF

   DO WHILE .t.
      IF ( n := ascan( ::obj_, {|e_| e_[ OBJ_ROW ] == nRow } ) ) == 0
         EXIT
      ENDIF
      VouchAShrink( ::obj_, n )
   ENDDO
   IF empty( ::obj_ )
      aadd( ::obj_, ::scrObjBlank() )
   ENDIF

   aeval( ::obj_, {|e_,i| iif( e_[ OBJ_ROW ] > nRow, ::obj_[ i, OBJ_TO_ROW ] -= 1, '' ) } )
   aeval( ::obj_, {|e_,i| iif( e_[ OBJ_ROW ] > nRow, ::obj_[ i, OBJ_ROW    ] -= 1, '' ) } )

   IF isLast
      ::scn_:ROW_REP--
      ::scn_:ROW_CUR--
   ENDIF

   ::scn_:REFRESH := OBJ_REFRESH_ALL

   RETURN NIL

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrIsBoxIn()
   RETURN ascan( ::obj_,{|e_| VouchInRange( ::scn_:ROW_REP, e_[ OBJ_ROW ], e_[ OBJ_TO_ROW ] );
                                     .AND. ;
                            ( e_[ OBJ_TYPE ] == OBJ_O_BOX ) } )    >    0

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrObjCopy()

   IF ::scn_:MODE == OBJ_MODE_SELECT
      ::scn_:OBJ_COPIED := ::scn_:OBJ_SELECTED
   ELSEIF ::scn_:OBJ_HILITE > 0
      ::scn_:OBJ_COPIED := ::scn_:OBJ_HILITE
   ENDIF
   RETURN NIL

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrObjPas()       //  Paste Copied OBJECT
   LOCAL nObj, o_, oldRow, oldCol, oldRow2, oldcol2

   IF ( nObj := ::scn_:OBJ_COPIED ) > 0 .AND. ::scn_:OBJ_SELECTED == 0
      o_:= aclone( ::obj_[ nObj ] )

      oldRow  := o_[ OBJ_ROW    ] ; oldCol  := o_[ OBJ_COL    ]
      oldRow2 := o_[ OBJ_TO_ROW ] ; oldCol2 := o_[ OBJ_TO_COL ]

      o_[ OBJ_ROW ]         := ::scn_:ROW_REP
      o_[ OBJ_COL ]         := ::scn_:COL_REP
      IF o_[ OBJ_TYPE   ]   == OBJ_O_FIELD 
         o_[ OBJ_TO_ROW ]   := ::scn_:ROW_REP
         o_[ OBJ_TO_COL ]   := ::scn_:COL_REP + len( o_[ OBJ_TEXT ] ) - 1
      ELSEIF o_[ OBJ_TYPE ] == OBJ_O_BOX 
         o_[ OBJ_TO_ROW ]   := ::scn_:ROW_REP + ( oldRow2 - oldRow )
         o_[ OBJ_TO_COL ]   := ::scn_:COL_REP + ( oldCol2 - oldCol )
      ELSEIF o_[ OBJ_TYPE ] == OBJ_O_TEXT
         o_[ OBJ_TO_ROW]    := ::scn_:ROW_REP
         o_[ OBJ_TO_COL ]   := ::scn_:COL_REP + ( oldCol2 - oldCol )
      ENDIF
      o_[ OBJ_SECTION ]     := 1

      aadd( ::obj_, o_ )

      ::scrOrdObj()
      ::scn_:OBJ_SELECTED  := 0
      ::scn_:REFRESH       := OBJ_REFRESH_LINE
      ::scn_:MODE          := 0
      ::scn_:OBJ_COPIED    := 0
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
   ::scn_:OBJ_SELECTED := 0
   ::scn_:REFRESH      := OBJ_REFRESH_LINE

   IF nUnique > 0
      IF ( n := ascan( ::scn_:FIELDS, {|e_| e_[1] == nUnique } ) ) > 0
         VouchAShrink( ::scn_:FIELDS, n )
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

   @ ::scn_:ROW_CUR, ::scn_:COL_CUR SAY ''

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
      ::scn_:GRAPHICS := ! ::scn_:GRAPHICS
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

METHOD hbCUIEditor:scrTxtProp( nObj )
   LOCAL sel_, v_

   ::obj_[ nObj,OBJ_F_LEN  ] := len( ::obj_[ nObj,OBJ_EQN ] )
   ::obj_[ nObj,OBJ_F_TYPE ] := 'C'

   v_:= ::scrObj2Vv( ::obj_[ nObj ] )
   sel_:= ::scrVvSelAble()

   sel_[ VV_ID        ] := .F.
   sel_[ VV_ALIGN     ] := .T.
   sel_[ VV_COLOR     ] := .T.
   sel_[ VV_F_LEN     ] := .F.
   sel_[ VV_F_DEC     ] := .F.
   sel_[ VV_REPEATED  ] := .F.
   sel_[ VV_VERTICLE  ] := .F.
   sel_[ VV_WRAP_SEMI ] := .F.
   sel_[ VV_ZERO      ] := .F.
   sel_[ VV_EQN       ] := .F.

//   scrField( nObj, 3, ::obj_, ::scn_, v_, sel_, 'W/B    ' )

   RETURN v_

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrOnLastCol( nObj )
   LOCAL nOff, i

   IF ::obj_[ nObj, OBJ_TYPE ] == OBJ_O_BOX
      nOff := ::obj_[ nObj, OBJ_TO_COL ] - ::scn_:COL_CUR - 1
      FOR i := 1 TO nOff
         ::scrMovRgt()
         ::scrMove()
         ::scrStatus()
      NEXT

      nOff := ::obj_[ nObj, OBJ_TO_ROW ] - ::scn_:ROW_CUR - 1
      FOR i := 1 TO nOff
         ::scrMovDn()
         ::scrMove()
         ::scrStatus()
      NEXT

      SetPos( ::scn_:ROW_CUR, ::scn_:COL_CUR )
   ENDIF

   RETURN NIL

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrOnFirstCol( nObj, type_ )
   LOCAL nCur, nOff

   IF ::scn_:COL_REP <> ::obj_[ nObj,OBJ_COL ]
      IF VouchInArray( ::obj_[ nObj, OBJ_TYPE ], type_ )
         IF ::obj_[ nObj, OBJ_TYPE ] == OBJ_O_BOX
            nCur := ::scn_:COL_CUR
            nOff := ::scn_:COL_REP - ::obj_[ nObj, OBJ_COL ]
            ::scn_:COL_CUR := max( ::scn_:LEFT, ::scn_:COL_CUR - nOff )
            ::scn_:COL_REP := ::obj_[ nObj, OBJ_COL]
            IF nOff > nCur - ::scn_:LEFT
               ::scn_:REFRESH := OBJ_REFRESH_ALL
               ::scn_:COL_DIS += nOff - ( nCur - ::scn_:LEFT )
            ENDIF

            nCur := ::scn_:ROW_CUR
            nOff := ::scn_:ROW_REP - ::obj_[ nObj, OBJ_ROW ]
            ::scn_:ROW_CUR := max( ::scn_:TOP, ::scn_:ROW_CUR - nOff )
            ::scn_:ROW_REP := ::obj_[ nObj,OBJ_ROW ]
            IF nOff > nCur - ::scn_:TOP
               ::scn_:REFRESH := OBJ_REFRESH_ALL
               ::scn_:ROW_DIS += nOff - ( nCur - ::scn_:TOP )
            ENDIF

         ELSE
            IF ::scn_:LASTKEY == K_RIGHT
               nCur := ::scn_:COL_CUR
               nOff := ::obj_[ nObj, OBJ_TO_COL ] - ::scn_:COL_REP + 1 //  NEXT Col TO OBJECT
               IF ::scn_:COL_REP + nOff > ::scn_:COL_MAX
                  ::scn_:COL_MAX := ::scn_:COL_REP + nOff
               ENDIF
               ::scn_:COL_CUR := min( ::scn_:RIGHT, ::scn_:COL_CUR + nOff )
               ::scn_:COL_REP := ::obj_[ nObj,OBJ_TO_COL ] + 1
               IF nOff > ::scn_:RIGHT - nCur
                  ::scn_:REFRESH := OBJ_REFRESH_ALL
                  ::scn_:COL_DIS -= nOff - ( ::scn_:RIGHT - nCur )
               ENDIF
               ::scn_:OBJ_HILITE := 0
            ELSE
               nCur := ::scn_:COL_CUR 
               nOff := ::scn_:COL_REP - ::obj_[ nObj,OBJ_COL ]
               ::scn_:COL_CUR := max( ::scn_:LEFT, ::scn_:COL_CUR - nOff )
               ::scn_:COL_REP := ::obj_[ nObj,OBJ_COL ]
               IF nOff > nCur - ::scn_:LEFT
                  ::scn_:REFRESH := OBJ_REFRESH_ALL
                  ::scn_:COL_DIS += nOff - ( nCur - ::scn_:LEFT )
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
      IF     ::obj_[ n ,OBJ_TYPE ] == OBJ_O_TEXT
         s := substr( ::obj_[ n, OBJ_EQN ], nCol - ::obj_[ n, OBJ_COL ] + 1, 1 )

      ELSEIF ::obj_[n,OBJ_TYPE]==OBJ_O_FIELD .OR. ::obj_[n,OBJ_TYPE]==OBJ_O_EXP
         s := substr(::obj_[n,OBJ_ID ],nCol-::obj_[n,OBJ_COL]+1,1)

      ELSEIF ::obj_[n,OBJ_TYPE]==OBJ_O_BOX .OR. ::obj_[n,OBJ_TYPE]==OBJ_O_BMP
         IF     nRow == ::obj_[n,OBJ_ROW   ]
            IF     nCol == ::obj_[n,OBJ_COL]
               s := substr(::obj_[n,OBJ_BOX_SHAPE],1,1)
            ELSEIF nCol == ::obj_[n,OBJ_TO_COL]
               s := substr(::obj_[n,OBJ_BOX_SHAPE],3,1)
            ELSE
               s := substr(::obj_[n,OBJ_BOX_SHAPE],2,1)
            ENDIF
         ELSEIF nRow == ::obj_[n,OBJ_TO_ROW]
            IF     nCol == ::obj_[n,OBJ_COL]
               s := substr(::obj_[n,OBJ_BOX_SHAPE],7,1)
            ELSEIF nCol == ::obj_[n,OBJ_TO_COL]
               s := substr(::obj_[n,OBJ_BOX_SHAPE],5,1)
            ELSE
               s := substr(::obj_[n,OBJ_BOX_SHAPE],6,1)
            ENDIF
         ELSE
            IF     nCol == ::obj_[n,OBJ_COL]
               s := substr(::obj_[n,OBJ_BOX_SHAPE],8,1)   //  4.8 are Same
            ELSEIF nCol == ::obj_[n,OBJ_TO_COL]
               s := substr(::obj_[n,OBJ_BOX_SHAPE],4,1)
            ELSE
               s := substr(::obj_[n,OBJ_BOX_SHAPE],9,1)
               s := IF(empty(s),THE_FILL,s)
            ENDIF
         ENDIF
      ENDIF
   ENDIF
   RETURN s

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrTextBlock()
   LOCAL n, nKey
   LOCAL key_:= { K_RIGHT,K_LEFT,K_UP,K_DOWN,K_ENTER }

   ::scn_:TEXT_BLOCK_ := { ::scn_:ROW_REP, ::scn_:COL_REP, ::scn_:ROW_REP, ::scn_:COL_REP }

   ::scrMsg( 'Use <Arrow Keys> TO Select Text Block, <Enter> TO Finish' )
   ::scrMove()
   ::scrStatus()

   DO WHILE .t.
      nKey := ::scrInkey( key_ )

      DO CASE
      CASE nKey == key_[ 1 ]
         IF ::scrMovRgt()
            ::scn_:TEXT_BLOCK_[ 4 ]++
         ENDIF
      CASE nKey == key_[ 2 ]
         IF ::scrMovLft()
            ::scn_:TEXT_BLOCK_[ 4 ]--
         ENDIF
      CASE nKey == key_[ 3 ]
         IF ::scrMovUp()
            ::scn_:TEXT_BLOCK_[ 3 ]--
         ENDIF
      CASE nKey == key_[ 4 ]
         IF ::scrMovDn()
            ::scn_:TEXT_BLOCK_[ 3 ]++
         ENDIF
      CASE nKey == key_[ 5 ]
         EXIT
      ENDCASE

      IF ::scn_:TEXT_BLOCK_[ 3 ] < ::scn_:TEXT_BLOCK_[ 1 ]
         n := ::scn_:TEXT_BLOCK_[ 1 ]
         ::scn_:TEXT_BLOCK_[ 1 ] := ::scn_:TEXT_BLOCK_[ 3 ]
         ::scn_:TEXT_BLOCK_[ 3 ] := n
      ENDIF

      IF ::scn_:TEXT_BLOCK_[ 4 ] < ::scn_:TEXT_BLOCK_[ 2 ]
         n := ::scn_:TEXT_BLOCK_[ 2 ]
         ::scn_:TEXT_BLOCK_[ 2 ] := ::scn_:TEXT_BLOCK_[ 4 ]
         ::scn_:TEXT_BLOCK_[ 4 ] := n
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

   IF ! empty( ::scn_:TEXT_BLOCK_ )
      //  CREATE a ghost movement block
      ::scrMsg('Use Arrow Keys TO Move Selected Block')
      //  Check FOR current cursor position
      gst_:= { ::scn_:ROW_REP , ::scn_:COL_REP,;
               ::scn_:ROW_REP + ::scn_:TEXT_BLOCK_[ 3 ] - ::scn_:TEXT_BLOCK_[ 1 ],;
               ::scn_:COL_REP + ::scn_:TEXT_BLOCK_[ 4 ] - ::scn_:TEXT_BLOCK_[ 2 ] }
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
   LOCAL old_:= aclone( ::scn_:TEXT_BLOCK_ )

   FOR i := gst_[ 1 ] TO gst_[ 3 ]
      n := -1
      DO WHILE .t.
         n := ascan( ::obj_, {|e_| e_[ OBJ_ROW ] == i ;
                                       .AND. ;
                        ( VouchInRange(e_[OBJ_COL],gst_[2],gst_[4]);
                                       .OR. ;
                          VouchInRange(e_[OBJ_TO_COL],gst_[2],gst_[4])) ;
                                       .AND.;
                                   ! VouchInArray( n,del_ ) } )
         IF n > 0
            IF ::obj_[ n,OBJ_TYPE ] == OBJ_O_TEXT
               aadd( del_, n )

               s1    := '' ; s3 := ''
               s     := ::obj_[ n, OBJ_EQN ]
               nCol  := ::obj_[ n, OBJ_COL ]

               IF gst_[2] <= ::obj_[ n, OBJ_COL ] .AND. gst_[ 4 ] >= ::obj_[ n, OBJ_TO_COL ]
                  //  Only deletion of OBJECT
                  //  s2 := s
               ELSEIF gst_[2] >=  nCol
                  s1 := substr(s,1,gst_[2]-nCol)
                  //  s2 := substr(s,gst_[2]-nCol+1,gst_[4]-nCol+1)
                  s3 := substr(s,gst_[4]-nCol+2)
               ELSEIF gst_[2] <   nCol
                  s1 := substr(s,1,gst_[2]-nCol)
                  //  s2 := substr(s,gst_[2]-nCol+1,gst_[4]-nCol+1)
                  s3 := substr(s,gst_[4]-nCol+2)
               ENDIF

               IF len( s1 ) > 0
                  aadd( ins_, ::scrObjBlank() )
                  n1 := len( ins_ )

                  ins_[ n1, OBJ_TYPE    ] := OBJ_O_TEXT
                  ins_[ n1, OBJ_ROW     ] := ::obj_[ n, OBJ_ROW ]
                  ins_[ n1, OBJ_COL     ] := ::obj_[ n, OBJ_COL ]
                  ins_[ n1, OBJ_EQN     ] := s1
                  ins_[ n1, OBJ_ID      ] := 'Text'
                  ins_[ n1, OBJ_COLOR   ] := 'W/B'
                  ins_[ n1, OBJ_SECTION ] := ::obj_[ n, OBJ_SECTION ]
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
                  ins_[ n1, OBJ_ID      ] := 'Text'
                  ins_[ n1, OBJ_COLOR   ] := 'W/B'
                  ins_[ n1, OBJ_SECTION ] := ::obj_[ n, OBJ_SECTION ]
                  ins_[ n1, OBJ_TO_ROW  ] := ::obj_[ n, OBJ_ROW     ]
                  ins_[ n1, OBJ_TO_COL  ] := ins_[ n1, OBJ_COL ] + len( s3 ) - 1
               ENDIF

            ELSEIF ::obj_[n,OBJ_TYPE] == OBJ_O_FIELD .OR. ::obj_[n,OBJ_TYPE] == OBJ_O_EXP
               aadd( del_, n )

            ELSEIF ::obj_[n,OBJ_TYPE] == OBJ_O_BOX

            ELSEIF ::obj_[n,OBJ_TYPE] == OBJ_O_BMP

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
         n := ascan(::obj_,{|e_| e_[OBJ_ROW]==i;
                                      .AND. ;
                     ( VouchInRange(e_[OBJ_COL],old_[2],old_[4]);
                                       .OR. ;
                       VouchInRange(e_[OBJ_TO_COL],old_[2],old_[4])) ;
                                      .AND. ;
                                  ! VouchInArray( n, del_ ) } )
         IF n > 0
            IF     ::obj_[ n,OBJ_TYPE ] == OBJ_O_TEXT
               aadd( del_, n )

               //  TO be retained as it is
               s1    := '' ; s2 := '' ; s3 := ''
               s     := ::obj_[ n,OBJ_EQN]
               nCol  := ::obj_[ n,OBJ_COL]

               IF old_[2] <= ::obj_[n,OBJ_COL] .AND. old_[4] >= ::obj_[n,OBJ_TO_COL]
                  s2 := s   //  Insert WITH moved coordinates
               ELSEIF  old_[ 2 ] >= ::obj_[ n, OBJ_COL ]
                  s1 := substr( s, 1, old_[ 2 ] - nCol )
                  s2 := substr( s, old_[ 2 ] - nCol + 1, old_[ 4 ] - old_[ 2 ] + 1 )
                  s3 := substr( s, old_[ 4 ] - nCol + 2 )
               ELSEIF old_[ 2 ] < nCol
                  s1 := substr(s,1,old_[2]-nCol)
                  s2 := substr(s,old_[2]-nCol+1,old_[4]-old_[2]+1)
                  s3 := substr(s,old_[4]-nCol+2)
               ENDIF

               IF nMode == 0
                  IF len(s1)>0
                     aadd( ins_, ::scrObjBlank() ) ; n1 := len( ins_ )
                     
                     ins_[ n1, OBJ_TYPE    ] := OBJ_O_TEXT
                     ins_[ n1, OBJ_ROW     ] := ::obj_[ n, OBJ_ROW ]
                     ins_[ n1, OBJ_COL     ] := ::obj_[ n, OBJ_COL ]
                     ins_[ n1, OBJ_EQN     ] := s1
                     ins_[ n1, OBJ_ID      ] := 'Text'
                     ins_[ n1, OBJ_COLOR   ] := 'W/B'
                     ins_[ n1, OBJ_SECTION ] := ::obj_[ n, OBJ_SECTION ]
                     ins_[ n1, OBJ_TO_ROW  ] := ::obj_[ n, OBJ_ROW     ]
                     ins_[ n1, OBJ_TO_COL  ] := ins_[ n1,OBJ_COL     ] + len( s1 ) - 1
                  ENDIF
                  IF len(s3) > 0
                     aadd( ins_, ::scrObjBlank() ) ; n1 := len( ins_ )
                     
                     ins_[ n1, OBJ_TYPE    ] := OBJ_O_TEXT
                     ins_[ n1, OBJ_ROW     ] := ::obj_[ n, OBJ_ROW ]
                     ins_[ n1, OBJ_COL     ] := old_[ 4 ] + 1
                     ins_[ n1, OBJ_EQN     ] := s3
                     ins_[ n1, OBJ_ID      ] := 'Text'
                     ins_[ n1, OBJ_COLOR   ] := 'W/B'
                     ins_[ n1, OBJ_SECTION ] := ::obj_[ n, OBJ_SECTION]
                     ins_[ n1, OBJ_TO_ROW  ] := ::obj_[ n, OBJ_ROW    ]
                     ins_[ n1, OBJ_TO_COL  ] := ins_[ n1,OBJ_COL    ] + len( s3 ) - 1
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

            ELSEIF ::obj_[n,OBJ_TYPE] == OBJ_O_FIELD
               IF nMode == 0
                  aadd(del_,n)
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

   ::scn_:TEXT_BLOCK_ := {}

   RETURN Self

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrTextDel()
   LOCAL i,n,n1,s,s1,s3,nCol,ins_:={},del_:={},d_:={},old_:={}

   old_:= ::scn_:TEXT_BLOCK_
   FOR i := old_[1] TO old_[3]    //  Rows
      n := -1

      DO WHILE .t.
         n := ascan(::obj_,{|e_| e_[OBJ_ROW]==i;
                                      .AND. ;
                     (VouchInRange(e_[OBJ_COL],   old_[2],old_[4]);
                                       .OR. ;
                      VouchInRange(e_[OBJ_TO_COL],old_[2],old_[4])) ;
                                      .AND. ;
                                  !VouchInArray(n,del_) })
         IF n > 0
            IF     ::obj_[n,OBJ_TYPE] == OBJ_O_TEXT
               aadd(del_,n)

               //  TO be retained as it is
               s1    := '' ; s3 := ''
               s     := ::obj_[n,OBJ_EQN]
               nCol  := ::obj_[n,OBJ_COL]

               IF old_[2] <= ::obj_[n,OBJ_COL] .AND. old_[4] >= ::obj_[n,OBJ_TO_COL]
                  //  s2 := s   //  Insert WITH moved coordinates
               ELSEIF  old_[2] >= ::obj_[n,OBJ_COL]
                  s1 := substr(s,1,old_[2]-nCol)
                  //  s2 := substr(s,old_[2]-nCol+1,old_[4]-old_[2]+1)
                  s3 := substr(s,old_[4]-nCol+2)
               ELSEIF old_[2] < nCol
                  s1 := substr(s,1,old_[2]-nCol)
                  //  s2 := substr(s,old_[2]-nCol+1,old_[4]-old_[2]+1)
                  s3 := substr(s,old_[4]-nCol+2)
               ENDIF

               IF len( s1 ) > 0
                  aadd( ins_, ::scrObjBlank() )
                  n1 := len( ins_ )
                  ins_[ n1, OBJ_TYPE    ] := OBJ_O_TEXT
                  ins_[ n1, OBJ_ROW     ] := ::obj_[ n,OBJ_ROW ]
                  ins_[ n1, OBJ_COL     ] := ::obj_[ n,OBJ_COL ]
                  ins_[ n1, OBJ_EQN     ] := s1
                  ins_[ n1, OBJ_ID      ] := 'Text'
                  ins_[ n1, OBJ_COLOR   ] := 'W/B'
                  ins_[ n1, OBJ_SECTION ] := ::obj_[n,OBJ_SECTION]
                  ins_[ n1, OBJ_TO_ROW  ] := ::obj_[n,OBJ_ROW    ]
                  ins_[ n1, OBJ_TO_COL  ] := ins_[n1,OBJ_COL]+len(s1)-1
               ENDIF
               IF len( s3 ) > 0
                  aadd( ins_, ::scrObjBlank() )
                  n1 := len( ins_ )
                  ins_[n1,OBJ_TYPE    ] := OBJ_O_TEXT
                  ins_[n1,OBJ_ROW     ] := ::obj_[ n, OBJ_ROW ]
                  ins_[n1,OBJ_COL     ] := old_[ 4 ] + 1
                  ins_[n1,OBJ_EQN     ] := s3
                  ins_[n1,OBJ_ID      ] := 'Text'
                  ins_[n1,OBJ_COLOR   ] := 'W/B'
                  ins_[n1,OBJ_SECTION ] := ::obj_[ n, OBJ_SECTION ]
                  ins_[n1,OBJ_TO_ROW  ] := ::obj_[ n, OBJ_ROW     ]
                  ins_[n1,OBJ_TO_COL  ] := ins_[ n1, OBJ_COL ] + len( s3 ) - 1
               ENDIF

            ELSEIF ::obj_[n,OBJ_TYPE] == OBJ_O_FIELD .OR. ;
                   ::obj_[n,OBJ_TYPE] == OBJ_O_EXP
               aadd(del_,n)

            ENDIF
         ELSE
            EXIT
         ENDIF
      ENDDO
      //nn++
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
   ::scn_:TEXT_BLOCK_ := {}

   RETURN Self

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrAddBox( nObj )
   LOCAL nKey, o_, border, cClr, cFile, nnObj

   DEFAULT nObj TO 0
   
   cFile  := ""
   nnObj  := nObj
   
   IF empty( nObj )
      o_:= ::scrObjBlank()

      o_[ OBJ_TYPE       ] := OBJ_O_BOX
      o_[ OBJ_ROW        ] := ::scn_:ROW_REP 
      o_[ OBJ_COL        ] := ::scn_:COL_REP 
      o_[ OBJ_TO_ROW     ] := ::scn_:ROW_REP 
      o_[ OBJ_TO_COL     ] := ::scn_:COL_REP 
      o_[ OBJ_SECTION    ] := 1
      o_[ OBJ_F_LEN      ] := 9
      o_[ OBJ_MDL_F_TYPE ] := 62

      aadd( ::obj_, o_ )
      nObj := len( ::obj_ )
   ENDIF

   border := 'ÚÄ¿³ÙÄÀ³'            //  can be DOUBLE_SINGLE, etc
   cClr     := "w/b"
   ::obj_[ nObj, OBJ_BORDER ] := 0.5

   ::obj_[ nObj, OBJ_BOX_SHAPE ] := border
   ::obj_[ nObj, OBJ_COLOR     ] := cClr
   ::obj_[ nObj, OBJ_ID        ] := "Frame"
   ::obj_[ nObj, OBJ_EQN       ] := cFile
   ::obj_[ nObj, OBJ_PATTERN   ] := 'CLEAR     '

   IF ! empty( nnObj )
      ::scrOnLastCol( nObj )
      ::scrMove()
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
   ::scn_:REFRESH := OBJ_REFRESH_ALL

   RETURN NIL

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrAddFld( nObj )
   LOCAL h_:={}, w_, o_, v_, sel_

   DEFAULT nObj TO 0
   
   sel_:= ::scrVvSelAble()
   v_  := iif( nObj > 0, ::scrObj2Vv( ::obj_[ nObj ] ), ::scrVvBlank() )

   IF nObj == 0
      v_[ VV_FIELD   ] := 0
      v_[ VV_ID      ] := space( 40 )
      v_[ VV_F_PIC   ] := space( 15 )
      v_[ VV_COLOR   ] := 'N/W   '
      v_[ VV_EQN     ] := ""
   ENDIF

   sel_[ VV_F_TYPE   ] := .T.
   sel_[ VV_ALIGN    ] := .f.
   sel_[ VV_PRN_LEN  ] := .f.
   sel_[ VV_PRN_LEN  ] := .f.
   sel_[ VV_ALIGN    ] := .f.
   sel_[ VV_COLOR    ] := .f.
   sel_[ VV_POINT    ] := .f.
   sel_[ VV_COL_JUST ] := .f.
   sel_[ VV_PATTERN  ] := .f.

   aadd( h_, '  Title                    ' )
   aadd( h_, '  Field                    ' )
   aadd( h_, '  Type                     ' )
   aadd( h_, '  Width                    ' )
   aadd( h_, '  Decimals                 ' )
   aadd( h_, '  Calculate                ' )
   aadd( h_, '  Expression               ' )
   aadd( h_, '  Printed Width            ' )
   aadd( h_, '  Picture                  ' )
   aadd( h_, '  Pitch                    ' )
   aadd( h_, '  Font                     ' )
   aadd( h_, '  Bold                     ' )
   aadd( h_, '  Italics                  ' )
   aadd( h_, '  UnderLine                ' )
   aadd( h_, '  SuperScript              ' )
   aadd( h_, '  SubScript                ' )
   aadd( h_, '  Half Height              ' )
   aadd( h_, '  Alignment                ' )
   aadd( h_, '  Color                    ' )
   aadd( h_, '  Zero as Blank            ' )
   aadd( h_, '  Supress Repeated Values  ' )
   aadd( h_, '  Verticle Stretch         ' )
   aadd( h_, '  Wrap Semi Colons         ' )
   aadd( h_, '  The FOR Condition        ' )
   aadd( h_, '  Unique Id                ' )
   aadd( h_, '  Field Type . Module      ' )
   aadd( h_, '  Point Size               ' )
   aadd( h_, '  Column FOR Justification ' )
   aadd( h_, '  Pattern TO fill a frame  ' )
   aadd( h_, '  Border Thickness         ' )

   w_:= afill( array( len( h_ ) ), {|| .f. } )
   w_[ 1 ] := {| | .t. } 
   w_[ 2 ] := {| | .t. } 
   w_[ 3 ] := {| | VouchMenuM( 'MN_TYFLD' ) }
   w_[ 4 ] := {|v| v := oAchGet( 3 ), iif( v == 'D', !oCPut( 8 ), iif( v == 'L', !oCPut( 1 ), .t. ) ) }
   w_[ 5 ] := {|v| v := oAchGet( 3 ), iif( v <> 'N', !oCPut( 0 ), .t. ) }

   B_GETS HEADERS h_ VALUES v_ TITLE 'Configure Field' WHEN w_ SELECTABLES sel_ INTO v_

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
      
      o_:= ::scrVv2Obj( v_, o_ )

      o_[ OBJ_TYPE    ] := OBJ_O_FIELD
      o_[ OBJ_ROW     ] := iif( nObj == 0, ::scn_:ROW_REP, o_[ OBJ_ROW ] )
      o_[ OBJ_COL     ] := iif( nObj == 0, ::scn_:COL_REP, o_[ OBJ_COL ] )
      o_[ OBJ_TEXT    ] := padc( alltrim( v_[ VV_ID ] ), v_[ VV_F_LEN ] )
      o_[ OBJ_COLOR   ] := iif( empty( o_[ OBJ_COLOR ] ), 'W+/W', o_[ OBJ_COLOR ] )
      o_[ OBJ_TO_ROW  ] := iif( nObj == 0, ::scn_:ROW_REP, o_[ OBJ_TO_ROW ] )
      o_[ OBJ_TO_COL  ] := iif( nObj == 0, ::scn_:COL_REP, o_[ OBJ_COL ] ) + v_[ VV_F_LEN ] - 1
      o_[ OBJ_SECTION ] := 1

      IF nObj == 0
         aadd( ::obj_, o_ )
         nObj := len( ::obj_ )
      ELSE
         ::obj_[ nObj ] := o_
      ENDIF

      ::scn_:OBJ_SELECTED := 0
      ::scn_:REFRESH      := OBJ_REFRESH_LINE
      ::scn_:MODE         := 0
   ENDIF

   IF nObj > 0
      ::scrOrdObj()
   ENDIF

   RETURN Self

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrAddTxt( nMode )
   LOCAL txt_:={},n, lClub,i
   LOCAL n1,s1,s2,nTxt,nDel
   LOCAL nRepCol := ::scn_:COL_REP, nRepRow := ::scn_:ROW_REP
   LOCAL lOrder := .f.
   LOCAL nKey := ::scn_:LASTKEY
   
   //  nMode   1.Add   2.Del   3.BS

   //  Scan obj_ FOR Text Objects Related WITH Current Report Row
   aeval( ::obj_,{|e_| iif( e_[ OBJ_TYPE ] == OBJ_O_TEXT .AND. e_[ OBJ_ROW ] == nRepRow, aadd( txt_,e_ ),'' ) } )
   IF nMode == 1      //  New Character
      IF empty( txt_ ) .OR. ascan( txt_,{|e_| VouchInRange( nRepCol, e_[OBJ_COL],e_[OBJ_TO_COL] ) } ) == 0
         aadd( txt_, ::scrObjBlank() ) ; lOrder := .t.
         nTxt := len( txt_ )
         txt_[ nTxt, OBJ_TYPE    ]  := OBJ_O_TEXT
         txt_[ nTxt, OBJ_F_TYPE  ]  := 'C'
         txt_[ nTxt, OBJ_F_LEN   ]  := 1
         txt_[ nTxt, OBJ_ALIGN   ]  := 'L'
         txt_[ nTxt, OBJ_ROW     ]  := ::scn_:ROW_REP
         txt_[ nTxt, OBJ_COL     ]  := ::scn_:COL_REP
         txt_[ nTxt, OBJ_EQN     ]  := ''
         txt_[ nTxt, OBJ_ID      ]  := 'Text'
         txt_[ nTxt, OBJ_COLOR   ]  := 'N/W'
         txt_[ nTxt, OBJ_PITCH   ]  := 10
         txt_[ nTxt, OBJ_SECTION ]  := 1
         txt_[ nTxt, OBJ_TO_ROW  ]  := ::scn_:ROW_REP
         txt_[ nTxt, OBJ_TO_COL  ]  := ::scn_:COL_REP
      ENDIF
   ENDIF

   nTxt := ascan( txt_,{|e_| VouchInRange( nRepCol, e_[ OBJ_COL ], e_[ OBJ_TO_COL ] ) } )

   IF     nMode == 1
      txt_[nTxt,OBJ_EQN] := substr( txt_[nTxt,OBJ_EQN],1,;
                        ::scn_:COL_REP-txt_[nTxt,OBJ_COL] ) + ;
                                 chr( nKey ) + ;
           substr( txt_[nTxt,OBJ_EQN],::scn_:COL_REP-txt_[nTxt,OBJ_COL] + ;
                                            iif( ReadInsert(),1,2 ) )
      txt_[nTxt,OBJ_TO_COL] := txt_[nTxt,OBJ_COL]+len( txt_[nTxt,OBJ_EQN] )-1

   ELSEIF nMode == 2  .OR. nMode == 3 //  Delete
      IF readInsert()
         txt_[nTxt,OBJ_EQN] := substr( txt_[nTxt,OBJ_EQN],1,;
                        ::scn_:COL_REP-txt_[nTxt,OBJ_COL] ) + ;
           substr( txt_[nTxt,OBJ_EQN],::scn_:COL_REP-txt_[nTxt,OBJ_COL] + 2 )
         txt_[nTxt,OBJ_TO_COL] := txt_[nTxt,OBJ_COL]+len( txt_[nTxt,OBJ_EQN] )-1
      ELSE             //  Divide it IN two objects
         s1   := substr( txt_[nTxt,OBJ_EQN],1,::scn_:COL_REP-txt_[nTxt,OBJ_COL] )
         s2   := substr( txt_[nTxt,OBJ_EQN],::scn_:COL_REP-txt_[nTxt,OBJ_COL] + 2 )
         nDel := 0
         IF len( s1 ) > 0
            txt_[nTxt,OBJ_EQN]     := s1
            txt_[nTxt,OBJ_TO_COL]  := txt_[nTxt,OBJ_COL]+len( s1 )-1
            txt_[nTxt,OBJ_PRN_LEN] := len( s1 )
         ELSE
            nDel := nTxt
         ENDIF

         IF len( s2 ) > 0
            IF nDel == 0
               aadd( txt_,aclone( txt_[nTxt] ) ) 
               lOrder := .t.
               n1                := len( txt_ )
            ELSE
               n1 := nDel
            ENDIF
            txt_[n1,OBJ_TYPE]    := OBJ_O_TEXT
            txt_[n1,OBJ_F_TYPE]  := 'C'
            txt_[n1,OBJ_F_LEN]   := len( s2 )
            txt_[n1,OBJ_PRN_LEN] := len( s2 )
            //  txt_[n1,OBJ_ALIGN]   := 'L'
            txt_[n1,OBJ_ROW]     := ::scn_:ROW_REP
            txt_[n1,OBJ_COL]     := ::scn_:COL_REP+1
            txt_[n1,OBJ_EQN]     := s2
            txt_[n1,OBJ_ID]      := 'Text'
            txt_[n1,OBJ_SECTION] := 1
            txt_[n1,OBJ_TO_ROW]  := ::scn_:ROW_REP
            txt_[n1,OBJ_TO_COL]  := txt_[n1,OBJ_COL]+len( s2 )-1
         ENDIF
         IF len( s1 )==0.AND.len( s2 )==0
            VouchAShrink( txt_,nTxt )
            IF empty( txt_ )
               aadd( txt_,::scrObjBlank() )
            ENDIF
         ENDIF
      ENDIF
   ENDIF

   IF !empty( txt_ )
      DO WHILE .t.
         IF( n := ascan( txt_,{|e_| e_[OBJ_TO_COL] < e_[OBJ_COL] } ) ) > 0
            VouchAShrink( txt_,n )
         ELSE
            EXIT
         ENDIF
      ENDDO
      IF empty( txt_ )
         aadd( txt_,::scrObjBlank() )
      ENDIF
      //  CLUB DIFFERENT TEXT OBJECTS IF THESE ARE ADJACENT
      asort( txt_,,,{|e_,f_| e_[OBJ_COL]<f_[OBJ_COL] } )

      DO WHILE .t.
         lClub := .f.
//         n     := txt_[1,OBJ_TO_COL]
         FOR i := 2 TO len( txt_ )
            IF txt_[i,OBJ_COL] == txt_[i-1,OBJ_TO_COL]+1
               txt_[i-1,OBJ_EQN]    += txt_[i,OBJ_EQN]    //  Club both
               txt_[i-1,OBJ_TO_COL] := txt_[i-1,OBJ_COL] + len( txt_[i-1,OBJ_EQN] ) - 1
               txt_[i-1,OBJ_F_LEN]  := len( txt_[i-1,OBJ_EQN] )
               VouchAShrink( txt_,i )
//               lUpdate := .t.
               lClub   := .t.
            ENDIF
         NEXT
         IF !lClub
            EXIT
         ENDIF
      ENDDO
   ENDIF

   DO WHILE .t.
      IF( n := ascan( ::obj_,{|e_| e_[OBJ_TYPE] == OBJ_O_TEXT .AND. ;
                                e_[OBJ_ROW ] == ::scn_:ROW_REP } ) ) > 0
         VouchAShrink( ::obj_,n )
         IF empty( ::obj_ )
            aadd( ::obj_, ::scrObjBlank() )
         ENDIF
      ELSE
         EXIT
      ENDIF
   ENDDO

   aeval( txt_,{|e_| iif( e_[OBJ_ROW]>0,aadd( ::obj_,e_ ),'' ) } )   //  Now attach txt_

   DO WHILE .t.
      IF( n := ascan( ::obj_,{|e_| e_[OBJ_TO_COL] < e_[OBJ_COL] } ) ) > 0
         VouchAShrink( ::obj_,n )
         IF empty( ::obj_ )
            aadd( ::obj_,::scrObjBlank() )
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

   ::scn_:REFRESH := OBJ_REFRESH_LINE

   RETURN NIL

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrProperty()

   ::scn_:PROPERTY := {}

   RETURN NIL

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrMsg( msg )
   LOCAL row := row(), col := col()

   @ maxrow(),0 SAY padc( " ", maxcol()+1 ) COLOR "W+/W"
   IF empty( msg )
      msg := "F1 Help  F5 Edit  F6 Select  F7 Copy  F8 Paste  F9 Box  F10 Field"
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

METHOD hbCUIEditor:scrConfig()
   LOCAL s, n

   ::scn_:SECTORS_       := {}
   ::scn_:DESIGN         := 1
   ::scn_:FOR_ROWS       := { { 0,'' } }

   ::scn_:TOP            := 1
   ::scn_:LEFT           := 0
   ::scn_:BOTTOM         := maxrow()-2
   ::scn_:RIGHT          := maxcol()
   
   ::scn_:MODE           := 0
   ::scn_:ROW_CUR        := ::scn_:TOP  
   ::scn_:COL_CUR        := ::scn_:LEFT 
   ::scn_:ROW_REP        := 1
   ::scn_:COL_REP        := 1
   ::scn_:ROW_DIS        := ::scn_:TOP  - 1
   ::scn_:COL_DIS        := ::scn_:LEFT - 1
                       
   ::scn_:ROW_MENU       := 0
   ::scn_:ROW_RULER      := 0
   ::scn_:STATUS_ROW     := maxrow() - 1
   ::scn_:STATUS_COL     := 0
   ::scn_:STATUS_COL_TO  := maxcol()
                       
   ::scn_:COL_MAX        := 400
   ::scn_:ROW_PREV       := ::scn_:TOP  
   ::scn_:COL_PREV       := ::scn_:LEFT 
   ::scn_:REP_LINES      := 200
                       
   ::scn_:CLR_STATUS     := "W+/BG"
   ::scn_:CLR_TEXT       := 'W+/B'
   ::scn_:CLR_BOX        := 'W/B'
   ::scn_:CLR_FIELD      := 'W+/W'
   ::scn_:CLR_HILITE     := 'GR+/BG'
   ::scn_:CLR_WINDOW     := 'W+/BG'
   ::scn_:CLR_RULER      := "N/W"
   ::scn_:CLR_MENU       := "W+/B"
   ::scn_:CLR_OVERALL    := "N/W"
   ::scn_:CLR_PREV       := 'B/W'
   ::scn_:CLR_SELECT     := 'GR+/N'
                       
   ::scn_:OBJ_HILITE     := 0 
   ::scn_:OBJ_SELECTED   := 0 
                       
   s := '.'
   FOR n := 1 TO 40
      s += '.......' + strtran( str( n,3 ), ' ', '.' )
   NEXT
   ::scn_:RULER          := s
                       
   ::scn_:DRAW_FILL      := '±±±±±±±±±'
   ::scn_:OBJ_ID_        := { 'Bitmap','Line','Text','Field','Expression','BitMap' }
   ::scn_:REFRESH        := OBJ_REFRESH_ALL
   ::scn_:OBJ_COPIED     := 0
   ::scn_:BOX_SHAPE      := 'ÚÄ¿³ÙÄÀ³'
   ::scn_:CHR_PREV       := ''
   ::scn_:DESIGN_ID      := "Module"
   ::scn_:FILE           := "Untitled"
   ::scn_:PROPERTY       := {}
   ::scn_:GRAPHICS       := .f.
   ::scn_:TEXT_BLOCK_    := {}
   ::scn_:FIELDS         := {}
   ::scn_:LASTKEY        := 0

   RETURN Self

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrReConfig()

   ::scn_:MODE           := 0
   ::scn_:TOP            := 1
   ::scn_:LEFT           := 0
   ::scn_:BOTTOM         := maxrow()-2
   ::scn_:RIGHT          := maxcol()
   
   ::scn_:MODE           := 0
   ::scn_:ROW_CUR        := ::scn_:TOP  
   ::scn_:COL_CUR        := ::scn_:LEFT 
   ::scn_:ROW_REP        := 1
   ::scn_:COL_REP        := 1
   ::scn_:ROW_DIS        := ::scn_:TOP  - 1
   ::scn_:COL_DIS        := ::scn_:LEFT - 1
                       
   ::scn_:ROW_MENU       := 0
   ::scn_:ROW_RULER      := 0
   ::scn_:STATUS_ROW     := maxrow() - 1
   ::scn_:STATUS_COL     := 0
   ::scn_:STATUS_COL_TO  := maxcol()
                       
   ::scn_:COL_MAX        := 400
   ::scn_:ROW_PREV       := ::scn_:TOP  
   ::scn_:COL_PREV       := ::scn_:LEFT 
   ::scn_:REP_LINES      := 200

   RETURN NIL

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrSectors()

   aadd( ::scn_:SECTORS_, { 1, 'Screen', 'R    ', 100, 'W+/BG', '', .f., .f. } )

   RETURN 100

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrAddPrp( sct_ )

   aadd( ::scn_:SECTORS_, { sct_[1], sct_[2], sct_[3], sct_[4], sct_[5], sct_[6], sct_[7], sct_[8] } )

   RETURN NIL

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrObjBlank()
   LOCAL o_:= array( OBJ_INIT_VRBLS )

   o_[ OBJ_TYPE       ] := 0
   o_[ OBJ_ROW        ] := 0
   o_[ OBJ_COL        ] := 0
   o_[ OBJ_TEXT       ] := ''
   o_[ OBJ_COLOR      ] := 'W/B    '
   o_[ OBJ_TO_ROW     ] := 0
   o_[ OBJ_TO_COL     ] := 0
   o_[ OBJ_ID         ] := ''
   o_[ OBJ_SECTION    ] := 1
   o_[ OBJ_ALIAS      ] := ''
   o_[ OBJ_FIELD      ] := 0
   o_[ OBJ_EQN        ] := ''
   o_[ OBJ_F_TYPE     ] := ''
   o_[ OBJ_F_LEN      ] := 0
   o_[ OBJ_F_DEC      ] := 0
   o_[ OBJ_F_PIC      ] := ''
   o_[ OBJ_ALIGN      ] := 'L'
   o_[ OBJ_PITCH      ] := 10
   o_[ OBJ_FONT       ] := 'COURIER '
   o_[ OBJ_BOLD       ] := .F.
   o_[ OBJ_ITALIC     ] := .F.
   o_[ OBJ_UNDERLN    ] := .F.
   o_[ OBJ_S_SCRPT    ] := .F.
   o_[ OBJ_U_SCRPT    ] := .F.
   o_[ OBJ_PRN_LEN    ] := 0
   o_[ OBJ_HALF_H     ] := .F.
   o_[ OBJ_ZERO       ] := .T.
   o_[ OBJ_REPEATED   ] := 'NO    '
   o_[ OBJ_VERTICLE   ] := .F.
   o_[ OBJ_WRAP_SEMI  ] := .F.
   o_[ OBJ_FOR        ] := space( 80 )
   o_[ OBJ_SEC_ROW    ] := 0
   o_[ OBJ_ATTRB      ] := 'NONE    '
   o_[ OBJ_VAL        ] := ' '
   o_[ OBJ_OBJ_UNIQUE ] := 0
   o_[ OBJ_MDL_F_TYPE ] := 0
   o_[ OBJ_POINT      ] := 0
   o_[ OBJ_COL_JUST   ] := 0
   o_[ OBJ_PATTERN    ] := 'SOLID     '
   o_[ OBJ_BORDER     ] := 0.50

   RETURN o_

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrVvBlank()
   LOCAL v_:= array( VV_INIT_VRBLS )

   v_[ VV_ID         ]  := 'New            '
   v_[ VV_FIELD      ]  := 0
   v_[ VV_F_TYPE     ]  := 'C'
   v_[ VV_F_LEN      ]  := 25
   v_[ VV_F_DEC      ]  := 0
   v_[ VV_ATTRB      ]  := 'NONE    '
   v_[ VV_EQN        ]  := '               '
   v_[ VV_PRN_LEN    ]  := 25
   v_[ VV_F_PIC      ]  := '               '
   v_[ VV_PITCH      ]  := 10
   v_[ VV_FONT       ]  := 'COURIER '
   v_[ VV_BOLD       ]  := .f.
   v_[ VV_ITALIC     ]  := .f.
   v_[ VV_UNDERLN    ]  := .f.
   v_[ VV_S_SCRPT    ]  := .f.
   v_[ VV_U_SCRPT    ]  := .f.
   v_[ VV_HALF_H     ]  := .f.
   v_[ VV_ALIGN      ]  := 'C'
   v_[ VV_COLOR      ]  := 'W+/B   '
   v_[ VV_ZERO       ]  := .t.
   v_[ VV_REPEATED   ]  := 'NO    '
   v_[ VV_VERTICLE   ]  := .F.
   v_[ VV_WRAP_SEMI  ]  := .F.
   v_[ VV_FOR        ]  := space( 80 )
   v_[ VV_OBJ_UNIQUE ]  := 0
   v_[ VV_MDL_F_TYPE ]  := 0
   v_[ VV_POINT      ]  := 12
   v_[ VV_COL_JUST   ]  := 0
   v_[ VV_PATTERN    ]  := 'SOLID     '
   v_[ VV_BORDER     ]  := 0.50

   RETURN v_

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrVvSelAble()
   LOCAL sel_:= array( VV_INIT_VRBLS )

   sel_[ VV_ID         ]  := .t.
   sel_[ VV_FIELD      ]  := .f.
   sel_[ VV_F_TYPE     ]  := .t.
   sel_[ VV_F_LEN      ]  := .t.
   sel_[ VV_F_DEC      ]  := .t.
   sel_[ VV_ATTRB      ]  := .f.
   sel_[ VV_EQN        ]  := .f.
   sel_[ VV_PRN_LEN    ]  := .f.
   sel_[ VV_F_PIC      ]  := .t.
   sel_[ VV_PITCH      ]  := .f.
   sel_[ VV_FONT       ]  := .f.
   sel_[ VV_BOLD       ]  := .f.
   sel_[ VV_ITALIC     ]  := .f.
   sel_[ VV_UNDERLN    ]  := .f.
   sel_[ VV_S_SCRPT    ]  := .f.    
   sel_[ VV_U_SCRPT    ]  := .f.  
   sel_[ VV_HALF_H     ]  := .f.
   sel_[ VV_ALIGN      ]  := .f.    
   sel_[ VV_COLOR      ]  := .t.    
   sel_[ VV_ZERO       ]  := .f.
   sel_[ VV_REPEATED   ]  := .f.
   sel_[ VV_VERTICLE   ]  := .f.
   sel_[ VV_WRAP_SEMI  ]  := .f.
   sel_[ VV_FOR        ]  := .f.
   sel_[ VV_OBJ_UNIQUE ]  := .f.
   sel_[ VV_MDL_F_TYPE ]  := .f.
   sel_[ VV_POINT      ]  := .f.
   sel_[ VV_COL_JUST   ]  := .f.
   sel_[ VV_PATTERN    ]  := .f.
   sel_[ VV_BORDER     ]  := .f.

   RETURN sel_

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrObj2Vv( o_ )
   LOCAL v_:={}

   aadd( v_, pad( o_[OBJ_ID ],15 ) )
   aadd( v_, o_[ OBJ_FIELD      ] )
   aadd( v_, o_[ OBJ_F_TYPE     ] )
   aadd( v_, o_[ OBJ_F_LEN      ] )
   aadd( v_, o_[ OBJ_F_DEC      ] )
   aadd( v_, o_[ OBJ_ATTRB      ] )
   aadd( v_, o_[ OBJ_EQN        ] )
   aadd( v_, o_[ OBJ_PRN_LEN    ] )
   aadd( v_, o_[ OBJ_F_PIC      ] )
   aadd( v_, o_[ OBJ_PITCH      ] )
   aadd( v_, o_[ OBJ_FONT       ] )
   aadd( v_, o_[ OBJ_BOLD       ] )
   aadd( v_, o_[ OBJ_ITALIC     ] )
   aadd( v_, o_[ OBJ_UNDERLN    ] )
   aadd( v_, o_[ OBJ_S_SCRPT    ] )
   aadd( v_, o_[ OBJ_U_SCRPT    ] )
   aadd( v_, o_[ OBJ_HALF_H     ] )
   aadd( v_, o_[ OBJ_ALIGN      ] )
   aadd( v_, o_[ OBJ_COLOR      ] )
   aadd( v_, o_[ OBJ_ZERO       ] )
   aadd( v_, o_[ OBJ_REPEATED   ] )
   aadd( v_, o_[ OBJ_VERTICLE   ] )
   aadd( v_, o_[ OBJ_WRAP_SEMI  ] )
   aadd( v_, o_[ OBJ_FOR        ] )
   aadd( v_, o_[ OBJ_OBJ_UNIQUE ] )
   aadd( v_, o_[ OBJ_MDL_F_TYPE ] )
   aadd( v_, o_[ OBJ_POINT      ] )
   aadd( v_, o_[ OBJ_COL_JUST   ] )
   aadd( v_, o_[ OBJ_PATTERN    ] )
   aadd( v_, o_[ OBJ_BORDER     ] )

   RETURN v_

//----------------------------------------------------------------------//

METHOD hbCUIEditor:scrVv2Obj( v_, o_ )

   o_[ OBJ_ID         ] := v_[ VV_ID         ]
   o_[ OBJ_FIELD      ] := v_[ VV_FIELD      ]
   o_[ OBJ_F_TYPE     ] := v_[ VV_F_TYPE     ]
   o_[ OBJ_F_LEN      ] := v_[ VV_F_LEN      ]
   o_[ OBJ_F_DEC      ] := v_[ VV_F_DEC      ]
   o_[ OBJ_ATTRB      ] := v_[ VV_ATTRB      ]
   o_[ OBJ_EQN        ] := v_[ VV_EQN        ]
   o_[ OBJ_PRN_LEN    ] := v_[ VV_PRN_LEN    ]
   o_[ OBJ_F_PIC      ] := v_[ VV_F_PIC      ]
   o_[ OBJ_PITCH      ] := v_[ VV_PITCH      ]
   o_[ OBJ_FONT       ] := v_[ VV_FONT       ]
   o_[ OBJ_BOLD       ] := v_[ VV_BOLD       ]
   o_[ OBJ_ITALIC     ] := v_[ VV_ITALIC     ]
   o_[ OBJ_UNDERLN    ] := v_[ VV_UNDERLN    ]
   o_[ OBJ_S_SCRPT    ] := v_[ VV_S_SCRPT    ]
   o_[ OBJ_U_SCRPT    ] := v_[ VV_U_SCRPT    ]
   o_[ OBJ_HALF_H     ] := v_[ VV_HALF_H     ]
   o_[ OBJ_ALIGN      ] := v_[ VV_ALIGN      ]
   o_[ OBJ_COLOR      ] := v_[ VV_COLOR      ]
   o_[ OBJ_ZERO       ] := v_[ VV_ZERO       ]
   o_[ OBJ_REPEATED   ] := v_[ VV_REPEATED   ]
   o_[ OBJ_VERTICLE   ] := v_[ VV_VERTICLE   ]
   o_[ OBJ_WRAP_SEMI  ] := v_[ VV_WRAP_SEMI  ]
   o_[ OBJ_FOR        ] := v_[ VV_FOR        ]
   o_[ OBJ_OBJ_UNIQUE ] := v_[ VV_OBJ_UNIQUE ]
   o_[ OBJ_MDL_F_TYPE ] := v_[ VV_MDL_F_TYPE ]
   o_[ OBJ_POINT      ] := v_[ VV_POINT      ]
   o_[ OBJ_COL_JUST   ] := v_[ VV_COL_JUST   ]
   o_[ OBJ_PATTERN    ] := v_[ VV_PATTERN    ]
   o_[ OBJ_BORDER     ] := v_[ VV_BORDER     ]

   RETURN o_

//----------------------------------------------------------------------//
