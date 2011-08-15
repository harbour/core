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

   DATA SECTORS_                                  INIT {}
   DATA DESIGN                                    INIT 1
   DATA FOR_ROWS                                  INIT { { 0,"" } }
   DATA TOP                                       INIT 1
   DATA LEFT                                      INIT 0
   DATA BOTTOM                                    INIT maxrow() - 2
   DATA RIGHT                                     INIT maxcol()
   DATA MODE                                      INIT 0
   DATA ROW_CUR                                   INIT ::TOP
   DATA COL_CUR                                   INIT ::LEFT
   DATA ROW_REP                                   INIT 1
   DATA COL_REP                                   INIT 1
   DATA ROW_DIS                                   INIT ::TOP - 1
   DATA COL_DIS                                   INIT ::LEFT - 1
   DATA ROW_MENU                                  INIT 0
   DATA ROW_RULER                                 INIT 0
   DATA STATUS_ROW                                INIT maxrow() - 1
   DATA STATUS_COL                                INIT 0
   DATA STATUS_COL_TO                             INIT maxcol()
   DATA COL_MAX                                   INIT 400
   DATA ROW_PREV                                  INIT ::TOP
   DATA COL_PREV                                  INIT ::LEFT
   DATA REP_LINES                                 INIT 200
   DATA CLR_STATUS                                INIT "W+/BG"
   DATA CLR_TEXT                                  INIT "W+/B"
   DATA CLR_BOX                                   INIT "W/B"
   DATA CLR_FIELD                                 INIT CLR_GET
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

   DATA obj_
   DATA scn_

   DATA nCurObj


   ENDCLASS

/*----------------------------------------------------------------------*/

FUNCTION Operate( obj_,scn_ )
   LOCAL nObj, bError
   LOCAL grf_:= { 43,45,46,48,49,50,51,52,53,54,55,56,57 }

   readinsert( .t. )

   scrDisplay( scn_ )
   scrMove( obj_,scn_ )
   scrStatus( obj_,scn_ )
   keyboard( chr( K_UP ) )

   bError := errorblock( {|e| break( e ) } )
   BEGIN SEQUENCE

   DO WHILE .t.
      scn_[ SCN_ROW_PREV ] := scn_[ SCN_ROW_CUR ]
      scn_[ SCN_COL_PREV ] := scn_[ SCN_COL_CUR ]
      scn_[ SCN_REFRESH  ] := OBJ_REFRESH_NIL

      setCursor( .t. )
      setCursor( IF( readInsert(),2,1 ) )

      DO WHILE .t.
         scn_[ SCN_LASTKEY ] := inkey( 0, INKEY_ALL + HB_INKEY_GTEVENT )
         IF scn_[ SCN_LASTKEY ] <> K_MOUSEMOVE
            EXIT
         ENDIF
      ENDDO

      DO CASE
      CASE scn_[ SCN_GRAPHICS ] .AND. ascan( grf_,scn_[ SCN_LASTKEY ] ) > 0
         //processkey()

      CASE scrMouse( obj_, scn_, scn_[ SCN_LASTKEY ] )
#IF 0
      CASE scn_[ SCN_LASTKEY ] == K_ALT_F6
         graphChar()
         scn_[ SCN_GRAPHICS ] := ! scn_[ SCN_GRAPHICS ]
         scn_[ SCN_REFRESH  ] := OBJ_REFRESH_ALL
#ENDIF
      /*  Save Report */
      CASE scn_[ SCN_LASTKEY ] == K_ESC
         //EXIT
         __KeyBoard( chr( K_CTRL_ENTER ) )
      CASE scn_[ SCN_LASTKEY ] == K_CTRL_ENTER
         EXIT

      CASE scn_[ SCN_LASTKEY ] == K_RIGHT
         scrMovRgt( scn_ )
      CASE scn_[ SCN_LASTKEY ] == K_LEFT
         scrMovLft( scn_ )
      CASE scn_[ SCN_LASTKEY ] == K_UP
         scrMovUp( scn_ )
      CASE scn_[ SCN_LASTKEY ] == K_DOWN
         scrMovDn( scn_ )
      CASE scn_[ SCN_LASTKEY ] == K_MWBACKWARD
         scrMovDn( scn_ )
      CASE scn_[ SCN_LASTKEY ] == K_MWFORWARD
         scrMovUp( scn_ )
      CASE scn_[ SCN_LASTKEY ] == K_HOME
         scn_[ SCN_COL_REP ] := 1
         scn_[ SCN_COL_CUR ] := scn_[ SCN_LEFT ]
         scn_[ SCN_COL_DIS ] := scn_[ SCN_LEFT ]-1
         scn_[ SCN_REFRESH ] := OBJ_REFRESH_ALL
      CASE scn_[ SCN_LASTKEY ] == K_END
         scn_[SCN_COL_REP] := scn_[SCN_COL_MAX]
         scn_[SCN_COL_CUR] := scn_[SCN_RIGHT]
         scn_[SCN_COL_DIS] := (scn_[SCN_LEFT]-1) - (scn_[SCN_COL_REP]-(scn_[SCN_RIGHT]-scn_[SCN_LEFT]+1))
         scn_[SCN_REFRESH] := OBJ_REFRESH_ALL
      CASE scn_[ SCN_LASTKEY ] == K_PGUP
         //  scrMovPgUp(scn_)
         scn_[SCN_ROW_REP] := 1
         scn_[SCN_ROW_CUR] := scn_[SCN_TOP]
         scn_[SCN_ROW_DIS] := scn_[SCN_TOP] - 1
         scn_[SCN_REFRESH] := OBJ_REFRESH_ALL
      CASE scn_[ SCN_LASTKEY ] == K_PGDN
         //  scn_[SCN_ROW_REP] := scn_[SCN_REP_LINES]
         //  scn_[SCN_ROW_CUR] := scn_[SCN_BOTTOM]

      CASE scn_[ SCN_LASTKEY ] == K_INS
         readInsert( !readInsert() )
         setcursor( iif( readInsert(), 2, 1 ) )

      CASE scn_[ SCN_LASTKEY ] == K_ENTER
         IF scn_[ SCN_MODE ] == OBJ_MODE_SELECT .AND. scn_[ SCN_OBJ_SELECTED ] > 0
            obj_[ scn_[ SCN_OBJ_SELECTED ], OBJ_SECTION ] := scrSecCur( scn_, scn_[ SCN_ROW_REP ] )
            scn_[ SCN_COL_MAX ]   := max( scn_[ SCN_COL_MAX ], obj_[ scn_[ SCN_OBJ_SELECTED ], OBJ_TO_COL ] + 1 )
            scn_[ SCN_MODE         ] := OBJ_MODE_IDLE
            scn_[ SCN_REFRESH      ] := OBJ_REFRESH_LINE
            scn_[ SCN_OBJ_SELECTED ] := 0
            scrMsg()
         ENDIF

      CASE VouchInRange( scn_[ SCN_LASTKEY ], K_SPACE, 254 ) .AND. scn_[ SCN_MODE ] <> OBJ_MODE_SELECT
         scrAddTxt( obj_, scn_, scn_[ SCN_LASTKEY ], 1 )

      CASE scn_[ SCN_LASTKEY ] == K_F1                           //  Help
         help( 'NWREPORT' )

      CASE scn_[ SCN_LASTKEY ] == K_F2                           //  Calculator
         //calculate()

      CASE scn_[ SCN_LASTKEY ] == K_F3                           //  OBJECT
         scrObject(obj_,scn_)

      CASE scn_[ SCN_LASTKEY ] == K_F4                           //  Properties
         scrProperty( obj_,scn_ )

      CASE scn_[ SCN_LASTKEY ] == K_F7 .OR. scn_[ SCN_LASTKEY ] == K_ALT_C      //  Copy
         scrObjCopy(obj_,scn_)

      CASE scn_[ SCN_LASTKEY ] == K_F8 .OR. scn_[ SCN_LASTKEY ] == K_ALT_V      //  Paste
         scrObjPas(obj_,scn_)

      CASE scn_[ SCN_LASTKEY ] == K_F9                           //  Box
         scrAddBox( obj_,scn_,0 )

      CASE scn_[ SCN_LASTKEY ] == K_F10                          //  Fields
         scrAddFld( obj_,scn_,0 )

      CASE scn_[ SCN_LASTKEY ] == K_DEL
         IF ! empty( scn_[ SCN_TEXT_BLOCK_] )
            obj_:= scrTextDel( obj_,scn_ )
            scrOrdObj( obj_, scn_ )
            scn_[ SCN_MODE         ] := 0
            scn_[ SCN_OBJ_SELECTED ] := 0
            scn_[ SCN_OBJ_HILITE   ] := 0
            scn_[ SCN_REFRESH      ] := OBJ_REFRESH_ALL
         ELSEIF scrIsObjTxt( obj_,scn_ )
            scrAddTxt( obj_, scn_, scn_[ SCN_LASTKEY ], 2 )
         ELSEIF scn_[ SCN_MODE ] == OBJ_MODE_SELECT
            obj_:= scrObjDel( obj_, scn_, scn_[ SCN_OBJ_SELECTED ] )
            scn_[ SCN_MODE         ] := 0
            scn_[ SCN_OBJ_SELECTED ] := 0
         ELSEIF scn_[ SCN_OBJ_HILITE ] > 0
            obj_:= scrObjDel( obj_, scn_, scn_[ SCN_OBJ_HILITE ] )
            scn_[ SCN_MODE         ] := 0
            scn_[ SCN_OBJ_SELECTED ] := 0
            scn_[ SCN_OBJ_HILITE   ] := 0
            scn_[ SCN_REFRESH      ] := OBJ_REFRESH_ALL
         ENDIF

      CASE scn_[ SCN_LASTKEY ] == K_BS
         IF scn_[ SCN_MODE ] <> OBJ_MODE_SELECT
            IF scrMovLft( scn_ )
               IF scrIsObjTxt( obj_,scn_ )
                  scrAddTxt( obj_,scn_,scn_[ SCN_LASTKEY ], 3 )
               ENDIF
            ENDIF
         ENDIF

      CASE scn_[ SCN_LASTKEY ] == K_ALT_B
         scrAddBox( obj_, scn_, 0 )

      CASE scn_[ SCN_LASTKEY ] == K_ALT_F
         scrAddFld( obj_, scn_, 0 )
      CASE scn_[ SCN_LASTKEY ] == K_ALT_E
         //scrAddExp( obj_, scn_, 0 )
      CASE scn_[ SCN_LASTKEY ] == K_ALT_N
         scrAddLine( obj_, scn_ )
      CASE scn_[ SCN_LASTKEY ] == K_ALT_O
         scrDelLine( obj_, scn_ )
      CASE scn_[ SCN_LASTKEY ] == K_ALT_W
         scrRepCol( obj_, scn_ )
      CASE scn_[ SCN_LASTKEY ] == K_CTRL_F6    //  Selection of Block
         scrTextBlock( obj_, scn_ )
      CASE scn_[ SCN_LASTKEY ] == K_CTRL_F7    //  Move, Copy
         obj_:= scrTextMove( obj_, scn_, 1 )
      CASE scn_[ SCN_LASTKEY ] == K_CTRL_F8    //  Move, Cut AND Paste
         obj_:= scrTextMove( obj_, scn_, 0 )
      CASE scn_[ SCN_LASTKEY ] == K_CTRL_Z
         scrExport( obj_, scn_ )
      CASE scn_[ SCN_LASTKEY ] == K_CTRL_W
         scrImport( @obj_, @scn_ )
      CASE scn_[ SCN_LASTKEY ] == HB_K_RESIZE
         ScrWvtConfig( obj_, scn_ )
         scrDisplay( scn_ )
         scrMove( obj_, scn_ )
         scrStatus( obj_, scn_ )

      ENDCASE

      IF scn_[ SCN_MODE    ] == OBJ_MODE_SELECT
         scn_[ SCN_REFRESH ] := iif( scn_[ SCN_REFRESH ] == OBJ_REFRESH_NIL,;
                                       OBJ_REFRESH_LINE, scn_[ SCN_REFRESH ] )
         scrUpdObjRC( obj_, scn_ )
      ENDIF

      //  Check on which OBJECT cursor is placed
      //
      nObj := scrChkObj( obj_, scn_ )

      IF nObj > 0 .AND. scn_[ SCN_MODE ] <> OBJ_MODE_SELECT
         scn_[ SCN_REFRESH    ] := iif( scn_[ SCN_REFRESH ] == OBJ_REFRESH_NIL, OBJ_REFRESH_LINE, scn_[ SCN_REFRESH ] )
         scn_[ SCN_OBJ_HILITE ] := nObj
         scrOnFirstCol( obj_, scn_, nObj, { OBJ_O_FIELD, OBJ_O_EXP } )

      ELSEIF ! empty( scn_[ SCN_OBJ_HILITE ] )
         scn_[ SCN_REFRESH    ] := iif( scn_[ SCN_REFRESH ] == OBJ_REFRESH_NIL, OBJ_REFRESH_LINE, scn_[ SCN_REFRESH ] )
         scn_[ SCN_OBJ_HILITE ] := 0

      ENDIF

      IF nObj > 0 .AND. scn_[ SCN_LASTKEY ] == K_F5   //  Edit
         IF obj_[ nObj, OBJ_TYPE ] == OBJ_O_FIELD
            scrAddFld( obj_, scn_, nObj )
         ELSEIF obj_[ nObj, OBJ_TYPE ] == OBJ_O_TEXT
            scrTxtProp( obj_, scn_, nObj )
         ELSEIF obj_[ nObj, OBJ_TYPE ] == OBJ_O_BOX
            scrAddBox( obj_, scn_, nObj )
         ENDIF
      ENDIF

      //  Is the OBJECT selected
      IF nObj > 0 .AND. scn_[ SCN_LASTKEY ] == K_F6 .AND. obj_[ nObj,OBJ_TYPE] == OBJ_O_BOX
         scn_[SCN_MODE]         := OBJ_MODE_SELECT
         scn_[SCN_OBJ_SELECTED] := nObj
         scrOnFirstCol( obj_, scn_, nObj, { OBJ_O_BOX } )
         scrMsg( "Box is Selected. Use Arrow Keys TO Move, Enter TO Finished !" )

      ELSEIF nObj > 0 .AND. scn_[ SCN_LASTKEY ] == K_F6 .AND. ! ( obj_[ nObj,OBJ_TYPE ] == OBJ_O_BOX )
         scn_[ SCN_MODE         ] := OBJ_MODE_SELECT
         scn_[ SCN_OBJ_SELECTED ] := nObj
         scrOnFirstCol( obj_, scn_, nObj, { OBJ_O_TEXT } )
         scrMsg( "OBJECT is Selected. Use Arrow Keys TO Move, Enter TO Finished" )

      ENDIF

      IF     scn_[ SCN_REFRESH ] == OBJ_REFRESH_ALL
         scrMove( obj_, scn_ )
      ELSEIF scn_[ SCN_REFRESH ] == OBJ_REFRESH_LINE
         IF scrIsBoxIn( obj_, scn_ )
            scrMove( obj_, scn_ )
         ELSE
            scrMoveLine( obj_, scn_ )
         ENDIF
      ENDIF

      scrStatus( obj_, scn_ )                        //  Status Line

      IF scn_[ SCN_GRAPHICS ]                       //  Graphics Window
         //grfRest()
      ENDIF
   ENDDO

   RECOVER
      alert('ERROR has occured WHILE designing this document, save it anyway!')
   END SEQUENCE
   errorBlock( bError )

   IF scn_[ SCN_GRAPHICS ]
      //graphChar()
      scn_[ SCN_GRAPHICS ] := .f.
   ENDIF

   scrOrdObj( obj_ )

   RETURN { obj_,scn_ }

//----------------------------------------------------------------------//

FUNCTION scrOrdObj(obj_)

   //  Objects are ordered as per their type
   asort( obj_,,, {|e_,f_| e_[ OBJ_TYPE ] < f_[ OBJ_TYPE ] } )

   RETURN obj_

//----------------------------------------------------------------------//

FUNCTION scrMovRgt( scn_ )
   LOCAL lMoved := .t.

   scn_[SCN_COL_CUR]++
   IF scn_[SCN_COL_CUR] > scn_[SCN_RIGHT]
      IF scn_[SCN_COL_MAX] > scn_[SCN_COL_REP]
         scn_[SCN_COL_DIS]--
         scn_[SCN_COL_CUR]--
         scn_[SCN_COL_REP]++
         scn_[SCN_REFRESH] := OBJ_REFRESH_ALL
      ELSE
         lMoved := .f.
         tone( 100,1 )
         scn_[SCN_COL_CUR]--
      ENDIF
   ELSE
      scn_[SCN_COL_REP]++
   ENDIF
   RETURN lMoved

//----------------------------------------------------------------------//

FUNCTION scrMovLft( scn_ )
   LOCAL lMoved := .t.
   scn_[SCN_COL_CUR]--
   IF scn_[SCN_COL_CUR] < scn_[SCN_LEFT]
      IF scn_[SCN_COL_REP] > 1
         scn_[SCN_COL_DIS]++
         scn_[SCN_COL_CUR]++
         scn_[SCN_COL_REP]--
         scn_[SCN_REFRESH] := OBJ_REFRESH_ALL
      ELSE
         lMoved := .f.
         tone(200,1)
         scn_[SCN_COL_CUR]++
      ENDIF
   ELSE
      scn_[SCN_COL_REP]--
   ENDIF
   RETURN lMoved

//----------------------------------------------------------------------//

FUNCTION scrMovUp(scn_)
   LOCAL lMoved := .t.

   scn_[SCN_ROW_CUR]--
   IF scn_[SCN_ROW_CUR] < scn_[SCN_TOP]
      scn_[SCN_ROW_CUR] := scn_[SCN_TOP]
      IF scn_[SCN_ROW_REP] > 1
         scn_[SCN_ROW_DIS]++
         scn_[SCN_ROW_REP]--
         scn_[SCN_REFRESH] := OBJ_REFRESH_ALL
      ELSE
         lMoved := .f.
         tone(300,1)
      ENDIF
   ELSE
      scn_[SCN_ROW_REP]--
   ENDIF
   RETURN lMoved

//----------------------------------------------------------------------//

FUNCTION scrMovDn( scn_ )
   LOCAL lMoved := .t.

   scn_[ SCN_ROW_CUR ]++
   IF scn_[ SCN_ROW_CUR ]  > scn_[ SCN_BOTTOM ]
      scn_[ SCN_ROW_CUR ] := scn_[ SCN_BOTTOM ]
      IF scn_[ SCN_ROW_REP ] < scn_[ SCN_REP_LINES ]
         scn_[ SCN_ROW_DIS ]--
         scn_[ SCN_ROW_REP ]++
         scn_[ SCN_REFRESH ] := OBJ_REFRESH_ALL
      ELSE
         lMoved := .f.
         tone(300,1)
      ENDIF
   ELSE
      scn_[ SCN_ROW_REP ]++
   ENDIF
   RETURN lMoved

//----------------------------------------------------------------------//

FUNCTION scrMovPgUp(scn_)
   LOCAL lMoved := .f.

   IF scn_[SCN_ROW_CUR] == scn_[SCN_TOP]
      IF scn_[SCN_ROW_REP] > 1
         scn_[SCN_ROW_CUR] := scn_[SCN_TOP]
         scn_[SCN_ROW_REP] := 1
         scn_[SCN_ROW_DIS] := scn_[SCN_TOP] - 1
         lMoved := .t.
         scn_[SCN_REFRESH] := OBJ_REFRESH_ALL
      ENDIF
   ELSE                    //  IF scn_[SCN_ROW_CUR] == scn_[SCN_ROW_BOTTOM]
      scn_[SCN_ROW_CUR] := scn_[SCN_TOP]
      scn_[SCN_ROW_REP] := scn_[SCN_ROW_REP] - (scn_[SCN_ROW_CUR] - scn_[SCN_TOP])
      scn_[SCN_ROW_DIS] := scn_[SCN_ROW_DIS] - (scn_[SCN_ROW_CUR] - scn_[SCN_TOP])
      lMoved := .t.
   ENDIF

   RETURN lMoved

//----------------------------------------------------------------------//

STATIC FUNCTION scrIsObjTxt(obj_,scn_)
   RETURN ascan(obj_, {|e_| e_[OBJ_TYPE]==OBJ_O_TEXT;
         .AND. ;
         VouchInRange(scn_[SCN_ROW_REP],e_[OBJ_ROW],e_[OBJ_TO_ROW]) ;
         .AND. ;
         VouchInRange(scn_[SCN_COL_REP],e_[OBJ_COL],e_[OBJ_TO_COL]) }) > 0

//----------------------------------------------------------------------//

FUNCTION scrChkObj(obj_,scn_)
   LOCAL n
   n := ascan(obj_,{|e_| IF(e_[OBJ_TYPE]==OBJ_O_BOX .OR. e_[OBJ_TYPE]==OBJ_O_BMP,.f.,;
         VouchInRange(scn_[SCN_ROW_REP],e_[OBJ_ROW],e_[OBJ_TO_ROW]) ;
         .AND. ;
         VouchInRange(scn_[SCN_COL_REP],e_[OBJ_COL],e_[OBJ_TO_COL])) })
   IF empty(n)  //  No OBJECT other than box, check box,BMP
      n := ascan(obj_,{|e_| ;
         VouchInRange(scn_[SCN_ROW_REP],e_[OBJ_ROW],e_[OBJ_TO_ROW]) ;
         .AND. ;
         VouchInRange(scn_[SCN_COL_REP],e_[OBJ_COL],e_[OBJ_TO_COL]) })
   ENDIF
   RETURN n

//----------------------------------------------------------------------//

STATIC FUNCTION scrUpdObjRC(obj_,scn_)
   LOCAL nW, nH
   LOCAL nObj := scn_[ SCN_OBJ_SELECTED ]

   IF nObj > 0
      nH := obj_[ nObj,OBJ_TO_ROW ] - obj_[ nObj,OBJ_ROW ]
      nW := obj_[ nObj,OBJ_TO_COL ] - obj_[ nObj,OBJ_COL ]

      obj_[ nObj,OBJ_ROW    ] := scn_[SCN_ROW_REP]
      obj_[ nObj,OBJ_COL    ] := scn_[SCN_COL_REP]

      IF obj_[ nObj,OBJ_TYPE ] == OBJ_O_BOX
         obj_[ nObj,OBJ_TO_ROW ] := obj_[ nObj,OBJ_ROW ] + nH
         obj_[ nObj,OBJ_TO_COL ] := obj_[ nObj,OBJ_COL ] + nW
      ELSE
         obj_[ nObj,OBJ_TO_ROW ] := scn_[SCN_ROW_REP]
         obj_[ nObj,OBJ_TO_COL ] := scn_[SCN_COL_REP] + ;
                  len( obj_[ nObj, iif( obj_[ nObj,OBJ_TYPE ] == OBJ_O_TEXT, OBJ_EQN, OBJ_TEXT ) ] ) - 1
      ENDIF
   ENDIF
   RETURN NIL

//----------------------------------------------------------------------//

STATIC FUNCTION scrRepCol( obj_,scn_ )
   LOCAL oCol := scn_[SCN_COL_MAX], nCol

   HB_SYMBOL_UNUSED( obj_ )

   nCol := VouchGetSome( 'Number of Columns?', oCol )

   IF !empty( nCol )
      nCol := max( 10,nCol )
      scn_[SCN_COL_MAX ]             := nCol
      scn_[SCN_RIGHT   ]             := min( maxCol(), scn_[SCN_LEFT]+nCol-1 )
      scn_[SCN_REFRESH ]             := OBJ_REFRESH_ALL
      scn_[ SCN_PROPERTY, REP_COLS ] := nCol
   ENDIF

   RETURN NIL

//----------------------------------------------------------------------//
/*
   This is the routine FROM where row based equations can be implemented
*/
STATIC FUNCTION scrAddLine(obj_,scn_)
   LOCAL nRow := scn_[SCN_ROW_REP], nSct

   scn_[SCN_REFRESH] := OBJ_REFRESH_ALL
   scn_[SCN_BOTTOM ] := min(scn_[SCN_BOTTOM]+1,maxrow()-3)

   nSct := scrSecOrd(scn_,nRow /*scn_[SCN_ROW_REP]*/)

   scn_[SCN_SECTORS_,nSct,SCT_ROWS]++
   scn_[SCN_REP_LINES]++

   aeval(obj_,{|e_,i| IF(e_[OBJ_ROW] >= nRow, obj_[i,OBJ_TO_ROW] += 1,'') })
   aeval(obj_,{|e_,i| IF(e_[OBJ_ROW] >= nRow, obj_[i,OBJ_ROW   ] += 1,'') })

   scn_[SCN_REFRESH] := OBJ_REFRESH_ALL

   RETURN NIL

//----------------------------------------------------------------------//

STATIC FUNCTION scrDelLine( obj_, scn_ )
   LOCAL nRow := scn_[ SCN_ROW_REP ]
   LOCAL nSct, n, isLast

   isLast := nRow == scn_[ SCN_REP_LINES ]

   nSct   := scrSecOrd( scn_,nRow /*scn_[SCN_ROW_REP]*/ )
   IF scn_[ SCN_SECTORS_, nSct, SCT_ROWS ] == 1    //  A Single Row Must remain IN one group
      RETURN NIL
   ENDIF

   scn_[ SCN_SECTORS_, nSct,SCT_ROWS ]--
   scn_[ SCN_REP_LINES ]--

   IF scn_[ SCN_REP_LINES ] < (scn_[SCN_BOTTOM]-scn_[SCN_TOP]+1)
      scn_[ SCN_BOTTOM ] := max( scn_[ SCN_TOP ], min( scn_[ SCN_BOTTOM ] - 1, maxrow() - 3 ) )
   ENDIF

   DO WHILE .t.
      IF ( n := ascan( obj_, {|e_| e_[ OBJ_ROW ] == nRow } ) ) == 0
         EXIT
      ENDIF
      VouchAShrink( obj_, n )
   ENDDO
   IF empty( obj_ )
      aadd( obj_, scrObjBlank() )
   ENDIF

   aeval( obj_, {|e_,i| iif( e_[ OBJ_ROW ] > nRow, obj_[ i, OBJ_TO_ROW ] -= 1, '' ) } )
   aeval( obj_, {|e_,i| iif( e_[ OBJ_ROW ] > nRow, obj_[ i, OBJ_ROW    ] -= 1, '' ) } )

   IF isLast
      scn_[SCN_ROW_REP]--
      scn_[SCN_ROW_CUR]--
   ENDIF

   scn_[SCN_REFRESH] := OBJ_REFRESH_ALL

   RETURN NIL

//----------------------------------------------------------------------//

STATIC FUNCTION scrIsBoxIn(obj_,scn_)
   RETURN ascan( obj_,{|e_| VouchInRange( scn_[SCN_ROW_REP ], e_[ OBJ_ROW ], e_[ OBJ_TO_ROW ] );
                                     .AND. ;
                            (e_[OBJ_TYPE] == OBJ_O_BOX ;
                                     .OR. ;
                             e_[OBJ_TYPE] == OBJ_O_BMP ) } )    >    0

//----------------------------------------------------------------------//

STATIC FUNCTION scrObjCopy(obj_,scn_)

   HB_SYMBOL_UNUSED( obj_ )

   IF scn_[SCN_MODE] == OBJ_MODE_SELECT
      scn_[SCN_OBJ_COPIED] := scn_[SCN_OBJ_SELECTED]
   ELSEIF scn_[SCN_OBJ_HILITE] > 0
      scn_[SCN_OBJ_COPIED] := scn_[SCN_OBJ_HILITE]
   ENDIF
   RETURN NIL

//----------------------------------------------------------------------//

STATIC FUNCTION scrObjPas(obj_,scn_)       //  Paste Copied OBJECT
   LOCAL nObj,o_,oldRow,oldCol,oldRow2,oldcol2

   IF (nObj := scn_[SCN_OBJ_COPIED]) > 0 .AND. scn_[SCN_OBJ_SELECTED] == 0
      o_:= aclone(obj_[ nObj])

      oldRow  := o_[OBJ_ROW]    ; oldCol  := o_[OBJ_COL]
      oldRow2 := o_[OBJ_TO_ROW] ; oldCol2 := o_[OBJ_TO_COL]

      o_[OBJ_ROW]         := scn_[SCN_ROW_REP]
      o_[OBJ_COL]         := scn_[SCN_COL_REP]
      IF o_[OBJ_TYPE  ]   == OBJ_O_FIELD .OR. o_[OBJ_TYPE] == OBJ_O_EXP
         o_[OBJ_TO_ROW]   := scn_[SCN_ROW_REP]
         o_[OBJ_TO_COL]   := scn_[SCN_COL_REP] + len(o_[OBJ_TEXT]) - 1
      ELSEIF o_[OBJ_TYPE] == OBJ_O_BOX .OR. o_[OBJ_TYPE] == OBJ_O_BMP
         o_[OBJ_TO_ROW]   := scn_[SCN_ROW_REP] + (oldRow2-oldRow)
         o_[OBJ_TO_COL]   := scn_[SCN_COL_REP] + (oldCol2-oldCol)
      ELSEIF o_[OBJ_TYPE] == OBJ_O_TEXT
         o_[OBJ_TO_ROW]   := scn_[SCN_ROW_REP]
         o_[OBJ_TO_COL]   := scn_[SCN_COL_REP] + (oldCol2-oldCol)
      ENDIF

      o_[OBJ_SECTION]     := scrSecCur(scn_,scn_[SCN_ROW_REP])

      aadd(obj_,o_)

      scrOrdObj( obj_ )
      scn_[ SCN_OBJ_SELECTED ] := 0
      scn_[ SCN_REFRESH      ] := OBJ_REFRESH_LINE
      scn_[ SCN_MODE         ] := 0
      scn_[ SCN_OBJ_COPIED   ] := 0
   ENDIF
   RETURN NIL

//----------------------------------------------------------------------//

STATIC FUNCTION scrObjDel(obj_,scn_,nObj)
   LOCAL s_:= obj_,nUnique := obj_[ nObj,OBJ_OBJ_UNIQUE],n

   VouchAShrink(s_,nObj)
   IF empty(s_)
      aadd(s_,scrObjBlank())
   ENDIF
   scn_[SCN_OBJ_SELECTED] := 0
   scn_[SCN_REFRESH]      := OBJ_REFRESH_LINE

   IF scn_[SCN_DESIGN] == DGN_MODULE .AND. nUnique > 0
      IF (n := ascan(scn_[SCN_FIELDS],{|e_| e_[1] == nUnique })) > 0
         VouchAShrink(scn_[SCN_FIELDS],n)
      ENDIF
   ENDIF
   RETURN s_

//----------------------------------------------------------------------//

STATIC FUNCTION scrObject(obj_,scn_)
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

   @ scn_[ SCN_ROW_CUR ], scn_[ SCN_COL_CUR ] SAY ''

   DO CASE
   CASE nObj == 1                              //  Field
      scrAddFld( obj_, scn_, 0 )
   CASE nObj == 2                              //  Box
      scrAddBox( obj_, scn_, 0 )
   CASE nObj == 3                              //  Blank

   CASE nObj == 4                              //  Columns
      scrRepCol( obj_, scn_ )
   CASE nObj == 5                              //  Graphcs
      //graphChar()
      scn_[ SCN_GRAPHICS ] := ! scn_[ SCN_GRAPHICS ]
   CASE nObj == 6                              //  Blank

   CASE nObj == 7                              //  Copy
      scrObjCopy( obj_, scn_ )
   CASE nObj ==81                              //  Paste
      scrObjPas( obj_, scn_)
   CASE nObj == 9                              //  Block Selection
      scrTextBlock( obj_, scn_ )
   CASE nObj == 10                             //  Copy Selectin
      obj_:= scrTextMove( obj_, scn_, 1 )
   CASE nObj == 11                             //  Copy & Cut Selection
      obj_:= scrTextMove( obj_, scn_, 0 )
   CASE nObj == 12                             //  Blank

   CASE nObj == 13                             //  Matrix
   ENDCASE

   RETURN nObj

//----------------------------------------------------------------------//

STATIC FUNCTION scrTxtProp( obj_, scn_, nObj )
   LOCAL sel_, v_

   obj_[ nObj,OBJ_F_LEN  ] := len(obj_[ nObj,OBJ_EQN ] )
   obj_[ nObj,OBJ_F_TYPE ] := 'C'

   v_:= scrObj2Vv( obj_[ nObj ] )
   sel_:= scrVvSelAble(scn_)

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

   scrField( nObj, 3, obj_, scn_, v_, sel_, 'W/B    ' )

   RETURN NIL

//----------------------------------------------------------------------//

FUNCTION scrOnLastCol( obj_, scn_, nObj )
   LOCAL nOff, i

   IF obj_[ nObj, OBJ_TYPE ] == OBJ_O_BOX
      nOff := obj_[ nObj, OBJ_TO_COL ] - scn_[ SCN_COL_CUR ] - 1
      FOR i := 1 TO nOff
         scrMovRgt( scn_ )
         scrMove( obj_,scn_ )
         scrStatus( obj_,scn_ )
      NEXT

      nOff := obj_[ nObj, OBJ_TO_ROW ] - scn_[ SCN_ROW_CUR ] - 1
      FOR i := 1 TO nOff
         scrMovDn( scn_ )
         scrMove( obj_,scn_ )
         scrStatus( obj_,scn_ )
      NEXT

      SetPos( scn_[ SCN_ROW_CUR ], scn_[ SCN_COL_CUR ] )
   ENDIF

   RETURN NIL

//----------------------------------------------------------------------//

STATIC FUNCTION scrOnFirstCol( obj_,scn_,nObj,type_ )
   LOCAL nCur, nOff

   IF scn_[ SCN_COL_REP ] <> obj_[ nObj,OBJ_COL ]
      IF VouchInArray( obj_[ nObj, OBJ_TYPE ], type_ )
         IF obj_[ nObj, OBJ_TYPE ] == OBJ_O_BOX
            nCur := scn_[ SCN_COL_CUR ]
            nOff := scn_[ SCN_COL_REP ] - obj_[ nObj, OBJ_COL ]
            scn_[ SCN_COL_CUR ] := max( scn_[ SCN_LEFT ], scn_[ SCN_COL_CUR ] - nOff )
            scn_[ SCN_COL_REP ] := obj_[ nObj, OBJ_COL]
            IF nOff > nCur - scn_[ SCN_LEFT ]
               scn_[ SCN_REFRESH ] := OBJ_REFRESH_ALL
               scn_[ SCN_COL_DIS ] += nOff - ( nCur - scn_[ SCN_LEFT ] )
            ENDIF

            nCur := scn_[ SCN_ROW_CUR ]
            nOff := scn_[ SCN_ROW_REP ] - obj_[ nObj, OBJ_ROW ]
            scn_[ SCN_ROW_CUR ] := max( scn_[ SCN_TOP ], scn_[ SCN_ROW_CUR ] - nOff )
            scn_[ SCN_ROW_REP ] := obj_[ nObj,OBJ_ROW ]
            IF nOff > nCur - scn_[ SCN_TOP ]
               scn_[ SCN_REFRESH ] := OBJ_REFRESH_ALL
               scn_[ SCN_ROW_DIS ] += nOff - ( nCur - scn_[ SCN_TOP ] )
            ENDIF

         ELSE
            IF scn_[ SCN_LASTKEY ] == K_RIGHT
               nCur := scn_[ SCN_COL_CUR ]
               nOff := obj_[ nObj, OBJ_TO_COL ] - scn_[ SCN_COL_REP ] + 1 //  NEXT Col TO OBJECT
               IF scn_[SCN_COL_REP] + nOff > scn_[SCN_COL_MAX]
                  scn_[SCN_COL_MAX] := scn_[ SCN_COL_REP ] + nOff
               ENDIF
               scn_[SCN_COL_CUR] := min( scn_[ SCN_RIGHT ], scn_[ SCN_COL_CUR ] + nOff )
               scn_[SCN_COL_REP] := obj_[ nObj,OBJ_TO_COL ] + 1
               IF nOff > scn_[SCN_RIGHT] - nCur
                  scn_[SCN_REFRESH] := OBJ_REFRESH_ALL
                  scn_[SCN_COL_DIS] -= nOff-(scn_[SCN_RIGHT]-nCur)
               ENDIF
               scn_[SCN_OBJ_HILITE] := 0
            ELSE
               nCur := scn_[ SCN_COL_CUR ]
               nOff := scn_[ SCN_COL_REP ] - obj_[ nObj,OBJ_COL ]
               scn_[ SCN_COL_CUR ] := max( scn_[ SCN_LEFT ], scn_[ SCN_COL_CUR ]-nOff )
               scn_[ SCN_COL_REP ] := obj_[ nObj,OBJ_COL]
               IF nOff > nCur-scn_[SCN_LEFT]
                  scn_[SCN_REFRESH] := OBJ_REFRESH_ALL
                  scn_[SCN_COL_DIS] += nOff-(nCur-scn_[SCN_LEFT])
               ENDIF
           ENDIF
        ENDIF
      ENDIF
   ENDIF

   RETURN NIL

//----------------------------------------------------------------------//

FUNCTION scrGetChar(obj_,nRow,nCol)
   LOCAL s := THE_FILL,n

   //  Locate Text
   n := ascan(obj_,{|e_| e_[ OBJ_ROW ] == nRow .AND. ;
                     VouchInRange( nCol, e_[ OBJ_COL ], e_[ OBJ_TO_COL ] ) } )
   IF n == 0   //  Locate Box
      n := ascan(obj_,{|e_| VouchInRange( nRow, e_[ OBJ_ROW ], e_[ OBJ_TO_ROW ] ) .AND. ;
                            VouchInRange( nCol, e_[ OBJ_COL ], e_[ OBJ_TO_COL ] ) } )
   ENDIF

   IF n > 0
      IF     obj_[n,OBJ_TYPE]==OBJ_O_TEXT
         s := substr(obj_[n,OBJ_EQN],nCol-obj_[n,OBJ_COL]+1,1)

      ELSEIF obj_[n,OBJ_TYPE]==OBJ_O_FIELD .OR. obj_[n,OBJ_TYPE]==OBJ_O_EXP
         s := substr(obj_[n,OBJ_ID ],nCol-obj_[n,OBJ_COL]+1,1)

      ELSEIF obj_[n,OBJ_TYPE]==OBJ_O_BOX .OR. obj_[n,OBJ_TYPE]==OBJ_O_BMP
         IF     nRow == obj_[n,OBJ_ROW   ]
            IF     nCol == obj_[n,OBJ_COL]
               s := substr(obj_[n,OBJ_BOX_SHAPE],1,1)
            ELSEIF nCol == obj_[n,OBJ_TO_COL]
               s := substr(obj_[n,OBJ_BOX_SHAPE],3,1)
            ELSE
               s := substr(obj_[n,OBJ_BOX_SHAPE],2,1)
            ENDIF
         ELSEIF nRow == obj_[n,OBJ_TO_ROW]
            IF     nCol == obj_[n,OBJ_COL]
               s := substr(obj_[n,OBJ_BOX_SHAPE],7,1)
            ELSEIF nCol == obj_[n,OBJ_TO_COL]
               s := substr(obj_[n,OBJ_BOX_SHAPE],5,1)
            ELSE
               s := substr(obj_[n,OBJ_BOX_SHAPE],6,1)
            ENDIF
         ELSE
            IF     nCol == obj_[n,OBJ_COL]
               s := substr(obj_[n,OBJ_BOX_SHAPE],8,1)   //  4.8 are Same
            ELSEIF nCol == obj_[n,OBJ_TO_COL]
               s := substr(obj_[n,OBJ_BOX_SHAPE],4,1)
            ELSE
               s := substr(obj_[n,OBJ_BOX_SHAPE],9,1)
               s := IF(empty(s),THE_FILL,s)
            ENDIF
         ENDIF
      ENDIF
   ENDIF
   RETURN s

//----------------------------------------------------------------------//

STATIC FUNCTION scrTextBlock(obj_,scn_)
   LOCAL n, nKey
   LOCAL key_:= { K_RIGHT,K_LEFT,K_UP,K_DOWN,K_ENTER }

   scrMsg( 'Use <Arrow Keys> TO Select Text Block, <Enter> TO Finish' )
   scn_[ SCN_TEXT_BLOCK_ ] := { scn_[ SCN_ROW_REP ], scn_[ SCN_COL_REP ], scn_[ SCN_ROW_REP ], scn_[ SCN_COL_REP ] }
   scrMove( obj_, scn_ )
   scrStatus( obj_, scn_ )

   DO WHILE .t.
      nKey := scrInkey( key_ )

      DO CASE
      CASE nKey == key_[1]
         IF scrMovRgt(scn_)
            scn_[SCN_TEXT_BLOCK_,4]++
         ENDIF
      CASE nKey == key_[2]
         IF scrMovLft(scn_)
            scn_[SCN_TEXT_BLOCK_,4]--
         ENDIF
      CASE nKey == key_[3]
         IF scrMovUp(scn_)
            scn_[SCN_TEXT_BLOCK_,3]--
         ENDIF
      CASE nKey == key_[4]
         IF scrMovDn(scn_)
            scn_[SCN_TEXT_BLOCK_,3]++
         ENDIF
      CASE nKey == key_[5]
         EXIT
      ENDCASE

      IF scn_[SCN_TEXT_BLOCK_,3] < scn_[SCN_TEXT_BLOCK_,1]
         n := scn_[SCN_TEXT_BLOCK_,1]
         scn_[SCN_TEXT_BLOCK_,1] := scn_[SCN_TEXT_BLOCK_,3]
         scn_[SCN_TEXT_BLOCK_,3] := n
      ENDIF

      IF scn_[SCN_TEXT_BLOCK_,4] < scn_[SCN_TEXT_BLOCK_,2]
         n := scn_[SCN_TEXT_BLOCK_,2]
         scn_[SCN_TEXT_BLOCK_,2] := scn_[SCN_TEXT_BLOCK_,4]
         scn_[SCN_TEXT_BLOCK_,4] := n
      ENDIF

      scrMove(obj_,scn_)
      scrStatus(obj_,scn_)
   ENDDO
   scrMsg('')

   RETURN NIL

//----------------------------------------------------------------------//

STATIC FUNCTION scrTextMove(obj_,scn_,nMode)
   LOCAL gst_,nKey
   LOCAL crs := setCursor(0)
   LOCAL key_:= {K_RIGHT,K_LEFT,K_UP,K_DOWN,K_ENTER}

   DEFAULT nMode TO 0   //  0.Paste   1.Copy

   IF ! empty( scn_[ SCN_TEXT_BLOCK_ ] )
      //  CREATE a ghost movement block
      scrMsg('Use Arrow Keys TO Move Selected Block')
      //  Check FOR current cursor position
      gst_:= { scn_[ SCN_ROW_REP ] , scn_[ SCN_COL_REP ],;
               scn_[ SCN_ROW_REP ] + scn_[ SCN_TEXT_BLOCK_, 3 ] - scn_[ SCN_TEXT_BLOCK_, 1 ],;
               scn_[ SCN_COL_REP ] + scn_[ SCN_TEXT_BLOCK_, 4 ] - scn_[ SCN_TEXT_BLOCK_, 2 ] }
      DO WHILE .t.
         scrMove(obj_,scn_)
         scrDispGhost(obj_,scn_,gst_)
         scrStatus(obj_,scn_)

         nKey := scrInkey(key_)
         DO CASE
         CASE nKey == key_[1]
            IF scrMovRgt(scn_)
               gst_[2]++ ; gst_[4]++
            ENDIF
         CASE nKey == key_[2]
            IF scrMovLft(scn_)
               gst_[2]-- ; gst_[4]--
            ENDIF
         CASE nKey == key_[3]
            IF scrMovUp(scn_)
               gst_[1]-- ; gst_[3]--
            ENDIF
         CASE nKey == key_[4]
            IF scrMovDn(scn_)
               gst_[1]++ ; gst_[3]++
            ENDIF
         CASE nKey == key_[5]
            EXIT
         ENDCASE
      ENDDO
      //  Post Selected Block TO Moved Area
      obj_:= scrTextPost( obj_, scn_, gst_, nMode )

      scrOrdObj( obj_, scn_ )

      scrMove( obj_, scn_ )
      scrStatus( obj_, scn_ )

      scrMsg()
   ENDIF
   setCursor(crs)

   RETURN obj_

//----------------------------------------------------------------------//

STATIC FUNCTION scrTextPost( obj_, scn_, gst_, nMode )
   LOCAL n,i,s,s1,s2,s3,n1,nWid,nCol,nn
   LOCAL del_:={0},ins_:={},d_:={},ddd_
   LOCAL old_:= scn_[ SCN_TEXT_BLOCK_ ]

   FOR i := gst_[ 1 ] TO gst_[ 3 ]
      n := -1
      DO WHILE .t.
         n := ascan( obj_, {|e_| e_[ OBJ_ROW ] == i ;
                                       .AND. ;
                        ( VouchInRange(e_[OBJ_COL],gst_[2],gst_[4]);
                                       .OR. ;
                          VouchInRange(e_[OBJ_TO_COL],gst_[2],gst_[4])) ;
                                       .AND.;
                                   ! VouchInArray( n,del_ ) } )
         IF n > 0
            IF obj_[ n,OBJ_TYPE ] == OBJ_O_TEXT
               aadd( del_, n )

               s1    := '' ; s3 := ''
               s     := obj_[ n, OBJ_EQN ]
               nCol  := obj_[ n, OBJ_COL ]

               IF gst_[2] <= obj_[ n, OBJ_COL ] .AND. gst_[ 4 ] >= obj_[ n, OBJ_TO_COL ]
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
                  aadd( ins_,scrObjBlank() )
                  n1 := len( ins_ )

                  ins_[ n1, OBJ_TYPE    ] := OBJ_O_TEXT
                  ins_[ n1, OBJ_ROW     ] := obj_[ n, OBJ_ROW ]
                  ins_[ n1, OBJ_COL     ] := obj_[ n, OBJ_COL ]
                  ins_[ n1, OBJ_EQN     ] := s1
                  ins_[ n1, OBJ_ID      ] := 'Text'
                  ins_[ n1, OBJ_COLOR   ] := 'W/B'
                  ins_[ n1, OBJ_SECTION ] := obj_[ n, OBJ_SECTION ]
                  ins_[ n1, OBJ_TO_ROW  ] := obj_[ n, OBJ_ROW     ]
                  ins_[ n1, OBJ_TO_COL  ] := ins_[ n1, OBJ_COL ] + len( s1 ) - 1
               ENDIF

               IF len( s3 ) > 0
                  aadd( ins_, scrObjBlank() )
                  n1 := len( ins_ )

                  ins_[ n1, OBJ_TYPE    ] := OBJ_O_TEXT
                  ins_[ n1, OBJ_ROW     ] := obj_[n, OBJ_ROW]
                  ins_[ n1, OBJ_COL     ] := gst_[ 4 ] + 1
                  ins_[ n1, OBJ_EQN     ] := s3
                  ins_[ n1, OBJ_ID      ] := 'Text'
                  ins_[ n1, OBJ_COLOR   ] := 'W/B'
                  ins_[ n1, OBJ_SECTION ] := obj_[ n, OBJ_SECTION ]
                  ins_[ n1, OBJ_TO_ROW  ] := obj_[ n, OBJ_ROW     ]
                  ins_[ n1, OBJ_TO_COL  ] := ins_[ n1, OBJ_COL ] + len( s3 ) - 1
               ENDIF

            ELSEIF obj_[n,OBJ_TYPE] == OBJ_O_FIELD .OR. obj_[n,OBJ_TYPE] == OBJ_O_EXP
               aadd( del_, n )

            ELSEIF obj_[n,OBJ_TYPE] == OBJ_O_BOX

            ELSEIF obj_[n,OBJ_TYPE] == OBJ_O_BMP

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
         n := ascan(obj_,{|e_| e_[OBJ_ROW]==i;
                                      .AND. ;
                     (VouchInRange(e_[OBJ_COL],old_[2],old_[4]);
                                       .OR. ;
                      VouchInRange(e_[OBJ_TO_COL],old_[2],old_[4])) ;
                                      .AND. ;
                                  !VouchInArray(n,del_) })
         IF n > 0
            IF     obj_[n,OBJ_TYPE] == OBJ_O_TEXT
               aadd(del_,n)

               //  TO be retained as it is
               s1    := '' ; s2 := '' ; s3 := ''
               s     := obj_[n,OBJ_EQN]
               nCol  := obj_[n,OBJ_COL]

               IF old_[2] <= obj_[n,OBJ_COL] .AND. old_[4] >= obj_[n,OBJ_TO_COL]
                  s2 := s   //  Insert WITH moved coordinates
               ELSEIF  old_[2] >= obj_[n,OBJ_COL]
                  s1 := substr(s,1,old_[2]-nCol)
                  s2 := substr(s,old_[2]-nCol+1,old_[4]-old_[2]+1)
                  s3 := substr(s,old_[4]-nCol+2)
               ELSEIF old_[2] < nCol
                  s1 := substr(s,1,old_[2]-nCol)
                  s2 := substr(s,old_[2]-nCol+1,old_[4]-old_[2]+1)
                  s3 := substr(s,old_[4]-nCol+2)
               ENDIF

               IF nMode == 0
                  IF len(s1)>0
                     aadd(ins_,scrObjBlank())
                     n1 := len(ins_)
                     ins_[n1,OBJ_TYPE]    := OBJ_O_TEXT
                     ins_[n1,OBJ_ROW]     := obj_[n,OBJ_ROW]
                     ins_[n1,OBJ_COL]     := obj_[n,OBJ_COL]
                     ins_[n1,OBJ_EQN]     := s1
                     ins_[n1,OBJ_ID]      := 'Text'
                     ins_[n1,OBJ_COLOR]   := 'W/B'
                     ins_[n1,OBJ_SECTION] := obj_[n,OBJ_SECTION]
                     ins_[n1,OBJ_TO_ROW]  := obj_[n,OBJ_ROW    ]
                     ins_[n1,OBJ_TO_COL]  := ins_[n1,OBJ_COL]+len(s1)-1
                  ENDIF
                  IF len(s3) > 0
                     aadd(ins_,scrObjBlank())
                     n1                   := len(ins_)
                     ins_[n1,OBJ_TYPE]    := OBJ_O_TEXT
                     ins_[n1,OBJ_ROW]     := obj_[n,OBJ_ROW]
                     ins_[n1,OBJ_COL]     := old_[4]+1
                     ins_[n1,OBJ_EQN]     := s3
                     ins_[n1,OBJ_ID]      := 'Text'
                     ins_[n1,OBJ_COLOR]   := 'W/B'
                     ins_[n1,OBJ_SECTION] := obj_[n,OBJ_SECTION]
                     ins_[n1,OBJ_TO_ROW]  := obj_[n,OBJ_ROW    ]
                     ins_[n1,OBJ_TO_COL]  := ins_[n1,OBJ_COL]+len(s3)-1
                  ENDIF
               ENDIF

               IF len(s2) > 0
                  aadd(ins_,aclone(obj_[n]))
                  n1                   := len(ins_)
                  ins_[n1,OBJ_ROW]     := gst_[1]+nn
                  ins_[n1,OBJ_COL]     := gst_[2]+IF(old_[2]-obj_[n,OBJ_COL]>=0,;
                                             0,abs(old_[2]-obj_[n,OBJ_COL]))
                  ins_[n1,OBJ_TO_ROW]  := ins_[n1,OBJ_ROW]
                  ins_[n1,OBJ_TO_COL]  := ins_[n1,OBJ_COL]+len(s2)-1
                  ins_[n1,OBJ_EQN]     := s2
               ENDIF

            ELSEIF obj_[n,OBJ_TYPE] == OBJ_O_FIELD .OR. ;
                   obj_[n,OBJ_TYPE] == OBJ_O_EXP
               IF nMode == 0
                  aadd(del_,n)
               ENDIF

               //  Same OBJECT is TO be inserted IN moved block
               aadd(ins_,aclone(obj_[n]))
               n1                  := len(ins_)
               ins_[n1,OBJ_ROW]    := gst_[1]+nn
               nWid                := obj_[n,OBJ_TO_COL]-obj_[n,OBJ_COL]
               ins_[n1,OBJ_COL]    := gst_[2]+old_[2]-obj_[n,OBJ_COL]
               ins_[n1,OBJ_TO_ROW] := ins_[n1,OBJ_ROW]
               ins_[n1,OBJ_TO_COL] := ins_[n1,OBJ_COL]+nWid
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
      FOR i := 1 TO len( obj_)
         IF ascan( del_, i ) == 0
            aadd( d_, obj_[ i ] )
         ENDIF
      NEXT
      obj_:= aclone( d_ )
      IF empty( obj_ )
         aadd( obj_, scrObjBlank() )
      ENDIF
   ENDIF

   aeval( ins_, {|e_| aadd( obj_, e_ ) } )

   scn_[SCN_TEXT_BLOCK_] := {}

   RETURN obj_

//----------------------------------------------------------------------//

STATIC FUNCTION scrTextDel(obj_,scn_)
   LOCAL i,n,n1,s,s1,s3,nCol,ins_:={},del_:={},d_:={},old_:={}

   old_:= scn_[SCN_TEXT_BLOCK_]
   FOR i := old_[1] TO old_[3]    //  Rows
      n := -1

      DO WHILE .t.
         n := ascan(obj_,{|e_| e_[OBJ_ROW]==i;
                                      .AND. ;
                     (VouchInRange(e_[OBJ_COL],   old_[2],old_[4]);
                                       .OR. ;
                      VouchInRange(e_[OBJ_TO_COL],old_[2],old_[4])) ;
                                      .AND. ;
                                  !VouchInArray(n,del_) })
         IF n > 0
            IF     obj_[n,OBJ_TYPE] == OBJ_O_TEXT
               aadd(del_,n)

               //  TO be retained as it is
               s1    := '' ; s3 := ''
               s     := obj_[n,OBJ_EQN]
               nCol  := obj_[n,OBJ_COL]

               IF old_[2] <= obj_[n,OBJ_COL] .AND. old_[4] >= obj_[n,OBJ_TO_COL]
                  //  s2 := s   //  Insert WITH moved coordinates
               ELSEIF  old_[2] >= obj_[n,OBJ_COL]
                  s1 := substr(s,1,old_[2]-nCol)
                  //  s2 := substr(s,old_[2]-nCol+1,old_[4]-old_[2]+1)
                  s3 := substr(s,old_[4]-nCol+2)
               ELSEIF old_[2] < nCol
                  s1 := substr(s,1,old_[2]-nCol)
                  //  s2 := substr(s,old_[2]-nCol+1,old_[4]-old_[2]+1)
                  s3 := substr(s,old_[4]-nCol+2)
               ENDIF

               IF len(s1)>0
                  aadd(ins_,scrObjBlank())
                  n1 := len(ins_)
                  ins_[n1,OBJ_TYPE]    := OBJ_O_TEXT
                  ins_[n1,OBJ_ROW]     := obj_[n,OBJ_ROW]
                  ins_[n1,OBJ_COL]     := obj_[n,OBJ_COL]
                  ins_[n1,OBJ_EQN]     := s1
                  ins_[n1,OBJ_ID]      := 'Text'
                  ins_[n1,OBJ_COLOR]   := 'W/B'
                  ins_[n1,OBJ_SECTION] := obj_[n,OBJ_SECTION]
                  ins_[n1,OBJ_TO_ROW]  := obj_[n,OBJ_ROW    ]
                  ins_[n1,OBJ_TO_COL]  := ins_[n1,OBJ_COL]+len(s1)-1
               ENDIF
               IF len(s3) > 0
                  aadd(ins_,scrObjBlank())
                  n1                   := len(ins_)
                  ins_[n1,OBJ_TYPE]    := OBJ_O_TEXT
                  ins_[n1,OBJ_ROW]     := obj_[n,OBJ_ROW]
                  ins_[n1,OBJ_COL]     := old_[4]+1
                  ins_[n1,OBJ_EQN]     := s3
                  ins_[n1,OBJ_ID]      := 'Text'
                  ins_[n1,OBJ_COLOR]   := 'W/B'
                  ins_[n1,OBJ_SECTION] := obj_[n,OBJ_SECTION]
                  ins_[n1,OBJ_TO_ROW]  := obj_[n,OBJ_ROW    ]
                  ins_[n1,OBJ_TO_COL]  := ins_[n1,OBJ_COL]+len(s3)-1
               ENDIF

            ELSEIF obj_[n,OBJ_TYPE] == OBJ_O_FIELD .OR. ;
                   obj_[n,OBJ_TYPE] == OBJ_O_EXP
               aadd(del_,n)

            ENDIF
         ELSE
            EXIT
         ENDIF
      ENDDO
      //nn++
   NEXT

   IF !empty(del_)
      FOR i := 1 TO len(obj_)
         IF ascan(del_,i) == 0
            aadd(d_,obj_[i])
         ENDIF
      NEXT
      obj_:= aclone(d_)
      IF empty(obj_)
         aadd(obj_,scrObjBlank())
      ENDIF
   ENDIF

   aeval(ins_,{|e_| aadd(obj_,e_) })
   scn_[SCN_TEXT_BLOCK_] := {}

   RETURN obj_

//----------------------------------------------------------------------//

STATIC FUNCTION scrExport( obj_,scn_ )
   LOCAL cFile := 'SCREEN'
   LOCAL rpt_

   cFile := VouchGetSome( 'Screen Name Please', pad( cFile,100 ) )
   IF !empty( cFile )
      rpt_:= objScn2Rpt( obj_, scn_, scn_[ SCN_NMODE ] )
      //save_array( rpt_, trim( cFile ) + '.vsc' )
      uiDebug( rpt_ )
   ENDIF

   RETURN file( cFile )

//----------------------------------------------------------------------//

STATIC FUNCTION scrImport( obj_,scn_ )
   LOCAL cFile := 'SCREEN'
   LOCAL rpt_:={},aa_

   cFile := VouchGetSome( 'Screen TO Import?', pad( cFile,100 ) )
   IF ! empty( cFile )
      //rpt_:= rest_array( trim( cFile ) + '.vsc' )
   ENDIF
   IF !empty( rpt_ )
      aa_:= rpt2ObjScn( scn_[SCN_COBJECT], rpt_, scn_[SCN_NMODE], {},;
                        scn_[SCN_CRPT], scn_[SCN_NWHERE], scn_[SCN_LMSG] )
      obj_:= aa_[ 1 ]
      scn_:= aa_[ 2 ]
   ENDIF

   scn_[SCN_REFRESH] := OBJ_REFRESH_ALL

   RETURN NIL

//----------------------------------------------------------------------//

