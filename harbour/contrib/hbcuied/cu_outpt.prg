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
#include "inkey.ch"
#include "wvtwin.ch"

/*----------------------------------------------------------------------*/

#define K_WVT_FULLSCREEN  299701

#define K_WVT_BITMAP      299705
#define K_WVT_FRAME       299706
#define K_WVT_ELLIPSE     299707
#define K_WVT_LINE_H      299708
#define K_WVT_LINE_V      299709
#define K_WVT_GRID        299710
#define K_WVT_BARCODE     299711
#define K_WVT_TEXTBOX     299712

#define K_WVT_CARRY       299721
#define K_WVT_BROUGHT     299722
#define K_WVT_SUMMARY     299723
#define K_WVT_GROUP       299724
#define K_WVT_MATRIX      299725

//----------------------------------------------------------------------//

FUNCTION ScrDisplay( scn_ )

   dispbegin()
   setcursor(0)
   setColor( scn_[ SCN_CLR_OVERALL ] )
   cls

   @ scn_[SCN_ROW_RULER], scn_[SCN_LEFT] ;
   SAY substr( scn_[SCN_RULER], 1, scn_[SCN_RIGHT] - scn_[SCN_LEFT] + 1 ) ;
   COLOR scn_[ SCN_CLR_RULER ]

   scrMsg()

   setcolor( scn_[ SCN_CLR_WINDOW ] )
   setCursor(2)
   dispend()

   RETURN NIL

//----------------------------------------------------------------------//

FUNCTION scrStatus(obj_,scn_)
   LOCAL s,objId,nSct,cClr:='GR+/BG'
   LOCAL typ_

   dispbegin()
   s := pad(scn_[SCN_DESIGN_ID],8)+' ³ '

   s += pad(scn_[SCN_FILE],12)+;
             ' ³ '+;
             ' Row:'+;
             str( scn_[SCN_ROW_REP]-1, 3 )+;
             ' Col:'+;
             str( scn_[SCN_COL_REP]-1, 3 )+;
             ' ³ ' +;
             IF(readInsert(),'Ins ','    ')+;
             ' ³ '

   objId := ''
   IF scn_[SCN_OBJ_HILITE] > 0
      objId := scn_[SCN_OBJ_ID_,obj_[scn_[SCN_OBJ_HILITE],OBJ_TYPE]]
      IF obj_[scn_[SCN_OBJ_HILITE],OBJ_TYPE] == OBJ_O_BOX
         typ_:= {'Bitmap','Frame','Ellipse','Line (H)','Line (V)','Grid','BarCode','Text Box'}
         objId := typ_[obj_[scn_[SCN_OBJ_HILITE],OBJ_MDL_F_TYPE]-60]
      ENDIF
   ELSEIF scn_[SCN_OBJ_SELECTED] > 0
      objId := scn_[SCN_OBJ_ID_,obj_[scn_[SCN_OBJ_SELECTED],OBJ_TYPE]]
      IF obj_[scn_[SCN_OBJ_SELECTED],OBJ_TYPE] == OBJ_O_BOX
         typ_:= {'Bitmap','Frame','Ellipse','Line (H)','Line (V)','Grid','BarCode','Text Box'}
         objId := typ_[obj_[scn_[SCN_OBJ_SELECTED],OBJ_MDL_F_TYPE]-60]
      ENDIF
   ENDIF

   s += pad(trim(objId),10)+' ³ '

   IF ( nSct := scrSecOrd(scn_,scn_[SCN_ROW_REP])) <> NIL
      s += scn_[ SCN_SECTORS_,nSct,SCT_ID ]
      #ifdef __WVT__
         cClr := scn_[ SCN_SECTORS_,nSct,SCT_COLOR ]
      #ENDIF
   ENDIF

   @ scn_[SCN_STATUS_ROW], scn_[SCN_STATUS_COL] ;
        say pad(s,scn_[SCN_STATUS_COL_TO]-scn_[SCN_STATUS_COL]+1) ;
        color scn_[SCN_CLR_STATUS]

   /* Ruler */
   s := substr(scn_[SCN_RULER],;
      max( 1,scn_[SCN_COL_REP]-scn_[SCN_COL_CUR]+scn_[SCN_LEFT]),;
         scn_[ SCN_RIGHT]-scn_[SCN_LEFT]+1 )
   DispBox( scn_[SCN_TOP]-1,0,scn_[SCN_TOP]-1,maxcol(),'         ',scn_[SCN_CLR_OVERALL] )
   @ scn_[SCN_ROW_RULER],scn_[SCN_LEFT] SAY s COLOR scn_[SCN_CLR_RULER]
   @ scn_[SCN_ROW_RULER],scn_[SCN_COL_CUR] say ;
      substr( s, scn_[SCN_COL_CUR]-scn_[SCN_LEFT]+1,1)  COLOR cClr

   @ scn_[SCN_ROW_CUR],scn_[SCN_COL_CUR] SAY ''

   //  Now update current cursor position
   scn_[SCN_ROW_PREV] := scn_[SCN_ROW_CUR]
   scn_[SCN_COL_PREV] := scn_[SCN_COL_CUR]

   dispend()
   RETURN NIL

//----------------------------------------------------------------------//

FUNCTION scrHiLite(obj_,scn_,mode)
   LOCAL nObj   := scn_[SCN_OBJ_HILITE]
   LOCAL cColor := IF(mode,scn_[SCN_CLR_HILITE],obj_[nObj,OBJ_COLOR])

   IF obj_[nObj,OBJ_TYPE] == OBJ_O_BOX
      DispBegin()
      dispBox( obj_[nObj,OBJ_ROW]    + scn_[SCN_ROW_DIS],;
               obj_[nObj,OBJ_COL]    + scn_[SCN_COL_DIS],;
               obj_[nObj,OBJ_TO_ROW] + scn_[SCN_ROW_DIS],;
               obj_[nObj,OBJ_TO_COL] + scn_[SCN_COL_DIS],;
               obj_[nObj,OBJ_BOX_SHAPE],;
               'gr+/b' /* cColor */)
      DispEnd()
   ELSEIF obj_[nObj,OBJ_TYPE] == OBJ_O_FIELD .OR. ;
          obj_[nObj,OBJ_TYPE] == OBJ_O_EXP
      DispBegin()
      @ obj_[nObj,OBJ_ROW]+scn_[SCN_ROW_DIS], ;
        obj_[nObj,OBJ_COL]+scn_[SCN_COL_DIS] ;
      say obj_[nObj,OBJ_TEXT] color cColor
      DispEnd()
   ENDIF
   RETURN NIL

//----------------------------------------------------------------------//

FUNCTION scrDispSelctd(obj_,scn_)
   LOCAL i,j,nCol,nRow

   IF !empty(scn_[SCN_TEXT_BLOCK_])
      DispBegin()

      FOR i := scn_[SCN_TEXT_BLOCK_,1] TO scn_[SCN_TEXT_BLOCK_,3]
         IF (nRow := i+scn_[SCN_ROW_DIS]) <= scn_[SCN_BOTTOM]
            FOR j := scn_[SCN_TEXT_BLOCK_,2] TO scn_[SCN_TEXT_BLOCK_,4]
               IF (nCol := j+scn_[SCN_COL_DIS]) <= scn_[SCN_RIGHT]
                  @ nRow,nCol SAY scrGetChar(obj_,i,j) COLOR 'GR+/R'
               ENDIF
            NEXT
         ENDIF
      NEXT

      DispEnd()
   ENDIF
   RETURN NIL

//----------------------------------------------------------------------//

FUNCTION scrDispGhost(obj_,scn_,gst_)
   LOCAL i,j,nRow,nCol

   HB_SYMBOL_UNUSED( obj_ )

   DispBegin()
   FOR i := gst_[1] TO gst_[3]
      IF (nRow := i+scn_[SCN_ROW_DIS]) <= scn_[SCN_BOTTOM]
         FOR j := gst_[2] TO gst_[4]
            IF (nCol := j+scn_[SCN_COL_DIS]) <= scn_[SCN_RIGHT]
               @ nRow,nCol SAY THE_FILL COLOR 'GR+/R'
            ENDIF
         NEXT
      ENDIF
   NEXT
   DispEnd()

   RETURN NIL

//----------------------------------------------------------------------//

FUNCTION scrMove(obj_,scn_)
   LOCAL i,scrT,scrB,scrL,scrR,k,n,nRepOff,nRowWnd,nTo
   LOCAL crs := setCursor( 0 )
   LOCAL nOff, cText, nRow, nCol, cColor

   dispBegin()

   IF scn_[SCN_DESIGN] == DGN_MODULE .OR. scn_[SCN_DESIGN]==DGN_SCREEN
      scrT := VouchWndSave(0,0,scn_[SCN_TOP]-1,maxcol())
      scrB := VouchWndSave(scn_[SCN_BOTTOM]+1,0,maxrow(),maxcol())
      scrL := VouchWndSave(0,0,maxrow(),scn_[SCN_LEFT]-1)
      scrR := VouchWndSave(0,scn_[SCN_RIGHT]+1,maxrow(),maxcol())
   ELSE
      scrT := VouchWndSave(0,0,scn_[SCN_TOP]-2,maxcol())
      scrB := VouchWndSave(maxrow()-2,0,maxrow(),maxcol())
      DispBox( scn_[SCN_TOP],0,maxrow()-2,maxcol(),'         ',scn_[SCN_CLR_OVERALL] )

      nRowWnd := scn_[SCN_BOTTOM ] - scn_[SCN_TOP] + 1
      nRepOff := scn_[SCN_ROW_REP] - (scn_[SCN_ROW_CUR] - scn_[SCN_TOP])
      nTo     := (nRowWnd + nRepOff - 1)

      k := 0
      FOR i := nRepOff TO nTo
         IF ( n := scrSecOrd( scn_,i ) ) <> NIL
            @ scn_[ SCN_TOP ]+k, 0 SAY scn_[ SCN_SECTORS_, n, SCT_SAY ] ;
            COLOR scn_[ SCN_SECTORS_,n,SCT_COLOR ]
            k++
         ENDIF
      NEXT

      scrL := VouchWndSave(0,0,maxrow(),scn_[SCN_LEFT]-1)
      scrR := VouchWndSave(0,scn_[SCN_RIGHT]+1,maxrow()-2,maxcol())
   ENDIF

   dispBox(scn_[ SCN_TOP       ],;
           scn_[ SCN_LEFT      ],;
           scn_[ SCN_BOTTOM    ],;
           scn_[ SCN_RIGHT     ],;
           scn_[ SCN_DRAW_FILL ],;
           scn_[ SCN_CLR_PREV  ] )

   FOR i := 1 TO len( obj_ )
      IF obj_[ i,OBJ_ROW ] + scn_[ SCN_ROW_DIS ] <= scn_[ SCN_BOTTOM ] .AND. ;
         obj_[ i,OBJ_COL ] + scn_[ SCN_COL_DIS ] <= scn_[ SCN_RIGHT  ]

         nOff := obj_[ i,OBJ_COL ] + scn_[ SCN_COL_DIS ]
         nRow := obj_[ i,OBJ_ROW ] + scn_[ SCN_ROW_DIS ]
         nCol := obj_[ i,OBJ_COL ] + scn_[ SCN_COL_DIS ]

         IF nOff < 0
            nCol := 0
         ENDIF

         IF obj_[i,OBJ_TYPE] == OBJ_O_BOX
            DO CASE
            CASE VouchInArray(obj_[ i,OBJ_MDL_F_TYPE ], { 61,62,63,67,68 } )
               dispBox( obj_[ i,OBJ_ROW    ] + scn_[ SCN_ROW_DIS ],;
                        obj_[ i,OBJ_COL    ] + scn_[ SCN_COL_DIS ],;
                        obj_[ i,OBJ_TO_ROW ] + scn_[ SCN_ROW_DIS ],;
                        obj_[ i,OBJ_TO_COL ] + scn_[ SCN_COL_DIS ],;
                        substr( obj_[ i,OBJ_BOX_SHAPE ], 1, 8 ),;
                        IF( scn_[ SCN_OBJ_HILITE ] == i, scn_[ SCN_CLR_HILITE ],;
                              'w/b' /* obj_[i,OBJ_COLOR] */))
            CASE VouchInArray(obj_[i,OBJ_MDL_F_TYPE], {64,65} )    //  Line
               @  obj_[i,OBJ_ROW]    + scn_[SCN_ROW_DIS],;
                  obj_[i,OBJ_COL]    + scn_[SCN_COL_DIS] ;
               TO obj_[i,OBJ_TO_ROW] + scn_[SCN_ROW_DIS],;
                  obj_[i,OBJ_TO_COL] + scn_[SCN_COL_DIS] ;
               COLOR IF(scn_[SCN_OBJ_HILITE]==i,scn_[SCN_CLR_HILITE],;
                              'w/b' /* obj_[i,OBJ_COLOR] */)
            ENDCASE
         ENDIF

         IF obj_[i,OBJ_TYPE] == OBJ_O_FIELD .OR. obj_[i,OBJ_TYPE] == OBJ_O_EXP
            cText  := obj_[ i,OBJ_TEXT ]
            cColor := IF( scn_[ SCN_OBJ_SELECTED ] == i,scn_[ SCN_CLR_SELECT ],;
                      IF( scn_[ SCN_OBJ_HILITE   ] == i,scn_[ SCN_CLR_HILITE ],;
                                                 'W+/W' /* obj_[i,OBJ_COLOR] */ ))
            IF nOff < 0
               cText := substr( obj_[ i,OBJ_TEXT ], abs( nOff ) + 1 )
            ENDIF
            @ nRow, nCol SAY cText COLOR cColor
         ENDIF

         IF obj_[i,OBJ_TYPE] == OBJ_O_TEXT
            cText  := obj_[ i,OBJ_EQN ]
            cColor := IF( scn_[SCN_OBJ_SELECTED]==i,scn_[SCN_CLR_SELECT],;
                      IF( empty(obj_[i,OBJ_COLOR]),scn_[SCN_CLR_TEXT],;
                                                'W/B' /* obj_[i,OBJ_COLOR] */))
            IF nOff < 0
               cText := substr( obj_[ i,OBJ_EQN ], abs( nOff ) + 1 )
            ENDIF

            @ nRow, nCol SAY cText COLOR cColor
         ENDIF
      ELSEIF ( obj_[ i,OBJ_ROW ] + scn_[ SCN_ROW_DIS ] > scn_[ SCN_BOTTOM ] )

      ENDIF
   NEXT

   ScrDispSelctd( obj_,scn_ )     //  Display Selected Screen

   IF scn_[ SCN_TOP    ] > 0
      VouchWndRest(scrT)
   ENDIF
   IF scn_[ SCN_LEFT   ] > 0
      VouchWndRest(scrL)
   ENDIF
   IF scn_[ SCN_BOTTOM ] < maxrow()
      VouchWndRest(scrB)
   ENDIF
   IF scn_[ SCN_RIGHT  ] < maxcol()
      VouchWndRest(scrR)
   ENDIF

   dispEnd()
   setcursor( crs )
   RETURN NIL

//----------------------------------------------------------------------//

FUNCTION scrMoveLine(obj_,scn_)
   LOCAL scrL,scrR,i,crs, nRow, nCol, cText, nOff, cColor

   crs := setCursor( 0 )

   IF scn_[ SCN_ROW_PREV ] == scn_[ SCN_ROW_CUR ]
      dispbegin()
      dispBox(scn_[SCN_ROW_CUR  ],;
              scn_[SCN_LEFT     ],;
              scn_[SCN_ROW_CUR  ],;
              scn_[SCN_RIGHT    ],;
              scn_[SCN_DRAW_FILL],;
              scn_[SCN_CLR_PREV ] )

      scrL := VouchWndSave(scn_[SCN_ROW_CUR],0,scn_[SCN_ROW_CUR],scn_[SCN_LEFT]-1)
      IF scn_[SCN_RIGHT] < maxcol()
         scrR := VouchWndSave( scn_[SCN_ROW_CUR],scn_[SCN_RIGHT]+1,scn_[SCN_ROW_CUR],maxcol() )
      ENDIF

      FOR i := 1 TO len(obj_)
         nOff := obj_[i,OBJ_COL]+scn_[SCN_COL_DIS]
         nRow := obj_[i,OBJ_ROW]+scn_[SCN_ROW_DIS]
         nCol := nOff
         #ifdef __WVT__
            nCol := IF( nOff < 0, 0, nOff )
         #ENDIF

         IF obj_[i,OBJ_TYPE] == OBJ_O_BOX
            DO CASE
            CASE VouchInArray(obj_[i,OBJ_MDL_F_TYPE],{64,65})    //  Lines V.H
               @  obj_[i,OBJ_ROW]    + scn_[SCN_ROW_DIS],;
                  obj_[i,OBJ_COL]    + scn_[SCN_COL_DIS] ;
               TO obj_[i,OBJ_TO_ROW] + scn_[SCN_ROW_DIS],;
                  obj_[i,OBJ_TO_COL] + scn_[SCN_COL_DIS] ;
               COLOR IF(scn_[SCN_OBJ_HILITE]==i,scn_[SCN_CLR_HILITE],;
                              'w/b' /* obj_[i,OBJ_COLOR] */)
            ENDCASE
         ENDIF

         IF obj_[ i,OBJ_ROW ] == scn_[ SCN_ROW_REP ]
            IF obj_[i,OBJ_TYPE] == OBJ_O_FIELD .OR. obj_[i,OBJ_TYPE] == OBJ_O_EXP
               cText := obj_[i,OBJ_TEXT]
               cColor := IF(scn_[SCN_OBJ_SELECTED]==i,scn_[SCN_CLR_SELECT],;
                         IF(scn_[SCN_OBJ_HILITE  ]==i,scn_[SCN_CLR_HILITE],;
                                               'W+/W' /* obj_[i,OBJ_COLOR] */ ))
               #ifdef __WVT__
                  IF nOff < 0
                     cText := substr( cText, abs( nOff )+1 )
                  ENDIF
               #ENDIF
               @ nRow, nCol SAY cText COLOR cColor
            ENDIF

            IF obj_[i,OBJ_TYPE] == OBJ_O_TEXT
               cText  := obj_[i,OBJ_EQN]
               cColor := IF(scn_[SCN_OBJ_SELECTED]==i,scn_[SCN_CLR_SELECT],;
                         IF(empty(obj_[i,OBJ_COLOR]),scn_[SCN_CLR_TEXT],;
                                      'W/B' /* obj_[i,OBJ_COLOR] */))
               #ifdef __WVT__
                  cText := IF( nOff < 0, substr( cText, abs( nOff )+1 ), cText )
               #ENDIF
               @ nRow, nCol SAY cText COLOR cColor
            ENDIF
         ENDIF
      NEXT

      scrDispSelctd(obj_,scn_)     //  Display Selected Screen

      VouchWndRest(scrL)
      IF scn_[SCN_RIGHT] < maxcol()
         VouchWndRest(scrR)
      ENDIF

      dispEnd()
   ELSE
      scrMove(obj_,scn_)
   ENDIF
   setCursor(crs)
   RETURN NIL

//----------------------------------------------------------------------//

FUNCTION scrMouse( obj_, scn_, nEvent )
   LOCAL nmRow, nmCol
   LOCAL aEvents_:= { K_LBUTTONUP, K_LBUTTONDOWN, K_MMLEFTDOWN }

   STATIC nLastCol  := 0
   STATIC nLastRow  := 0
   STATIC lAnchored := .f.
   STATIC nCursor

   IF !( VouchInArray( nEvent, aEvents_ ) )
      RETURN .f.
   ENDIF

   nmRow := mRow()
   nmCol := mCol()

   IF nmRow < scn_[ SCN_TOP ] .OR. nmRow > scn_[ SCN_BOTTOM ] .OR. ;
                   nmCol < scn_[ SCN_LEFT ] .OR. nmCol > scn_[ SCN_RIGHT ]
      RETURN .f.
   ENDIF

   scrToMouse( scn_, nmRow, nmCol )

   IF nEvent == K_LDBLCLK

   ELSEIF nEvent == K_MMLEFTDOWN /*K_LBUTTONDOWN */ .AND. !( lAnchored )
      IF scrChkObj( obj_,scn_ ) > 0 .AND. scn_[ SCN_MODE ] <> OBJ_MODE_SELECT
         nCursor := SetCursor( 0 )

         lAnchored := .t.
         scn_[ SCN_LASTKEY ] := K_F6
//         Wvt_SetMousePos( scn_[ SCN_ROW_CUR ], scn_[ SCN_COL_CUR ] )
      ENDIF

   ELSEIF nEvent == K_MMLEFTDOWN .AND. lAnchored

   ELSEIF nEvent == K_LBUTTONUP  .AND. lAnchored
//      Wvt_SetMousePos( scn_[ SCN_ROW_CUR ], scn_[ SCN_COL_CUR ] )
      SetCursor( nCursor )
      lAnchored := .f.
      __keyboard( chr( K_ENTER ) )

   ELSEIF nEvent == K_LBUTTONUP

   ENDIF

   RETURN .t.

//----------------------------------------------------------------------//

FUNCTION scrToMouse( scn_, nmRow, nmCol )
   LOCAL nRowOff, nColOff

   nRowOff :=    nmRow - scn_[SCN_ROW_CUR]
   IF nRowOff <> 0
      scn_[ SCN_ROW_CUR ] += nRowOff
      scn_[ SCN_ROW_REP ] += nRowOff
   ENDIF

   nColOff := nmCol - scn_[ SCN_COL_CUR ]
   IF nColOff <> 0
      scn_[ SCN_COL_CUR ] += nColOff
      scn_[ SCN_COL_REP ] += nColOff
   ENDIF

   RETURN NIL

//----------------------------------------------------------------------//

