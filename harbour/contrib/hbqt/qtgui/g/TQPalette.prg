/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/


#include "hbclass.ch"


FUNCTION QPalette( ... )
   RETURN HB_QPalette():new( ... )


CREATE CLASS QPalette INHERIT HbQtObjectHandler FUNCTION HB_QPalette

   METHOD  new( ... )

   METHOD  alternateBase                 // (  )                                               -> oQBrush
   METHOD  base                          // (  )                                               -> oQBrush
   METHOD  brightText                    // (  )                                               -> oQBrush
   METHOD  brush                         // ( nGroup, nRole )                                  -> oQBrush
                                         // ( nRole )                                          -> oQBrush
   METHOD  button                        // (  )                                               -> oQBrush
   METHOD  buttonText                    // (  )                                               -> oQBrush
   METHOD  cacheKey                      // (  )                                               -> nQint64
   METHOD  color                         // ( nGroup, nRole )                                  -> oQColor
                                         // ( nRole )                                          -> oQColor
   METHOD  currentColorGroup             // (  )                                               -> nColorGroup
   METHOD  dark                          // (  )                                               -> oQBrush
   METHOD  highlight                     // (  )                                               -> oQBrush
   METHOD  highlightedText               // (  )                                               -> oQBrush
   METHOD  isBrushSet                    // ( nCg, nCr )                                       -> lBool
   METHOD  isCopyOf                      // ( oQPalette )                                      -> lBool
   METHOD  isEqual                       // ( nCg1, nCg2 )                                     -> lBool
   METHOD  light                         // (  )                                               -> oQBrush
   METHOD  link                          // (  )                                               -> oQBrush
   METHOD  linkVisited                   // (  )                                               -> oQBrush
   METHOD  mid                           // (  )                                               -> oQBrush
   METHOD  midlight                      // (  )                                               -> oQBrush
   METHOD  resolve                       // ( oQPalette )                                      -> oQPalette
   METHOD  setBrush                      // ( nRole, oQBrush )                                 -> NIL
                                         // ( nGroup, nRole, oQBrush )                         -> NIL
   METHOD  setColor                      // ( nRole, oQColor )                                 -> NIL
                                         // ( nGroup, nRole, oQColor )                         -> NIL
   METHOD  setColorGroup                 // ( nCg, oQBrush, oQBrush, oQBrush, oQBrush, oQBrush, oQBrush, oQBrush, oQBrush, oQBrush ) -> NIL
   METHOD  setCurrentColorGroup          // ( nCg )                                            -> NIL
   METHOD  shadow                        // (  )                                               -> oQBrush
   METHOD  text                          // (  )                                               -> oQBrush
   METHOD  toolTipBase                   // (  )                                               -> oQBrush
   METHOD  toolTipText                   // (  )                                               -> oQBrush
   METHOD  window                        // (  )                                               -> oQBrush
   METHOD  windowText                    // (  )                                               -> oQBrush

   ENDCLASS


METHOD QPalette:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPalette( ... )
   RETURN Self


METHOD QPalette:alternateBase( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QBrush():from( Qt_QPalette_alternateBase( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPalette:base( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QBrush():from( Qt_QPalette_base( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPalette:brightText( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QBrush():from( Qt_QPalette_brightText( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPalette:brush( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN HB_QBrush():from( Qt_QPalette_brush( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN HB_QBrush():from( Qt_QPalette_brush_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPalette:button( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QBrush():from( Qt_QPalette_button( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPalette:buttonText( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QBrush():from( Qt_QPalette_buttonText( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPalette:cacheKey( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPalette_cacheKey( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPalette:color( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN HB_QColor():from( Qt_QPalette_color( ::pPtr, ... ) )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN HB_QColor():from( Qt_QPalette_color_1( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPalette:currentColorGroup( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPalette_currentColorGroup( ::pPtr, ... )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPalette:dark( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QBrush():from( Qt_QPalette_dark( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPalette:highlight( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QBrush():from( Qt_QPalette_highlight( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPalette:highlightedText( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QBrush():from( Qt_QPalette_highlightedText( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPalette:isBrushSet( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QPalette_isBrushSet( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPalette:isCopyOf( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QPalette_isCopyOf( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPalette:isEqual( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QPalette_isEqual( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPalette:light( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QBrush():from( Qt_QPalette_light( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPalette:link( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QBrush():from( Qt_QPalette_link( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPalette:linkVisited( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QBrush():from( Qt_QPalette_linkVisited( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPalette:mid( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QBrush():from( Qt_QPalette_mid( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPalette:midlight( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QBrush():from( Qt_QPalette_midlight( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPalette:resolve( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN HB_QPalette():from( Qt_QPalette_resolve( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPalette:setBrush( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QPalette_setBrush_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QPalette_setBrush( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPalette:setColor( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) )
         RETURN Qt_QPalette_setColor_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QPalette_setColor( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPalette:setColorGroup( ... )
   SWITCH PCount()
   CASE 10
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) ) .AND. hb_isObject( hb_pvalue( 3 ) ) .AND. hb_isObject( hb_pvalue( 4 ) ) .AND. hb_isObject( hb_pvalue( 5 ) ) .AND. hb_isObject( hb_pvalue( 6 ) ) .AND. hb_isObject( hb_pvalue( 7 ) ) .AND. hb_isObject( hb_pvalue( 8 ) ) .AND. hb_isObject( hb_pvalue( 9 ) ) .AND. hb_isObject( hb_pvalue( 10 ) )
         RETURN Qt_QPalette_setColorGroup( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPalette:setCurrentColorGroup( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QPalette_setCurrentColorGroup( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPalette:shadow( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QBrush():from( Qt_QPalette_shadow( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPalette:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QBrush():from( Qt_QPalette_text( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPalette:toolTipBase( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QBrush():from( Qt_QPalette_toolTipBase( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPalette:toolTipText( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QBrush():from( Qt_QPalette_toolTipText( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPalette:window( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QBrush():from( Qt_QPalette_window( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()


METHOD QPalette:windowText( ... )
   SWITCH PCount()
   CASE 0
      RETURN HB_QBrush():from( Qt_QPalette_windowText( ::pPtr, ... ) )
   ENDSWITCH
   RETURN hbqt_error()

