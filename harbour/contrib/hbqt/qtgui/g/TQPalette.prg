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

   METHOD  alternateBase()
   METHOD  base()
   METHOD  brightText()
   METHOD  brush( ... )
   METHOD  button()
   METHOD  buttonText()
   METHOD  cacheKey()
   METHOD  color( ... )
   METHOD  currentColorGroup()
   METHOD  dark()
   METHOD  highlight()
   METHOD  highlightedText()
   METHOD  isBrushSet( nCg, nCr )
   METHOD  isCopyOf( pP )
   METHOD  isEqual( nCg1, nCg2 )
   METHOD  light()
   METHOD  link()
   METHOD  linkVisited()
   METHOD  mid()
   METHOD  midlight()
   METHOD  resolve( pOther )
   METHOD  setBrush( ... )
   METHOD  setColor( ... )
   METHOD  setColorGroup( nCg, pWindowText, pButton, pLight, pDark, pMid, pText, pBright_text, pBase, pWindow )
   METHOD  setCurrentColorGroup( nCg )
   METHOD  shadow()
   METHOD  text()
   METHOD  toolTipBase()
   METHOD  toolTipText()
   METHOD  window()
   METHOD  windowText()

   ENDCLASS


METHOD QPalette:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPalette( ... )
   RETURN Self


METHOD QPalette:alternateBase()
   RETURN HB_QBrush():from( Qt_QPalette_alternateBase( ::pPtr ) )


METHOD QPalette:base()
   RETURN HB_QBrush():from( Qt_QPalette_base( ::pPtr ) )


METHOD QPalette:brightText()
   RETURN HB_QBrush():from( Qt_QPalette_brightText( ::pPtr ) )


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


METHOD QPalette:button()
   RETURN HB_QBrush():from( Qt_QPalette_button( ::pPtr ) )


METHOD QPalette:buttonText()
   RETURN HB_QBrush():from( Qt_QPalette_buttonText( ::pPtr ) )


METHOD QPalette:cacheKey()
   RETURN Qt_QPalette_cacheKey( ::pPtr )


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


METHOD QPalette:currentColorGroup()
   RETURN Qt_QPalette_currentColorGroup( ::pPtr )


METHOD QPalette:dark()
   RETURN HB_QBrush():from( Qt_QPalette_dark( ::pPtr ) )


METHOD QPalette:highlight()
   RETURN HB_QBrush():from( Qt_QPalette_highlight( ::pPtr ) )


METHOD QPalette:highlightedText()
   RETURN HB_QBrush():from( Qt_QPalette_highlightedText( ::pPtr ) )


METHOD QPalette:isBrushSet( nCg, nCr )
   RETURN Qt_QPalette_isBrushSet( ::pPtr, nCg, nCr )


METHOD QPalette:isCopyOf( pP )
   RETURN Qt_QPalette_isCopyOf( ::pPtr, hbqt_ptr( pP ) )


METHOD QPalette:isEqual( nCg1, nCg2 )
   RETURN Qt_QPalette_isEqual( ::pPtr, nCg1, nCg2 )


METHOD QPalette:light()
   RETURN HB_QBrush():from( Qt_QPalette_light( ::pPtr ) )


METHOD QPalette:link()
   RETURN HB_QBrush():from( Qt_QPalette_link( ::pPtr ) )


METHOD QPalette:linkVisited()
   RETURN HB_QBrush():from( Qt_QPalette_linkVisited( ::pPtr ) )


METHOD QPalette:mid()
   RETURN HB_QBrush():from( Qt_QPalette_mid( ::pPtr ) )


METHOD QPalette:midlight()
   RETURN HB_QBrush():from( Qt_QPalette_midlight( ::pPtr ) )


METHOD QPalette:resolve( pOther )
   RETURN HB_QPalette():from( Qt_QPalette_resolve( ::pPtr, hbqt_ptr( pOther ) ) )


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


METHOD QPalette:setColorGroup( nCg, pWindowText, pButton, pLight, pDark, pMid, pText, pBright_text, pBase, pWindow )
   RETURN Qt_QPalette_setColorGroup( ::pPtr, nCg, hbqt_ptr( pWindowText ), hbqt_ptr( pButton ), hbqt_ptr( pLight ), hbqt_ptr( pDark ), hbqt_ptr( pMid ), hbqt_ptr( pText ), hbqt_ptr( pBright_text ), hbqt_ptr( pBase ), hbqt_ptr( pWindow ) )


METHOD QPalette:setCurrentColorGroup( nCg )
   RETURN Qt_QPalette_setCurrentColorGroup( ::pPtr, nCg )


METHOD QPalette:shadow()
   RETURN HB_QBrush():from( Qt_QPalette_shadow( ::pPtr ) )


METHOD QPalette:text()
   RETURN HB_QBrush():from( Qt_QPalette_text( ::pPtr ) )


METHOD QPalette:toolTipBase()
   RETURN HB_QBrush():from( Qt_QPalette_toolTipBase( ::pPtr ) )


METHOD QPalette:toolTipText()
   RETURN HB_QBrush():from( Qt_QPalette_toolTipText( ::pPtr ) )


METHOD QPalette:window()
   RETURN HB_QBrush():from( Qt_QPalette_window( ::pPtr ) )


METHOD QPalette:windowText()
   RETURN HB_QBrush():from( Qt_QPalette_windowText( ::pPtr ) )

