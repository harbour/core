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
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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


#include "hbclass.ch"


FUNCTION QPalette( ... )
   RETURN HB_QPalette():new( ... )


CREATE CLASS QPalette INHERIT HbQtObjectHandler FUNCTION HB_QPalette

   METHOD  new( ... )

   METHOD  alternateBase()
   METHOD  base()
   METHOD  brightText()
   METHOD  brush( nGroup, nRole )
   METHOD  brush_1( nRole )
   METHOD  button()
   METHOD  buttonText()
   METHOD  cacheKey()
   METHOD  color( nGroup, nRole )
   METHOD  color_1( nRole )
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
   METHOD  setBrush( nRole, pBrush )
   METHOD  setBrush_1( nGroup, nRole, pBrush )
   METHOD  setColor( nRole, pColor )
   METHOD  setColor_1( nGroup, nRole, pColor )
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
   RETURN Qt_QPalette_alternateBase( ::pPtr )


METHOD QPalette:base()
   RETURN Qt_QPalette_base( ::pPtr )


METHOD QPalette:brightText()
   RETURN Qt_QPalette_brightText( ::pPtr )


METHOD QPalette:brush( nGroup, nRole )
   RETURN Qt_QPalette_brush( ::pPtr, nGroup, nRole )


METHOD QPalette:brush_1( nRole )
   RETURN Qt_QPalette_brush_1( ::pPtr, nRole )


METHOD QPalette:button()
   RETURN Qt_QPalette_button( ::pPtr )


METHOD QPalette:buttonText()
   RETURN Qt_QPalette_buttonText( ::pPtr )


METHOD QPalette:cacheKey()
   RETURN Qt_QPalette_cacheKey( ::pPtr )


METHOD QPalette:color( nGroup, nRole )
   RETURN Qt_QPalette_color( ::pPtr, nGroup, nRole )


METHOD QPalette:color_1( nRole )
   RETURN Qt_QPalette_color_1( ::pPtr, nRole )


METHOD QPalette:currentColorGroup()
   RETURN Qt_QPalette_currentColorGroup( ::pPtr )


METHOD QPalette:dark()
   RETURN Qt_QPalette_dark( ::pPtr )


METHOD QPalette:highlight()
   RETURN Qt_QPalette_highlight( ::pPtr )


METHOD QPalette:highlightedText()
   RETURN Qt_QPalette_highlightedText( ::pPtr )


METHOD QPalette:isBrushSet( nCg, nCr )
   RETURN Qt_QPalette_isBrushSet( ::pPtr, nCg, nCr )


METHOD QPalette:isCopyOf( pP )
   RETURN Qt_QPalette_isCopyOf( ::pPtr, hbqt_ptr( pP ) )


METHOD QPalette:isEqual( nCg1, nCg2 )
   RETURN Qt_QPalette_isEqual( ::pPtr, nCg1, nCg2 )


METHOD QPalette:light()
   RETURN Qt_QPalette_light( ::pPtr )


METHOD QPalette:link()
   RETURN Qt_QPalette_link( ::pPtr )


METHOD QPalette:linkVisited()
   RETURN Qt_QPalette_linkVisited( ::pPtr )


METHOD QPalette:mid()
   RETURN Qt_QPalette_mid( ::pPtr )


METHOD QPalette:midlight()
   RETURN Qt_QPalette_midlight( ::pPtr )


METHOD QPalette:resolve( pOther )
   RETURN Qt_QPalette_resolve( ::pPtr, hbqt_ptr( pOther ) )


METHOD QPalette:setBrush( nRole, pBrush )
   RETURN Qt_QPalette_setBrush( ::pPtr, nRole, hbqt_ptr( pBrush ) )


METHOD QPalette:setBrush_1( nGroup, nRole, pBrush )
   RETURN Qt_QPalette_setBrush_1( ::pPtr, nGroup, nRole, hbqt_ptr( pBrush ) )


METHOD QPalette:setColor( nRole, pColor )
   RETURN Qt_QPalette_setColor( ::pPtr, nRole, hbqt_ptr( pColor ) )


METHOD QPalette:setColor_1( nGroup, nRole, pColor )
   RETURN Qt_QPalette_setColor_1( ::pPtr, nGroup, nRole, hbqt_ptr( pColor ) )


METHOD QPalette:setColorGroup( nCg, pWindowText, pButton, pLight, pDark, pMid, pText, pBright_text, pBase, pWindow )
   RETURN Qt_QPalette_setColorGroup( ::pPtr, nCg, hbqt_ptr( pWindowText ), hbqt_ptr( pButton ), hbqt_ptr( pLight ), hbqt_ptr( pDark ), hbqt_ptr( pMid ), hbqt_ptr( pText ), hbqt_ptr( pBright_text ), hbqt_ptr( pBase ), hbqt_ptr( pWindow ) )


METHOD QPalette:setCurrentColorGroup( nCg )
   RETURN Qt_QPalette_setCurrentColorGroup( ::pPtr, nCg )


METHOD QPalette:shadow()
   RETURN Qt_QPalette_shadow( ::pPtr )


METHOD QPalette:text()
   RETURN Qt_QPalette_text( ::pPtr )


METHOD QPalette:toolTipBase()
   RETURN Qt_QPalette_toolTipBase( ::pPtr )


METHOD QPalette:toolTipText()
   RETURN Qt_QPalette_toolTipText( ::pPtr )


METHOD QPalette:window()
   RETURN Qt_QPalette_window( ::pPtr )


METHOD QPalette:windowText()
   RETURN Qt_QPalette_windowText( ::pPtr )

