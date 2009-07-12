/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://www.harbour-project.org
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


CREATE CLASS QPalette

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )
   METHOD  Destroy()                           INLINE  Qt_QPalette_destroy( ::pPtr )

   METHOD  alternateBase()                     INLINE  Qt_QPalette_alternateBase( ::pPtr )
   METHOD  base()                              INLINE  Qt_QPalette_base( ::pPtr )
   METHOD  brightText()                        INLINE  Qt_QPalette_brightText( ::pPtr )
   METHOD  brush( nGroup, nRole )              INLINE  Qt_QPalette_brush( ::pPtr, nGroup, nRole )
   METHOD  brush_1( nRole )                    INLINE  Qt_QPalette_brush_1( ::pPtr, nRole )
   METHOD  button()                            INLINE  Qt_QPalette_button( ::pPtr )
   METHOD  buttonText()                        INLINE  Qt_QPalette_buttonText( ::pPtr )
   METHOD  cacheKey()                          INLINE  Qt_QPalette_cacheKey( ::pPtr )
   METHOD  color( nGroup, nRole )              INLINE  Qt_QPalette_color( ::pPtr, nGroup, nRole )
   METHOD  color_1( nRole )                    INLINE  Qt_QPalette_color_1( ::pPtr, nRole )
   METHOD  currentColorGroup()                 INLINE  Qt_QPalette_currentColorGroup( ::pPtr )
   METHOD  dark()                              INLINE  Qt_QPalette_dark( ::pPtr )
   METHOD  highlight()                         INLINE  Qt_QPalette_highlight( ::pPtr )
   METHOD  highlightedText()                   INLINE  Qt_QPalette_highlightedText( ::pPtr )
   METHOD  isBrushSet( nCg, nCr )              INLINE  Qt_QPalette_isBrushSet( ::pPtr, nCg, nCr )
   METHOD  isCopyOf( pP )                      INLINE  Qt_QPalette_isCopyOf( ::pPtr, pP )
   METHOD  isEqual( nCg1, nCg2 )               INLINE  Qt_QPalette_isEqual( ::pPtr, nCg1, nCg2 )
   METHOD  light()                             INLINE  Qt_QPalette_light( ::pPtr )
   METHOD  link()                              INLINE  Qt_QPalette_link( ::pPtr )
   METHOD  linkVisited()                       INLINE  Qt_QPalette_linkVisited( ::pPtr )
   METHOD  mid()                               INLINE  Qt_QPalette_mid( ::pPtr )
   METHOD  midlight()                          INLINE  Qt_QPalette_midlight( ::pPtr )
   METHOD  resolve( pOther )                   INLINE  Qt_QPalette_resolve( ::pPtr, pOther )
   METHOD  setBrush( nRole, pBrush )           INLINE  Qt_QPalette_setBrush( ::pPtr, nRole, pBrush )
   METHOD  setBrush_1( nGroup, nRole, pBrush )  INLINE  Qt_QPalette_setBrush_1( ::pPtr, nGroup, nRole, pBrush )
   METHOD  setColor( nRole, pColor )           INLINE  Qt_QPalette_setColor( ::pPtr, nRole, pColor )
   METHOD  setColor_1( nGroup, nRole, pColor )  INLINE  Qt_QPalette_setColor_1( ::pPtr, nGroup, nRole, pColor )
   METHOD  setColorGroup( nCg, pWindowText, pButton, pLight, pDark, pMid, pText, pBright_text, pBase, pWindow )  INLINE  Qt_QPalette_setColorGroup( ::pPtr, nCg, pWindowText, pButton, pLight, pDark, pMid, pText, pBright_text, pBase, pWindow )
   METHOD  setCurrentColorGroup( nCg )         INLINE  Qt_QPalette_setCurrentColorGroup( ::pPtr, nCg )
   METHOD  shadow()                            INLINE  Qt_QPalette_shadow( ::pPtr )
   METHOD  text()                              INLINE  Qt_QPalette_text( ::pPtr )
   METHOD  toolTipBase()                       INLINE  Qt_QPalette_toolTipBase( ::pPtr )
   METHOD  toolTipText()                       INLINE  Qt_QPalette_toolTipText( ::pPtr )
   METHOD  window()                            INLINE  Qt_QPalette_window( ::pPtr )
   METHOD  windowText()                        INLINE  Qt_QPalette_windowText( ::pPtr )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( ... ) CLASS QPalette

   ::pPtr := Qt_QPalette( ... )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QPalette

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

