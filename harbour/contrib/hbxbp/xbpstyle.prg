/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Xbp*Classes
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
 *                            Class XbpStyle()
 *
 *                             Pritpal Bedi
 *                               21Jun2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"

#include "xbp.ch"
#include "appevent.ch"

/*----------------------------------------------------------------------*/

CLASS XbpStyle

   DATA   style                                   INIT   ""

   DATA   qtWidget
   DATA   xbpPart

   DATA   colorFG
   DATA   colorBG

   METHOD init()                                  INLINE Self
   METHOD Create()
   METHOD Configure()                             VIRTUAL
   METHOD Destroy()                               VIRTUAL

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpStyle:create()
   LOCAL s := ''

   #if 0
   IF empty( ::xbpPart )
      RETURN nil
   ENDIF
   #endif

   s += ::qtWidget + '{ '

   IF !empty( ::colorFG )
      s += 'color: '
      IF hb_isNumeric( ::colorFG )
         s += hb_ntos( ::colorFG )
      ELSE
         s += ::colorFG
      ENDIF
      s += '; '
   ENDIF

   IF !empty( ::colorBG )
      s += 'background-color: '
      IF hb_isNumeric( ::colorBG )
         s += hb_ntos( ::colorBG )
      ELSE
         s += ::colorBG
      ENDIF
      s += '; '
   ENDIF

   s += ' }'

   ::style := s

   RETURN Self

/*----------------------------------------------------------------------*/

FUNCTION Xbp_XbpToQTName( cXbpPart )
   LOCAL aQt := {}
   LOCAL n, cQTWidget

   cXbpPart := upper( cXbpPart )

   aadd( aQt, { 'XBPDIALOG'      , 'QMainWindow' } )
   aadd( aQt, { 'XBPDRAWINGAREA' , 'QWidget'     } )
   aadd( aQt, { 'XBPPUSHBUTTON'  , 'QPushButton' } )
   aadd( aQt, { 'XBPCHECKBOX'    , 'QCheckBox'   } )
   aadd( aQt, { 'XBPTREEVIEW'    , 'QTreeWidget' } )
   aadd( aQt, { 'XBPTREEVIEWITEM', ' ' } )
   aadd( aQt, { 'XBPRADIOBUTTON' , 'QRadioButton'} )
   aadd( aQt, { 'XBPSCROLLBAR'   , 'QScrollBar'  } )
   aadd( aQt, { 'XBPSTATUSBAR'   , 'QStatusBar'  } )
   aadd( aQt, { 'XBPTABPAGE'     , 'QTabWidget'  } )
   aadd( aQt, { 'XBPSTATIC'      , ' ' } )

   IF ( n := ascan( aQt, {|e_| e_[ 1 ] == cXbpPart } ) ) > 0
      cQTWidget := aQt[ n,2 ]
   ENDIF

   RETURN cQTWidget

/*----------------------------------------------------------------------*/
