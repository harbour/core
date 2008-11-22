/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Wvg*Classes
 *
 * Copyright 2008 Pritpal Bedi <pritpal@vouchcac.com>
 * http://www.harbour-project.org
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
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//
//                               EkOnkar
//                         ( The LORD is ONE )
//
//                  Xbase++ Compatible xbpDialog Class
//
//                 Pritpal Bedi <pritpal@vouchcac.com>
//                             17Nov2008
//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//
//----------------------------------------------------------------------//

#include 'hbclass.ch'
#include 'common.ch'
#include 'hbgtinfo.ch'
#include 'hbgtwvg.ch'
#include 'wvtwin.ch'
#include 'inkey.ch'

//----------------------------------------------------------------------//
CLASS WvgDialog FROM WvgWindow

   METHOD init()
   METHOD create()
   METHOD configure()
   METHOD destroy()

   ENDCLASS
//----------------------------------------------------------------------//
METHOD init( oParent, oOwner, aPos, aSize, aPresParams, lVisible ) CLASS WvgDialog

   ::WvgWindow:init( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   ::className  := 'WVGDIALOG'
   ::resizeMode := 0
   ::mouseMode  := 0
   ::objType    := objTypeDialog

   ::style      := WS_THICKFRAME+WS_OVERLAPPED+WS_CAPTION+WS_SYSMENU+WS_MINIMIZEBOX+WS_MAXIMIZEBOX;

   RETURN Self
//----------------------------------------------------------------------//
METHOD create( oParent, oOwner, aPos, aSize, aPresParams, lVisible ) CLASS WvgDialog

   ::WvgWindow:create( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   if ::lModal
      ::pGT  := hb_gtCreate( 'WGU' )
      ::pGTp := hb_gtSelect( ::pGT )
   else
      hb_gtReload( 'WGU' )
      ::pGT := hb_gtSelect()
   endif

   hb_gtInfo( HB_GTI_PRESPARAMS, { ::exStyle, ::style, ::aPos[ 1 ], ::aPos[ 2 ], ;
                                   ::aSize[ 1 ], ::aSize[ 2 ], ::pGTp, .F., .F. } )

   if ::visible
      hb_gtInfo( HB_GTI_SPEC, HB_GTS_SHOWWINDOW, SW_NORMAL )
   else
      hb_gtInfo( HB_GTI_SPEC, HB_GTS_SHOWWINDOW, SW_HIDE   )
   endif

   ::hWnd := hb_gtInfo( HB_GTI_SPEC, HB_GTS_WINDOWHANDLE )

   hb_gtInfo( HB_GTI_RESIZABLE , ::resizable )
   hb_gtInfo( HB_GTI_CLOSABLE  , ::closable  )
   hb_gtInfo( HB_GTI_WINTITLE  , ::title     )

   if !empty( ::icon )
      if hb_isNumeric( ::icon )
         hb_gtInfo( HB_GTI_ICONRES, ::icon )

      elseif hb_isChar( ::icon )
         hb_gtInfo( HB_GTI_ICONFILE, ::icon )

      endif
   endif

   if ::lModal
      hb_gtInfo( HB_GTI_DISABLE, ::pGTp )
   endif

   if ::visible
      ::lHasInputFocus := .t.
   endif

   hb_gtInfo( HB_GTI_NOTIFIERBLOCK, {|nEvent, ...| ::notifier( nEvent, ... ) } )

   RETURN Self
//----------------------------------------------------------------------//
METHOD configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible ) CLASS WvgDialog

   ::WvgWindow:configure( oParent, oOwner, aPos, aSize, aPresParams, lVisible )

   RETURN Self
//----------------------------------------------------------------------//
METHOD destroy() CLASS WvgDialog

   ::pGT  := NIL
   ::pGTp := NIL

   RETURN Self
//----------------------------------------------------------------------//
