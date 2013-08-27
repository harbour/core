/*
 * Harbour Project source code:
 * TBrowse Class
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * This implementation contains code and notes by:
 * Copyright 2008 Viktor Szakats (harbour syenar.net)
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#define HB_CLS_NOTOBJECT

#include "hbclass.ch"

#define _TBR_COORD( n )       Int( n )

CREATE CLASS xpp_TBrowse INHERIT TBrowse

   EXPORTED:

   METHOD viewArea()
   METHOD firstScrCol()

   METHOD _left()
   METHOD _right()
   METHOD _end()

ENDCLASS

METHOD viewArea() CLASS xpp_TBrowse

   LOCAL nWidth, nFrozenWidth

   IF ::nConfigure != 0
      ::doConfigure()
   ENDIF

   // TOFIX

   nWidth := nFrozenWidth := _TBR_COORD( ::n_Right ) - _TBR_COORD( ::n_Left ) + 1
   /* _MAXFREEZE( ::nFrozen, ::aColData, @nWidth ) */
   nFrozenWidth -= nWidth

   RETURN { ;
      ::n_Top + ::nHeadHeight + iif( ::lHeadSep, 1, 0 ), ;
      ::n_Left, ;
      ::n_Bottom - ::nFootHeight - iif( ::lFootSep, 1, 0 ), ;
      ::n_Right, ;
      nFrozenWidth }

/* NOTE: Returns the left margin relative column position of the first
         non-frozen column. Xbase++ compatible method. */
METHOD firstScrCol() CLASS xpp_TBrowse

   IF ::nConfigure != 0
      ::doConfigure()
   ENDIF

   // TOFIX

// RETURN iif( ::leftVisible == 0, 0, ::aColData[ ::leftVisible ][ _TBCI_COLPOS ] )
   RETURN 0

METHOD _left() CLASS xpp_TBrowse
   RETURN ::left()

METHOD _right() CLASS xpp_TBrowse
   RETURN ::right()

METHOD _end() CLASS xpp_TBrowse
   RETURN ::end()
