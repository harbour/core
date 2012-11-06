/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * hb_SCRMAXROW(), hb_SCRMAXCOL() functions
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#include "hbapi.h"
#include "hbapigt.h"

/*
 * Normally it gives exactly the same result in all standard GT drivers
 * as MaxRow()/MaxCol(). The difference can appear in some extended
 * GT drivers which have additional functionality, f.e. CTW GT which
 * is upper level GT and add CTIII Window support. When it's activated
 * then MaxRow() will return current window max row and hb_ScrMaxRow() real
 * screen (window 0) max row what is the exact behavior of MaxRow()
 * in CT3, [druzus]
 */

HB_FUNC( HB_SCRMAXROW ) /* Return the maximum screen row number (zero origin) */
{
   int iRows, iCols;

   hb_gtScrDim( &iRows, &iCols );
   hb_retni( iRows - 1 );
}

HB_FUNC( HB_SCRMAXCOL ) /* Return the maximum screen column number (zero origin) */
{
   int iRows, iCols;

   hb_gtScrDim( &iRows, &iCols );
   hb_retni( iCols - 1 );
}
