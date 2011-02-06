/*
 * $Id$
 */

/*
 * Copyright 2008 Pritpal Bedi <pritpal@vouchcac.com>
 *
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option )
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.   If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/ ).
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
 * not apply to the code that you add in this way.   To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "hbhpdf.h"

/*
HPDF_STATUS
HPDF_PDFA_SetPDFAConformance (HPDF_Doc pdf,HPDF_PDFAType pdfatype)
*/
HB_FUNC( HPDF_PDFA_SETPDFACONFORMANCE )
{
/* TOFIX: These functions are not exported from libharu. Until we find
          out it is design decision or bug, I excluded them from dynamic
          builds. [vszakats] */
#if HB_HPDF_VERS( 2, 2, 0 ) && ! defined( HB_DYNLIB )
   hb_retnl( HPDF_PDFA_SetPDFAConformance( hb_HPDF_Doc_par( 1 ), ( HPDF_PDFAType ) hb_parni( 2 ) ) );
#else
   hb_retnl( -1 );
#endif
}
