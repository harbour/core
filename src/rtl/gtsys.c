/*
 * Null and multi_GT switch video subsystem.
 *
 * Copyright 2003 Przemyslaw Czerpak <druzus@polbox.com>
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

/* NOTE: User programs should never call this layer directly! */

/* This definition has to be placed before #include "hbapigt.h" */
#define HB_GT_NAME  NUL

#include "hbgtcore.h"

/* NOTE: Must be in sync with hbgtcore.c */
#if defined( HB_GT_LIB )
   HB_GT_REQUEST( HB_GT_LIB )
#elif defined( HB_OS_WIN_CE )
   HB_GT_REQUEST( WVT )
#elif defined( HB_OS_WIN )
   HB_GT_REQUEST( WIN )
#elif defined( HB_OS_DOS )
   HB_GT_REQUEST( DOS )
#elif defined( HB_OS_OS2 )
   HB_GT_REQUEST( OS2 )
#elif defined( HB_OS_VXWORKS ) || defined( HB_OS_SYMBIAN )
   HB_GT_REQUEST( STD )
#elif defined( HB_OS_UNIX )
   HB_GT_REQUEST( TRM )
#else
   HB_GT_REQUEST( STD )
#endif

HB_FUNC( HB_GTSYS )
{
#if defined( HB_OS_WIN ) && defined( __BORLANDC__ ) && defined( __cplusplus )
   hb_gt_ForceLink_HB_GT_WIN();
#endif
}
