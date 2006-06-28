/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    Mini GT for GUI programs.
 *    Now it supports only low level TONE and CLIPBOARD code for W32
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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


/* NOTE: User programs should never call this layer directly! */


#include "hbapi.h"

/*
 * This GT is called GUI but we introduce a hack to make
 * Windows users happy ;-) and we will add aliased name equal
 * to the default GT REQUESTed by our RTL library, [druzus]
 */

#if defined( HB_OS_WIN_32 )

#if defined(HB_GT_DEFAULT)
#  define HB_GT_NAME HB_GT_DEFAULT
#elif defined(HB_GT_LIB)
#  define HB_GT_NAME HB_GT_LIB
#else
#  define HB_GT_NAME WIN
#endif

/* Small trick to check if the default GT is not already set to GUI */
#define GUI 1
#define gui 1

#if HB_GT_NAME + 1 == 1

#undef GUI
#undef gui

#include "hbgtcore.h"
#include "hbinit.h"

HB_GT_REQUEST( GUI );
HB_GT_ANNOUNCE( HB_GT_NAME );

HB_CALL_ON_STARTUP_BEGIN( _hb_startup_gt_hack_ )
   hb_gtSetDefault( "GUI" );
HB_CALL_ON_STARTUP_END( _hb_startup_gt_hack_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_startup_gt_hack_
#elif defined(HB_MSC_STARTUP)
   #if _MSC_VER >= 1010
      #pragma data_seg( ".CRT$XIY" )
      #pragma comment( linker, "/Merge:.CRT=.data" )
   #else
      #pragma data_seg( "XIY" )
   #endif
   static HB_$INITSYM hb_vm_auto__hb_startup_gt_hack_ = _hb_startup_gt_hack_;
   #pragma data_seg()
#endif


#endif

#endif /* HB_OS_WIN_32 */
