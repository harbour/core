/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * OLE library C header
 *
 * Copyright 2009 Viktor Szakats <syenar.01 syenar hu>
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapicls.h"
#include "hbapierr.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbdate.h"
#include "hbinit.h"

/* This option can resolve compilation problems in C++ mode for some
 * compilers like OpenWatcom but not for all, f.e. it will not help
 * BCC when used with -P (C++ mode) switch.
 */
/*
#if defined( __cplusplus ) && !defined( CINTERFACE )
   #define CINTERFACE 1
#endif
*/

/* This code uses named union so this declaration is necessary for
 * compilers where nameless unions are default
 */
#if !defined( NONAMELESSUNION )
   #define NONAMELESSUNION
#endif

#include <ole2.h>

/* macros used to hide type of interface: C or C++
 */
#if defined( __cplusplus ) && !defined( CINTERFACE ) && \
   ( defined( __BORLANDC__ ) || defined( _MSC_VER ) || \
     ( defined(__WATCOMC__) && ( __WATCOMC__ >= 1270 ) ) )
#  define HB_OLE_C_API        0
#  define HB_ID_REF( id )     ( id )
#else
#  define HB_OLE_C_API        1
#  define HB_ID_REF( id )     ( &id )
#endif

HB_EXTERN_BEGIN

HB_EXPORT void       hb_oleInit( void );
HB_EXPORT void       hb_oleItemToVariant( VARIANT* pVariant, PHB_ITEM pItem );
HB_EXPORT void       hb_oleVariantToItem( PHB_ITEM pItem, VARIANT* pVariant );
HB_EXPORT IDispatch* hb_oleParam( int iParam );
HB_EXPORT PHB_ITEM   hb_oleItemPut( PHB_ITEM pItem, IDispatch* pDisp );

HB_EXTERN_END
