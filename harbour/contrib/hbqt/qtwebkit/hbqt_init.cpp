/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta (marcosgambeta at gmail dot com)
 * Copyright 2009 Pritpal Bedi (pritpal@vouchcac.com)
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
 * Copyright 2010 Francesco Perillo ()
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

#include "hbqt.h"
#include "hbqtinit.h"

#include "hbvm.h"
#include "hbinit.h"

#if QT_VERSION >= 0x040500

/*----------------------------------------------------------------------*/

static void hbqt_registerCallbacks( void )
{
}

/*----------------------------------------------------------------------*/

HB_FUNC( __HBQTWEBKIT ) {;}

static void hbqt_lib_init( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   hbqt_registerCallbacks();
}

static void hbqt_lib_exit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );
}

HB_CALL_ON_STARTUP_BEGIN( _hbqtwebkit_init_ )
   hb_vmAtInit( hbqt_lib_init, NULL );
   hb_vmAtExit( hbqt_lib_exit, NULL );
HB_CALL_ON_STARTUP_END( _hbqtwebkit_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hbqtwebkit_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( _hbqtwebkit_init_ )
   #include "hbiniseg.h"
#endif

#endif
