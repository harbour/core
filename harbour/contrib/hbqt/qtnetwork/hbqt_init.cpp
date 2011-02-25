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

#include "hbvm.h"
#include "hbinit.h"

#if QT_VERSION >= 0x040500

#include <QtNetwork/QUrlInfo>
#include <QtNetwork/QNetworkProxy>
#include <QtNetwork/QHttpResponseHeader>
#include <QtNetwork/QNetworkRequest>

HB_EXTERN_BEGIN
extern void * hbqt_gcAllocate_QObject( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QHttpResponseHeader( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QNetworkRequest( void * pObj, bool bNew );
#if 0  // Classes not initiated yet!
extern void * hbqt_gcAllocate_QNetworkProxy( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QUrlInfo( void * pObj, bool bNew );
#endif
HB_EXTERN_END

/*----------------------------------------------------------------------*/

static void hbqt_SlotsExecQHttpResponseHeader( PHB_ITEM * codeBlock, void ** arguments, QByteArray pList )
{
   Q_UNUSED( pList );

   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPush( hbqt_create_object( hbqt_gcAllocate_QHttpResponseHeader( new QHttpResponseHeader( ( *reinterpret_cast< QHttpResponseHeader( * ) >( arguments[ 1 ] ) ) ), true ), ( const char * ) "QHttpResponseHeader", 0 ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQNetworkProxyPointer( PHB_ITEM * codeBlock, void ** arguments, QByteArray pList )
{
   Q_UNUSED( pList );

   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushPointer( new QNetworkProxy( ( *reinterpret_cast< QNetworkProxy( * )>( arguments[ 1 ] ) ) ) ); /* TOFIX: Pass .prg level object to callback */
   hb_vmPush( hbqt_create_object( hbqt_gcAllocate_QObject( ( *reinterpret_cast< void*( * ) >( arguments[ 1 ] ) ), false ), ( const char * ) pList.data(), 2 ) );
   hb_vmSend( 2 );
}

static void hbqt_SlotsExecQNetworkRequest( PHB_ITEM * codeBlock, void ** arguments, QByteArray pList )
{
   Q_UNUSED( pList );

   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPush( hbqt_create_object( hbqt_gcAllocate_QNetworkRequest( new QNetworkRequest( ( *reinterpret_cast< QNetworkRequest( * ) >( arguments[ 1 ] ) ) ), true ), ( const char * ) "QNetworkRequest", 0 ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQUrlInfo( PHB_ITEM * codeBlock, void ** arguments, QByteArray pList )
{
   Q_UNUSED( pList );

   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushPointer( new QUrlInfo( ( *reinterpret_cast< QUrlInfo( * )>( arguments[ 1 ] ) ) ) ); /* TOFIX: Pass .prg level object to callback */
   hb_vmSend( 1 );
}

/*----------------------------------------------------------------------*/

static void hbqt_registerCallbacks( void )
{
   hbqt_slots_register_callback( "QHttpResponseHeader"   , hbqt_SlotsExecQHttpResponseHeader  );
   hbqt_slots_register_callback( "QNetworkProxy$pointer" , hbqt_SlotsExecQNetworkProxyPointer );
   hbqt_slots_register_callback( "QNetworkRequest"       , hbqt_SlotsExecQNetworkRequest      );
   hbqt_slots_register_callback( "QUrlInfo"              , hbqt_SlotsExecQUrlInfo             );
}

/*----------------------------------------------------------------------*/

HB_FUNC( __HBQTNETWORK ) {;}

static void hbqt_lib_init( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   hbqt_registerCallbacks();
}

static void hbqt_lib_exit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );
}

HB_CALL_ON_STARTUP_BEGIN( _hbqtnetwork_init_ )
   hb_vmAtInit( hbqt_lib_init, NULL );
   hb_vmAtExit( hbqt_lib_exit, NULL );
HB_CALL_ON_STARTUP_END( _hbqtnetwork_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hbqtnetwork_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( _hbqtnetwork_init_ )
   #include "hbiniseg.h"
#endif

#endif

