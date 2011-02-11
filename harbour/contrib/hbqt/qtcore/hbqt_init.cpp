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

#include <QtCore/QTextCodec>

#include <QtCore/QProcess>
#include <QtCore/QUrl>
#include <QtCore/QDate>
#include <QtCore/QDateTime>
#include <QtCore/QTime>
#include <QtCore/QPointer>
#include <QtCore/QByteArray>
#include <QtCore/QModelIndex>
#include <QtCore/QRectF>

#define _RET_GC_PTR_

#ifdef _RET_GC_PTR_
HB_EXTERN_BEGIN
extern void * hbqt_gcAllocate_QTime( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QSize( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QSizeF( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QPoint( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QRect( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QRectF( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QUrl( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QDate( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QDateTime( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QTime( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QModelIndex( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QStringList( void * pObj, bool bNew );
HB_EXTERN_END
#endif

/*----------------------------------------------------------------------*/

static void hbqt_SlotsExecPointer( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushPointer( *reinterpret_cast< void*( * )>( arguments[ 1 ] ) ); /* TOFIX: Pass .prg level object to callback */
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecPointerPointer( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushPointer( *reinterpret_cast< void*( * )>( arguments[ 1 ] ) ); /* TOFIX: Pass .prg level object to callback */
   hb_vmPushPointer( *reinterpret_cast< void*( * )>( arguments[ 2 ] ) ); /* TOFIX: Pass .prg level object to callback */
   hb_vmSend( 2 );
}

static void hbqt_SlotsExecPointerInt( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushPointer( *reinterpret_cast< void*( * )>( arguments[ 1 ] ) ); /* TOFIX: Pass .prg level object to callback */
   hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 2 ] ) ); /* TOFIX: Pass .prg level object to callback */
   hb_vmSend( 2 );
}

static void hbqt_SlotsExecBool( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushLogical( *reinterpret_cast< bool( * ) >( arguments[ 1 ] ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecDouble( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushDouble( *reinterpret_cast< double( * ) >( arguments[ 1 ] ), 4 );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecInt( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 1 ] ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecIntInt( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 1 ] ) );
   hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 2 ] ) );
   hb_vmSend( 2 );
}

static void hbqt_SlotsExecIntIntInt( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 1 ] ) );
   hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 2 ] ) );
   hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 3 ] ) );
   hb_vmSend( 3 );
}

static void hbqt_SlotsExecIntIntIntInt( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 1 ] ) );
   hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 2 ] ) );
   hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 3 ] ) );
   hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 4 ] ) );
   hb_vmSend( 4 );
}

static void hbqt_SlotsExecString( PHB_ITEM * codeBlock, void ** arguments )
{
   QString text = *reinterpret_cast< QString( * ) >( arguments[ 1 ] );
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushString( text.toAscii().data(), text.toAscii().length() );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecModel( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
#ifdef _RET_GC_PTR_
   hb_vmPushPointerGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( *reinterpret_cast< QModelIndex( * ) >( arguments[ 1 ] ) ) ), true ) ); /* TOFIX: Pass .prg level object to callback */
#else
   hb_vmPushPointer( new QModelIndex( ( *reinterpret_cast< QModelIndex( * ) >( arguments[ 1 ] ) ) ) ); /* TOFIX: Pass .prg level object to callback */
#endif
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecModelModel( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
#ifdef _RET_GC_PTR_
   hb_vmPushPointerGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( *reinterpret_cast< QModelIndex( * ) >( arguments[ 1 ] ) ) ), true ) ); /* TOFIX: Pass .prg level object to callback */
   hb_vmPushPointerGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( *reinterpret_cast< QModelIndex( * ) >( arguments[ 1 ] ) ) ), true ) ); /* TOFIX: Pass .prg level object to callback */
#else
   hb_vmPushPointer( new QModelIndex( ( *reinterpret_cast< QModelIndex( * ) >( arguments[ 1 ] ) ) ) ); /* TOFIX: Pass .prg level object to callback */
   hb_vmPushPointer( new QModelIndex( ( *reinterpret_cast< QModelIndex( * ) >( arguments[ 2 ] ) ) ) ); /* TOFIX: Pass .prg level object to callback */
#endif
   hb_vmSend( 2 );
}

static void hbqt_SlotsExecStringList( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
#ifdef _RET_GC_PTR_
   hb_vmPushPointerGC( hbqt_gcAllocate_QStringList( new QStringList( ( *reinterpret_cast< QStringList( * ) >( arguments[ 1 ] ) ) ), true ) ); /* TOFIX: Pass .prg level object to callback */
#else
   hb_vmPushPointer( new QStringList( ( *reinterpret_cast<QStringList( * )>( arguments[ 1 ] ) ) ) ); /* TOFIX: Pass .prg level object to callback */
#endif
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQPoint( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
#ifdef _RET_GC_PTR_
   hb_vmPushPointerGC( hbqt_gcAllocate_QPoint( new QPoint( ( *reinterpret_cast< QPoint( * ) >( arguments[ 1 ] ) ) ), true ) ); /* TOFIX: Pass .prg level object to callback */
#else
   hb_vmPushPointer( new QPoint( ( *reinterpret_cast< QPoint( * )>( arguments[ 1 ] ) ) ) ); /* TOFIX: Pass .prg level object to callback */
#endif
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQUrl( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
#ifdef _RET_GC_PTR_
   hb_vmPushPointerGC( hbqt_gcAllocate_QUrl( new QUrl( ( *reinterpret_cast< QUrl( * ) >( arguments[ 1 ] ) ) ), true ) ); /* TOFIX: Pass .prg level object to callback */
#else
   hb_vmPushPointer( new QUrl( ( *reinterpret_cast< QUrl( * )>( arguments[ 1 ] ) ) ) ); /* TOFIX: Pass .prg level object to callback */
#endif
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQDate( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
#ifdef _RET_GC_PTR_
   hb_vmPushPointerGC( hbqt_gcAllocate_QDate( new QDate( ( *reinterpret_cast< QDate( * ) >( arguments[ 1 ] ) ) ), true ) ); /* TOFIX: Pass .prg level object to callback */
#else
   hb_vmPushPointer( new QDate( ( *reinterpret_cast< QDate( * ) >( arguments[ 1 ] ) ) ) ); /* TOFIX: Pass .prg level object to callback */
#endif
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQDateTime( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
#ifdef _RET_GC_PTR_
   hb_vmPushPointerGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( *reinterpret_cast< QDateTime( * ) >( arguments[ 1 ] ) ) ), true ) ); /* TOFIX: Pass .prg level object to callback */
#else
   hb_vmPushPointer( new QDateTime( ( *reinterpret_cast< QDateTime( * ) >( arguments[ 1 ] ) ) ) ); /* TOFIX: Pass .prg level object to callback */
#endif
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQTime( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
#ifdef _RET_GC_PTR_
   hb_vmPushPointerGC( hbqt_gcAllocate_QTime( new QTime( ( *reinterpret_cast< QTime( * ) >( arguments[ 1 ] ) ) ), true ) ); /* TOFIX: Pass .prg level object to callback */
#else
   hb_vmPushPointer( new QTime( ( *reinterpret_cast< QTime( * ) >( arguments[ 1 ] ) ) ) ); /* TOFIX: Pass .prg level object to callback */
#endif
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQRectF( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
#ifdef _RET_GC_PTR_
   hb_vmPushPointerGC( hbqt_gcAllocate_QRectF( new QRectF( ( *reinterpret_cast< QRectF( * ) >( arguments[ 1 ] ) ) ), true ) ); /* TOFIX: Pass .prg level object to callback */
#else
   hb_vmPushPointer( new QRectF( ( *reinterpret_cast< QRectF( * )>( arguments[ 1 ] ) ) ) ); /* TOFIX: Pass .prg level object to callback */
#endif
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQRectInt( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
#ifdef _RET_GC_PTR_
   hb_vmPushPointerGC( hbqt_gcAllocate_QRect( new QRect( ( *reinterpret_cast< QRect( * ) >( arguments[ 1 ] ) ) ), true ) ); /* TOFIX: Pass .prg level object to callback */
#else
   hb_vmPushPointer( new QRect( ( *reinterpret_cast< QRect( * )>( arguments[ 1 ] ) ) ) ); /* TOFIX: Pass .prg level object to callback */
#endif
   hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 2 ] ) );
   hb_vmSend( 2 );
}

static void hbqt_SlotsExecQRect( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
#ifdef _RET_GC_PTR_
   hb_vmPushPointerGC( hbqt_gcAllocate_QRect( new QRect( ( *reinterpret_cast< QRect( * ) >( arguments[ 1 ] ) ) ), true ) ); /* TOFIX: Pass .prg level object to callback */
#else
   hb_vmPushPointer( new QRect( ( *reinterpret_cast< QRect( * )>( arguments[ 1 ] ) ) ) ); /* TOFIX: Pass .prg level object to callback */
#endif
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQSizeF( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
#ifdef _RET_GC_PTR_
   hb_vmPushPointerGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( *reinterpret_cast< QSizeF( * ) >( arguments[ 1 ] ) ) ), true ) ); /* TOFIX: Pass .prg level object to callback */
#else
   hb_vmPushPointer( new QSizeF( ( *reinterpret_cast< QSizeF( * )>( arguments[ 1 ] ) ) ) ); /* TOFIX: Pass .prg level object to callback */
#endif
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecModelIndexIntInt( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
#ifdef _RET_GC_PTR_
   hb_vmPushPointerGC( hbqt_gcAllocate_QModelIndex( new QModelIndex( ( *reinterpret_cast< QModelIndex( * ) >( arguments[ 1 ] ) ) ), true ) ); /* TOFIX: Pass .prg level object to callback */
#else
   hb_vmPushPointer( new QModelIndex( ( *reinterpret_cast< QModelIndex( * ) >( arguments[ 1 ] ) ) ) ); /* TOFIX: Pass .prg level object to callback */
#endif
   hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 2 ] ) );
   hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 3 ] ) );
   hb_vmSend( 3 );
}

static void hbqt_SlotsExecModelIndexList( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushPointer( new QModelIndexList( ( *reinterpret_cast< QModelIndexList( * ) >( arguments[ 1 ] ) ) ) ); /* TOFIX: Pass .prg level object to callback */
   hb_vmSend( 1 );
}

/*----------------------------------------------------------------------*/

static void hbqt_registerCallbacks( void )
{
   hbqt_slots_register_callback( "qint64"                  , hbqt_SlotsExecInt              );
   hbqt_slots_register_callback( "qint64$qint64"           , hbqt_SlotsExecIntInt           );
   hbqt_slots_register_callback( "int"                     , hbqt_SlotsExecInt              );
   hbqt_slots_register_callback( "int$int"                 , hbqt_SlotsExecIntInt           );
   hbqt_slots_register_callback( "int$int$int"             , hbqt_SlotsExecIntIntInt        );
   hbqt_slots_register_callback( "int$int$int$int"         , hbqt_SlotsExecIntIntIntInt     );
   hbqt_slots_register_callback( "bool"                    , hbqt_SlotsExecBool             );
   hbqt_slots_register_callback( "double"                  , hbqt_SlotsExecDouble           );
   hbqt_slots_register_callback( "pointer"                 , hbqt_SlotsExecPointer          );
   hbqt_slots_register_callback( "pointer$pointer"         , hbqt_SlotsExecPointerPointer   );
   hbqt_slots_register_callback( "pointer$int"             , hbqt_SlotsExecPointerInt       );
   hbqt_slots_register_callback( "QDate"                   , hbqt_SlotsExecQDate            );
   hbqt_slots_register_callback( "QDateTime"               , hbqt_SlotsExecQDateTime        );
   hbqt_slots_register_callback( "QModelIndex"             , hbqt_SlotsExecModel            );
   hbqt_slots_register_callback( "QModelIndex$int$int"     , hbqt_SlotsExecModelIndexIntInt );
   hbqt_slots_register_callback( "QModelIndexList"         , hbqt_SlotsExecModelIndexList   );
   hbqt_slots_register_callback( "QModelIndex$QModelIndex" , hbqt_SlotsExecModelModel       );
   hbqt_slots_register_callback( "QPoint"                  , hbqt_SlotsExecQPoint           );
   hbqt_slots_register_callback( "QRect$int"               , hbqt_SlotsExecQRectInt         );
   hbqt_slots_register_callback( "QRect"                   , hbqt_SlotsExecQRect            );
   hbqt_slots_register_callback( "QRectF"                  , hbqt_SlotsExecQRectF           );
   hbqt_slots_register_callback( "QSizeF"                  , hbqt_SlotsExecQSizeF           );
   hbqt_slots_register_callback( "QString"                 , hbqt_SlotsExecString           );
   hbqt_slots_register_callback( "QStringList"             , hbqt_SlotsExecStringList       );
   hbqt_slots_register_callback( "QTime"                   , hbqt_SlotsExecQTime            );
   hbqt_slots_register_callback( "QUrl"                    , hbqt_SlotsExecQUrl             );
}

/*----------------------------------------------------------------------*/

HB_FUNC( __HBQTCORE ) {;}

static void hbqt_lib_init( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   QTextCodec * codec = QTextCodec::codecForName( "UTF8" );
   QTextCodec::setCodecForTr( codec );
   QTextCodec::setCodecForCStrings( codec );

   hbqt_registerCallbacks();
}

static void hbqt_lib_exit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );
}

HB_CALL_ON_STARTUP_BEGIN( _hbqtcore_init_ )
   hb_vmAtInit( hbqt_lib_init, NULL );
   hb_vmAtExit( hbqt_lib_exit, NULL );
HB_CALL_ON_STARTUP_END( _hbqtcore_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hbqtcore_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( _hbqtcore_init_ )
   #include "hbiniseg.h"
#endif

#endif
