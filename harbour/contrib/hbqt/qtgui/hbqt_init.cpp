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

#include "hbapierr.h"
#include "hbvm.h"
#include "hbinit.h"

#if QT_VERSION >= 0x040500

#include <QtGui/QApplication>

#include <QtGui/QItemSelection>
#include <QtGui/QTextCursor>
#include <QtGui/QTextCharFormat>
#include <QtGui/QTextBlock>
#include <QtGui/QSessionManager>
#include <QtGui/QColor>

#define _RET_GC_PTR_

#ifdef _RET_GC_PTR_
HB_EXTERN_BEGIN
extern void * hbqt_gcAllocate_QColor( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QItemSelection( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QTextCharFormat( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QFont( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QTextCursor( void * pObj, bool bNew );
extern void * hbqt_gcAllocate_QTextBlock( void * pObj, bool bNew );
HB_EXTERN_END
#endif

/*----------------------------------------------------------------------*/

static void hbqt_SlotsExecQColor( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
#ifdef _RET_GC_PTR_
   hb_vmPushPointerGC( hbqt_gcAllocate_QColor( new QColor( ( *reinterpret_cast< QColor( * ) >( arguments[ 1 ] ) ) ), true ) ); /* TOFIX: Pass .prg level object to callback */
#else
   hb_vmPushPointer( new QColor( ( *reinterpret_cast< QColor( * )>( arguments[ 1 ] ) ) ) ); /* TOFIX: Pass .prg level object to callback */
#endif
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecItemSelItemSel( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
#ifdef _RET_GC_PTR_
   hb_vmPushPointerGC( hbqt_gcAllocate_QItemSelection( new QItemSelection( ( *reinterpret_cast< QItemSelection( * ) >( arguments[ 1 ] ) ) ), true ) ); /* TOFIX: Pass .prg level object to callback */
   hb_vmPushPointerGC( hbqt_gcAllocate_QItemSelection( new QItemSelection( ( *reinterpret_cast< QItemSelection( * ) >( arguments[ 1 ] ) ) ), true ) ); /* TOFIX: Pass .prg level object to callback */
#else
   hb_vmPushPointer( new QItemSelection( ( *reinterpret_cast< QItemSelection( * )>( arguments[ 1 ] ) ) ) ); /* TOFIX: Pass .prg level object to callback */
   hb_vmPushPointer( new QItemSelection( ( *reinterpret_cast< QItemSelection( * )>( arguments[ 2 ] ) ) ) ); /* TOFIX: Pass .prg level object to callback */
#endif
   hb_vmSend( 2 );
}

static void hbqt_SlotsExecQTextCharFormat( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
#ifdef _RET_GC_PTR_
   hb_vmPushPointerGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( ( *reinterpret_cast< QTextCharFormat( * ) >( arguments[ 1 ] ) ) ), true ) ); /* TOFIX: Pass .prg level object to callback */
#else
   hb_vmPushPointer( new QTextCharFormat( ( *reinterpret_cast<QTextCharFormat( * )>( arguments[ 1 ] ) ) ) ); /* TOFIX: Pass .prg level object to callback */
#endif
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQFont( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
#ifdef _RET_GC_PTR_
   hb_vmPushPointerGC( hbqt_gcAllocate_QFont( new QFont( ( *reinterpret_cast< QFont( * ) >( arguments[ 1 ] ) ) ), true ) ); /* TOFIX: Pass .prg level object to callback */
#else
   hb_vmPushPointer( new QFont( ( *reinterpret_cast< QFont( * )>( arguments[ 1 ] ) ) ) ); /* TOFIX: Pass .prg level object to callback */
#endif
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQTextCursor( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
#ifdef _RET_GC_PTR_
   hb_vmPushPointerGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( ( *reinterpret_cast< QTextCursor( * ) >( arguments[ 1 ] ) ) ), true ) ); /* TOFIX: Pass .prg level object to callback */
#else
   hb_vmPushPointer( new QTextCursor( ( *reinterpret_cast< QTextCursor( * )>( arguments[ 1 ] ) ) ) ); /* TOFIX: Pass .prg level object to callback */
#endif
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQTextBlock( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
#ifdef _RET_GC_PTR_
   hb_vmPushPointerGC( hbqt_gcAllocate_QTextBlock( new QTextBlock( ( *reinterpret_cast< QTextBlock( * ) >( arguments[ 1 ] ) ) ), true ) ); /* TOFIX: Pass .prg level object to callback */
#else
   hb_vmPushPointer( new QTextBlock( ( *reinterpret_cast< QTextBlock( * )>( arguments[ 1 ] ) ) ) ); /* TOFIX: Pass .prg level object to callback */
#endif
   hb_vmSend( 1 );
}

/*----------------------------------------------------------------------*/

static void hbqt_registerCallbacks( void )
{
   hbqt_slots_register_callback( "QColor"                           , hbqt_SlotsExecQColor          );
   hbqt_slots_register_callback( "QFont"                            , hbqt_SlotsExecQFont           );
   hbqt_slots_register_callback( "QItemSelection$QItemSelection"    , hbqt_SlotsExecItemSelItemSel  );
   hbqt_slots_register_callback( "QTextBlock"                       , hbqt_SlotsExecQTextBlock      );
   hbqt_slots_register_callback( "QTextCharFormat"                  , hbqt_SlotsExecQTextCharFormat );
   hbqt_slots_register_callback( "QTextCursor"                      , hbqt_SlotsExecQTextCursor     );
}

/*----------------------------------------------------------------------*/

static QApplication * s_app = NULL;

static int s_argc;
static char ** s_argv;

HB_FUNC_EXTERN( __HBQTCORE );

HB_FUNC( __HBQTGUI )
{
   HB_FUNC_EXEC( __HBQTCORE );
}

HB_EXTERN_BEGIN
extern HB_EXPORT QApplication * __hbqtgui_app( void );
HB_EXTERN_END

QApplication * __hbqtgui_app( void )
{
   return s_app;
}

static void hbqt_lib_init( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   s_argc = hb_cmdargARGC();
   s_argv = hb_cmdargARGV();

   s_app = new QApplication( s_argc, s_argv );

   if( ! s_app )
      hb_errInternal( 11001, "hbqt_lib_init(): HBQTGUI Initilization Error.", NULL, NULL );

   hb_cmdargInit( s_argc, s_argv );

   hbqt_registerCallbacks();
}

static void hbqt_lib_exit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );
}

HB_CALL_ON_STARTUP_BEGIN( _hbqtgui_init_ )
   hb_vmAtInit( hbqt_lib_init, NULL );
   hb_vmAtExit( hbqt_lib_exit, NULL );
HB_CALL_ON_STARTUP_END( _hbqtgui_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hbqtgui_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( _hbqtgui_init_ )
   #include "hbiniseg.h"
#endif

#endif
