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

#include "hbapiitm.h"
#include "hbstack.h"
#include "hbvm.h"
#include "hbinit.h"

#if QT_VERSION >= 0x040500

#include <QtGui/QItemSelection>
#include <QtGui/QTextCursor>
#include <QtGui/QTextCharFormat>
#include <QtGui/QTextBlock>
#include <QtGui/QSessionManager>
#include <QtGui/QColor>

/*----------------------------------------------------------------------*/

static void hbqt_SlotsExecQColor( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushPointer( new QColor( ( *reinterpret_cast< QColor( * )>( arguments[ 1 ] ) ) ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecItemSelItemSel( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushPointer( new QItemSelection( ( *reinterpret_cast< QItemSelection( * )>( arguments[ 1 ] ) ) ) );
   hb_vmPushPointer( new QItemSelection( ( *reinterpret_cast< QItemSelection( * )>( arguments[ 2 ] ) ) ) );
   hb_vmSend( 2 );
}

static void hbqt_SlotsExecQTextCharFormat( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushPointer( new QTextCharFormat( ( *reinterpret_cast<QTextCharFormat( * )>( arguments[ 1 ] ) ) ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQFont( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushPointer( new QFont( ( *reinterpret_cast< QFont( * )>( arguments[ 1 ] ) ) ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQTextCursor( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushPointer( new QTextCursor( ( *reinterpret_cast< QTextCursor( * )>( arguments[ 1 ] ) ) ) );
   hb_vmSend( 1 );
}

static void hbqt_SlotsExecQTextBlock( PHB_ITEM * codeBlock, void ** arguments )
{
   hb_vmPushEvalSym();
   hb_vmPush( codeBlock );
   hb_vmPushPointer( new QTextBlock( ( *reinterpret_cast< QTextBlock( * )>( arguments[ 1 ] ) ) ) );
   hb_vmSend( 1 );
}

/*----------------------------------------------------------------------*/

static void hbqt_registerCallbacks( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   hbqt_slots_register_callback( "QColor"                           , hbqt_SlotsExecQColor          );
   hbqt_slots_register_callback( "QFont"                            , hbqt_SlotsExecQFont           );
   hbqt_slots_register_callback( "QItemSelection$QItemSelection"    , hbqt_SlotsExecItemSelItemSel  );
   hbqt_slots_register_callback( "QTextBlock"                       , hbqt_SlotsExecQTextBlock      );
   hbqt_slots_register_callback( "QTextCharFormat"                  , hbqt_SlotsExecQTextCharFormat );
   hbqt_slots_register_callback( "QTextCursor"                      , hbqt_SlotsExecQTextCursor     );
}

HB_CALL_ON_STARTUP_BEGIN( _hb_hbqtgui_register_init_ )
   hb_vmAtInit( hbqt_registerCallbacks, NULL );
HB_CALL_ON_STARTUP_END( _hb_hbqtgui_register_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_hbqtgui_register_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( _hb_hbqtgui_register_init_ )
   #include "hbiniseg.h"
#endif

/*----------------------------------------------------------------------*/

HB_FUNC( __HBQT_REGISTERGUICALLBACKS )
{
   hbqt_registerCallbacks( NULL );
}

#endif
