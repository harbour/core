/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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

#include "hbqtcore.h"
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum ColorSpec { NormalColor, CustomColor, ManyColor }
 *  enum Type { Tty, GuiClient, GuiServer }
 */

#include <QtCore/QPointer>

#include "hbapi.h"
#include "hbapierr.h"
#include "hbvm.h"
#include "hbinit.h"

#include <QtGui/QFont>
#include <QtGui/QFontMetrics>
#include <QtGui/QImage>
#include <QtGui/QPalette>
#include <QtGui/QApplication>
#include <QtCore/QLocale>
#include <QtCore/QTextCodec>
#include <QtGui/QIcon>

static QApplication * s_app = NULL;

static int s_argc;
static char ** s_argv;

HB_FUNC( HB_QT ) {;}

/*
 * QApplication ( int & argc, char ** argv )
 * QApplication ( int & argc, char ** argv, bool GUIenabled )
 * QApplication ( int & argc, char ** argv, Type type )
 * QApplication ( Display * display, Qt::HANDLE visual = 0, Qt::HANDLE colormap = 0 )
 * QApplication ( Display * display, int & argc, char ** argv, Qt::HANDLE visual = 0, Qt::HANDLE colormap = 0 )
 * virtual ~QApplication ()
*/


static void hbqt_Exit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   //s_app->quit();
   //s_app = NULL;
}

static void hbqt_Init( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   QTextCodec *codec = QTextCodec::codecForLocale();
   QTextCodec::setCodecForTr( codec );
   QTextCodec::setCodecForCStrings( codec );

   s_argc = hb_cmdargARGC();
   s_argv = hb_cmdargARGV();

   s_app = new QApplication( s_argc, s_argv );

   if( ! s_app )
      hb_errInternal( 11001, "hbqt_Init(): QT Initilization Error.", NULL, NULL );

   hb_cmdargInit( s_argc, s_argv );
}

HB_CALL_ON_STARTUP_BEGIN( _hb_hbqt_init_ )
   hb_vmAtInit( hbqt_Init, NULL );
   hb_vmAtExit( hbqt_Exit, NULL );
HB_CALL_ON_STARTUP_END( _hb_hbqt_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_hbqt_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( _hb_hbqt_init_ )
   #include "hbiniseg.h"
#endif

HB_FUNC( QT_QAPPLICATION_EXECUTE )
{
   hb_retni( s_app->exec() );
}

HB_FUNC( QT_QAPPLICATION_QUIT )
{
   s_app->quit();
}

typedef struct
{
   QPointer< QApplication > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QApplication;

HBQT_GC_FUNC( hbqt_gcRelease_QApplication )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QApplication( void * pObj, bool bNew )
{
   HBQT_GC_T_QApplication * p = ( HBQT_GC_T_QApplication * ) hb_gcAllocate( sizeof( HBQT_GC_T_QApplication ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QApplication >( ( QApplication * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QApplication;
   p->type = HBQT_TYPE_QApplication;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QApplication  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QApplication", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QAPPLICATION )
{
   //hb_retptr( ( QApplication * ) s_app );
   hb_retptr ( s_app );
}

/*
 * virtual void commitData ( QSessionManager & manager )
 */
HB_FUNC( QT_QAPPLICATION_COMMITDATA )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      ( p )->commitData( *hbqt_par_QSessionManager( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_COMMITDATA FP=( p )->commitData( *hbqt_par_QSessionManager( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool isSessionRestored () const
 */
HB_FUNC( QT_QAPPLICATION_ISSESSIONRESTORED )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retl( ( p )->isSessionRestored() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_ISSESSIONRESTORED FP=hb_retl( ( p )->isSessionRestored() ); p is NULL" ) );
   }
}

/*
 * virtual void saveState ( QSessionManager & manager )
 */
HB_FUNC( QT_QAPPLICATION_SAVESTATE )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      ( p )->saveState( *hbqt_par_QSessionManager( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_SAVESTATE FP=( p )->saveState( *hbqt_par_QSessionManager( 2 ) ); p is NULL" ) );
   }
}

/*
 * QString sessionId () const
 */
HB_FUNC( QT_QAPPLICATION_SESSIONID )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retc( ( p )->sessionId().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_SESSIONID FP=hb_retc( ( p )->sessionId().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString sessionKey () const
 */
HB_FUNC( QT_QAPPLICATION_SESSIONKEY )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retc( ( p )->sessionKey().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_SESSIONKEY FP=hb_retc( ( p )->sessionKey().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString styleSheet () const
 */
HB_FUNC( QT_QAPPLICATION_STYLESHEET )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retc( ( p )->styleSheet().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_STYLESHEET FP=hb_retc( ( p )->styleSheet().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QWidget * activeModalWidget ()
 */
HB_FUNC( QT_QAPPLICATION_ACTIVEMODALWIDGET )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->activeModalWidget(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_ACTIVEMODALWIDGET FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->activeModalWidget(), false ) ); p is NULL" ) );
   }
}

/*
 * QWidget * activePopupWidget ()
 */
HB_FUNC( QT_QAPPLICATION_ACTIVEPOPUPWIDGET )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->activePopupWidget(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_ACTIVEPOPUPWIDGET FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->activePopupWidget(), false ) ); p is NULL" ) );
   }
}

/*
 * QWidget * activeWindow ()
 */
HB_FUNC( QT_QAPPLICATION_ACTIVEWINDOW )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->activeWindow(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_ACTIVEWINDOW FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->activeWindow(), false ) ); p is NULL" ) );
   }
}

/*
 * void alert ( QWidget * widget, int msec = 0 )
 */
HB_FUNC( QT_QAPPLICATION_ALERT )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      ( p )->alert( hbqt_par_QWidget( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_ALERT FP=( p )->alert( hbqt_par_QWidget( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void beep ()
 */
HB_FUNC( QT_QAPPLICATION_BEEP )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      ( p )->beep();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_BEEP FP=( p )->beep(); p is NULL" ) );
   }
}

/*
 * void changeOverrideCursor ( const QCursor & cursor )
 */
HB_FUNC( QT_QAPPLICATION_CHANGEOVERRIDECURSOR )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      ( p )->changeOverrideCursor( *hbqt_par_QCursor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_CHANGEOVERRIDECURSOR FP=( p )->changeOverrideCursor( *hbqt_par_QCursor( 2 ) ); p is NULL" ) );
   }
}

/*
 * QClipboard * clipboard ()
 */
HB_FUNC( QT_QAPPLICATION_CLIPBOARD )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QClipboard( ( p )->clipboard(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_CLIPBOARD FP=hb_retptrGC( hbqt_gcAllocate_QClipboard( ( p )->clipboard(), false ) ); p is NULL" ) );
   }
}

/*
 * int colorSpec ()
 */
HB_FUNC( QT_QAPPLICATION_COLORSPEC )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retni( ( p )->colorSpec() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_COLORSPEC FP=hb_retni( ( p )->colorSpec() ); p is NULL" ) );
   }
}

/*
 * int cursorFlashTime ()
 */
HB_FUNC( QT_QAPPLICATION_CURSORFLASHTIME )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retni( ( p )->cursorFlashTime() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_CURSORFLASHTIME FP=hb_retni( ( p )->cursorFlashTime() ); p is NULL" ) );
   }
}

/*
 * QDesktopWidget * desktop ()
 */
HB_FUNC( QT_QAPPLICATION_DESKTOP )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDesktopWidget( ( p )->desktop(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_DESKTOP FP=hb_retptrGC( hbqt_gcAllocate_QDesktopWidget( ( p )->desktop(), false ) ); p is NULL" ) );
   }
}

/*
 * bool desktopSettingsAware ()
 */
HB_FUNC( QT_QAPPLICATION_DESKTOPSETTINGSAWARE )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retl( ( p )->desktopSettingsAware() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_DESKTOPSETTINGSAWARE FP=hb_retl( ( p )->desktopSettingsAware() ); p is NULL" ) );
   }
}

/*
 * int doubleClickInterval ()
 */
HB_FUNC( QT_QAPPLICATION_DOUBLECLICKINTERVAL )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retni( ( p )->doubleClickInterval() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_DOUBLECLICKINTERVAL FP=hb_retni( ( p )->doubleClickInterval() ); p is NULL" ) );
   }
}

/*
 * int exec ()
 */
HB_FUNC( QT_QAPPLICATION_EXEC )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retni( ( p )->exec() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_EXEC FP=hb_retni( ( p )->exec() ); p is NULL" ) );
   }
}

/*
 * QWidget * focusWidget ()
 */
HB_FUNC( QT_QAPPLICATION_FOCUSWIDGET )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->focusWidget(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_FOCUSWIDGET FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->focusWidget(), false ) ); p is NULL" ) );
   }
}

/*
 * QFont font ()
 */
HB_FUNC( QT_QAPPLICATION_FONT )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_FONT FP=hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) ); p is NULL" ) );
   }
}

/*
 * QFont font ( const QWidget * widget )
 */
HB_FUNC( QT_QAPPLICATION_FONT_1 )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font( hbqt_par_QWidget( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_FONT_1 FP=hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font( hbqt_par_QWidget( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QFont font ( const char * className )
 */
HB_FUNC( QT_QAPPLICATION_FONT_2 )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font( hbqt_par_char( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_FONT_2 FP=hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font( hbqt_par_char( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QFontMetrics fontMetrics ()
 */
HB_FUNC( QT_QAPPLICATION_FONTMETRICS )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFontMetrics( new QFontMetrics( ( p )->fontMetrics() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_FONTMETRICS FP=hb_retptrGC( hbqt_gcAllocate_QFontMetrics( new QFontMetrics( ( p )->fontMetrics() ), true ) ); p is NULL" ) );
   }
}

/*
 * QSize globalStrut ()
 */
HB_FUNC( QT_QAPPLICATION_GLOBALSTRUT )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->globalStrut() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_GLOBALSTRUT FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->globalStrut() ), true ) ); p is NULL" ) );
   }
}

/*
 * bool isEffectEnabled ( Qt::UIEffect effect )
 */
HB_FUNC( QT_QAPPLICATION_ISEFFECTENABLED )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retl( ( p )->isEffectEnabled( ( Qt::UIEffect ) hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_ISEFFECTENABLED FP=hb_retl( ( p )->isEffectEnabled( ( Qt::UIEffect ) hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool isLeftToRight ()
 */
HB_FUNC( QT_QAPPLICATION_ISLEFTTORIGHT )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retl( ( p )->isLeftToRight() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_ISLEFTTORIGHT FP=hb_retl( ( p )->isLeftToRight() ); p is NULL" ) );
   }
}

/*
 * bool isRightToLeft ()
 */
HB_FUNC( QT_QAPPLICATION_ISRIGHTTOLEFT )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retl( ( p )->isRightToLeft() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_ISRIGHTTOLEFT FP=hb_retl( ( p )->isRightToLeft() ); p is NULL" ) );
   }
}

/*
 * Qt::LayoutDirection keyboardInputDirection ()
 */
HB_FUNC( QT_QAPPLICATION_KEYBOARDINPUTDIRECTION )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retni( ( Qt::LayoutDirection ) ( p )->keyboardInputDirection() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_KEYBOARDINPUTDIRECTION FP=hb_retni( ( Qt::LayoutDirection ) ( p )->keyboardInputDirection() ); p is NULL" ) );
   }
}

/*
 * int keyboardInputInterval ()
 */
HB_FUNC( QT_QAPPLICATION_KEYBOARDINPUTINTERVAL )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retni( ( p )->keyboardInputInterval() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_KEYBOARDINPUTINTERVAL FP=hb_retni( ( p )->keyboardInputInterval() ); p is NULL" ) );
   }
}

/*
 * QLocale keyboardInputLocale ()
 */
HB_FUNC( QT_QAPPLICATION_KEYBOARDINPUTLOCALE )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QLocale( new QLocale( ( p )->keyboardInputLocale() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_KEYBOARDINPUTLOCALE FP=hb_retptrGC( hbqt_gcAllocate_QLocale( new QLocale( ( p )->keyboardInputLocale() ), true ) ); p is NULL" ) );
   }
}

/*
 * Qt::KeyboardModifiers keyboardModifiers ()
 */
HB_FUNC( QT_QAPPLICATION_KEYBOARDMODIFIERS )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retni( ( Qt::KeyboardModifiers ) ( p )->keyboardModifiers() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_KEYBOARDMODIFIERS FP=hb_retni( ( Qt::KeyboardModifiers ) ( p )->keyboardModifiers() ); p is NULL" ) );
   }
}

/*
 * Qt::LayoutDirection layoutDirection ()
 */
HB_FUNC( QT_QAPPLICATION_LAYOUTDIRECTION )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retni( ( Qt::LayoutDirection ) ( p )->layoutDirection() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_LAYOUTDIRECTION FP=hb_retni( ( Qt::LayoutDirection ) ( p )->layoutDirection() ); p is NULL" ) );
   }
}

/*
 * Qt::MouseButtons mouseButtons ()
 */
HB_FUNC( QT_QAPPLICATION_MOUSEBUTTONS )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retni( ( Qt::MouseButtons ) ( p )->mouseButtons() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_MOUSEBUTTONS FP=hb_retni( ( Qt::MouseButtons ) ( p )->mouseButtons() ); p is NULL" ) );
   }
}

/*
 * QCursor * overrideCursor ()
 */
HB_FUNC( QT_QAPPLICATION_OVERRIDECURSOR )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QCursor( ( p )->overrideCursor(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_OVERRIDECURSOR FP=hb_retptrGC( hbqt_gcAllocate_QCursor( ( p )->overrideCursor(), false ) ); p is NULL" ) );
   }
}

/*
 * QPalette palette ()
 */
HB_FUNC( QT_QAPPLICATION_PALETTE )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPalette( new QPalette( ( p )->palette() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_PALETTE FP=hb_retptrGC( hbqt_gcAllocate_QPalette( new QPalette( ( p )->palette() ), true ) ); p is NULL" ) );
   }
}

/*
 * QPalette palette ( const QWidget * widget )
 */
HB_FUNC( QT_QAPPLICATION_PALETTE_1 )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPalette( new QPalette( ( p )->palette( hbqt_par_QWidget( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_PALETTE_1 FP=hb_retptrGC( hbqt_gcAllocate_QPalette( new QPalette( ( p )->palette( hbqt_par_QWidget( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPalette palette ( const char * className )
 */
HB_FUNC( QT_QAPPLICATION_PALETTE_2 )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPalette( new QPalette( ( p )->palette( hbqt_par_char( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_PALETTE_2 FP=hb_retptrGC( hbqt_gcAllocate_QPalette( new QPalette( ( p )->palette( hbqt_par_char( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * bool quitOnLastWindowClosed ()
 */
HB_FUNC( QT_QAPPLICATION_QUITONLASTWINDOWCLOSED )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retl( ( p )->quitOnLastWindowClosed() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_QUITONLASTWINDOWCLOSED FP=hb_retl( ( p )->quitOnLastWindowClosed() ); p is NULL" ) );
   }
}

/*
 * void restoreOverrideCursor ()
 */
HB_FUNC( QT_QAPPLICATION_RESTOREOVERRIDECURSOR )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      ( p )->restoreOverrideCursor();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_RESTOREOVERRIDECURSOR FP=( p )->restoreOverrideCursor(); p is NULL" ) );
   }
}

/*
 * void setActiveWindow ( QWidget * active )
 */
HB_FUNC( QT_QAPPLICATION_SETACTIVEWINDOW )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      ( p )->setActiveWindow( hbqt_par_QWidget( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_SETACTIVEWINDOW FP=( p )->setActiveWindow( hbqt_par_QWidget( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setColorSpec ( int spec )
 */
HB_FUNC( QT_QAPPLICATION_SETCOLORSPEC )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      ( p )->setColorSpec( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_SETCOLORSPEC FP=( p )->setColorSpec( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCursorFlashTime ( int )
 */
HB_FUNC( QT_QAPPLICATION_SETCURSORFLASHTIME )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      ( p )->setCursorFlashTime( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_SETCURSORFLASHTIME FP=( p )->setCursorFlashTime( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDesktopSettingsAware ( bool on )
 */
HB_FUNC( QT_QAPPLICATION_SETDESKTOPSETTINGSAWARE )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      ( p )->setDesktopSettingsAware( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_SETDESKTOPSETTINGSAWARE FP=( p )->setDesktopSettingsAware( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setDoubleClickInterval ( int )
 */
HB_FUNC( QT_QAPPLICATION_SETDOUBLECLICKINTERVAL )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      ( p )->setDoubleClickInterval( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_SETDOUBLECLICKINTERVAL FP=( p )->setDoubleClickInterval( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setEffectEnabled ( Qt::UIEffect effect, bool enable = true )
 */
HB_FUNC( QT_QAPPLICATION_SETEFFECTENABLED )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      ( p )->setEffectEnabled( ( Qt::UIEffect ) hb_parni( 2 ), hb_parl( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_SETEFFECTENABLED FP=( p )->setEffectEnabled( ( Qt::UIEffect ) hb_parni( 2 ), hb_parl( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setFont ( const QFont & font, const char * className = 0 )
 */
HB_FUNC( QT_QAPPLICATION_SETFONT )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      ( p )->setFont( *hbqt_par_QFont( 2 ), hbqt_par_char( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_SETFONT FP=( p )->setFont( *hbqt_par_QFont( 2 ), hbqt_par_char( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setGlobalStrut ( const QSize & )
 */
HB_FUNC( QT_QAPPLICATION_SETGLOBALSTRUT )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      ( p )->setGlobalStrut( *hbqt_par_QSize( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_SETGLOBALSTRUT FP=( p )->setGlobalStrut( *hbqt_par_QSize( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setGraphicsSystem ( const QString & system )
 */
HB_FUNC( QT_QAPPLICATION_SETGRAPHICSSYSTEM )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      ( p )->setGraphicsSystem( QApplication::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_SETGRAPHICSSYSTEM FP=( p )->setGraphicsSystem( QApplication::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setKeyboardInputInterval ( int )
 */
HB_FUNC( QT_QAPPLICATION_SETKEYBOARDINPUTINTERVAL )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      ( p )->setKeyboardInputInterval( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_SETKEYBOARDINPUTINTERVAL FP=( p )->setKeyboardInputInterval( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setLayoutDirection ( Qt::LayoutDirection direction )
 */
HB_FUNC( QT_QAPPLICATION_SETLAYOUTDIRECTION )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      ( p )->setLayoutDirection( ( Qt::LayoutDirection ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_SETLAYOUTDIRECTION FP=( p )->setLayoutDirection( ( Qt::LayoutDirection ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setOverrideCursor ( const QCursor & cursor )
 */
HB_FUNC( QT_QAPPLICATION_SETOVERRIDECURSOR )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      ( p )->setOverrideCursor( *hbqt_par_QCursor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_SETOVERRIDECURSOR FP=( p )->setOverrideCursor( *hbqt_par_QCursor( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPalette ( const QPalette & palette, const char * className = 0 )
 */
HB_FUNC( QT_QAPPLICATION_SETPALETTE )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      ( p )->setPalette( *hbqt_par_QPalette( 2 ), hbqt_par_char( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_SETPALETTE FP=( p )->setPalette( *hbqt_par_QPalette( 2 ), hbqt_par_char( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setQuitOnLastWindowClosed ( bool quit )
 */
HB_FUNC( QT_QAPPLICATION_SETQUITONLASTWINDOWCLOSED )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      ( p )->setQuitOnLastWindowClosed( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_SETQUITONLASTWINDOWCLOSED FP=( p )->setQuitOnLastWindowClosed( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setStartDragDistance ( int l )
 */
HB_FUNC( QT_QAPPLICATION_SETSTARTDRAGDISTANCE )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      ( p )->setStartDragDistance( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_SETSTARTDRAGDISTANCE FP=( p )->setStartDragDistance( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setStartDragTime ( int ms )
 */
HB_FUNC( QT_QAPPLICATION_SETSTARTDRAGTIME )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      ( p )->setStartDragTime( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_SETSTARTDRAGTIME FP=( p )->setStartDragTime( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setStyle ( QStyle * style )
 */
HB_FUNC( QT_QAPPLICATION_SETSTYLE )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      ( p )->setStyle( hbqt_par_QStyle( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_SETSTYLE FP=( p )->setStyle( hbqt_par_QStyle( 2 ) ); p is NULL" ) );
   }
}

/*
 * QStyle * setStyle ( const QString & style )
 */
HB_FUNC( QT_QAPPLICATION_SETSTYLE_1 )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStyle( ( p )->setStyle( QApplication::tr( hb_parc( 2 ) ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_SETSTYLE_1 FP=hb_retptrGC( hbqt_gcAllocate_QStyle( ( p )->setStyle( QApplication::tr( hb_parc( 2 ) ) ), false ) ); p is NULL" ) );
   }
}

/*
 * void setWheelScrollLines ( int )
 */
HB_FUNC( QT_QAPPLICATION_SETWHEELSCROLLLINES )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      ( p )->setWheelScrollLines( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_SETWHEELSCROLLLINES FP=( p )->setWheelScrollLines( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setWindowIcon ( const QIcon & icon )
 */
HB_FUNC( QT_QAPPLICATION_SETWINDOWICON )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      ( p )->setWindowIcon( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QIcon( 2 ) : QIcon( hbqt_par_QString( 2 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_SETWINDOWICON FP=( p )->setWindowIcon( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QIcon( 2 ) : QIcon( hbqt_par_QString( 2 ) ) ) ); p is NULL" ) );
   }
}

/*
 * int startDragDistance ()
 */
HB_FUNC( QT_QAPPLICATION_STARTDRAGDISTANCE )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retni( ( p )->startDragDistance() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_STARTDRAGDISTANCE FP=hb_retni( ( p )->startDragDistance() ); p is NULL" ) );
   }
}

/*
 * int startDragTime ()
 */
HB_FUNC( QT_QAPPLICATION_STARTDRAGTIME )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retni( ( p )->startDragTime() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_STARTDRAGTIME FP=hb_retni( ( p )->startDragTime() ); p is NULL" ) );
   }
}

/*
 * QStyle * style ()
 */
HB_FUNC( QT_QAPPLICATION_STYLE )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStyle( ( p )->style(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_STYLE FP=hb_retptrGC( hbqt_gcAllocate_QStyle( ( p )->style(), false ) ); p is NULL" ) );
   }
}

/*
 * void syncX ()
 */
HB_FUNC( QT_QAPPLICATION_SYNCX )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      ( p )->syncX();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_SYNCX FP=( p )->syncX(); p is NULL" ) );
   }
}

/*
 * QWidget * topLevelAt ( const QPoint & point )
 */
HB_FUNC( QT_QAPPLICATION_TOPLEVELAT )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->topLevelAt( *hbqt_par_QPoint( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_TOPLEVELAT FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->topLevelAt( *hbqt_par_QPoint( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QWidget * topLevelAt ( int x, int y )
 */
HB_FUNC( QT_QAPPLICATION_TOPLEVELAT_1 )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->topLevelAt( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_TOPLEVELAT_1 FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->topLevelAt( hb_parni( 2 ), hb_parni( 3 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * Type type ()
 */
HB_FUNC( QT_QAPPLICATION_TYPE )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retni( ( QApplication::Type ) ( p )->type() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_TYPE FP=hb_retni( ( QApplication::Type ) ( p )->type() ); p is NULL" ) );
   }
}

/*
 * int wheelScrollLines ()
 */
HB_FUNC( QT_QAPPLICATION_WHEELSCROLLLINES )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retni( ( p )->wheelScrollLines() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_WHEELSCROLLLINES FP=hb_retni( ( p )->wheelScrollLines() ); p is NULL" ) );
   }
}

/*
 * QWidget * widgetAt ( const QPoint & point )
 */
HB_FUNC( QT_QAPPLICATION_WIDGETAT )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widgetAt( *hbqt_par_QPoint( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_WIDGETAT FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widgetAt( *hbqt_par_QPoint( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QWidget * widgetAt ( int x, int y )
 */
HB_FUNC( QT_QAPPLICATION_WIDGETAT_1 )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widgetAt( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_WIDGETAT_1 FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widgetAt( hb_parni( 2 ), hb_parni( 3 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QIcon windowIcon ()
 */
HB_FUNC( QT_QAPPLICATION_WINDOWICON )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->windowIcon() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_WINDOWICON FP=hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->windowIcon() ), true ) ); p is NULL" ) );
   }
}

/*
 * void aboutQt ()
 */
HB_FUNC( QT_QAPPLICATION_ABOUTQT )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      ( p )->aboutQt();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_ABOUTQT FP=( p )->aboutQt(); p is NULL" ) );
   }
}

/*
 * void closeAllWindows ()
 */
HB_FUNC( QT_QAPPLICATION_CLOSEALLWINDOWS )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      ( p )->closeAllWindows();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_CLOSEALLWINDOWS FP=( p )->closeAllWindows(); p is NULL" ) );
   }
}

/*
 * void setStyleSheet ( const QString & sheet )
 */
HB_FUNC( QT_QAPPLICATION_SETSTYLESHEET )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
      ( p )->setStyleSheet( QApplication::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QAPPLICATION_SETSTYLESHEET FP=( p )->setStyleSheet( QApplication::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
