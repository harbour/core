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
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
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

/*
 *  Constructed[ 74/74 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  // QInputContext * inputContext () const
 *  // virtual bool macEventFilter ( EventHandlerCallRef, EventRef )
 *  // virtual bool qwsEventFilter ( QWSEvent * )
 *  // int qwsProcessEvent ( QWSEvent * )
 *  // void qwsSetCustomColors ( QRgb * colortable, int start, int numColors )
 *  // void setInputContext ( QInputContext * inputContext )
 *  // QWidgetList allWidgets ()
 *  //bool keypadNavigationEnabled ()
 *  //QDecoration & qwsDecoration ()
 *  //void qwsSetDecoration ( QDecoration * )
 *  //QDecoration * qwsSetDecoration ( const QString & decoration )
 *  //void setKeypadNavigationEnabled ( bool enable )
 *  // QWidgetList topLevelWidgets ()
 *  //int autoMaximizeThreshold () const
 *  //bool autoSipEnabled () const
 *  //void setAutoMaximizeThreshold ( const int threshold )
 *  //void setAutoSipEnabled ( const bool enabled )
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
#include <QtGui/QIcon>

static QApplication * s_app = NULL;

static int s_argc;
static char ** s_argv;

HB_FUNC_EXTERN( HB_QTCORE );

HB_FUNC( HB_QTGUI )
{
   HB_FUNC_EXEC( HB_QTCORE );
}

HB_FUNC( HB_QT ) /* For compatibility */
{
   HB_FUNC_EXEC( HB_QTCORE );
}

/*
 * QApplication ( int & argc, char ** argv )
 * QApplication ( int & argc, char ** argv, bool GUIenabled )
 * QApplication ( int & argc, char ** argv, Type type )
 * QApplication ( Display * display, Qt::HANDLE visual = 0, Qt::HANDLE colormap = 0 )
 * QApplication ( Display * display, int & argc, char ** argv, Qt::HANDLE visual = 0, Qt::HANDLE colormap = 0 )
 * virtual ~QApplication ()
*/


static void hbqtgui_Exit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   //s_app->quit();
   //s_app = NULL;
}

static void hbqtgui_Init( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   s_argc = hb_cmdargARGC();
   s_argv = hb_cmdargARGV();

   s_app = new QApplication( s_argc, s_argv );

   if( ! s_app )
      hb_errInternal( 11001, "hbqtgui_Init(): QT Initilization Error.", NULL, NULL );

   hb_cmdargInit( s_argc, s_argv );
}

HB_CALL_ON_STARTUP_BEGIN( _hb_hbqtgui_init_ )
   hb_vmAtInit( hbqtgui_Init, NULL );
   hb_vmAtExit( hbqtgui_Exit, NULL );
HB_CALL_ON_STARTUP_END( _hb_hbqtgui_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_hbqtgui_init_
#elif defined( HB_DATASEG_STARTUP )
   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( _hb_hbqtgui_init_ )
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
   {
      ( p )->commitData( *hbqt_par_QSessionManager( 2 ) );
   }
}

/*
 * bool isSessionRestored () const
 */
HB_FUNC( QT_QAPPLICATION_ISSESSIONRESTORED )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retl( ( p )->isSessionRestored() );
   }
}

/*
 * virtual void saveState ( QSessionManager & manager )
 */
HB_FUNC( QT_QAPPLICATION_SAVESTATE )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      ( p )->saveState( *hbqt_par_QSessionManager( 2 ) );
   }
}

/*
 * QString sessionId () const
 */
HB_FUNC( QT_QAPPLICATION_SESSIONID )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->sessionId().toUtf8().data() );
   }
}

/*
 * QString sessionKey () const
 */
HB_FUNC( QT_QAPPLICATION_SESSIONKEY )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->sessionKey().toUtf8().data() );
   }
}

/*
 * QString styleSheet () const
 */
HB_FUNC( QT_QAPPLICATION_STYLESHEET )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->styleSheet().toUtf8().data() );
   }
}

/*
 * QWidget * activeModalWidget ()
 */
HB_FUNC( QT_QAPPLICATION_ACTIVEMODALWIDGET )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->activeModalWidget(), false ) );
   }
}

/*
 * QWidget * activePopupWidget ()
 */
HB_FUNC( QT_QAPPLICATION_ACTIVEPOPUPWIDGET )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->activePopupWidget(), false ) );
   }
}

/*
 * QWidget * activeWindow ()
 */
HB_FUNC( QT_QAPPLICATION_ACTIVEWINDOW )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->activeWindow(), false ) );
   }
}

/*
 * void alert ( QWidget * widget, int msec = 0 )
 */
HB_FUNC( QT_QAPPLICATION_ALERT )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      ( p )->alert( hbqt_par_QWidget( 2 ), hb_parni( 3 ) );
   }
}

/*
 * void beep ()
 */
HB_FUNC( QT_QAPPLICATION_BEEP )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      ( p )->beep();
   }
}

/*
 * void changeOverrideCursor ( const QCursor & cursor )
 */
HB_FUNC( QT_QAPPLICATION_CHANGEOVERRIDECURSOR )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      ( p )->changeOverrideCursor( *hbqt_par_QCursor( 2 ) );
   }
}

/*
 * QClipboard * clipboard ()
 */
HB_FUNC( QT_QAPPLICATION_CLIPBOARD )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QClipboard( ( p )->clipboard(), false ) );
   }
}

/*
 * int colorSpec ()
 */
HB_FUNC( QT_QAPPLICATION_COLORSPEC )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retni( ( p )->colorSpec() );
   }
}

/*
 * int cursorFlashTime ()
 */
HB_FUNC( QT_QAPPLICATION_CURSORFLASHTIME )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retni( ( p )->cursorFlashTime() );
   }
}

/*
 * QDesktopWidget * desktop ()
 */
HB_FUNC( QT_QAPPLICATION_DESKTOP )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QDesktopWidget( ( p )->desktop(), false ) );
   }
}

/*
 * bool desktopSettingsAware ()
 */
HB_FUNC( QT_QAPPLICATION_DESKTOPSETTINGSAWARE )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retl( ( p )->desktopSettingsAware() );
   }
}

/*
 * int doubleClickInterval ()
 */
HB_FUNC( QT_QAPPLICATION_DOUBLECLICKINTERVAL )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retni( ( p )->doubleClickInterval() );
   }
}

/*
 * int exec ()
 */
HB_FUNC( QT_QAPPLICATION_EXEC )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retni( ( p )->exec() );
   }
}

/*
 * QWidget * focusWidget ()
 */
HB_FUNC( QT_QAPPLICATION_FOCUSWIDGET )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->focusWidget(), false ) );
   }
}

/*
 * QFont font ()
 */
HB_FUNC( QT_QAPPLICATION_FONT )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) );
   }
}

/*
 * QFont font ( const QWidget * widget )
 */
HB_FUNC( QT_QAPPLICATION_FONT_1 )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font( hbqt_par_QWidget( 2 ) ) ), true ) );
   }
}

/*
 * QFont font ( const char * className )
 */
HB_FUNC( QT_QAPPLICATION_FONT_2 )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font( ( const char * ) hb_parc( 2 ) ) ), true ) );
   }
}

/*
 * QFontMetrics fontMetrics ()
 */
HB_FUNC( QT_QAPPLICATION_FONTMETRICS )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QFontMetrics( new QFontMetrics( ( p )->fontMetrics() ), true ) );
   }
}

/*
 * QSize globalStrut ()
 */
HB_FUNC( QT_QAPPLICATION_GLOBALSTRUT )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->globalStrut() ), true ) );
   }
}

/*
 * bool isEffectEnabled ( Qt::UIEffect effect )
 */
HB_FUNC( QT_QAPPLICATION_ISEFFECTENABLED )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retl( ( p )->isEffectEnabled( ( Qt::UIEffect ) hb_parni( 2 ) ) );
   }
}

/*
 * bool isLeftToRight ()
 */
HB_FUNC( QT_QAPPLICATION_ISLEFTTORIGHT )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retl( ( p )->isLeftToRight() );
   }
}

/*
 * bool isRightToLeft ()
 */
HB_FUNC( QT_QAPPLICATION_ISRIGHTTOLEFT )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retl( ( p )->isRightToLeft() );
   }
}

/*
 * Qt::LayoutDirection keyboardInputDirection ()
 */
HB_FUNC( QT_QAPPLICATION_KEYBOARDINPUTDIRECTION )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retni( ( Qt::LayoutDirection ) ( p )->keyboardInputDirection() );
   }
}

/*
 * int keyboardInputInterval ()
 */
HB_FUNC( QT_QAPPLICATION_KEYBOARDINPUTINTERVAL )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retni( ( p )->keyboardInputInterval() );
   }
}

/*
 * QLocale keyboardInputLocale ()
 */
HB_FUNC( QT_QAPPLICATION_KEYBOARDINPUTLOCALE )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QLocale( new QLocale( ( p )->keyboardInputLocale() ), true ) );
   }
}

/*
 * Qt::KeyboardModifiers keyboardModifiers ()
 */
HB_FUNC( QT_QAPPLICATION_KEYBOARDMODIFIERS )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retni( ( Qt::KeyboardModifiers ) ( p )->keyboardModifiers() );
   }
}

/*
 * Qt::LayoutDirection layoutDirection ()
 */
HB_FUNC( QT_QAPPLICATION_LAYOUTDIRECTION )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retni( ( Qt::LayoutDirection ) ( p )->layoutDirection() );
   }
}

/*
 * Qt::MouseButtons mouseButtons ()
 */
HB_FUNC( QT_QAPPLICATION_MOUSEBUTTONS )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retni( ( Qt::MouseButtons ) ( p )->mouseButtons() );
   }
}

/*
 * QCursor * overrideCursor ()
 */
HB_FUNC( QT_QAPPLICATION_OVERRIDECURSOR )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QCursor( ( p )->overrideCursor(), false ) );
   }
}

/*
 * QPalette palette ()
 */
HB_FUNC( QT_QAPPLICATION_PALETTE )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPalette( new QPalette( ( p )->palette() ), true ) );
   }
}

/*
 * QPalette palette ( const QWidget * widget )
 */
HB_FUNC( QT_QAPPLICATION_PALETTE_1 )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPalette( new QPalette( ( p )->palette( hbqt_par_QWidget( 2 ) ) ), true ) );
   }
}

/*
 * QPalette palette ( const char * className )
 */
HB_FUNC( QT_QAPPLICATION_PALETTE_2 )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPalette( new QPalette( ( p )->palette( ( const char * ) hb_parc( 2 ) ) ), true ) );
   }
}

/*
 * bool quitOnLastWindowClosed ()
 */
HB_FUNC( QT_QAPPLICATION_QUITONLASTWINDOWCLOSED )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retl( ( p )->quitOnLastWindowClosed() );
   }
}

/*
 * void restoreOverrideCursor ()
 */
HB_FUNC( QT_QAPPLICATION_RESTOREOVERRIDECURSOR )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      ( p )->restoreOverrideCursor();
   }
}

/*
 * void setActiveWindow ( QWidget * active )
 */
HB_FUNC( QT_QAPPLICATION_SETACTIVEWINDOW )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      ( p )->setActiveWindow( hbqt_par_QWidget( 2 ) );
   }
}

/*
 * void setColorSpec ( int spec )
 */
HB_FUNC( QT_QAPPLICATION_SETCOLORSPEC )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      ( p )->setColorSpec( hb_parni( 2 ) );
   }
}

/*
 * void setCursorFlashTime ( int )
 */
HB_FUNC( QT_QAPPLICATION_SETCURSORFLASHTIME )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      ( p )->setCursorFlashTime( hb_parni( 2 ) );
   }
}

/*
 * void setDesktopSettingsAware ( bool on )
 */
HB_FUNC( QT_QAPPLICATION_SETDESKTOPSETTINGSAWARE )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      ( p )->setDesktopSettingsAware( hb_parl( 2 ) );
   }
}

/*
 * void setDoubleClickInterval ( int )
 */
HB_FUNC( QT_QAPPLICATION_SETDOUBLECLICKINTERVAL )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      ( p )->setDoubleClickInterval( hb_parni( 2 ) );
   }
}

/*
 * void setEffectEnabled ( Qt::UIEffect effect, bool enable = true )
 */
HB_FUNC( QT_QAPPLICATION_SETEFFECTENABLED )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      ( p )->setEffectEnabled( ( Qt::UIEffect ) hb_parni( 2 ), hb_parl( 3 ) );
   }
}

/*
 * void setFont ( const QFont & font, const char * className = 0 )
 */
HB_FUNC( QT_QAPPLICATION_SETFONT )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      ( p )->setFont( *hbqt_par_QFont( 2 ), ( const char * ) hb_parc( 3 ) );
   }
}

/*
 * void setGlobalStrut ( const QSize & )
 */
HB_FUNC( QT_QAPPLICATION_SETGLOBALSTRUT )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      ( p )->setGlobalStrut( *hbqt_par_QSize( 2 ) );
   }
}

/*
 * void setGraphicsSystem ( const QString & system )
 */
HB_FUNC( QT_QAPPLICATION_SETGRAPHICSSYSTEM )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      void * pText;
      ( p )->setGraphicsSystem( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void setKeyboardInputInterval ( int )
 */
HB_FUNC( QT_QAPPLICATION_SETKEYBOARDINPUTINTERVAL )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      ( p )->setKeyboardInputInterval( hb_parni( 2 ) );
   }
}

/*
 * void setLayoutDirection ( Qt::LayoutDirection direction )
 */
HB_FUNC( QT_QAPPLICATION_SETLAYOUTDIRECTION )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      ( p )->setLayoutDirection( ( Qt::LayoutDirection ) hb_parni( 2 ) );
   }
}

/*
 * void setOverrideCursor ( const QCursor & cursor )
 */
HB_FUNC( QT_QAPPLICATION_SETOVERRIDECURSOR )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      ( p )->setOverrideCursor( *hbqt_par_QCursor( 2 ) );
   }
}

/*
 * void setPalette ( const QPalette & palette, const char * className = 0 )
 */
HB_FUNC( QT_QAPPLICATION_SETPALETTE )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      ( p )->setPalette( *hbqt_par_QPalette( 2 ), ( const char * ) hb_parc( 3 ) );
   }
}

/*
 * void setQuitOnLastWindowClosed ( bool quit )
 */
HB_FUNC( QT_QAPPLICATION_SETQUITONLASTWINDOWCLOSED )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      ( p )->setQuitOnLastWindowClosed( hb_parl( 2 ) );
   }
}

/*
 * void setStartDragDistance ( int l )
 */
HB_FUNC( QT_QAPPLICATION_SETSTARTDRAGDISTANCE )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      ( p )->setStartDragDistance( hb_parni( 2 ) );
   }
}

/*
 * void setStartDragTime ( int ms )
 */
HB_FUNC( QT_QAPPLICATION_SETSTARTDRAGTIME )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      ( p )->setStartDragTime( hb_parni( 2 ) );
   }
}

/*
 * void setStyle ( QStyle * style )
 */
HB_FUNC( QT_QAPPLICATION_SETSTYLE )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      ( p )->setStyle( hbqt_par_QStyle( 2 ) );
   }
}

/*
 * QStyle * setStyle ( const QString & style )
 */
HB_FUNC( QT_QAPPLICATION_SETSTYLE_1 )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QStyle( ( p )->setStyle( hb_parstr_utf8( 2, &pText, NULL ) ), false ) );
      hb_strfree( pText );
   }
}

/*
 * void setWheelScrollLines ( int )
 */
HB_FUNC( QT_QAPPLICATION_SETWHEELSCROLLLINES )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      ( p )->setWheelScrollLines( hb_parni( 2 ) );
   }
}

/*
 * void setWindowIcon ( const QIcon & icon )
 */
HB_FUNC( QT_QAPPLICATION_SETWINDOWICON )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      ( p )->setWindowIcon( ( HB_ISCHAR( 2 ) ? QIcon( hbqt_par_QString( 2 ) ) : *hbqt_par_QIcon( 2 )) );
   }
}

/*
 * int startDragDistance ()
 */
HB_FUNC( QT_QAPPLICATION_STARTDRAGDISTANCE )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retni( ( p )->startDragDistance() );
   }
}

/*
 * int startDragTime ()
 */
HB_FUNC( QT_QAPPLICATION_STARTDRAGTIME )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retni( ( p )->startDragTime() );
   }
}

/*
 * QStyle * style ()
 */
HB_FUNC( QT_QAPPLICATION_STYLE )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStyle( ( p )->style(), false ) );
   }
}

/*
 * void syncX ()
 */
HB_FUNC( QT_QAPPLICATION_SYNCX )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      ( p )->syncX();
   }
}

/*
 * QWidget * topLevelAt ( const QPoint & point )
 */
HB_FUNC( QT_QAPPLICATION_TOPLEVELAT )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->topLevelAt( *hbqt_par_QPoint( 2 ) ), false ) );
   }
}

/*
 * QWidget * topLevelAt ( int x, int y )
 */
HB_FUNC( QT_QAPPLICATION_TOPLEVELAT_1 )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->topLevelAt( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
   }
}

/*
 * Type type ()
 */
HB_FUNC( QT_QAPPLICATION_TYPE )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retni( ( QApplication::Type ) ( p )->type() );
   }
}

/*
 * int wheelScrollLines ()
 */
HB_FUNC( QT_QAPPLICATION_WHEELSCROLLLINES )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retni( ( p )->wheelScrollLines() );
   }
}

/*
 * QWidget * widgetAt ( const QPoint & point )
 */
HB_FUNC( QT_QAPPLICATION_WIDGETAT )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widgetAt( *hbqt_par_QPoint( 2 ) ), false ) );
   }
}

/*
 * QWidget * widgetAt ( int x, int y )
 */
HB_FUNC( QT_QAPPLICATION_WIDGETAT_1 )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widgetAt( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
   }
}

/*
 * QIcon windowIcon ()
 */
HB_FUNC( QT_QAPPLICATION_WINDOWICON )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->windowIcon() ), true ) );
   }
}

/*
 * void aboutQt ()
 */
HB_FUNC( QT_QAPPLICATION_ABOUTQT )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      ( p )->aboutQt();
   }
}

/*
 * void closeAllWindows ()
 */
HB_FUNC( QT_QAPPLICATION_CLOSEALLWINDOWS )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      ( p )->closeAllWindows();
   }
}

/*
 * void setStyleSheet ( const QString & sheet )
 */
HB_FUNC( QT_QAPPLICATION_SETSTYLESHEET )
{
   QApplication * p = hbqt_par_QApplication( 1 );
   if( p )
   {
      void * pText;
      ( p )->setStyleSheet( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
