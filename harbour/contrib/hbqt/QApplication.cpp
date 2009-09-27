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
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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
/*----------------------------------------------------------------------*/

#include "hbapi.h"
#include "hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum ColorSpec { NormalColor, CustomColor, ManyColor }
 *  enum Type { Tty, GuiClient, GuiServer }
 */


#include "hbapi.h"
#include "hbinit.h"
#include "hbdefs.h"
#include "hbapifs.h"
#include "hbapiitm.h"
#include "hbstack.h"
#include "hbvm.h"
#include "hbdate.h"
#include "hbapierr.h"

#include <qfont.h>
#include <qfontmetrics.h>
#include <qimage.h>
#include <qpalette.h>
#include <QtGui/QApplication>
#include <QtCore/QLocale>
#include <QtGui/QIcon>

void release_codeblocks();

static QApplication * app = NULL;
static bool hbqtinit = false;

static int s_argc;
static char ** s_argv;

/*
 * QApplication ( int & argc, char ** argv )
 * QApplication ( int & argc, char ** argv, bool GUIenabled )
 * QApplication ( int & argc, char ** argv, Type type )
 * QApplication ( Display * display, Qt::HANDLE visual = 0, Qt::HANDLE colormap = 0 )
 * QApplication ( Display * display, int & argc, char ** argv, Qt::HANDLE visual = 0, Qt::HANDLE colormap = 0 )
 * virtual ~QApplication ()
*/

#if 0
HB_FUNC( QT_QAPPLICATION )
{
   int i_argc = 0;
   char** c_argv = NULL;
   app = new QApplication( i_argc, c_argv );
   hb_retptr( ( QApplication* ) app);
}
#endif

HB_FUNC( QT_QAPPLICATION )
{
#if 0
   int i_argc;
   char ** c_argv;
   i_argc = hb_cmdargARGC();
   c_argv = hb_cmdargARGV();
   app = new QApplication( i_argc, c_argv );
#endif
   hb_retptr( ( QApplication * ) app );
}

static void hbqt_Exit( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   release_codeblocks();
}

static void hbqt_Init( void * cargo )
{
   HB_SYMBOL_UNUSED( cargo );

   s_argc = hb_cmdargARGC();
   s_argv = hb_cmdargARGV();

   app = new QApplication( s_argc, s_argv );

   if( app )
      hbqtinit = true;

   if( ! hbqtinit )
      hb_errInternal( 11001, "hbqt_Init(): QT Initilization Error.", NULL, NULL );

   hb_cmdargInit( s_argc, s_argv );

   hb_vmAtExit( hbqt_Exit, NULL );
}

HB_CALL_ON_STARTUP_BEGIN( _hb_hbqt_init_ )
   hb_vmAtInit( hbqt_Init, NULL );
HB_CALL_ON_STARTUP_END( _hb_hbqt_init_ )

#if defined( HB_PRAGMA_STARTUP )
   #pragma startup _hb_hbqt_init_
#elif defined( HB_MSC_STARTUP )
   #if defined( HB_OS_WIN_64 )
      #pragma section( HB_MSC_START_SEGMENT, long, read )
   #endif
   #pragma data_seg( HB_MSC_START_SEGMENT )
   static HB_$INITSYM hb_auto_hbqt_init_ = _hb_hbqt_init_;
   #pragma data_seg()
#endif

HB_FUNC( QT_QAPPLICATION_EXECUTE )
{
   hb_retni( app->exec() );
}

HB_FUNC( QT_QAPPLICATION_QUIT )
{
   app->quit();
}

#if 0
HB_FUNC( QT_QAPPLICATION_EXECUTE )
{
   hb_retni( app->exec() );
}

HB_FUNC( QT_QAPPLICATION_SETSTYLE )
{
   app->setStyle( hb_parcx( 2 ) );
}

HB_FUNC( QT_QAPPLICATION_QUIT )
{
   app->quit();
}
#endif

/*
 * DESTRUCTOR
 */
HB_FUNC( QT_QAPPLICATION_DESTROY )
{
   delete hbqt_par_QApplication( 1 );
}

/*
 * virtual void commitData ( QSessionManager & manager )
 */
HB_FUNC( QT_QAPPLICATION_COMMITDATA )
{
   hbqt_par_QApplication( 1 )->commitData( *hbqt_par_QSessionManager( 2 ) );
}

/*
 * QInputContext * inputContext () const
 */
HB_FUNC( QT_QAPPLICATION_INPUTCONTEXT )
{
   hb_retptr( ( QInputContext* ) hbqt_par_QApplication( 1 )->inputContext() );
}

/*
 * bool isSessionRestored () const
 */
HB_FUNC( QT_QAPPLICATION_ISSESSIONRESTORED )
{
   hb_retl( hbqt_par_QApplication( 1 )->isSessionRestored() );
}

/*
 * virtual void saveState ( QSessionManager & manager )
 */
HB_FUNC( QT_QAPPLICATION_SAVESTATE )
{
   hbqt_par_QApplication( 1 )->saveState( *hbqt_par_QSessionManager( 2 ) );
}

/*
 * QString sessionId () const
 */
HB_FUNC( QT_QAPPLICATION_SESSIONID )
{
   hb_retc( hbqt_par_QApplication( 1 )->sessionId().toAscii().data() );
}

/*
 * QString sessionKey () const
 */
HB_FUNC( QT_QAPPLICATION_SESSIONKEY )
{
   hb_retc( hbqt_par_QApplication( 1 )->sessionKey().toAscii().data() );
}

/*
 * void setInputContext ( QInputContext * inputContext )
 */
HB_FUNC( QT_QAPPLICATION_SETINPUTCONTEXT )
{
   hbqt_par_QApplication( 1 )->setInputContext( hbqt_par_QInputContext( 2 ) );
}

/*
 * QString styleSheet () const
 */
HB_FUNC( QT_QAPPLICATION_STYLESHEET )
{
   hb_retc( hbqt_par_QApplication( 1 )->styleSheet().toAscii().data() );
}

/*
 * QWidget * activeModalWidget ()
 */
HB_FUNC( QT_QAPPLICATION_ACTIVEMODALWIDGET )
{
   hb_retptr( ( QWidget* ) hbqt_par_QApplication( 1 )->activeModalWidget() );
}

/*
 * QWidget * activePopupWidget ()
 */
HB_FUNC( QT_QAPPLICATION_ACTIVEPOPUPWIDGET )
{
   hb_retptr( ( QWidget* ) hbqt_par_QApplication( 1 )->activePopupWidget() );
}

/*
 * QWidget * activeWindow ()
 */
HB_FUNC( QT_QAPPLICATION_ACTIVEWINDOW )
{
   hb_retptr( ( QWidget* ) hbqt_par_QApplication( 1 )->activeWindow() );
}

/*
 * void alert ( QWidget * widget, int msec = 0 )
 */
HB_FUNC( QT_QAPPLICATION_ALERT )
{
   hbqt_par_QApplication( 1 )->alert( hbqt_par_QWidget( 2 ), hb_parni( 3 ) );
}

/*
 * QWidgetList allWidgets ()
 */
HB_FUNC( QT_QAPPLICATION_ALLWIDGETS )
{
   hb_retptr( new QWidgetList( hbqt_par_QApplication( 1 )->allWidgets() ) );
}

/*
 * void beep ()
 */
HB_FUNC( QT_QAPPLICATION_BEEP )
{
   hbqt_par_QApplication( 1 )->beep();
}

/*
 * void changeOverrideCursor ( const QCursor & cursor )
 */
HB_FUNC( QT_QAPPLICATION_CHANGEOVERRIDECURSOR )
{
   hbqt_par_QApplication( 1 )->changeOverrideCursor( *hbqt_par_QCursor( 2 ) );
}

/*
 * QClipboard * clipboard ()
 */
HB_FUNC( QT_QAPPLICATION_CLIPBOARD )
{
   hb_retptr( ( QClipboard* ) hbqt_par_QApplication( 1 )->clipboard() );
}

/*
 * int colorSpec ()
 */
HB_FUNC( QT_QAPPLICATION_COLORSPEC )
{
   hb_retni( hbqt_par_QApplication( 1 )->colorSpec() );
}

/*
 * int cursorFlashTime ()
 */
HB_FUNC( QT_QAPPLICATION_CURSORFLASHTIME )
{
   hb_retni( hbqt_par_QApplication( 1 )->cursorFlashTime() );
}

/*
 * QDesktopWidget * desktop ()
 */
HB_FUNC( QT_QAPPLICATION_DESKTOP )
{
   hb_retptr( ( QDesktopWidget* ) hbqt_par_QApplication( 1 )->desktop() );
}

/*
 * bool desktopSettingsAware ()
 */
HB_FUNC( QT_QAPPLICATION_DESKTOPSETTINGSAWARE )
{
   hb_retl( hbqt_par_QApplication( 1 )->desktopSettingsAware() );
}

/*
 * int doubleClickInterval ()
 */
HB_FUNC( QT_QAPPLICATION_DOUBLECLICKINTERVAL )
{
   hb_retni( hbqt_par_QApplication( 1 )->doubleClickInterval() );
}

/*
 * int exec ()
 */
HB_FUNC( QT_QAPPLICATION_EXEC )
{
   hb_retni( hbqt_par_QApplication( 1 )->exec() );
}

/*
 * QWidget * focusWidget ()
 */
HB_FUNC( QT_QAPPLICATION_FOCUSWIDGET )
{
   hb_retptr( ( QWidget* ) hbqt_par_QApplication( 1 )->focusWidget() );
}

/*
 * QFont font ()
 */
HB_FUNC( QT_QAPPLICATION_FONT )
{
   hb_retptr( new QFont( hbqt_par_QApplication( 1 )->font() ) );
}

/*
 * QFont font ( const QWidget * widget )
 */
HB_FUNC( QT_QAPPLICATION_FONT_1 )
{
   hb_retptr( new QFont( hbqt_par_QApplication( 1 )->font( hbqt_par_QWidget( 2 ) ) ) );
}

/*
 * QFont font ( const char * className )
 */
HB_FUNC( QT_QAPPLICATION_FONT_2 )
{
   hb_retptr( new QFont( hbqt_par_QApplication( 1 )->font( hbqt_par_char( 2 ) ) ) );
}

/*
 * QFontMetrics fontMetrics ()
 */
HB_FUNC( QT_QAPPLICATION_FONTMETRICS )
{
   hb_retptr( new QFontMetrics( hbqt_par_QApplication( 1 )->fontMetrics() ) );
}

/*
 * QSize globalStrut ()
 */
HB_FUNC( QT_QAPPLICATION_GLOBALSTRUT )
{
   hb_retptr( new QSize( hbqt_par_QApplication( 1 )->globalStrut() ) );
}

/*
 * bool isEffectEnabled ( Qt::UIEffect effect )
 */
HB_FUNC( QT_QAPPLICATION_ISEFFECTENABLED )
{
   hb_retl( hbqt_par_QApplication( 1 )->isEffectEnabled( ( Qt::UIEffect ) hb_parni( 2 ) ) );
}

/*
 * bool isLeftToRight ()
 */
HB_FUNC( QT_QAPPLICATION_ISLEFTTORIGHT )
{
   hb_retl( hbqt_par_QApplication( 1 )->isLeftToRight() );
}

/*
 * bool isRightToLeft ()
 */
HB_FUNC( QT_QAPPLICATION_ISRIGHTTOLEFT )
{
   hb_retl( hbqt_par_QApplication( 1 )->isRightToLeft() );
}

/*
 * Qt::LayoutDirection keyboardInputDirection ()
 */
HB_FUNC( QT_QAPPLICATION_KEYBOARDINPUTDIRECTION )
{
   hb_retni( ( Qt::LayoutDirection ) hbqt_par_QApplication( 1 )->keyboardInputDirection() );
}

/*
 * int keyboardInputInterval ()
 */
HB_FUNC( QT_QAPPLICATION_KEYBOARDINPUTINTERVAL )
{
   hb_retni( hbqt_par_QApplication( 1 )->keyboardInputInterval() );
}

/*
 * QLocale keyboardInputLocale ()
 */
HB_FUNC( QT_QAPPLICATION_KEYBOARDINPUTLOCALE )
{
   hb_retptr( new QLocale( hbqt_par_QApplication( 1 )->keyboardInputLocale() ) );
}

/*
 * Qt::KeyboardModifiers keyboardModifiers ()
 */
HB_FUNC( QT_QAPPLICATION_KEYBOARDMODIFIERS )
{
   hb_retni( ( Qt::KeyboardModifiers ) hbqt_par_QApplication( 1 )->keyboardModifiers() );
}

/*
 * Qt::LayoutDirection layoutDirection ()
 */
HB_FUNC( QT_QAPPLICATION_LAYOUTDIRECTION )
{
   hb_retni( ( Qt::LayoutDirection ) hbqt_par_QApplication( 1 )->layoutDirection() );
}

/*
 * Qt::MouseButtons mouseButtons ()
 */
HB_FUNC( QT_QAPPLICATION_MOUSEBUTTONS )
{
   hb_retni( ( Qt::MouseButtons ) hbqt_par_QApplication( 1 )->mouseButtons() );
}

/*
 * QCursor * overrideCursor ()
 */
HB_FUNC( QT_QAPPLICATION_OVERRIDECURSOR )
{
   hb_retptr( ( QCursor* ) hbqt_par_QApplication( 1 )->overrideCursor() );
}

/*
 * QPalette palette ()
 */
HB_FUNC( QT_QAPPLICATION_PALETTE )
{
   hb_retptr( new QPalette( hbqt_par_QApplication( 1 )->palette() ) );
}

/*
 * QPalette palette ( const QWidget * widget )
 */
HB_FUNC( QT_QAPPLICATION_PALETTE_1 )
{
   hb_retptr( new QPalette( hbqt_par_QApplication( 1 )->palette( hbqt_par_QWidget( 2 ) ) ) );
}

/*
 * QPalette palette ( const char * className )
 */
HB_FUNC( QT_QAPPLICATION_PALETTE_2 )
{
   hb_retptr( new QPalette( hbqt_par_QApplication( 1 )->palette( hbqt_par_char( 2 ) ) ) );
}

/*
 * bool quitOnLastWindowClosed ()
 */
HB_FUNC( QT_QAPPLICATION_QUITONLASTWINDOWCLOSED )
{
   hb_retl( hbqt_par_QApplication( 1 )->quitOnLastWindowClosed() );
}

/*
 * void restoreOverrideCursor ()
 */
HB_FUNC( QT_QAPPLICATION_RESTOREOVERRIDECURSOR )
{
   hbqt_par_QApplication( 1 )->restoreOverrideCursor();
}

/*
 * void setActiveWindow ( QWidget * active )
 */
HB_FUNC( QT_QAPPLICATION_SETACTIVEWINDOW )
{
   hbqt_par_QApplication( 1 )->setActiveWindow( hbqt_par_QWidget( 2 ) );
}

/*
 * void setColorSpec ( int spec )
 */
HB_FUNC( QT_QAPPLICATION_SETCOLORSPEC )
{
   hbqt_par_QApplication( 1 )->setColorSpec( hb_parni( 2 ) );
}

/*
 * void setCursorFlashTime ( int )
 */
HB_FUNC( QT_QAPPLICATION_SETCURSORFLASHTIME )
{
   hbqt_par_QApplication( 1 )->setCursorFlashTime( hb_parni( 2 ) );
}

/*
 * void setDesktopSettingsAware ( bool on )
 */
HB_FUNC( QT_QAPPLICATION_SETDESKTOPSETTINGSAWARE )
{
   hbqt_par_QApplication( 1 )->setDesktopSettingsAware( hb_parl( 2 ) );
}

/*
 * void setDoubleClickInterval ( int )
 */
HB_FUNC( QT_QAPPLICATION_SETDOUBLECLICKINTERVAL )
{
   hbqt_par_QApplication( 1 )->setDoubleClickInterval( hb_parni( 2 ) );
}

/*
 * void setEffectEnabled ( Qt::UIEffect effect, bool enable = true )
 */
HB_FUNC( QT_QAPPLICATION_SETEFFECTENABLED )
{
   hbqt_par_QApplication( 1 )->setEffectEnabled( ( Qt::UIEffect ) hb_parni( 2 ), hb_parl( 3 ) );
}

/*
 * void setFont ( const QFont & font, const char * className = 0 )
 */
HB_FUNC( QT_QAPPLICATION_SETFONT )
{
   hbqt_par_QApplication( 1 )->setFont( *hbqt_par_QFont( 2 ), hbqt_par_char( 3 ) );
}

/*
 * void setGlobalStrut ( const QSize & )
 */
HB_FUNC( QT_QAPPLICATION_SETGLOBALSTRUT )
{
   hbqt_par_QApplication( 1 )->setGlobalStrut( *hbqt_par_QSize( 2 ) );
}

/*
 * void setGraphicsSystem ( const QString & system )
 */
HB_FUNC( QT_QAPPLICATION_SETGRAPHICSSYSTEM )
{
   hbqt_par_QApplication( 1 )->setGraphicsSystem( hbqt_par_QString( 2 ) );
}

/*
 * void setKeyboardInputInterval ( int )
 */
HB_FUNC( QT_QAPPLICATION_SETKEYBOARDINPUTINTERVAL )
{
   hbqt_par_QApplication( 1 )->setKeyboardInputInterval( hb_parni( 2 ) );
}

/*
 * void setLayoutDirection ( Qt::LayoutDirection direction )
 */
HB_FUNC( QT_QAPPLICATION_SETLAYOUTDIRECTION )
{
   hbqt_par_QApplication( 1 )->setLayoutDirection( ( Qt::LayoutDirection ) hb_parni( 2 ) );
}

/*
 * void setOverrideCursor ( const QCursor & cursor )
 */
HB_FUNC( QT_QAPPLICATION_SETOVERRIDECURSOR )
{
   hbqt_par_QApplication( 1 )->setOverrideCursor( *hbqt_par_QCursor( 2 ) );
}

/*
 * void setPalette ( const QPalette & palette, const char * className = 0 )
 */
HB_FUNC( QT_QAPPLICATION_SETPALETTE )
{
   hbqt_par_QApplication( 1 )->setPalette( *hbqt_par_QPalette( 2 ), hbqt_par_char( 3 ) );
}

/*
 * void setQuitOnLastWindowClosed ( bool quit )
 */
HB_FUNC( QT_QAPPLICATION_SETQUITONLASTWINDOWCLOSED )
{
   hbqt_par_QApplication( 1 )->setQuitOnLastWindowClosed( hb_parl( 2 ) );
}

/*
 * void setStartDragDistance ( int l )
 */
HB_FUNC( QT_QAPPLICATION_SETSTARTDRAGDISTANCE )
{
   hbqt_par_QApplication( 1 )->setStartDragDistance( hb_parni( 2 ) );
}

/*
 * void setStartDragTime ( int ms )
 */
HB_FUNC( QT_QAPPLICATION_SETSTARTDRAGTIME )
{
   hbqt_par_QApplication( 1 )->setStartDragTime( hb_parni( 2 ) );
}

/*
 * void setStyle ( QStyle * style )
 */
HB_FUNC( QT_QAPPLICATION_SETSTYLE )
{
   hbqt_par_QApplication( 1 )->setStyle( hbqt_par_QStyle( 2 ) );
}

/*
 * QStyle * setStyle ( const QString & style )
 */
HB_FUNC( QT_QAPPLICATION_SETSTYLE_1 )
{
   hb_retptr( ( QStyle* ) hbqt_par_QApplication( 1 )->setStyle( hbqt_par_QString( 2 ) ) );
}

/*
 * void setWheelScrollLines ( int )
 */
HB_FUNC( QT_QAPPLICATION_SETWHEELSCROLLLINES )
{
   hbqt_par_QApplication( 1 )->setWheelScrollLines( hb_parni( 2 ) );
}

/*
 * void setWindowIcon ( const QIcon & icon )
 */
HB_FUNC( QT_QAPPLICATION_SETWINDOWICON )
{
   hbqt_par_QApplication( 1 )->setWindowIcon( QIcon( hbqt_par_QString( 2 ) ) );
}

/*
 * int startDragDistance ()
 */
HB_FUNC( QT_QAPPLICATION_STARTDRAGDISTANCE )
{
   hb_retni( hbqt_par_QApplication( 1 )->startDragDistance() );
}

/*
 * int startDragTime ()
 */
HB_FUNC( QT_QAPPLICATION_STARTDRAGTIME )
{
   hb_retni( hbqt_par_QApplication( 1 )->startDragTime() );
}

/*
 * QStyle * style ()
 */
HB_FUNC( QT_QAPPLICATION_STYLE )
{
   hb_retptr( ( QStyle* ) hbqt_par_QApplication( 1 )->style() );
}

/*
 * void syncX ()
 */
HB_FUNC( QT_QAPPLICATION_SYNCX )
{
   hbqt_par_QApplication( 1 )->syncX();
}

/*
 * QWidget * topLevelAt ( const QPoint & point )
 */
HB_FUNC( QT_QAPPLICATION_TOPLEVELAT )
{
   hb_retptr( ( QWidget* ) hbqt_par_QApplication( 1 )->topLevelAt( *hbqt_par_QPoint( 2 ) ) );
}

/*
 * QWidget * topLevelAt ( int x, int y )
 */
HB_FUNC( QT_QAPPLICATION_TOPLEVELAT_1 )
{
   hb_retptr( ( QWidget* ) hbqt_par_QApplication( 1 )->topLevelAt( hb_parni( 2 ), hb_parni( 3 ) ) );
}

/*
 * QWidgetList topLevelWidgets ()
 */
HB_FUNC( QT_QAPPLICATION_TOPLEVELWIDGETS )
{
   hb_retptr( new QWidgetList( hbqt_par_QApplication( 1 )->topLevelWidgets() ) );
}

/*
 * Type type ()
 */
HB_FUNC( QT_QAPPLICATION_TYPE )
{
   hb_retni( ( QApplication::Type ) hbqt_par_QApplication( 1 )->type() );
}

/*
 * int wheelScrollLines ()
 */
HB_FUNC( QT_QAPPLICATION_WHEELSCROLLLINES )
{
   hb_retni( hbqt_par_QApplication( 1 )->wheelScrollLines() );
}

/*
 * QWidget * widgetAt ( const QPoint & point )
 */
HB_FUNC( QT_QAPPLICATION_WIDGETAT )
{
   hb_retptr( ( QWidget* ) hbqt_par_QApplication( 1 )->widgetAt( *hbqt_par_QPoint( 2 ) ) );
}

/*
 * QWidget * widgetAt ( int x, int y )
 */
HB_FUNC( QT_QAPPLICATION_WIDGETAT_1 )
{
   hb_retptr( ( QWidget* ) hbqt_par_QApplication( 1 )->widgetAt( hb_parni( 2 ), hb_parni( 3 ) ) );
}

/*
 * QIcon windowIcon ()
 */
HB_FUNC( QT_QAPPLICATION_WINDOWICON )
{
   hb_retptr( new QIcon( hbqt_par_QApplication( 1 )->windowIcon() ) );
}

/*
 * void aboutQt ()
 */
HB_FUNC( QT_QAPPLICATION_ABOUTQT )
{
   hbqt_par_QApplication( 1 )->aboutQt();
}

/*
 * void closeAllWindows ()
 */
HB_FUNC( QT_QAPPLICATION_CLOSEALLWINDOWS )
{
   hbqt_par_QApplication( 1 )->closeAllWindows();
}

/*
 * void setStyleSheet ( const QString & sheet )
 */
HB_FUNC( QT_QAPPLICATION_SETSTYLESHEET )
{
   hbqt_par_QApplication( 1 )->setStyleSheet( hbqt_par_QString( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
