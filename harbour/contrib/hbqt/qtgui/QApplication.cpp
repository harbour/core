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
#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum ColorSpec { NormalColor, CustomColor, ManyColor }
 *  enum Type { Tty, GuiClient, GuiServer }
 */

#include <QtCore/QPointer>

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

   HB_TRACE( HB_TR_ALWAYS, ( "hbqt_exit 0 %p", s_app ) );

   s_app->quit();
   s_app = NULL;

   HB_TRACE( HB_TR_ALWAYS, ( "hbqt_exit 1 %p", s_app ) );
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
  void * ph;
  bool bNew;
  QT_G_FUNC_PTR func;
  QPointer< QApplication > pq;
} QGC_POINTER_QApplication;

QT_G_FUNC( hbqt_gcRelease_QApplication )
{
   QGC_POINTER_QApplication * p = ( QGC_POINTER_QApplication * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph && p->pq )
      {
         const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            delete ( ( QApplication * ) p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QApplication               ph=%p pq=%p %i B %i KB", p->ph, (void *)(p->pq), ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "NO__rel_QApplication               ph=%p pq=%p %i B %i KB", p->ph, (void *)(p->pq), ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_QApplication                Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_QApplication                Object not created with - new" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QApplication( void * pObj, bool bNew )
{
   QGC_POINTER_QApplication * p = ( QGC_POINTER_QApplication * ) hb_gcAllocate( sizeof( QGC_POINTER_QApplication ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QApplication;

   if( bNew )
   {
      new( & p->pq ) QPointer< QApplication >( ( QApplication * ) pObj );
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QApplication               ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QAPPLICATION )
{
   void * pObj = NULL;

   pObj = ( QApplication * ) s_app ;

   hb_retptrGC( hbqt_gcAllocate_QApplication( pObj, true ) );
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
   hb_retptrGC( hbqt_gcAllocate_QInputContext( hbqt_par_QApplication( 1 )->inputContext(), false ) );
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
   hb_retptrGC( hbqt_gcAllocate_QWidget( hbqt_par_QApplication( 1 )->activeModalWidget(), false ) );
}

/*
 * QWidget * activePopupWidget ()
 */
HB_FUNC( QT_QAPPLICATION_ACTIVEPOPUPWIDGET )
{
   hb_retptrGC( hbqt_gcAllocate_QWidget( hbqt_par_QApplication( 1 )->activePopupWidget(), false ) );
}

/*
 * QWidget * activeWindow ()
 */
HB_FUNC( QT_QAPPLICATION_ACTIVEWINDOW )
{
   hb_retptrGC( hbqt_gcAllocate_QWidget( hbqt_par_QApplication( 1 )->activeWindow(), false ) );
}

/*
 * void alert ( QWidget * widget, int msec = 0 )
 */
HB_FUNC( QT_QAPPLICATION_ALERT )
{
   hbqt_par_QApplication( 1 )->alert( hbqt_par_QWidget( 2 ), hb_parni( 3 ) );
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
   hb_retptrGC( hbqt_gcAllocate_QClipboard( hbqt_par_QApplication( 1 )->clipboard(), false ) );
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
   hb_retptrGC( hbqt_gcAllocate_QDesktopWidget( hbqt_par_QApplication( 1 )->desktop(), false ) );
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
   hb_retptrGC( hbqt_gcAllocate_QWidget( hbqt_par_QApplication( 1 )->focusWidget(), false ) );
}

/*
 * QFont font ()
 */
HB_FUNC( QT_QAPPLICATION_FONT )
{
   hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( hbqt_par_QApplication( 1 )->font() ), true ) );
}

/*
 * QFont font ( const QWidget * widget )
 */
HB_FUNC( QT_QAPPLICATION_FONT_1 )
{
   hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( hbqt_par_QApplication( 1 )->font( hbqt_par_QWidget( 2 ) ) ), true ) );
}

/*
 * QFont font ( const char * className )
 */
HB_FUNC( QT_QAPPLICATION_FONT_2 )
{
   hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( hbqt_par_QApplication( 1 )->font( hbqt_par_char( 2 ) ) ), true ) );
}

/*
 * QFontMetrics fontMetrics ()
 */
HB_FUNC( QT_QAPPLICATION_FONTMETRICS )
{
   hb_retptrGC( hbqt_gcAllocate_QFontMetrics( new QFontMetrics( hbqt_par_QApplication( 1 )->fontMetrics() ), true ) );
}

/*
 * QSize globalStrut ()
 */
HB_FUNC( QT_QAPPLICATION_GLOBALSTRUT )
{
   hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( hbqt_par_QApplication( 1 )->globalStrut() ), true ) );
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
   hb_retptrGC( hbqt_gcAllocate_QLocale( new QLocale( hbqt_par_QApplication( 1 )->keyboardInputLocale() ), true ) );
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
   hb_retptrGC( hbqt_gcAllocate_QCursor( hbqt_par_QApplication( 1 )->overrideCursor(), false ) );
}

/*
 * QPalette palette ()
 */
HB_FUNC( QT_QAPPLICATION_PALETTE )
{
   hb_retptrGC( hbqt_gcAllocate_QPalette( new QPalette( hbqt_par_QApplication( 1 )->palette() ), true ) );
}

/*
 * QPalette palette ( const QWidget * widget )
 */
HB_FUNC( QT_QAPPLICATION_PALETTE_1 )
{
   hb_retptrGC( hbqt_gcAllocate_QPalette( new QPalette( hbqt_par_QApplication( 1 )->palette( hbqt_par_QWidget( 2 ) ) ), true ) );
}

/*
 * QPalette palette ( const char * className )
 */
HB_FUNC( QT_QAPPLICATION_PALETTE_2 )
{
   hb_retptrGC( hbqt_gcAllocate_QPalette( new QPalette( hbqt_par_QApplication( 1 )->palette( hbqt_par_char( 2 ) ) ), true ) );
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
   hbqt_par_QApplication( 1 )->setGraphicsSystem( QApplication::tr( hb_parc( 2 ) ) );
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
   hb_retptrGC( hbqt_gcAllocate_QStyle( hbqt_par_QApplication( 1 )->setStyle( QApplication::tr( hb_parc( 2 ) ) ), false ) );
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
   hb_retptrGC( hbqt_gcAllocate_QStyle( hbqt_par_QApplication( 1 )->style(), false ) );
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
   hb_retptrGC( hbqt_gcAllocate_QWidget( hbqt_par_QApplication( 1 )->topLevelAt( *hbqt_par_QPoint( 2 ) ), false ) );
}

/*
 * QWidget * topLevelAt ( int x, int y )
 */
HB_FUNC( QT_QAPPLICATION_TOPLEVELAT_1 )
{
   hb_retptrGC( hbqt_gcAllocate_QWidget( hbqt_par_QApplication( 1 )->topLevelAt( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
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
   hb_retptrGC( hbqt_gcAllocate_QWidget( hbqt_par_QApplication( 1 )->widgetAt( *hbqt_par_QPoint( 2 ) ), false ) );
}

/*
 * QWidget * widgetAt ( int x, int y )
 */
HB_FUNC( QT_QAPPLICATION_WIDGETAT_1 )
{
   hb_retptrGC( hbqt_gcAllocate_QWidget( hbqt_par_QApplication( 1 )->widgetAt( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
}

/*
 * QIcon windowIcon ()
 */
HB_FUNC( QT_QAPPLICATION_WINDOWICON )
{
   hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( hbqt_par_QApplication( 1 )->windowIcon() ), true ) );
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
   hbqt_par_QApplication( 1 )->setStyleSheet( QApplication::tr( hb_parc( 2 ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
