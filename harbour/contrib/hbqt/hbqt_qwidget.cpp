/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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

#if QT_VERSION >= 0x040500

#include <QtGui/QWidget>
#include <QtGui/QIcon>

/*----------------------------------------------------------------------*/
/*
QWidget( QWidget * parent = 0, Qt::WindowFlags f = 0 )
*/
HB_FUNC( QT_QWIDGET )
{
   hb_retptr( new QWidget( hbqt_par_QWidget( 1 ), ( Qt::WindowFlags ) hb_parni( 2 ) ) );
}

/*
bool acceptDrops () const
*/
HB_FUNC( QT_QWIDGET_ACCEPTDROPS )
{
   hb_retl( hbqt_par_QWidget( 1 )->acceptDrops() );
}

/*
QString accessibleDescription () const
*/
HB_FUNC( QT_QWIDGET_ACCESSIBLEDESCRIPTION )
{
   hb_retc( hbqt_par_QWidget( 1 )->accessibleDescription().toLatin1().data() );
}

/*
QString accessibleName () const
*/
HB_FUNC( QT_QWIDGET_ACCESSIBLENAME )
{
   hb_retc( hbqt_par_QWidget( 1 )->accessibleName().toLatin1().data() );
}

/*
void activateWindow ()
*/
HB_FUNC( QT_QWIDGET_ACTIVATEWINDOW )
{
   hbqt_par_QWidget( 1 )->activateWindow();
}

/*
void addAction( QAction * action )
*/
HB_FUNC( QT_QWIDGET_ADDACTION )
{
   hbqt_par_QWidget( 1 )->addAction( hbqt_par_QAction( 2 ) );
}

/*
void adjustSize ()
*/
HB_FUNC( QT_QWIDGET_ADJUSTSIZE )
{
   hbqt_par_QWidget( 1 )->adjustSize();
}

/*
bool autoFillBackground () const
*/
HB_FUNC( QT_QWIDGET_AUTOFILLBACKGROUND )
{
   hb_retl( hbqt_par_QWidget( 1 )->autoFillBackground() );
}

/*
QPalette::ColorRole backgroundRole () const
*/
HB_FUNC( QT_QWIDGET_BACKGROUNDROLE )
{
   hb_retni( hbqt_par_QWidget( 1 )->backgroundRole() );
}

/*
QWidget * childAt( int x, int y ) const
*/
HB_FUNC( QT_QWIDGET_CHILDAT )
{
   hb_retptr( ( QWidget* ) hbqt_par_QWidget( 1 )->childAt( hb_parni( 2 ), hb_parni( 3 ) ) );
}

/*
void clearFocus ()
*/
HB_FUNC( QT_QWIDGET_CLEARFOCUS )
{
   hbqt_par_QWidget( 1 )->clearFocus();
}

/*
void clearMask ()
*/
HB_FUNC( QT_QWIDGET_CLEARMASK )
{
   hbqt_par_QWidget( 1 )->clearMask();
}

/*
Qt::ContextMenuPolicy contextMenuPolicy () const
*/
HB_FUNC( QT_QWIDGET_CONTEXTMENUPOLICY )
{
   hb_retni( hbqt_par_QWidget( 1 )->contextMenuPolicy() );
}

/*
void ensurePolished () const
*/
HB_FUNC( QT_QWIDGET_ENSUREPOLISHED )
{
   hbqt_par_QWidget( 1 )->ensurePolished();
}

/*
Qt::FocusPolicy focusPolicy () const
*/
HB_FUNC( QT_QWIDGET_FOCUSPOLICY )
{
   hb_retni( hbqt_par_QWidget( 1 )->focusPolicy() );
}

/*
QWidget * focusProxy () const
*/
HB_FUNC( QT_QWIDGET_FOCUSPROXY )
{
   hb_retptr( ( QWidget* ) hbqt_par_QWidget( 1 )->focusProxy() );
}

/*
QWidget * focusWidget () const
*/
HB_FUNC( QT_QWIDGET_FOCUSWIDGET )
{
   hb_retptr( ( QWidget* ) hbqt_par_QWidget( 1 )->focusWidget() );
}

/*
QPalette::ColorRole foregroundRole () const
*/
HB_FUNC( QT_QWIDGET_FOREGROUNDROLE )
{
   hb_retni( hbqt_par_QWidget( 1 )->foregroundRole() );
}

/*
void grabKeyboard ()
*/
HB_FUNC( QT_QWIDGET_GRABKEYBOARD )
{
   hbqt_par_QWidget( 1 )->grabKeyboard();
}

/*
void grabMouse ()
*/
HB_FUNC( QT_QWIDGET_GRABMOUSE )
{
   hbqt_par_QWidget( 1 )->grabMouse();
}

/*
QGraphicsProxyWidget * graphicsProxyWidget () const
*/
HB_FUNC( QT_QWIDGET_GRAPHICSPROXYWIDGET )
{
   hb_retptr( ( QGraphicsProxyWidget* ) hbqt_par_QWidget( 1 )->graphicsProxyWidget() );
}

/*
bool hasFocus () const
*/
HB_FUNC( QT_QWIDGET_HASFOCUS )
{
   hb_retl( hbqt_par_QWidget( 1 )->hasFocus() );
}

/*
bool hasMouseTracking () const
*/
HB_FUNC( QT_QWIDGET_HASMOUSETRACKING )
{
   hb_retl( hbqt_par_QWidget( 1 )->hasMouseTracking() );
}

/*
int height () const
*/
HB_FUNC( QT_QWIDGET_HEIGHT )
{
   hb_retni( hbqt_par_QWidget( 1 )->height() );
}

/*
virtual int heightForWidth( int w ) const
*/
HB_FUNC( QT_QWIDGET_HEIGHTFORWIDTH )
{
   hb_retni( hbqt_par_QWidget( 1 )->heightForWidth( hb_parni( 2 ) ) );
}

/*
QInputContext * inputContext ()
*/
HB_FUNC( QT_QWIDGET_INPUTCONTEXT )
{
   hb_retptr( ( QInputContext* ) hbqt_par_QWidget( 1 )->inputContext() );
}

/*
void insertAction( QAction * before, QAction * action )
*/
HB_FUNC( QT_QWIDGET_INSERTACTION )
{
   hbqt_par_QWidget( 1 )->insertAction( hbqt_par_QAction( 2 ), hbqt_par_QAction( 3 ) );
}

/*
bool isActiveWindow () const
*/
HB_FUNC( QT_QWIDGET_ISACTIVEWINDOW )
{
   hb_retl( hbqt_par_QWidget( 1 )->isActiveWindow() );
}

/*
bool isAncestorOf( const QWidget * child ) const
*/
HB_FUNC( QT_QWIDGET_ISANCESTOROF )
{
   hb_retl( hbqt_par_QWidget( 1 )->isAncestorOf( hbqt_par_QWidget( 2 ) ) );
}

/*
bool isEnabled () const
*/
HB_FUNC( QT_QWIDGET_ISENABLED )
{
   hb_retl( hbqt_par_QWidget( 1 )->isEnabled() );
}

/*
bool isEnabledTo( QWidget * ancestor ) const
*/
HB_FUNC( QT_QWIDGET_ISENABLEDTO )
{
   hb_retl( hbqt_par_QWidget( 1 )->isEnabledTo( hbqt_par_QWidget( 2 ) ) );
}

/*
bool isFullScreen () const
*/
HB_FUNC( QT_QWIDGET_ISFULLSCREEN )
{
   hb_retl( hbqt_par_QWidget( 1 )->isFullScreen() );
}

/*
bool isHidden () const
*/
HB_FUNC( QT_QWIDGET_ISHIDDEN )
{
   hb_retl( hbqt_par_QWidget( 1 )->isHidden() );
}

/*
bool isMaximized () const
*/
HB_FUNC( QT_QWIDGET_ISMAXIMIZED )
{
   hb_retl( hbqt_par_QWidget( 1 )->isMaximized() );
}

/*
bool isMinimized () const
*/
HB_FUNC( QT_QWIDGET_ISMINIMIZED )
{
   hb_retl( hbqt_par_QWidget( 1 )->isMinimized() );
}

/*
bool isModal () const
*/
HB_FUNC( QT_QWIDGET_ISMODAL )
{
   hb_retl( hbqt_par_QWidget( 1 )->isModal() );
}

/*
bool isVisible () const
*/
HB_FUNC( QT_QWIDGET_ISVISIBLE )
{
   hb_retl( hbqt_par_QWidget( 1 )->isVisible() );
}

/*
bool isVisibleTo( QWidget * ancestor ) const
*/
HB_FUNC( QT_QWIDGET_ISVISIBLETO )
{
   hb_retl( hbqt_par_QWidget( 1 )->isVisibleTo( hbqt_par_QWidget( 2 ) ) );
}

/*
bool isWindow () const
*/
HB_FUNC( QT_QWIDGET_ISWINDOW )
{
   hb_retl( hbqt_par_QWidget( 1 )->isWindow() );
}

/*
bool isWindowModified () const
*/
HB_FUNC( QT_QWIDGET_ISWINDOWMODIFIED )
{
   hb_retl( hbqt_par_QWidget( 1 )->isWindowModified() );
}

/*
QLayout * layout () const
*/
HB_FUNC( QT_QWIDGET_LAYOUT )
{
   hb_retptr( ( QLayout* ) hbqt_par_QWidget( 1 )->layout() );
}

/*
Qt::LayoutDirection layoutDirection () const
*/
HB_FUNC( QT_QWIDGET_LAYOUTDIRECTION )
{
   hb_retni( hbqt_par_QWidget( 1 )->layoutDirection() );
}

/*
int maximumHeight () const
*/
HB_FUNC( QT_QWIDGET_MAXIMUMHEIGHT )
{
   hb_retni( hbqt_par_QWidget( 1 )->maximumHeight() );
}

/*
int maximumWidth () const
*/
HB_FUNC( QT_QWIDGET_MAXIMUMWIDTH )
{
   hb_retni( hbqt_par_QWidget( 1 )->maximumWidth() );
}

/*
int minimumHeight () const
*/
HB_FUNC( QT_QWIDGET_MINIMUMHEIGHT )
{
   hb_retni( hbqt_par_QWidget( 1 )->minimumHeight() );
}

/*
int minimumWidth () const
*/
HB_FUNC( QT_QWIDGET_MINIMUMWIDTH )
{
   hb_retni( hbqt_par_QWidget( 1 )->minimumWidth() );
}

/*
void move( int x, int y )
*/
HB_FUNC( QT_QWIDGET_MOVE )
{
   hbqt_par_QWidget( 1 )->move( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
QWidget * nativeParentWidget () const
*/
HB_FUNC( QT_QWIDGET_NATIVEPARENTWIDGET )
{
   hb_retptr( ( QWidget* ) hbqt_par_QWidget( 1 )->nativeParentWidget() );
}

/*
QWidget * nextInFocusChain () const
*/
HB_FUNC( QT_QWIDGET_NEXTINFOCUSCHAIN )
{
   hb_retptr( ( QWidget* ) hbqt_par_QWidget( 1 )->nextInFocusChain() );
}

/*
virtual QPaintEngine * paintEngine () const
*/
HB_FUNC( QT_QWIDGET_PAINTENGINE )
{
   hb_retptr( ( QPaintEngine* ) hbqt_par_QWidget( 1 )->paintEngine() );
}

/*
QWidget * parentWidget () const
*/
HB_FUNC( QT_QWIDGET_PARENTWIDGET )
{
   hb_retptr( ( QWidget* ) hbqt_par_QWidget( 1 )->parentWidget() );
}

/*
void releaseKeyboard ()
*/
HB_FUNC( QT_QWIDGET_RELEASEKEYBOARD )
{
   hbqt_par_QWidget( 1 )->releaseKeyboard();
}

/*
void releaseMouse ()
*/
HB_FUNC( QT_QWIDGET_RELEASEMOUSE )
{
   hbqt_par_QWidget( 1 )->releaseMouse();
}

/*
void releaseShortcut( int id )
*/
HB_FUNC( QT_QWIDGET_RELEASESHORTCUT )
{
   hbqt_par_QWidget( 1 )->releaseShortcut( hb_parni( 2 ) );
}

/*
void removeAction( QAction * action )
*/
HB_FUNC( QT_QWIDGET_REMOVEACTION )
{
   hbqt_par_QWidget( 1 )->removeAction( hbqt_par_QAction( 2 ) );
}

/*
void resize( int w, int h )
*/
HB_FUNC( QT_QWIDGET_RESIZE )
{
   hbqt_par_QWidget( 1 )->resize( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
void scroll( int dx, int dy )
*/
HB_FUNC( QT_QWIDGET_SCROLL )
{
   hbqt_par_QWidget( 1 )->scroll( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
void setAcceptDrops( bool on )
*/
HB_FUNC( QT_QWIDGET_SETACCEPTDROPS )
{
   hbqt_par_QWidget( 1 )->setAcceptDrops( hb_parl( 2 ) );
}

/*
void setAccessibleDescription( const QString & description )
*/
HB_FUNC( QT_QWIDGET_SETACCESSIBLEDESCRIPTION )
{
   hbqt_par_QWidget( 1 )->setAccessibleDescription( hbqt_par_QString( 2 ) );
}

/*
void setAccessibleName( const QString & name )
*/
HB_FUNC( QT_QWIDGET_SETACCESSIBLENAME )
{
   hbqt_par_QWidget( 1 )->setAccessibleName( hbqt_par_QString( 2 ) );
}

/*
void setAttribute( Qt::WidgetAttribute attribute, bool on = true )
*/
HB_FUNC( QT_QWIDGET_SETATTRIBUTE )
{
   hbqt_par_QWidget( 1 )->setAttribute( ( Qt::WidgetAttribute ) hb_parni( 2 ), hb_parl( 3 ) );
}

/*
void setAutoFillBackground( bool enabled )
*/
HB_FUNC( QT_QWIDGET_SETAUTOFILLBACKGROUND )
{
   hbqt_par_QWidget( 1 )->setAutoFillBackground( hb_parl( 2 ) );
}

/*
void setBackgroundRole( QPalette::ColorRole role )
*/
HB_FUNC( QT_QWIDGET_SETBACKGROUNDROLE )
{
   hbqt_par_QWidget( 1 )->setBackgroundRole( ( QPalette::ColorRole ) hb_parni( 2 ) );
}

/*
void setBaseSize( int basew, int baseh )
*/
HB_FUNC( QT_QWIDGET_SETBASESIZE )
{
   hbqt_par_QWidget( 1 )->setBaseSize( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
void setContentsMargins( int left, int top, int right, int bottom )
*/
HB_FUNC( QT_QWIDGET_SETCONTENTSMARGINS )
{
   hbqt_par_QWidget( 1 )->setContentsMargins( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/*
void setContextMenuPolicy( Qt::ContextMenuPolicy policy )
*/
HB_FUNC( QT_QWIDGET_SETCONTEXTMENUPOLICY )
{
   hbqt_par_QWidget( 1 )->setContextMenuPolicy( ( Qt::ContextMenuPolicy ) hb_parni( 2 ) );
}

/*
void setFixedHeight( int h )
*/
HB_FUNC( QT_QWIDGET_SETFIXEDHEIGHT )
{
   hbqt_par_QWidget( 1 )->setFixedHeight( hb_parni( 2 ) );
}

/*
void setFixedSize( int w, int h )
*/
HB_FUNC( QT_QWIDGET_SETFIXEDSIZE )
{
   hbqt_par_QWidget( 1 )->setFixedSize( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
void setFixedWidth( int w )
*/
HB_FUNC( QT_QWIDGET_SETFIXEDWIDTH )
{
   hbqt_par_QWidget( 1 )->setFixedWidth( hb_parni( 2 ) );
}

/*
void setFocusPolicy( Qt::FocusPolicy policy )
*/
HB_FUNC( QT_QWIDGET_SETFOCUSPOLICY )
{
   hbqt_par_QWidget( 1 )->setFocusPolicy( ( Qt::FocusPolicy ) hb_parni( 2 ) );
}

/*
void setFocusProxy( QWidget * w )
*/
HB_FUNC( QT_QWIDGET_SETFOCUSPROXY )
{
   hbqt_par_QWidget( 1 )->setFocusProxy( hbqt_par_QWidget( 2 ) );
}

/*
void setForegroundRole( QPalette::ColorRole role )
*/
HB_FUNC( QT_QWIDGET_SETFOREGROUNDROLE )
{
   hbqt_par_QWidget( 1 )->setForegroundRole( ( QPalette::ColorRole ) hb_parni( 2 ) );
}

/*
void setGeometry( int x, int y, int w, int h )
*/
HB_FUNC( QT_QWIDGET_SETGEOMETRY )
{
   hbqt_par_QWidget( 1 )->setGeometry( hb_parni( 2 ), hb_parni( 2 ), hb_parni( 2 ), hb_parni( 2 ) );
}

/*
void setInputContext( QInputContext * context )
*/
HB_FUNC( QT_QWIDGET_SETINPUTCONTEXT )
{
   hbqt_par_QWidget( 1 )->setInputContext( hbqt_par_QInputContext( 2 ) );
}

/*
void setLayout( QLayout * layout )
*/
HB_FUNC( QT_QWIDGET_SETLAYOUT )
{
   hbqt_par_QWidget( 1 )->setLayout( hbqt_par_QLayout( 2 ) );
}

/*
void setLayoutDirection( Qt::LayoutDirection direction )
*/
HB_FUNC( QT_QWIDGET_SETLAYOUTDIRECTION )
{
   hbqt_par_QWidget( 1 )->setLayoutDirection( ( Qt::LayoutDirection ) hb_parni( 2 ) );
}

/*
void setMaximumHeight( int maxh )
*/
HB_FUNC( QT_QWIDGET_SETMAXIMUMHEIGHT )
{
   hbqt_par_QWidget( 1 )->setMaximumHeight( hb_parni( 2 ) );
}

/*
void setMaximumSize( int maxw, int maxh )
*/
HB_FUNC( QT_QWIDGET_SETMAXIMUMSIZE )
{
   hbqt_par_QWidget( 1 )->setMaximumSize( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
void setMaximumWidth( int maxw )
*/
HB_FUNC( QT_QWIDGET_SETMAXIMUMWIDTH )
{
   hbqt_par_QWidget( 1 )->setMaximumWidth( hb_parni( 2 ) );
}

/*
void setMinimumHeight( int minh )
*/
HB_FUNC( QT_QWIDGET_SETMINIMUMHEIGHT )
{
   hbqt_par_QWidget( 1 )->setMinimumHeight( hb_parni( 2 ) );
}

/*
void setMinimumSize( int minw, int minh )
*/
HB_FUNC( QT_QWIDGET_SETMINIMUMSIZE )
{
   hbqt_par_QWidget( 1 )->setMinimumSize( hb_parni( 2 ), hb_parni( 2 ) );
}

/*
void setMinimumWidth( int minw )
*/
HB_FUNC( QT_QWIDGET_SETMINIMUMWIDTH )
{
   hbqt_par_QWidget( 1 )->setMinimumWidth( hb_parni( 2 ) );
}

/*
void setMouseTracking( bool enable )
*/
HB_FUNC( QT_QWIDGET_SETMOUSETRACKING )
{
   hbqt_par_QWidget( 1 )->setMouseTracking( hb_parl( 2 ) );
}

/*
void setParent( QWidget * parent )
*/
HB_FUNC( QT_QWIDGET_SETPARENT )
{
   hbqt_par_QWidget( 1 )->setParent( hbqt_par_QWidget( 2 ) );
}

/*
void setShortcutAutoRepeat( int id, bool enable = true )
*/
HB_FUNC( QT_QWIDGET_SETSHORTCUTAUTOREPEAT )
{
   hbqt_par_QWidget( 1 )->setShortcutAutoRepeat( hb_parni( 2 ), hb_parl( 3 ) );
}

/*
void setShortcutEnabled( int id, bool enable = true )
*/
HB_FUNC( QT_QWIDGET_SETSHORTCUTENABLED )
{
   hbqt_par_QWidget( 1 )->setShortcutEnabled( hb_parni( 2 ), hb_parl( 3 ) );
}

/*
void setSizeIncrement( int w, int h )
*/
HB_FUNC( QT_QWIDGET_SETSIZEINCREMENT )
{
   hbqt_par_QWidget( 1 )->setSizeIncrement( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
void setSizePolicy( QSizePolicy::Policy horizontal, QSizePolicy::Policy vertical )
*/
HB_FUNC( QT_QWIDGET_SETSIZEPOLICY )
{
   hbqt_par_QWidget( 1 )->setSizePolicy( ( QSizePolicy::Policy ) hb_parni( 2 ),
                                         ( QSizePolicy::Policy ) hb_parni( 3 ) );
}

/*
void setStatusTip( const QString & )
*/
HB_FUNC( QT_QWIDGET_SETSTATUSTIP )
{
   hbqt_par_QWidget( 1 )->setStatusTip( hbqt_par_QString( 2 ) );
}

/*
void setStyle( QStyle * style )
*/
HB_FUNC( QT_QWIDGET_SETSTYLE )
{
   hbqt_par_QWidget( 1 )->setStyle( hbqt_par_QStyle( 2 ) );
}

/*
void setToolTip( const QString & )
*/
HB_FUNC( QT_QWIDGET_SETTOOLTIP )
{
   hbqt_par_QWidget( 1 )->setToolTip( hbqt_par_QString( 2 ) );
}

/*
void setUpdatesEnabled( bool enable )
*/
HB_FUNC( QT_QWIDGET_SETUPDATESENABLED )
{
   hbqt_par_QWidget( 1 )->setUpdatesEnabled( hb_parl( 2 ) );
}

/*
void setWhatsThis( const QString & )
*/
HB_FUNC( QT_QWIDGET_SETWHATSTHIS )
{
   hbqt_par_QWidget( 1 )->setWhatsThis( hbqt_par_QString( 2 ) );
}

/*
void setWindowFilePath( const QString & filePath )
*/
HB_FUNC( QT_QWIDGET_SETWINDOWFILEPATH )
{
   hbqt_par_QWidget( 1 )->setWindowFilePath( hbqt_par_QString( 2 ) );
}

/*
void setWindowFlags( Qt::WindowFlags type )
*/
HB_FUNC( QT_QWIDGET_SETWINDOWFLAGS )
{
   hbqt_par_QWidget( 1 )->setWindowFlags(  (Qt::WindowFlags) hb_parni( 2 ) );
}

/*
void setWindowIcon( const QIcon & icon )
*/
HB_FUNC( QT_QWIDGET_SETWINDOWICON )
{
   hbqt_par_QWidget( 1 )->setWindowIcon( QIcon( hbqt_par_QString( 2 ) ) );
}

/*
void setWindowIconText( const QString & )
*/
HB_FUNC( QT_QWIDGET_SETWINDOWICONTEXT )
{
   hbqt_par_QWidget( 1 )->setWindowIconText( hbqt_par_QString( 2 ) );
}

/*
void setWindowModality( Qt::WindowModality windowModality )
*/
HB_FUNC( QT_QWIDGET_SETWINDOWMODALITY )
{
   hbqt_par_QWidget( 1 )->setWindowModality(  (Qt::WindowModality) hb_parni( 2 ) );
}

/*
void setWindowRole( const QString & role )
*/
HB_FUNC( QT_QWIDGET_SETWINDOWROLE )
{
   hbqt_par_QWidget( 1 )->setWindowRole( hbqt_par_QString( 2 ) );
}

/*
void setWindowState( Qt::WindowStates windowState )
*/
HB_FUNC( QT_QWIDGET_SETWINDOWSTATE )
{
   hbqt_par_QWidget( 1 )->setWindowState(  (Qt::WindowStates) hb_parni( 2 ) );
}

/*
void setWindowSurface( QWindowSurface * surface )   (preliminary)
*/
HB_FUNC( QT_QWIDGET_SETWINDOWSURFACE )
{
   hbqt_par_QWidget( 1 )->setWindowSurface( hbqt_par_QWindowSurface( 2 ) );
}

/*
void stackUnder( QWidget * w )
*/
HB_FUNC( QT_QWIDGET_STACKUNDER )
{
   hbqt_par_QWidget( 1 )->stackUnder( hbqt_par_QWidget( 2 ) );
}

/*
QString statusTip () const
*/
HB_FUNC( QT_QWIDGET_STATUSTIP )
{
   hb_retc( hbqt_par_QWidget( 1 )->statusTip().toLatin1().data() );
}

/*
QStyle * style () const
*/
HB_FUNC( QT_QWIDGET_STYLE )
{
   hb_retptr( ( QStyle* ) hbqt_par_QWidget( 1 )->style() );
}

/*
QString styleSheet () const
*/
HB_FUNC( QT_QWIDGET_STYLESHEET )
{
   hb_retc( hbqt_par_QWidget( 1 )->styleSheet().toLatin1().data() );
}

/*
bool testAttribute( Qt::WidgetAttribute attribute ) const
*/
HB_FUNC( QT_QWIDGET_TESTATTRIBUTE )
{
   hb_retl( hbqt_par_QWidget( 1 )->testAttribute(  (Qt::WidgetAttribute) hb_parni( 2 ) ) );
}

/*
QString toolTip () const
*/
HB_FUNC( QT_QWIDGET_TOOLTIP )
{
   hb_retc( hbqt_par_QWidget( 1 )->toolTip().toLatin1().data() );
}

/*
bool underMouse () const
*/
HB_FUNC( QT_QWIDGET_UNDERMOUSE )
{
   hb_retl( hbqt_par_QWidget( 1 )->underMouse() );
}

/*
void unsetCursor ()
*/
HB_FUNC( QT_QWIDGET_UNSETCURSOR )
{
   hbqt_par_QWidget( 1 )->unsetCursor();
}

/*
void unsetLayoutDirection ()
*/
HB_FUNC( QT_QWIDGET_UNSETLAYOUTDIRECTION )
{
   hbqt_par_QWidget( 1 )->unsetLayoutDirection();
}

/*
void unsetLocale ()
*/
HB_FUNC( QT_QWIDGET_UNSETLOCALE )
{
   hbqt_par_QWidget( 1 )->unsetLocale();
}

/*
void update( int x, int y, int w, int h )
*/
HB_FUNC( QT_QWIDGET_UPDATE_1 )
{
   hbqt_par_QWidget( 1 )->update( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/*
void updateGeometry ()
*/
HB_FUNC( QT_QWIDGET_UPDATEGEOMETRY )
{
   hbqt_par_QWidget( 1 )->updateGeometry();
}

/*
bool updatesEnabled () const
*/
HB_FUNC( QT_QWIDGET_UPDATESENABLED )
{
   hb_retl( hbqt_par_QWidget( 1 )->updatesEnabled() );
}

/*
QString whatsThis () const
*/
HB_FUNC( QT_QWIDGET_WHATSTHIS )
{
   hb_retc( hbqt_par_QWidget( 1 )->whatsThis().toLatin1().data() );
}

/*
int width () const
*/
HB_FUNC( QT_QWIDGET_WIDTH )
{
   hb_retni( hbqt_par_QWidget( 1 )->width() );
}

/*
QWidget * window () const
*/
HB_FUNC( QT_QWIDGET_WINDOW )
{
   hb_retptr( ( QWidget* ) hbqt_par_QWidget( 1 )->window() );
}

/*
QString windowFilePath () const
*/
HB_FUNC( QT_QWIDGET_WINDOWFILEPATH )
{
   hb_retc( hbqt_par_QWidget( 1 )->windowFilePath().toLatin1().data() );
}

/*
Qt::WindowFlags windowFlags () const
*/
HB_FUNC( QT_QWIDGET_WINDOWFLAGS )
{
   hb_retni( hbqt_par_QWidget( 1 )->windowFlags() );
}

/*
QString windowIconText () const
*/
HB_FUNC( QT_QWIDGET_WINDOWICONTEXT )
{
   hb_retc( hbqt_par_QWidget( 1 )->windowIconText().toLatin1().data() );
}

/*
Qt::WindowModality windowModality () const
*/
HB_FUNC( QT_QWIDGET_WINDOWMODALITY )
{
   hb_retni( hbqt_par_QWidget( 1 )->windowModality() );
}

/*
QString windowRole () const
*/
HB_FUNC( QT_QWIDGET_WINDOWROLE )
{
   hb_retc( hbqt_par_QWidget( 1 )->windowRole().toLatin1().data() );
}

/*
Qt::WindowStates windowState () const
*/
HB_FUNC( QT_QWIDGET_WINDOWSTATE )
{
   hb_retni( hbqt_par_QWidget( 1 )->windowState() );
}

/*
QWindowSurface * windowSurface () const   (preliminary)
*/
HB_FUNC( QT_QWIDGET_WINDOWSURFACE )
{
   hb_retptr( ( QWindowSurface* ) hbqt_par_QWidget( 1 )->windowSurface() );
}

/*
QString windowTitle () const
*/
HB_FUNC( QT_QWIDGET_WINDOWTITLE )
{
   hb_retc( hbqt_par_QWidget( 1 )->windowTitle().toLatin1().data() );
}

/*
Qt::WindowType windowType () const
*/
HB_FUNC( QT_QWIDGET_WINDOWTYPE )
{
   hb_retni( hbqt_par_QWidget( 1 )->windowType() );
}

/*
int x () const
*/
HB_FUNC( QT_QWIDGET_X )
{
   hb_retni( hbqt_par_QWidget( 1 )->x() );
}

/*
int y () const
*/
HB_FUNC( QT_QWIDGET_Y )
{
   hb_retni( hbqt_par_QWidget( 1 )->y() );
}

/*
bool close ()
*/
HB_FUNC( QT_QWIDGET_CLOSE )
{
   hb_retl( hbqt_par_QWidget( 1 )->close() );
}

/*
void hide ()
*/
HB_FUNC( QT_QWIDGET_HIDE )
{
   hbqt_par_QWidget( 1 )->hide();
}

/*
void lower ()
*/
HB_FUNC( QT_QWIDGET_LOWER )
{
   hbqt_par_QWidget( 1 )->lower();
}

/*
void raise ()
*/
HB_FUNC( QT_QWIDGET_RAISE )
{
   hbqt_par_QWidget( 1 )->raise();
}

/*
void repaint ()
*/
HB_FUNC( QT_QWIDGET_REPAINT )
{
   hbqt_par_QWidget( 1 )->repaint();
}

/*
void setDisabled( bool disable )
*/
HB_FUNC( QT_QWIDGET_SETDISABLED )
{
   hbqt_par_QWidget( 1 )->setDisabled( hb_parl( 2 ) );
}

/*
void setEnabled( bool )
*/
HB_FUNC( QT_QWIDGET_SETENABLED )
{
   hbqt_par_QWidget( 1 )->setEnabled( hb_parl( 2 ) );
}

/*
void setFocus ()
*/
HB_FUNC( QT_QWIDGET_SETFOCUS )
{
   hbqt_par_QWidget( 1 )->setFocus();
}

/*
void setHidden( bool hidden )
*/
HB_FUNC( QT_QWIDGET_SETHIDDEN )
{
   hbqt_par_QWidget( 1 )->setHidden( hb_parl( 2 ) );
}

/*
void setStyleSheet( const QString & styleSheet )
*/
HB_FUNC( QT_QWIDGET_SETSTYLESHEET )
{
   hbqt_par_QWidget( 1 )->setStyleSheet( hbqt_par_QString( 2 ) );
}

/*
virtual void setVisible( bool visible )
*/
HB_FUNC( QT_QWIDGET_SETVISIBLE )
{
   hbqt_par_QWidget( 1 )->setVisible( hb_parl( 2 ) );
}

/*
void setWindowModified( bool )
*/
HB_FUNC( QT_QWIDGET_SETWINDOWMODIFIED )
{
   hbqt_par_QWidget( 1 )->setWindowModified( hb_parl( 2 ) );
}

/*
void setWindowTitle( const QString & )
*/
HB_FUNC( QT_QWIDGET_SETWINDOWTITLE )
{
   hbqt_par_QWidget( 1 )->setWindowTitle( hbqt_par_QString( 2 ) );
}

/*
void show ()
*/
HB_FUNC( QT_QWIDGET_SHOW )
{
   hbqt_par_QWidget( 1 )->show();
}

/*
void showFullScreen ()
*/
HB_FUNC( QT_QWIDGET_SHOWFULLSCREEN )
{
   hbqt_par_QWidget( 1 )->showFullScreen();
}

/*
void showMaximized ()
*/
HB_FUNC( QT_QWIDGET_SHOWMAXIMIZED )
{
   hbqt_par_QWidget( 1 )->showMaximized();
}

/*
void showMinimized ()
*/
HB_FUNC( QT_QWIDGET_SHOWMINIMIZED )
{
   hbqt_par_QWidget( 1 )->showMinimized();
}

/*
void showNormal ()
*/
HB_FUNC( QT_QWIDGET_SHOWNORMAL )
{
   hbqt_par_QWidget( 1 )->showNormal();
}

/*
void update ()
*/
HB_FUNC( QT_QWIDGET_UPDATE )
{
   hbqt_par_QWidget( 1 )->update();
}

/*----------------------------------------------------------------------*/
#endif
