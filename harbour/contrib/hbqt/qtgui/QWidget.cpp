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
#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum RenderFlag { DrawWindowBackground, DrawChildren, IgnoreMask }
 *  # From QPaintDevice : Parent Class
 *  enum PaintDeviceMetric { PdmWidth, PdmHeight, PdmWidthMM, PdmHeightMM, ..., PdmPhysicalDpiY }
 */

/*
 *  Constructed[ 211/229 [ 92.14% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QList<QAction *> actions () const
 *  void addActions ( QList<QAction *> actions )
 *  void insertActions ( QAction * before, QList<QAction *> actions )
 *
 *  *** Commented out protos which construct fine but do not compile ***
 *
 *  // WId effectiveWinId () const
 *  // virtual HDC getDC () const
 *  // bool hasEditFocus () const
 *  // Qt::HANDLE macCGHandle () const
 *  // Qt::HANDLE macQDHandle () const
 *  // virtual void releaseDC ( HDC hdc ) const
 *  //void render ( QPaintDevice * target, const QPoint & targetOffset = QPoint(), const QRegion & sourceRegion = QRegion(), RenderFlags renderFlags = RenderFlags( DrawWindowBackground | DrawChildren ) )
 *  //void render ( QPainter * painter, const QPoint & targetOffset = QPoint(), const QRegion & sourceRegion = QRegion(), RenderFlags renderFlags = RenderFlags( DrawWindowBackground | DrawChildren ) )
 *  // void setEditFocus ( bool enable )
 *  //void setSizePolicy ( QSizePolicy )
 *  // void setWindowSurface ( QWindowSurface * surface )
 *  //WId winId () const
 *  // QWindowSurface * windowSurface () const   (preliminary)
 *  //Qt::HANDLE x11PictureHandle () const
 *  //QWidget * find ( WId id )
 */

#include <QtCore/QPointer>

#include <QtGui/QWidget>
#include <QtGui/QIcon>
#include <QtCore/QVariant>
#include <QtCore/QLocale>


/*
 * QWidget( QWidget * parent = 0, Qt::WindowFlags f = 0 )
 * ~QWidget ()
 */

typedef struct
{
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< QWidget > pq;
} QGC_POINTER_QWidget;

QT_G_FUNC( hbqt_gcRelease_QWidget )
{
   QGC_POINTER_QWidget * p = ( QGC_POINTER_QWidget * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QWidget                      p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QWidget                     ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         switch( hbqt_get_object_release_method() )
         {
         case HBQT_RELEASE_WITH_DELETE:
            delete ( ( QWidget * ) p->ph );
            break;
         case HBQT_RELEASE_WITH_DESTRUTOR:
            ( ( QWidget * ) p->ph )->~QWidget();
            break;
         case HBQT_RELEASE_WITH_DELETE_LATER:
            ( ( QWidget * ) p->ph )->deleteLater();
            break;
         }
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "hbqt_gcRelease_QWidget                     Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "NO hbqt_gcRelease_QWidget                     Object Name Missing!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL hbqt_gcRelease_QWidget                     Object Already deleted!" ) );
   }
}

void * hbqt_gcAllocate_QWidget( void * pObj )
{
   QGC_POINTER_QWidget * p = ( QGC_POINTER_QWidget * ) hb_gcAllocate( sizeof( QGC_POINTER_QWidget ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->func = hbqt_gcRelease_QWidget;
   new( & p->pq ) QPointer< QWidget >( ( QWidget * ) pObj );
   HB_TRACE( HB_TR_DEBUG, ( "          new_QWidget                     %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return p;
}

HB_FUNC( QT_QWIDGET )
{
   void * pObj = NULL;

   pObj = new QWidget( hbqt_par_QWidget( 1 ), ( Qt::WindowFlags ) hb_parni( 2 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QWidget( pObj ) );
}
/*
 * bool acceptDrops () const
 */
HB_FUNC( QT_QWIDGET_ACCEPTDROPS )
{
   hb_retl( hbqt_par_QWidget( 1 )->acceptDrops() );
}

/*
 * QString accessibleDescription () const
 */
HB_FUNC( QT_QWIDGET_ACCESSIBLEDESCRIPTION )
{
   hb_retc( hbqt_par_QWidget( 1 )->accessibleDescription().toAscii().data() );
}

/*
 * QString accessibleName () const
 */
HB_FUNC( QT_QWIDGET_ACCESSIBLENAME )
{
   hb_retc( hbqt_par_QWidget( 1 )->accessibleName().toAscii().data() );
}

/*
 * void activateWindow ()
 */
HB_FUNC( QT_QWIDGET_ACTIVATEWINDOW )
{
   hbqt_par_QWidget( 1 )->activateWindow();
}

/*
 * void addAction ( QAction * action )
 */
HB_FUNC( QT_QWIDGET_ADDACTION )
{
   hbqt_par_QWidget( 1 )->addAction( hbqt_par_QAction( 2 ) );
}

/*
 * void adjustSize ()
 */
HB_FUNC( QT_QWIDGET_ADJUSTSIZE )
{
   hbqt_par_QWidget( 1 )->adjustSize();
}

/*
 * bool autoFillBackground () const
 */
HB_FUNC( QT_QWIDGET_AUTOFILLBACKGROUND )
{
   hb_retl( hbqt_par_QWidget( 1 )->autoFillBackground() );
}

/*
 * QPalette::ColorRole backgroundRole () const
 */
HB_FUNC( QT_QWIDGET_BACKGROUNDROLE )
{
   hb_retni( ( QPalette::ColorRole ) hbqt_par_QWidget( 1 )->backgroundRole() );
}

/*
 * QSize baseSize () const
 */
HB_FUNC( QT_QWIDGET_BASESIZE )
{
   hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( hbqt_par_QWidget( 1 )->baseSize() ) ) );
}

/*
 * QWidget * childAt ( int x, int y ) const
 */
HB_FUNC( QT_QWIDGET_CHILDAT )
{
   hb_retptr( ( QWidget* ) hbqt_par_QWidget( 1 )->childAt( hb_parni( 2 ), hb_parni( 3 ) ) );
}

/*
 * QWidget * childAt ( const QPoint & p ) const
 */
HB_FUNC( QT_QWIDGET_CHILDAT_1 )
{
   hb_retptr( ( QWidget* ) hbqt_par_QWidget( 1 )->childAt( *hbqt_par_QPoint( 2 ) ) );
}

/*
 * QRect childrenRect () const
 */
HB_FUNC( QT_QWIDGET_CHILDRENRECT )
{
   hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( hbqt_par_QWidget( 1 )->childrenRect() ) ) );
}

/*
 * QRegion childrenRegion () const
 */
HB_FUNC( QT_QWIDGET_CHILDRENREGION )
{
   hb_retptrGC( hbqt_gcAllocate_QRegion( new QRegion( hbqt_par_QWidget( 1 )->childrenRegion() ) ) );
}

/*
 * void clearFocus ()
 */
HB_FUNC( QT_QWIDGET_CLEARFOCUS )
{
   hbqt_par_QWidget( 1 )->clearFocus();
}

/*
 * void clearMask ()
 */
HB_FUNC( QT_QWIDGET_CLEARMASK )
{
   hbqt_par_QWidget( 1 )->clearMask();
}

/*
 * QRect contentsRect () const
 */
HB_FUNC( QT_QWIDGET_CONTENTSRECT )
{
   hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( hbqt_par_QWidget( 1 )->contentsRect() ) ) );
}

/*
 * Qt::ContextMenuPolicy contextMenuPolicy () const
 */
HB_FUNC( QT_QWIDGET_CONTEXTMENUPOLICY )
{
   hb_retni( ( Qt::ContextMenuPolicy ) hbqt_par_QWidget( 1 )->contextMenuPolicy() );
}

/*
 * QCursor cursor () const
 */
HB_FUNC( QT_QWIDGET_CURSOR )
{
   hb_retptrGC( hbqt_gcAllocate_QCursor( new QCursor( hbqt_par_QWidget( 1 )->cursor() ) ) );
}

/*
 * void ensurePolished () const
 */
HB_FUNC( QT_QWIDGET_ENSUREPOLISHED )
{
   hbqt_par_QWidget( 1 )->ensurePolished();
}

/*
 * Qt::FocusPolicy focusPolicy () const
 */
HB_FUNC( QT_QWIDGET_FOCUSPOLICY )
{
   hb_retni( ( Qt::FocusPolicy ) hbqt_par_QWidget( 1 )->focusPolicy() );
}

/*
 * QWidget * focusProxy () const
 */
HB_FUNC( QT_QWIDGET_FOCUSPROXY )
{
   hb_retptr( ( QWidget* ) hbqt_par_QWidget( 1 )->focusProxy() );
}

/*
 * QWidget * focusWidget () const
 */
HB_FUNC( QT_QWIDGET_FOCUSWIDGET )
{
   hb_retptr( ( QWidget* ) hbqt_par_QWidget( 1 )->focusWidget() );
}

/*
 * const QFont & font () const
 */
HB_FUNC( QT_QWIDGET_FONT )
{
   hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( hbqt_par_QWidget( 1 )->font() ) ) );
}

/*
 * QFontInfo fontInfo () const
 */
HB_FUNC( QT_QWIDGET_FONTINFO )
{
   hb_retptrGC( hbqt_gcAllocate_QFontInfo( new QFontInfo( hbqt_par_QWidget( 1 )->fontInfo() ) ) );
}

/*
 * QFontMetrics fontMetrics () const
 */
HB_FUNC( QT_QWIDGET_FONTMETRICS )
{
   hb_retptrGC( hbqt_gcAllocate_QFontMetrics( new QFontMetrics( hbqt_par_QWidget( 1 )->fontMetrics() ) ) );
}

/*
 * QPalette::ColorRole foregroundRole () const
 */
HB_FUNC( QT_QWIDGET_FOREGROUNDROLE )
{
   hb_retni( ( QPalette::ColorRole ) hbqt_par_QWidget( 1 )->foregroundRole() );
}

/*
 * QRect frameGeometry () const
 */
HB_FUNC( QT_QWIDGET_FRAMEGEOMETRY )
{
   hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( hbqt_par_QWidget( 1 )->frameGeometry() ) ) );
}

/*
 * QSize frameSize () const
 */
HB_FUNC( QT_QWIDGET_FRAMESIZE )
{
   hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( hbqt_par_QWidget( 1 )->frameSize() ) ) );
}

/*
 * const QRect & geometry () const
 */
HB_FUNC( QT_QWIDGET_GEOMETRY )
{
   hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( hbqt_par_QWidget( 1 )->geometry() ) ) );
}

/*
 * void getContentsMargins ( int * left, int * top, int * right, int * bottom ) const
 */
HB_FUNC( QT_QWIDGET_GETCONTENTSMARGINS )
{
   int iLeft = 0;
   int iTop = 0;
   int iRight = 0;
   int iBottom = 0;

   hbqt_par_QWidget( 1 )->getContentsMargins( &iLeft, &iTop, &iRight, &iBottom );

   hb_storni( iLeft, 2 );
   hb_storni( iTop, 3 );
   hb_storni( iRight, 4 );
   hb_storni( iBottom, 5 );
}

/*
 * void grabKeyboard ()
 */
HB_FUNC( QT_QWIDGET_GRABKEYBOARD )
{
   hbqt_par_QWidget( 1 )->grabKeyboard();
}

/*
 * void grabMouse ()
 */
HB_FUNC( QT_QWIDGET_GRABMOUSE )
{
   hbqt_par_QWidget( 1 )->grabMouse();
}

/*
 * void grabMouse ( const QCursor & cursor )
 */
HB_FUNC( QT_QWIDGET_GRABMOUSE_1 )
{
   hbqt_par_QWidget( 1 )->grabMouse( *hbqt_par_QCursor( 2 ) );
}

/*
 * int grabShortcut ( const QKeySequence & key, Qt::ShortcutContext context = Qt::WindowShortcut )
 */
HB_FUNC( QT_QWIDGET_GRABSHORTCUT )
{
   hb_retni( hbqt_par_QWidget( 1 )->grabShortcut( *hbqt_par_QKeySequence( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ShortcutContext ) hb_parni( 3 ) : ( Qt::ShortcutContext ) Qt::WindowShortcut ) ) );
}

/*
 * QGraphicsProxyWidget * graphicsProxyWidget () const
 */
HB_FUNC( QT_QWIDGET_GRAPHICSPROXYWIDGET )
{
   hb_retptr( ( QGraphicsProxyWidget* ) hbqt_par_QWidget( 1 )->graphicsProxyWidget() );
}

/*
 * bool hasFocus () const
 */
HB_FUNC( QT_QWIDGET_HASFOCUS )
{
   hb_retl( hbqt_par_QWidget( 1 )->hasFocus() );
}

/*
 * bool hasMouseTracking () const
 */
HB_FUNC( QT_QWIDGET_HASMOUSETRACKING )
{
   hb_retl( hbqt_par_QWidget( 1 )->hasMouseTracking() );
}

/*
 * int height () const
 */
HB_FUNC( QT_QWIDGET_HEIGHT )
{
   hb_retni( hbqt_par_QWidget( 1 )->height() );
}

/*
 * virtual int heightForWidth ( int w ) const
 */
HB_FUNC( QT_QWIDGET_HEIGHTFORWIDTH )
{
   hb_retni( hbqt_par_QWidget( 1 )->heightForWidth( hb_parni( 2 ) ) );
}

/*
 * QInputContext * inputContext ()
 */
HB_FUNC( QT_QWIDGET_INPUTCONTEXT )
{
   hb_retptr( ( QInputContext* ) hbqt_par_QWidget( 1 )->inputContext() );
}

/*
 * virtual QVariant inputMethodQuery ( Qt::InputMethodQuery query ) const
 */
HB_FUNC( QT_QWIDGET_INPUTMETHODQUERY )
{
   hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( hbqt_par_QWidget( 1 )->inputMethodQuery( ( Qt::InputMethodQuery ) hb_parni( 2 ) ) ) ) );
}

/*
 * void insertAction ( QAction * before, QAction * action )
 */
HB_FUNC( QT_QWIDGET_INSERTACTION )
{
   hbqt_par_QWidget( 1 )->insertAction( hbqt_par_QAction( 2 ), hbqt_par_QAction( 3 ) );
}

/*
 * bool isActiveWindow () const
 */
HB_FUNC( QT_QWIDGET_ISACTIVEWINDOW )
{
   hb_retl( hbqt_par_QWidget( 1 )->isActiveWindow() );
}

/*
 * bool isAncestorOf ( const QWidget * child ) const
 */
HB_FUNC( QT_QWIDGET_ISANCESTOROF )
{
   hb_retl( hbqt_par_QWidget( 1 )->isAncestorOf( hbqt_par_QWidget( 2 ) ) );
}

/*
 * bool isEnabled () const
 */
HB_FUNC( QT_QWIDGET_ISENABLED )
{
   hb_retl( hbqt_par_QWidget( 1 )->isEnabled() );
}

/*
 * bool isEnabledTo ( QWidget * ancestor ) const
 */
HB_FUNC( QT_QWIDGET_ISENABLEDTO )
{
   hb_retl( hbqt_par_QWidget( 1 )->isEnabledTo( hbqt_par_QWidget( 2 ) ) );
}

/*
 * bool isFullScreen () const
 */
HB_FUNC( QT_QWIDGET_ISFULLSCREEN )
{
   hb_retl( hbqt_par_QWidget( 1 )->isFullScreen() );
}

/*
 * bool isHidden () const
 */
HB_FUNC( QT_QWIDGET_ISHIDDEN )
{
   hb_retl( hbqt_par_QWidget( 1 )->isHidden() );
}

/*
 * bool isMaximized () const
 */
HB_FUNC( QT_QWIDGET_ISMAXIMIZED )
{
   hb_retl( hbqt_par_QWidget( 1 )->isMaximized() );
}

/*
 * bool isMinimized () const
 */
HB_FUNC( QT_QWIDGET_ISMINIMIZED )
{
   hb_retl( hbqt_par_QWidget( 1 )->isMinimized() );
}

/*
 * bool isModal () const
 */
HB_FUNC( QT_QWIDGET_ISMODAL )
{
   hb_retl( hbqt_par_QWidget( 1 )->isModal() );
}

/*
 * bool isVisible () const
 */
HB_FUNC( QT_QWIDGET_ISVISIBLE )
{
   hb_retl( hbqt_par_QWidget( 1 )->isVisible() );
}

/*
 * bool isVisibleTo ( QWidget * ancestor ) const
 */
HB_FUNC( QT_QWIDGET_ISVISIBLETO )
{
   hb_retl( hbqt_par_QWidget( 1 )->isVisibleTo( hbqt_par_QWidget( 2 ) ) );
}

/*
 * bool isWindow () const
 */
HB_FUNC( QT_QWIDGET_ISWINDOW )
{
   hb_retl( hbqt_par_QWidget( 1 )->isWindow() );
}

/*
 * bool isWindowModified () const
 */
HB_FUNC( QT_QWIDGET_ISWINDOWMODIFIED )
{
   hb_retl( hbqt_par_QWidget( 1 )->isWindowModified() );
}

/*
 * QLayout * layout () const
 */
HB_FUNC( QT_QWIDGET_LAYOUT )
{
   hb_retptr( ( QLayout* ) hbqt_par_QWidget( 1 )->layout() );
}

/*
 * Qt::LayoutDirection layoutDirection () const
 */
HB_FUNC( QT_QWIDGET_LAYOUTDIRECTION )
{
   hb_retni( ( Qt::LayoutDirection ) hbqt_par_QWidget( 1 )->layoutDirection() );
}

/*
 * QLocale locale () const
 */
HB_FUNC( QT_QWIDGET_LOCALE )
{
   hb_retptrGC( hbqt_gcAllocate_QLocale( new QLocale( hbqt_par_QWidget( 1 )->locale() ) ) );
}

/*
 * QPoint mapFrom ( QWidget * parent, const QPoint & pos ) const
 */
HB_FUNC( QT_QWIDGET_MAPFROM )
{
   hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( hbqt_par_QWidget( 1 )->mapFrom( hbqt_par_QWidget( 2 ), *hbqt_par_QPoint( 3 ) ) ) ) );
}

/*
 * QPoint mapFromGlobal ( const QPoint & pos ) const
 */
HB_FUNC( QT_QWIDGET_MAPFROMGLOBAL )
{
   hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( hbqt_par_QWidget( 1 )->mapFromGlobal( *hbqt_par_QPoint( 2 ) ) ) ) );
}

/*
 * QPoint mapFromParent ( const QPoint & pos ) const
 */
HB_FUNC( QT_QWIDGET_MAPFROMPARENT )
{
   hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( hbqt_par_QWidget( 1 )->mapFromParent( *hbqt_par_QPoint( 2 ) ) ) ) );
}

/*
 * QPoint mapTo ( QWidget * parent, const QPoint & pos ) const
 */
HB_FUNC( QT_QWIDGET_MAPTO )
{
   hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( hbqt_par_QWidget( 1 )->mapTo( hbqt_par_QWidget( 2 ), *hbqt_par_QPoint( 3 ) ) ) ) );
}

/*
 * QPoint mapToGlobal ( const QPoint & pos ) const
 */
HB_FUNC( QT_QWIDGET_MAPTOGLOBAL )
{
   hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( hbqt_par_QWidget( 1 )->mapToGlobal( *hbqt_par_QPoint( 2 ) ) ) ) );
}

/*
 * QPoint mapToParent ( const QPoint & pos ) const
 */
HB_FUNC( QT_QWIDGET_MAPTOPARENT )
{
   hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( hbqt_par_QWidget( 1 )->mapToParent( *hbqt_par_QPoint( 2 ) ) ) ) );
}

/*
 * QRegion mask () const
 */
HB_FUNC( QT_QWIDGET_MASK )
{
   hb_retptrGC( hbqt_gcAllocate_QRegion( new QRegion( hbqt_par_QWidget( 1 )->mask() ) ) );
}

/*
 * int maximumHeight () const
 */
HB_FUNC( QT_QWIDGET_MAXIMUMHEIGHT )
{
   hb_retni( hbqt_par_QWidget( 1 )->maximumHeight() );
}

/*
 * QSize maximumSize () const
 */
HB_FUNC( QT_QWIDGET_MAXIMUMSIZE )
{
   hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( hbqt_par_QWidget( 1 )->maximumSize() ) ) );
}

/*
 * int maximumWidth () const
 */
HB_FUNC( QT_QWIDGET_MAXIMUMWIDTH )
{
   hb_retni( hbqt_par_QWidget( 1 )->maximumWidth() );
}

/*
 * int minimumHeight () const
 */
HB_FUNC( QT_QWIDGET_MINIMUMHEIGHT )
{
   hb_retni( hbqt_par_QWidget( 1 )->minimumHeight() );
}

/*
 * QSize minimumSize () const
 */
HB_FUNC( QT_QWIDGET_MINIMUMSIZE )
{
   hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( hbqt_par_QWidget( 1 )->minimumSize() ) ) );
}

/*
 * virtual QSize minimumSizeHint () const
 */
HB_FUNC( QT_QWIDGET_MINIMUMSIZEHINT )
{
   hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( hbqt_par_QWidget( 1 )->minimumSizeHint() ) ) );
}

/*
 * int minimumWidth () const
 */
HB_FUNC( QT_QWIDGET_MINIMUMWIDTH )
{
   hb_retni( hbqt_par_QWidget( 1 )->minimumWidth() );
}

/*
 * void move ( int x, int y )
 */
HB_FUNC( QT_QWIDGET_MOVE )
{
   hbqt_par_QWidget( 1 )->move( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void move ( const QPoint & )
 */
HB_FUNC( QT_QWIDGET_MOVE_1 )
{
   hbqt_par_QWidget( 1 )->move( *hbqt_par_QPoint( 2 ) );
}

/*
 * QWidget * nativeParentWidget () const
 */
HB_FUNC( QT_QWIDGET_NATIVEPARENTWIDGET )
{
   hb_retptr( ( QWidget* ) hbqt_par_QWidget( 1 )->nativeParentWidget() );
}

/*
 * QWidget * nextInFocusChain () const
 */
HB_FUNC( QT_QWIDGET_NEXTINFOCUSCHAIN )
{
   hb_retptr( ( QWidget* ) hbqt_par_QWidget( 1 )->nextInFocusChain() );
}

/*
 * QRect normalGeometry () const
 */
HB_FUNC( QT_QWIDGET_NORMALGEOMETRY )
{
   hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( hbqt_par_QWidget( 1 )->normalGeometry() ) ) );
}

/*
 * void overrideWindowFlags ( Qt::WindowFlags flags )
 */
HB_FUNC( QT_QWIDGET_OVERRIDEWINDOWFLAGS )
{
   hbqt_par_QWidget( 1 )->overrideWindowFlags( ( Qt::WindowFlags ) hb_parni( 2 ) );
}

/*
 * virtual QPaintEngine * paintEngine () const
 */
HB_FUNC( QT_QWIDGET_PAINTENGINE )
{
   hb_retptr( ( QPaintEngine* ) hbqt_par_QWidget( 1 )->paintEngine() );
}

/*
 * const QPalette & palette () const
 */
HB_FUNC( QT_QWIDGET_PALETTE )
{
   hb_retptrGC( hbqt_gcAllocate_QPalette( new QPalette( hbqt_par_QWidget( 1 )->palette() ) ) );
}

/*
 * QWidget * parentWidget () const
 */
HB_FUNC( QT_QWIDGET_PARENTWIDGET )
{
   hb_retptr( ( QWidget* ) hbqt_par_QWidget( 1 )->parentWidget() );
}

/*
 * QPoint pos () const
 */
HB_FUNC( QT_QWIDGET_POS )
{
   hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( hbqt_par_QWidget( 1 )->pos() ) ) );
}

/*
 * QRect rect () const
 */
HB_FUNC( QT_QWIDGET_RECT )
{
   hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( hbqt_par_QWidget( 1 )->rect() ) ) );
}

/*
 * void releaseKeyboard ()
 */
HB_FUNC( QT_QWIDGET_RELEASEKEYBOARD )
{
   hbqt_par_QWidget( 1 )->releaseKeyboard();
}

/*
 * void releaseMouse ()
 */
HB_FUNC( QT_QWIDGET_RELEASEMOUSE )
{
   hbqt_par_QWidget( 1 )->releaseMouse();
}

/*
 * void releaseShortcut ( int id )
 */
HB_FUNC( QT_QWIDGET_RELEASESHORTCUT )
{
   hbqt_par_QWidget( 1 )->releaseShortcut( hb_parni( 2 ) );
}

/*
 * void removeAction ( QAction * action )
 */
HB_FUNC( QT_QWIDGET_REMOVEACTION )
{
   hbqt_par_QWidget( 1 )->removeAction( hbqt_par_QAction( 2 ) );
}

/*
 * void repaint ( int x, int y, int w, int h )
 */
HB_FUNC( QT_QWIDGET_REPAINT )
{
   hbqt_par_QWidget( 1 )->repaint( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/*
 * void repaint ( const QRect & rect )
 */
HB_FUNC( QT_QWIDGET_REPAINT_1 )
{
   hbqt_par_QWidget( 1 )->repaint( *hbqt_par_QRect( 2 ) );
}

/*
 * void repaint ( const QRegion & rgn )
 */
HB_FUNC( QT_QWIDGET_REPAINT_2 )
{
   hbqt_par_QWidget( 1 )->repaint( *hbqt_par_QRegion( 2 ) );
}

/*
 * void resize ( int w, int h )
 */
HB_FUNC( QT_QWIDGET_RESIZE )
{
   hbqt_par_QWidget( 1 )->resize( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void resize ( const QSize & )
 */
HB_FUNC( QT_QWIDGET_RESIZE_1 )
{
   hbqt_par_QWidget( 1 )->resize( *hbqt_par_QSize( 2 ) );
}

/*
 * bool restoreGeometry ( const QByteArray & geometry )
 */
HB_FUNC( QT_QWIDGET_RESTOREGEOMETRY )
{
   hb_retl( hbqt_par_QWidget( 1 )->restoreGeometry( *hbqt_par_QByteArray( 2 ) ) );
}

/*
 * QByteArray saveGeometry () const
 */
HB_FUNC( QT_QWIDGET_SAVEGEOMETRY )
{
   hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( hbqt_par_QWidget( 1 )->saveGeometry() ) ) );
}

/*
 * void scroll ( int dx, int dy )
 */
HB_FUNC( QT_QWIDGET_SCROLL )
{
   hbqt_par_QWidget( 1 )->scroll( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void scroll ( int dx, int dy, const QRect & r )
 */
HB_FUNC( QT_QWIDGET_SCROLL_1 )
{
   hbqt_par_QWidget( 1 )->scroll( hb_parni( 2 ), hb_parni( 3 ), *hbqt_par_QRect( 4 ) );
}

/*
 * void setAcceptDrops ( bool on )
 */
HB_FUNC( QT_QWIDGET_SETACCEPTDROPS )
{
   hbqt_par_QWidget( 1 )->setAcceptDrops( hb_parl( 2 ) );
}

/*
 * void setAccessibleDescription ( const QString & description )
 */
HB_FUNC( QT_QWIDGET_SETACCESSIBLEDESCRIPTION )
{
   hbqt_par_QWidget( 1 )->setAccessibleDescription( QWidget::tr( hb_parc( 2 ) ) );
}

/*
 * void setAccessibleName ( const QString & name )
 */
HB_FUNC( QT_QWIDGET_SETACCESSIBLENAME )
{
   hbqt_par_QWidget( 1 )->setAccessibleName( QWidget::tr( hb_parc( 2 ) ) );
}

/*
 * void setAttribute ( Qt::WidgetAttribute attribute, bool on = true )
 */
HB_FUNC( QT_QWIDGET_SETATTRIBUTE )
{
   hbqt_par_QWidget( 1 )->setAttribute( ( Qt::WidgetAttribute ) hb_parni( 2 ), hb_parl( 3 ) );
}

/*
 * void setAutoFillBackground ( bool enabled )
 */
HB_FUNC( QT_QWIDGET_SETAUTOFILLBACKGROUND )
{
   hbqt_par_QWidget( 1 )->setAutoFillBackground( hb_parl( 2 ) );
}

/*
 * void setBackgroundRole ( QPalette::ColorRole role )
 */
HB_FUNC( QT_QWIDGET_SETBACKGROUNDROLE )
{
   hbqt_par_QWidget( 1 )->setBackgroundRole( ( QPalette::ColorRole ) hb_parni( 2 ) );
}

/*
 * void setBaseSize ( const QSize & )
 */
HB_FUNC( QT_QWIDGET_SETBASESIZE )
{
   hbqt_par_QWidget( 1 )->setBaseSize( *hbqt_par_QSize( 2 ) );
}

/*
 * void setBaseSize ( int basew, int baseh )
 */
HB_FUNC( QT_QWIDGET_SETBASESIZE_1 )
{
   hbqt_par_QWidget( 1 )->setBaseSize( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void setContentsMargins ( int left, int top, int right, int bottom )
 */
HB_FUNC( QT_QWIDGET_SETCONTENTSMARGINS )
{
   hbqt_par_QWidget( 1 )->setContentsMargins( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/*
 * void setContextMenuPolicy ( Qt::ContextMenuPolicy policy )
 */
HB_FUNC( QT_QWIDGET_SETCONTEXTMENUPOLICY )
{
   hbqt_par_QWidget( 1 )->setContextMenuPolicy( ( Qt::ContextMenuPolicy ) hb_parni( 2 ) );
}

/*
 * void setCursor ( const QCursor & )
 */
HB_FUNC( QT_QWIDGET_SETCURSOR )
{
   hbqt_par_QWidget( 1 )->setCursor( *hbqt_par_QCursor( 2 ) );
}

/*
 * void setFixedHeight ( int h )
 */
HB_FUNC( QT_QWIDGET_SETFIXEDHEIGHT )
{
   hbqt_par_QWidget( 1 )->setFixedHeight( hb_parni( 2 ) );
}

/*
 * void setFixedSize ( const QSize & s )
 */
HB_FUNC( QT_QWIDGET_SETFIXEDSIZE )
{
   hbqt_par_QWidget( 1 )->setFixedSize( *hbqt_par_QSize( 2 ) );
}

/*
 * void setFixedSize ( int w, int h )
 */
HB_FUNC( QT_QWIDGET_SETFIXEDSIZE_1 )
{
   hbqt_par_QWidget( 1 )->setFixedSize( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void setFixedWidth ( int w )
 */
HB_FUNC( QT_QWIDGET_SETFIXEDWIDTH )
{
   hbqt_par_QWidget( 1 )->setFixedWidth( hb_parni( 2 ) );
}

/*
 * void setFocus ( Qt::FocusReason reason )
 */
HB_FUNC( QT_QWIDGET_SETFOCUS )
{
   hbqt_par_QWidget( 1 )->setFocus( ( Qt::FocusReason ) hb_parni( 2 ) );
}

/*
 * void setFocusPolicy ( Qt::FocusPolicy policy )
 */
HB_FUNC( QT_QWIDGET_SETFOCUSPOLICY )
{
   hbqt_par_QWidget( 1 )->setFocusPolicy( ( Qt::FocusPolicy ) hb_parni( 2 ) );
}

/*
 * void setFocusProxy ( QWidget * w )
 */
HB_FUNC( QT_QWIDGET_SETFOCUSPROXY )
{
   hbqt_par_QWidget( 1 )->setFocusProxy( hbqt_par_QWidget( 2 ) );
}

/*
 * void setFont ( const QFont & )
 */
HB_FUNC( QT_QWIDGET_SETFONT )
{
   hbqt_par_QWidget( 1 )->setFont( *hbqt_par_QFont( 2 ) );
}

/*
 * void setForegroundRole ( QPalette::ColorRole role )
 */
HB_FUNC( QT_QWIDGET_SETFOREGROUNDROLE )
{
   hbqt_par_QWidget( 1 )->setForegroundRole( ( QPalette::ColorRole ) hb_parni( 2 ) );
}

/*
 * void setGeometry ( const QRect & )
 */
HB_FUNC( QT_QWIDGET_SETGEOMETRY )
{
   hbqt_par_QWidget( 1 )->setGeometry( *hbqt_par_QRect( 2 ) );
}

/*
 * void setGeometry ( int x, int y, int w, int h )
 */
HB_FUNC( QT_QWIDGET_SETGEOMETRY_1 )
{
   hbqt_par_QWidget( 1 )->setGeometry( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/*
 * void setInputContext ( QInputContext * context )
 */
HB_FUNC( QT_QWIDGET_SETINPUTCONTEXT )
{
   hbqt_par_QWidget( 1 )->setInputContext( hbqt_par_QInputContext( 2 ) );
}

/*
 * void setLayout ( QLayout * layout )
 */
HB_FUNC( QT_QWIDGET_SETLAYOUT )
{
   hbqt_par_QWidget( 1 )->setLayout( hbqt_par_QLayout( 2 ) );
}

/*
 * void setLayoutDirection ( Qt::LayoutDirection direction )
 */
HB_FUNC( QT_QWIDGET_SETLAYOUTDIRECTION )
{
   hbqt_par_QWidget( 1 )->setLayoutDirection( ( Qt::LayoutDirection ) hb_parni( 2 ) );
}

/*
 * void setLocale ( const QLocale & locale )
 */
HB_FUNC( QT_QWIDGET_SETLOCALE )
{
   hbqt_par_QWidget( 1 )->setLocale( *hbqt_par_QLocale( 2 ) );
}

/*
 * void setMask ( const QBitmap & bitmap )
 */
HB_FUNC( QT_QWIDGET_SETMASK )
{
   hbqt_par_QWidget( 1 )->setMask( *hbqt_par_QBitmap( 2 ) );
}

/*
 * void setMask ( const QRegion & region )
 */
HB_FUNC( QT_QWIDGET_SETMASK_1 )
{
   hbqt_par_QWidget( 1 )->setMask( *hbqt_par_QRegion( 2 ) );
}

/*
 * void setMaximumHeight ( int maxh )
 */
HB_FUNC( QT_QWIDGET_SETMAXIMUMHEIGHT )
{
   hbqt_par_QWidget( 1 )->setMaximumHeight( hb_parni( 2 ) );
}

/*
 * void setMaximumSize ( const QSize & )
 */
HB_FUNC( QT_QWIDGET_SETMAXIMUMSIZE )
{
   hbqt_par_QWidget( 1 )->setMaximumSize( *hbqt_par_QSize( 2 ) );
}

/*
 * void setMaximumSize ( int maxw, int maxh )
 */
HB_FUNC( QT_QWIDGET_SETMAXIMUMSIZE_1 )
{
   hbqt_par_QWidget( 1 )->setMaximumSize( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void setMaximumWidth ( int maxw )
 */
HB_FUNC( QT_QWIDGET_SETMAXIMUMWIDTH )
{
   hbqt_par_QWidget( 1 )->setMaximumWidth( hb_parni( 2 ) );
}

/*
 * void setMinimumHeight ( int minh )
 */
HB_FUNC( QT_QWIDGET_SETMINIMUMHEIGHT )
{
   hbqt_par_QWidget( 1 )->setMinimumHeight( hb_parni( 2 ) );
}

/*
 * void setMinimumSize ( const QSize & )
 */
HB_FUNC( QT_QWIDGET_SETMINIMUMSIZE )
{
   hbqt_par_QWidget( 1 )->setMinimumSize( *hbqt_par_QSize( 2 ) );
}

/*
 * void setMinimumSize ( int minw, int minh )
 */
HB_FUNC( QT_QWIDGET_SETMINIMUMSIZE_1 )
{
   hbqt_par_QWidget( 1 )->setMinimumSize( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void setMinimumWidth ( int minw )
 */
HB_FUNC( QT_QWIDGET_SETMINIMUMWIDTH )
{
   hbqt_par_QWidget( 1 )->setMinimumWidth( hb_parni( 2 ) );
}

/*
 * void setMouseTracking ( bool enable )
 */
HB_FUNC( QT_QWIDGET_SETMOUSETRACKING )
{
   hbqt_par_QWidget( 1 )->setMouseTracking( hb_parl( 2 ) );
}

/*
 * void setPalette ( const QPalette & )
 */
HB_FUNC( QT_QWIDGET_SETPALETTE )
{
   hbqt_par_QWidget( 1 )->setPalette( *hbqt_par_QPalette( 2 ) );
}

/*
 * void setParent ( QWidget * parent )
 */
HB_FUNC( QT_QWIDGET_SETPARENT )
{
   hbqt_par_QWidget( 1 )->setParent( hbqt_par_QWidget( 2 ) );
}

/*
 * void setParent ( QWidget * parent, Qt::WindowFlags f )
 */
HB_FUNC( QT_QWIDGET_SETPARENT_1 )
{
   hbqt_par_QWidget( 1 )->setParent( hbqt_par_QWidget( 2 ), ( Qt::WindowFlags ) hb_parni( 3 ) );
}

/*
 * void setShortcutAutoRepeat ( int id, bool enable = true )
 */
HB_FUNC( QT_QWIDGET_SETSHORTCUTAUTOREPEAT )
{
   hbqt_par_QWidget( 1 )->setShortcutAutoRepeat( hb_parni( 2 ), hb_parl( 3 ) );
}

/*
 * void setShortcutEnabled ( int id, bool enable = true )
 */
HB_FUNC( QT_QWIDGET_SETSHORTCUTENABLED )
{
   hbqt_par_QWidget( 1 )->setShortcutEnabled( hb_parni( 2 ), hb_parl( 3 ) );
}

/*
 * void setSizeIncrement ( const QSize & )
 */
HB_FUNC( QT_QWIDGET_SETSIZEINCREMENT )
{
   hbqt_par_QWidget( 1 )->setSizeIncrement( *hbqt_par_QSize( 2 ) );
}

/*
 * void setSizeIncrement ( int w, int h )
 */
HB_FUNC( QT_QWIDGET_SETSIZEINCREMENT_1 )
{
   hbqt_par_QWidget( 1 )->setSizeIncrement( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void setSizePolicy ( QSizePolicy::Policy horizontal, QSizePolicy::Policy vertical )
 */
HB_FUNC( QT_QWIDGET_SETSIZEPOLICY )
{
   hbqt_par_QWidget( 1 )->setSizePolicy( ( QSizePolicy::Policy ) hb_parni( 2 ), ( QSizePolicy::Policy ) hb_parni( 3 ) );
}

/*
 * void setStatusTip ( const QString & )
 */
HB_FUNC( QT_QWIDGET_SETSTATUSTIP )
{
   hbqt_par_QWidget( 1 )->setStatusTip( QWidget::tr( hb_parc( 2 ) ) );
}

/*
 * void setStyle ( QStyle * style )
 */
HB_FUNC( QT_QWIDGET_SETSTYLE )
{
   hbqt_par_QWidget( 1 )->setStyle( hbqt_par_QStyle( 2 ) );
}

/*
 * void setToolTip ( const QString & )
 */
HB_FUNC( QT_QWIDGET_SETTOOLTIP )
{
   hbqt_par_QWidget( 1 )->setToolTip( QWidget::tr( hb_parc( 2 ) ) );
}

/*
 * void setUpdatesEnabled ( bool enable )
 */
HB_FUNC( QT_QWIDGET_SETUPDATESENABLED )
{
   hbqt_par_QWidget( 1 )->setUpdatesEnabled( hb_parl( 2 ) );
}

/*
 * void setWhatsThis ( const QString & )
 */
HB_FUNC( QT_QWIDGET_SETWHATSTHIS )
{
   hbqt_par_QWidget( 1 )->setWhatsThis( QWidget::tr( hb_parc( 2 ) ) );
}

/*
 * void setWindowFilePath ( const QString & filePath )
 */
HB_FUNC( QT_QWIDGET_SETWINDOWFILEPATH )
{
   hbqt_par_QWidget( 1 )->setWindowFilePath( QWidget::tr( hb_parc( 2 ) ) );
}

/*
 * void setWindowFlags ( Qt::WindowFlags type )
 */
HB_FUNC( QT_QWIDGET_SETWINDOWFLAGS )
{
   hbqt_par_QWidget( 1 )->setWindowFlags( ( Qt::WindowFlags ) hb_parni( 2 ) );
}

/*
 * void setWindowIcon ( const QIcon & icon )
 */
HB_FUNC( QT_QWIDGET_SETWINDOWICON )
{
   hbqt_par_QWidget( 1 )->setWindowIcon( QIcon( hbqt_par_QString( 2 ) ) );
}

/*
 * void setWindowIconText ( const QString & )
 */
HB_FUNC( QT_QWIDGET_SETWINDOWICONTEXT )
{
   hbqt_par_QWidget( 1 )->setWindowIconText( QWidget::tr( hb_parc( 2 ) ) );
}

/*
 * void setWindowModality ( Qt::WindowModality windowModality )
 */
HB_FUNC( QT_QWIDGET_SETWINDOWMODALITY )
{
   hbqt_par_QWidget( 1 )->setWindowModality( ( Qt::WindowModality ) hb_parni( 2 ) );
}

/*
 * void setWindowOpacity ( qreal level )
 */
HB_FUNC( QT_QWIDGET_SETWINDOWOPACITY )
{
   hbqt_par_QWidget( 1 )->setWindowOpacity( hb_parnd( 2 ) );
}

/*
 * void setWindowRole ( const QString & role )
 */
HB_FUNC( QT_QWIDGET_SETWINDOWROLE )
{
   hbqt_par_QWidget( 1 )->setWindowRole( QWidget::tr( hb_parc( 2 ) ) );
}

/*
 * void setWindowState ( Qt::WindowStates windowState )
 */
HB_FUNC( QT_QWIDGET_SETWINDOWSTATE )
{
   hbqt_par_QWidget( 1 )->setWindowState( ( Qt::WindowStates ) hb_parni( 2 ) );
}

/*
 * QSize size () const
 */
HB_FUNC( QT_QWIDGET_SIZE )
{
   hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( hbqt_par_QWidget( 1 )->size() ) ) );
}

/*
 * virtual QSize sizeHint () const
 */
HB_FUNC( QT_QWIDGET_SIZEHINT )
{
   hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( hbqt_par_QWidget( 1 )->sizeHint() ) ) );
}

/*
 * QSize sizeIncrement () const
 */
HB_FUNC( QT_QWIDGET_SIZEINCREMENT )
{
   hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( hbqt_par_QWidget( 1 )->sizeIncrement() ) ) );
}

/*
 * QSizePolicy sizePolicy () const
 */
HB_FUNC( QT_QWIDGET_SIZEPOLICY )
{
   hb_retptrGC( hbqt_gcAllocate_QSizePolicy( new QSizePolicy( hbqt_par_QWidget( 1 )->sizePolicy() ) ) );
}

/*
 * void stackUnder ( QWidget * w )
 */
HB_FUNC( QT_QWIDGET_STACKUNDER )
{
   hbqt_par_QWidget( 1 )->stackUnder( hbqt_par_QWidget( 2 ) );
}

/*
 * QString statusTip () const
 */
HB_FUNC( QT_QWIDGET_STATUSTIP )
{
   hb_retc( hbqt_par_QWidget( 1 )->statusTip().toAscii().data() );
}

/*
 * QStyle * style () const
 */
HB_FUNC( QT_QWIDGET_STYLE )
{
   hb_retptr( ( QStyle* ) hbqt_par_QWidget( 1 )->style() );
}

/*
 * QString styleSheet () const
 */
HB_FUNC( QT_QWIDGET_STYLESHEET )
{
   hb_retc( hbqt_par_QWidget( 1 )->styleSheet().toAscii().data() );
}

/*
 * bool testAttribute ( Qt::WidgetAttribute attribute ) const
 */
HB_FUNC( QT_QWIDGET_TESTATTRIBUTE )
{
   hb_retl( hbqt_par_QWidget( 1 )->testAttribute( ( Qt::WidgetAttribute ) hb_parni( 2 ) ) );
}

/*
 * QString toolTip () const
 */
HB_FUNC( QT_QWIDGET_TOOLTIP )
{
   hb_retc( hbqt_par_QWidget( 1 )->toolTip().toAscii().data() );
}

/*
 * bool underMouse () const
 */
HB_FUNC( QT_QWIDGET_UNDERMOUSE )
{
   hb_retl( hbqt_par_QWidget( 1 )->underMouse() );
}

/*
 * void unsetCursor ()
 */
HB_FUNC( QT_QWIDGET_UNSETCURSOR )
{
   hbqt_par_QWidget( 1 )->unsetCursor();
}

/*
 * void unsetLayoutDirection ()
 */
HB_FUNC( QT_QWIDGET_UNSETLAYOUTDIRECTION )
{
   hbqt_par_QWidget( 1 )->unsetLayoutDirection();
}

/*
 * void unsetLocale ()
 */
HB_FUNC( QT_QWIDGET_UNSETLOCALE )
{
   hbqt_par_QWidget( 1 )->unsetLocale();
}

/*
 * void update ( int x, int y, int w, int h )
 */
HB_FUNC( QT_QWIDGET_UPDATE )
{
   hbqt_par_QWidget( 1 )->update( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/*
 * void update ( const QRect & rect )
 */
HB_FUNC( QT_QWIDGET_UPDATE_1 )
{
   hbqt_par_QWidget( 1 )->update( *hbqt_par_QRect( 2 ) );
}

/*
 * void update ( const QRegion & rgn )
 */
HB_FUNC( QT_QWIDGET_UPDATE_2 )
{
   hbqt_par_QWidget( 1 )->update( *hbqt_par_QRegion( 2 ) );
}

/*
 * void updateGeometry ()
 */
HB_FUNC( QT_QWIDGET_UPDATEGEOMETRY )
{
   hbqt_par_QWidget( 1 )->updateGeometry();
}

/*
 * bool updatesEnabled () const
 */
HB_FUNC( QT_QWIDGET_UPDATESENABLED )
{
   hb_retl( hbqt_par_QWidget( 1 )->updatesEnabled() );
}

/*
 * QRegion visibleRegion () const
 */
HB_FUNC( QT_QWIDGET_VISIBLEREGION )
{
   hb_retptrGC( hbqt_gcAllocate_QRegion( new QRegion( hbqt_par_QWidget( 1 )->visibleRegion() ) ) );
}

/*
 * QString whatsThis () const
 */
HB_FUNC( QT_QWIDGET_WHATSTHIS )
{
   hb_retc( hbqt_par_QWidget( 1 )->whatsThis().toAscii().data() );
}

/*
 * int width () const
 */
HB_FUNC( QT_QWIDGET_WIDTH )
{
   hb_retni( hbqt_par_QWidget( 1 )->width() );
}

/*
 * QWidget * window () const
 */
HB_FUNC( QT_QWIDGET_WINDOW )
{
   hb_retptr( ( QWidget* ) hbqt_par_QWidget( 1 )->window() );
}

/*
 * QString windowFilePath () const
 */
HB_FUNC( QT_QWIDGET_WINDOWFILEPATH )
{
   hb_retc( hbqt_par_QWidget( 1 )->windowFilePath().toAscii().data() );
}

/*
 * Qt::WindowFlags windowFlags () const
 */
HB_FUNC( QT_QWIDGET_WINDOWFLAGS )
{
   hb_retni( ( Qt::WindowFlags ) hbqt_par_QWidget( 1 )->windowFlags() );
}

/*
 * QIcon windowIcon () const
 */
HB_FUNC( QT_QWIDGET_WINDOWICON )
{
   hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( hbqt_par_QWidget( 1 )->windowIcon() ) ) );
}

/*
 * QString windowIconText () const
 */
HB_FUNC( QT_QWIDGET_WINDOWICONTEXT )
{
   hb_retc( hbqt_par_QWidget( 1 )->windowIconText().toAscii().data() );
}

/*
 * Qt::WindowModality windowModality () const
 */
HB_FUNC( QT_QWIDGET_WINDOWMODALITY )
{
   hb_retni( ( Qt::WindowModality ) hbqt_par_QWidget( 1 )->windowModality() );
}

/*
 * qreal windowOpacity () const
 */
HB_FUNC( QT_QWIDGET_WINDOWOPACITY )
{
   hb_retnd( hbqt_par_QWidget( 1 )->windowOpacity() );
}

/*
 * QString windowRole () const
 */
HB_FUNC( QT_QWIDGET_WINDOWROLE )
{
   hb_retc( hbqt_par_QWidget( 1 )->windowRole().toAscii().data() );
}

/*
 * Qt::WindowStates windowState () const
 */
HB_FUNC( QT_QWIDGET_WINDOWSTATE )
{
   hb_retni( ( Qt::WindowStates ) hbqt_par_QWidget( 1 )->windowState() );
}

/*
 * QString windowTitle () const
 */
HB_FUNC( QT_QWIDGET_WINDOWTITLE )
{
   hb_retc( hbqt_par_QWidget( 1 )->windowTitle().toAscii().data() );
}

/*
 * Qt::WindowType windowType () const
 */
HB_FUNC( QT_QWIDGET_WINDOWTYPE )
{
   hb_retni( ( Qt::WindowType ) hbqt_par_QWidget( 1 )->windowType() );
}

/*
 * int x () const
 */
HB_FUNC( QT_QWIDGET_X )
{
   hb_retni( hbqt_par_QWidget( 1 )->x() );
}

/*
 * int y () const
 */
HB_FUNC( QT_QWIDGET_Y )
{
   hb_retni( hbqt_par_QWidget( 1 )->y() );
}

/*
 * QWidget * keyboardGrabber ()
 */
HB_FUNC( QT_QWIDGET_KEYBOARDGRABBER )
{
   hb_retptr( ( QWidget* ) hbqt_par_QWidget( 1 )->keyboardGrabber() );
}

/*
 * QWidget * mouseGrabber ()
 */
HB_FUNC( QT_QWIDGET_MOUSEGRABBER )
{
   hb_retptr( ( QWidget* ) hbqt_par_QWidget( 1 )->mouseGrabber() );
}

/*
 * void setTabOrder ( QWidget * first, QWidget * second )
 */
HB_FUNC( QT_QWIDGET_SETTABORDER )
{
   hbqt_par_QWidget( 1 )->setTabOrder( hbqt_par_QWidget( 2 ), hbqt_par_QWidget( 3 ) );
}

/*
 * bool close ()
 */
HB_FUNC( QT_QWIDGET_CLOSE )
{
   hb_retl( hbqt_par_QWidget( 1 )->close() );
}

/*
 * void hide ()
 */
HB_FUNC( QT_QWIDGET_HIDE )
{
   hbqt_par_QWidget( 1 )->hide();
}

/*
 * void lower ()
 */
HB_FUNC( QT_QWIDGET_LOWER )
{
   hbqt_par_QWidget( 1 )->lower();
}

/*
 * void raise ()
 */
HB_FUNC( QT_QWIDGET_RAISE )
{
   hbqt_par_QWidget( 1 )->raise();
}

/*
 * void repaint ()
 */
HB_FUNC( QT_QWIDGET_REPAINT_3 )
{
   hbqt_par_QWidget( 1 )->repaint();
}

/*
 * void setDisabled ( bool disable )
 */
HB_FUNC( QT_QWIDGET_SETDISABLED )
{
   hbqt_par_QWidget( 1 )->setDisabled( hb_parl( 2 ) );
}

/*
 * void setEnabled ( bool enable )
 */
HB_FUNC( QT_QWIDGET_SETENABLED )
{
   hbqt_par_QWidget( 1 )->setEnabled( hb_parl( 2 ) );
}

/*
 * void setFocus ()
 */
HB_FUNC( QT_QWIDGET_SETFOCUS_1 )
{
   hbqt_par_QWidget( 1 )->setFocus();
}

/*
 * void setHidden ( bool hidden )
 */
HB_FUNC( QT_QWIDGET_SETHIDDEN )
{
   hbqt_par_QWidget( 1 )->setHidden( hb_parl( 2 ) );
}

/*
 * void setStyleSheet ( const QString & styleSheet )
 */
HB_FUNC( QT_QWIDGET_SETSTYLESHEET )
{
   hbqt_par_QWidget( 1 )->setStyleSheet( QWidget::tr( hb_parc( 2 ) ) );
}

/*
 * virtual void setVisible ( bool visible )
 */
HB_FUNC( QT_QWIDGET_SETVISIBLE )
{
   hbqt_par_QWidget( 1 )->setVisible( hb_parl( 2 ) );
}

/*
 * void setWindowModified ( bool modified )
 */
HB_FUNC( QT_QWIDGET_SETWINDOWMODIFIED )
{
   hbqt_par_QWidget( 1 )->setWindowModified( hb_parl( 2 ) );
}

/*
 * void setWindowTitle ( const QString & title )
 */
HB_FUNC( QT_QWIDGET_SETWINDOWTITLE )
{
   hbqt_par_QWidget( 1 )->setWindowTitle( QWidget::tr( hb_parc( 2 ) ) );
}

/*
 * void show ()
 */
HB_FUNC( QT_QWIDGET_SHOW )
{
   hbqt_par_QWidget( 1 )->show();
}

/*
 * void showFullScreen ()
 */
HB_FUNC( QT_QWIDGET_SHOWFULLSCREEN )
{
   hbqt_par_QWidget( 1 )->showFullScreen();
}

/*
 * void showMaximized ()
 */
HB_FUNC( QT_QWIDGET_SHOWMAXIMIZED )
{
   hbqt_par_QWidget( 1 )->showMaximized();
}

/*
 * void showMinimized ()
 */
HB_FUNC( QT_QWIDGET_SHOWMINIMIZED )
{
   hbqt_par_QWidget( 1 )->showMinimized();
}

/*
 * void showNormal ()
 */
HB_FUNC( QT_QWIDGET_SHOWNORMAL )
{
   hbqt_par_QWidget( 1 )->showNormal();
}

/*
 * void update ()
 */
HB_FUNC( QT_QWIDGET_UPDATE_3 )
{
   hbqt_par_QWidget( 1 )->update();
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
