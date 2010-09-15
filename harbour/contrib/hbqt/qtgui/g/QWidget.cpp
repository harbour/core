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
 *  enum RenderFlag { DrawWindowBackground, DrawChildren, IgnoreMask }
 *  # From QPaintDevice : Parent Class
 *  enum PaintDeviceMetric { PdmWidth, PdmHeight, PdmWidthMM, PdmHeightMM, ..., PdmPhysicalDpiY }
 */

/*
 *  Constructed[ 206/230 [ 89.57% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  }
 *  void addActions ( QList<QAction *> actions )
 *  void insertActions ( QAction * before, QList<QAction *> actions )
 *
 *  *** Commented out protos which construct fine but do not compile ***
 *
 *  // QString accessibleDescription () const
 *  // QString accessibleName () const
 *  // WId effectiveWinId () const
 *  // virtual HDC getDC () const
 *  // QGraphicsProxyWidget * graphicsProxyWidget () const
 *  // bool hasEditFocus () const
 *  // QInputContext * inputContext ()
 *  // Qt::HANDLE macCGHandle () const
 *  // Qt::HANDLE macQDHandle () const
 *  // virtual void releaseDC ( HDC hdc ) const
 *  //void render ( QPaintDevice * target, const QPoint & targetOffset = QPoint(), const QRegion & sourceRegion = QRegion(), RenderFlags renderFlags = RenderFlags( DrawWindowBackground | DrawChildren ) )
 *  //void render ( QPainter * painter, const QPoint & targetOffset = QPoint(), const QRegion & sourceRegion = QRegion(), RenderFlags renderFlags = RenderFlags( DrawWindowBackground | DrawChildren ) )
 *  // void setAccessibleDescription ( const QString & description )
 *  // void setAccessibleName ( const QString & name )
 *  // void setEditFocus ( bool enable )
 *  // void setInputContext ( QInputContext * context )
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
   QPointer< QWidget > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QWidget;

HBQT_GC_FUNC( hbqt_gcRelease_QWidget )
{
   QWidget  * ph = NULL ;
   HBQT_GC_T_QWidget * p = ( HBQT_GC_T_QWidget * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QWidget   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QWidget   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QWidget          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QWidget    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QWidget    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QWidget( void * pObj, bool bNew )
{
   HBQT_GC_T_QWidget * p = ( HBQT_GC_T_QWidget * ) hb_gcAllocate( sizeof( HBQT_GC_T_QWidget ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QWidget >( ( QWidget * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QWidget;
   p->type = HBQT_TYPE_QWidget;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QWidget  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QWidget", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QWIDGET )
{
   QWidget * pObj = NULL;

   if( hb_pcount() >= 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QWidget( hbqt_par_QWidget( 1 ), ( Qt::WindowFlags ) ( HB_ISNUM( 2 ) ? hb_parni( 2 ) : 0 ) ) ;
   }
   else
   {
      pObj = new QWidget() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QWidget( ( void * ) pObj, true ) );
}

/*
 * bool acceptDrops () const
 */
HB_FUNC( QT_QWIDGET_ACCEPTDROPS )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->acceptDrops() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_ACCEPTDROPS FP=hb_retl( ( p )->acceptDrops() ); p is NULL" ) );
   }
}

/*
 * QList<QAction *> actions () const
 */
HB_FUNC( QT_QWIDGET_ACTIONS )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QAction *>( ( p )->actions() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_ACTIONS FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QAction *>( ( p )->actions() ), true ) ); p is NULL" ) );
   }
}

/*
 * void activateWindow ()
 */
HB_FUNC( QT_QWIDGET_ACTIVATEWINDOW )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->activateWindow();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_ACTIVATEWINDOW FP=( p )->activateWindow(); p is NULL" ) );
   }
}

/*
 * void addAction ( QAction * action )
 */
HB_FUNC( QT_QWIDGET_ADDACTION )
{
   HBQT_GC_T_QWidget * q =  (HBQT_GC_T_QWidget *)hb_parptrGC( hbqt_gcFuncs(), 1 );
   HBQT_GC_T * p =  (HBQT_GC_T *)hb_parptrGC( hbqt_gcFuncs(), 2 );

   HB_TRACE( HB_TR_DEBUG, ( "Entering function QT_QWIDGET_ADDACTION()" ) );
   if( p && p->ph && q && q->ph )
   {
      HB_TRACE( HB_TR_DEBUG, ( "QT_QWIDGET_ADDACTION() Qt object: %p is attached to: %p", (void * )p->ph, (void *)q->ph ) );
      // p->bNew = HB_FALSE;  // The ownership of action is not transferred to this QWidget.
      ( q->ph )->addAction( ( QAction * ) p->ph );
   }
}

/*
 * void adjustSize ()
 */
HB_FUNC( QT_QWIDGET_ADJUSTSIZE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->adjustSize();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_ADJUSTSIZE FP=( p )->adjustSize(); p is NULL" ) );
   }
}

/*
 * bool autoFillBackground () const
 */
HB_FUNC( QT_QWIDGET_AUTOFILLBACKGROUND )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->autoFillBackground() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_AUTOFILLBACKGROUND FP=hb_retl( ( p )->autoFillBackground() ); p is NULL" ) );
   }
}

/*
 * QPalette::ColorRole backgroundRole () const
 */
HB_FUNC( QT_QWIDGET_BACKGROUNDROLE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( QPalette::ColorRole ) ( p )->backgroundRole() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_BACKGROUNDROLE FP=hb_retni( ( QPalette::ColorRole ) ( p )->backgroundRole() ); p is NULL" ) );
   }
}

/*
 * QSize baseSize () const
 */
HB_FUNC( QT_QWIDGET_BASESIZE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->baseSize() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_BASESIZE FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->baseSize() ), true ) ); p is NULL" ) );
   }
}

/*
 * QWidget * childAt ( int x, int y ) const
 */
HB_FUNC( QT_QWIDGET_CHILDAT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->childAt( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_CHILDAT FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->childAt( hb_parni( 2 ), hb_parni( 3 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QWidget * childAt ( const QPoint & p ) const
 */
HB_FUNC( QT_QWIDGET_CHILDAT_1 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->childAt( *hbqt_par_QPoint( 2 ) ), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_CHILDAT_1 FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->childAt( *hbqt_par_QPoint( 2 ) ), false ) ); p is NULL" ) );
   }
}

/*
 * QRect childrenRect () const
 */
HB_FUNC( QT_QWIDGET_CHILDRENRECT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->childrenRect() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_CHILDRENRECT FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->childrenRect() ), true ) ); p is NULL" ) );
   }
}

/*
 * QRegion childrenRegion () const
 */
HB_FUNC( QT_QWIDGET_CHILDRENREGION )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRegion( new QRegion( ( p )->childrenRegion() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_CHILDRENREGION FP=hb_retptrGC( hbqt_gcAllocate_QRegion( new QRegion( ( p )->childrenRegion() ), true ) ); p is NULL" ) );
   }
}

/*
 * void clearFocus ()
 */
HB_FUNC( QT_QWIDGET_CLEARFOCUS )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->clearFocus();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_CLEARFOCUS FP=( p )->clearFocus(); p is NULL" ) );
   }
}

/*
 * void clearMask ()
 */
HB_FUNC( QT_QWIDGET_CLEARMASK )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->clearMask();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_CLEARMASK FP=( p )->clearMask(); p is NULL" ) );
   }
}

/*
 * QRect contentsRect () const
 */
HB_FUNC( QT_QWIDGET_CONTENTSRECT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->contentsRect() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_CONTENTSRECT FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->contentsRect() ), true ) ); p is NULL" ) );
   }
}

/*
 * Qt::ContextMenuPolicy contextMenuPolicy () const
 */
HB_FUNC( QT_QWIDGET_CONTEXTMENUPOLICY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( Qt::ContextMenuPolicy ) ( p )->contextMenuPolicy() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_CONTEXTMENUPOLICY FP=hb_retni( ( Qt::ContextMenuPolicy ) ( p )->contextMenuPolicy() ); p is NULL" ) );
   }
}

/*
 * QCursor cursor () const
 */
HB_FUNC( QT_QWIDGET_CURSOR )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QCursor( new QCursor( ( p )->cursor() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_CURSOR FP=hb_retptrGC( hbqt_gcAllocate_QCursor( new QCursor( ( p )->cursor() ), true ) ); p is NULL" ) );
   }
}

/*
 * void ensurePolished () const
 */
HB_FUNC( QT_QWIDGET_ENSUREPOLISHED )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->ensurePolished();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_ENSUREPOLISHED FP=( p )->ensurePolished(); p is NULL" ) );
   }
}

/*
 * Qt::FocusPolicy focusPolicy () const
 */
HB_FUNC( QT_QWIDGET_FOCUSPOLICY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( Qt::FocusPolicy ) ( p )->focusPolicy() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_FOCUSPOLICY FP=hb_retni( ( Qt::FocusPolicy ) ( p )->focusPolicy() ); p is NULL" ) );
   }
}

/*
 * QWidget * focusProxy () const
 */
HB_FUNC( QT_QWIDGET_FOCUSPROXY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->focusProxy(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_FOCUSPROXY FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->focusProxy(), false ) ); p is NULL" ) );
   }
}

/*
 * QWidget * focusWidget () const
 */
HB_FUNC( QT_QWIDGET_FOCUSWIDGET )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->focusWidget(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_FOCUSWIDGET FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->focusWidget(), false ) ); p is NULL" ) );
   }
}

/*
 * const QFont & font () const
 */
HB_FUNC( QT_QWIDGET_FONT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_FONT FP=hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) ); p is NULL" ) );
   }
}

/*
 * QFontInfo fontInfo () const
 */
HB_FUNC( QT_QWIDGET_FONTINFO )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFontInfo( new QFontInfo( ( p )->fontInfo() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_FONTINFO FP=hb_retptrGC( hbqt_gcAllocate_QFontInfo( new QFontInfo( ( p )->fontInfo() ), true ) ); p is NULL" ) );
   }
}

/*
 * QFontMetrics fontMetrics () const
 */
HB_FUNC( QT_QWIDGET_FONTMETRICS )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFontMetrics( new QFontMetrics( ( p )->fontMetrics() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_FONTMETRICS FP=hb_retptrGC( hbqt_gcAllocate_QFontMetrics( new QFontMetrics( ( p )->fontMetrics() ), true ) ); p is NULL" ) );
   }
}

/*
 * QPalette::ColorRole foregroundRole () const
 */
HB_FUNC( QT_QWIDGET_FOREGROUNDROLE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( QPalette::ColorRole ) ( p )->foregroundRole() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_FOREGROUNDROLE FP=hb_retni( ( QPalette::ColorRole ) ( p )->foregroundRole() ); p is NULL" ) );
   }
}

/*
 * QRect frameGeometry () const
 */
HB_FUNC( QT_QWIDGET_FRAMEGEOMETRY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->frameGeometry() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_FRAMEGEOMETRY FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->frameGeometry() ), true ) ); p is NULL" ) );
   }
}

/*
 * QSize frameSize () const
 */
HB_FUNC( QT_QWIDGET_FRAMESIZE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->frameSize() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_FRAMESIZE FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->frameSize() ), true ) ); p is NULL" ) );
   }
}

/*
 * const QRect & geometry () const
 */
HB_FUNC( QT_QWIDGET_GEOMETRY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->geometry() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_GEOMETRY FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->geometry() ), true ) ); p is NULL" ) );
   }
}

/*
 * void getContentsMargins ( int * left, int * top, int * right, int * bottom ) const
 */
HB_FUNC( QT_QWIDGET_GETCONTENTSMARGINS )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   int iLeft = 0;
   int iTop = 0;
   int iRight = 0;
   int iBottom = 0;

   if( p )
      ( p )->getContentsMargins( &iLeft, &iTop, &iRight, &iBottom );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_GETCONTENTSMARGINS FP=( p )->getContentsMargins( &iLeft, &iTop, &iRight, &iBottom ); p is NULL" ) );
   }

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
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->grabKeyboard();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_GRABKEYBOARD FP=( p )->grabKeyboard(); p is NULL" ) );
   }
}

/*
 * void grabMouse ()
 */
HB_FUNC( QT_QWIDGET_GRABMOUSE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->grabMouse();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_GRABMOUSE FP=( p )->grabMouse(); p is NULL" ) );
   }
}

/*
 * void grabMouse ( const QCursor & cursor )
 */
HB_FUNC( QT_QWIDGET_GRABMOUSE_1 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->grabMouse( *hbqt_par_QCursor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_GRABMOUSE_1 FP=( p )->grabMouse( *hbqt_par_QCursor( 2 ) ); p is NULL" ) );
   }
}

/*
 * int grabShortcut ( const QKeySequence & key, Qt::ShortcutContext context = Qt::WindowShortcut )
 */
HB_FUNC( QT_QWIDGET_GRABSHORTCUT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( p )->grabShortcut( *hbqt_par_QKeySequence( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ShortcutContext ) hb_parni( 3 ) : ( Qt::ShortcutContext ) Qt::WindowShortcut ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_GRABSHORTCUT FP=hb_retni( ( p )->grabShortcut( *hbqt_par_QKeySequence( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ShortcutContext ) hb_parni( 3 ) : ( Qt::ShortcutContext ) Qt::WindowShortcut ) ) ); p is NULL" ) );
   }
}

/*
 * bool hasFocus () const
 */
HB_FUNC( QT_QWIDGET_HASFOCUS )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->hasFocus() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_HASFOCUS FP=hb_retl( ( p )->hasFocus() ); p is NULL" ) );
   }
}

/*
 * bool hasMouseTracking () const
 */
HB_FUNC( QT_QWIDGET_HASMOUSETRACKING )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->hasMouseTracking() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_HASMOUSETRACKING FP=hb_retl( ( p )->hasMouseTracking() ); p is NULL" ) );
   }
}

/*
 * int height () const
 */
HB_FUNC( QT_QWIDGET_HEIGHT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( p )->height() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_HEIGHT FP=hb_retni( ( p )->height() ); p is NULL" ) );
   }
}

/*
 * virtual int heightForWidth ( int w ) const
 */
HB_FUNC( QT_QWIDGET_HEIGHTFORWIDTH )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( p )->heightForWidth( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_HEIGHTFORWIDTH FP=hb_retni( ( p )->heightForWidth( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * virtual QVariant inputMethodQuery ( Qt::InputMethodQuery query ) const
 */
HB_FUNC( QT_QWIDGET_INPUTMETHODQUERY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->inputMethodQuery( ( Qt::InputMethodQuery ) hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_INPUTMETHODQUERY FP=hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->inputMethodQuery( ( Qt::InputMethodQuery ) hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * void insertAction ( QAction * before, QAction * action )
 */
HB_FUNC( QT_QWIDGET_INSERTACTION )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->insertAction( hbqt_par_QAction( 2 ), hbqt_par_QAction( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_INSERTACTION FP=( p )->insertAction( hbqt_par_QAction( 2 ), hbqt_par_QAction( 3 ) ); p is NULL" ) );
   }
}

/*
 * bool isActiveWindow () const
 */
HB_FUNC( QT_QWIDGET_ISACTIVEWINDOW )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->isActiveWindow() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_ISACTIVEWINDOW FP=hb_retl( ( p )->isActiveWindow() ); p is NULL" ) );
   }
}

/*
 * bool isAncestorOf ( const QWidget * child ) const
 */
HB_FUNC( QT_QWIDGET_ISANCESTOROF )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->isAncestorOf( hbqt_par_QWidget( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_ISANCESTOROF FP=hb_retl( ( p )->isAncestorOf( hbqt_par_QWidget( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool isEnabled () const
 */
HB_FUNC( QT_QWIDGET_ISENABLED )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->isEnabled() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_ISENABLED FP=hb_retl( ( p )->isEnabled() ); p is NULL" ) );
   }
}

/*
 * bool isEnabledTo ( QWidget * ancestor ) const
 */
HB_FUNC( QT_QWIDGET_ISENABLEDTO )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->isEnabledTo( hbqt_par_QWidget( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_ISENABLEDTO FP=hb_retl( ( p )->isEnabledTo( hbqt_par_QWidget( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool isFullScreen () const
 */
HB_FUNC( QT_QWIDGET_ISFULLSCREEN )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->isFullScreen() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_ISFULLSCREEN FP=hb_retl( ( p )->isFullScreen() ); p is NULL" ) );
   }
}

/*
 * bool isHidden () const
 */
HB_FUNC( QT_QWIDGET_ISHIDDEN )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->isHidden() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_ISHIDDEN FP=hb_retl( ( p )->isHidden() ); p is NULL" ) );
   }
}

/*
 * bool isMaximized () const
 */
HB_FUNC( QT_QWIDGET_ISMAXIMIZED )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->isMaximized() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_ISMAXIMIZED FP=hb_retl( ( p )->isMaximized() ); p is NULL" ) );
   }
}

/*
 * bool isMinimized () const
 */
HB_FUNC( QT_QWIDGET_ISMINIMIZED )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->isMinimized() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_ISMINIMIZED FP=hb_retl( ( p )->isMinimized() ); p is NULL" ) );
   }
}

/*
 * bool isModal () const
 */
HB_FUNC( QT_QWIDGET_ISMODAL )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->isModal() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_ISMODAL FP=hb_retl( ( p )->isModal() ); p is NULL" ) );
   }
}

/*
 * bool isVisible () const
 */
HB_FUNC( QT_QWIDGET_ISVISIBLE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->isVisible() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_ISVISIBLE FP=hb_retl( ( p )->isVisible() ); p is NULL" ) );
   }
}

/*
 * bool isVisibleTo ( QWidget * ancestor ) const
 */
HB_FUNC( QT_QWIDGET_ISVISIBLETO )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->isVisibleTo( hbqt_par_QWidget( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_ISVISIBLETO FP=hb_retl( ( p )->isVisibleTo( hbqt_par_QWidget( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool isWindow () const
 */
HB_FUNC( QT_QWIDGET_ISWINDOW )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->isWindow() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_ISWINDOW FP=hb_retl( ( p )->isWindow() ); p is NULL" ) );
   }
}

/*
 * bool isWindowModified () const
 */
HB_FUNC( QT_QWIDGET_ISWINDOWMODIFIED )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->isWindowModified() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_ISWINDOWMODIFIED FP=hb_retl( ( p )->isWindowModified() ); p is NULL" ) );
   }
}

/*
 * QLayout * layout () const
 */
HB_FUNC( QT_QWIDGET_LAYOUT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QLayout( ( p )->layout(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_LAYOUT FP=hb_retptrGC( hbqt_gcAllocate_QLayout( ( p )->layout(), false ) ); p is NULL" ) );
   }
}

/*
 * Qt::LayoutDirection layoutDirection () const
 */
HB_FUNC( QT_QWIDGET_LAYOUTDIRECTION )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( Qt::LayoutDirection ) ( p )->layoutDirection() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_LAYOUTDIRECTION FP=hb_retni( ( Qt::LayoutDirection ) ( p )->layoutDirection() ); p is NULL" ) );
   }
}

/*
 * QLocale locale () const
 */
HB_FUNC( QT_QWIDGET_LOCALE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QLocale( new QLocale( ( p )->locale() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_LOCALE FP=hb_retptrGC( hbqt_gcAllocate_QLocale( new QLocale( ( p )->locale() ), true ) ); p is NULL" ) );
   }
}

/*
 * QPoint mapFrom ( QWidget * parent, const QPoint & pos ) const
 */
HB_FUNC( QT_QWIDGET_MAPFROM )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->mapFrom( hbqt_par_QWidget( 2 ), *hbqt_par_QPoint( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_MAPFROM FP=hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->mapFrom( hbqt_par_QWidget( 2 ), *hbqt_par_QPoint( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPoint mapFromGlobal ( const QPoint & pos ) const
 */
HB_FUNC( QT_QWIDGET_MAPFROMGLOBAL )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->mapFromGlobal( *hbqt_par_QPoint( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_MAPFROMGLOBAL FP=hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->mapFromGlobal( *hbqt_par_QPoint( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPoint mapFromParent ( const QPoint & pos ) const
 */
HB_FUNC( QT_QWIDGET_MAPFROMPARENT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->mapFromParent( *hbqt_par_QPoint( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_MAPFROMPARENT FP=hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->mapFromParent( *hbqt_par_QPoint( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPoint mapTo ( QWidget * parent, const QPoint & pos ) const
 */
HB_FUNC( QT_QWIDGET_MAPTO )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->mapTo( hbqt_par_QWidget( 2 ), *hbqt_par_QPoint( 3 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_MAPTO FP=hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->mapTo( hbqt_par_QWidget( 2 ), *hbqt_par_QPoint( 3 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPoint mapToGlobal ( const QPoint & pos ) const
 */
HB_FUNC( QT_QWIDGET_MAPTOGLOBAL )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->mapToGlobal( *hbqt_par_QPoint( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_MAPTOGLOBAL FP=hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->mapToGlobal( *hbqt_par_QPoint( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QPoint mapToParent ( const QPoint & pos ) const
 */
HB_FUNC( QT_QWIDGET_MAPTOPARENT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->mapToParent( *hbqt_par_QPoint( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_MAPTOPARENT FP=hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->mapToParent( *hbqt_par_QPoint( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QRegion mask () const
 */
HB_FUNC( QT_QWIDGET_MASK )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRegion( new QRegion( ( p )->mask() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_MASK FP=hb_retptrGC( hbqt_gcAllocate_QRegion( new QRegion( ( p )->mask() ), true ) ); p is NULL" ) );
   }
}

/*
 * int maximumHeight () const
 */
HB_FUNC( QT_QWIDGET_MAXIMUMHEIGHT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( p )->maximumHeight() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_MAXIMUMHEIGHT FP=hb_retni( ( p )->maximumHeight() ); p is NULL" ) );
   }
}

/*
 * QSize maximumSize () const
 */
HB_FUNC( QT_QWIDGET_MAXIMUMSIZE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->maximumSize() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_MAXIMUMSIZE FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->maximumSize() ), true ) ); p is NULL" ) );
   }
}

/*
 * int maximumWidth () const
 */
HB_FUNC( QT_QWIDGET_MAXIMUMWIDTH )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( p )->maximumWidth() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_MAXIMUMWIDTH FP=hb_retni( ( p )->maximumWidth() ); p is NULL" ) );
   }
}

/*
 * int minimumHeight () const
 */
HB_FUNC( QT_QWIDGET_MINIMUMHEIGHT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( p )->minimumHeight() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_MINIMUMHEIGHT FP=hb_retni( ( p )->minimumHeight() ); p is NULL" ) );
   }
}

/*
 * QSize minimumSize () const
 */
HB_FUNC( QT_QWIDGET_MINIMUMSIZE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->minimumSize() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_MINIMUMSIZE FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->minimumSize() ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual QSize minimumSizeHint () const
 */
HB_FUNC( QT_QWIDGET_MINIMUMSIZEHINT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->minimumSizeHint() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_MINIMUMSIZEHINT FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->minimumSizeHint() ), true ) ); p is NULL" ) );
   }
}

/*
 * int minimumWidth () const
 */
HB_FUNC( QT_QWIDGET_MINIMUMWIDTH )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( p )->minimumWidth() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_MINIMUMWIDTH FP=hb_retni( ( p )->minimumWidth() ); p is NULL" ) );
   }
}

/*
 * void move ( int x, int y )
 */
HB_FUNC( QT_QWIDGET_MOVE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->move( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_MOVE FP=( p )->move( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void move ( const QPoint & )
 */
HB_FUNC( QT_QWIDGET_MOVE_1 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->move( *hbqt_par_QPoint( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_MOVE_1 FP=( p )->move( *hbqt_par_QPoint( 2 ) ); p is NULL" ) );
   }
}

/*
 * QWidget * nativeParentWidget () const
 */
HB_FUNC( QT_QWIDGET_NATIVEPARENTWIDGET )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->nativeParentWidget(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_NATIVEPARENTWIDGET FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->nativeParentWidget(), false ) ); p is NULL" ) );
   }
}

/*
 * QWidget * nextInFocusChain () const
 */
HB_FUNC( QT_QWIDGET_NEXTINFOCUSCHAIN )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->nextInFocusChain(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_NEXTINFOCUSCHAIN FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->nextInFocusChain(), false ) ); p is NULL" ) );
   }
}

/*
 * QRect normalGeometry () const
 */
HB_FUNC( QT_QWIDGET_NORMALGEOMETRY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->normalGeometry() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_NORMALGEOMETRY FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->normalGeometry() ), true ) ); p is NULL" ) );
   }
}

/*
 * void overrideWindowFlags ( Qt::WindowFlags flags )
 */
HB_FUNC( QT_QWIDGET_OVERRIDEWINDOWFLAGS )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->overrideWindowFlags( ( Qt::WindowFlags ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_OVERRIDEWINDOWFLAGS FP=( p )->overrideWindowFlags( ( Qt::WindowFlags ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * virtual QPaintEngine * paintEngine () const
 */
HB_FUNC( QT_QWIDGET_PAINTENGINE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPaintEngine( ( p )->paintEngine(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_PAINTENGINE FP=hb_retptrGC( hbqt_gcAllocate_QPaintEngine( ( p )->paintEngine(), false ) ); p is NULL" ) );
   }
}

/*
 * const QPalette & palette () const
 */
HB_FUNC( QT_QWIDGET_PALETTE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPalette( new QPalette( ( p )->palette() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_PALETTE FP=hb_retptrGC( hbqt_gcAllocate_QPalette( new QPalette( ( p )->palette() ), true ) ); p is NULL" ) );
   }
}

/*
 * QWidget * parentWidget () const
 */
HB_FUNC( QT_QWIDGET_PARENTWIDGET )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->parentWidget(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_PARENTWIDGET FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->parentWidget(), false ) ); p is NULL" ) );
   }
}

/*
 * QPoint pos () const
 */
HB_FUNC( QT_QWIDGET_POS )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->pos() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_POS FP=hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->pos() ), true ) ); p is NULL" ) );
   }
}

/*
 * QRect rect () const
 */
HB_FUNC( QT_QWIDGET_RECT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->rect() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_RECT FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->rect() ), true ) ); p is NULL" ) );
   }
}

/*
 * void releaseKeyboard ()
 */
HB_FUNC( QT_QWIDGET_RELEASEKEYBOARD )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->releaseKeyboard();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_RELEASEKEYBOARD FP=( p )->releaseKeyboard(); p is NULL" ) );
   }
}

/*
 * void releaseMouse ()
 */
HB_FUNC( QT_QWIDGET_RELEASEMOUSE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->releaseMouse();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_RELEASEMOUSE FP=( p )->releaseMouse(); p is NULL" ) );
   }
}

/*
 * void releaseShortcut ( int id )
 */
HB_FUNC( QT_QWIDGET_RELEASESHORTCUT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->releaseShortcut( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_RELEASESHORTCUT FP=( p )->releaseShortcut( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void removeAction ( QAction * action )
 */
HB_FUNC( QT_QWIDGET_REMOVEACTION )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->removeAction( hbqt_par_QAction( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_REMOVEACTION FP=( p )->removeAction( hbqt_par_QAction( 2 ) ); p is NULL" ) );
   }
}

/*
 * void repaint ( int x, int y, int w, int h )
 */
HB_FUNC( QT_QWIDGET_REPAINT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->repaint( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_REPAINT FP=( p )->repaint( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ); p is NULL" ) );
   }
}

/*
 * void repaint ( const QRect & rect )
 */
HB_FUNC( QT_QWIDGET_REPAINT_1 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->repaint( *hbqt_par_QRect( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_REPAINT_1 FP=( p )->repaint( *hbqt_par_QRect( 2 ) ); p is NULL" ) );
   }
}

/*
 * void repaint ( const QRegion & rgn )
 */
HB_FUNC( QT_QWIDGET_REPAINT_2 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->repaint( *hbqt_par_QRegion( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_REPAINT_2 FP=( p )->repaint( *hbqt_par_QRegion( 2 ) ); p is NULL" ) );
   }
}

/*
 * void resize ( int w, int h )
 */
HB_FUNC( QT_QWIDGET_RESIZE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->resize( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_RESIZE FP=( p )->resize( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void resize ( const QSize & )
 */
HB_FUNC( QT_QWIDGET_RESIZE_1 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->resize( *hbqt_par_QSize( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_RESIZE_1 FP=( p )->resize( *hbqt_par_QSize( 2 ) ); p is NULL" ) );
   }
}

/*
 * bool restoreGeometry ( const QByteArray & geometry )
 */
HB_FUNC( QT_QWIDGET_RESTOREGEOMETRY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->restoreGeometry( *hbqt_par_QByteArray( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_RESTOREGEOMETRY FP=hb_retl( ( p )->restoreGeometry( *hbqt_par_QByteArray( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QByteArray saveGeometry () const
 */
HB_FUNC( QT_QWIDGET_SAVEGEOMETRY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->saveGeometry() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SAVEGEOMETRY FP=hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->saveGeometry() ), true ) ); p is NULL" ) );
   }
}

/*
 * void scroll ( int dx, int dy )
 */
HB_FUNC( QT_QWIDGET_SCROLL )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->scroll( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SCROLL FP=( p )->scroll( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void scroll ( int dx, int dy, const QRect & r )
 */
HB_FUNC( QT_QWIDGET_SCROLL_1 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->scroll( hb_parni( 2 ), hb_parni( 3 ), *hbqt_par_QRect( 4 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SCROLL_1 FP=( p )->scroll( hb_parni( 2 ), hb_parni( 3 ), *hbqt_par_QRect( 4 ) ); p is NULL" ) );
   }
}

/*
 * void setAcceptDrops ( bool on )
 */
HB_FUNC( QT_QWIDGET_SETACCEPTDROPS )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setAcceptDrops( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETACCEPTDROPS FP=( p )->setAcceptDrops( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setAttribute ( Qt::WidgetAttribute attribute, bool on = true )
 */
HB_FUNC( QT_QWIDGET_SETATTRIBUTE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setAttribute( ( Qt::WidgetAttribute ) hb_parni( 2 ), hb_parl( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETATTRIBUTE FP=( p )->setAttribute( ( Qt::WidgetAttribute ) hb_parni( 2 ), hb_parl( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setAutoFillBackground ( bool enabled )
 */
HB_FUNC( QT_QWIDGET_SETAUTOFILLBACKGROUND )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setAutoFillBackground( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETAUTOFILLBACKGROUND FP=( p )->setAutoFillBackground( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setBackgroundRole ( QPalette::ColorRole role )
 */
HB_FUNC( QT_QWIDGET_SETBACKGROUNDROLE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setBackgroundRole( ( QPalette::ColorRole ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETBACKGROUNDROLE FP=( p )->setBackgroundRole( ( QPalette::ColorRole ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setBaseSize ( const QSize & )
 */
HB_FUNC( QT_QWIDGET_SETBASESIZE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setBaseSize( *hbqt_par_QSize( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETBASESIZE FP=( p )->setBaseSize( *hbqt_par_QSize( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setBaseSize ( int basew, int baseh )
 */
HB_FUNC( QT_QWIDGET_SETBASESIZE_1 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setBaseSize( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETBASESIZE_1 FP=( p )->setBaseSize( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setContentsMargins ( int left, int top, int right, int bottom )
 */
HB_FUNC( QT_QWIDGET_SETCONTENTSMARGINS )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setContentsMargins( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETCONTENTSMARGINS FP=( p )->setContentsMargins( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ); p is NULL" ) );
   }
}

/*
 * void setContextMenuPolicy ( Qt::ContextMenuPolicy policy )
 */
HB_FUNC( QT_QWIDGET_SETCONTEXTMENUPOLICY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setContextMenuPolicy( ( Qt::ContextMenuPolicy ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETCONTEXTMENUPOLICY FP=( p )->setContextMenuPolicy( ( Qt::ContextMenuPolicy ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCursor ( const QCursor & )
 */
HB_FUNC( QT_QWIDGET_SETCURSOR )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setCursor( *hbqt_par_QCursor( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETCURSOR FP=( p )->setCursor( *hbqt_par_QCursor( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFixedHeight ( int h )
 */
HB_FUNC( QT_QWIDGET_SETFIXEDHEIGHT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setFixedHeight( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETFIXEDHEIGHT FP=( p )->setFixedHeight( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFixedSize ( const QSize & s )
 */
HB_FUNC( QT_QWIDGET_SETFIXEDSIZE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setFixedSize( *hbqt_par_QSize( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETFIXEDSIZE FP=( p )->setFixedSize( *hbqt_par_QSize( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFixedSize ( int w, int h )
 */
HB_FUNC( QT_QWIDGET_SETFIXEDSIZE_1 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setFixedSize( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETFIXEDSIZE_1 FP=( p )->setFixedSize( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setFixedWidth ( int w )
 */
HB_FUNC( QT_QWIDGET_SETFIXEDWIDTH )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setFixedWidth( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETFIXEDWIDTH FP=( p )->setFixedWidth( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFocus ( Qt::FocusReason reason )
 */
HB_FUNC( QT_QWIDGET_SETFOCUS )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setFocus( ( Qt::FocusReason ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETFOCUS FP=( p )->setFocus( ( Qt::FocusReason ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFocusPolicy ( Qt::FocusPolicy policy )
 */
HB_FUNC( QT_QWIDGET_SETFOCUSPOLICY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setFocusPolicy( ( Qt::FocusPolicy ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETFOCUSPOLICY FP=( p )->setFocusPolicy( ( Qt::FocusPolicy ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFocusProxy ( QWidget * w )
 */
HB_FUNC( QT_QWIDGET_SETFOCUSPROXY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setFocusProxy( hbqt_par_QWidget( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETFOCUSPROXY FP=( p )->setFocusProxy( hbqt_par_QWidget( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFont ( const QFont & )
 */
HB_FUNC( QT_QWIDGET_SETFONT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setFont( *hbqt_par_QFont( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETFONT FP=( p )->setFont( *hbqt_par_QFont( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setForegroundRole ( QPalette::ColorRole role )
 */
HB_FUNC( QT_QWIDGET_SETFOREGROUNDROLE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setForegroundRole( ( QPalette::ColorRole ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETFOREGROUNDROLE FP=( p )->setForegroundRole( ( QPalette::ColorRole ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setGeometry ( const QRect & )
 */
HB_FUNC( QT_QWIDGET_SETGEOMETRY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setGeometry( *hbqt_par_QRect( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETGEOMETRY FP=( p )->setGeometry( *hbqt_par_QRect( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setGeometry ( int x, int y, int w, int h )
 */
HB_FUNC( QT_QWIDGET_SETGEOMETRY_1 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setGeometry( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETGEOMETRY_1 FP=( p )->setGeometry( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ); p is NULL" ) );
   }
}

/*
 * void setLayout ( QLayout * layout )
 */
HB_FUNC( QT_QWIDGET_SETLAYOUT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setLayout( hbqt_par_QLayout( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETLAYOUT FP=( p )->setLayout( hbqt_par_QLayout( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setLayoutDirection ( Qt::LayoutDirection direction )
 */
HB_FUNC( QT_QWIDGET_SETLAYOUTDIRECTION )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setLayoutDirection( ( Qt::LayoutDirection ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETLAYOUTDIRECTION FP=( p )->setLayoutDirection( ( Qt::LayoutDirection ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setLocale ( const QLocale & locale )
 */
HB_FUNC( QT_QWIDGET_SETLOCALE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setLocale( *hbqt_par_QLocale( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETLOCALE FP=( p )->setLocale( *hbqt_par_QLocale( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMask ( const QBitmap & bitmap )
 */
HB_FUNC( QT_QWIDGET_SETMASK )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setMask( *hbqt_par_QBitmap( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETMASK FP=( p )->setMask( *hbqt_par_QBitmap( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMask ( const QRegion & region )
 */
HB_FUNC( QT_QWIDGET_SETMASK_1 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setMask( *hbqt_par_QRegion( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETMASK_1 FP=( p )->setMask( *hbqt_par_QRegion( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMaximumHeight ( int maxh )
 */
HB_FUNC( QT_QWIDGET_SETMAXIMUMHEIGHT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setMaximumHeight( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETMAXIMUMHEIGHT FP=( p )->setMaximumHeight( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMaximumSize ( const QSize & )
 */
HB_FUNC( QT_QWIDGET_SETMAXIMUMSIZE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setMaximumSize( *hbqt_par_QSize( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETMAXIMUMSIZE FP=( p )->setMaximumSize( *hbqt_par_QSize( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMaximumSize ( int maxw, int maxh )
 */
HB_FUNC( QT_QWIDGET_SETMAXIMUMSIZE_1 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setMaximumSize( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETMAXIMUMSIZE_1 FP=( p )->setMaximumSize( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setMaximumWidth ( int maxw )
 */
HB_FUNC( QT_QWIDGET_SETMAXIMUMWIDTH )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setMaximumWidth( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETMAXIMUMWIDTH FP=( p )->setMaximumWidth( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMinimumHeight ( int minh )
 */
HB_FUNC( QT_QWIDGET_SETMINIMUMHEIGHT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setMinimumHeight( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETMINIMUMHEIGHT FP=( p )->setMinimumHeight( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMinimumSize ( const QSize & )
 */
HB_FUNC( QT_QWIDGET_SETMINIMUMSIZE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setMinimumSize( *hbqt_par_QSize( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETMINIMUMSIZE FP=( p )->setMinimumSize( *hbqt_par_QSize( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMinimumSize ( int minw, int minh )
 */
HB_FUNC( QT_QWIDGET_SETMINIMUMSIZE_1 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setMinimumSize( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETMINIMUMSIZE_1 FP=( p )->setMinimumSize( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setMinimumWidth ( int minw )
 */
HB_FUNC( QT_QWIDGET_SETMINIMUMWIDTH )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setMinimumWidth( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETMINIMUMWIDTH FP=( p )->setMinimumWidth( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setMouseTracking ( bool enable )
 */
HB_FUNC( QT_QWIDGET_SETMOUSETRACKING )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setMouseTracking( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETMOUSETRACKING FP=( p )->setMouseTracking( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPalette ( const QPalette & )
 */
HB_FUNC( QT_QWIDGET_SETPALETTE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setPalette( *hbqt_par_QPalette( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETPALETTE FP=( p )->setPalette( *hbqt_par_QPalette( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setParent ( QWidget * parent )
 */
HB_FUNC( QT_QWIDGET_SETPARENT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setParent( hbqt_par_QWidget( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETPARENT FP=( p )->setParent( hbqt_par_QWidget( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setParent ( QWidget * parent, Qt::WindowFlags f )
 */
HB_FUNC( QT_QWIDGET_SETPARENT_1 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setParent( hbqt_par_QWidget( 2 ), ( Qt::WindowFlags ) hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETPARENT_1 FP=( p )->setParent( hbqt_par_QWidget( 2 ), ( Qt::WindowFlags ) hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setShortcutAutoRepeat ( int id, bool enable = true )
 */
HB_FUNC( QT_QWIDGET_SETSHORTCUTAUTOREPEAT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setShortcutAutoRepeat( hb_parni( 2 ), hb_parl( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETSHORTCUTAUTOREPEAT FP=( p )->setShortcutAutoRepeat( hb_parni( 2 ), hb_parl( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setShortcutEnabled ( int id, bool enable = true )
 */
HB_FUNC( QT_QWIDGET_SETSHORTCUTENABLED )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setShortcutEnabled( hb_parni( 2 ), hb_parl( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETSHORTCUTENABLED FP=( p )->setShortcutEnabled( hb_parni( 2 ), hb_parl( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setSizeIncrement ( const QSize & )
 */
HB_FUNC( QT_QWIDGET_SETSIZEINCREMENT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setSizeIncrement( *hbqt_par_QSize( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETSIZEINCREMENT FP=( p )->setSizeIncrement( *hbqt_par_QSize( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setSizeIncrement ( int w, int h )
 */
HB_FUNC( QT_QWIDGET_SETSIZEINCREMENT_1 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setSizeIncrement( hb_parni( 2 ), hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETSIZEINCREMENT_1 FP=( p )->setSizeIncrement( hb_parni( 2 ), hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setSizePolicy ( const QSizePolicy & policy )
 */
HB_FUNC( QT_QWIDGET_SETSIZEPOLICY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setSizePolicy( *hbqt_par_QSizePolicy( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETSIZEPOLICY FP=( p )->setSizePolicy( *hbqt_par_QSizePolicy( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setSizePolicy ( QSizePolicy::Policy horizontal, QSizePolicy::Policy vertical )
 */
HB_FUNC( QT_QWIDGET_SETSIZEPOLICY_1 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setSizePolicy( ( QSizePolicy::Policy ) hb_parni( 2 ), ( QSizePolicy::Policy ) hb_parni( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETSIZEPOLICY_1 FP=( p )->setSizePolicy( ( QSizePolicy::Policy ) hb_parni( 2 ), ( QSizePolicy::Policy ) hb_parni( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setStatusTip ( const QString & )
 */
HB_FUNC( QT_QWIDGET_SETSTATUSTIP )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setStatusTip( QWidget::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETSTATUSTIP FP=( p )->setStatusTip( QWidget::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setStyle ( QStyle * style )
 */
HB_FUNC( QT_QWIDGET_SETSTYLE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setStyle( hbqt_par_QStyle( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETSTYLE FP=( p )->setStyle( hbqt_par_QStyle( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setToolTip ( const QString & )
 */
HB_FUNC( QT_QWIDGET_SETTOOLTIP )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setToolTip( QWidget::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETTOOLTIP FP=( p )->setToolTip( QWidget::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setUpdatesEnabled ( bool enable )
 */
HB_FUNC( QT_QWIDGET_SETUPDATESENABLED )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setUpdatesEnabled( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETUPDATESENABLED FP=( p )->setUpdatesEnabled( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setWhatsThis ( const QString & )
 */
HB_FUNC( QT_QWIDGET_SETWHATSTHIS )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setWhatsThis( QWidget::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETWHATSTHIS FP=( p )->setWhatsThis( QWidget::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setWindowFilePath ( const QString & filePath )
 */
HB_FUNC( QT_QWIDGET_SETWINDOWFILEPATH )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setWindowFilePath( QWidget::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETWINDOWFILEPATH FP=( p )->setWindowFilePath( QWidget::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setWindowFlags ( Qt::WindowFlags type )
 */
HB_FUNC( QT_QWIDGET_SETWINDOWFLAGS )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setWindowFlags( ( Qt::WindowFlags ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETWINDOWFLAGS FP=( p )->setWindowFlags( ( Qt::WindowFlags ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setWindowIcon ( const QIcon & icon )
 */
HB_FUNC( QT_QWIDGET_SETWINDOWICON )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setWindowIcon( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QIcon( 2 ) : QIcon( hbqt_par_QString( 2 ) ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETWINDOWICON FP=( p )->setWindowIcon( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QIcon( 2 ) : QIcon( hbqt_par_QString( 2 ) ) ) ); p is NULL" ) );
   }
}

/*
 * void setWindowIconText ( const QString & )
 */
HB_FUNC( QT_QWIDGET_SETWINDOWICONTEXT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setWindowIconText( QWidget::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETWINDOWICONTEXT FP=( p )->setWindowIconText( QWidget::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setWindowModality ( Qt::WindowModality windowModality )
 */
HB_FUNC( QT_QWIDGET_SETWINDOWMODALITY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setWindowModality( ( Qt::WindowModality ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETWINDOWMODALITY FP=( p )->setWindowModality( ( Qt::WindowModality ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setWindowOpacity ( qreal level )
 */
HB_FUNC( QT_QWIDGET_SETWINDOWOPACITY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setWindowOpacity( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETWINDOWOPACITY FP=( p )->setWindowOpacity( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setWindowRole ( const QString & role )
 */
HB_FUNC( QT_QWIDGET_SETWINDOWROLE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setWindowRole( QWidget::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETWINDOWROLE FP=( p )->setWindowRole( QWidget::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void setWindowState ( Qt::WindowStates windowState )
 */
HB_FUNC( QT_QWIDGET_SETWINDOWSTATE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setWindowState( ( Qt::WindowStates ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETWINDOWSTATE FP=( p )->setWindowState( ( Qt::WindowStates ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * QSize size () const
 */
HB_FUNC( QT_QWIDGET_SIZE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->size() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SIZE FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->size() ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual QSize sizeHint () const
 */
HB_FUNC( QT_QWIDGET_SIZEHINT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeHint() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SIZEHINT FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeHint() ), true ) ); p is NULL" ) );
   }
}

/*
 * QSize sizeIncrement () const
 */
HB_FUNC( QT_QWIDGET_SIZEINCREMENT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeIncrement() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SIZEINCREMENT FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeIncrement() ), true ) ); p is NULL" ) );
   }
}

/*
 * QSizePolicy sizePolicy () const
 */
HB_FUNC( QT_QWIDGET_SIZEPOLICY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSizePolicy( new QSizePolicy( ( p )->sizePolicy() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SIZEPOLICY FP=hb_retptrGC( hbqt_gcAllocate_QSizePolicy( new QSizePolicy( ( p )->sizePolicy() ), true ) ); p is NULL" ) );
   }
}

/*
 * void stackUnder ( QWidget * w )
 */
HB_FUNC( QT_QWIDGET_STACKUNDER )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->stackUnder( hbqt_par_QWidget( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_STACKUNDER FP=( p )->stackUnder( hbqt_par_QWidget( 2 ) ); p is NULL" ) );
   }
}

/*
 * QString statusTip () const
 */
HB_FUNC( QT_QWIDGET_STATUSTIP )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retc( ( p )->statusTip().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_STATUSTIP FP=hb_retc( ( p )->statusTip().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QStyle * style () const
 */
HB_FUNC( QT_QWIDGET_STYLE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStyle( ( p )->style(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_STYLE FP=hb_retptrGC( hbqt_gcAllocate_QStyle( ( p )->style(), false ) ); p is NULL" ) );
   }
}

/*
 * QString styleSheet () const
 */
HB_FUNC( QT_QWIDGET_STYLESHEET )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retc( ( p )->styleSheet().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_STYLESHEET FP=hb_retc( ( p )->styleSheet().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * bool testAttribute ( Qt::WidgetAttribute attribute ) const
 */
HB_FUNC( QT_QWIDGET_TESTATTRIBUTE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->testAttribute( ( Qt::WidgetAttribute ) hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_TESTATTRIBUTE FP=hb_retl( ( p )->testAttribute( ( Qt::WidgetAttribute ) hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QString toolTip () const
 */
HB_FUNC( QT_QWIDGET_TOOLTIP )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retc( ( p )->toolTip().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_TOOLTIP FP=hb_retc( ( p )->toolTip().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * bool underMouse () const
 */
HB_FUNC( QT_QWIDGET_UNDERMOUSE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->underMouse() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_UNDERMOUSE FP=hb_retl( ( p )->underMouse() ); p is NULL" ) );
   }
}

/*
 * void unsetCursor ()
 */
HB_FUNC( QT_QWIDGET_UNSETCURSOR )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->unsetCursor();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_UNSETCURSOR FP=( p )->unsetCursor(); p is NULL" ) );
   }
}

/*
 * void unsetLayoutDirection ()
 */
HB_FUNC( QT_QWIDGET_UNSETLAYOUTDIRECTION )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->unsetLayoutDirection();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_UNSETLAYOUTDIRECTION FP=( p )->unsetLayoutDirection(); p is NULL" ) );
   }
}

/*
 * void unsetLocale ()
 */
HB_FUNC( QT_QWIDGET_UNSETLOCALE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->unsetLocale();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_UNSETLOCALE FP=( p )->unsetLocale(); p is NULL" ) );
   }
}

/*
 * void update ( int x, int y, int w, int h )
 */
HB_FUNC( QT_QWIDGET_UPDATE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->update( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_UPDATE FP=( p )->update( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) ); p is NULL" ) );
   }
}

/*
 * void update ( const QRect & rect )
 */
HB_FUNC( QT_QWIDGET_UPDATE_1 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->update( *hbqt_par_QRect( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_UPDATE_1 FP=( p )->update( *hbqt_par_QRect( 2 ) ); p is NULL" ) );
   }
}

/*
 * void update ( const QRegion & rgn )
 */
HB_FUNC( QT_QWIDGET_UPDATE_2 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->update( *hbqt_par_QRegion( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_UPDATE_2 FP=( p )->update( *hbqt_par_QRegion( 2 ) ); p is NULL" ) );
   }
}

/*
 * void updateGeometry ()
 */
HB_FUNC( QT_QWIDGET_UPDATEGEOMETRY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->updateGeometry();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_UPDATEGEOMETRY FP=( p )->updateGeometry(); p is NULL" ) );
   }
}

/*
 * bool updatesEnabled () const
 */
HB_FUNC( QT_QWIDGET_UPDATESENABLED )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->updatesEnabled() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_UPDATESENABLED FP=hb_retl( ( p )->updatesEnabled() ); p is NULL" ) );
   }
}

/*
 * QRegion visibleRegion () const
 */
HB_FUNC( QT_QWIDGET_VISIBLEREGION )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRegion( new QRegion( ( p )->visibleRegion() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_VISIBLEREGION FP=hb_retptrGC( hbqt_gcAllocate_QRegion( new QRegion( ( p )->visibleRegion() ), true ) ); p is NULL" ) );
   }
}

/*
 * QString whatsThis () const
 */
HB_FUNC( QT_QWIDGET_WHATSTHIS )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retc( ( p )->whatsThis().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_WHATSTHIS FP=hb_retc( ( p )->whatsThis().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * int width () const
 */
HB_FUNC( QT_QWIDGET_WIDTH )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( p )->width() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_WIDTH FP=hb_retni( ( p )->width() ); p is NULL" ) );
   }
}

/*
 * QWidget * window () const
 */
HB_FUNC( QT_QWIDGET_WINDOW )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->window(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_WINDOW FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->window(), false ) ); p is NULL" ) );
   }
}

/*
 * QString windowFilePath () const
 */
HB_FUNC( QT_QWIDGET_WINDOWFILEPATH )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retc( ( p )->windowFilePath().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_WINDOWFILEPATH FP=hb_retc( ( p )->windowFilePath().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * Qt::WindowFlags windowFlags () const
 */
HB_FUNC( QT_QWIDGET_WINDOWFLAGS )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( Qt::WindowFlags ) ( p )->windowFlags() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_WINDOWFLAGS FP=hb_retni( ( Qt::WindowFlags ) ( p )->windowFlags() ); p is NULL" ) );
   }
}

/*
 * QIcon windowIcon () const
 */
HB_FUNC( QT_QWIDGET_WINDOWICON )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->windowIcon() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_WINDOWICON FP=hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->windowIcon() ), true ) ); p is NULL" ) );
   }
}

/*
 * QString windowIconText () const
 */
HB_FUNC( QT_QWIDGET_WINDOWICONTEXT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retc( ( p )->windowIconText().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_WINDOWICONTEXT FP=hb_retc( ( p )->windowIconText().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * Qt::WindowModality windowModality () const
 */
HB_FUNC( QT_QWIDGET_WINDOWMODALITY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( Qt::WindowModality ) ( p )->windowModality() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_WINDOWMODALITY FP=hb_retni( ( Qt::WindowModality ) ( p )->windowModality() ); p is NULL" ) );
   }
}

/*
 * qreal windowOpacity () const
 */
HB_FUNC( QT_QWIDGET_WINDOWOPACITY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retnd( ( p )->windowOpacity() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_WINDOWOPACITY FP=hb_retnd( ( p )->windowOpacity() ); p is NULL" ) );
   }
}

/*
 * QString windowRole () const
 */
HB_FUNC( QT_QWIDGET_WINDOWROLE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retc( ( p )->windowRole().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_WINDOWROLE FP=hb_retc( ( p )->windowRole().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * Qt::WindowStates windowState () const
 */
HB_FUNC( QT_QWIDGET_WINDOWSTATE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( Qt::WindowStates ) ( p )->windowState() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_WINDOWSTATE FP=hb_retni( ( Qt::WindowStates ) ( p )->windowState() ); p is NULL" ) );
   }
}

/*
 * QString windowTitle () const
 */
HB_FUNC( QT_QWIDGET_WINDOWTITLE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retc( ( p )->windowTitle().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_WINDOWTITLE FP=hb_retc( ( p )->windowTitle().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * Qt::WindowType windowType () const
 */
HB_FUNC( QT_QWIDGET_WINDOWTYPE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( Qt::WindowType ) ( p )->windowType() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_WINDOWTYPE FP=hb_retni( ( Qt::WindowType ) ( p )->windowType() ); p is NULL" ) );
   }
}

/*
 * int x () const
 */
HB_FUNC( QT_QWIDGET_X )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( p )->x() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_X FP=hb_retni( ( p )->x() ); p is NULL" ) );
   }
}

/*
 * int y () const
 */
HB_FUNC( QT_QWIDGET_Y )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( p )->y() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_Y FP=hb_retni( ( p )->y() ); p is NULL" ) );
   }
}

/*
 * QWidget * keyboardGrabber ()
 */
HB_FUNC( QT_QWIDGET_KEYBOARDGRABBER )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->keyboardGrabber(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_KEYBOARDGRABBER FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->keyboardGrabber(), false ) ); p is NULL" ) );
   }
}

/*
 * QWidget * mouseGrabber ()
 */
HB_FUNC( QT_QWIDGET_MOUSEGRABBER )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->mouseGrabber(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_MOUSEGRABBER FP=hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->mouseGrabber(), false ) ); p is NULL" ) );
   }
}

/*
 * void setTabOrder ( QWidget * first, QWidget * second )
 */
HB_FUNC( QT_QWIDGET_SETTABORDER )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setTabOrder( hbqt_par_QWidget( 2 ), hbqt_par_QWidget( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETTABORDER FP=( p )->setTabOrder( hbqt_par_QWidget( 2 ), hbqt_par_QWidget( 3 ) ); p is NULL" ) );
   }
}

/*
 * bool close ()
 */
HB_FUNC( QT_QWIDGET_CLOSE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->close() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_CLOSE FP=hb_retl( ( p )->close() ); p is NULL" ) );
   }
}

/*
 * void hide ()
 */
HB_FUNC( QT_QWIDGET_HIDE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->hide();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_HIDE FP=( p )->hide(); p is NULL" ) );
   }
}

/*
 * void lower ()
 */
HB_FUNC( QT_QWIDGET_LOWER )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->lower();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_LOWER FP=( p )->lower(); p is NULL" ) );
   }
}

/*
 * void raise ()
 */
HB_FUNC( QT_QWIDGET_RAISE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->raise();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_RAISE FP=( p )->raise(); p is NULL" ) );
   }
}

/*
 * void repaint ()
 */
HB_FUNC( QT_QWIDGET_REPAINT_3 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->repaint();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_REPAINT_3 FP=( p )->repaint(); p is NULL" ) );
   }
}

/*
 * void setDisabled ( bool disable )
 */
HB_FUNC( QT_QWIDGET_SETDISABLED )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setDisabled( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETDISABLED FP=( p )->setDisabled( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setEnabled ( bool enable )
 */
HB_FUNC( QT_QWIDGET_SETENABLED )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setEnabled( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETENABLED FP=( p )->setEnabled( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFocus ()
 */
HB_FUNC( QT_QWIDGET_SETFOCUS_1 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setFocus();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETFOCUS_1 FP=( p )->setFocus(); p is NULL" ) );
   }
}

/*
 * void setHidden ( bool hidden )
 */
HB_FUNC( QT_QWIDGET_SETHIDDEN )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setHidden( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETHIDDEN FP=( p )->setHidden( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setStyleSheet ( const QString & styleSheet )
 */
HB_FUNC( QT_QWIDGET_SETSTYLESHEET )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setStyleSheet( QWidget::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETSTYLESHEET FP=( p )->setStyleSheet( QWidget::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * virtual void setVisible ( bool visible )
 */
HB_FUNC( QT_QWIDGET_SETVISIBLE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setVisible( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETVISIBLE FP=( p )->setVisible( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setWindowModified ( bool modified )
 */
HB_FUNC( QT_QWIDGET_SETWINDOWMODIFIED )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setWindowModified( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETWINDOWMODIFIED FP=( p )->setWindowModified( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setWindowTitle ( const QString & title )
 */
HB_FUNC( QT_QWIDGET_SETWINDOWTITLE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setWindowTitle( QWidget::tr( hb_parc( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SETWINDOWTITLE FP=( p )->setWindowTitle( QWidget::tr( hb_parc( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void show ()
 */
HB_FUNC( QT_QWIDGET_SHOW )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->show();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SHOW FP=( p )->show(); p is NULL" ) );
   }
}

/*
 * void showFullScreen ()
 */
HB_FUNC( QT_QWIDGET_SHOWFULLSCREEN )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->showFullScreen();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SHOWFULLSCREEN FP=( p )->showFullScreen(); p is NULL" ) );
   }
}

/*
 * void showMaximized ()
 */
HB_FUNC( QT_QWIDGET_SHOWMAXIMIZED )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->showMaximized();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SHOWMAXIMIZED FP=( p )->showMaximized(); p is NULL" ) );
   }
}

/*
 * void showMinimized ()
 */
HB_FUNC( QT_QWIDGET_SHOWMINIMIZED )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->showMinimized();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SHOWMINIMIZED FP=( p )->showMinimized(); p is NULL" ) );
   }
}

/*
 * void showNormal ()
 */
HB_FUNC( QT_QWIDGET_SHOWNORMAL )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->showNormal();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_SHOWNORMAL FP=( p )->showNormal(); p is NULL" ) );
   }
}

/*
 * void update ()
 */
HB_FUNC( QT_QWIDGET_UPDATE_3 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->update();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QWIDGET_UPDATE_3 FP=( p )->update(); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
