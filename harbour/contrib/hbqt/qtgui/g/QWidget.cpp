/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project QT wrapper
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * For full copyright message and credits, see: CREDITS.txt
 *
 */

#include "hbqtcore.h"
#include "hbqtgui.h"

#if QT_VERSION >= 0x040500

/*
 *  enum RenderFlag { DrawWindowBackground, DrawChildren, IgnoreMask }
 *  # From QPaintDevice : Parent Class
 *  enum PaintDeviceMetric { PdmWidth, PdmHeight, PdmWidthMM, PdmHeightMM, ..., PdmPhysicalDpiY }
 */

/*
 *  Constructed[ 206/208 [ 99.04% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  void addActions ( QList<QAction *> actions )
 *  void insertActions ( QAction * before, QList<QAction *> actions )
 *
 *  *** Commented out protostypes ***
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
 *  // WId winId () const
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
   HBQT_GC_T_QWidget * p = ( HBQT_GC_T_QWidget * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QWidget * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
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

/* bool acceptDrops () const */
HB_FUNC( QT_QWIDGET_ACCEPTDROPS )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->acceptDrops() );
}

/* QList<QAction *> actions () const */
HB_FUNC( QT_QWIDGET_ACTIONS )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QAction *>( ( p )->actions() ), true ) );
}

/* void activateWindow () */
HB_FUNC( QT_QWIDGET_ACTIVATEWINDOW )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->activateWindow();
}

/* void addAction ( QAction * action ) */
HB_FUNC( QT_QWIDGET_ADDACTION )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->addAction( hbqt_par_QAction( 2 ) );
}

/* void adjustSize () */
HB_FUNC( QT_QWIDGET_ADJUSTSIZE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->adjustSize();
}

/* bool autoFillBackground () const */
HB_FUNC( QT_QWIDGET_AUTOFILLBACKGROUND )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->autoFillBackground() );
}

/* QPalette::ColorRole backgroundRole () const */
HB_FUNC( QT_QWIDGET_BACKGROUNDROLE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( QPalette::ColorRole ) ( p )->backgroundRole() );
}

/* QSize baseSize () const */
HB_FUNC( QT_QWIDGET_BASESIZE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->baseSize() ), true ) );
}

/* QWidget * childAt ( int x, int y ) const */
HB_FUNC( QT_QWIDGET_CHILDAT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->childAt( hb_parni( 2 ), hb_parni( 3 ) ), false ) );
}

/* QWidget * childAt ( const QPoint & p ) const */
HB_FUNC( QT_QWIDGET_CHILDAT_1 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->childAt( *hbqt_par_QPoint( 2 ) ), false ) );
}

/* QRect childrenRect () const */
HB_FUNC( QT_QWIDGET_CHILDRENRECT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->childrenRect() ), true ) );
}

/* QRegion childrenRegion () const */
HB_FUNC( QT_QWIDGET_CHILDRENREGION )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRegion( new QRegion( ( p )->childrenRegion() ), true ) );
}

/* void clearFocus () */
HB_FUNC( QT_QWIDGET_CLEARFOCUS )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->clearFocus();
}

/* void clearMask () */
HB_FUNC( QT_QWIDGET_CLEARMASK )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->clearMask();
}

/* QRect contentsRect () const */
HB_FUNC( QT_QWIDGET_CONTENTSRECT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->contentsRect() ), true ) );
}

/* Qt::ContextMenuPolicy contextMenuPolicy () const */
HB_FUNC( QT_QWIDGET_CONTEXTMENUPOLICY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( Qt::ContextMenuPolicy ) ( p )->contextMenuPolicy() );
}

/* QCursor cursor () const */
HB_FUNC( QT_QWIDGET_CURSOR )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QCursor( new QCursor( ( p )->cursor() ), true ) );
}

/* void ensurePolished () const */
HB_FUNC( QT_QWIDGET_ENSUREPOLISHED )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->ensurePolished();
}

/* Qt::FocusPolicy focusPolicy () const */
HB_FUNC( QT_QWIDGET_FOCUSPOLICY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( Qt::FocusPolicy ) ( p )->focusPolicy() );
}

/* QWidget * focusProxy () const */
HB_FUNC( QT_QWIDGET_FOCUSPROXY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->focusProxy(), false ) );
}

/* QWidget * focusWidget () const */
HB_FUNC( QT_QWIDGET_FOCUSWIDGET )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->focusWidget(), false ) );
}

/* const QFont & font () const */
HB_FUNC( QT_QWIDGET_FONT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) );
}

/* QFontInfo fontInfo () const */
HB_FUNC( QT_QWIDGET_FONTINFO )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFontInfo( new QFontInfo( ( p )->fontInfo() ), true ) );
}

/* QFontMetrics fontMetrics () const */
HB_FUNC( QT_QWIDGET_FONTMETRICS )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFontMetrics( new QFontMetrics( ( p )->fontMetrics() ), true ) );
}

/* QPalette::ColorRole foregroundRole () const */
HB_FUNC( QT_QWIDGET_FOREGROUNDROLE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( QPalette::ColorRole ) ( p )->foregroundRole() );
}

/* QRect frameGeometry () const */
HB_FUNC( QT_QWIDGET_FRAMEGEOMETRY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->frameGeometry() ), true ) );
}

/* QSize frameSize () const */
HB_FUNC( QT_QWIDGET_FRAMESIZE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->frameSize() ), true ) );
}

/* const QRect & geometry () const */
HB_FUNC( QT_QWIDGET_GEOMETRY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->geometry() ), true ) );
}

/* void getContentsMargins ( int * left, int * top, int * right, int * bottom ) const */
HB_FUNC( QT_QWIDGET_GETCONTENTSMARGINS )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   int iLeft = 0;
   int iTop = 0;
   int iRight = 0;
   int iBottom = 0;

   if( p )
      ( p )->getContentsMargins( &iLeft, &iTop, &iRight, &iBottom );

   hb_storni( iLeft, 2 );
   hb_storni( iTop, 3 );
   hb_storni( iRight, 4 );
   hb_storni( iBottom, 5 );
}

/* void grabKeyboard () */
HB_FUNC( QT_QWIDGET_GRABKEYBOARD )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->grabKeyboard();
}

/* void grabMouse () */
HB_FUNC( QT_QWIDGET_GRABMOUSE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->grabMouse();
}

/* void grabMouse ( const QCursor & cursor ) */
HB_FUNC( QT_QWIDGET_GRABMOUSE_1 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->grabMouse( *hbqt_par_QCursor( 2 ) );
}

/* int grabShortcut ( const QKeySequence & key, Qt::ShortcutContext context = Qt::WindowShortcut ) */
HB_FUNC( QT_QWIDGET_GRABSHORTCUT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( p )->grabShortcut( *hbqt_par_QKeySequence( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ShortcutContext ) hb_parni( 3 ) : ( Qt::ShortcutContext ) Qt::WindowShortcut ) ) );
}

/* bool hasFocus () const */
HB_FUNC( QT_QWIDGET_HASFOCUS )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->hasFocus() );
}

/* bool hasMouseTracking () const */
HB_FUNC( QT_QWIDGET_HASMOUSETRACKING )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->hasMouseTracking() );
}

/* int height () const */
HB_FUNC( QT_QWIDGET_HEIGHT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( p )->height() );
}

/* virtual int heightForWidth ( int w ) const */
HB_FUNC( QT_QWIDGET_HEIGHTFORWIDTH )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( p )->heightForWidth( hb_parni( 2 ) ) );
}

/* virtual QVariant inputMethodQuery ( Qt::InputMethodQuery query ) const */
HB_FUNC( QT_QWIDGET_INPUTMETHODQUERY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->inputMethodQuery( ( Qt::InputMethodQuery ) hb_parni( 2 ) ) ), true ) );
}

/* void insertAction ( QAction * before, QAction * action ) */
HB_FUNC( QT_QWIDGET_INSERTACTION )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->insertAction( hbqt_par_QAction( 2 ), hbqt_par_QAction( 3 ) );
}

/* bool isActiveWindow () const */
HB_FUNC( QT_QWIDGET_ISACTIVEWINDOW )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->isActiveWindow() );
}

/* bool isAncestorOf ( const QWidget * child ) const */
HB_FUNC( QT_QWIDGET_ISANCESTOROF )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->isAncestorOf( hbqt_par_QWidget( 2 ) ) );
}

/* bool isEnabled () const */
HB_FUNC( QT_QWIDGET_ISENABLED )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->isEnabled() );
}

/* bool isEnabledTo ( QWidget * ancestor ) const */
HB_FUNC( QT_QWIDGET_ISENABLEDTO )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->isEnabledTo( hbqt_par_QWidget( 2 ) ) );
}

/* bool isFullScreen () const */
HB_FUNC( QT_QWIDGET_ISFULLSCREEN )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->isFullScreen() );
}

/* bool isHidden () const */
HB_FUNC( QT_QWIDGET_ISHIDDEN )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->isHidden() );
}

/* bool isMaximized () const */
HB_FUNC( QT_QWIDGET_ISMAXIMIZED )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->isMaximized() );
}

/* bool isMinimized () const */
HB_FUNC( QT_QWIDGET_ISMINIMIZED )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->isMinimized() );
}

/* bool isModal () const */
HB_FUNC( QT_QWIDGET_ISMODAL )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->isModal() );
}

/* bool isVisible () const */
HB_FUNC( QT_QWIDGET_ISVISIBLE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->isVisible() );
}

/* bool isVisibleTo ( QWidget * ancestor ) const */
HB_FUNC( QT_QWIDGET_ISVISIBLETO )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->isVisibleTo( hbqt_par_QWidget( 2 ) ) );
}

/* bool isWindow () const */
HB_FUNC( QT_QWIDGET_ISWINDOW )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->isWindow() );
}

/* bool isWindowModified () const */
HB_FUNC( QT_QWIDGET_ISWINDOWMODIFIED )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->isWindowModified() );
}

/* QLayout * layout () const */
HB_FUNC( QT_QWIDGET_LAYOUT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QLayout( ( p )->layout(), false ) );
}

/* Qt::LayoutDirection layoutDirection () const */
HB_FUNC( QT_QWIDGET_LAYOUTDIRECTION )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( Qt::LayoutDirection ) ( p )->layoutDirection() );
}

/* QLocale locale () const */
HB_FUNC( QT_QWIDGET_LOCALE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QLocale( new QLocale( ( p )->locale() ), true ) );
}

/* QPoint mapFrom ( QWidget * parent, const QPoint & pos ) const */
HB_FUNC( QT_QWIDGET_MAPFROM )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->mapFrom( hbqt_par_QWidget( 2 ), *hbqt_par_QPoint( 3 ) ) ), true ) );
}

/* QPoint mapFromGlobal ( const QPoint & pos ) const */
HB_FUNC( QT_QWIDGET_MAPFROMGLOBAL )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->mapFromGlobal( *hbqt_par_QPoint( 2 ) ) ), true ) );
}

/* QPoint mapFromParent ( const QPoint & pos ) const */
HB_FUNC( QT_QWIDGET_MAPFROMPARENT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->mapFromParent( *hbqt_par_QPoint( 2 ) ) ), true ) );
}

/* QPoint mapTo ( QWidget * parent, const QPoint & pos ) const */
HB_FUNC( QT_QWIDGET_MAPTO )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->mapTo( hbqt_par_QWidget( 2 ), *hbqt_par_QPoint( 3 ) ) ), true ) );
}

/* QPoint mapToGlobal ( const QPoint & pos ) const */
HB_FUNC( QT_QWIDGET_MAPTOGLOBAL )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->mapToGlobal( *hbqt_par_QPoint( 2 ) ) ), true ) );
}

/* QPoint mapToParent ( const QPoint & pos ) const */
HB_FUNC( QT_QWIDGET_MAPTOPARENT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->mapToParent( *hbqt_par_QPoint( 2 ) ) ), true ) );
}

/* QRegion mask () const */
HB_FUNC( QT_QWIDGET_MASK )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRegion( new QRegion( ( p )->mask() ), true ) );
}

/* int maximumHeight () const */
HB_FUNC( QT_QWIDGET_MAXIMUMHEIGHT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( p )->maximumHeight() );
}

/* QSize maximumSize () const */
HB_FUNC( QT_QWIDGET_MAXIMUMSIZE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->maximumSize() ), true ) );
}

/* int maximumWidth () const */
HB_FUNC( QT_QWIDGET_MAXIMUMWIDTH )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( p )->maximumWidth() );
}

/* int minimumHeight () const */
HB_FUNC( QT_QWIDGET_MINIMUMHEIGHT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( p )->minimumHeight() );
}

/* QSize minimumSize () const */
HB_FUNC( QT_QWIDGET_MINIMUMSIZE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->minimumSize() ), true ) );
}

/* virtual QSize minimumSizeHint () const */
HB_FUNC( QT_QWIDGET_MINIMUMSIZEHINT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->minimumSizeHint() ), true ) );
}

/* int minimumWidth () const */
HB_FUNC( QT_QWIDGET_MINIMUMWIDTH )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( p )->minimumWidth() );
}

/* void move ( int x, int y ) */
HB_FUNC( QT_QWIDGET_MOVE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->move( hb_parni( 2 ), hb_parni( 3 ) );
}

/* void move ( const QPoint & ) */
HB_FUNC( QT_QWIDGET_MOVE_1 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->move( *hbqt_par_QPoint( 2 ) );
}

/* QWidget * nativeParentWidget () const */
HB_FUNC( QT_QWIDGET_NATIVEPARENTWIDGET )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->nativeParentWidget(), false ) );
}

/* QWidget * nextInFocusChain () const */
HB_FUNC( QT_QWIDGET_NEXTINFOCUSCHAIN )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->nextInFocusChain(), false ) );
}

/* QRect normalGeometry () const */
HB_FUNC( QT_QWIDGET_NORMALGEOMETRY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->normalGeometry() ), true ) );
}

/* void overrideWindowFlags ( Qt::WindowFlags flags ) */
HB_FUNC( QT_QWIDGET_OVERRIDEWINDOWFLAGS )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->overrideWindowFlags( ( Qt::WindowFlags ) hb_parni( 2 ) );
}

/* virtual QPaintEngine * paintEngine () const */
HB_FUNC( QT_QWIDGET_PAINTENGINE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPaintEngine( ( p )->paintEngine(), false ) );
}

/* const QPalette & palette () const */
HB_FUNC( QT_QWIDGET_PALETTE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPalette( new QPalette( ( p )->palette() ), true ) );
}

/* QWidget * parentWidget () const */
HB_FUNC( QT_QWIDGET_PARENTWIDGET )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->parentWidget(), false ) );
}

/* QPoint pos () const */
HB_FUNC( QT_QWIDGET_POS )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->pos() ), true ) );
}

/* QRect rect () const */
HB_FUNC( QT_QWIDGET_RECT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->rect() ), true ) );
}

/* void releaseKeyboard () */
HB_FUNC( QT_QWIDGET_RELEASEKEYBOARD )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->releaseKeyboard();
}

/* void releaseMouse () */
HB_FUNC( QT_QWIDGET_RELEASEMOUSE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->releaseMouse();
}

/* void releaseShortcut ( int id ) */
HB_FUNC( QT_QWIDGET_RELEASESHORTCUT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->releaseShortcut( hb_parni( 2 ) );
}

/* void removeAction ( QAction * action ) */
HB_FUNC( QT_QWIDGET_REMOVEACTION )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->removeAction( hbqt_par_QAction( 2 ) );
}

/* void repaint ( int x, int y, int w, int h ) */
HB_FUNC( QT_QWIDGET_REPAINT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->repaint( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/* void repaint ( const QRect & rect ) */
HB_FUNC( QT_QWIDGET_REPAINT_1 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->repaint( *hbqt_par_QRect( 2 ) );
}

/* void repaint ( const QRegion & rgn ) */
HB_FUNC( QT_QWIDGET_REPAINT_2 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->repaint( *hbqt_par_QRegion( 2 ) );
}

/* void resize ( int w, int h ) */
HB_FUNC( QT_QWIDGET_RESIZE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->resize( hb_parni( 2 ), hb_parni( 3 ) );
}

/* void resize ( const QSize & ) */
HB_FUNC( QT_QWIDGET_RESIZE_1 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->resize( *hbqt_par_QSize( 2 ) );
}

/* bool restoreGeometry ( const QByteArray & geometry ) */
HB_FUNC( QT_QWIDGET_RESTOREGEOMETRY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->restoreGeometry( *hbqt_par_QByteArray( 2 ) ) );
}

/* QByteArray saveGeometry () const */
HB_FUNC( QT_QWIDGET_SAVEGEOMETRY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->saveGeometry() ), true ) );
}

/* void scroll ( int dx, int dy ) */
HB_FUNC( QT_QWIDGET_SCROLL )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->scroll( hb_parni( 2 ), hb_parni( 3 ) );
}

/* void scroll ( int dx, int dy, const QRect & r ) */
HB_FUNC( QT_QWIDGET_SCROLL_1 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->scroll( hb_parni( 2 ), hb_parni( 3 ), *hbqt_par_QRect( 4 ) );
}

/* void setAcceptDrops ( bool on ) */
HB_FUNC( QT_QWIDGET_SETACCEPTDROPS )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setAcceptDrops( hb_parl( 2 ) );
}

/* void setAttribute ( Qt::WidgetAttribute attribute, bool on = true ) */
HB_FUNC( QT_QWIDGET_SETATTRIBUTE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setAttribute( ( Qt::WidgetAttribute ) hb_parni( 2 ), hb_parl( 3 ) );
}

/* void setAutoFillBackground ( bool enabled ) */
HB_FUNC( QT_QWIDGET_SETAUTOFILLBACKGROUND )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setAutoFillBackground( hb_parl( 2 ) );
}

/* void setBackgroundRole ( QPalette::ColorRole role ) */
HB_FUNC( QT_QWIDGET_SETBACKGROUNDROLE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setBackgroundRole( ( QPalette::ColorRole ) hb_parni( 2 ) );
}

/* void setBaseSize ( const QSize & ) */
HB_FUNC( QT_QWIDGET_SETBASESIZE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setBaseSize( *hbqt_par_QSize( 2 ) );
}

/* void setBaseSize ( int basew, int baseh ) */
HB_FUNC( QT_QWIDGET_SETBASESIZE_1 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setBaseSize( hb_parni( 2 ), hb_parni( 3 ) );
}

/* void setContentsMargins ( int left, int top, int right, int bottom ) */
HB_FUNC( QT_QWIDGET_SETCONTENTSMARGINS )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setContentsMargins( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/* void setContextMenuPolicy ( Qt::ContextMenuPolicy policy ) */
HB_FUNC( QT_QWIDGET_SETCONTEXTMENUPOLICY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setContextMenuPolicy( ( Qt::ContextMenuPolicy ) hb_parni( 2 ) );
}

/* void setCursor ( const QCursor & ) */
HB_FUNC( QT_QWIDGET_SETCURSOR )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setCursor( *hbqt_par_QCursor( 2 ) );
}

/* void setFixedHeight ( int h ) */
HB_FUNC( QT_QWIDGET_SETFIXEDHEIGHT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setFixedHeight( hb_parni( 2 ) );
}

/* void setFixedSize ( const QSize & s ) */
HB_FUNC( QT_QWIDGET_SETFIXEDSIZE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setFixedSize( *hbqt_par_QSize( 2 ) );
}

/* void setFixedSize ( int w, int h ) */
HB_FUNC( QT_QWIDGET_SETFIXEDSIZE_1 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setFixedSize( hb_parni( 2 ), hb_parni( 3 ) );
}

/* void setFixedWidth ( int w ) */
HB_FUNC( QT_QWIDGET_SETFIXEDWIDTH )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setFixedWidth( hb_parni( 2 ) );
}

/* void setFocus ( Qt::FocusReason reason ) */
HB_FUNC( QT_QWIDGET_SETFOCUS )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setFocus( ( Qt::FocusReason ) hb_parni( 2 ) );
}

/* void setFocusPolicy ( Qt::FocusPolicy policy ) */
HB_FUNC( QT_QWIDGET_SETFOCUSPOLICY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setFocusPolicy( ( Qt::FocusPolicy ) hb_parni( 2 ) );
}

/* void setFocusProxy ( QWidget * w ) */
HB_FUNC( QT_QWIDGET_SETFOCUSPROXY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setFocusProxy( hbqt_par_QWidget( 2 ) );
}

/* void setFont ( const QFont & ) */
HB_FUNC( QT_QWIDGET_SETFONT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setFont( *hbqt_par_QFont( 2 ) );
}

/* void setForegroundRole ( QPalette::ColorRole role ) */
HB_FUNC( QT_QWIDGET_SETFOREGROUNDROLE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setForegroundRole( ( QPalette::ColorRole ) hb_parni( 2 ) );
}

/* void setGeometry ( const QRect & ) */
HB_FUNC( QT_QWIDGET_SETGEOMETRY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setGeometry( *hbqt_par_QRect( 2 ) );
}

/* void setGeometry ( int x, int y, int w, int h ) */
HB_FUNC( QT_QWIDGET_SETGEOMETRY_1 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setGeometry( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/* void setLayout ( QLayout * layout ) */
HB_FUNC( QT_QWIDGET_SETLAYOUT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setLayout( hbqt_par_QLayout( 2 ) );
}

/* void setLayoutDirection ( Qt::LayoutDirection direction ) */
HB_FUNC( QT_QWIDGET_SETLAYOUTDIRECTION )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setLayoutDirection( ( Qt::LayoutDirection ) hb_parni( 2 ) );
}

/* void setLocale ( const QLocale & locale ) */
HB_FUNC( QT_QWIDGET_SETLOCALE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setLocale( *hbqt_par_QLocale( 2 ) );
}

/* void setMask ( const QBitmap & bitmap ) */
HB_FUNC( QT_QWIDGET_SETMASK )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setMask( *hbqt_par_QBitmap( 2 ) );
}

/* void setMask ( const QRegion & region ) */
HB_FUNC( QT_QWIDGET_SETMASK_1 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setMask( *hbqt_par_QRegion( 2 ) );
}

/* void setMaximumHeight ( int maxh ) */
HB_FUNC( QT_QWIDGET_SETMAXIMUMHEIGHT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setMaximumHeight( hb_parni( 2 ) );
}

/* void setMaximumSize ( const QSize & ) */
HB_FUNC( QT_QWIDGET_SETMAXIMUMSIZE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setMaximumSize( *hbqt_par_QSize( 2 ) );
}

/* void setMaximumSize ( int maxw, int maxh ) */
HB_FUNC( QT_QWIDGET_SETMAXIMUMSIZE_1 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setMaximumSize( hb_parni( 2 ), hb_parni( 3 ) );
}

/* void setMaximumWidth ( int maxw ) */
HB_FUNC( QT_QWIDGET_SETMAXIMUMWIDTH )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setMaximumWidth( hb_parni( 2 ) );
}

/* void setMinimumHeight ( int minh ) */
HB_FUNC( QT_QWIDGET_SETMINIMUMHEIGHT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setMinimumHeight( hb_parni( 2 ) );
}

/* void setMinimumSize ( const QSize & ) */
HB_FUNC( QT_QWIDGET_SETMINIMUMSIZE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setMinimumSize( *hbqt_par_QSize( 2 ) );
}

/* void setMinimumSize ( int minw, int minh ) */
HB_FUNC( QT_QWIDGET_SETMINIMUMSIZE_1 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setMinimumSize( hb_parni( 2 ), hb_parni( 3 ) );
}

/* void setMinimumWidth ( int minw ) */
HB_FUNC( QT_QWIDGET_SETMINIMUMWIDTH )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setMinimumWidth( hb_parni( 2 ) );
}

/* void setMouseTracking ( bool enable ) */
HB_FUNC( QT_QWIDGET_SETMOUSETRACKING )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setMouseTracking( hb_parl( 2 ) );
}

/* void setPalette ( const QPalette & ) */
HB_FUNC( QT_QWIDGET_SETPALETTE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setPalette( *hbqt_par_QPalette( 2 ) );
}

/* void setParent ( QWidget * parent ) */
HB_FUNC( QT_QWIDGET_SETPARENT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setParent( hbqt_par_QWidget( 2 ) );
}

/* void setParent ( QWidget * parent, Qt::WindowFlags f ) */
HB_FUNC( QT_QWIDGET_SETPARENT_1 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setParent( hbqt_par_QWidget( 2 ), ( Qt::WindowFlags ) hb_parni( 3 ) );
}

/* void setShortcutAutoRepeat ( int id, bool enable = true ) */
HB_FUNC( QT_QWIDGET_SETSHORTCUTAUTOREPEAT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setShortcutAutoRepeat( hb_parni( 2 ), hb_parl( 3 ) );
}

/* void setShortcutEnabled ( int id, bool enable = true ) */
HB_FUNC( QT_QWIDGET_SETSHORTCUTENABLED )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setShortcutEnabled( hb_parni( 2 ), hb_parl( 3 ) );
}

/* void setSizeIncrement ( const QSize & ) */
HB_FUNC( QT_QWIDGET_SETSIZEINCREMENT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setSizeIncrement( *hbqt_par_QSize( 2 ) );
}

/* void setSizeIncrement ( int w, int h ) */
HB_FUNC( QT_QWIDGET_SETSIZEINCREMENT_1 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setSizeIncrement( hb_parni( 2 ), hb_parni( 3 ) );
}

/* void setSizePolicy ( const QSizePolicy & policy ) */
HB_FUNC( QT_QWIDGET_SETSIZEPOLICY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setSizePolicy( *hbqt_par_QSizePolicy( 2 ) );
}

/* void setSizePolicy ( QSizePolicy::Policy horizontal, QSizePolicy::Policy vertical ) */
HB_FUNC( QT_QWIDGET_SETSIZEPOLICY_1 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setSizePolicy( ( QSizePolicy::Policy ) hb_parni( 2 ), ( QSizePolicy::Policy ) hb_parni( 3 ) );
}

/* void setStatusTip ( const QString & ) */
HB_FUNC( QT_QWIDGET_SETSTATUSTIP )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
   {
      void * pText;
      ( p )->setStatusTip( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setStyle ( QStyle * style ) */
HB_FUNC( QT_QWIDGET_SETSTYLE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setStyle( hbqt_par_QStyle( 2 ) );
}

/* void setToolTip ( const QString & ) */
HB_FUNC( QT_QWIDGET_SETTOOLTIP )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
   {
      void * pText;
      ( p )->setToolTip( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setUpdatesEnabled ( bool enable ) */
HB_FUNC( QT_QWIDGET_SETUPDATESENABLED )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setUpdatesEnabled( hb_parl( 2 ) );
}

/* void setWhatsThis ( const QString & ) */
HB_FUNC( QT_QWIDGET_SETWHATSTHIS )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
   {
      void * pText;
      ( p )->setWhatsThis( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setWindowFilePath ( const QString & filePath ) */
HB_FUNC( QT_QWIDGET_SETWINDOWFILEPATH )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
   {
      void * pText;
      ( p )->setWindowFilePath( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setWindowFlags ( Qt::WindowFlags type ) */
HB_FUNC( QT_QWIDGET_SETWINDOWFLAGS )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setWindowFlags( ( Qt::WindowFlags ) hb_parni( 2 ) );
}

/* void setWindowIcon ( const QIcon & icon ) */
HB_FUNC( QT_QWIDGET_SETWINDOWICON )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setWindowIcon( ( HB_ISCHAR( 2 ) ? QIcon( hbqt_par_QString( 2 ) ) : *hbqt_par_QIcon( 2 )) );
}

/* void setWindowIconText ( const QString & ) */
HB_FUNC( QT_QWIDGET_SETWINDOWICONTEXT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
   {
      void * pText;
      ( p )->setWindowIconText( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setWindowModality ( Qt::WindowModality windowModality ) */
HB_FUNC( QT_QWIDGET_SETWINDOWMODALITY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setWindowModality( ( Qt::WindowModality ) hb_parni( 2 ) );
}

/* void setWindowOpacity ( qreal level ) */
HB_FUNC( QT_QWIDGET_SETWINDOWOPACITY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setWindowOpacity( hb_parnd( 2 ) );
}

/* void setWindowRole ( const QString & role ) */
HB_FUNC( QT_QWIDGET_SETWINDOWROLE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
   {
      void * pText;
      ( p )->setWindowRole( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void setWindowState ( Qt::WindowStates windowState ) */
HB_FUNC( QT_QWIDGET_SETWINDOWSTATE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setWindowState( ( Qt::WindowStates ) hb_parni( 2 ) );
}

/* QSize size () const */
HB_FUNC( QT_QWIDGET_SIZE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->size() ), true ) );
}

/* virtual QSize sizeHint () const */
HB_FUNC( QT_QWIDGET_SIZEHINT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeHint() ), true ) );
}

/* QSize sizeIncrement () const */
HB_FUNC( QT_QWIDGET_SIZEINCREMENT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->sizeIncrement() ), true ) );
}

/* QSizePolicy sizePolicy () const */
HB_FUNC( QT_QWIDGET_SIZEPOLICY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSizePolicy( new QSizePolicy( ( p )->sizePolicy() ), true ) );
}

/* void stackUnder ( QWidget * w ) */
HB_FUNC( QT_QWIDGET_STACKUNDER )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->stackUnder( hbqt_par_QWidget( 2 ) );
}

/* QString statusTip () const */
HB_FUNC( QT_QWIDGET_STATUSTIP )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retstr_utf8( ( p )->statusTip().toUtf8().data() );
}

/* QStyle * style () const */
HB_FUNC( QT_QWIDGET_STYLE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStyle( ( p )->style(), false ) );
}

/* QString styleSheet () const */
HB_FUNC( QT_QWIDGET_STYLESHEET )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retstr_utf8( ( p )->styleSheet().toUtf8().data() );
}

/* bool testAttribute ( Qt::WidgetAttribute attribute ) const */
HB_FUNC( QT_QWIDGET_TESTATTRIBUTE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->testAttribute( ( Qt::WidgetAttribute ) hb_parni( 2 ) ) );
}

/* QString toolTip () const */
HB_FUNC( QT_QWIDGET_TOOLTIP )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retstr_utf8( ( p )->toolTip().toUtf8().data() );
}

/* bool underMouse () const */
HB_FUNC( QT_QWIDGET_UNDERMOUSE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->underMouse() );
}

/* void unsetCursor () */
HB_FUNC( QT_QWIDGET_UNSETCURSOR )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->unsetCursor();
}

/* void unsetLayoutDirection () */
HB_FUNC( QT_QWIDGET_UNSETLAYOUTDIRECTION )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->unsetLayoutDirection();
}

/* void unsetLocale () */
HB_FUNC( QT_QWIDGET_UNSETLOCALE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->unsetLocale();
}

/* void update ( int x, int y, int w, int h ) */
HB_FUNC( QT_QWIDGET_UPDATE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->update( hb_parni( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ) );
}

/* void update ( const QRect & rect ) */
HB_FUNC( QT_QWIDGET_UPDATE_1 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->update( *hbqt_par_QRect( 2 ) );
}

/* void update ( const QRegion & rgn ) */
HB_FUNC( QT_QWIDGET_UPDATE_2 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->update( *hbqt_par_QRegion( 2 ) );
}

/* void updateGeometry () */
HB_FUNC( QT_QWIDGET_UPDATEGEOMETRY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->updateGeometry();
}

/* bool updatesEnabled () const */
HB_FUNC( QT_QWIDGET_UPDATESENABLED )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->updatesEnabled() );
}

/* QRegion visibleRegion () const */
HB_FUNC( QT_QWIDGET_VISIBLEREGION )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRegion( new QRegion( ( p )->visibleRegion() ), true ) );
}

/* QString whatsThis () const */
HB_FUNC( QT_QWIDGET_WHATSTHIS )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retstr_utf8( ( p )->whatsThis().toUtf8().data() );
}

/* int width () const */
HB_FUNC( QT_QWIDGET_WIDTH )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( p )->width() );
}

/* QWidget * window () const */
HB_FUNC( QT_QWIDGET_WINDOW )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->window(), false ) );
}

/* QString windowFilePath () const */
HB_FUNC( QT_QWIDGET_WINDOWFILEPATH )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retstr_utf8( ( p )->windowFilePath().toUtf8().data() );
}

/* Qt::WindowFlags windowFlags () const */
HB_FUNC( QT_QWIDGET_WINDOWFLAGS )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( Qt::WindowFlags ) ( p )->windowFlags() );
}

/* QIcon windowIcon () const */
HB_FUNC( QT_QWIDGET_WINDOWICON )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QIcon( new QIcon( ( p )->windowIcon() ), true ) );
}

/* QString windowIconText () const */
HB_FUNC( QT_QWIDGET_WINDOWICONTEXT )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retstr_utf8( ( p )->windowIconText().toUtf8().data() );
}

/* Qt::WindowModality windowModality () const */
HB_FUNC( QT_QWIDGET_WINDOWMODALITY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( Qt::WindowModality ) ( p )->windowModality() );
}

/* qreal windowOpacity () const */
HB_FUNC( QT_QWIDGET_WINDOWOPACITY )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retnd( ( p )->windowOpacity() );
}

/* QString windowRole () const */
HB_FUNC( QT_QWIDGET_WINDOWROLE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retstr_utf8( ( p )->windowRole().toUtf8().data() );
}

/* Qt::WindowStates windowState () const */
HB_FUNC( QT_QWIDGET_WINDOWSTATE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( Qt::WindowStates ) ( p )->windowState() );
}

/* QString windowTitle () const */
HB_FUNC( QT_QWIDGET_WINDOWTITLE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retstr_utf8( ( p )->windowTitle().toUtf8().data() );
}

/* Qt::WindowType windowType () const */
HB_FUNC( QT_QWIDGET_WINDOWTYPE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( Qt::WindowType ) ( p )->windowType() );
}

/* int x () const */
HB_FUNC( QT_QWIDGET_X )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( p )->x() );
}

/* int y () const */
HB_FUNC( QT_QWIDGET_Y )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retni( ( p )->y() );
}

/* QWidget * keyboardGrabber () */
HB_FUNC( QT_QWIDGET_KEYBOARDGRABBER )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->keyboardGrabber(), false ) );
}

/* QWidget * mouseGrabber () */
HB_FUNC( QT_QWIDGET_MOUSEGRABBER )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->mouseGrabber(), false ) );
}

/* void setTabOrder ( QWidget * first, QWidget * second ) */
HB_FUNC( QT_QWIDGET_SETTABORDER )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setTabOrder( hbqt_par_QWidget( 2 ), hbqt_par_QWidget( 3 ) );
}

/* bool close () */
HB_FUNC( QT_QWIDGET_CLOSE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      hb_retl( ( p )->close() );
}

/* void hide () */
HB_FUNC( QT_QWIDGET_HIDE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->hide();
}

/* void lower () */
HB_FUNC( QT_QWIDGET_LOWER )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->lower();
}

/* void raise () */
HB_FUNC( QT_QWIDGET_RAISE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->raise();
}

/* void repaint () */
HB_FUNC( QT_QWIDGET_REPAINT_3 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->repaint();
}

/* void setDisabled ( bool disable ) */
HB_FUNC( QT_QWIDGET_SETDISABLED )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setDisabled( hb_parl( 2 ) );
}

/* void setEnabled ( bool enable ) */
HB_FUNC( QT_QWIDGET_SETENABLED )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setEnabled( hb_parl( 2 ) );
}

/* void setFocus () */
HB_FUNC( QT_QWIDGET_SETFOCUS_1 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setFocus();
}

/* void setHidden ( bool hidden ) */
HB_FUNC( QT_QWIDGET_SETHIDDEN )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setHidden( hb_parl( 2 ) );
}

/* void setStyleSheet ( const QString & styleSheet ) */
HB_FUNC( QT_QWIDGET_SETSTYLESHEET )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
   {
      void * pText;
      ( p )->setStyleSheet( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* virtual void setVisible ( bool visible ) */
HB_FUNC( QT_QWIDGET_SETVISIBLE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setVisible( hb_parl( 2 ) );
}

/* void setWindowModified ( bool modified ) */
HB_FUNC( QT_QWIDGET_SETWINDOWMODIFIED )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->setWindowModified( hb_parl( 2 ) );
}

/* void setWindowTitle ( const QString & title ) */
HB_FUNC( QT_QWIDGET_SETWINDOWTITLE )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
   {
      void * pText;
      ( p )->setWindowTitle( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* void show () */
HB_FUNC( QT_QWIDGET_SHOW )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->show();
}

/* void showFullScreen () */
HB_FUNC( QT_QWIDGET_SHOWFULLSCREEN )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->showFullScreen();
}

/* void showMaximized () */
HB_FUNC( QT_QWIDGET_SHOWMAXIMIZED )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->showMaximized();
}

/* void showMinimized () */
HB_FUNC( QT_QWIDGET_SHOWMINIMIZED )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->showMinimized();
}

/* void showNormal () */
HB_FUNC( QT_QWIDGET_SHOWNORMAL )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->showNormal();
}

/* void update () */
HB_FUNC( QT_QWIDGET_UPDATE_3 )
{
   QWidget * p = hbqt_par_QWidget( 1 );
   if( p )
      ( p )->update();
}


#endif /* #if QT_VERSION >= 0x040500 */
