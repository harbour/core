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
 *  enum AreaOption { DontMaximizeSubWindowOnActivation }
 *  flags AreaOptions
 *  enum ViewMode { SubWindowView, TabbedView }
 *  enum WindowOrder { CreationOrder, StackingOrder, ActivationHistoryOrder }
 */

/*
 *  Constructed[ 26/26 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QMdiArea>


/*
 * QMdiArea ( QWidget * parent = 0 )
 * ~QMdiArea ()
 *
 */

typedef struct
{
   QPointer< QMdiArea > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QMdiArea;

HBQT_GC_FUNC( hbqt_gcRelease_QMdiArea )
{
   QMdiArea  * ph = NULL;
   HBQT_GC_T_QMdiArea * p = ( HBQT_GC_T_QMdiArea * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            delete ( p->ph );
            p->ph = NULL;
         }
         else
            p->ph = NULL;
      }
      else
         p->ph = NULL;
   }
   else
      p->ph = NULL;
}

void * hbqt_gcAllocate_QMdiArea( void * pObj, bool bNew )
{
   HBQT_GC_T_QMdiArea * p = ( HBQT_GC_T_QMdiArea * ) hb_gcAllocate( sizeof( HBQT_GC_T_QMdiArea ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QMdiArea >( ( QMdiArea * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QMdiArea;
   p->type = HBQT_TYPE_QMdiArea;

   return p;
}

HB_FUNC( QT_QMDIAREA )
{
   QMdiArea * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QMdiArea( hbqt_par_QWidget( 1 ) ) ;
   }
   else
   {
      pObj = new QMdiArea() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QMdiArea( ( void * ) pObj, true ) );
}

/* WindowOrder activationOrder () const */
HB_FUNC( QT_QMDIAREA_ACTIVATIONORDER )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      hb_retni( ( QMdiArea::WindowOrder ) ( p )->activationOrder() );
}

/* QMdiSubWindow * activeSubWindow () const */
HB_FUNC( QT_QMDIAREA_ACTIVESUBWINDOW )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMdiSubWindow( ( p )->activeSubWindow(), false ) );
}

/* QMdiSubWindow * addSubWindow ( QWidget * widget, Qt::WindowFlags windowFlags = 0 ) */
HB_FUNC( QT_QMDIAREA_ADDSUBWINDOW )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMdiSubWindow( ( p )->addSubWindow( hbqt_par_QWidget( 2 ), ( Qt::WindowFlags ) hb_parni( 3 ) ), false ) );
}

/* QBrush background () const */
HB_FUNC( QT_QMDIAREA_BACKGROUND )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->background() ), true ) );
}

/* QMdiSubWindow * currentSubWindow () const */
HB_FUNC( QT_QMDIAREA_CURRENTSUBWINDOW )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMdiSubWindow( ( p )->currentSubWindow(), false ) );
}

/* bool documentMode () const */
HB_FUNC( QT_QMDIAREA_DOCUMENTMODE )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      hb_retl( ( p )->documentMode() );
}

/* void removeSubWindow ( QWidget * widget ) */
HB_FUNC( QT_QMDIAREA_REMOVESUBWINDOW )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      ( p )->removeSubWindow( hbqt_par_QWidget( 2 ) );
}

/* void setActivationOrder ( WindowOrder order ) */
HB_FUNC( QT_QMDIAREA_SETACTIVATIONORDER )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      ( p )->setActivationOrder( ( QMdiArea::WindowOrder ) hb_parni( 2 ) );
}

/* void setBackground ( const QBrush & background ) */
HB_FUNC( QT_QMDIAREA_SETBACKGROUND )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      ( p )->setBackground( *hbqt_par_QBrush( 2 ) );
}

/* void setDocumentMode ( bool enabled ) */
HB_FUNC( QT_QMDIAREA_SETDOCUMENTMODE )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      ( p )->setDocumentMode( hb_parl( 2 ) );
}

/* void setOption ( AreaOption option, bool on = true ) */
HB_FUNC( QT_QMDIAREA_SETOPTION )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      ( p )->setOption( ( QMdiArea::AreaOption ) hb_parni( 2 ), hb_parl( 3 ) );
}

/* void setTabPosition ( QTabWidget::TabPosition position ) */
HB_FUNC( QT_QMDIAREA_SETTABPOSITION )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      ( p )->setTabPosition( ( QTabWidget::TabPosition ) hb_parni( 2 ) );
}

/* void setTabShape ( QTabWidget::TabShape shape ) */
HB_FUNC( QT_QMDIAREA_SETTABSHAPE )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      ( p )->setTabShape( ( QTabWidget::TabShape ) hb_parni( 2 ) );
}

/* void setViewMode ( ViewMode mode ) */
HB_FUNC( QT_QMDIAREA_SETVIEWMODE )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      ( p )->setViewMode( ( QMdiArea::ViewMode ) hb_parni( 2 ) );
}

/* QList<QMdiSubWindow *> subWindowList ( WindowOrder order = CreationOrder ) const */
HB_FUNC( QT_QMDIAREA_SUBWINDOWLIST )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QMdiSubWindow *>( ( p )->subWindowList( ( HB_ISNUM( 2 ) ? ( QMdiArea::WindowOrder ) hb_parni( 2 ) : ( QMdiArea::WindowOrder ) QMdiArea::CreationOrder ) ) ), true ) );
}

/* QTabWidget::TabPosition tabPosition () const */
HB_FUNC( QT_QMDIAREA_TABPOSITION )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      hb_retni( ( QTabWidget::TabPosition ) ( p )->tabPosition() );
}

/* QTabWidget::TabShape tabShape () const */
HB_FUNC( QT_QMDIAREA_TABSHAPE )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      hb_retni( ( QTabWidget::TabShape ) ( p )->tabShape() );
}

/* bool testOption ( AreaOption option ) const */
HB_FUNC( QT_QMDIAREA_TESTOPTION )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      hb_retl( ( p )->testOption( ( QMdiArea::AreaOption ) hb_parni( 2 ) ) );
}

/* ViewMode viewMode () const */
HB_FUNC( QT_QMDIAREA_VIEWMODE )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      hb_retni( ( QMdiArea::ViewMode ) ( p )->viewMode() );
}

/* void activateNextSubWindow () */
HB_FUNC( QT_QMDIAREA_ACTIVATENEXTSUBWINDOW )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      ( p )->activateNextSubWindow();
}

/* void activatePreviousSubWindow () */
HB_FUNC( QT_QMDIAREA_ACTIVATEPREVIOUSSUBWINDOW )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      ( p )->activatePreviousSubWindow();
}

/* void cascadeSubWindows () */
HB_FUNC( QT_QMDIAREA_CASCADESUBWINDOWS )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      ( p )->cascadeSubWindows();
}

/* void closeActiveSubWindow () */
HB_FUNC( QT_QMDIAREA_CLOSEACTIVESUBWINDOW )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      ( p )->closeActiveSubWindow();
}

/* void closeAllSubWindows () */
HB_FUNC( QT_QMDIAREA_CLOSEALLSUBWINDOWS )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      ( p )->closeAllSubWindows();
}

/* void setActiveSubWindow ( QMdiSubWindow * window ) */
HB_FUNC( QT_QMDIAREA_SETACTIVESUBWINDOW )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      ( p )->setActiveSubWindow( hbqt_par_QMdiSubWindow( 2 ) );
}

/* void tileSubWindows () */
HB_FUNC( QT_QMDIAREA_TILESUBWINDOWS )
{
   QMdiArea * p = hbqt_par_QMdiArea( 1 );
   if( p )
      ( p )->tileSubWindows();
}


#endif /* #if QT_VERSION >= 0x040500 */
