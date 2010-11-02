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
 *  enum SubWindowOption { RubberBandResize, RubberBandMove }
 *  flags SubWindowOptions
 */

/*
 *  Constructed[ 14/14 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QMdiSubWindow>


/*
 * QMdiSubWindow ( QWidget * parent = 0, Qt::WindowFlags flags = 0 )
 * ~QMdiSubWindow ()
 *
 */

typedef struct
{
   QPointer< QMdiSubWindow > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QMdiSubWindow;

HBQT_GC_FUNC( hbqt_gcRelease_QMdiSubWindow )
{
   HBQT_GC_T_QMdiSubWindow * p = ( HBQT_GC_T_QMdiSubWindow * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QMdiSubWindow * ph = p->ph;
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

void * hbqt_gcAllocate_QMdiSubWindow( void * pObj, bool bNew )
{
   HBQT_GC_T_QMdiSubWindow * p = ( HBQT_GC_T_QMdiSubWindow * ) hb_gcAllocate( sizeof( HBQT_GC_T_QMdiSubWindow ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QMdiSubWindow >( ( QMdiSubWindow * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QMdiSubWindow;
   p->type = HBQT_TYPE_QMdiSubWindow;

   return p;
}

HB_FUNC( QT_QMDISUBWINDOW )
{
   QMdiSubWindow * pObj = NULL;

   if( hb_pcount() >= 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QMdiSubWindow( hbqt_par_QWidget( 1 ), ( Qt::WindowFlags ) hb_parni( 2 ) ) ;
   }
   else
   {
      pObj = new QMdiSubWindow() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QMdiSubWindow( ( void * ) pObj, true ) );
}

/* bool isShaded () const */
HB_FUNC( QT_QMDISUBWINDOW_ISSHADED )
{
   QMdiSubWindow * p = hbqt_par_QMdiSubWindow( 1 );
   if( p )
      hb_retl( ( p )->isShaded() );
}

/* int keyboardPageStep () const */
HB_FUNC( QT_QMDISUBWINDOW_KEYBOARDPAGESTEP )
{
   QMdiSubWindow * p = hbqt_par_QMdiSubWindow( 1 );
   if( p )
      hb_retni( ( p )->keyboardPageStep() );
}

/* int keyboardSingleStep () const */
HB_FUNC( QT_QMDISUBWINDOW_KEYBOARDSINGLESTEP )
{
   QMdiSubWindow * p = hbqt_par_QMdiSubWindow( 1 );
   if( p )
      hb_retni( ( p )->keyboardSingleStep() );
}

/* QMdiArea * mdiArea () const */
HB_FUNC( QT_QMDISUBWINDOW_MDIAREA )
{
   QMdiSubWindow * p = hbqt_par_QMdiSubWindow( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMdiArea( ( p )->mdiArea(), false ) );
}

/* void setKeyboardPageStep ( int step ) */
HB_FUNC( QT_QMDISUBWINDOW_SETKEYBOARDPAGESTEP )
{
   QMdiSubWindow * p = hbqt_par_QMdiSubWindow( 1 );
   if( p )
      ( p )->setKeyboardPageStep( hb_parni( 2 ) );
}

/* void setKeyboardSingleStep ( int step ) */
HB_FUNC( QT_QMDISUBWINDOW_SETKEYBOARDSINGLESTEP )
{
   QMdiSubWindow * p = hbqt_par_QMdiSubWindow( 1 );
   if( p )
      ( p )->setKeyboardSingleStep( hb_parni( 2 ) );
}

/* void setOption ( SubWindowOption option, bool on = true ) */
HB_FUNC( QT_QMDISUBWINDOW_SETOPTION )
{
   QMdiSubWindow * p = hbqt_par_QMdiSubWindow( 1 );
   if( p )
      ( p )->setOption( ( QMdiSubWindow::SubWindowOption ) hb_parni( 2 ), hb_parl( 3 ) );
}

/* void setSystemMenu ( QMenu * systemMenu ) */
HB_FUNC( QT_QMDISUBWINDOW_SETSYSTEMMENU )
{
   QMdiSubWindow * p = hbqt_par_QMdiSubWindow( 1 );
   if( p )
      ( p )->setSystemMenu( hbqt_par_QMenu( 2 ) );
}

/* void setWidget ( QWidget * widget ) */
HB_FUNC( QT_QMDISUBWINDOW_SETWIDGET )
{
   QMdiSubWindow * p = hbqt_par_QMdiSubWindow( 1 );
   if( p )
      ( p )->setWidget( hbqt_par_QWidget( 2 ) );
}

/* QMenu * systemMenu () const */
HB_FUNC( QT_QMDISUBWINDOW_SYSTEMMENU )
{
   QMdiSubWindow * p = hbqt_par_QMdiSubWindow( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QMenu( ( p )->systemMenu(), false ) );
}

/* bool testOption ( SubWindowOption option ) const */
HB_FUNC( QT_QMDISUBWINDOW_TESTOPTION )
{
   QMdiSubWindow * p = hbqt_par_QMdiSubWindow( 1 );
   if( p )
      hb_retl( ( p )->testOption( ( QMdiSubWindow::SubWindowOption ) hb_parni( 2 ) ) );
}

/* QWidget * widget () const */
HB_FUNC( QT_QMDISUBWINDOW_WIDGET )
{
   QMdiSubWindow * p = hbqt_par_QMdiSubWindow( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->widget(), false ) );
}

/* void showShaded () */
HB_FUNC( QT_QMDISUBWINDOW_SHOWSHADED )
{
   QMdiSubWindow * p = hbqt_par_QMdiSubWindow( 1 );
   if( p )
      ( p )->showShaded();
}

/* void showSystemMenu () */
HB_FUNC( QT_QMDISUBWINDOW_SHOWSYSTEMMENU )
{
   QMdiSubWindow * p = hbqt_par_QMdiSubWindow( 1 );
   if( p )
      ( p )->showSystemMenu();
}


#endif /* #if QT_VERSION >= 0x040500 */
