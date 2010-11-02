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
 *  Constructed[ 12/12 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QDesktopWidget>


/*
 * QDesktopWidget ()
 * ~QDesktopWidget ()
 */

typedef struct
{
   QPointer< QDesktopWidget > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QDesktopWidget;

HBQT_GC_FUNC( hbqt_gcRelease_QDesktopWidget )
{
   HBQT_GC_T_QDesktopWidget * p = ( HBQT_GC_T_QDesktopWidget * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QDesktopWidget * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QDesktopWidget( void * pObj, bool bNew )
{
   HBQT_GC_T_QDesktopWidget * p = ( HBQT_GC_T_QDesktopWidget * ) hb_gcAllocate( sizeof( HBQT_GC_T_QDesktopWidget ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QDesktopWidget >( ( QDesktopWidget * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QDesktopWidget;
   p->type = HBQT_TYPE_QDesktopWidget;

   return p;
}

HB_FUNC( QT_QDESKTOPWIDGET )
{
   QDesktopWidget * pObj = NULL;

   pObj = new QDesktopWidget() ;

   hb_retptrGC( hbqt_gcAllocate_QDesktopWidget( ( void * ) pObj, true ) );
}

/* const QRect availableGeometry ( int screen = -1 ) const */
HB_FUNC( QT_QDESKTOPWIDGET_AVAILABLEGEOMETRY )
{
   QDesktopWidget * p = hbqt_par_QDesktopWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->availableGeometry( hb_parnidef( 2, -1 ) ) ), true ) );
}

/* const QRect availableGeometry ( const QWidget * widget ) const */
HB_FUNC( QT_QDESKTOPWIDGET_AVAILABLEGEOMETRY_1 )
{
   QDesktopWidget * p = hbqt_par_QDesktopWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->availableGeometry( hbqt_par_QWidget( 2 ) ) ), true ) );
}

/* const QRect availableGeometry ( const QPoint & p ) const */
HB_FUNC( QT_QDESKTOPWIDGET_AVAILABLEGEOMETRY_2 )
{
   QDesktopWidget * p = hbqt_par_QDesktopWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->availableGeometry( *hbqt_par_QPoint( 2 ) ) ), true ) );
}

/* bool isVirtualDesktop () const */
HB_FUNC( QT_QDESKTOPWIDGET_ISVIRTUALDESKTOP )
{
   QDesktopWidget * p = hbqt_par_QDesktopWidget( 1 );
   if( p )
      hb_retl( ( p )->isVirtualDesktop() );
}

/* int numScreens () const */
HB_FUNC( QT_QDESKTOPWIDGET_NUMSCREENS )
{
   QDesktopWidget * p = hbqt_par_QDesktopWidget( 1 );
   if( p )
      hb_retni( ( p )->numScreens() );
}

/* int primaryScreen () const */
HB_FUNC( QT_QDESKTOPWIDGET_PRIMARYSCREEN )
{
   QDesktopWidget * p = hbqt_par_QDesktopWidget( 1 );
   if( p )
      hb_retni( ( p )->primaryScreen() );
}

/* QWidget * screen ( int screen = -1 ) */
HB_FUNC( QT_QDESKTOPWIDGET_SCREEN )
{
   QDesktopWidget * p = hbqt_par_QDesktopWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QWidget( ( p )->screen( hb_parnidef( 2, -1 ) ), false ) );
}

/* const QRect screenGeometry ( int screen = -1 ) const */
HB_FUNC( QT_QDESKTOPWIDGET_SCREENGEOMETRY )
{
   QDesktopWidget * p = hbqt_par_QDesktopWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->screenGeometry( hb_parnidef( 2, -1 ) ) ), true ) );
}

/* const QRect screenGeometry ( const QWidget * widget ) const */
HB_FUNC( QT_QDESKTOPWIDGET_SCREENGEOMETRY_1 )
{
   QDesktopWidget * p = hbqt_par_QDesktopWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->screenGeometry( hbqt_par_QWidget( 2 ) ) ), true ) );
}

/* const QRect screenGeometry ( const QPoint & p ) const */
HB_FUNC( QT_QDESKTOPWIDGET_SCREENGEOMETRY_2 )
{
   QDesktopWidget * p = hbqt_par_QDesktopWidget( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->screenGeometry( *hbqt_par_QPoint( 2 ) ) ), true ) );
}

/* int screenNumber ( const QWidget * widget = 0 ) const */
HB_FUNC( QT_QDESKTOPWIDGET_SCREENNUMBER )
{
   QDesktopWidget * p = hbqt_par_QDesktopWidget( 1 );
   if( p )
      hb_retni( ( p )->screenNumber( hbqt_par_QWidget( 2 ) ) );
}

/* int screenNumber ( const QPoint & point ) const */
HB_FUNC( QT_QDESKTOPWIDGET_SCREENNUMBER_1 )
{
   QDesktopWidget * p = hbqt_par_QDesktopWidget( 1 );
   if( p )
      hb_retni( ( p )->screenNumber( *hbqt_par_QPoint( 2 ) ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
