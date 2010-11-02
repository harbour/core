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
 *  Constructed[ 6/6 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QSplashScreen>


/*
 * QSplashScreen ( const QPixmap & pixmap = QPixmap(), Qt::WindowFlags f = 0 )
 * QSplashScreen ( QWidget * parent, const QPixmap & pixmap = QPixmap(), Qt::WindowFlags f = 0 )
 * virtual ~QSplashScreen ()
 */

typedef struct
{
   QPointer< QSplashScreen > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QSplashScreen;

HBQT_GC_FUNC( hbqt_gcRelease_QSplashScreen )
{
   HBQT_GC_T_QSplashScreen * p = ( HBQT_GC_T_QSplashScreen * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      QSplashScreen * ph = p->ph;
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

void * hbqt_gcAllocate_QSplashScreen( void * pObj, bool bNew )
{
   HBQT_GC_T_QSplashScreen * p = ( HBQT_GC_T_QSplashScreen * ) hb_gcAllocate( sizeof( HBQT_GC_T_QSplashScreen ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QSplashScreen >( ( QSplashScreen * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QSplashScreen;
   p->type = HBQT_TYPE_QSplashScreen;

   return p;
}

HB_FUNC( QT_QSPLASHSCREEN )
{
   QSplashScreen * pObj = NULL;

   pObj = new QSplashScreen( hbqt_par_QWidget( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QSplashScreen( ( void * ) pObj, true ) );
}

/* void finish ( QWidget * mainWin ) */
HB_FUNC( QT_QSPLASHSCREEN_FINISH )
{
   QSplashScreen * p = hbqt_par_QSplashScreen( 1 );
   if( p )
      ( p )->finish( hbqt_par_QWidget( 2 ) );
}

/* const QPixmap pixmap () const */
HB_FUNC( QT_QSPLASHSCREEN_PIXMAP )
{
   QSplashScreen * p = hbqt_par_QSplashScreen( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->pixmap() ), true ) );
}

/* void repaint () */
HB_FUNC( QT_QSPLASHSCREEN_REPAINT )
{
   QSplashScreen * p = hbqt_par_QSplashScreen( 1 );
   if( p )
      ( p )->repaint();
}

/* void setPixmap ( const QPixmap & pixmap ) */
HB_FUNC( QT_QSPLASHSCREEN_SETPIXMAP )
{
   QSplashScreen * p = hbqt_par_QSplashScreen( 1 );
   if( p )
      ( p )->setPixmap( *hbqt_par_QPixmap( 2 ) );
}

/* void clearMessage () */
HB_FUNC( QT_QSPLASHSCREEN_CLEARMESSAGE )
{
   QSplashScreen * p = hbqt_par_QSplashScreen( 1 );
   if( p )
      ( p )->clearMessage();
}

/* void showMessage ( const QString & message, int alignment = Qt::AlignLeft, const QColor & color = Qt::black ) */
HB_FUNC( QT_QSPLASHSCREEN_SHOWMESSAGE )
{
   QSplashScreen * p = hbqt_par_QSplashScreen( 1 );
   if( p )
   {
      void * pText;
      ( p )->showMessage( hb_parstr_utf8( 2, &pText, NULL ), hb_parnidef( 3, Qt::AlignLeft ), *hbqt_par_QColor( 4 ) );
      hb_strfree( pText );
   }
}


#endif /* #if QT_VERSION >= 0x040500 */
