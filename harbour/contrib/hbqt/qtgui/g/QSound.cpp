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
 *  Constructed[ 7/7 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QSound>


/*
 * QSound ( const QString & filename, QObject * parent = 0 )
 * ~QSound ()
 */

typedef struct
{
   QPointer< QSound > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QSound;

HBQT_GC_FUNC( hbqt_gcRelease_QSound )
{
   HBQT_GC_T_QSound * p = ( HBQT_GC_T_QSound * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QSound * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QSound( void * pObj, bool bNew )
{
   HBQT_GC_T_QSound * p = ( HBQT_GC_T_QSound * ) hb_gcAllocate( sizeof( HBQT_GC_T_QSound ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QSound >( ( QSound * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QSound;
   p->type = HBQT_TYPE_QSound;

   return p;
}

HB_FUNC( QT_QSOUND )
{
   QSound * pObj = NULL;

   pObj = new QSound( hbqt_par_QString( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QSound( ( void * ) pObj, true ) );
}

/* QString fileName () const */
HB_FUNC( QT_QSOUND_FILENAME )
{
   QSound * p = hbqt_par_QSound( 1 );
   if( p )
      hb_retstr_utf8( ( p )->fileName().toUtf8().data() );
}

/* bool isFinished () const */
HB_FUNC( QT_QSOUND_ISFINISHED )
{
   QSound * p = hbqt_par_QSound( 1 );
   if( p )
      hb_retl( ( p )->isFinished() );
}

/* int loops () const */
HB_FUNC( QT_QSOUND_LOOPS )
{
   QSound * p = hbqt_par_QSound( 1 );
   if( p )
      hb_retni( ( p )->loops() );
}

/* int loopsRemaining () const */
HB_FUNC( QT_QSOUND_LOOPSREMAINING )
{
   QSound * p = hbqt_par_QSound( 1 );
   if( p )
      hb_retni( ( p )->loopsRemaining() );
}

/* void setLoops ( int number ) */
HB_FUNC( QT_QSOUND_SETLOOPS )
{
   QSound * p = hbqt_par_QSound( 1 );
   if( p )
      ( p )->setLoops( hb_parni( 2 ) );
}

/* bool isAvailable () */
HB_FUNC( QT_QSOUND_ISAVAILABLE )
{
   QSound * p = hbqt_par_QSound( 1 );
   if( p )
      hb_retl( ( p )->isAvailable() );
}

/* void play ( const QString & filename ) */
HB_FUNC( QT_QSOUND_PLAY )
{
   QSound * p = hbqt_par_QSound( 1 );
   if( p )
   {
      void * pText;
      ( p )->play( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}


#endif /* #if QT_VERSION >= 0x040500 */
