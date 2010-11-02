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

#if QT_VERSION >= 0x040500

/*
 *  Constructed[ 5/5 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  //const QByteArray & buffer () const
 */

#include <QtCore/QPointer>

#include <QtCore/QBuffer>


/*
 * QBuffer ( QObject * parent = 0 )
 * QBuffer ( QByteArray * byteArray, QObject * parent = 0 )
 * ~QBuffer ()
 */

typedef struct
{
   QPointer< QBuffer > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QBuffer;

HBQT_GC_FUNC( hbqt_gcRelease_QBuffer )
{
   HBQT_GC_T_QBuffer * p = ( HBQT_GC_T_QBuffer * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
      {
         QBuffer * ph = p->ph;
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
            delete ( p->ph );
      }
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QBuffer( void * pObj, bool bNew )
{
   HBQT_GC_T_QBuffer * p = ( HBQT_GC_T_QBuffer * ) hb_gcAllocate( sizeof( HBQT_GC_T_QBuffer ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QBuffer >( ( QBuffer * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QBuffer;
   p->type = HBQT_TYPE_QBuffer;

   return p;
}

HB_FUNC( QT_QBUFFER )
{
   QBuffer * pObj = NULL;

   pObj = new QBuffer() ;

   hb_retptrGC( hbqt_gcAllocate_QBuffer( ( void * ) pObj, true ) );
}

/* QByteArray & buffer () */
HB_FUNC( QT_QBUFFER_BUFFER )
{
   QBuffer * p = hbqt_par_QBuffer( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->buffer() ), true ) );
}

/* const QByteArray & data () const */
HB_FUNC( QT_QBUFFER_DATA )
{
   QBuffer * p = hbqt_par_QBuffer( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->data() ), true ) );
}

/* void setBuffer ( QByteArray * byteArray ) */
HB_FUNC( QT_QBUFFER_SETBUFFER )
{
   QBuffer * p = hbqt_par_QBuffer( 1 );
   if( p )
      ( p )->setBuffer( hbqt_par_QByteArray( 2 ) );
}

/* void setData ( const char * data, int size ) */
HB_FUNC( QT_QBUFFER_SETDATA )
{
   QBuffer * p = hbqt_par_QBuffer( 1 );
   if( p )
      ( p )->setData( ( const char * ) hb_parc( 2 ), hb_parni( 3 ) );
}

/* void setData ( const QByteArray & data ) */
HB_FUNC( QT_QBUFFER_SETDATA_1 )
{
   QBuffer * p = hbqt_par_QBuffer( 1 );
   if( p )
      ( p )->setData( *hbqt_par_QByteArray( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
