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
 *  Constructed[ 9/9 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  //QBitmap fromData ( const QSize & size, const uchar * bits, QImage::Format monoFormat = QImage::Format_MonoLSB )
 */

#include <QtCore/QPointer>

#include <QtGui/QBitmap>


/*
 * QBitmap ()
 * QBitmap ( const QPixmap & pixmap )
 * QBitmap ( int width, int height )
 * QBitmap ( const QSize & size )
 * QBitmap ( const QString & fileName, const char * format = 0 )
 * ~QBitmap ()
 */

typedef struct
{
   QBitmap * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QBitmap;

HBQT_GC_FUNC( hbqt_gcRelease_QBitmap )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QBitmap * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QBitmap( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QBitmap * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QBitmap;
   p->type = HBQT_TYPE_QBitmap;

   return p;
}

HB_FUNC( QT_QBITMAP )
{
   QBitmap * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QBitmap( *hbqt_par_QBitmap( 1 ) ) ;
   }
   else if( hb_pcount() == 1 && HB_ISCHAR( 1 ) )
   {
      pObj = new QBitmap( hbqt_par_QString( 1 ), ( const char * ) 0 ) ;
   }
   else if( hb_pcount() == 2 && HB_ISCHAR( 1 ) && HB_ISCHAR( 2 ) )
   {
      pObj = new QBitmap( hbqt_par_QString( 1 ), hb_parc( 2 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISNUM( 1 ) && HB_ISNUM( 2 ) )
   {
      pObj = new QBitmap( hb_parni( 1 ), hb_parni( 2 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISCHAR( 1 ) && HB_ISPOINTER( 2 ) )
   {
      if(      ( QString ) "QPixmap" == hbqt_par_QString( 1 ) )
      {
         pObj = new QBitmap( *hbqt_par_QPixmap( 2 ) ) ;
      }
      else if( ( QString ) "QSize"   == hbqt_par_QString( 1 ) )
      {
         pObj = new QBitmap( *hbqt_par_QSize( 2 ) ) ;
      }
      else
      {
         pObj = new QBitmap() ;
      }
   }
   else
   {
      pObj = new QBitmap() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QBitmap( ( void * ) pObj, true ) );
}

/* QBitmap () */
HB_FUNC( QT_QBITMAP_QBITMAP )
{
   hb_retptrGC( hbqt_gcAllocate_QBitmap( new QBitmap(), true ) );
}

/* QBitmap ( const QPixmap & pixmap ) */
HB_FUNC( QT_QBITMAP_QBITMAP_1 )
{
   hb_retptrGC( hbqt_gcAllocate_QBitmap( new QBitmap( *hbqt_par_QPixmap( 2 ) ), true ) );
}

/* QBitmap ( int width, int height ) */
HB_FUNC( QT_QBITMAP_QBITMAP_2 )
{
   hb_retptrGC( hbqt_gcAllocate_QBitmap( new QBitmap( hb_parni( 2 ), hb_parni( 3 ) ), true ) );
}

/* QBitmap ( const QSize & size ) */
HB_FUNC( QT_QBITMAP_QBITMAP_3 )
{
   hb_retptrGC( hbqt_gcAllocate_QBitmap( new QBitmap( *hbqt_par_QSize( 2 ) ), true ) );
}

/* QBitmap ( const QString & fileName, const char * format = 0 ) */
HB_FUNC( QT_QBITMAP_QBITMAP_4 )
{
      void * pText;
   hb_retptrGC( hbqt_gcAllocate_QBitmap( new QBitmap( hb_parstr_utf8( 2, &pText, NULL ), ( const char * ) hb_parc( 3 ) ), true ) );
      hb_strfree( pText );
}

/* void clear () */
HB_FUNC( QT_QBITMAP_CLEAR )
{
   QBitmap * p = hbqt_par_QBitmap( 1 );
   if( p )
      ( p )->clear();
}

/* QBitmap transformed ( const QTransform & matrix ) const */
HB_FUNC( QT_QBITMAP_TRANSFORMED )
{
   QBitmap * p = hbqt_par_QBitmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBitmap( new QBitmap( ( p )->transformed( *hbqt_par_QTransform( 2 ) ) ), true ) );
}

/* QBitmap transformed ( const QMatrix & matrix ) const */
HB_FUNC( QT_QBITMAP_TRANSFORMED_1 )
{
   QBitmap * p = hbqt_par_QBitmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBitmap( new QBitmap( ( p )->transformed( *hbqt_par_QMatrix( 2 ) ) ), true ) );
}

/* QBitmap fromImage ( const QImage & image, Qt::ImageConversionFlags flags = Qt::AutoColor ) */
HB_FUNC( QT_QBITMAP_FROMIMAGE )
{
   QBitmap * p = hbqt_par_QBitmap( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBitmap( new QBitmap( ( p )->fromImage( *hbqt_par_QImage( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::ImageConversionFlags ) hb_parni( 3 ) : ( Qt::ImageConversionFlags ) Qt::AutoColor ) ) ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
