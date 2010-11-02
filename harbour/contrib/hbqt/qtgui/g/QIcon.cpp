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
 *  enum Mode { Normal, Disabled, Active, Selected }
 *  enum State { Off, On }
 */

/*
 *  Constructed[ 10/10 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  //QPixmap pixmap ( int extent, Mode mode = Normal, State state = Off ) const        // Not Implemented
 */

#include <QtCore/QPointer>

#include <QtGui/QIcon>


/*
 * QIcon ()
 * QIcon ( const QPixmap & pixmap )
 * QIcon ( const QIcon & other )
 * QIcon ( const QString & fileName )
 * QIcon ( QIconEngine * engine )
 * QIcon ( QIconEngineV2 * engine )
 * ~QIcon ()
 */

typedef struct
{
   QIcon * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QIcon;

HBQT_GC_FUNC( hbqt_gcRelease_QIcon )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QIcon * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QIcon( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QIcon * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QIcon;
   p->type = HBQT_TYPE_QIcon;

   return p;
}

HB_FUNC( QT_QICON )
{
   QIcon * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISCHAR( 1 ) )
   {
      pObj = new QIcon( hbqt_par_QString( 1 ) ) ;
   }
   else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      HBQT_GC_T * q = ( HBQT_GC_T * ) hb_parptrGC( hbqt_gcFuncs(), 1 );
      if( q )
      {
         if( q->type == HBQT_TYPE_QIcon )
         {
            pObj = new QIcon( *hbqt_par_QIcon( 1 ) ) ;
         }
         else if( q->type == HBQT_TYPE_QPixmap )
         {
            pObj = new QIcon( *hbqt_par_QPixmap( 1 ) ) ;
         }
      }
      else
      {
         pObj = new QIcon( *hbqt_par_QIcon( 1 ) ) ;
      }
   }
   else
   {
      pObj = new QIcon() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QIcon( ( void * ) pObj, true ) );
}

/* QSize actualSize ( const QSize & size, Mode mode = Normal, State state = Off ) const */
HB_FUNC( QT_QICON_ACTUALSIZE )
{
   QIcon * p = hbqt_par_QIcon( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->actualSize( *hbqt_par_QSize( 2 ), ( HB_ISNUM( 3 ) ? ( QIcon::Mode ) hb_parni( 3 ) : ( QIcon::Mode ) QIcon::Normal ), ( HB_ISNUM( 4 ) ? ( QIcon::State ) hb_parni( 4 ) : ( QIcon::State ) QIcon::Off ) ) ), true ) );
}

/* void addFile ( const QString & fileName, const QSize & size = QSize(), Mode mode = Normal, State state = Off ) */
HB_FUNC( QT_QICON_ADDFILE )
{
   QIcon * p = hbqt_par_QIcon( 1 );
   if( p )
   {
      void * pText;
      ( p )->addFile( hb_parstr_utf8( 2, &pText, NULL ), ( HB_ISOBJECT( 3 ) ? *hbqt_par_QSize( 3 ) : QSize() ), ( HB_ISNUM( 4 ) ? ( QIcon::Mode ) hb_parni( 4 ) : ( QIcon::Mode ) QIcon::Normal ), ( HB_ISNUM( 5 ) ? ( QIcon::State ) hb_parni( 5 ) : ( QIcon::State ) QIcon::Off ) );
      hb_strfree( pText );
   }
}

/* void addPixmap ( const QPixmap & pixmap, Mode mode = Normal, State state = Off ) */
HB_FUNC( QT_QICON_ADDPIXMAP )
{
   QIcon * p = hbqt_par_QIcon( 1 );
   if( p )
      ( p )->addPixmap( *hbqt_par_QPixmap( 2 ), ( HB_ISNUM( 3 ) ? ( QIcon::Mode ) hb_parni( 3 ) : ( QIcon::Mode ) QIcon::Normal ), ( HB_ISNUM( 4 ) ? ( QIcon::State ) hb_parni( 4 ) : ( QIcon::State ) QIcon::Off ) );
}

/* QList<QSize> availableSizes ( Mode mode = Normal, State state = Off ) const */
HB_FUNC( QT_QICON_AVAILABLESIZES )
{
   QIcon * p = hbqt_par_QIcon( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QSize>( ( p )->availableSizes( ( HB_ISNUM( 2 ) ? ( QIcon::Mode ) hb_parni( 2 ) : ( QIcon::Mode ) QIcon::Normal ), ( HB_ISNUM( 3 ) ? ( QIcon::State ) hb_parni( 3 ) : ( QIcon::State ) QIcon::Off ) ) ), true ) );
}

/* qint64 cacheKey () const */
HB_FUNC( QT_QICON_CACHEKEY )
{
   QIcon * p = hbqt_par_QIcon( 1 );
   if( p )
      hb_retnint( ( p )->cacheKey() );
}

/* bool isNull () const */
HB_FUNC( QT_QICON_ISNULL )
{
   QIcon * p = hbqt_par_QIcon( 1 );
   if( p )
      hb_retl( ( p )->isNull() );
}

/* void paint ( QPainter * painter, const QRect & rect, Qt::Alignment alignment = Qt::AlignCenter, Mode mode = Normal, State state = Off ) const */
HB_FUNC( QT_QICON_PAINT )
{
   QIcon * p = hbqt_par_QIcon( 1 );
   if( p )
      ( p )->paint( hbqt_par_QPainter( 2 ), *hbqt_par_QRect( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::Alignment ) hb_parni( 4 ) : ( Qt::Alignment ) Qt::AlignCenter ), ( HB_ISNUM( 5 ) ? ( QIcon::Mode ) hb_parni( 5 ) : ( QIcon::Mode ) QIcon::Normal ), ( HB_ISNUM( 6 ) ? ( QIcon::State ) hb_parni( 6 ) : ( QIcon::State ) QIcon::Off ) );
}

/* void paint ( QPainter * painter, int x, int y, int w, int h, Qt::Alignment alignment = Qt::AlignCenter, Mode mode = Normal, State state = Off ) const */
HB_FUNC( QT_QICON_PAINT_1 )
{
   QIcon * p = hbqt_par_QIcon( 1 );
   if( p )
      ( p )->paint( hbqt_par_QPainter( 2 ), hb_parni( 3 ), hb_parni( 4 ), hb_parni( 5 ), hb_parni( 6 ), ( HB_ISNUM( 7 ) ? ( Qt::Alignment ) hb_parni( 7 ) : ( Qt::Alignment ) Qt::AlignCenter ), ( HB_ISNUM( 8 ) ? ( QIcon::Mode ) hb_parni( 8 ) : ( QIcon::Mode ) QIcon::Normal ), ( HB_ISNUM( 9 ) ? ( QIcon::State ) hb_parni( 9 ) : ( QIcon::State ) QIcon::Off ) );
}

/* QPixmap pixmap ( const QSize & size, Mode mode = Normal, State state = Off ) const */
HB_FUNC( QT_QICON_PIXMAP )
{
   QIcon * p = hbqt_par_QIcon( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->pixmap( *hbqt_par_QSize( 2 ), ( HB_ISNUM( 3 ) ? ( QIcon::Mode ) hb_parni( 3 ) : ( QIcon::Mode ) QIcon::Normal ), ( HB_ISNUM( 4 ) ? ( QIcon::State ) hb_parni( 4 ) : ( QIcon::State ) QIcon::Off ) ) ), true ) );
}

/* QPixmap pixmap ( int w, int h, Mode mode = Normal, State state = Off ) const */
HB_FUNC( QT_QICON_PIXMAP_1 )
{
   QIcon * p = hbqt_par_QIcon( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->pixmap( hb_parni( 2 ), hb_parni( 3 ), ( HB_ISNUM( 4 ) ? ( QIcon::Mode ) hb_parni( 4 ) : ( QIcon::Mode ) QIcon::Normal ), ( HB_ISNUM( 5 ) ? ( QIcon::State ) hb_parni( 5 ) : ( QIcon::State ) QIcon::Off ) ) ), true ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
