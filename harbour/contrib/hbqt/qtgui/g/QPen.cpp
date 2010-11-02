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
 *  Constructed[ 21/23 [ 91.30% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  QVector<qreal> dashPattern () const
 *  void setDashPattern ( const QVector<qreal> & pattern )
 */

#include <QtCore/QPointer>

#include <QtGui/QPen>


/*
 * QPen ()
 * QPen ( Qt::PenStyle style )
 * QPen ( const QColor & color )
 * QPen ( const QBrush & brush, qreal width, Qt::PenStyle style = Qt::SolidLine, Qt::PenCapStyle cap = Qt::SquareCap, Qt::PenJoinStyle join = Qt::BevelJoin )
 * QPen ( const QPen & pen )
 * ~QPen ()
 */

typedef struct
{
   QPen * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QPen;

HBQT_GC_FUNC( hbqt_gcRelease_QPen )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QPen * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QPen( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QPen * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPen;
   p->type = HBQT_TYPE_QPen;

   return p;
}

HB_FUNC( QT_QPEN )
{
   QPen * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISNUM( 1 ) )
   {
      pObj = new QPen( ( Qt::PenStyle ) hb_parni( 1 ) ) ;
   }
   else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QPen( *hbqt_par_QPen( 1 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISCHAR( 1 ) && HB_ISPOINTER( 2 ) )
   {
      QString objName = ( QString ) hbqt_par_QString( 1 );

      if( objName == ( QString ) "QColor" )
      {
         pObj = new QPen( *hbqt_par_QColor( 2 ) ) ;
      }
      else
      {
         pObj = new QPen() ;
      }
   }
   else if( hb_pcount() >= 2 && HB_ISPOINTER( 1 ) && HB_ISNUM( 2 ) )
   {
      Qt::PenStyle iStyle = HB_ISNUM( 3 ) ? ( Qt::PenStyle ) hb_parni( 3 ) : Qt::SolidLine;
      Qt::PenCapStyle iCap = HB_ISNUM( 4 ) ? ( Qt::PenCapStyle ) hb_parni( 4 ) : Qt::SquareCap;
      Qt::PenJoinStyle iJoin = HB_ISNUM( 5 ) ? ( Qt::PenJoinStyle ) hb_parni( 5 ) : Qt::BevelJoin;

      pObj = new QPen( *hbqt_par_QBrush( 1 ), hb_parnd( 2 ), iStyle, iCap, iJoin ) ;
   }
   else
   {
      pObj = new QPen() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QPen( ( void * ) pObj, true ) );
}

/* QBrush brush () const */
HB_FUNC( QT_QPEN_BRUSH )
{
   QPen * p = hbqt_par_QPen( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->brush() ), true ) );
}

/* Qt::PenCapStyle capStyle () const */
HB_FUNC( QT_QPEN_CAPSTYLE )
{
   QPen * p = hbqt_par_QPen( 1 );
   if( p )
      hb_retni( ( Qt::PenCapStyle ) ( p )->capStyle() );
}

/* QColor color () const */
HB_FUNC( QT_QPEN_COLOR )
{
   QPen * p = hbqt_par_QPen( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->color() ), true ) );
}

/* qreal dashOffset () const */
HB_FUNC( QT_QPEN_DASHOFFSET )
{
   QPen * p = hbqt_par_QPen( 1 );
   if( p )
      hb_retnd( ( p )->dashOffset() );
}

/* bool isCosmetic () const */
HB_FUNC( QT_QPEN_ISCOSMETIC )
{
   QPen * p = hbqt_par_QPen( 1 );
   if( p )
      hb_retl( ( p )->isCosmetic() );
}

/* bool isSolid () const */
HB_FUNC( QT_QPEN_ISSOLID )
{
   QPen * p = hbqt_par_QPen( 1 );
   if( p )
      hb_retl( ( p )->isSolid() );
}

/* Qt::PenJoinStyle joinStyle () const */
HB_FUNC( QT_QPEN_JOINSTYLE )
{
   QPen * p = hbqt_par_QPen( 1 );
   if( p )
      hb_retni( ( Qt::PenJoinStyle ) ( p )->joinStyle() );
}

/* qreal miterLimit () const */
HB_FUNC( QT_QPEN_MITERLIMIT )
{
   QPen * p = hbqt_par_QPen( 1 );
   if( p )
      hb_retnd( ( p )->miterLimit() );
}

/* void setBrush ( const QBrush & brush ) */
HB_FUNC( QT_QPEN_SETBRUSH )
{
   QPen * p = hbqt_par_QPen( 1 );
   if( p )
      ( p )->setBrush( *hbqt_par_QBrush( 2 ) );
}

/* void setCapStyle ( Qt::PenCapStyle style ) */
HB_FUNC( QT_QPEN_SETCAPSTYLE )
{
   QPen * p = hbqt_par_QPen( 1 );
   if( p )
      ( p )->setCapStyle( ( Qt::PenCapStyle ) hb_parni( 2 ) );
}

/* void setColor ( const QColor & color ) */
HB_FUNC( QT_QPEN_SETCOLOR )
{
   QPen * p = hbqt_par_QPen( 1 );
   if( p )
      ( p )->setColor( *hbqt_par_QColor( 2 ) );
}

/* void setCosmetic ( bool cosmetic ) */
HB_FUNC( QT_QPEN_SETCOSMETIC )
{
   QPen * p = hbqt_par_QPen( 1 );
   if( p )
      ( p )->setCosmetic( hb_parl( 2 ) );
}

/* void setDashOffset ( qreal offset ) */
HB_FUNC( QT_QPEN_SETDASHOFFSET )
{
   QPen * p = hbqt_par_QPen( 1 );
   if( p )
      ( p )->setDashOffset( hb_parnd( 2 ) );
}

/* void setJoinStyle ( Qt::PenJoinStyle style ) */
HB_FUNC( QT_QPEN_SETJOINSTYLE )
{
   QPen * p = hbqt_par_QPen( 1 );
   if( p )
      ( p )->setJoinStyle( ( Qt::PenJoinStyle ) hb_parni( 2 ) );
}

/* void setMiterLimit ( qreal limit ) */
HB_FUNC( QT_QPEN_SETMITERLIMIT )
{
   QPen * p = hbqt_par_QPen( 1 );
   if( p )
      ( p )->setMiterLimit( hb_parnd( 2 ) );
}

/* void setStyle ( Qt::PenStyle style ) */
HB_FUNC( QT_QPEN_SETSTYLE )
{
   QPen * p = hbqt_par_QPen( 1 );
   if( p )
      ( p )->setStyle( ( Qt::PenStyle ) hb_parni( 2 ) );
}

/* void setWidth ( int width ) */
HB_FUNC( QT_QPEN_SETWIDTH )
{
   QPen * p = hbqt_par_QPen( 1 );
   if( p )
      ( p )->setWidth( hb_parni( 2 ) );
}

/* void setWidthF ( qreal width ) */
HB_FUNC( QT_QPEN_SETWIDTHF )
{
   QPen * p = hbqt_par_QPen( 1 );
   if( p )
      ( p )->setWidthF( hb_parnd( 2 ) );
}

/* Qt::PenStyle style () const */
HB_FUNC( QT_QPEN_STYLE )
{
   QPen * p = hbqt_par_QPen( 1 );
   if( p )
      hb_retni( ( Qt::PenStyle ) ( p )->style() );
}

/* int width () const */
HB_FUNC( QT_QPEN_WIDTH )
{
   QPen * p = hbqt_par_QPen( 1 );
   if( p )
      hb_retni( ( p )->width() );
}

/* qreal widthF () const */
HB_FUNC( QT_QPEN_WIDTHF )
{
   QPen * p = hbqt_par_QPen( 1 );
   if( p )
      hb_retnd( ( p )->widthF() );
}


#endif /* #if QT_VERSION >= 0x040500 */
