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
 *  Constructed[ 68/68 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QGraphicsItem>
#include "hbqt_hbqgraphicsitem.h"

/*
 * HBQGraphicsItem ()
 * HBQGraphicsItem ( QGraphicsItem * parent )
 * HBQGraphicsItem ( int type, QGraphicsItem * parent )
 * ~HBQGraphicsItem ()
 */

typedef struct
{
   HBQGraphicsItem * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_HBQGraphicsItem;

HBQT_GC_FUNC( hbqt_gcRelease_HBQGraphicsItem )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( HBQGraphicsItem * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_HBQGraphicsItem( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( HBQGraphicsItem * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_HBQGraphicsItem;
   p->type = HBQT_TYPE_HBQGraphicsItem;

   return p;
}

HB_FUNC( QT_HBQGRAPHICSITEM )
{
   HBQGraphicsItem * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISNUM( 1 ) )
   {
      pObj = new HBQGraphicsItem( hb_parni( 1 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISNUM( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj = new HBQGraphicsItem( hb_parni( 1 ), hbqt_par_QGraphicsItem( 2 ) ) ;
   }
   else {
      pObj = new HBQGraphicsItem() ;
   }

   hb_retptrGC( hbqt_gcAllocate_HBQGraphicsItem( ( void * ) pObj, true ) );
}

/* void           hbSetBlock( PHB_ITEM block ) */
HB_FUNC( QT_HBQGRAPHICSITEM_HBSETBLOCK )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->hbSetBlock( hb_param( 2, HB_IT_ANY ) );
}

/* QRectF         boundingRect() const */
HB_FUNC( QT_HBQGRAPHICSITEM_BOUNDINGRECT )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->boundingRect() ), true ) );
}

/* virtual void   paint( QPainter * painter, const QStyleOptionGraphicsItem * option, QWidget * widget = 0 ) */
HB_FUNC( QT_HBQGRAPHICSITEM_PAINT )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->paint( hbqt_par_QPainter( 2 ), hbqt_par_QStyleOptionGraphicsItem( 3 ), hbqt_par_QWidget( 4 ) );
}

/* int            determineResizeMode( const QPointF & pos ) */
HB_FUNC( QT_HBQGRAPHICSITEM_DETERMINERESIZEMODE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retni( ( p )->determineResizeMode( *hbqt_par_QPointF( 2 ) ) );
}

/* QRectF         adjustRect( QRectF & rect ) */
HB_FUNC( QT_HBQGRAPHICSITEM_ADJUSTRECT )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->adjustRect( *hbqt_par_QRectF( 2 ) ) ), true ) );
}

/* virtual void   prepare( QPainter * painter ) */
HB_FUNC( QT_HBQGRAPHICSITEM_PREPARE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->prepare( hbqt_par_QPainter( 2 ) );
}

/* QPen           pen() */
HB_FUNC( QT_HBQGRAPHICSITEM_PEN )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPen( new QPen( ( p )->pen() ), true ) );
}

/* void           setPen( const QPen & pen ) */
HB_FUNC( QT_HBQGRAPHICSITEM_SETPEN )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setPen( *hbqt_par_QPen( 2 ) );
}

/* QBrush         brush() */
HB_FUNC( QT_HBQGRAPHICSITEM_BRUSH )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->brush() ), true ) );
}

/* void           setBrush( const QBrush & brush ) */
HB_FUNC( QT_HBQGRAPHICSITEM_SETBRUSH )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setBrush( *hbqt_par_QBrush( 2 ) );
}

/* QBrush         backgroundBrush() */
HB_FUNC( QT_HBQGRAPHICSITEM_BACKGROUNDBRUSH )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->backgroundBrush() ), true ) );
}

/* void           setBackgroundBrush( const QBrush & brush ) */
HB_FUNC( QT_HBQGRAPHICSITEM_SETBACKGROUNDBRUSH )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setBackgroundBrush( *hbqt_par_QBrush( 2 ) );
}

/* QFont          font() */
HB_FUNC( QT_HBQGRAPHICSITEM_FONT )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QFont( new QFont( ( p )->font() ), true ) );
}

/* void           setFont( const QFont & font ) */
HB_FUNC( QT_HBQGRAPHICSITEM_SETFONT )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setFont( *hbqt_par_QFont( 2 ) );
}

/* int            lineStyle() */
HB_FUNC( QT_HBQGRAPHICSITEM_LINESTYLE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retni( ( p )->lineStyle() );
}

/* void           setLineStyle( int lineStyle ) */
HB_FUNC( QT_HBQGRAPHICSITEM_SETLINESTYLE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setLineStyle( hb_parni( 2 ) );
}

/* int            startAngle() */
HB_FUNC( QT_HBQGRAPHICSITEM_STARTANGLE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retni( ( p )->startAngle() );
}

/* void           setStartAngle( int startAngle ) */
HB_FUNC( QT_HBQGRAPHICSITEM_SETSTARTANGLE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setStartAngle( hb_parni( 2 ) );
}

/* int            spanAngle() */
HB_FUNC( QT_HBQGRAPHICSITEM_SPANANGLE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retni( ( p )->spanAngle() );
}

/* void           setSpanAngle( int spanAngle ) */
HB_FUNC( QT_HBQGRAPHICSITEM_SETSPANANGLE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setSpanAngle( hb_parni( 2 ) );
}

/* qreal          width() const */
HB_FUNC( QT_HBQGRAPHICSITEM_WIDTH )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retnd( ( p )->width() );
}

/* void           setWidth( qreal width ) */
HB_FUNC( QT_HBQGRAPHICSITEM_SETWIDTH )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setWidth( hb_parnd( 2 ) );
}

/* qreal          height() const */
HB_FUNC( QT_HBQGRAPHICSITEM_HEIGHT )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retnd( ( p )->height() );
}

/* void           setHeight( qreal height ) */
HB_FUNC( QT_HBQGRAPHICSITEM_SETHEIGHT )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setHeight( hb_parnd( 2 ) );
}

/* int            opacity() */
HB_FUNC( QT_HBQGRAPHICSITEM_OPACITY )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retni( ( p )->opacity() );
}

/* void           setOpacity( const int opacity ) */
HB_FUNC( QT_HBQGRAPHICSITEM_SETOPACITY )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setOpacity( hb_parni( 2 ) );
}

/* QRectF         geometry() */
HB_FUNC( QT_HBQGRAPHICSITEM_GEOMETRY )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->geometry() ), true ) );
}

/* void           setGeometry( const QRectF & rect ) */
HB_FUNC( QT_HBQGRAPHICSITEM_SETGEOMETRY )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setGeometry( *hbqt_par_QRectF( 2 ) );
}

/* QString        objectType() */
HB_FUNC( QT_HBQGRAPHICSITEM_OBJECTTYPE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retstr_utf8( ( p )->objectType().toUtf8().data() );
}

/* void           setObjectType( const QString & type ) */
HB_FUNC( QT_HBQGRAPHICSITEM_SETOBJECTTYPE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
   {
      void * pText;
      ( p )->setObjectType( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* QString        objectName() */
HB_FUNC( QT_HBQGRAPHICSITEM_OBJECTNAME )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retstr_utf8( ( p )->objectName().toUtf8().data() );
}

/* void           setObjectName( const QString & name ) */
HB_FUNC( QT_HBQGRAPHICSITEM_SETOBJECTNAME )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
   {
      void * pText;
      ( p )->setObjectName( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* QString        text() */
HB_FUNC( QT_HBQGRAPHICSITEM_TEXT )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retstr_utf8( ( p )->text().toUtf8().data() );
}

/* void           setText( const QString & type ) */
HB_FUNC( QT_HBQGRAPHICSITEM_SETTEXT )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
   {
      void * pText;
      ( p )->setText( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/* int            paintType() */
HB_FUNC( QT_HBQGRAPHICSITEM_PAINTTYPE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retni( ( p )->paintType() );
}

/* void           setPaintType( int paintType ) */
HB_FUNC( QT_HBQGRAPHICSITEM_SETPAINTTYPE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setPaintType( hb_parni( 2 ) );
}

/* int            frameType() */
HB_FUNC( QT_HBQGRAPHICSITEM_FRAMETYPE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retni( ( p )->frameType() );
}

/* void           setFrameType( int frameType ) */
HB_FUNC( QT_HBQGRAPHICSITEM_SETFRAMETYPE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setFrameType( hb_parni( 2 ) );
}

/* int            drawTextType() */
HB_FUNC( QT_HBQGRAPHICSITEM_DRAWTEXTTYPE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retni( ( p )->drawTextType() );
}

/* void           setDrawTextType( int drawTextType ) */
HB_FUNC( QT_HBQGRAPHICSITEM_SETDRAWTEXTTYPE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setDrawTextType( hb_parni( 2 ) );
}

/* QPixmap        pixmap() */
HB_FUNC( QT_HBQGRAPHICSITEM_PIXMAP )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPixmap( new QPixmap( ( p )->pixmap() ), true ) );
}

/* void           setPixmap( const QPixmap & pixmap ) */
HB_FUNC( QT_HBQGRAPHICSITEM_SETPIXMAP )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setPixmap( *hbqt_par_QPixmap( 2 ) );
}

/* QColor         textColor() */
HB_FUNC( QT_HBQGRAPHICSITEM_TEXTCOLOR )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->textColor() ), true ) );
}

/* void           setTextColor( const QColor & color ) */
HB_FUNC( QT_HBQGRAPHICSITEM_SETTEXTCOLOR )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setTextColor( *hbqt_par_QColor( 2 ) );
}

/* int            borderWidth() */
HB_FUNC( QT_HBQGRAPHICSITEM_BORDERWIDTH )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retni( ( p )->borderWidth() );
}

/* void           setBorderWidth( int bWidth ) */
HB_FUNC( QT_HBQGRAPHICSITEM_SETBORDERWIDTH )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setBorderWidth( hb_parni( 2 ) );
}

/* QColor         borderColor() */
HB_FUNC( QT_HBQGRAPHICSITEM_BORDERCOLOR )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->borderColor() ), true ) );
}

/* void           setBorderColor( const QColor & color ) */
HB_FUNC( QT_HBQGRAPHICSITEM_SETBORDERCOLOR )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setBorderColor( *hbqt_par_QColor( 2 ) );
}

/* int            sizePolicy() */
HB_FUNC( QT_HBQGRAPHICSITEM_SIZEPOLICY )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retni( ( p )->sizePolicy() );
}

/* void           setSizePolicy( int sizePolicy ) */
HB_FUNC( QT_HBQGRAPHICSITEM_SETSIZEPOLICY )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setSizePolicy( hb_parni( 2 ) );
}

/* int            textFlags() */
HB_FUNC( QT_HBQGRAPHICSITEM_TEXTFLAGS )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retni( ( p )->textFlags() );
}

/* void           setTextFlags( int textFlags ) */
HB_FUNC( QT_HBQGRAPHICSITEM_SETTEXTFLAGS )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setTextFlags( hb_parni( 2 ) );
}

/* int            resizeFlags() */
HB_FUNC( QT_HBQGRAPHICSITEM_RESIZEFLAGS )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retni( ( p )->resizeFlags() );
}

/* void           setResizeFlags( int resizeFlags ) */
HB_FUNC( QT_HBQGRAPHICSITEM_SETRESIZEFLAGS )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setResizeFlags( hb_parni( 2 ) );
}

/* int            resizeHandle() */
HB_FUNC( QT_HBQGRAPHICSITEM_RESIZEHANDLE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retni( ( p )->resizeHandle() );
}

/* void           setResizeHandle( int resizeHandle ) */
HB_FUNC( QT_HBQGRAPHICSITEM_SETRESIZEHANDLE )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setResizeHandle( hb_parni( 2 ) );
}

/* int            barsIdentation() */
HB_FUNC( QT_HBQGRAPHICSITEM_BARSIDENTATION )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retni( ( p )->barsIdentation() );
}

/* void           setBarsIdentation( int barsIdentation ) */
HB_FUNC( QT_HBQGRAPHICSITEM_SETBARSIDENTATION )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setBarsIdentation( hb_parni( 2 ) );
}

/* bool           drawBorder() */
HB_FUNC( QT_HBQGRAPHICSITEM_DRAWBORDER )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->drawBorder() );
}

/* void           setDrawBorder( bool drawBorder ) */
HB_FUNC( QT_HBQGRAPHICSITEM_SETDRAWBORDER )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setDrawBorder( hb_parl( 2 ) );
}

/* bool           showGrid() */
HB_FUNC( QT_HBQGRAPHICSITEM_SHOWGRID )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->showGrid() );
}

/* void           setShowGrid( bool showGrid ) */
HB_FUNC( QT_HBQGRAPHICSITEM_SETSHOWGRID )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setShowGrid( hb_parl( 2 ) );
}

/* bool           showLabels() */
HB_FUNC( QT_HBQGRAPHICSITEM_SHOWLABELS )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retl( ( p )->showLabels() );
}

/* void           setShowLabels( bool showLabels ) */
HB_FUNC( QT_HBQGRAPHICSITEM_SETSHOWLABELS )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setShowLabels( hb_parl( 2 ) );
}

/* qreal          toColorFactor() */
HB_FUNC( QT_HBQGRAPHICSITEM_TOCOLORFACTOR )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      hb_retnd( ( p )->toColorFactor() );
}

/* void           setToColorFactor( qreal toColorFactor ) */
HB_FUNC( QT_HBQGRAPHICSITEM_SETTOCOLORFACTOR )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setToColorFactor( hb_parnd( 2 ) );
}

/* void           setBarValues( const QStringList & list ) */
HB_FUNC( QT_HBQGRAPHICSITEM_SETBARVALUES )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setBarValues( *hbqt_par_QStringList( 2 ) );
}

/* void           setLegendColorRectWidth( int legendColorRectWidth ) */
HB_FUNC( QT_HBQGRAPHICSITEM_SETLEGENDCOLORRECTWIDTH )
{
   HBQGraphicsItem * p = hbqt_par_HBQGraphicsItem( 1 );
   if( p )
      ( p )->setLegendColorRectWidth( hb_parni( 2 ) );
}


#endif /* #if QT_VERSION >= 0x040500 */
