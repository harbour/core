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
 *  enum FormatType { InvalidFormat, BlockFormat, CharFormat, ListFormat, ..., UserFormat }
 *  enum ObjectTypes { NoObject, ImageObject, TableObject, TableCellObject, UserObject }
 *  enum PageBreakFlag { PageBreak_Auto, PageBreak_AlwaysBefore, PageBreak_AlwaysAfter }
 *  flags PageBreakFlags
 *  enum Property { ObjectIndex, CssFloat, LayoutDirection, OutlinePen, ..., UserProperty }
 */

/*
 *  Constructed[ 41/44 [ 93.18% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  QVector<QTextLength> lengthVectorProperty ( int propertyId ) const
 *  QMap<int, QVariant> properties () const
 *  void setProperty ( int propertyId, const QVector<QTextLength> & value )
 *
 *  *** Commented out protostypes ***
 *
 *  // QTextTableCellFormat toTableCellFormat () const
 */

#include <QtCore/QPointer>

#include <QtGui/QTextFormat>


/*
 * QTextFormat ()
 * QTextFormat ( int type )
 * QTextFormat ( const QTextFormat & other )
 * ~QTextFormat ()
 */

typedef struct
{
   QTextFormat * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QTextFormat;

HBQT_GC_FUNC( hbqt_gcRelease_QTextFormat )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p )
   {
      if( p->bNew && p->ph )
         delete ( ( QTextFormat * ) p->ph );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextFormat( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QTextFormat * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextFormat;
   p->type = HBQT_TYPE_QTextFormat;

   return p;
}

HB_FUNC( QT_QTEXTFORMAT )
{
   QTextFormat * pObj = NULL;

   pObj = new QTextFormat() ;

   hb_retptrGC( hbqt_gcAllocate_QTextFormat( ( void * ) pObj, true ) );
}

/* QBrush background () const */
HB_FUNC( QT_QTEXTFORMAT_BACKGROUND )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->background() ), true ) );
}

/* bool boolProperty ( int propertyId ) const */
HB_FUNC( QT_QTEXTFORMAT_BOOLPROPERTY )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retl( ( p )->boolProperty( hb_parni( 2 ) ) );
}

/* QBrush brushProperty ( int propertyId ) const */
HB_FUNC( QT_QTEXTFORMAT_BRUSHPROPERTY )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->brushProperty( hb_parni( 2 ) ) ), true ) );
}

/* void clearBackground () */
HB_FUNC( QT_QTEXTFORMAT_CLEARBACKGROUND )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      ( p )->clearBackground();
}

/* void clearForeground () */
HB_FUNC( QT_QTEXTFORMAT_CLEARFOREGROUND )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      ( p )->clearForeground();
}

/* void clearProperty ( int propertyId ) */
HB_FUNC( QT_QTEXTFORMAT_CLEARPROPERTY )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      ( p )->clearProperty( hb_parni( 2 ) );
}

/* QColor colorProperty ( int propertyId ) const */
HB_FUNC( QT_QTEXTFORMAT_COLORPROPERTY )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->colorProperty( hb_parni( 2 ) ) ), true ) );
}

/* qreal doubleProperty ( int propertyId ) const */
HB_FUNC( QT_QTEXTFORMAT_DOUBLEPROPERTY )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retnd( ( p )->doubleProperty( hb_parni( 2 ) ) );
}

/* QBrush foreground () const */
HB_FUNC( QT_QTEXTFORMAT_FOREGROUND )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->foreground() ), true ) );
}

/* bool hasProperty ( int propertyId ) const */
HB_FUNC( QT_QTEXTFORMAT_HASPROPERTY )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retl( ( p )->hasProperty( hb_parni( 2 ) ) );
}

/* int intProperty ( int propertyId ) const */
HB_FUNC( QT_QTEXTFORMAT_INTPROPERTY )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retni( ( p )->intProperty( hb_parni( 2 ) ) );
}

/* bool isBlockFormat () const */
HB_FUNC( QT_QTEXTFORMAT_ISBLOCKFORMAT )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retl( ( p )->isBlockFormat() );
}

/* bool isCharFormat () const */
HB_FUNC( QT_QTEXTFORMAT_ISCHARFORMAT )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retl( ( p )->isCharFormat() );
}

/* bool isFrameFormat () const */
HB_FUNC( QT_QTEXTFORMAT_ISFRAMEFORMAT )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retl( ( p )->isFrameFormat() );
}

/* bool isImageFormat () const */
HB_FUNC( QT_QTEXTFORMAT_ISIMAGEFORMAT )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retl( ( p )->isImageFormat() );
}

/* bool isListFormat () const */
HB_FUNC( QT_QTEXTFORMAT_ISLISTFORMAT )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retl( ( p )->isListFormat() );
}

/* bool isTableCellFormat () const */
HB_FUNC( QT_QTEXTFORMAT_ISTABLECELLFORMAT )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retl( ( p )->isTableCellFormat() );
}

/* bool isTableFormat () const */
HB_FUNC( QT_QTEXTFORMAT_ISTABLEFORMAT )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retl( ( p )->isTableFormat() );
}

/* bool isValid () const */
HB_FUNC( QT_QTEXTFORMAT_ISVALID )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
}

/* Qt::LayoutDirection layoutDirection () const */
HB_FUNC( QT_QTEXTFORMAT_LAYOUTDIRECTION )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retni( ( Qt::LayoutDirection ) ( p )->layoutDirection() );
}

/* QTextLength lengthProperty ( int propertyId ) const */
HB_FUNC( QT_QTEXTFORMAT_LENGTHPROPERTY )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextLength( new QTextLength( ( p )->lengthProperty( hb_parni( 2 ) ) ), true ) );
}

/* void merge ( const QTextFormat & other ) */
HB_FUNC( QT_QTEXTFORMAT_MERGE )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      ( p )->merge( *hbqt_par_QTextFormat( 2 ) );
}

/* int objectIndex () const */
HB_FUNC( QT_QTEXTFORMAT_OBJECTINDEX )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retni( ( p )->objectIndex() );
}

/* int objectType () const */
HB_FUNC( QT_QTEXTFORMAT_OBJECTTYPE )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retni( ( p )->objectType() );
}

/* QPen penProperty ( int propertyId ) const */
HB_FUNC( QT_QTEXTFORMAT_PENPROPERTY )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPen( new QPen( ( p )->penProperty( hb_parni( 2 ) ) ), true ) );
}

/* QVariant property ( int propertyId ) const */
HB_FUNC( QT_QTEXTFORMAT_PROPERTY )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->property( hb_parni( 2 ) ) ), true ) );
}

/* int propertyCount () const */
HB_FUNC( QT_QTEXTFORMAT_PROPERTYCOUNT )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retni( ( p )->propertyCount() );
}

/* void setBackground ( const QBrush & brush ) */
HB_FUNC( QT_QTEXTFORMAT_SETBACKGROUND )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      ( p )->setBackground( *hbqt_par_QBrush( 2 ) );
}

/* void setForeground ( const QBrush & brush ) */
HB_FUNC( QT_QTEXTFORMAT_SETFOREGROUND )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      ( p )->setForeground( *hbqt_par_QBrush( 2 ) );
}

/* void setLayoutDirection ( Qt::LayoutDirection direction ) */
HB_FUNC( QT_QTEXTFORMAT_SETLAYOUTDIRECTION )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      ( p )->setLayoutDirection( ( Qt::LayoutDirection ) hb_parni( 2 ) );
}

/* void setObjectIndex ( int index ) */
HB_FUNC( QT_QTEXTFORMAT_SETOBJECTINDEX )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      ( p )->setObjectIndex( hb_parni( 2 ) );
}

/* void setObjectType ( int type ) */
HB_FUNC( QT_QTEXTFORMAT_SETOBJECTTYPE )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      ( p )->setObjectType( hb_parni( 2 ) );
}

/* void setProperty ( int propertyId, const QVariant & value ) */
HB_FUNC( QT_QTEXTFORMAT_SETPROPERTY )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      ( p )->setProperty( hb_parni( 2 ), *hbqt_par_QVariant( 3 ) );
}

/* QString stringProperty ( int propertyId ) const */
HB_FUNC( QT_QTEXTFORMAT_STRINGPROPERTY )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retstr_utf8( ( p )->stringProperty( hb_parni( 2 ) ).toUtf8().data() );
}

/* QTextBlockFormat toBlockFormat () const */
HB_FUNC( QT_QTEXTFORMAT_TOBLOCKFORMAT )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextBlockFormat( new QTextBlockFormat( ( p )->toBlockFormat() ), true ) );
}

/* QTextCharFormat toCharFormat () const */
HB_FUNC( QT_QTEXTFORMAT_TOCHARFORMAT )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( ( p )->toCharFormat() ), true ) );
}

/* QTextFrameFormat toFrameFormat () const */
HB_FUNC( QT_QTEXTFORMAT_TOFRAMEFORMAT )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextFrameFormat( new QTextFrameFormat( ( p )->toFrameFormat() ), true ) );
}

/* QTextImageFormat toImageFormat () const */
HB_FUNC( QT_QTEXTFORMAT_TOIMAGEFORMAT )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextImageFormat( new QTextImageFormat( ( p )->toImageFormat() ), true ) );
}

/* QTextListFormat toListFormat () const */
HB_FUNC( QT_QTEXTFORMAT_TOLISTFORMAT )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextListFormat( new QTextListFormat( ( p )->toListFormat() ), true ) );
}

/* QTextTableFormat toTableFormat () const */
HB_FUNC( QT_QTEXTFORMAT_TOTABLEFORMAT )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextTableFormat( new QTextTableFormat( ( p )->toTableFormat() ), true ) );
}

/* int type () const */
HB_FUNC( QT_QTEXTFORMAT_TYPE )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retni( ( p )->type() );
}


#endif /* #if QT_VERSION >= 0x040500 */
