/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */
/*----------------------------------------------------------------------*/

#include "hbqtcore.h"
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum FormatType { InvalidFormat, BlockFormat, CharFormat, ListFormat, ..., UserFormat }
 *  enum ObjectTypes { NoObject, ImageObject, TableObject, TableCellObject, UserObject }
 *  enum PageBreakFlag { PageBreak_Auto, PageBreak_AlwaysBefore, PageBreak_AlwaysAfter }
 *  flags PageBreakFlags
 *  enum Property { ObjectIndex, CssFloat, LayoutDirection, OutlinePen, ..., UserProperty }
 */

/*
 *  Constructed[ 41/45 [ 91.11% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QVector<QTextLength> lengthVectorProperty ( int propertyId ) const
 *  QMap<int, QVariant> properties () const
 *  void setProperty ( int propertyId, const QVector<QTextLength> & value )
 *
 *  *** Commented out protos which construct fine but do not compile ***
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
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QTextFormat;

QT_G_FUNC( hbqt_gcRelease_QTextFormat )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QTextFormat   /.\\", p->ph ) );
         delete ( ( QTextFormat * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QTextFormat   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QTextFormat    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QTextFormat    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextFormat( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QTextFormat * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextFormat;
   p->type = HBQT_TYPE_QTextFormat;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QTextFormat", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QTextFormat", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QTEXTFORMAT )
{
   QTextFormat * pObj = NULL;

   pObj =  new QTextFormat() ;

   hb_retptrGC( hbqt_gcAllocate_QTextFormat( ( void * ) pObj, true ) );
}

/*
 * QBrush background () const
 */
HB_FUNC( QT_QTEXTFORMAT_BACKGROUND )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->background() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_BACKGROUND FP=hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->background() ), true ) ); p is NULL" ) );
   }
}

/*
 * bool boolProperty ( int propertyId ) const
 */
HB_FUNC( QT_QTEXTFORMAT_BOOLPROPERTY )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retl( ( p )->boolProperty( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_BOOLPROPERTY FP=hb_retl( ( p )->boolProperty( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QBrush brushProperty ( int propertyId ) const
 */
HB_FUNC( QT_QTEXTFORMAT_BRUSHPROPERTY )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->brushProperty( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_BRUSHPROPERTY FP=hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->brushProperty( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * void clearBackground ()
 */
HB_FUNC( QT_QTEXTFORMAT_CLEARBACKGROUND )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      ( p )->clearBackground();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_CLEARBACKGROUND FP=( p )->clearBackground(); p is NULL" ) );
   }
}

/*
 * void clearForeground ()
 */
HB_FUNC( QT_QTEXTFORMAT_CLEARFOREGROUND )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      ( p )->clearForeground();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_CLEARFOREGROUND FP=( p )->clearForeground(); p is NULL" ) );
   }
}

/*
 * void clearProperty ( int propertyId )
 */
HB_FUNC( QT_QTEXTFORMAT_CLEARPROPERTY )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      ( p )->clearProperty( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_CLEARPROPERTY FP=( p )->clearProperty( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * QColor colorProperty ( int propertyId ) const
 */
HB_FUNC( QT_QTEXTFORMAT_COLORPROPERTY )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->colorProperty( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_COLORPROPERTY FP=hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( ( p )->colorProperty( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * qreal doubleProperty ( int propertyId ) const
 */
HB_FUNC( QT_QTEXTFORMAT_DOUBLEPROPERTY )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retnd( ( p )->doubleProperty( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_DOUBLEPROPERTY FP=hb_retnd( ( p )->doubleProperty( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QBrush foreground () const
 */
HB_FUNC( QT_QTEXTFORMAT_FOREGROUND )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->foreground() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_FOREGROUND FP=hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( ( p )->foreground() ), true ) ); p is NULL" ) );
   }
}

/*
 * bool hasProperty ( int propertyId ) const
 */
HB_FUNC( QT_QTEXTFORMAT_HASPROPERTY )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retl( ( p )->hasProperty( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_HASPROPERTY FP=hb_retl( ( p )->hasProperty( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * int intProperty ( int propertyId ) const
 */
HB_FUNC( QT_QTEXTFORMAT_INTPROPERTY )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retni( ( p )->intProperty( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_INTPROPERTY FP=hb_retni( ( p )->intProperty( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool isBlockFormat () const
 */
HB_FUNC( QT_QTEXTFORMAT_ISBLOCKFORMAT )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retl( ( p )->isBlockFormat() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_ISBLOCKFORMAT FP=hb_retl( ( p )->isBlockFormat() ); p is NULL" ) );
   }
}

/*
 * bool isCharFormat () const
 */
HB_FUNC( QT_QTEXTFORMAT_ISCHARFORMAT )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retl( ( p )->isCharFormat() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_ISCHARFORMAT FP=hb_retl( ( p )->isCharFormat() ); p is NULL" ) );
   }
}

/*
 * bool isFrameFormat () const
 */
HB_FUNC( QT_QTEXTFORMAT_ISFRAMEFORMAT )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retl( ( p )->isFrameFormat() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_ISFRAMEFORMAT FP=hb_retl( ( p )->isFrameFormat() ); p is NULL" ) );
   }
}

/*
 * bool isImageFormat () const
 */
HB_FUNC( QT_QTEXTFORMAT_ISIMAGEFORMAT )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retl( ( p )->isImageFormat() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_ISIMAGEFORMAT FP=hb_retl( ( p )->isImageFormat() ); p is NULL" ) );
   }
}

/*
 * bool isListFormat () const
 */
HB_FUNC( QT_QTEXTFORMAT_ISLISTFORMAT )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retl( ( p )->isListFormat() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_ISLISTFORMAT FP=hb_retl( ( p )->isListFormat() ); p is NULL" ) );
   }
}

/*
 * bool isTableCellFormat () const
 */
HB_FUNC( QT_QTEXTFORMAT_ISTABLECELLFORMAT )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retl( ( p )->isTableCellFormat() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_ISTABLECELLFORMAT FP=hb_retl( ( p )->isTableCellFormat() ); p is NULL" ) );
   }
}

/*
 * bool isTableFormat () const
 */
HB_FUNC( QT_QTEXTFORMAT_ISTABLEFORMAT )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retl( ( p )->isTableFormat() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_ISTABLEFORMAT FP=hb_retl( ( p )->isTableFormat() ); p is NULL" ) );
   }
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QTEXTFORMAT_ISVALID )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_ISVALID FP=hb_retl( ( p )->isValid() ); p is NULL" ) );
   }
}

/*
 * Qt::LayoutDirection layoutDirection () const
 */
HB_FUNC( QT_QTEXTFORMAT_LAYOUTDIRECTION )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retni( ( Qt::LayoutDirection ) ( p )->layoutDirection() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_LAYOUTDIRECTION FP=hb_retni( ( Qt::LayoutDirection ) ( p )->layoutDirection() ); p is NULL" ) );
   }
}

/*
 * QTextLength lengthProperty ( int propertyId ) const
 */
HB_FUNC( QT_QTEXTFORMAT_LENGTHPROPERTY )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextLength( new QTextLength( ( p )->lengthProperty( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_LENGTHPROPERTY FP=hb_retptrGC( hbqt_gcAllocate_QTextLength( new QTextLength( ( p )->lengthProperty( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * void merge ( const QTextFormat & other )
 */
HB_FUNC( QT_QTEXTFORMAT_MERGE )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      ( p )->merge( *hbqt_par_QTextFormat( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_MERGE FP=( p )->merge( *hbqt_par_QTextFormat( 2 ) ); p is NULL" ) );
   }
}

/*
 * int objectIndex () const
 */
HB_FUNC( QT_QTEXTFORMAT_OBJECTINDEX )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retni( ( p )->objectIndex() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_OBJECTINDEX FP=hb_retni( ( p )->objectIndex() ); p is NULL" ) );
   }
}

/*
 * int objectType () const
 */
HB_FUNC( QT_QTEXTFORMAT_OBJECTTYPE )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retni( ( p )->objectType() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_OBJECTTYPE FP=hb_retni( ( p )->objectType() ); p is NULL" ) );
   }
}

/*
 * QPen penProperty ( int propertyId ) const
 */
HB_FUNC( QT_QTEXTFORMAT_PENPROPERTY )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPen( new QPen( ( p )->penProperty( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_PENPROPERTY FP=hb_retptrGC( hbqt_gcAllocate_QPen( new QPen( ( p )->penProperty( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QVariant property ( int propertyId ) const
 */
HB_FUNC( QT_QTEXTFORMAT_PROPERTY )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->property( hb_parni( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_PROPERTY FP=hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->property( hb_parni( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * int propertyCount () const
 */
HB_FUNC( QT_QTEXTFORMAT_PROPERTYCOUNT )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retni( ( p )->propertyCount() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_PROPERTYCOUNT FP=hb_retni( ( p )->propertyCount() ); p is NULL" ) );
   }
}

/*
 * void setBackground ( const QBrush & brush )
 */
HB_FUNC( QT_QTEXTFORMAT_SETBACKGROUND )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      ( p )->setBackground( *hbqt_par_QBrush( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_SETBACKGROUND FP=( p )->setBackground( *hbqt_par_QBrush( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setForeground ( const QBrush & brush )
 */
HB_FUNC( QT_QTEXTFORMAT_SETFOREGROUND )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      ( p )->setForeground( *hbqt_par_QBrush( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_SETFOREGROUND FP=( p )->setForeground( *hbqt_par_QBrush( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setLayoutDirection ( Qt::LayoutDirection direction )
 */
HB_FUNC( QT_QTEXTFORMAT_SETLAYOUTDIRECTION )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      ( p )->setLayoutDirection( ( Qt::LayoutDirection ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_SETLAYOUTDIRECTION FP=( p )->setLayoutDirection( ( Qt::LayoutDirection ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setObjectIndex ( int index )
 */
HB_FUNC( QT_QTEXTFORMAT_SETOBJECTINDEX )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      ( p )->setObjectIndex( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_SETOBJECTINDEX FP=( p )->setObjectIndex( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setObjectType ( int type )
 */
HB_FUNC( QT_QTEXTFORMAT_SETOBJECTTYPE )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      ( p )->setObjectType( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_SETOBJECTTYPE FP=( p )->setObjectType( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setProperty ( int propertyId, const QVariant & value )
 */
HB_FUNC( QT_QTEXTFORMAT_SETPROPERTY )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      ( p )->setProperty( hb_parni( 2 ), *hbqt_par_QVariant( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_SETPROPERTY FP=( p )->setProperty( hb_parni( 2 ), *hbqt_par_QVariant( 3 ) ); p is NULL" ) );
   }
}

/*
 * QString stringProperty ( int propertyId ) const
 */
HB_FUNC( QT_QTEXTFORMAT_STRINGPROPERTY )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retc( ( p )->stringProperty( hb_parni( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_STRINGPROPERTY FP=hb_retc( ( p )->stringProperty( hb_parni( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QTextBlockFormat toBlockFormat () const
 */
HB_FUNC( QT_QTEXTFORMAT_TOBLOCKFORMAT )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextBlockFormat( new QTextBlockFormat( ( p )->toBlockFormat() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_TOBLOCKFORMAT FP=hb_retptrGC( hbqt_gcAllocate_QTextBlockFormat( new QTextBlockFormat( ( p )->toBlockFormat() ), true ) ); p is NULL" ) );
   }
}

/*
 * QTextCharFormat toCharFormat () const
 */
HB_FUNC( QT_QTEXTFORMAT_TOCHARFORMAT )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( ( p )->toCharFormat() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_TOCHARFORMAT FP=hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( ( p )->toCharFormat() ), true ) ); p is NULL" ) );
   }
}

/*
 * QTextFrameFormat toFrameFormat () const
 */
HB_FUNC( QT_QTEXTFORMAT_TOFRAMEFORMAT )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextFrameFormat( new QTextFrameFormat( ( p )->toFrameFormat() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_TOFRAMEFORMAT FP=hb_retptrGC( hbqt_gcAllocate_QTextFrameFormat( new QTextFrameFormat( ( p )->toFrameFormat() ), true ) ); p is NULL" ) );
   }
}

/*
 * QTextImageFormat toImageFormat () const
 */
HB_FUNC( QT_QTEXTFORMAT_TOIMAGEFORMAT )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextImageFormat( new QTextImageFormat( ( p )->toImageFormat() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_TOIMAGEFORMAT FP=hb_retptrGC( hbqt_gcAllocate_QTextImageFormat( new QTextImageFormat( ( p )->toImageFormat() ), true ) ); p is NULL" ) );
   }
}

/*
 * QTextListFormat toListFormat () const
 */
HB_FUNC( QT_QTEXTFORMAT_TOLISTFORMAT )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextListFormat( new QTextListFormat( ( p )->toListFormat() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_TOLISTFORMAT FP=hb_retptrGC( hbqt_gcAllocate_QTextListFormat( new QTextListFormat( ( p )->toListFormat() ), true ) ); p is NULL" ) );
   }
}

/*
 * QTextTableFormat toTableFormat () const
 */
HB_FUNC( QT_QTEXTFORMAT_TOTABLEFORMAT )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextTableFormat( new QTextTableFormat( ( p )->toTableFormat() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_TOTABLEFORMAT FP=hb_retptrGC( hbqt_gcAllocate_QTextTableFormat( new QTextTableFormat( ( p )->toTableFormat() ), true ) ); p is NULL" ) );
   }
}

/*
 * int type () const
 */
HB_FUNC( QT_QTEXTFORMAT_TYPE )
{
   QTextFormat * p = hbqt_par_QTextFormat( 1 );
   if( p )
      hb_retni( ( p )->type() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFORMAT_TYPE FP=hb_retni( ( p )->type() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
