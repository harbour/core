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
 * www - http://www.harbour-project.org
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

#include "hbapi.h"
#include "../hbqt.h"

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
  void * ph;
  bool bNew;
  QT_G_FUNC_PTR func;
} QGC_POINTER_QTextFormat;

QT_G_FUNC( hbqt_gcRelease_QTextFormat )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QTextFormat * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QTextFormat                ph=%p %i B %i KB", p->ph, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_QTextFormat                 Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_QTextFormat                 Object not created with - new" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextFormat( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextFormat;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QTextFormat                ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QTEXTFORMAT )
{
   void * pObj = NULL;

   pObj = ( QTextFormat* ) new QTextFormat() ;

   hb_retptrGC( hbqt_gcAllocate_QTextFormat( pObj, true ) );
}

/*
 * QBrush background () const
 */
HB_FUNC( QT_QTEXTFORMAT_BACKGROUND )
{
   hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( hbqt_par_QTextFormat( 1 )->background() ), true ) );
}

/*
 * bool boolProperty ( int propertyId ) const
 */
HB_FUNC( QT_QTEXTFORMAT_BOOLPROPERTY )
{
   hb_retl( hbqt_par_QTextFormat( 1 )->boolProperty( hb_parni( 2 ) ) );
}

/*
 * QBrush brushProperty ( int propertyId ) const
 */
HB_FUNC( QT_QTEXTFORMAT_BRUSHPROPERTY )
{
   hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( hbqt_par_QTextFormat( 1 )->brushProperty( hb_parni( 2 ) ) ), true ) );
}

/*
 * void clearBackground ()
 */
HB_FUNC( QT_QTEXTFORMAT_CLEARBACKGROUND )
{
   hbqt_par_QTextFormat( 1 )->clearBackground();
}

/*
 * void clearForeground ()
 */
HB_FUNC( QT_QTEXTFORMAT_CLEARFOREGROUND )
{
   hbqt_par_QTextFormat( 1 )->clearForeground();
}

/*
 * void clearProperty ( int propertyId )
 */
HB_FUNC( QT_QTEXTFORMAT_CLEARPROPERTY )
{
   hbqt_par_QTextFormat( 1 )->clearProperty( hb_parni( 2 ) );
}

/*
 * QColor colorProperty ( int propertyId ) const
 */
HB_FUNC( QT_QTEXTFORMAT_COLORPROPERTY )
{
   hb_retptrGC( hbqt_gcAllocate_QColor( new QColor( hbqt_par_QTextFormat( 1 )->colorProperty( hb_parni( 2 ) ) ), true ) );
}

/*
 * qreal doubleProperty ( int propertyId ) const
 */
HB_FUNC( QT_QTEXTFORMAT_DOUBLEPROPERTY )
{
   hb_retnd( hbqt_par_QTextFormat( 1 )->doubleProperty( hb_parni( 2 ) ) );
}

/*
 * QBrush foreground () const
 */
HB_FUNC( QT_QTEXTFORMAT_FOREGROUND )
{
   hb_retptrGC( hbqt_gcAllocate_QBrush( new QBrush( hbqt_par_QTextFormat( 1 )->foreground() ), true ) );
}

/*
 * bool hasProperty ( int propertyId ) const
 */
HB_FUNC( QT_QTEXTFORMAT_HASPROPERTY )
{
   hb_retl( hbqt_par_QTextFormat( 1 )->hasProperty( hb_parni( 2 ) ) );
}

/*
 * int intProperty ( int propertyId ) const
 */
HB_FUNC( QT_QTEXTFORMAT_INTPROPERTY )
{
   hb_retni( hbqt_par_QTextFormat( 1 )->intProperty( hb_parni( 2 ) ) );
}

/*
 * bool isBlockFormat () const
 */
HB_FUNC( QT_QTEXTFORMAT_ISBLOCKFORMAT )
{
   hb_retl( hbqt_par_QTextFormat( 1 )->isBlockFormat() );
}

/*
 * bool isCharFormat () const
 */
HB_FUNC( QT_QTEXTFORMAT_ISCHARFORMAT )
{
   hb_retl( hbqt_par_QTextFormat( 1 )->isCharFormat() );
}

/*
 * bool isFrameFormat () const
 */
HB_FUNC( QT_QTEXTFORMAT_ISFRAMEFORMAT )
{
   hb_retl( hbqt_par_QTextFormat( 1 )->isFrameFormat() );
}

/*
 * bool isImageFormat () const
 */
HB_FUNC( QT_QTEXTFORMAT_ISIMAGEFORMAT )
{
   hb_retl( hbqt_par_QTextFormat( 1 )->isImageFormat() );
}

/*
 * bool isListFormat () const
 */
HB_FUNC( QT_QTEXTFORMAT_ISLISTFORMAT )
{
   hb_retl( hbqt_par_QTextFormat( 1 )->isListFormat() );
}

/*
 * bool isTableCellFormat () const
 */
HB_FUNC( QT_QTEXTFORMAT_ISTABLECELLFORMAT )
{
   hb_retl( hbqt_par_QTextFormat( 1 )->isTableCellFormat() );
}

/*
 * bool isTableFormat () const
 */
HB_FUNC( QT_QTEXTFORMAT_ISTABLEFORMAT )
{
   hb_retl( hbqt_par_QTextFormat( 1 )->isTableFormat() );
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QTEXTFORMAT_ISVALID )
{
   hb_retl( hbqt_par_QTextFormat( 1 )->isValid() );
}

/*
 * Qt::LayoutDirection layoutDirection () const
 */
HB_FUNC( QT_QTEXTFORMAT_LAYOUTDIRECTION )
{
   hb_retni( ( Qt::LayoutDirection ) hbqt_par_QTextFormat( 1 )->layoutDirection() );
}

/*
 * QTextLength lengthProperty ( int propertyId ) const
 */
HB_FUNC( QT_QTEXTFORMAT_LENGTHPROPERTY )
{
   hb_retptrGC( hbqt_gcAllocate_QTextLength( new QTextLength( hbqt_par_QTextFormat( 1 )->lengthProperty( hb_parni( 2 ) ) ), true ) );
}

/*
 * void merge ( const QTextFormat & other )
 */
HB_FUNC( QT_QTEXTFORMAT_MERGE )
{
   hbqt_par_QTextFormat( 1 )->merge( *hbqt_par_QTextFormat( 2 ) );
}

/*
 * int objectIndex () const
 */
HB_FUNC( QT_QTEXTFORMAT_OBJECTINDEX )
{
   hb_retni( hbqt_par_QTextFormat( 1 )->objectIndex() );
}

/*
 * int objectType () const
 */
HB_FUNC( QT_QTEXTFORMAT_OBJECTTYPE )
{
   hb_retni( hbqt_par_QTextFormat( 1 )->objectType() );
}

/*
 * QPen penProperty ( int propertyId ) const
 */
HB_FUNC( QT_QTEXTFORMAT_PENPROPERTY )
{
   hb_retptrGC( hbqt_gcAllocate_QPen( new QPen( hbqt_par_QTextFormat( 1 )->penProperty( hb_parni( 2 ) ) ), true ) );
}

/*
 * QVariant property ( int propertyId ) const
 */
HB_FUNC( QT_QTEXTFORMAT_PROPERTY )
{
   hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( hbqt_par_QTextFormat( 1 )->property( hb_parni( 2 ) ) ), true ) );
}

/*
 * int propertyCount () const
 */
HB_FUNC( QT_QTEXTFORMAT_PROPERTYCOUNT )
{
   hb_retni( hbqt_par_QTextFormat( 1 )->propertyCount() );
}

/*
 * void setBackground ( const QBrush & brush )
 */
HB_FUNC( QT_QTEXTFORMAT_SETBACKGROUND )
{
   hbqt_par_QTextFormat( 1 )->setBackground( *hbqt_par_QBrush( 2 ) );
}

/*
 * void setForeground ( const QBrush & brush )
 */
HB_FUNC( QT_QTEXTFORMAT_SETFOREGROUND )
{
   hbqt_par_QTextFormat( 1 )->setForeground( *hbqt_par_QBrush( 2 ) );
}

/*
 * void setLayoutDirection ( Qt::LayoutDirection direction )
 */
HB_FUNC( QT_QTEXTFORMAT_SETLAYOUTDIRECTION )
{
   hbqt_par_QTextFormat( 1 )->setLayoutDirection( ( Qt::LayoutDirection ) hb_parni( 2 ) );
}

/*
 * void setObjectIndex ( int index )
 */
HB_FUNC( QT_QTEXTFORMAT_SETOBJECTINDEX )
{
   hbqt_par_QTextFormat( 1 )->setObjectIndex( hb_parni( 2 ) );
}

/*
 * void setObjectType ( int type )
 */
HB_FUNC( QT_QTEXTFORMAT_SETOBJECTTYPE )
{
   hbqt_par_QTextFormat( 1 )->setObjectType( hb_parni( 2 ) );
}

/*
 * void setProperty ( int propertyId, const QVariant & value )
 */
HB_FUNC( QT_QTEXTFORMAT_SETPROPERTY )
{
   hbqt_par_QTextFormat( 1 )->setProperty( hb_parni( 2 ), *hbqt_par_QVariant( 3 ) );
}

/*
 * QString stringProperty ( int propertyId ) const
 */
HB_FUNC( QT_QTEXTFORMAT_STRINGPROPERTY )
{
   hb_retc( hbqt_par_QTextFormat( 1 )->stringProperty( hb_parni( 2 ) ).toAscii().data() );
}

/*
 * QTextBlockFormat toBlockFormat () const
 */
HB_FUNC( QT_QTEXTFORMAT_TOBLOCKFORMAT )
{
   hb_retptrGC( hbqt_gcAllocate_QTextBlockFormat( new QTextBlockFormat( hbqt_par_QTextFormat( 1 )->toBlockFormat() ), true ) );
}

/*
 * QTextCharFormat toCharFormat () const
 */
HB_FUNC( QT_QTEXTFORMAT_TOCHARFORMAT )
{
   hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( hbqt_par_QTextFormat( 1 )->toCharFormat() ), true ) );
}

/*
 * QTextFrameFormat toFrameFormat () const
 */
HB_FUNC( QT_QTEXTFORMAT_TOFRAMEFORMAT )
{
   hb_retptrGC( hbqt_gcAllocate_QTextFrameFormat( new QTextFrameFormat( hbqt_par_QTextFormat( 1 )->toFrameFormat() ), true ) );
}

/*
 * QTextImageFormat toImageFormat () const
 */
HB_FUNC( QT_QTEXTFORMAT_TOIMAGEFORMAT )
{
   hb_retptrGC( hbqt_gcAllocate_QTextImageFormat( new QTextImageFormat( hbqt_par_QTextFormat( 1 )->toImageFormat() ), true ) );
}

/*
 * QTextListFormat toListFormat () const
 */
HB_FUNC( QT_QTEXTFORMAT_TOLISTFORMAT )
{
   hb_retptrGC( hbqt_gcAllocate_QTextListFormat( new QTextListFormat( hbqt_par_QTextFormat( 1 )->toListFormat() ), true ) );
}

/*
 * QTextTableFormat toTableFormat () const
 */
HB_FUNC( QT_QTEXTFORMAT_TOTABLEFORMAT )
{
   hb_retptrGC( hbqt_gcAllocate_QTextTableFormat( new QTextTableFormat( hbqt_par_QTextFormat( 1 )->toTableFormat() ), true ) );
}

/*
 * int type () const
 */
HB_FUNC( QT_QTEXTFORMAT_TYPE )
{
   hb_retni( hbqt_par_QTextFormat( 1 )->type() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
