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

#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum Type { Invalid, BitArray, Bitmap, Bool, ..., UserType }
 */

/*
 *  Constructed[ 38/42 [ 90.48% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QHash<QString, QVariant> toHash () const
 *  QMap<QString, QVariant> toMap () const
 *
 *  *** Commented out protos which construct fine but do not compile ***
 *
 *  //const char * typeName () const
 *  //T value () const
 */

#include <QtCore/QPointer>

#include <QtCore/QLine>
#include <QtCore/QRect>
#include <QtCore/QStringList>
#include <QtCore/QVariant>
#include <QtCore/QBitArray>
#include <QtCore/QTime>
#include <QtCore/QUrl>
#include <QtCore/QLocale>


/*
QVariant ()
QVariant ( Qt::GlobalColor color )
QVariant ( Type type )
QVariant ( int typeOrUserType, const void * copy )
QVariant ( const QVariant & p )
QVariant ( QDataStream & s )
QVariant ( int val )
QVariant ( uint val )
QVariant ( qlonglong val )
QVariant ( qulonglong val )
QVariant ( bool val )
QVariant ( double val )
QVariant ( const char * val )
QVariant ( const QByteArray & val )
QVariant ( const QBitArray & val )
QVariant ( const QString & val )
QVariant ( const QLatin1String & val )
QVariant ( const QStringList & val )
QVariant ( const QChar & c )
QVariant ( const QDate & val )
QVariant ( const QTime & val )
QVariant ( const QDateTime & val )
QVariant ( const QList<QVariant> & val )
QVariant ( const QMap<QString, QVariant> & val )
QVariant ( const QHash<QString, QVariant> & val )
QVariant ( const QSize & val )
QVariant ( const QSizeF & val )
QVariant ( const QPoint & val )
QVariant ( const QPointF & val )
QVariant ( const QLine & val )
QVariant ( const QLineF & val )
QVariant ( const QRect & val )
QVariant ( const QRectF & val )
QVariant ( const QUrl & val )
QVariant ( const QLocale & l )
QVariant ( const QRegExp & regExp )
~QVariant ()
 */

typedef struct
{
   QVariant * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QVariant;

QT_G_FUNC( hbqt_gcRelease_QVariant )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QVariant   /.\\", p->ph ) );
         delete ( ( QVariant * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QVariant   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QVariant    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QVariant    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QVariant( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QVariant * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QVariant;
   p->type = HBQT_TYPE_QVariant;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QVariant", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QVariant", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QVARIANT )
{
   QVariant * pObj = NULL;

   pObj =  new QVariant() ;

   hb_retptrGC( hbqt_gcAllocate_QVariant( ( void * ) pObj, true ) );
}

/*
 * bool canConvert ( Type t ) const
 */
HB_FUNC( QT_QVARIANT_CANCONVERT )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
      hb_retl( ( p )->canConvert( ( QVariant::Type ) hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_CANCONVERT FP=hb_retl( ( p )->canConvert( ( QVariant::Type ) hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool canConvert ( Type t ) const
 */
HB_FUNC( QT_QVARIANT_CANCONVERT_1 )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
      hb_retl( ( p )->canConvert( ( QVariant::Type ) hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_CANCONVERT_1 FP=hb_retl( ( p )->canConvert( ( QVariant::Type ) hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void clear ()
 */
HB_FUNC( QT_QVARIANT_CLEAR )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
      ( p )->clear();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_CLEAR FP=( p )->clear(); p is NULL" ) );
   }
}

/*
 * bool convert ( Type t )
 */
HB_FUNC( QT_QVARIANT_CONVERT )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
      hb_retl( ( p )->convert( ( QVariant::Type ) hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_CONVERT FP=hb_retl( ( p )->convert( ( QVariant::Type ) hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QVARIANT_ISNULL )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
      hb_retl( ( p )->isNull() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_ISNULL FP=hb_retl( ( p )->isNull() ); p is NULL" ) );
   }
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QVARIANT_ISVALID )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_ISVALID FP=hb_retl( ( p )->isValid() ); p is NULL" ) );
   }
}

/*
 * void setValue ( const T & value )
 */
HB_FUNC( QT_QVARIANT_SETVALUE )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
      ( p )->setValue( hb_param( 2, HB_IT_ANY ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_SETVALUE FP=( p )->setValue( hb_param( 2, HB_IT_ANY ) ); p is NULL" ) );
   }
}

/*
 * QBitArray toBitArray () const
 */
HB_FUNC( QT_QVARIANT_TOBITARRAY )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QBitArray( new QBitArray( ( p )->toBitArray() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_TOBITARRAY FP=hb_retptrGC( hbqt_gcAllocate_QBitArray( new QBitArray( ( p )->toBitArray() ), true ) ); p is NULL" ) );
   }
}

/*
 * bool toBool () const
 */
HB_FUNC( QT_QVARIANT_TOBOOL )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
      hb_retl( ( p )->toBool() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_TOBOOL FP=hb_retl( ( p )->toBool() ); p is NULL" ) );
   }
}

/*
 * QByteArray toByteArray () const
 */
HB_FUNC( QT_QVARIANT_TOBYTEARRAY )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->toByteArray() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_TOBYTEARRAY FP=hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->toByteArray() ), true ) ); p is NULL" ) );
   }
}

/*
 * QChar toChar () const
 */
HB_FUNC( QT_QVARIANT_TOCHAR )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->toChar() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_TOCHAR FP=hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->toChar() ), true ) ); p is NULL" ) );
   }
}

/*
 * QDate toDate () const
 */
HB_FUNC( QT_QVARIANT_TODATE )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->toDate() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_TODATE FP=hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->toDate() ), true ) ); p is NULL" ) );
   }
}

/*
 * QDateTime toDateTime () const
 */
HB_FUNC( QT_QVARIANT_TODATETIME )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->toDateTime() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_TODATETIME FP=hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->toDateTime() ), true ) ); p is NULL" ) );
   }
}

/*
 * double toDouble ( bool * ok = 0 ) const
 */
HB_FUNC( QT_QVARIANT_TODOUBLE )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   bool iOk = 0;

   if( p )
      hb_retnd( ( p )->toDouble( &iOk ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_TODOUBLE FP=hb_retnd( ( p )->toDouble( &iOk ) ); p is NULL" ) );
   }

   hb_stornl( iOk, 2 );
}

/*
 * int toInt ( bool * ok = 0 ) const
 */
HB_FUNC( QT_QVARIANT_TOINT )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   bool iOk = 0;

   if( p )
      hb_retni( ( p )->toInt( &iOk ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_TOINT FP=hb_retni( ( p )->toInt( &iOk ) ); p is NULL" ) );
   }

   hb_stornl( iOk, 2 );
}

/*
 * QLine toLine () const
 */
HB_FUNC( QT_QVARIANT_TOLINE )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QLine( new QLine( ( p )->toLine() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_TOLINE FP=hb_retptrGC( hbqt_gcAllocate_QLine( new QLine( ( p )->toLine() ), true ) ); p is NULL" ) );
   }
}

/*
 * QLineF toLineF () const
 */
HB_FUNC( QT_QVARIANT_TOLINEF )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QLineF( new QLineF( ( p )->toLineF() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_TOLINEF FP=hb_retptrGC( hbqt_gcAllocate_QLineF( new QLineF( ( p )->toLineF() ), true ) ); p is NULL" ) );
   }
}

/*
 * QList<QVariant> toList () const
 */
HB_FUNC( QT_QVARIANT_TOLIST )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QVariant>( ( p )->toList() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_TOLIST FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QVariant>( ( p )->toList() ), true ) ); p is NULL" ) );
   }
}

/*
 * QLocale toLocale () const
 */
HB_FUNC( QT_QVARIANT_TOLOCALE )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QLocale( new QLocale( ( p )->toLocale() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_TOLOCALE FP=hb_retptrGC( hbqt_gcAllocate_QLocale( new QLocale( ( p )->toLocale() ), true ) ); p is NULL" ) );
   }
}

/*
 * qlonglong toLongLong ( bool * ok = 0 ) const
 */
HB_FUNC( QT_QVARIANT_TOLONGLONG )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   bool iOk = 0;

   if( p )
      hb_retnint( ( p )->toLongLong( &iOk ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_TOLONGLONG FP=hb_retnint( ( p )->toLongLong( &iOk ) ); p is NULL" ) );
   }

   hb_stornl( iOk, 2 );
}

/*
 * QPoint toPoint () const
 */
HB_FUNC( QT_QVARIANT_TOPOINT )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->toPoint() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_TOPOINT FP=hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->toPoint() ), true ) ); p is NULL" ) );
   }
}

/*
 * QPointF toPointF () const
 */
HB_FUNC( QT_QVARIANT_TOPOINTF )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->toPointF() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_TOPOINTF FP=hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->toPointF() ), true ) ); p is NULL" ) );
   }
}

/*
 * QRect toRect () const
 */
HB_FUNC( QT_QVARIANT_TORECT )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->toRect() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_TORECT FP=hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->toRect() ), true ) ); p is NULL" ) );
   }
}

/*
 * QRectF toRectF () const
 */
HB_FUNC( QT_QVARIANT_TORECTF )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->toRectF() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_TORECTF FP=hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->toRectF() ), true ) ); p is NULL" ) );
   }
}

/*
 * QRegExp toRegExp () const
 */
HB_FUNC( QT_QVARIANT_TOREGEXP )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QRegExp( new QRegExp( ( p )->toRegExp() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_TOREGEXP FP=hb_retptrGC( hbqt_gcAllocate_QRegExp( new QRegExp( ( p )->toRegExp() ), true ) ); p is NULL" ) );
   }
}

/*
 * QSize toSize () const
 */
HB_FUNC( QT_QVARIANT_TOSIZE )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->toSize() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_TOSIZE FP=hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->toSize() ), true ) ); p is NULL" ) );
   }
}

/*
 * QSizeF toSizeF () const
 */
HB_FUNC( QT_QVARIANT_TOSIZEF )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->toSizeF() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_TOSIZEF FP=hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->toSizeF() ), true ) ); p is NULL" ) );
   }
}

/*
 * QString toString () const
 */
HB_FUNC( QT_QVARIANT_TOSTRING )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
      hb_retc( ( p )->toString().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_TOSTRING FP=hb_retc( ( p )->toString().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QStringList toStringList () const
 */
HB_FUNC( QT_QVARIANT_TOSTRINGLIST )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->toStringList() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_TOSTRINGLIST FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->toStringList() ), true ) ); p is NULL" ) );
   }
}

/*
 * QTime toTime () const
 */
HB_FUNC( QT_QVARIANT_TOTIME )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->toTime() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_TOTIME FP=hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->toTime() ), true ) ); p is NULL" ) );
   }
}

/*
 * uint toUInt ( bool * ok = 0 ) const
 */
HB_FUNC( QT_QVARIANT_TOUINT )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   bool iOk = 0;

   if( p )
      hb_retni( ( p )->toUInt( &iOk ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_TOUINT FP=hb_retni( ( p )->toUInt( &iOk ) ); p is NULL" ) );
   }

   hb_stornl( iOk, 2 );
}

/*
 * qulonglong toULongLong ( bool * ok = 0 ) const
 */
HB_FUNC( QT_QVARIANT_TOULONGLONG )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   bool iOk = 0;

   if( p )
      hb_retnint( ( p )->toULongLong( &iOk ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_TOULONGLONG FP=hb_retnint( ( p )->toULongLong( &iOk ) ); p is NULL" ) );
   }

   hb_stornl( iOk, 2 );
}

/*
 * QUrl toUrl () const
 */
HB_FUNC( QT_QVARIANT_TOURL )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->toUrl() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_TOURL FP=hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->toUrl() ), true ) ); p is NULL" ) );
   }
}

/*
 * Type type () const
 */
HB_FUNC( QT_QVARIANT_TYPE )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
      hb_retni( ( QVariant::Type ) ( p )->type() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_TYPE FP=hb_retni( ( QVariant::Type ) ( p )->type() ); p is NULL" ) );
   }
}

/*
 * int userType () const
 */
HB_FUNC( QT_QVARIANT_USERTYPE )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
      hb_retni( ( p )->userType() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_USERTYPE FP=hb_retni( ( p )->userType() ); p is NULL" ) );
   }
}

/*
 * QVariant fromValue ( const T & value )
 */
HB_FUNC( QT_QVARIANT_FROMVALUE )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->fromValue( hb_param( 2, HB_IT_ANY ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_FROMVALUE FP=hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->fromValue( hb_param( 2, HB_IT_ANY ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * Type nameToType ( const char * name )
 */
HB_FUNC( QT_QVARIANT_NAMETOTYPE )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
      hb_retni( ( QVariant::Type ) ( p )->nameToType( hbqt_par_char( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_NAMETOTYPE FP=hb_retni( ( QVariant::Type ) ( p )->nameToType( hbqt_par_char( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * const char * typeToName ( Type typ )
 */
HB_FUNC( QT_QVARIANT_TYPETONAME )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
      hb_retc( ( p )->typeToName( ( QVariant::Type ) hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QVARIANT_TYPETONAME FP=hb_retc( ( p )->typeToName( ( QVariant::Type ) hb_parni( 2 ) ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
