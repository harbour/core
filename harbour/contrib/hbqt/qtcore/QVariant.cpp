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
 *  enum Type { Invalid, BitArray, Bitmap, Bool, ..., UserType }
 */

/*
 *  Constructed[ 36/42 [ 85.71% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QHash<QString, QVariant> toHash () const
 *  QList<QVariant> toList () const
 *  QMap<QString, QVariant> toMap () const
 *
 *  *** Commented out protos which construct fine but do not compile ***
 *
 *  //QChar toChar () const
 *  //const char * typeName () const
 *  //T value () const
 */

#include <QtCore/QPointer>

#include <QLine>
#include <QRect>
#include <QStringList>
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
  void * ph;
  bool bNew;
  QT_G_FUNC_PTR func;
} QGC_POINTER_QVariant;

QT_G_FUNC( hbqt_gcRelease_QVariant )
{
      QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QVariant * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QVariant                   ph=%p %i B %i KB", p->ph, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_QVariant                    Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_QVariant                    Object not created with - new" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QVariant( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QVariant;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QVariant                   ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QVARIANT )
{
   void * pObj = NULL;

   pObj = ( QVariant* ) new QVariant() ;

   hb_retptrGC( hbqt_gcAllocate_QVariant( pObj, true ) );
}
/*
 * bool canConvert ( Type t ) const
 */
HB_FUNC( QT_QVARIANT_CANCONVERT )
{
   hb_retl( hbqt_par_QVariant( 1 )->canConvert( ( QVariant::Type ) hb_parni( 2 ) ) );
}

/*
 * bool canConvert ( Type t ) const
 */
HB_FUNC( QT_QVARIANT_CANCONVERT_1 )
{
   hb_retl( hbqt_par_QVariant( 1 )->canConvert( ( QVariant::Type ) hb_parni( 2 ) ) );
}

/*
 * void clear ()
 */
HB_FUNC( QT_QVARIANT_CLEAR )
{
   hbqt_par_QVariant( 1 )->clear();
}

/*
 * bool convert ( Type t )
 */
HB_FUNC( QT_QVARIANT_CONVERT )
{
   hb_retl( hbqt_par_QVariant( 1 )->convert( ( QVariant::Type ) hb_parni( 2 ) ) );
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QVARIANT_ISNULL )
{
   hb_retl( hbqt_par_QVariant( 1 )->isNull() );
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QVARIANT_ISVALID )
{
   hb_retl( hbqt_par_QVariant( 1 )->isValid() );
}

/*
 * void setValue ( const T & value )
 */
HB_FUNC( QT_QVARIANT_SETVALUE )
{
   hbqt_par_QVariant( 1 )->setValue( hb_param( 2, HB_IT_ANY ) );
}

/*
 * QBitArray toBitArray () const
 */
HB_FUNC( QT_QVARIANT_TOBITARRAY )
{
   hb_retptrGC( hbqt_gcAllocate_QBitArray( new QBitArray( hbqt_par_QVariant( 1 )->toBitArray() ), true ) );
}

/*
 * bool toBool () const
 */
HB_FUNC( QT_QVARIANT_TOBOOL )
{
   hb_retl( hbqt_par_QVariant( 1 )->toBool() );
}

/*
 * QByteArray toByteArray () const
 */
HB_FUNC( QT_QVARIANT_TOBYTEARRAY )
{
   hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( hbqt_par_QVariant( 1 )->toByteArray() ), true ) );
}

/*
 * QDate toDate () const
 */
HB_FUNC( QT_QVARIANT_TODATE )
{
   hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( hbqt_par_QVariant( 1 )->toDate() ), true ) );
}

/*
 * QDateTime toDateTime () const
 */
HB_FUNC( QT_QVARIANT_TODATETIME )
{
   hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( hbqt_par_QVariant( 1 )->toDateTime() ), true ) );
}

/*
 * double toDouble ( bool * ok = 0 ) const
 */
HB_FUNC( QT_QVARIANT_TODOUBLE )
{
   bool iOk = 0;

   hb_retnd( hbqt_par_QVariant( 1 )->toDouble( &iOk ) );

   hb_stornl( iOk, 2 );
}

/*
 * int toInt ( bool * ok = 0 ) const
 */
HB_FUNC( QT_QVARIANT_TOINT )
{
   bool iOk = 0;

   hb_retni( hbqt_par_QVariant( 1 )->toInt( &iOk ) );

   hb_stornl( iOk, 2 );
}

/*
 * QLine toLine () const
 */
HB_FUNC( QT_QVARIANT_TOLINE )
{
   hb_retptrGC( hbqt_gcAllocate_QLine( new QLine( hbqt_par_QVariant( 1 )->toLine() ), true ) );
}

/*
 * QLineF toLineF () const
 */
HB_FUNC( QT_QVARIANT_TOLINEF )
{
   hb_retptrGC( hbqt_gcAllocate_QLineF( new QLineF( hbqt_par_QVariant( 1 )->toLineF() ), true ) );
}

/*
 * QLocale toLocale () const
 */
HB_FUNC( QT_QVARIANT_TOLOCALE )
{
   hb_retptrGC( hbqt_gcAllocate_QLocale( new QLocale( hbqt_par_QVariant( 1 )->toLocale() ), true ) );
}

/*
 * qlonglong toLongLong ( bool * ok = 0 ) const
 */
HB_FUNC( QT_QVARIANT_TOLONGLONG )
{
   bool iOk = 0;

   hb_retnint( hbqt_par_QVariant( 1 )->toLongLong( &iOk ) );

   hb_stornl( iOk, 2 );
}

/*
 * QPoint toPoint () const
 */
HB_FUNC( QT_QVARIANT_TOPOINT )
{
   hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( hbqt_par_QVariant( 1 )->toPoint() ), true ) );
}

/*
 * QPointF toPointF () const
 */
HB_FUNC( QT_QVARIANT_TOPOINTF )
{
   hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( hbqt_par_QVariant( 1 )->toPointF() ), true ) );
}

/*
 * QRect toRect () const
 */
HB_FUNC( QT_QVARIANT_TORECT )
{
   hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( hbqt_par_QVariant( 1 )->toRect() ), true ) );
}

/*
 * QRectF toRectF () const
 */
HB_FUNC( QT_QVARIANT_TORECTF )
{
   hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( hbqt_par_QVariant( 1 )->toRectF() ), true ) );
}

/*
 * QRegExp toRegExp () const
 */
HB_FUNC( QT_QVARIANT_TOREGEXP )
{
   hb_retptrGC( hbqt_gcAllocate_QRegExp( new QRegExp( hbqt_par_QVariant( 1 )->toRegExp() ), true ) );
}

/*
 * QSize toSize () const
 */
HB_FUNC( QT_QVARIANT_TOSIZE )
{
   hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( hbqt_par_QVariant( 1 )->toSize() ), true ) );
}

/*
 * QSizeF toSizeF () const
 */
HB_FUNC( QT_QVARIANT_TOSIZEF )
{
   hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( hbqt_par_QVariant( 1 )->toSizeF() ), true ) );
}

/*
 * QString toString () const
 */
HB_FUNC( QT_QVARIANT_TOSTRING )
{
   hb_retc( hbqt_par_QVariant( 1 )->toString().toAscii().data() );
}

/*
 * QStringList toStringList () const
 */
HB_FUNC( QT_QVARIANT_TOSTRINGLIST )
{
   hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( hbqt_par_QVariant( 1 )->toStringList() ), true ) );
}

/*
 * QTime toTime () const
 */
HB_FUNC( QT_QVARIANT_TOTIME )
{
   hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( hbqt_par_QVariant( 1 )->toTime() ), true ) );
}

/*
 * uint toUInt ( bool * ok = 0 ) const
 */
HB_FUNC( QT_QVARIANT_TOUINT )
{
   bool iOk = 0;

   hb_retni( hbqt_par_QVariant( 1 )->toUInt( &iOk ) );

   hb_stornl( iOk, 2 );
}

/*
 * qulonglong toULongLong ( bool * ok = 0 ) const
 */
HB_FUNC( QT_QVARIANT_TOULONGLONG )
{
   bool iOk = 0;

   hb_retnint( hbqt_par_QVariant( 1 )->toULongLong( &iOk ) );

   hb_stornl( iOk, 2 );
}

/*
 * QUrl toUrl () const
 */
HB_FUNC( QT_QVARIANT_TOURL )
{
   hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( hbqt_par_QVariant( 1 )->toUrl() ), true ) );
}

/*
 * Type type () const
 */
HB_FUNC( QT_QVARIANT_TYPE )
{
   hb_retni( ( QVariant::Type ) hbqt_par_QVariant( 1 )->type() );
}

/*
 * int userType () const
 */
HB_FUNC( QT_QVARIANT_USERTYPE )
{
   hb_retni( hbqt_par_QVariant( 1 )->userType() );
}

/*
 * QVariant fromValue ( const T & value )
 */
HB_FUNC( QT_QVARIANT_FROMVALUE )
{
   hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( hbqt_par_QVariant( 1 )->fromValue( hb_param( 2, HB_IT_ANY ) ) ), true ) );
}

/*
 * Type nameToType ( const char * name )
 */
HB_FUNC( QT_QVARIANT_NAMETOTYPE )
{
   hb_retni( ( QVariant::Type ) hbqt_par_QVariant( 1 )->nameToType( hbqt_par_char( 2 ) ) );
}

/*
 * const char * typeToName ( Type typ )
 */
HB_FUNC( QT_QVARIANT_TYPETONAME )
{
   hb_retc( hbqt_par_QVariant( 1 )->typeToName( ( QVariant::Type ) hb_parni( 2 ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
