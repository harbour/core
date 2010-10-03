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
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/

#include "hbqtcore.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum Type { Invalid, BitArray, Bitmap, Bool, ..., UserType }
 */

/*
 *  Constructed[ 38/40 [ 95.00% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  QHash<QString, QVariant> toHash () const
 *  QMap<QString, QVariant> toMap () const
 *
 *  *** Commented out protostypes ***
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
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QVariant;

HBQT_GC_FUNC( hbqt_gcRelease_QVariant )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

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
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

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
   {
      hb_retl( ( p )->canConvert( ( QVariant::Type ) hb_parni( 2 ) ) );
   }
}

/*
 * bool canConvert ( Type t ) const
 */
HB_FUNC( QT_QVARIANT_CANCONVERT_1 )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
   {
      hb_retl( ( p )->canConvert( ( QVariant::Type ) hb_parni( 2 ) ) );
   }
}

/*
 * void clear ()
 */
HB_FUNC( QT_QVARIANT_CLEAR )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
   {
      ( p )->clear();
   }
}

/*
 * bool convert ( Type t )
 */
HB_FUNC( QT_QVARIANT_CONVERT )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
   {
      hb_retl( ( p )->convert( ( QVariant::Type ) hb_parni( 2 ) ) );
   }
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QVARIANT_ISNULL )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
   {
      hb_retl( ( p )->isNull() );
   }
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QVARIANT_ISVALID )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
   {
      hb_retl( ( p )->isValid() );
   }
}

/*
 * void setValue ( const T & value )
 */
HB_FUNC( QT_QVARIANT_SETVALUE )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
   {
      ( p )->setValue( hb_param( 2, HB_IT_ANY ) );
   }
}

/*
 * QBitArray toBitArray () const
 */
HB_FUNC( QT_QVARIANT_TOBITARRAY )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QBitArray( new QBitArray( ( p )->toBitArray() ), true ) );
   }
}

/*
 * bool toBool () const
 */
HB_FUNC( QT_QVARIANT_TOBOOL )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
   {
      hb_retl( ( p )->toBool() );
   }
}

/*
 * QByteArray toByteArray () const
 */
HB_FUNC( QT_QVARIANT_TOBYTEARRAY )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->toByteArray() ), true ) );
   }
}

/*
 * QChar toChar () const
 */
HB_FUNC( QT_QVARIANT_TOCHAR )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->toChar() ), true ) );
   }
}

/*
 * QDate toDate () const
 */
HB_FUNC( QT_QVARIANT_TODATE )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QDate( new QDate( ( p )->toDate() ), true ) );
   }
}

/*
 * QDateTime toDateTime () const
 */
HB_FUNC( QT_QVARIANT_TODATETIME )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QDateTime( new QDateTime( ( p )->toDateTime() ), true ) );
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
   {
      hb_retnd( ( p )->toDouble( &iOk ) );
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
   {
      hb_retni( ( p )->toInt( &iOk ) );
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
   {
      hb_retptrGC( hbqt_gcAllocate_QLine( new QLine( ( p )->toLine() ), true ) );
   }
}

/*
 * QLineF toLineF () const
 */
HB_FUNC( QT_QVARIANT_TOLINEF )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QLineF( new QLineF( ( p )->toLineF() ), true ) );
   }
}

/*
 * QList<QVariant> toList () const
 */
HB_FUNC( QT_QVARIANT_TOLIST )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QVariant>( ( p )->toList() ), true ) );
   }
}

/*
 * QLocale toLocale () const
 */
HB_FUNC( QT_QVARIANT_TOLOCALE )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QLocale( new QLocale( ( p )->toLocale() ), true ) );
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
   {
      hb_retnint( ( p )->toLongLong( &iOk ) );
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
   {
      hb_retptrGC( hbqt_gcAllocate_QPoint( new QPoint( ( p )->toPoint() ), true ) );
   }
}

/*
 * QPointF toPointF () const
 */
HB_FUNC( QT_QVARIANT_TOPOINTF )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QPointF( new QPointF( ( p )->toPointF() ), true ) );
   }
}

/*
 * QRect toRect () const
 */
HB_FUNC( QT_QVARIANT_TORECT )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->toRect() ), true ) );
   }
}

/*
 * QRectF toRectF () const
 */
HB_FUNC( QT_QVARIANT_TORECTF )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRectF( new QRectF( ( p )->toRectF() ), true ) );
   }
}

/*
 * QRegExp toRegExp () const
 */
HB_FUNC( QT_QVARIANT_TOREGEXP )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRegExp( new QRegExp( ( p )->toRegExp() ), true ) );
   }
}

/*
 * QSize toSize () const
 */
HB_FUNC( QT_QVARIANT_TOSIZE )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QSize( new QSize( ( p )->toSize() ), true ) );
   }
}

/*
 * QSizeF toSizeF () const
 */
HB_FUNC( QT_QVARIANT_TOSIZEF )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QSizeF( new QSizeF( ( p )->toSizeF() ), true ) );
   }
}

/*
 * QString toString () const
 */
HB_FUNC( QT_QVARIANT_TOSTRING )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->toString().toUtf8().data() );
   }
}

/*
 * QStringList toStringList () const
 */
HB_FUNC( QT_QVARIANT_TOSTRINGLIST )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->toStringList() ), true ) );
   }
}

/*
 * QTime toTime () const
 */
HB_FUNC( QT_QVARIANT_TOTIME )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QTime( new QTime( ( p )->toTime() ), true ) );
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
   {
      hb_retni( ( p )->toUInt( &iOk ) );
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
   {
      hb_retnint( ( p )->toULongLong( &iOk ) );
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
   {
      hb_retptrGC( hbqt_gcAllocate_QUrl( new QUrl( ( p )->toUrl() ), true ) );
   }
}

/*
 * Type type () const
 */
HB_FUNC( QT_QVARIANT_TYPE )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
   {
      hb_retni( ( QVariant::Type ) ( p )->type() );
   }
}

/*
 * int userType () const
 */
HB_FUNC( QT_QVARIANT_USERTYPE )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
   {
      hb_retni( ( p )->userType() );
   }
}

/*
 * QVariant fromValue ( const T & value )
 */
HB_FUNC( QT_QVARIANT_FROMVALUE )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->fromValue( hb_param( 2, HB_IT_ANY ) ) ), true ) );
   }
}

/*
 * Type nameToType ( const char * name )
 */
HB_FUNC( QT_QVARIANT_NAMETOTYPE )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
   {
      hb_retni( ( QVariant::Type ) ( p )->nameToType( ( const char * ) hb_parc( 2 ) ) );
   }
}

/*
 * const char * typeToName ( Type typ )
 */
HB_FUNC( QT_QVARIANT_TYPETONAME )
{
   QVariant * p = hbqt_par_QVariant( 1 );
   if( p )
   {
      hb_retc( ( p )->typeToName( ( QVariant::Type ) hb_parni( 2 ) ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
