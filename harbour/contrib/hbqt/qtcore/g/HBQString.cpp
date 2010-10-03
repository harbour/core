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
 *  class Null
 *  typedef ConstIterator
 *  typedef Iterator
 *  enum NormalizationForm { NormalizationForm_D, NormalizationForm_C, NormalizationForm_KD, NormalizationForm_KC }
 *  enum SectionFlag { SectionDefault, SectionSkipEmpty, SectionIncludeLeadingSep, SectionIncludeTrailingSep, SectionCaseInsensitiveSeps }
 *  flags SectionFlags
 *  enum SplitBehavior { KeepEmptyParts, SkipEmptyParts }
 *  typedef const_iterator
 *  typedef iterator
 */

/*
 *  Constructed[ 167/168 [ 99.40% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  QVector<uint> toUcs4 () const
 *
 *  *** Commented out protostypes ***
 *
 *  //iterator begin ()
 *  //const_iterator begin () const
 *  //const_iterator constBegin () const
 *  //const_iterator constEnd () const
 *  //bool contains ( const QString & str, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const
 *  //bool contains ( QChar ch, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const
 *  //bool contains ( const QRegExp & rx ) const
 *  //bool contains ( QRegExp & rx ) const
 *  //iterator end ()
 *  //const_iterator end () const
 *  //QString & sprintf ( const char * cformat, ... )
 *  //std::string toStdString () const
 *  //std::wstring toStdWString () const
 *  //int toWCharArray ( wchar_t * array ) const
 *  //const ushort * utf16 () const
 *  //QString & vsprintf ( const char * cformat, va_list ap )
 *  //QString fromStdString ( const std::string & str )
 *  //QString fromStdWString ( const std::wstring & str )
 *  //QString fromWCharArray ( const wchar_t * string, int size = -1 )
 */

#include <QtCore/QPointer>

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbvm.h"

#include <QtCore/QString>
#include <QtCore/QStringList>
#include <QtCore/QStringRef>

#include "../hbqt_hbqstring.h"

/*
 * HBQString ()
 * HBQString ( const QChar * unicode, int size )
 * HBQString ( QChar ch )
 * HBQString ( int size, QChar ch )
 * HBQString ( const QLatin1String & str )
 * HBQString ( const QString & other )
 * HBQString ( const char * str )
 * HBQString ( const QByteArray & ba )
 * ~HBQString ()
 */

typedef struct
{
   HBQString * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_HBQString;

HBQT_GC_FUNC( hbqt_gcRelease_HBQString )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_HBQString   /.\\", p->ph ) );
         delete ( ( HBQString * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_HBQString   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_HBQString    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_HBQString    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_HBQString( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( HBQString * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_HBQString;
   p->type = HBQT_TYPE_HBQString;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_HBQString", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_HBQString", pObj ) );
   }
   return p;
}

HB_FUNC( QT_HBQSTRING )
{
   HBQString * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISCHAR( 1 ) )
   {
      pObj = new HBQString( hbqt_par_QString( 1 ) ) ;
   }
   else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new HBQString( ( const char * ) hb_parptr( 1 ) ) ;
   }
   else
   {
      pObj = new HBQString() ;
   }

   hb_retptrGC( hbqt_gcAllocate_HBQString( ( void * ) pObj, true ) );
}

/*
 * QString & append ( const QString & str )
 */
HB_FUNC( QT_HBQSTRING_APPEND )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->append( hb_parstr_utf8( 2, &pText, NULL ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/*
 * QString & append ( const QStringRef & reference )
 */
HB_FUNC( QT_HBQSTRING_APPEND_1 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->append( *hbqt_par_QStringRef( 2 ) ).toUtf8().data() );
   }
}

/*
 * QString & append ( const QLatin1String & str )
 */
HB_FUNC( QT_HBQSTRING_APPEND_2 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->append( *hbqt_par_QLatin1String( 2 ) ).toUtf8().data() );
   }
}

/*
 * QString & append ( const QByteArray & ba )
 */
HB_FUNC( QT_HBQSTRING_APPEND_3 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->append( *hbqt_par_QByteArray( 2 ) ).toUtf8().data() );
   }
}

/*
 * QString & append ( const char * str )
 */
HB_FUNC( QT_HBQSTRING_APPEND_4 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->append( ( const char * ) hb_parc( 2 ) ).toUtf8().data() );
   }
}

/*
 * QString & append ( QChar ch )
 */
HB_FUNC( QT_HBQSTRING_APPEND_5 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->append( *hbqt_par_QChar( 2 ) ).toUtf8().data() );
   }
}

/*
 * QString arg ( const QString & a, int fieldWidth = 0, const QChar & fillChar = QLatin1Char( ' ' ) ) const
 */
HB_FUNC( QT_HBQSTRING_ARG )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->arg( hb_parstr_utf8( 2, &pText, NULL ), hb_parni( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QChar( 4 ) : QLatin1Char( ' ' ) ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/*
 * QString arg ( const QString & a1, const QString & a2 ) const
 */
HB_FUNC( QT_HBQSTRING_ARG_1 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->arg( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/*
 * QString arg ( const QString & a1, const QString & a2, const QString & a3 ) const
 */
HB_FUNC( QT_HBQSTRING_ARG_2 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->arg( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ), hb_parstr_utf8( 4, &pText, NULL ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/*
 * QString arg ( const QString & a1, const QString & a2, const QString & a3, const QString & a4 ) const
 */
HB_FUNC( QT_HBQSTRING_ARG_3 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->arg( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ), hb_parstr_utf8( 4, &pText, NULL ), hb_parstr_utf8( 5, &pText, NULL ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/*
 * QString arg ( const QString & a1, const QString & a2, const QString & a3, const QString & a4, const QString & a5 ) const
 */
HB_FUNC( QT_HBQSTRING_ARG_4 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->arg( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ), hb_parstr_utf8( 4, &pText, NULL ), hb_parstr_utf8( 5, &pText, NULL ), hb_parstr_utf8( 6, &pText, NULL ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/*
 * QString arg ( const QString & a1, const QString & a2, const QString & a3, const QString & a4, const QString & a5, const QString & a6 ) const
 */
HB_FUNC( QT_HBQSTRING_ARG_5 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->arg( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ), hb_parstr_utf8( 4, &pText, NULL ), hb_parstr_utf8( 5, &pText, NULL ), hb_parstr_utf8( 6, &pText, NULL ), hb_parstr_utf8( 7, &pText, NULL ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/*
 * QString arg ( const QString & a1, const QString & a2, const QString & a3, const QString & a4, const QString & a5, const QString & a6, const QString & a7 ) const
 */
HB_FUNC( QT_HBQSTRING_ARG_6 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->arg( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ), hb_parstr_utf8( 4, &pText, NULL ), hb_parstr_utf8( 5, &pText, NULL ), hb_parstr_utf8( 6, &pText, NULL ), hb_parstr_utf8( 7, &pText, NULL ), hb_parstr_utf8( 8, &pText, NULL ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/*
 * QString arg ( const QString & a1, const QString & a2, const QString & a3, const QString & a4, const QString & a5, const QString & a6, const QString & a7, const QString & a8 ) const
 */
HB_FUNC( QT_HBQSTRING_ARG_7 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->arg( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ), hb_parstr_utf8( 4, &pText, NULL ), hb_parstr_utf8( 5, &pText, NULL ), hb_parstr_utf8( 6, &pText, NULL ), hb_parstr_utf8( 7, &pText, NULL ), hb_parstr_utf8( 8, &pText, NULL ), hb_parstr_utf8( 9, &pText, NULL ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/*
 * QString arg ( const QString & a1, const QString & a2, const QString & a3, const QString & a4, const QString & a5, const QString & a6, const QString & a7, const QString & a8, const QString & a9 ) const
 */
HB_FUNC( QT_HBQSTRING_ARG_8 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->arg( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ), hb_parstr_utf8( 4, &pText, NULL ), hb_parstr_utf8( 5, &pText, NULL ), hb_parstr_utf8( 6, &pText, NULL ), hb_parstr_utf8( 7, &pText, NULL ), hb_parstr_utf8( 8, &pText, NULL ), hb_parstr_utf8( 9, &pText, NULL ), hb_parstr_utf8( 10, &pText, NULL ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/*
 * QString arg ( int a, int fieldWidth = 0, int base = 10, const QChar & fillChar = QLatin1Char( ' ' ) ) const
 */
HB_FUNC( QT_HBQSTRING_ARG_9 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->arg( hb_parni( 2 ), hb_parni( 3 ), hb_parnidef( 4, 10 ), ( HB_ISPOINTER( 5 ) ? *hbqt_par_QChar( 5 ) : QLatin1Char( ' ' ) ) ).toUtf8().data() );
   }
}

/*
 * QString arg ( uint a, int fieldWidth = 0, int base = 10, const QChar & fillChar = QLatin1Char( ' ' ) ) const
 */
HB_FUNC( QT_HBQSTRING_ARG_10 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->arg( hb_parni( 2 ), hb_parni( 3 ), hb_parnidef( 4, 10 ), ( HB_ISPOINTER( 5 ) ? *hbqt_par_QChar( 5 ) : QLatin1Char( ' ' ) ) ).toUtf8().data() );
   }
}

/*
 * QString arg ( long a, int fieldWidth = 0, int base = 10, const QChar & fillChar = QLatin1Char( ' ' ) ) const
 */
HB_FUNC( QT_HBQSTRING_ARG_11 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->arg( ( long ) hb_parnint( 2 ), hb_parni( 3 ), hb_parnidef( 4, 10 ), ( HB_ISPOINTER( 5 ) ? *hbqt_par_QChar( 5 ) : QLatin1Char( ' ' ) ) ).toUtf8().data() );
   }
}

/*
 * QString arg ( ulong a, int fieldWidth = 0, int base = 10, const QChar & fillChar = QLatin1Char( ' ' ) ) const
 */
HB_FUNC( QT_HBQSTRING_ARG_12 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->arg( ( ulong ) hb_parnint( 2 ), hb_parni( 3 ), hb_parnidef( 4, 10 ), ( HB_ISPOINTER( 5 ) ? *hbqt_par_QChar( 5 ) : QLatin1Char( ' ' ) ) ).toUtf8().data() );
   }
}

/*
 * QString arg ( qlonglong a, int fieldWidth = 0, int base = 10, const QChar & fillChar = QLatin1Char( ' ' ) ) const
 */
HB_FUNC( QT_HBQSTRING_ARG_13 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->arg( ( qlonglong ) hb_parnint( 2 ), hb_parni( 3 ), hb_parnidef( 4, 10 ), ( HB_ISPOINTER( 5 ) ? *hbqt_par_QChar( 5 ) : QLatin1Char( ' ' ) ) ).toUtf8().data() );
   }
}

/*
 * QString arg ( qulonglong a, int fieldWidth = 0, int base = 10, const QChar & fillChar = QLatin1Char( ' ' ) ) const
 */
HB_FUNC( QT_HBQSTRING_ARG_14 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->arg( ( qulonglong ) hb_parnint( 2 ), hb_parni( 3 ), hb_parnidef( 4, 10 ), ( HB_ISPOINTER( 5 ) ? *hbqt_par_QChar( 5 ) : QLatin1Char( ' ' ) ) ).toUtf8().data() );
   }
}

/*
 * QString arg ( short a, int fieldWidth = 0, int base = 10, const QChar & fillChar = QLatin1Char( ' ' ) ) const
 */
HB_FUNC( QT_HBQSTRING_ARG_15 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->arg( hb_parni( 2 ), hb_parni( 3 ), hb_parnidef( 4, 10 ), ( HB_ISPOINTER( 5 ) ? *hbqt_par_QChar( 5 ) : QLatin1Char( ' ' ) ) ).toUtf8().data() );
   }
}

/*
 * QString arg ( ushort a, int fieldWidth = 0, int base = 10, const QChar & fillChar = QLatin1Char( ' ' ) ) const
 */
HB_FUNC( QT_HBQSTRING_ARG_16 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->arg( hb_parni( 2 ), hb_parni( 3 ), hb_parnidef( 4, 10 ), ( HB_ISPOINTER( 5 ) ? *hbqt_par_QChar( 5 ) : QLatin1Char( ' ' ) ) ).toUtf8().data() );
   }
}

/*
 * QString arg ( QChar a, int fieldWidth = 0, const QChar & fillChar = QLatin1Char( ' ' ) ) const
 */
HB_FUNC( QT_HBQSTRING_ARG_17 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->arg( *hbqt_par_QChar( 2 ), hb_parni( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QChar( 4 ) : QLatin1Char( ' ' ) ) ).toUtf8().data() );
   }
}

/*
 * QString arg ( char a, int fieldWidth = 0, const QChar & fillChar = QLatin1Char( ' ' ) ) const
 */
HB_FUNC( QT_HBQSTRING_ARG_18 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->arg( ( char ) hb_parni( 2 ), hb_parni( 3 ), ( HB_ISPOINTER( 4 ) ? *hbqt_par_QChar( 4 ) : QLatin1Char( ' ' ) ) ).toUtf8().data() );
   }
}

/*
 * QString arg ( double a, int fieldWidth = 0, char format = 'g', int precision = -1, const QChar & fillChar = QLatin1Char( ' ' ) ) const
 */
HB_FUNC( QT_HBQSTRING_ARG_19 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->arg( hb_parnd( 2 ), hb_parni( 3 ), ( char ) hb_parni( 4 ), hb_parnidef( 5, -1 ), ( HB_ISPOINTER( 6 ) ? *hbqt_par_QChar( 6 ) : QLatin1Char( ' ' ) ) ).toUtf8().data() );
   }
}

/*
 * const QChar at ( int position ) const
 */
HB_FUNC( QT_HBQSTRING_AT )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( ( p )->at( hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * int capacity () const
 */
HB_FUNC( QT_HBQSTRING_CAPACITY )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retni( ( p )->capacity() );
   }
}

/*
 * void chop ( int n )
 */
HB_FUNC( QT_HBQSTRING_CHOP )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      ( p )->chop( hb_parni( 2 ) );
   }
}

/*
 * void clear ()
 */
HB_FUNC( QT_HBQSTRING_CLEAR )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      ( p )->clear();
   }
}

/*
 * int compare ( const QString & other ) const
 */
HB_FUNC( QT_HBQSTRING_COMPARE )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->compare( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * int compare ( const QString & other, Qt::CaseSensitivity cs ) const
 */
HB_FUNC( QT_HBQSTRING_COMPARE_1 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->compare( hb_parstr_utf8( 2, &pText, NULL ), ( Qt::CaseSensitivity ) hb_parni( 3 ) ) );
      hb_strfree( pText );
   }
}

/*
 * int compare ( const QLatin1String & other, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const
 */
HB_FUNC( QT_HBQSTRING_COMPARE_2 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retni( ( p )->compare( *hbqt_par_QLatin1String( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::CaseSensitivity ) hb_parni( 3 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ) );
   }
}

/*
 * int compare ( const QStringRef & ref, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const
 */
HB_FUNC( QT_HBQSTRING_COMPARE_3 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retni( ( p )->compare( *hbqt_par_QStringRef( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::CaseSensitivity ) hb_parni( 3 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ) );
   }
}

/*
 * const QChar * constData () const
 */
HB_FUNC( QT_HBQSTRING_CONSTDATA )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( *( ( p )->constData() ) ), true ) );
   }
}

/*
 * int count ( const QString & str, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const
 */
HB_FUNC( QT_HBQSTRING_COUNT )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->count( hb_parstr_utf8( 2, &pText, NULL ), ( HB_ISNUM( 3 ) ? ( Qt::CaseSensitivity ) hb_parni( 3 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ) );
      hb_strfree( pText );
   }
}

/*
 * int count ( QChar ch, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const
 */
HB_FUNC( QT_HBQSTRING_COUNT_1 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retni( ( p )->count( *hbqt_par_QChar( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::CaseSensitivity ) hb_parni( 3 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ) );
   }
}

/*
 * int count ( const QRegExp & rx ) const
 */
HB_FUNC( QT_HBQSTRING_COUNT_2 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retni( ( p )->count( *hbqt_par_QRegExp( 2 ) ) );
   }
}

/*
 * int count () const
 */
HB_FUNC( QT_HBQSTRING_COUNT_3 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retni( ( p )->count() );
   }
}

/*
 * QChar * data ()
 */
HB_FUNC( QT_HBQSTRING_DATA )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QChar( ( p )->data(), false ) );
   }
}

/*
 * const QChar * data () const
 */
HB_FUNC( QT_HBQSTRING_DATA_1 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( *( ( p )->data() ) ), true ) );
   }
}

/*
 * bool endsWith ( const QString & s, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const
 */
HB_FUNC( QT_HBQSTRING_ENDSWITH )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->endsWith( hb_parstr_utf8( 2, &pText, NULL ), ( HB_ISNUM( 3 ) ? ( Qt::CaseSensitivity ) hb_parni( 3 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ) );
      hb_strfree( pText );
   }
}

/*
 * bool endsWith ( const QLatin1String & s, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const
 */
HB_FUNC( QT_HBQSTRING_ENDSWITH_1 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retl( ( p )->endsWith( *hbqt_par_QLatin1String( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::CaseSensitivity ) hb_parni( 3 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ) );
   }
}

/*
 * bool endsWith ( const QChar & c, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const
 */
HB_FUNC( QT_HBQSTRING_ENDSWITH_2 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retl( ( p )->endsWith( *hbqt_par_QChar( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::CaseSensitivity ) hb_parni( 3 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ) );
   }
}

/*
 * QString & fill ( QChar ch, int size = -1 )
 */
HB_FUNC( QT_HBQSTRING_FILL )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->fill( *hbqt_par_QChar( 2 ), hb_parnidef( 3, -1 ) ).toUtf8().data() );
   }
}

/*
 * int indexOf ( const QString & str, int from = 0, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const
 */
HB_FUNC( QT_HBQSTRING_INDEXOF )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->indexOf( hb_parstr_utf8( 2, &pText, NULL ), hb_parni( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::CaseSensitivity ) hb_parni( 4 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ) );
      hb_strfree( pText );
   }
}

/*
 * int indexOf ( const QLatin1String & str, int from = 0, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const
 */
HB_FUNC( QT_HBQSTRING_INDEXOF_1 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retni( ( p )->indexOf( *hbqt_par_QLatin1String( 2 ), hb_parni( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::CaseSensitivity ) hb_parni( 4 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ) );
   }
}

/*
 * int indexOf ( QChar ch, int from = 0, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const
 */
HB_FUNC( QT_HBQSTRING_INDEXOF_2 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retni( ( p )->indexOf( *hbqt_par_QChar( 2 ), hb_parni( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::CaseSensitivity ) hb_parni( 4 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ) );
   }
}

/*
 * int indexOf ( const QRegExp & rx, int from = 0 ) const
 */
HB_FUNC( QT_HBQSTRING_INDEXOF_3 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retni( ( p )->indexOf( *hbqt_par_QRegExp( 2 ), hb_parni( 3 ) ) );
   }
}

/*
 * int indexOf ( QRegExp & rx, int from = 0 ) const
 */
HB_FUNC( QT_HBQSTRING_INDEXOF_4 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retni( ( p )->indexOf( *hbqt_par_QRegExp( 2 ), hb_parni( 3 ) ) );
   }
}

/*
 * QString & insert ( int position, const QString & str )
 */
HB_FUNC( QT_HBQSTRING_INSERT )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->insert( hb_parni( 2 ), hb_parstr_utf8( 3, &pText, NULL ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/*
 * QString & insert ( int position, const QLatin1String & str )
 */
HB_FUNC( QT_HBQSTRING_INSERT_1 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->insert( hb_parni( 2 ), *hbqt_par_QLatin1String( 3 ) ).toUtf8().data() );
   }
}

/*
 * QString & insert ( int position, const QChar * unicode, int size )
 */
HB_FUNC( QT_HBQSTRING_INSERT_2 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->insert( hb_parni( 2 ), hbqt_par_QChar( 3 ), hb_parni( 4 ) ).toUtf8().data() );
   }
}

/*
 * QString & insert ( int position, QChar ch )
 */
HB_FUNC( QT_HBQSTRING_INSERT_3 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->insert( hb_parni( 2 ), *hbqt_par_QChar( 3 ) ).toUtf8().data() );
   }
}

/*
 * bool isEmpty () const
 */
HB_FUNC( QT_HBQSTRING_ISEMPTY )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retl( ( p )->isEmpty() );
   }
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_HBQSTRING_ISNULL )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retl( ( p )->isNull() );
   }
}

/*
 * int lastIndexOf ( const QString & str, int from = -1, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const
 */
HB_FUNC( QT_HBQSTRING_LASTINDEXOF )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->lastIndexOf( hb_parstr_utf8( 2, &pText, NULL ), hb_parnidef( 3, -1 ), ( HB_ISNUM( 4 ) ? ( Qt::CaseSensitivity ) hb_parni( 4 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ) );
      hb_strfree( pText );
   }
}

/*
 * int lastIndexOf ( const QLatin1String & str, int from = -1, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const
 */
HB_FUNC( QT_HBQSTRING_LASTINDEXOF_1 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retni( ( p )->lastIndexOf( *hbqt_par_QLatin1String( 2 ), hb_parnidef( 3, -1 ), ( HB_ISNUM( 4 ) ? ( Qt::CaseSensitivity ) hb_parni( 4 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ) );
   }
}

/*
 * int lastIndexOf ( QChar ch, int from = -1, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const
 */
HB_FUNC( QT_HBQSTRING_LASTINDEXOF_2 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retni( ( p )->lastIndexOf( *hbqt_par_QChar( 2 ), hb_parnidef( 3, -1 ), ( HB_ISNUM( 4 ) ? ( Qt::CaseSensitivity ) hb_parni( 4 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ) );
   }
}

/*
 * int lastIndexOf ( const QRegExp & rx, int from = -1 ) const
 */
HB_FUNC( QT_HBQSTRING_LASTINDEXOF_3 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retni( ( p )->lastIndexOf( *hbqt_par_QRegExp( 2 ), hb_parnidef( 3, -1 ) ) );
   }
}

/*
 * int lastIndexOf ( QRegExp & rx, int from = -1 ) const
 */
HB_FUNC( QT_HBQSTRING_LASTINDEXOF_4 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retni( ( p )->lastIndexOf( *hbqt_par_QRegExp( 2 ), hb_parnidef( 3, -1 ) ) );
   }
}

/*
 * QString left ( int n ) const
 */
HB_FUNC( QT_HBQSTRING_LEFT )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->left( hb_parni( 2 ) ).toUtf8().data() );
   }
}

/*
 * QString leftJustified ( int width, QChar fill = QLatin1Char( ' ' ), bool truncate = false ) const
 */
HB_FUNC( QT_HBQSTRING_LEFTJUSTIFIED )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->leftJustified( hb_parni( 2 ), *hbqt_par_QChar( 3 ), hb_parl( 4 ) ).toUtf8().data() );
   }
}

/*
 * QStringRef leftRef ( int n ) const
 */
HB_FUNC( QT_HBQSTRING_LEFTREF )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStringRef( new QStringRef( ( p )->leftRef( hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * int length () const
 */
HB_FUNC( QT_HBQSTRING_LENGTH )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retni( ( p )->length() );
   }
}

/*
 * int localeAwareCompare ( const QStringRef & other ) const
 */
HB_FUNC( QT_HBQSTRING_LOCALEAWARECOMPARE )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retni( ( p )->localeAwareCompare( *hbqt_par_QStringRef( 2 ) ) );
   }
}

/*
 * int localeAwareCompare ( const QString & other ) const
 */
HB_FUNC( QT_HBQSTRING_LOCALEAWARECOMPARE_1 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->localeAwareCompare( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * QString mid ( int position, int n = -1 ) const
 */
HB_FUNC( QT_HBQSTRING_MID )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->mid( hb_parni( 2 ), hb_parnidef( 3, -1 ) ).toUtf8().data() );
   }
}

/*
 * QStringRef midRef ( int position, int n = -1 ) const
 */
HB_FUNC( QT_HBQSTRING_MIDREF )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStringRef( new QStringRef( ( p )->midRef( hb_parni( 2 ), hb_parnidef( 3, -1 ) ) ), true ) );
   }
}

/*
 * QString normalized ( NormalizationForm mode ) const
 */
HB_FUNC( QT_HBQSTRING_NORMALIZED )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->normalized( ( HBQString::NormalizationForm ) hb_parni( 2 ) ).toUtf8().data() );
   }
}

/*
 * QString normalized ( NormalizationForm mode, QChar::UnicodeVersion version ) const
 */
HB_FUNC( QT_HBQSTRING_NORMALIZED_1 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->normalized( ( HBQString::NormalizationForm ) hb_parni( 2 ), ( QChar::UnicodeVersion ) hb_parni( 3 ) ).toUtf8().data() );
   }
}

/*
 * QString & prepend ( const QString & str )
 */
HB_FUNC( QT_HBQSTRING_PREPEND )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->prepend( hb_parstr_utf8( 2, &pText, NULL ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/*
 * QString & prepend ( const QLatin1String & str )
 */
HB_FUNC( QT_HBQSTRING_PREPEND_1 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->prepend( *hbqt_par_QLatin1String( 2 ) ).toUtf8().data() );
   }
}

/*
 * QString & prepend ( const QByteArray & ba )
 */
HB_FUNC( QT_HBQSTRING_PREPEND_2 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->prepend( *hbqt_par_QByteArray( 2 ) ).toUtf8().data() );
   }
}

/*
 * QString & prepend ( const char * str )
 */
HB_FUNC( QT_HBQSTRING_PREPEND_3 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->prepend( ( const char * ) hb_parc( 2 ) ).toUtf8().data() );
   }
}

/*
 * QString & prepend ( QChar ch )
 */
HB_FUNC( QT_HBQSTRING_PREPEND_4 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->prepend( *hbqt_par_QChar( 2 ) ).toUtf8().data() );
   }
}

/*
 * void push_back ( const QString & other )
 */
HB_FUNC( QT_HBQSTRING_PUSH_BACK )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      ( p )->push_back( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void push_back ( QChar ch )
 */
HB_FUNC( QT_HBQSTRING_PUSH_BACK_1 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      ( p )->push_back( *hbqt_par_QChar( 2 ) );
   }
}

/*
 * void push_front ( const QString & other )
 */
HB_FUNC( QT_HBQSTRING_PUSH_FRONT )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      ( p )->push_front( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void push_front ( QChar ch )
 */
HB_FUNC( QT_HBQSTRING_PUSH_FRONT_1 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      ( p )->push_front( *hbqt_par_QChar( 2 ) );
   }
}

/*
 * QString & remove ( int position, int n )
 */
HB_FUNC( QT_HBQSTRING_REMOVE )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->remove( hb_parni( 2 ), hb_parni( 3 ) ).toUtf8().data() );
   }
}

/*
 * QString & remove ( QChar ch, Qt::CaseSensitivity cs = Qt::CaseSensitive )
 */
HB_FUNC( QT_HBQSTRING_REMOVE_1 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->remove( *hbqt_par_QChar( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::CaseSensitivity ) hb_parni( 3 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ).toUtf8().data() );
   }
}

/*
 * QString & remove ( const QString & str, Qt::CaseSensitivity cs = Qt::CaseSensitive )
 */
HB_FUNC( QT_HBQSTRING_REMOVE_2 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->remove( hb_parstr_utf8( 2, &pText, NULL ), ( HB_ISNUM( 3 ) ? ( Qt::CaseSensitivity ) hb_parni( 3 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/*
 * QString & remove ( const QRegExp & rx )
 */
HB_FUNC( QT_HBQSTRING_REMOVE_3 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->remove( *hbqt_par_QRegExp( 2 ) ).toUtf8().data() );
   }
}

/*
 * QString repeated ( int times ) const
 */
HB_FUNC( QT_HBQSTRING_REPEATED )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->repeated( hb_parni( 2 ) ).toUtf8().data() );
   }
}

/*
 * QString & replace ( int position, int n, const QString & after )
 */
HB_FUNC( QT_HBQSTRING_REPLACE )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->replace( hb_parni( 2 ), hb_parni( 3 ), hb_parstr_utf8( 4, &pText, NULL ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/*
 * QString & replace ( int position, int n, const QChar * unicode, int size )
 */
HB_FUNC( QT_HBQSTRING_REPLACE_1 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->replace( hb_parni( 2 ), hb_parni( 3 ), hbqt_par_QChar( 4 ), hb_parni( 5 ) ).toUtf8().data() );
   }
}

/*
 * QString & replace ( int position, int n, QChar after )
 */
HB_FUNC( QT_HBQSTRING_REPLACE_2 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->replace( hb_parni( 2 ), hb_parni( 3 ), *hbqt_par_QChar( 4 ) ).toUtf8().data() );
   }
}

/*
 * QString & replace ( const QString & before, const QString & after, Qt::CaseSensitivity cs = Qt::CaseSensitive )
 */
HB_FUNC( QT_HBQSTRING_REPLACE_3 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->replace( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ), ( HB_ISNUM( 4 ) ? ( Qt::CaseSensitivity ) hb_parni( 4 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/*
 * QString & replace ( const QChar * before, int blen, const QChar * after, int alen, Qt::CaseSensitivity cs = Qt::CaseSensitive )
 */
HB_FUNC( QT_HBQSTRING_REPLACE_4 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->replace( hbqt_par_QChar( 2 ), hb_parni( 3 ), hbqt_par_QChar( 4 ), hb_parni( 5 ), ( HB_ISNUM( 6 ) ? ( Qt::CaseSensitivity ) hb_parni( 6 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ).toUtf8().data() );
   }
}

/*
 * QString & replace ( QChar ch, const QString & after, Qt::CaseSensitivity cs = Qt::CaseSensitive )
 */
HB_FUNC( QT_HBQSTRING_REPLACE_5 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->replace( *hbqt_par_QChar( 2 ), hb_parstr_utf8( 3, &pText, NULL ), ( HB_ISNUM( 4 ) ? ( Qt::CaseSensitivity ) hb_parni( 4 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/*
 * QString & replace ( QChar before, QChar after, Qt::CaseSensitivity cs = Qt::CaseSensitive )
 */
HB_FUNC( QT_HBQSTRING_REPLACE_6 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->replace( *hbqt_par_QChar( 2 ), *hbqt_par_QChar( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::CaseSensitivity ) hb_parni( 4 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ).toUtf8().data() );
   }
}

/*
 * QString & replace ( const QLatin1String & before, const QLatin1String & after, Qt::CaseSensitivity cs = Qt::CaseSensitive )
 */
HB_FUNC( QT_HBQSTRING_REPLACE_7 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->replace( *hbqt_par_QLatin1String( 2 ), *hbqt_par_QLatin1String( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::CaseSensitivity ) hb_parni( 4 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ).toUtf8().data() );
   }
}

/*
 * QString & replace ( const QLatin1String & before, const QString & after, Qt::CaseSensitivity cs = Qt::CaseSensitive )
 */
HB_FUNC( QT_HBQSTRING_REPLACE_8 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->replace( *hbqt_par_QLatin1String( 2 ), hb_parstr_utf8( 3, &pText, NULL ), ( HB_ISNUM( 4 ) ? ( Qt::CaseSensitivity ) hb_parni( 4 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/*
 * QString & replace ( const QString & before, const QLatin1String & after, Qt::CaseSensitivity cs = Qt::CaseSensitive )
 */
HB_FUNC( QT_HBQSTRING_REPLACE_9 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->replace( hb_parstr_utf8( 2, &pText, NULL ), *hbqt_par_QLatin1String( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::CaseSensitivity ) hb_parni( 4 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/*
 * QString & replace ( QChar c, const QLatin1String & after, Qt::CaseSensitivity cs = Qt::CaseSensitive )
 */
HB_FUNC( QT_HBQSTRING_REPLACE_10 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->replace( *hbqt_par_QChar( 2 ), *hbqt_par_QLatin1String( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::CaseSensitivity ) hb_parni( 4 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ).toUtf8().data() );
   }
}

/*
 * QString & replace ( const QRegExp & rx, const QString & after )
 */
HB_FUNC( QT_HBQSTRING_REPLACE_11 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->replace( *hbqt_par_QRegExp( 2 ), hb_parstr_utf8( 3, &pText, NULL ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/*
 * void reserve ( int size )
 */
HB_FUNC( QT_HBQSTRING_RESERVE )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      ( p )->reserve( hb_parni( 2 ) );
   }
}

/*
 * void resize ( int size )
 */
HB_FUNC( QT_HBQSTRING_RESIZE )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      ( p )->resize( hb_parni( 2 ) );
   }
}

/*
 * QString right ( int n ) const
 */
HB_FUNC( QT_HBQSTRING_RIGHT )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->right( hb_parni( 2 ) ).toUtf8().data() );
   }
}

/*
 * QString rightJustified ( int width, QChar fill = QLatin1Char( ' ' ), bool truncate = false ) const
 */
HB_FUNC( QT_HBQSTRING_RIGHTJUSTIFIED )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->rightJustified( hb_parni( 2 ), *hbqt_par_QChar( 3 ), hb_parl( 4 ) ).toUtf8().data() );
   }
}

/*
 * QStringRef rightRef ( int n ) const
 */
HB_FUNC( QT_HBQSTRING_RIGHTREF )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStringRef( new QStringRef( ( p )->rightRef( hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * QString section ( QChar sep, int start, int end = -1, SectionFlags flags = SectionDefault ) const
 */
HB_FUNC( QT_HBQSTRING_SECTION )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->section( *hbqt_par_QChar( 2 ), hb_parni( 3 ), hb_parnidef( 4, -1 ), ( HB_ISNUM( 5 ) ? ( HBQString::SectionFlags ) hb_parni( 5 ) : ( HBQString::SectionFlags ) HBQString::SectionDefault ) ).toUtf8().data() );
   }
}

/*
 * QString section ( const QString & sep, int start, int end = -1, SectionFlags flags = SectionDefault ) const
 */
HB_FUNC( QT_HBQSTRING_SECTION_1 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->section( hb_parstr_utf8( 2, &pText, NULL ), hb_parni( 3 ), hb_parnidef( 4, -1 ), ( HB_ISNUM( 5 ) ? ( HBQString::SectionFlags ) hb_parni( 5 ) : ( HBQString::SectionFlags ) HBQString::SectionDefault ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/*
 * QString section ( const QRegExp & reg, int start, int end = -1, SectionFlags flags = SectionDefault ) const
 */
HB_FUNC( QT_HBQSTRING_SECTION_2 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->section( *hbqt_par_QRegExp( 2 ), hb_parni( 3 ), hb_parnidef( 4, -1 ), ( HB_ISNUM( 5 ) ? ( HBQString::SectionFlags ) hb_parni( 5 ) : ( HBQString::SectionFlags ) HBQString::SectionDefault ) ).toUtf8().data() );
   }
}

/*
 * QString & setNum ( int n, int base = 10 )
 */
HB_FUNC( QT_HBQSTRING_SETNUM )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->setNum( hb_parni( 2 ), hb_parnidef( 3, 10 ) ).toUtf8().data() );
   }
}

/*
 * QString & setNum ( uint n, int base = 10 )
 */
HB_FUNC( QT_HBQSTRING_SETNUM_1 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->setNum( hb_parni( 2 ), hb_parnidef( 3, 10 ) ).toUtf8().data() );
   }
}

/*
 * QString & setNum ( long n, int base = 10 )
 */
HB_FUNC( QT_HBQSTRING_SETNUM_2 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->setNum( ( long ) hb_parnint( 2 ), hb_parnidef( 3, 10 ) ).toUtf8().data() );
   }
}

/*
 * QString & setNum ( ulong n, int base = 10 )
 */
HB_FUNC( QT_HBQSTRING_SETNUM_3 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->setNum( ( ulong ) hb_parnint( 2 ), hb_parnidef( 3, 10 ) ).toUtf8().data() );
   }
}

/*
 * QString & setNum ( qlonglong n, int base = 10 )
 */
HB_FUNC( QT_HBQSTRING_SETNUM_4 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->setNum( ( qlonglong ) hb_parnint( 2 ), hb_parnidef( 3, 10 ) ).toUtf8().data() );
   }
}

/*
 * QString & setNum ( qulonglong n, int base = 10 )
 */
HB_FUNC( QT_HBQSTRING_SETNUM_5 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->setNum( ( qulonglong ) hb_parnint( 2 ), hb_parnidef( 3, 10 ) ).toUtf8().data() );
   }
}

/*
 * QString & setNum ( short n, int base = 10 )
 */
HB_FUNC( QT_HBQSTRING_SETNUM_6 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->setNum( hb_parni( 2 ), hb_parnidef( 3, 10 ) ).toUtf8().data() );
   }
}

/*
 * QString & setNum ( ushort n, int base = 10 )
 */
HB_FUNC( QT_HBQSTRING_SETNUM_7 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->setNum( hb_parni( 2 ), hb_parnidef( 3, 10 ) ).toUtf8().data() );
   }
}

/*
 * QString & setNum ( double n, char format = 'g', int precision = 6 )
 */
HB_FUNC( QT_HBQSTRING_SETNUM_8 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->setNum( hb_parnd( 2 ), ( char ) hb_parni( 3 ), hb_parnidef( 4, 6 ) ).toUtf8().data() );
   }
}

/*
 * QString & setNum ( float n, char format = 'g', int precision = 6 )
 */
HB_FUNC( QT_HBQSTRING_SETNUM_9 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->setNum( hb_parnd( 2 ), ( char ) hb_parni( 3 ), hb_parnidef( 4, 6 ) ).toUtf8().data() );
   }
}

/*
 * QString & setUnicode ( const QChar * unicode, int size )
 */
HB_FUNC( QT_HBQSTRING_SETUNICODE )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->setUnicode( hbqt_par_QChar( 2 ), hb_parni( 3 ) ).toUtf8().data() );
   }
}

/*
 * QString & setUtf16 ( const ushort * unicode, int size )
 */
HB_FUNC( QT_HBQSTRING_SETUTF16 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   ushort iUnicode = 0;

   if( p )
   {
      hb_retstr_utf8( ( p )->setUtf16( &iUnicode, hb_parni( 3 ) ).toUtf8().data() );
   }

   hb_storni( iUnicode, 2 );
}

/*
 * QString simplified () const
 */
HB_FUNC( QT_HBQSTRING_SIMPLIFIED )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->simplified().toUtf8().data() );
   }
}

/*
 * int size () const
 */
HB_FUNC( QT_HBQSTRING_SIZE )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retni( ( p )->size() );
   }
}

/*
 * QStringList split ( const QString & sep, SplitBehavior behavior = KeepEmptyParts, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const
 */
HB_FUNC( QT_HBQSTRING_SPLIT )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->split( hb_parstr_utf8( 2, &pText, NULL ), ( HB_ISNUM( 3 ) ? ( HBQString::SplitBehavior ) hb_parni( 3 ) : ( HBQString::SplitBehavior ) HBQString::KeepEmptyParts ), ( HB_ISNUM( 4 ) ? ( Qt::CaseSensitivity ) hb_parni( 4 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ) ), true ) );
      hb_strfree( pText );
   }
}

/*
 * QStringList split ( const QChar & sep, SplitBehavior behavior = KeepEmptyParts, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const
 */
HB_FUNC( QT_HBQSTRING_SPLIT_1 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->split( *hbqt_par_QChar( 2 ), ( HB_ISNUM( 3 ) ? ( HBQString::SplitBehavior ) hb_parni( 3 ) : ( HBQString::SplitBehavior ) HBQString::KeepEmptyParts ), ( HB_ISNUM( 4 ) ? ( Qt::CaseSensitivity ) hb_parni( 4 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ) ), true ) );
   }
}

/*
 * QStringList split ( const QRegExp & rx, SplitBehavior behavior = KeepEmptyParts ) const
 */
HB_FUNC( QT_HBQSTRING_SPLIT_2 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->split( *hbqt_par_QRegExp( 2 ), ( HB_ISNUM( 3 ) ? ( HBQString::SplitBehavior ) hb_parni( 3 ) : ( HBQString::SplitBehavior ) HBQString::KeepEmptyParts ) ) ), true ) );
   }
}

/*
 * void squeeze ()
 */
HB_FUNC( QT_HBQSTRING_SQUEEZE )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      ( p )->squeeze();
   }
}

/*
 * bool startsWith ( const QString & s, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const
 */
HB_FUNC( QT_HBQSTRING_STARTSWITH )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->startsWith( hb_parstr_utf8( 2, &pText, NULL ), ( HB_ISNUM( 3 ) ? ( Qt::CaseSensitivity ) hb_parni( 3 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ) );
      hb_strfree( pText );
   }
}

/*
 * bool startsWith ( const QLatin1String & s, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const
 */
HB_FUNC( QT_HBQSTRING_STARTSWITH_1 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retl( ( p )->startsWith( *hbqt_par_QLatin1String( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::CaseSensitivity ) hb_parni( 3 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ) );
   }
}

/*
 * bool startsWith ( const QChar & c, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const
 */
HB_FUNC( QT_HBQSTRING_STARTSWITH_2 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retl( ( p )->startsWith( *hbqt_par_QChar( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::CaseSensitivity ) hb_parni( 3 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ) );
   }
}

/*
 * QByteArray toAscii () const
 */
HB_FUNC( QT_HBQSTRING_TOASCII )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->toAscii() ), true ) );
   }
}

/*
 * QString toCaseFolded () const
 */
HB_FUNC( QT_HBQSTRING_TOCASEFOLDED )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->toCaseFolded().toUtf8().data() );
   }
}

/*
 * double toDouble ( bool * ok = 0 ) const
 */
HB_FUNC( QT_HBQSTRING_TODOUBLE )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   bool iOk = 0;

   if( p )
   {
      hb_retnd( ( p )->toDouble( &iOk ) );
   }

   hb_stornl( iOk, 2 );
}

/*
 * float toFloat ( bool * ok = 0 ) const
 */
HB_FUNC( QT_HBQSTRING_TOFLOAT )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   bool iOk = 0;

   if( p )
   {
      hb_retnd( ( p )->toFloat( &iOk ) );
   }

   hb_stornl( iOk, 2 );
}

/*
 * int toInt ( bool * ok = 0, int base = 10 ) const
 */
HB_FUNC( QT_HBQSTRING_TOINT )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   bool iOk = 0;

   if( p )
   {
      hb_retni( ( p )->toInt( &iOk, hb_parnidef( 3, 10 ) ) );
   }

   hb_stornl( iOk, 2 );
}

/*
 * QByteArray toLatin1 () const
 */
HB_FUNC( QT_HBQSTRING_TOLATIN1 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->toLatin1() ), true ) );
   }
}

/*
 * QByteArray toLocal8Bit () const
 */
HB_FUNC( QT_HBQSTRING_TOLOCAL8BIT )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->toLocal8Bit() ), true ) );
   }
}

/*
 * long toLong ( bool * ok = 0, int base = 10 ) const
 */
HB_FUNC( QT_HBQSTRING_TOLONG )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   bool iOk = 0;

   if( p )
   {
      hb_retnint( ( p )->toLong( &iOk, hb_parnidef( 3, 10 ) ) );
   }

   hb_stornl( iOk, 2 );
}

/*
 * qlonglong toLongLong ( bool * ok = 0, int base = 10 ) const
 */
HB_FUNC( QT_HBQSTRING_TOLONGLONG )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   bool iOk = 0;

   if( p )
   {
      hb_retnint( ( p )->toLongLong( &iOk, hb_parnidef( 3, 10 ) ) );
   }

   hb_stornl( iOk, 2 );
}

/*
 * QString toLower () const
 */
HB_FUNC( QT_HBQSTRING_TOLOWER )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->toLower().toUtf8().data() );
   }
}

/*
 * short toShort ( bool * ok = 0, int base = 10 ) const
 */
HB_FUNC( QT_HBQSTRING_TOSHORT )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   bool iOk = 0;

   if( p )
   {
      hb_retni( ( p )->toShort( &iOk, hb_parnidef( 3, 10 ) ) );
   }

   hb_stornl( iOk, 2 );
}

/*
 * uint toUInt ( bool * ok = 0, int base = 10 ) const
 */
HB_FUNC( QT_HBQSTRING_TOUINT )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   bool iOk = 0;

   if( p )
   {
      hb_retni( ( p )->toUInt( &iOk, hb_parnidef( 3, 10 ) ) );
   }

   hb_stornl( iOk, 2 );
}

/*
 * ulong toULong ( bool * ok = 0, int base = 10 ) const
 */
HB_FUNC( QT_HBQSTRING_TOULONG )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   bool iOk = 0;

   if( p )
   {
      hb_retnint( ( p )->toULong( &iOk, hb_parnidef( 3, 10 ) ) );
   }

   hb_stornl( iOk, 2 );
}

/*
 * qulonglong toULongLong ( bool * ok = 0, int base = 10 ) const
 */
HB_FUNC( QT_HBQSTRING_TOULONGLONG )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   bool iOk = 0;

   if( p )
   {
      hb_retnint( ( p )->toULongLong( &iOk, hb_parnidef( 3, 10 ) ) );
   }

   hb_stornl( iOk, 2 );
}

/*
 * ushort toUShort ( bool * ok = 0, int base = 10 ) const
 */
HB_FUNC( QT_HBQSTRING_TOUSHORT )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   bool iOk = 0;

   if( p )
   {
      hb_retni( ( p )->toUShort( &iOk, hb_parnidef( 3, 10 ) ) );
   }

   hb_stornl( iOk, 2 );
}

/*
 * QString toUpper () const
 */
HB_FUNC( QT_HBQSTRING_TOUPPER )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->toUpper().toUtf8().data() );
   }
}

/*
 * QByteArray toUtf8 () const
 */
HB_FUNC( QT_HBQSTRING_TOUTF8 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->toUtf8() ), true ) );
   }
}

/*
 * QString trimmed () const
 */
HB_FUNC( QT_HBQSTRING_TRIMMED )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->trimmed().toUtf8().data() );
   }
}

/*
 * void truncate ( int position )
 */
HB_FUNC( QT_HBQSTRING_TRUNCATE )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      ( p )->truncate( hb_parni( 2 ) );
   }
}

/*
 * const QChar * unicode () const
 */
HB_FUNC( QT_HBQSTRING_UNICODE )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QChar( new QChar( *( ( p )->unicode() ) ), true ) );
   }
}

/*
 * int compare ( const QString & s1, const QString & s2, Qt::CaseSensitivity cs )
 */
HB_FUNC( QT_HBQSTRING_COMPARE_4 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->compare( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ), ( Qt::CaseSensitivity ) hb_parni( 4 ) ) );
      hb_strfree( pText );
   }
}

/*
 * int compare ( const QString & s1, const QString & s2 )
 */
HB_FUNC( QT_HBQSTRING_COMPARE_5 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->compare( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * int compare ( const QString & s1, const QLatin1String & s2, Qt::CaseSensitivity cs = Qt::CaseSensitive )
 */
HB_FUNC( QT_HBQSTRING_COMPARE_6 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->compare( hb_parstr_utf8( 2, &pText, NULL ), *hbqt_par_QLatin1String( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::CaseSensitivity ) hb_parni( 4 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ) );
      hb_strfree( pText );
   }
}

/*
 * int compare ( const QLatin1String & s1, const QString & s2, Qt::CaseSensitivity cs = Qt::CaseSensitive )
 */
HB_FUNC( QT_HBQSTRING_COMPARE_7 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->compare( *hbqt_par_QLatin1String( 2 ), hb_parstr_utf8( 3, &pText, NULL ), ( HB_ISNUM( 4 ) ? ( Qt::CaseSensitivity ) hb_parni( 4 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ) );
      hb_strfree( pText );
   }
}

/*
 * int compare ( const QString & s1, const QStringRef & s2, Qt::CaseSensitivity cs = Qt::CaseSensitive )
 */
HB_FUNC( QT_HBQSTRING_COMPARE_8 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->compare( hb_parstr_utf8( 2, &pText, NULL ), *hbqt_par_QStringRef( 3 ), ( HB_ISNUM( 4 ) ? ( Qt::CaseSensitivity ) hb_parni( 4 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ) );
      hb_strfree( pText );
   }
}

/*
 * QString fromAscii ( const char * str, int size = -1 )
 */
HB_FUNC( QT_HBQSTRING_FROMASCII )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->fromAscii( ( const char * ) hb_parc( 2 ), hb_parnidef( 3, -1 ) ).toUtf8().data() );
   }
}

/*
 * QString fromLatin1 ( const char * str, int size = -1 )
 */
HB_FUNC( QT_HBQSTRING_FROMLATIN1 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->fromLatin1( ( const char * ) hb_parc( 2 ), hb_parnidef( 3, -1 ) ).toUtf8().data() );
   }
}

/*
 * QString fromLocal8Bit ( const char * str, int size = -1 )
 */
HB_FUNC( QT_HBQSTRING_FROMLOCAL8BIT )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->fromLocal8Bit( ( const char * ) hb_parc( 2 ), hb_parnidef( 3, -1 ) ).toUtf8().data() );
   }
}

/*
 * QString fromRawData ( const QChar * unicode, int size )
 */
HB_FUNC( QT_HBQSTRING_FROMRAWDATA )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->fromRawData( hbqt_par_QChar( 2 ), hb_parni( 3 ) ).toUtf8().data() );
   }
}

/*
 * QString fromUcs4 ( const uint * unicode, int size = -1 )
 */
HB_FUNC( QT_HBQSTRING_FROMUCS4 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   uint iUnicode = 0;

   if( p )
   {
      hb_retstr_utf8( ( p )->fromUcs4( &iUnicode, hb_parnidef( 3, -1 ) ).toUtf8().data() );
   }

   hb_storni( iUnicode, 2 );
}

/*
 * QString fromUtf8 ( const char * str, int size = -1 )
 */
HB_FUNC( QT_HBQSTRING_FROMUTF8 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->fromUtf8( ( const char * ) hb_parc( 2 ), hb_parnidef( 3, -1 ) ).toUtf8().data() );
   }
}

/*
 * QString fromUtf16 ( const ushort * unicode, int size = -1 )
 */
HB_FUNC( QT_HBQSTRING_FROMUTF16 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   ushort iUnicode = 0;

   if( p )
   {
      hb_retstr_utf8( ( p )->fromUtf16( &iUnicode, hb_parnidef( 3, -1 ) ).toUtf8().data() );
   }

   hb_storni( iUnicode, 2 );
}

/*
 * int localeAwareCompare ( const QString & s1, const QString & s2 )
 */
HB_FUNC( QT_HBQSTRING_LOCALEAWARECOMPARE_2 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->localeAwareCompare( hb_parstr_utf8( 2, &pText, NULL ), hb_parstr_utf8( 3, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * int localeAwareCompare ( const QString & s1, const QStringRef & s2 )
 */
HB_FUNC( QT_HBQSTRING_LOCALEAWARECOMPARE_3 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->localeAwareCompare( hb_parstr_utf8( 2, &pText, NULL ), *hbqt_par_QStringRef( 3 ) ) );
      hb_strfree( pText );
   }
}

/*
 * QString number ( long n, int base = 10 )
 */
HB_FUNC( QT_HBQSTRING_NUMBER )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->number( ( long ) hb_parnint( 2 ), hb_parnidef( 3, 10 ) ).toUtf8().data() );
   }
}

/*
 * QString number ( double n, char format = 'g', int precision = 6 )
 */
HB_FUNC( QT_HBQSTRING_NUMBER_1 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->number( hb_parnd( 2 ), ( char ) hb_parni( 3 ), hb_parnidef( 4, 6 ) ).toUtf8().data() );
   }
}

/*
 * QString number ( ulong n, int base = 10 )
 */
HB_FUNC( QT_HBQSTRING_NUMBER_2 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->number( ( ulong ) hb_parnint( 2 ), hb_parnidef( 3, 10 ) ).toUtf8().data() );
   }
}

/*
 * QString number ( int n, int base = 10 )
 */
HB_FUNC( QT_HBQSTRING_NUMBER_3 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->number( hb_parni( 2 ), hb_parnidef( 3, 10 ) ).toUtf8().data() );
   }
}

/*
 * QString number ( uint n, int base = 10 )
 */
HB_FUNC( QT_HBQSTRING_NUMBER_4 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->number( hb_parni( 2 ), hb_parnidef( 3, 10 ) ).toUtf8().data() );
   }
}

/*
 * QString number ( qlonglong n, int base = 10 )
 */
HB_FUNC( QT_HBQSTRING_NUMBER_5 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->number( ( qlonglong ) hb_parnint( 2 ), hb_parnidef( 3, 10 ) ).toUtf8().data() );
   }
}

/*
 * QString number ( qulonglong n, int base = 10 )
 */
HB_FUNC( QT_HBQSTRING_NUMBER_6 )
{
   HBQString * p = hbqt_par_HBQString( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->number( ( qulonglong ) hb_parnint( 2 ), hb_parnidef( 3, 10 ) ).toUtf8().data() );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
