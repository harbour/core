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
 *  Constructed[ 33/36 [ 91.67% ] ]
 *
 *  *** Unconvered Prototypes ***
 *
 *  QSet<QString> toSet () const
 *  std::list<QString> toStdList () const
 *  QVector<QString> toVector () const
 *
 *  *** Commented out protostypes ***
 *
 *  // bool contains ( const QString & str, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const
 *  // QStringList & replaceInStrings ( const QString & before, const QString & after, Qt::CaseSensitivity cs = Qt::CaseSensitive )
 *  // QStringList & replaceInStrings ( const QRegExp & rx, const QString & after )
 *  //QString & first ()
 *  //QString & front ()
 *  //QString & last ()
 */

#include <QtCore/QPointer>

#include <QtCore/QStringList>


/*
 * QStringList ()
 * QStringList ( const QString & str )
 * QStringList ( const QStringList & other )
 * QStringList ( const QList<QString> & other )
 */

typedef struct
{
   QStringList * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QStringList;

HBQT_GC_FUNC( hbqt_gcRelease_QStringList )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QStringList   /.\\", p->ph ) );
         delete ( ( QStringList * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QStringList   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QStringList    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QStringList    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QStringList( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QStringList * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QStringList;
   p->type = HBQT_TYPE_QStringList;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QStringList", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QStringList", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QSTRINGLIST )
{
   QStringList * pObj = NULL;

   pObj =  new QStringList() ;

   hb_retptrGC( hbqt_gcAllocate_QStringList( ( void * ) pObj, true ) );
}

/*
 * void append ( const QString & value )
 */
HB_FUNC( QT_QSTRINGLIST_APPEND )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      void * pText;
      ( p )->append( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * QStringList filter ( const QString & str, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const
 */
HB_FUNC( QT_QSTRINGLIST_FILTER )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->filter( hb_parstr_utf8( 2, &pText, NULL ), ( HB_ISNUM( 3 ) ? ( Qt::CaseSensitivity ) hb_parni( 3 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ) ), true ) );
      hb_strfree( pText );
   }
}

/*
 * QStringList filter ( const QRegExp & rx ) const
 */
HB_FUNC( QT_QSTRINGLIST_FILTER_1 )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->filter( *hbqt_par_QRegExp( 2 ) ) ), true ) );
   }
}

/*
 * int indexOf ( const QString & value, int from = 0 ) const
 */
HB_FUNC( QT_QSTRINGLIST_INDEXOF )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->indexOf( hb_parstr_utf8( 2, &pText, NULL ), hb_parni( 3 ) ) );
      hb_strfree( pText );
   }
}

/*
 * int indexOf ( const QRegExp & rx, int from = 0 ) const
 */
HB_FUNC( QT_QSTRINGLIST_INDEXOF_1 )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      hb_retni( ( p )->indexOf( *hbqt_par_QRegExp( 2 ), hb_parni( 3 ) ) );
   }
}

/*
 * int indexOf ( QRegExp & rx, int from = 0 ) const
 */
HB_FUNC( QT_QSTRINGLIST_INDEXOF_2 )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      hb_retni( ( p )->indexOf( *hbqt_par_QRegExp( 2 ), hb_parni( 3 ) ) );
   }
}

/*
 * QString join ( const QString & separator ) const
 */
HB_FUNC( QT_QSTRINGLIST_JOIN )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->join( hb_parstr_utf8( 2, &pText, NULL ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}

/*
 * int lastIndexOf ( const QRegExp & rx, int from = -1 ) const
 */
HB_FUNC( QT_QSTRINGLIST_LASTINDEXOF )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      hb_retni( ( p )->lastIndexOf( *hbqt_par_QRegExp( 2 ), hb_parnidef( 3, -1 ) ) );
   }
}

/*
 * int lastIndexOf ( const QString & value, int from = -1 ) const
 */
HB_FUNC( QT_QSTRINGLIST_LASTINDEXOF_1 )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->lastIndexOf( hb_parstr_utf8( 2, &pText, NULL ), hb_parnidef( 3, -1 ) ) );
      hb_strfree( pText );
   }
}

/*
 * int lastIndexOf ( QRegExp & rx, int from = -1 ) const
 */
HB_FUNC( QT_QSTRINGLIST_LASTINDEXOF_2 )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      hb_retni( ( p )->lastIndexOf( *hbqt_par_QRegExp( 2 ), hb_parnidef( 3, -1 ) ) );
   }
}

/*
 * int removeDuplicates ()
 */
HB_FUNC( QT_QSTRINGLIST_REMOVEDUPLICATES )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      hb_retni( ( p )->removeDuplicates() );
   }
}

/*
 * void sort ()
 */
HB_FUNC( QT_QSTRINGLIST_SORT )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      ( p )->sort();
   }
}

/*
 * QString & at ( int i ) const
 */
HB_FUNC( QT_QSTRINGLIST_AT )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->at( hb_parni( 2 ) ).toUtf8().data() );
   }
}

/*
 * QString & back ()
 */
HB_FUNC( QT_QSTRINGLIST_BACK )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->back().toUtf8().data() );
   }
}

/*
 * int count ( const QString & value ) const
 */
HB_FUNC( QT_QSTRINGLIST_COUNT )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->count( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * bool endsWith ( const QString & value ) const
 */
HB_FUNC( QT_QSTRINGLIST_ENDSWITH )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->endsWith( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * const QString & first () const
 */
HB_FUNC( QT_QSTRINGLIST_FIRST )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->first().toUtf8().data() );
   }
}

/*
 * const QString & front () const
 */
HB_FUNC( QT_QSTRINGLIST_FRONT )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->front().toUtf8().data() );
   }
}

/*
 * void insert ( int i, const QString & value )
 */
HB_FUNC( QT_QSTRINGLIST_INSERT )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      void * pText;
      ( p )->insert( hb_parni( 2 ), hb_parstr_utf8( 3, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * const QString & last () const
 */
HB_FUNC( QT_QSTRINGLIST_LAST )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->last().toUtf8().data() );
   }
}

/*
 * QList<QString> mid ( int pos, int length = -1 ) const
 */
HB_FUNC( QT_QSTRINGLIST_MID )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QString>( ( p )->mid( hb_parni( 2 ), hb_parnidef( 3, -1 ) ) ), true ) );
   }
}

/*
 * void prepend ( const QString & value )
 */
HB_FUNC( QT_QSTRINGLIST_PREPEND )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      void * pText;
      ( p )->prepend( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void push_back ( const QString & value )
 */
HB_FUNC( QT_QSTRINGLIST_PUSH_BACK )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      void * pText;
      ( p )->push_back( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * void push_front ( const QString & value )
 */
HB_FUNC( QT_QSTRINGLIST_PUSH_FRONT )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      void * pText;
      ( p )->push_front( hb_parstr_utf8( 2, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * int removeAll ( const QString & value )
 */
HB_FUNC( QT_QSTRINGLIST_REMOVEALL )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->removeAll( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * bool removeOne ( const QString & value )
 */
HB_FUNC( QT_QSTRINGLIST_REMOVEONE )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->removeOne( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * void replace ( int i, const QString & value )
 */
HB_FUNC( QT_QSTRINGLIST_REPLACE )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      void * pText;
      ( p )->replace( hb_parni( 2 ), hb_parstr_utf8( 3, &pText, NULL ) );
      hb_strfree( pText );
   }
}

/*
 * bool startsWith ( const QString & value ) const
 */
HB_FUNC( QT_QSTRINGLIST_STARTSWITH )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      void * pText;
      hb_retl( ( p )->startsWith( hb_parstr_utf8( 2, &pText, NULL ) ) );
      hb_strfree( pText );
   }
}

/*
 * QString takeAt ( int i )
 */
HB_FUNC( QT_QSTRINGLIST_TAKEAT )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->takeAt( hb_parni( 2 ) ).toUtf8().data() );
   }
}

/*
 * QString takeFirst ()
 */
HB_FUNC( QT_QSTRINGLIST_TAKEFIRST )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->takeFirst().toUtf8().data() );
   }
}

/*
 * QString takeLast ()
 */
HB_FUNC( QT_QSTRINGLIST_TAKELAST )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->takeLast().toUtf8().data() );
   }
}

/*
 * QString value ( int i ) const
 */
HB_FUNC( QT_QSTRINGLIST_VALUE )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->value( hb_parni( 2 ) ).toUtf8().data() );
   }
}

/*
 * QString value ( int i, const QString & defaultValue ) const
 */
HB_FUNC( QT_QSTRINGLIST_VALUE_1 )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
   {
      void * pText;
      hb_retstr_utf8( ( p )->value( hb_parni( 2 ), hb_parstr_utf8( 3, &pText, NULL ) ).toUtf8().data() );
      hb_strfree( pText );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
