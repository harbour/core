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

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  Constructed[ 36/42 [ 85.71% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QSet<QString> toSet () const
 *  std::list<QString> toStdList () const
 *  QVector<QString> toVector () const
 *
 *  *** Commented out protos which construct fine but do not compile ***
 *
 *  // bool contains ( const QString & str, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const
 *  // QStringList & replaceInStrings ( const QString & before, const QString & after, Qt::CaseSensitivity cs = Qt::CaseSensitive )
 *  // QStringList & replaceInStrings ( const QRegExp & rx, const QString & after )
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
      ( p )->append( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_APPEND FP=( p )->append( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * QStringList filter ( const QString & str, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const
 */
HB_FUNC( QT_QSTRINGLIST_FILTER )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->filter( hbqt_par_QString( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::CaseSensitivity ) hb_parni( 3 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_FILTER FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->filter( hbqt_par_QString( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::CaseSensitivity ) hb_parni( 3 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QStringList filter ( const QRegExp & rx ) const
 */
HB_FUNC( QT_QSTRINGLIST_FILTER_1 )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->filter( *hbqt_par_QRegExp( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_FILTER_1 FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->filter( *hbqt_par_QRegExp( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * int indexOf ( const QString & value, int from = 0 ) const
 */
HB_FUNC( QT_QSTRINGLIST_INDEXOF )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retni( ( p )->indexOf( hbqt_par_QString( 2 ), hb_parni( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_INDEXOF FP=hb_retni( ( p )->indexOf( hbqt_par_QString( 2 ), hb_parni( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * int indexOf ( const QRegExp & rx, int from = 0 ) const
 */
HB_FUNC( QT_QSTRINGLIST_INDEXOF_1 )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retni( ( p )->indexOf( *hbqt_par_QRegExp( 2 ), hb_parni( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_INDEXOF_1 FP=hb_retni( ( p )->indexOf( *hbqt_par_QRegExp( 2 ), hb_parni( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * int indexOf ( QRegExp & rx, int from = 0 ) const
 */
HB_FUNC( QT_QSTRINGLIST_INDEXOF_2 )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retni( ( p )->indexOf( *hbqt_par_QRegExp( 2 ), hb_parni( 3 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_INDEXOF_2 FP=hb_retni( ( p )->indexOf( *hbqt_par_QRegExp( 2 ), hb_parni( 3 ) ) ); p is NULL" ) );
   }
}

/*
 * QString join ( const QString & separator ) const
 */
HB_FUNC( QT_QSTRINGLIST_JOIN )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retc( ( p )->join( hbqt_par_QString( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_JOIN FP=hb_retc( ( p )->join( hbqt_par_QString( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * int lastIndexOf ( const QRegExp & rx, int from = -1 ) const
 */
HB_FUNC( QT_QSTRINGLIST_LASTINDEXOF )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retni( ( p )->lastIndexOf( *hbqt_par_QRegExp( 2 ), hb_parnidef( 3, -1 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_LASTINDEXOF FP=hb_retni( ( p )->lastIndexOf( *hbqt_par_QRegExp( 2 ), hb_parnidef( 3, -1 ) ) ); p is NULL" ) );
   }
}

/*
 * int lastIndexOf ( const QString & value, int from = -1 ) const
 */
HB_FUNC( QT_QSTRINGLIST_LASTINDEXOF_1 )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retni( ( p )->lastIndexOf( hbqt_par_QString( 2 ), hb_parnidef( 3, -1 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_LASTINDEXOF_1 FP=hb_retni( ( p )->lastIndexOf( hbqt_par_QString( 2 ), hb_parnidef( 3, -1 ) ) ); p is NULL" ) );
   }
}

/*
 * int lastIndexOf ( QRegExp & rx, int from = -1 ) const
 */
HB_FUNC( QT_QSTRINGLIST_LASTINDEXOF_2 )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retni( ( p )->lastIndexOf( *hbqt_par_QRegExp( 2 ), hb_parnidef( 3, -1 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_LASTINDEXOF_2 FP=hb_retni( ( p )->lastIndexOf( *hbqt_par_QRegExp( 2 ), hb_parnidef( 3, -1 ) ) ); p is NULL" ) );
   }
}

/*
 * int removeDuplicates ()
 */
HB_FUNC( QT_QSTRINGLIST_REMOVEDUPLICATES )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retni( ( p )->removeDuplicates() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_REMOVEDUPLICATES FP=hb_retni( ( p )->removeDuplicates() ); p is NULL" ) );
   }
}

/*
 * void sort ()
 */
HB_FUNC( QT_QSTRINGLIST_SORT )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      ( p )->sort();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_SORT FP=( p )->sort(); p is NULL" ) );
   }
}

/*
 * QString & at ( int i ) const
 */
HB_FUNC( QT_QSTRINGLIST_AT )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retc( ( p )->at( hb_parni( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_AT FP=hb_retc( ( p )->at( hb_parni( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString & back ()
 */
HB_FUNC( QT_QSTRINGLIST_BACK )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retc( ( p )->back().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_BACK FP=hb_retc( ( p )->back().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * int count ( const QString & value ) const
 */
HB_FUNC( QT_QSTRINGLIST_COUNT )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retni( ( p )->count( hbqt_par_QString( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_COUNT FP=hb_retni( ( p )->count( hbqt_par_QString( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool endsWith ( const QString & value ) const
 */
HB_FUNC( QT_QSTRINGLIST_ENDSWITH )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retl( ( p )->endsWith( hbqt_par_QString( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_ENDSWITH FP=hb_retl( ( p )->endsWith( hbqt_par_QString( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QString & first ()
 */
HB_FUNC( QT_QSTRINGLIST_FIRST )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retc( ( p )->first().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_FIRST FP=hb_retc( ( p )->first().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * const QString & first () const
 */
HB_FUNC( QT_QSTRINGLIST_FIRST_1 )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retc( ( p )->first().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_FIRST_1 FP=hb_retc( ( p )->first().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString & front ()
 */
HB_FUNC( QT_QSTRINGLIST_FRONT )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retc( ( p )->front().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_FRONT FP=hb_retc( ( p )->front().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * const QString & front () const
 */
HB_FUNC( QT_QSTRINGLIST_FRONT_1 )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retc( ( p )->front().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_FRONT_1 FP=hb_retc( ( p )->front().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * void insert ( int i, const QString & value )
 */
HB_FUNC( QT_QSTRINGLIST_INSERT )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      ( p )->insert( hb_parni( 2 ), hbqt_par_QString( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_INSERT FP=( p )->insert( hb_parni( 2 ), hbqt_par_QString( 3 ) ); p is NULL" ) );
   }
}

/*
 * QString & last ()
 */
HB_FUNC( QT_QSTRINGLIST_LAST )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retc( ( p )->last().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_LAST FP=hb_retc( ( p )->last().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * const QString & last () const
 */
HB_FUNC( QT_QSTRINGLIST_LAST_1 )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retc( ( p )->last().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_LAST_1 FP=hb_retc( ( p )->last().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QList<QString> mid ( int pos, int length = -1 ) const
 */
HB_FUNC( QT_QSTRINGLIST_MID )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QString>( ( p )->mid( hb_parni( 2 ), hb_parnidef( 3, -1 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_MID FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QString>( ( p )->mid( hb_parni( 2 ), hb_parnidef( 3, -1 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * void prepend ( const QString & value )
 */
HB_FUNC( QT_QSTRINGLIST_PREPEND )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      ( p )->prepend( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_PREPEND FP=( p )->prepend( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void push_back ( const QString & value )
 */
HB_FUNC( QT_QSTRINGLIST_PUSH_BACK )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      ( p )->push_back( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_PUSH_BACK FP=( p )->push_back( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void push_front ( const QString & value )
 */
HB_FUNC( QT_QSTRINGLIST_PUSH_FRONT )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      ( p )->push_front( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_PUSH_FRONT FP=( p )->push_front( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * int removeAll ( const QString & value )
 */
HB_FUNC( QT_QSTRINGLIST_REMOVEALL )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retni( ( p )->removeAll( hbqt_par_QString( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_REMOVEALL FP=hb_retni( ( p )->removeAll( hbqt_par_QString( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool removeOne ( const QString & value )
 */
HB_FUNC( QT_QSTRINGLIST_REMOVEONE )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retl( ( p )->removeOne( hbqt_par_QString( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_REMOVEONE FP=hb_retl( ( p )->removeOne( hbqt_par_QString( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * void replace ( int i, const QString & value )
 */
HB_FUNC( QT_QSTRINGLIST_REPLACE )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      ( p )->replace( hb_parni( 2 ), hbqt_par_QString( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_REPLACE FP=( p )->replace( hb_parni( 2 ), hbqt_par_QString( 3 ) ); p is NULL" ) );
   }
}

/*
 * bool startsWith ( const QString & value ) const
 */
HB_FUNC( QT_QSTRINGLIST_STARTSWITH )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retl( ( p )->startsWith( hbqt_par_QString( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_STARTSWITH FP=hb_retl( ( p )->startsWith( hbqt_par_QString( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * QString takeAt ( int i )
 */
HB_FUNC( QT_QSTRINGLIST_TAKEAT )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retc( ( p )->takeAt( hb_parni( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_TAKEAT FP=hb_retc( ( p )->takeAt( hb_parni( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString takeFirst ()
 */
HB_FUNC( QT_QSTRINGLIST_TAKEFIRST )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retc( ( p )->takeFirst().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_TAKEFIRST FP=hb_retc( ( p )->takeFirst().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString takeLast ()
 */
HB_FUNC( QT_QSTRINGLIST_TAKELAST )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retc( ( p )->takeLast().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_TAKELAST FP=hb_retc( ( p )->takeLast().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString value ( int i ) const
 */
HB_FUNC( QT_QSTRINGLIST_VALUE )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retc( ( p )->value( hb_parni( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_VALUE FP=hb_retc( ( p )->value( hb_parni( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString value ( int i, const QString & defaultValue ) const
 */
HB_FUNC( QT_QSTRINGLIST_VALUE_1 )
{
   QStringList * p = hbqt_par_QStringList( 1 );
   if( p )
      hb_retc( ( p )->value( hb_parni( 2 ), hbqt_par_QString( 3 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QSTRINGLIST_VALUE_1 FP=hb_retc( ( p )->value( hb_parni( 2 ), hbqt_par_QString( 3 ) ).toAscii().data() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
