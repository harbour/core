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
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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
 *  Constructed[ 97/101 [ 96.04% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QList<QByteArray> split ( char sep ) const
 *
 *  *** Commented out protos which construct fine but do not compile ***
 *
 *  // bool contains ( const QByteArray & ba ) const
 *  // bool contains ( const char * str ) const
 *  // bool contains ( char ch ) const
 */

#include <QtCore/QPointer>

#include <QtCore/QByteArray>


/* QByteArray ()
 * QByteArray ( const char * str )
 * QByteArray ( const char * data, int size )
 * QByteArray ( int size, char ch )
 * QByteArray ( const QByteArray & other )
 * ~QByteArray ()
 */

QT_G_FUNC( release_QByteArray )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QByteArray                   p=%p", p ) );
   HB_TRACE( HB_TR_DEBUG, ( "release_QByteArray                  ph=%p", p->ph ) );

   if( p && p->ph )
   {
      delete ( ( QByteArray * ) p->ph );
      p->ph = NULL;
      HB_TRACE( HB_TR_DEBUG, ( "YES release_QByteArray                  Object deleted! %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "DEL release_QByteArray                  Object Allready deleted!" ) );
   }
}

void * gcAllocate_QByteArray( void * pObj )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QByteArray;
   HB_TRACE( HB_TR_DEBUG, ( "          new_QByteArray                  %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   return( p );
}

HB_FUNC( QT_QBYTEARRAY )
{
   void * pObj = NULL;

   pObj = new QByteArray() ;

   hb_retptrGC( gcAllocate_QByteArray( pObj ) );
}
/*
 * QByteArray & append ( const QByteArray & ba )
 */
HB_FUNC( QT_QBYTEARRAY_APPEND )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->append( *hbqt_par_QByteArray( 2 ) ) ) ) );
}

/*
 * QByteArray & append ( const QString & str )
 */
HB_FUNC( QT_QBYTEARRAY_APPEND_1 )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->append( hbqt_par_QString( 2 ) ) ) ) );
}

/*
 * QByteArray & append ( const char * str )
 */
HB_FUNC( QT_QBYTEARRAY_APPEND_2 )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->append( hbqt_par_char( 2 ) ) ) ) );
}

/*
 * QByteArray & append ( const char * str, int len )
 */
HB_FUNC( QT_QBYTEARRAY_APPEND_3 )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->append( hbqt_par_char( 2 ), hb_parni( 3 ) ) ) ) );
}

/*
 * QByteArray & append ( char ch )
 */
HB_FUNC( QT_QBYTEARRAY_APPEND_4 )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->append( ( char ) hb_parni( 2 ) ) ) ) );
}

/*
 * char at ( int i ) const
 */
HB_FUNC( QT_QBYTEARRAY_AT )
{
   hb_retni( hbqt_par_QByteArray( 1 )->at( hb_parni( 2 ) ) );
}

/*
 * int capacity () const
 */
HB_FUNC( QT_QBYTEARRAY_CAPACITY )
{
   hb_retni( hbqt_par_QByteArray( 1 )->capacity() );
}

/*
 * void chop ( int n )
 */
HB_FUNC( QT_QBYTEARRAY_CHOP )
{
   hbqt_par_QByteArray( 1 )->chop( hb_parni( 2 ) );
}

/*
 * void clear ()
 */
HB_FUNC( QT_QBYTEARRAY_CLEAR )
{
   hbqt_par_QByteArray( 1 )->clear();
}

/*
 * const char * constData () const
 */
HB_FUNC( QT_QBYTEARRAY_CONSTDATA )
{
   hb_retc( hbqt_par_QByteArray( 1 )->constData() );
}

/*
 * int count ( const QByteArray & ba ) const
 */
HB_FUNC( QT_QBYTEARRAY_COUNT )
{
   hb_retni( hbqt_par_QByteArray( 1 )->count( *hbqt_par_QByteArray( 2 ) ) );
}

/*
 * int count ( const char * str ) const
 */
HB_FUNC( QT_QBYTEARRAY_COUNT_1 )
{
   hb_retni( hbqt_par_QByteArray( 1 )->count( hbqt_par_char( 2 ) ) );
}

/*
 * int count ( char ch ) const
 */
HB_FUNC( QT_QBYTEARRAY_COUNT_2 )
{
   hb_retni( hbqt_par_QByteArray( 1 )->count( ( char ) hb_parni( 2 ) ) );
}

/*
 * int count () const
 */
HB_FUNC( QT_QBYTEARRAY_COUNT_3 )
{
   hb_retni( hbqt_par_QByteArray( 1 )->count() );
}

/*
 * char * data ()
 */
HB_FUNC( QT_QBYTEARRAY_DATA )
{
   hb_retc( hbqt_par_QByteArray( 1 )->data() );
}

/*
 * const char * data () const
 */
HB_FUNC( QT_QBYTEARRAY_DATA_1 )
{
   hb_retc( hbqt_par_QByteArray( 1 )->data() );
}

/*
 * bool endsWith ( const QByteArray & ba ) const
 */
HB_FUNC( QT_QBYTEARRAY_ENDSWITH )
{
   hb_retl( hbqt_par_QByteArray( 1 )->endsWith( *hbqt_par_QByteArray( 2 ) ) );
}

/*
 * bool endsWith ( const char * str ) const
 */
HB_FUNC( QT_QBYTEARRAY_ENDSWITH_1 )
{
   hb_retl( hbqt_par_QByteArray( 1 )->endsWith( hbqt_par_char( 2 ) ) );
}

/*
 * bool endsWith ( char ch ) const
 */
HB_FUNC( QT_QBYTEARRAY_ENDSWITH_2 )
{
   hb_retl( hbqt_par_QByteArray( 1 )->endsWith( ( char ) hb_parni( 2 ) ) );
}

/*
 * QByteArray & fill ( char ch, int size = -1 )
 */
HB_FUNC( QT_QBYTEARRAY_FILL )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->fill( ( char ) hb_parni( 2 ), ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : -1 ) ) ) ) );
}

/*
 * int indexOf ( const QByteArray & ba, int from = 0 ) const
 */
HB_FUNC( QT_QBYTEARRAY_INDEXOF )
{
   hb_retni( hbqt_par_QByteArray( 1 )->indexOf( *hbqt_par_QByteArray( 2 ), hb_parni( 3 ) ) );
}

/*
 * int indexOf ( const QString & str, int from = 0 ) const
 */
HB_FUNC( QT_QBYTEARRAY_INDEXOF_1 )
{
   hb_retni( hbqt_par_QByteArray( 1 )->indexOf( hbqt_par_QString( 2 ), hb_parni( 3 ) ) );
}

/*
 * int indexOf ( const char * str, int from = 0 ) const
 */
HB_FUNC( QT_QBYTEARRAY_INDEXOF_2 )
{
   hb_retni( hbqt_par_QByteArray( 1 )->indexOf( hbqt_par_char( 2 ), hb_parni( 3 ) ) );
}

/*
 * int indexOf ( char ch, int from = 0 ) const
 */
HB_FUNC( QT_QBYTEARRAY_INDEXOF_3 )
{
   hb_retni( hbqt_par_QByteArray( 1 )->indexOf( ( char ) hb_parni( 2 ), hb_parni( 3 ) ) );
}

/*
 * QByteArray & insert ( int i, const QByteArray & ba )
 */
HB_FUNC( QT_QBYTEARRAY_INSERT )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->insert( hb_parni( 2 ), *hbqt_par_QByteArray( 3 ) ) ) ) );
}

/*
 * QByteArray & insert ( int i, const QString & str )
 */
HB_FUNC( QT_QBYTEARRAY_INSERT_1 )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->insert( hb_parni( 2 ), hbqt_par_QString( 3 ) ) ) ) );
}

/*
 * QByteArray & insert ( int i, const char * str )
 */
HB_FUNC( QT_QBYTEARRAY_INSERT_2 )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->insert( hb_parni( 2 ), hbqt_par_char( 3 ) ) ) ) );
}

/*
 * QByteArray & insert ( int i, char ch )
 */
HB_FUNC( QT_QBYTEARRAY_INSERT_3 )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->insert( hb_parni( 2 ), ( char ) hb_parni( 3 ) ) ) ) );
}

/*
 * bool isEmpty () const
 */
HB_FUNC( QT_QBYTEARRAY_ISEMPTY )
{
   hb_retl( hbqt_par_QByteArray( 1 )->isEmpty() );
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QBYTEARRAY_ISNULL )
{
   hb_retl( hbqt_par_QByteArray( 1 )->isNull() );
}

/*
 * int lastIndexOf ( const QByteArray & ba, int from = -1 ) const
 */
HB_FUNC( QT_QBYTEARRAY_LASTINDEXOF )
{
   hb_retni( hbqt_par_QByteArray( 1 )->lastIndexOf( *hbqt_par_QByteArray( 2 ), ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : -1 ) ) );
}

/*
 * int lastIndexOf ( const QString & str, int from = -1 ) const
 */
HB_FUNC( QT_QBYTEARRAY_LASTINDEXOF_1 )
{
   hb_retni( hbqt_par_QByteArray( 1 )->lastIndexOf( hbqt_par_QString( 2 ), ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : -1 ) ) );
}

/*
 * int lastIndexOf ( const char * str, int from = -1 ) const
 */
HB_FUNC( QT_QBYTEARRAY_LASTINDEXOF_2 )
{
   hb_retni( hbqt_par_QByteArray( 1 )->lastIndexOf( hbqt_par_char( 2 ), ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : -1 ) ) );
}

/*
 * int lastIndexOf ( char ch, int from = -1 ) const
 */
HB_FUNC( QT_QBYTEARRAY_LASTINDEXOF_3 )
{
   hb_retni( hbqt_par_QByteArray( 1 )->lastIndexOf( ( char ) hb_parni( 2 ), ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : -1 ) ) );
}

/*
 * QByteArray left ( int len ) const
 */
HB_FUNC( QT_QBYTEARRAY_LEFT )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->left( hb_parni( 2 ) ) ) ) );
}

/*
 * QByteArray leftJustified ( int width, char fill = ' ', bool truncate = false ) const
 */
HB_FUNC( QT_QBYTEARRAY_LEFTJUSTIFIED )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->leftJustified( hb_parni( 2 ), ( char ) hb_parni( 3 ), hb_parl( 4 ) ) ) ) );
}

/*
 * int length () const
 */
HB_FUNC( QT_QBYTEARRAY_LENGTH )
{
   hb_retni( hbqt_par_QByteArray( 1 )->length() );
}

/*
 * QByteArray mid ( int pos, int len = -1 ) const
 */
HB_FUNC( QT_QBYTEARRAY_MID )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->mid( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : -1 ) ) ) ) );
}

/*
 * QByteArray & prepend ( const QByteArray & ba )
 */
HB_FUNC( QT_QBYTEARRAY_PREPEND )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->prepend( *hbqt_par_QByteArray( 2 ) ) ) ) );
}

/*
 * QByteArray & prepend ( const char * str )
 */
HB_FUNC( QT_QBYTEARRAY_PREPEND_1 )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->prepend( hbqt_par_char( 2 ) ) ) ) );
}

/*
 * QByteArray & prepend ( char ch )
 */
HB_FUNC( QT_QBYTEARRAY_PREPEND_2 )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->prepend( ( char ) hb_parni( 2 ) ) ) ) );
}

/*
 * void push_back ( const QByteArray & other )
 */
HB_FUNC( QT_QBYTEARRAY_PUSH_BACK )
{
   hbqt_par_QByteArray( 1 )->push_back( *hbqt_par_QByteArray( 2 ) );
}

/*
 * void push_back ( const char * str )
 */
HB_FUNC( QT_QBYTEARRAY_PUSH_BACK_1 )
{
   hbqt_par_QByteArray( 1 )->push_back( hbqt_par_char( 2 ) );
}

/*
 * void push_back ( char ch )
 */
HB_FUNC( QT_QBYTEARRAY_PUSH_BACK_2 )
{
   hbqt_par_QByteArray( 1 )->push_back( ( char ) hb_parni( 2 ) );
}

/*
 * void push_front ( const QByteArray & other )
 */
HB_FUNC( QT_QBYTEARRAY_PUSH_FRONT )
{
   hbqt_par_QByteArray( 1 )->push_front( *hbqt_par_QByteArray( 2 ) );
}

/*
 * void push_front ( const char * str )
 */
HB_FUNC( QT_QBYTEARRAY_PUSH_FRONT_1 )
{
   hbqt_par_QByteArray( 1 )->push_front( hbqt_par_char( 2 ) );
}

/*
 * void push_front ( char ch )
 */
HB_FUNC( QT_QBYTEARRAY_PUSH_FRONT_2 )
{
   hbqt_par_QByteArray( 1 )->push_front( ( char ) hb_parni( 2 ) );
}

/*
 * QByteArray & remove ( int pos, int len )
 */
HB_FUNC( QT_QBYTEARRAY_REMOVE )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->remove( hb_parni( 2 ), hb_parni( 3 ) ) ) ) );
}

/*
 * QByteArray repeated ( int times ) const
 */
HB_FUNC( QT_QBYTEARRAY_REPEATED )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->repeated( hb_parni( 2 ) ) ) ) );
}

/*
 * QByteArray & replace ( int pos, int len, const QByteArray & after )
 */
HB_FUNC( QT_QBYTEARRAY_REPLACE )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->replace( hb_parni( 2 ), hb_parni( 3 ), *hbqt_par_QByteArray( 4 ) ) ) ) );
}

/*
 * QByteArray & replace ( int pos, int len, const char * after )
 */
HB_FUNC( QT_QBYTEARRAY_REPLACE_1 )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->replace( hb_parni( 2 ), hb_parni( 3 ), hbqt_par_char( 4 ) ) ) ) );
}

/*
 * QByteArray & replace ( const QByteArray & before, const QByteArray & after )
 */
HB_FUNC( QT_QBYTEARRAY_REPLACE_2 )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->replace( *hbqt_par_QByteArray( 2 ), *hbqt_par_QByteArray( 3 ) ) ) ) );
}

/*
 * QByteArray & replace ( const char * before, const QByteArray & after )
 */
HB_FUNC( QT_QBYTEARRAY_REPLACE_3 )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->replace( hbqt_par_char( 2 ), *hbqt_par_QByteArray( 3 ) ) ) ) );
}

/*
 * QByteArray & replace ( const char * before, int bsize, const char * after, int asize )
 */
HB_FUNC( QT_QBYTEARRAY_REPLACE_4 )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->replace( hbqt_par_char( 2 ), hb_parni( 3 ), hbqt_par_char( 4 ), hb_parni( 5 ) ) ) ) );
}

/*
 * QByteArray & replace ( const QByteArray & before, const char * after )
 */
HB_FUNC( QT_QBYTEARRAY_REPLACE_5 )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->replace( *hbqt_par_QByteArray( 2 ), hbqt_par_char( 3 ) ) ) ) );
}

/*
 * QByteArray & replace ( const QString & before, const QByteArray & after )
 */
HB_FUNC( QT_QBYTEARRAY_REPLACE_6 )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->replace( hbqt_par_QString( 2 ), *hbqt_par_QByteArray( 3 ) ) ) ) );
}

/*
 * QByteArray & replace ( const QString & before, const char * after )
 */
HB_FUNC( QT_QBYTEARRAY_REPLACE_7 )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->replace( hbqt_par_QString( 2 ), hbqt_par_char( 3 ) ) ) ) );
}

/*
 * QByteArray & replace ( const char * before, const char * after )
 */
HB_FUNC( QT_QBYTEARRAY_REPLACE_8 )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->replace( hbqt_par_char( 2 ), hbqt_par_char( 3 ) ) ) ) );
}

/*
 * QByteArray & replace ( char before, const QByteArray & after )
 */
HB_FUNC( QT_QBYTEARRAY_REPLACE_9 )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->replace( ( char ) hb_parni( 2 ), *hbqt_par_QByteArray( 3 ) ) ) ) );
}

/*
 * QByteArray & replace ( char before, const QString & after )
 */
HB_FUNC( QT_QBYTEARRAY_REPLACE_10 )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->replace( ( char ) hb_parni( 2 ), hbqt_par_QString( 3 ) ) ) ) );
}

/*
 * QByteArray & replace ( char before, const char * after )
 */
HB_FUNC( QT_QBYTEARRAY_REPLACE_11 )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->replace( ( char ) hb_parni( 2 ), hbqt_par_char( 3 ) ) ) ) );
}

/*
 * QByteArray & replace ( char before, char after )
 */
HB_FUNC( QT_QBYTEARRAY_REPLACE_12 )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->replace( ( char ) hb_parni( 2 ), ( char ) hb_parni( 3 ) ) ) ) );
}

/*
 * void reserve ( int size )
 */
HB_FUNC( QT_QBYTEARRAY_RESERVE )
{
   hbqt_par_QByteArray( 1 )->reserve( hb_parni( 2 ) );
}

/*
 * void resize ( int size )
 */
HB_FUNC( QT_QBYTEARRAY_RESIZE )
{
   hbqt_par_QByteArray( 1 )->resize( hb_parni( 2 ) );
}

/*
 * QByteArray right ( int len ) const
 */
HB_FUNC( QT_QBYTEARRAY_RIGHT )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->right( hb_parni( 2 ) ) ) ) );
}

/*
 * QByteArray rightJustified ( int width, char fill = ' ', bool truncate = false ) const
 */
HB_FUNC( QT_QBYTEARRAY_RIGHTJUSTIFIED )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->rightJustified( hb_parni( 2 ), ( char ) hb_parni( 3 ), hb_parl( 4 ) ) ) ) );
}

/*
 * QByteArray & setNum ( int n, int base = 10 )
 */
HB_FUNC( QT_QBYTEARRAY_SETNUM )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->setNum( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : 10 ) ) ) ) );
}

/*
 * QByteArray & setNum ( uint n, int base = 10 )
 */
HB_FUNC( QT_QBYTEARRAY_SETNUM_1 )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->setNum( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : 10 ) ) ) ) );
}

/*
 * QByteArray & setNum ( short n, int base = 10 )
 */
HB_FUNC( QT_QBYTEARRAY_SETNUM_2 )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->setNum( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : 10 ) ) ) ) );
}

/*
 * QByteArray & setNum ( ushort n, int base = 10 )
 */
HB_FUNC( QT_QBYTEARRAY_SETNUM_3 )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->setNum( hb_parni( 2 ), ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : 10 ) ) ) ) );
}

/*
 * QByteArray & setNum ( qlonglong n, int base = 10 )
 */
HB_FUNC( QT_QBYTEARRAY_SETNUM_4 )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->setNum( ( qlonglong ) hb_parnint( 2 ), ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : 10 ) ) ) ) );
}

/*
 * QByteArray & setNum ( qulonglong n, int base = 10 )
 */
HB_FUNC( QT_QBYTEARRAY_SETNUM_5 )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->setNum( ( qulonglong ) hb_parnint( 2 ), ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : 10 ) ) ) ) );
}

/*
 * QByteArray & setNum ( double n, char f = 'g', int prec = 6 )
 */
HB_FUNC( QT_QBYTEARRAY_SETNUM_6 )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->setNum( hb_parnd( 2 ), ( char ) hb_parni( 3 ), ( HB_ISNUM( 4 ) ? hb_parni( 4 ) : 6 ) ) ) ) );
}

/*
 * QByteArray & setNum ( float n, char f = 'g', int prec = 6 )
 */
HB_FUNC( QT_QBYTEARRAY_SETNUM_7 )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->setNum( hb_parnd( 2 ), ( char ) hb_parni( 3 ), ( HB_ISNUM( 4 ) ? hb_parni( 4 ) : 6 ) ) ) ) );
}

/*
 * QByteArray simplified () const
 */
HB_FUNC( QT_QBYTEARRAY_SIMPLIFIED )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->simplified() ) ) );
}

/*
 * int size () const
 */
HB_FUNC( QT_QBYTEARRAY_SIZE )
{
   hb_retni( hbqt_par_QByteArray( 1 )->size() );
}

/*
 * void squeeze ()
 */
HB_FUNC( QT_QBYTEARRAY_SQUEEZE )
{
   hbqt_par_QByteArray( 1 )->squeeze();
}

/*
 * bool startsWith ( const QByteArray & ba ) const
 */
HB_FUNC( QT_QBYTEARRAY_STARTSWITH )
{
   hb_retl( hbqt_par_QByteArray( 1 )->startsWith( *hbqt_par_QByteArray( 2 ) ) );
}

/*
 * bool startsWith ( const char * str ) const
 */
HB_FUNC( QT_QBYTEARRAY_STARTSWITH_1 )
{
   hb_retl( hbqt_par_QByteArray( 1 )->startsWith( hbqt_par_char( 2 ) ) );
}

/*
 * bool startsWith ( char ch ) const
 */
HB_FUNC( QT_QBYTEARRAY_STARTSWITH_2 )
{
   hb_retl( hbqt_par_QByteArray( 1 )->startsWith( ( char ) hb_parni( 2 ) ) );
}

/*
 * QByteArray toBase64 () const
 */
HB_FUNC( QT_QBYTEARRAY_TOBASE64 )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->toBase64() ) ) );
}

/*
 * double toDouble ( bool * ok = 0 ) const
 */
HB_FUNC( QT_QBYTEARRAY_TODOUBLE )
{
   bool iOk = 0;

   hb_retnd( hbqt_par_QByteArray( 1 )->toDouble( &iOk ) );

   hb_stornl( iOk, 2 );
}

/*
 * float toFloat ( bool * ok = 0 ) const
 */
HB_FUNC( QT_QBYTEARRAY_TOFLOAT )
{
   bool iOk = 0;

   hb_retnd( hbqt_par_QByteArray( 1 )->toFloat( &iOk ) );

   hb_stornl( iOk, 2 );
}

/*
 * QByteArray toHex () const
 */
HB_FUNC( QT_QBYTEARRAY_TOHEX )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->toHex() ) ) );
}

/*
 * int toInt ( bool * ok = 0, int base = 10 ) const
 */
HB_FUNC( QT_QBYTEARRAY_TOINT )
{
   bool iOk = 0;

   hb_retni( hbqt_par_QByteArray( 1 )->toInt( &iOk, ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : 10 ) ) );

   hb_stornl( iOk, 2 );
}

/*
 * long toLong ( bool * ok = 0, int base = 10 ) const
 */
HB_FUNC( QT_QBYTEARRAY_TOLONG )
{
   bool iOk = 0;

   hb_retnint( hbqt_par_QByteArray( 1 )->toLong( &iOk, ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : 10 ) ) );

   hb_stornl( iOk, 2 );
}

/*
 * qlonglong toLongLong ( bool * ok = 0, int base = 10 ) const
 */
HB_FUNC( QT_QBYTEARRAY_TOLONGLONG )
{
   bool iOk = 0;

   hb_retnint( hbqt_par_QByteArray( 1 )->toLongLong( &iOk, ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : 10 ) ) );

   hb_stornl( iOk, 2 );
}

/*
 * QByteArray toLower () const
 */
HB_FUNC( QT_QBYTEARRAY_TOLOWER )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->toLower() ) ) );
}

/*
 * QByteArray toPercentEncoding ( const QByteArray & exclude = QByteArray(), const QByteArray & include = QByteArray(), char percent = '%' ) const
 */
HB_FUNC( QT_QBYTEARRAY_TOPERCENTENCODING )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->toPercentEncoding( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QByteArray( 2 ) : QByteArray() ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QByteArray( 3 ) : QByteArray() ), ( char ) hb_parni( 4 ) ) ) ) );
}

/*
 * short toShort ( bool * ok = 0, int base = 10 ) const
 */
HB_FUNC( QT_QBYTEARRAY_TOSHORT )
{
   bool iOk = 0;

   hb_retni( hbqt_par_QByteArray( 1 )->toShort( &iOk, ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : 10 ) ) );

   hb_stornl( iOk, 2 );
}

/*
 * uint toUInt ( bool * ok = 0, int base = 10 ) const
 */
HB_FUNC( QT_QBYTEARRAY_TOUINT )
{
   bool iOk = 0;

   hb_retni( hbqt_par_QByteArray( 1 )->toUInt( &iOk, ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : 10 ) ) );

   hb_stornl( iOk, 2 );
}

/*
 * ulong toULong ( bool * ok = 0, int base = 10 ) const
 */
HB_FUNC( QT_QBYTEARRAY_TOULONG )
{
   bool iOk = 0;

   hb_retnint( hbqt_par_QByteArray( 1 )->toULong( &iOk, ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : 10 ) ) );

   hb_stornl( iOk, 2 );
}

/*
 * qulonglong toULongLong ( bool * ok = 0, int base = 10 ) const
 */
HB_FUNC( QT_QBYTEARRAY_TOULONGLONG )
{
   bool iOk = 0;

   hb_retnint( hbqt_par_QByteArray( 1 )->toULongLong( &iOk, ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : 10 ) ) );

   hb_stornl( iOk, 2 );
}

/*
 * ushort toUShort ( bool * ok = 0, int base = 10 ) const
 */
HB_FUNC( QT_QBYTEARRAY_TOUSHORT )
{
   bool iOk = 0;

   hb_retni( hbqt_par_QByteArray( 1 )->toUShort( &iOk, ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : 10 ) ) );

   hb_stornl( iOk, 2 );
}

/*
 * QByteArray toUpper () const
 */
HB_FUNC( QT_QBYTEARRAY_TOUPPER )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->toUpper() ) ) );
}

/*
 * QByteArray trimmed () const
 */
HB_FUNC( QT_QBYTEARRAY_TRIMMED )
{
   hb_retptrGC( gcAllocate_QByteArray( new QByteArray( hbqt_par_QByteArray( 1 )->trimmed() ) ) );
}

/*
 * void truncate ( int pos )
 */
HB_FUNC( QT_QBYTEARRAY_TRUNCATE )
{
   hbqt_par_QByteArray( 1 )->truncate( hb_parni( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
