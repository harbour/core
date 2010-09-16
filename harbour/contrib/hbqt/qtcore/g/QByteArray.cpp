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

#include <QtCore/QPointer>

#include <QtCore/QByteArray>


/* QByteArray ()
 * QByteArray ( const char * str )
 * QByteArray ( const char * data, int size )
 * QByteArray ( int size, char ch )
 * QByteArray ( const QByteArray & other )
 * ~QByteArray ()
 */

typedef struct
{
   QByteArray * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QByteArray;

HBQT_GC_FUNC( hbqt_gcRelease_QByteArray )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QByteArray   /.\\", p->ph ) );
         delete ( ( QByteArray * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QByteArray   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QByteArray    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QByteArray    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QByteArray( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QByteArray * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QByteArray;
   p->type = HBQT_TYPE_QByteArray;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QByteArray", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QByteArray", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QBYTEARRAY )
{
   QByteArray * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISCHAR( 1 ) )
   {
      pObj = new QByteArray( hb_parc( 1 ) ) ;
   }
   else
   {
      pObj = new QByteArray() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QByteArray( ( void * ) pObj, true ) );
}

/*
 * QByteArray & append ( const QByteArray & ba )
 */
HB_FUNC( QT_QBYTEARRAY_APPEND )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->append( *hbqt_par_QByteArray( 2 ) ) ), true ) );
   }
}

/*
 * QByteArray & append ( const QString & str )
 */
HB_FUNC( QT_QBYTEARRAY_APPEND_1 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->append( hb_parstr_utf8( 2, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/*
 * QByteArray & append ( const char * str )
 */
HB_FUNC( QT_QBYTEARRAY_APPEND_2 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->append( hbqt_par_char( 2 ) ) ), true ) );
   }
}

/*
 * QByteArray & append ( const char * str, int len )
 */
HB_FUNC( QT_QBYTEARRAY_APPEND_3 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->append( hbqt_par_char( 2 ), hb_parni( 3 ) ) ), true ) );
   }
}

/*
 * QByteArray & append ( char ch )
 */
HB_FUNC( QT_QBYTEARRAY_APPEND_4 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->append( ( char ) hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * char at ( int i ) const
 */
HB_FUNC( QT_QBYTEARRAY_AT )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retni( ( p )->at( hb_parni( 2 ) ) );
   }
}

/*
 * int capacity () const
 */
HB_FUNC( QT_QBYTEARRAY_CAPACITY )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retni( ( p )->capacity() );
   }
}

/*
 * void chop ( int n )
 */
HB_FUNC( QT_QBYTEARRAY_CHOP )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      ( p )->chop( hb_parni( 2 ) );
   }
}

/*
 * void clear ()
 */
HB_FUNC( QT_QBYTEARRAY_CLEAR )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      ( p )->clear();
   }
}

/*
 * const char * constData () const
 */
HB_FUNC( QT_QBYTEARRAY_CONSTDATA )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retc( ( p )->constData() );
   }
}

/*
 * int count ( const QByteArray & ba ) const
 */
HB_FUNC( QT_QBYTEARRAY_COUNT )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retni( ( p )->count( *hbqt_par_QByteArray( 2 ) ) );
   }
}

/*
 * int count ( const char * str ) const
 */
HB_FUNC( QT_QBYTEARRAY_COUNT_1 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retni( ( p )->count( hbqt_par_char( 2 ) ) );
   }
}

/*
 * int count ( char ch ) const
 */
HB_FUNC( QT_QBYTEARRAY_COUNT_2 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retni( ( p )->count( ( char ) hb_parni( 2 ) ) );
   }
}

/*
 * int count () const
 */
HB_FUNC( QT_QBYTEARRAY_COUNT_3 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retni( ( p )->count() );
   }
}

/*
 * char * data ()
 */
HB_FUNC( QT_QBYTEARRAY_DATA )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retc( ( p )->data() );
   }
}

/*
 * const char * data () const
 */
HB_FUNC( QT_QBYTEARRAY_DATA_1 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retc( ( p )->data() );
   }
}

/*
 * bool endsWith ( const QByteArray & ba ) const
 */
HB_FUNC( QT_QBYTEARRAY_ENDSWITH )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retl( ( p )->endsWith( *hbqt_par_QByteArray( 2 ) ) );
   }
}

/*
 * bool endsWith ( const char * str ) const
 */
HB_FUNC( QT_QBYTEARRAY_ENDSWITH_1 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retl( ( p )->endsWith( hbqt_par_char( 2 ) ) );
   }
}

/*
 * bool endsWith ( char ch ) const
 */
HB_FUNC( QT_QBYTEARRAY_ENDSWITH_2 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retl( ( p )->endsWith( ( char ) hb_parni( 2 ) ) );
   }
}

/*
 * QByteArray & fill ( char ch, int size = -1 )
 */
HB_FUNC( QT_QBYTEARRAY_FILL )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->fill( ( char ) hb_parni( 2 ), hb_parnidef( 3, -1 ) ) ), true ) );
   }
}

/*
 * int indexOf ( const QByteArray & ba, int from = 0 ) const
 */
HB_FUNC( QT_QBYTEARRAY_INDEXOF )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retni( ( p )->indexOf( *hbqt_par_QByteArray( 2 ), hb_parni( 3 ) ) );
   }
}

/*
 * int indexOf ( const QString & str, int from = 0 ) const
 */
HB_FUNC( QT_QBYTEARRAY_INDEXOF_1 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->indexOf( hb_parstr_utf8( 2, &pText, NULL ), hb_parni( 3 ) ) );
      hb_strfree( pText );
   }
}

/*
 * int indexOf ( const char * str, int from = 0 ) const
 */
HB_FUNC( QT_QBYTEARRAY_INDEXOF_2 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retni( ( p )->indexOf( hbqt_par_char( 2 ), hb_parni( 3 ) ) );
   }
}

/*
 * int indexOf ( char ch, int from = 0 ) const
 */
HB_FUNC( QT_QBYTEARRAY_INDEXOF_3 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retni( ( p )->indexOf( ( char ) hb_parni( 2 ), hb_parni( 3 ) ) );
   }
}

/*
 * QByteArray & insert ( int i, const QByteArray & ba )
 */
HB_FUNC( QT_QBYTEARRAY_INSERT )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->insert( hb_parni( 2 ), *hbqt_par_QByteArray( 3 ) ) ), true ) );
   }
}

/*
 * QByteArray & insert ( int i, const QString & str )
 */
HB_FUNC( QT_QBYTEARRAY_INSERT_1 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->insert( hb_parni( 2 ), hb_parstr_utf8( 3, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/*
 * QByteArray & insert ( int i, const char * str )
 */
HB_FUNC( QT_QBYTEARRAY_INSERT_2 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->insert( hb_parni( 2 ), hbqt_par_char( 3 ) ) ), true ) );
   }
}

/*
 * QByteArray & insert ( int i, char ch )
 */
HB_FUNC( QT_QBYTEARRAY_INSERT_3 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->insert( hb_parni( 2 ), ( char ) hb_parni( 3 ) ) ), true ) );
   }
}

/*
 * bool isEmpty () const
 */
HB_FUNC( QT_QBYTEARRAY_ISEMPTY )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retl( ( p )->isEmpty() );
   }
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QBYTEARRAY_ISNULL )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retl( ( p )->isNull() );
   }
}

/*
 * int lastIndexOf ( const QByteArray & ba, int from = -1 ) const
 */
HB_FUNC( QT_QBYTEARRAY_LASTINDEXOF )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retni( ( p )->lastIndexOf( *hbqt_par_QByteArray( 2 ), hb_parnidef( 3, -1 ) ) );
   }
}

/*
 * int lastIndexOf ( const QString & str, int from = -1 ) const
 */
HB_FUNC( QT_QBYTEARRAY_LASTINDEXOF_1 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      void * pText;
      hb_retni( ( p )->lastIndexOf( hb_parstr_utf8( 2, &pText, NULL ), hb_parnidef( 3, -1 ) ) );
      hb_strfree( pText );
   }
}

/*
 * int lastIndexOf ( const char * str, int from = -1 ) const
 */
HB_FUNC( QT_QBYTEARRAY_LASTINDEXOF_2 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retni( ( p )->lastIndexOf( hbqt_par_char( 2 ), hb_parnidef( 3, -1 ) ) );
   }
}

/*
 * int lastIndexOf ( char ch, int from = -1 ) const
 */
HB_FUNC( QT_QBYTEARRAY_LASTINDEXOF_3 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retni( ( p )->lastIndexOf( ( char ) hb_parni( 2 ), hb_parnidef( 3, -1 ) ) );
   }
}

/*
 * QByteArray left ( int len ) const
 */
HB_FUNC( QT_QBYTEARRAY_LEFT )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->left( hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * QByteArray leftJustified ( int width, char fill = ' ', bool truncate = false ) const
 */
HB_FUNC( QT_QBYTEARRAY_LEFTJUSTIFIED )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->leftJustified( hb_parni( 2 ), ( char ) hb_parni( 3 ), hb_parl( 4 ) ) ), true ) );
   }
}

/*
 * int length () const
 */
HB_FUNC( QT_QBYTEARRAY_LENGTH )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retni( ( p )->length() );
   }
}

/*
 * QByteArray mid ( int pos, int len = -1 ) const
 */
HB_FUNC( QT_QBYTEARRAY_MID )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->mid( hb_parni( 2 ), hb_parnidef( 3, -1 ) ) ), true ) );
   }
}

/*
 * QByteArray & prepend ( const QByteArray & ba )
 */
HB_FUNC( QT_QBYTEARRAY_PREPEND )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->prepend( *hbqt_par_QByteArray( 2 ) ) ), true ) );
   }
}

/*
 * QByteArray & prepend ( const char * str )
 */
HB_FUNC( QT_QBYTEARRAY_PREPEND_1 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->prepend( hbqt_par_char( 2 ) ) ), true ) );
   }
}

/*
 * QByteArray & prepend ( char ch )
 */
HB_FUNC( QT_QBYTEARRAY_PREPEND_2 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->prepend( ( char ) hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * void push_back ( const QByteArray & other )
 */
HB_FUNC( QT_QBYTEARRAY_PUSH_BACK )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      ( p )->push_back( *hbqt_par_QByteArray( 2 ) );
   }
}

/*
 * void push_back ( const char * str )
 */
HB_FUNC( QT_QBYTEARRAY_PUSH_BACK_1 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      ( p )->push_back( hbqt_par_char( 2 ) );
   }
}

/*
 * void push_back ( char ch )
 */
HB_FUNC( QT_QBYTEARRAY_PUSH_BACK_2 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      ( p )->push_back( ( char ) hb_parni( 2 ) );
   }
}

/*
 * void push_front ( const QByteArray & other )
 */
HB_FUNC( QT_QBYTEARRAY_PUSH_FRONT )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      ( p )->push_front( *hbqt_par_QByteArray( 2 ) );
   }
}

/*
 * void push_front ( const char * str )
 */
HB_FUNC( QT_QBYTEARRAY_PUSH_FRONT_1 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      ( p )->push_front( hbqt_par_char( 2 ) );
   }
}

/*
 * void push_front ( char ch )
 */
HB_FUNC( QT_QBYTEARRAY_PUSH_FRONT_2 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      ( p )->push_front( ( char ) hb_parni( 2 ) );
   }
}

/*
 * QByteArray & remove ( int pos, int len )
 */
HB_FUNC( QT_QBYTEARRAY_REMOVE )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->remove( hb_parni( 2 ), hb_parni( 3 ) ) ), true ) );
   }
}

/*
 * QByteArray repeated ( int times ) const
 */
HB_FUNC( QT_QBYTEARRAY_REPEATED )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->repeated( hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * QByteArray & replace ( int pos, int len, const QByteArray & after )
 */
HB_FUNC( QT_QBYTEARRAY_REPLACE )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->replace( hb_parni( 2 ), hb_parni( 3 ), *hbqt_par_QByteArray( 4 ) ) ), true ) );
   }
}

/*
 * QByteArray & replace ( int pos, int len, const char * after )
 */
HB_FUNC( QT_QBYTEARRAY_REPLACE_1 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->replace( hb_parni( 2 ), hb_parni( 3 ), hbqt_par_char( 4 ) ) ), true ) );
   }
}

/*
 * QByteArray & replace ( const QByteArray & before, const QByteArray & after )
 */
HB_FUNC( QT_QBYTEARRAY_REPLACE_2 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->replace( *hbqt_par_QByteArray( 2 ), *hbqt_par_QByteArray( 3 ) ) ), true ) );
   }
}

/*
 * QByteArray & replace ( const char * before, const QByteArray & after )
 */
HB_FUNC( QT_QBYTEARRAY_REPLACE_3 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->replace( hbqt_par_char( 2 ), *hbqt_par_QByteArray( 3 ) ) ), true ) );
   }
}

/*
 * QByteArray & replace ( const char * before, int bsize, const char * after, int asize )
 */
HB_FUNC( QT_QBYTEARRAY_REPLACE_4 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->replace( hbqt_par_char( 2 ), hb_parni( 3 ), hbqt_par_char( 4 ), hb_parni( 5 ) ) ), true ) );
   }
}

/*
 * QByteArray & replace ( const QByteArray & before, const char * after )
 */
HB_FUNC( QT_QBYTEARRAY_REPLACE_5 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->replace( *hbqt_par_QByteArray( 2 ), hbqt_par_char( 3 ) ) ), true ) );
   }
}

/*
 * QByteArray & replace ( const QString & before, const QByteArray & after )
 */
HB_FUNC( QT_QBYTEARRAY_REPLACE_6 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->replace( hb_parstr_utf8( 2, &pText, NULL ), *hbqt_par_QByteArray( 3 ) ) ), true ) );
      hb_strfree( pText );
   }
}

/*
 * QByteArray & replace ( const QString & before, const char * after )
 */
HB_FUNC( QT_QBYTEARRAY_REPLACE_7 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->replace( hb_parstr_utf8( 2, &pText, NULL ), hbqt_par_char( 3 ) ) ), true ) );
      hb_strfree( pText );
   }
}

/*
 * QByteArray & replace ( const char * before, const char * after )
 */
HB_FUNC( QT_QBYTEARRAY_REPLACE_8 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->replace( hbqt_par_char( 2 ), hbqt_par_char( 3 ) ) ), true ) );
   }
}

/*
 * QByteArray & replace ( char before, const QByteArray & after )
 */
HB_FUNC( QT_QBYTEARRAY_REPLACE_9 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->replace( ( char ) hb_parni( 2 ), *hbqt_par_QByteArray( 3 ) ) ), true ) );
   }
}

/*
 * QByteArray & replace ( char before, const QString & after )
 */
HB_FUNC( QT_QBYTEARRAY_REPLACE_10 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      void * pText;
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->replace( ( char ) hb_parni( 2 ), hb_parstr_utf8( 3, &pText, NULL ) ) ), true ) );
      hb_strfree( pText );
   }
}

/*
 * QByteArray & replace ( char before, const char * after )
 */
HB_FUNC( QT_QBYTEARRAY_REPLACE_11 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->replace( ( char ) hb_parni( 2 ), hbqt_par_char( 3 ) ) ), true ) );
   }
}

/*
 * QByteArray & replace ( char before, char after )
 */
HB_FUNC( QT_QBYTEARRAY_REPLACE_12 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->replace( ( char ) hb_parni( 2 ), ( char ) hb_parni( 3 ) ) ), true ) );
   }
}

/*
 * void reserve ( int size )
 */
HB_FUNC( QT_QBYTEARRAY_RESERVE )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      ( p )->reserve( hb_parni( 2 ) );
   }
}

/*
 * void resize ( int size )
 */
HB_FUNC( QT_QBYTEARRAY_RESIZE )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      ( p )->resize( hb_parni( 2 ) );
   }
}

/*
 * QByteArray right ( int len ) const
 */
HB_FUNC( QT_QBYTEARRAY_RIGHT )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->right( hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * QByteArray rightJustified ( int width, char fill = ' ', bool truncate = false ) const
 */
HB_FUNC( QT_QBYTEARRAY_RIGHTJUSTIFIED )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->rightJustified( hb_parni( 2 ), ( char ) hb_parni( 3 ), hb_parl( 4 ) ) ), true ) );
   }
}

/*
 * QByteArray & setNum ( int n, int base = 10 )
 */
HB_FUNC( QT_QBYTEARRAY_SETNUM )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->setNum( hb_parni( 2 ), hb_parnidef( 3, 10 ) ) ), true ) );
   }
}

/*
 * QByteArray & setNum ( uint n, int base = 10 )
 */
HB_FUNC( QT_QBYTEARRAY_SETNUM_1 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->setNum( hb_parni( 2 ), hb_parnidef( 3, 10 ) ) ), true ) );
   }
}

/*
 * QByteArray & setNum ( short n, int base = 10 )
 */
HB_FUNC( QT_QBYTEARRAY_SETNUM_2 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->setNum( hb_parni( 2 ), hb_parnidef( 3, 10 ) ) ), true ) );
   }
}

/*
 * QByteArray & setNum ( ushort n, int base = 10 )
 */
HB_FUNC( QT_QBYTEARRAY_SETNUM_3 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->setNum( hb_parni( 2 ), hb_parnidef( 3, 10 ) ) ), true ) );
   }
}

/*
 * QByteArray & setNum ( qlonglong n, int base = 10 )
 */
HB_FUNC( QT_QBYTEARRAY_SETNUM_4 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->setNum( ( qlonglong ) hb_parnint( 2 ), hb_parnidef( 3, 10 ) ) ), true ) );
   }
}

/*
 * QByteArray & setNum ( qulonglong n, int base = 10 )
 */
HB_FUNC( QT_QBYTEARRAY_SETNUM_5 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->setNum( ( qulonglong ) hb_parnint( 2 ), hb_parnidef( 3, 10 ) ) ), true ) );
   }
}

/*
 * QByteArray & setNum ( double n, char f = 'g', int prec = 6 )
 */
HB_FUNC( QT_QBYTEARRAY_SETNUM_6 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->setNum( hb_parnd( 2 ), ( char ) hb_parni( 3 ), hb_parnidef( 4, 6 ) ) ), true ) );
   }
}

/*
 * QByteArray & setNum ( float n, char f = 'g', int prec = 6 )
 */
HB_FUNC( QT_QBYTEARRAY_SETNUM_7 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->setNum( hb_parnd( 2 ), ( char ) hb_parni( 3 ), hb_parnidef( 4, 6 ) ) ), true ) );
   }
}

/*
 * QByteArray simplified () const
 */
HB_FUNC( QT_QBYTEARRAY_SIMPLIFIED )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->simplified() ), true ) );
   }
}

/*
 * int size () const
 */
HB_FUNC( QT_QBYTEARRAY_SIZE )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retni( ( p )->size() );
   }
}

/*
 * QList<QByteArray> split ( char sep ) const
 */
HB_FUNC( QT_QBYTEARRAY_SPLIT )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QByteArray>( ( p )->split( ( char ) hb_parni( 2 ) ) ), true ) );
   }
}

/*
 * void squeeze ()
 */
HB_FUNC( QT_QBYTEARRAY_SQUEEZE )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      ( p )->squeeze();
   }
}

/*
 * bool startsWith ( const QByteArray & ba ) const
 */
HB_FUNC( QT_QBYTEARRAY_STARTSWITH )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retl( ( p )->startsWith( *hbqt_par_QByteArray( 2 ) ) );
   }
}

/*
 * bool startsWith ( const char * str ) const
 */
HB_FUNC( QT_QBYTEARRAY_STARTSWITH_1 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retl( ( p )->startsWith( hbqt_par_char( 2 ) ) );
   }
}

/*
 * bool startsWith ( char ch ) const
 */
HB_FUNC( QT_QBYTEARRAY_STARTSWITH_2 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retl( ( p )->startsWith( ( char ) hb_parni( 2 ) ) );
   }
}

/*
 * QByteArray toBase64 () const
 */
HB_FUNC( QT_QBYTEARRAY_TOBASE64 )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->toBase64() ), true ) );
   }
}

/*
 * double toDouble ( bool * ok = 0 ) const
 */
HB_FUNC( QT_QBYTEARRAY_TODOUBLE )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
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
HB_FUNC( QT_QBYTEARRAY_TOFLOAT )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   bool iOk = 0;

   if( p )
   {
      hb_retnd( ( p )->toFloat( &iOk ) );
   }

   hb_stornl( iOk, 2 );
}

/*
 * QByteArray toHex () const
 */
HB_FUNC( QT_QBYTEARRAY_TOHEX )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->toHex() ), true ) );
   }
}

/*
 * int toInt ( bool * ok = 0, int base = 10 ) const
 */
HB_FUNC( QT_QBYTEARRAY_TOINT )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   bool iOk = 0;

   if( p )
   {
      hb_retni( ( p )->toInt( &iOk, hb_parnidef( 3, 10 ) ) );
   }

   hb_stornl( iOk, 2 );
}

/*
 * long toLong ( bool * ok = 0, int base = 10 ) const
 */
HB_FUNC( QT_QBYTEARRAY_TOLONG )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
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
HB_FUNC( QT_QBYTEARRAY_TOLONGLONG )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   bool iOk = 0;

   if( p )
   {
      hb_retnint( ( p )->toLongLong( &iOk, hb_parnidef( 3, 10 ) ) );
   }

   hb_stornl( iOk, 2 );
}

/*
 * QByteArray toLower () const
 */
HB_FUNC( QT_QBYTEARRAY_TOLOWER )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->toLower() ), true ) );
   }
}

/*
 * QByteArray toPercentEncoding ( const QByteArray & exclude = QByteArray(), const QByteArray & include = QByteArray(), char percent = '%' ) const
 */
HB_FUNC( QT_QBYTEARRAY_TOPERCENTENCODING )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->toPercentEncoding( ( HB_ISPOINTER( 2 ) ? *hbqt_par_QByteArray( 2 ) : QByteArray() ), ( HB_ISPOINTER( 3 ) ? *hbqt_par_QByteArray( 3 ) : QByteArray() ), ( char ) hb_parni( 4 ) ) ), true ) );
   }
}

/*
 * short toShort ( bool * ok = 0, int base = 10 ) const
 */
HB_FUNC( QT_QBYTEARRAY_TOSHORT )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
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
HB_FUNC( QT_QBYTEARRAY_TOUINT )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
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
HB_FUNC( QT_QBYTEARRAY_TOULONG )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
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
HB_FUNC( QT_QBYTEARRAY_TOULONGLONG )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
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
HB_FUNC( QT_QBYTEARRAY_TOUSHORT )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   bool iOk = 0;

   if( p )
   {
      hb_retni( ( p )->toUShort( &iOk, hb_parnidef( 3, 10 ) ) );
   }

   hb_stornl( iOk, 2 );
}

/*
 * QByteArray toUpper () const
 */
HB_FUNC( QT_QBYTEARRAY_TOUPPER )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->toUpper() ), true ) );
   }
}

/*
 * QByteArray trimmed () const
 */
HB_FUNC( QT_QBYTEARRAY_TRIMMED )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->trimmed() ), true ) );
   }
}

/*
 * void truncate ( int pos )
 */
HB_FUNC( QT_QBYTEARRAY_TRUNCATE )
{
   QByteArray * p = hbqt_par_QByteArray( 1 );
   if( p )
   {
      ( p )->truncate( hb_parni( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
