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
 *  Constructed[ 40/55 [ 72.73% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  void append ( const QList<T> & value )
 *  iterator begin ()
 *  iterator erase ( iterator pos )
 *  iterator erase ( iterator begin, iterator end )
 *  iterator insert ( iterator before, const T & value )
 *  QList<T> mid ( int pos, int length = -1 ) const
 *  QSet<T> toSet () const
 *  std::list<T> toStdList () const
 *  QVector<T> toVector () const
 *
 *  *** Commented out protos which construct fine but do not compile ***
 *
 *  //const_iterator begin () const
 *  //const_iterator constBegin () const
 *  //const_iterator constEnd () const
 *  //bool contains ( const T & value ) const
 *  //iterator end ()
 *  //const_iterator end () const
 */

#include <QtCore/QPointer>

#include <QtCore/QList>


/*
 * QList ()
 * QList ( const QList<T> & other )
 * ~QList ()
 */

QT_G_FUNC( release_QList )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QList                        p=%p", p ) );
   HB_TRACE( HB_TR_DEBUG, ( "release_QList                       ph=%p", p->ph ) );

   if( p && p->ph )
   {
      ( ( QList< void * > * ) p->ph )->~QList();
      p->ph = NULL;
      HB_TRACE( HB_TR_DEBUG, ( "release_QList                       Object deleted!" ) );
      #if defined(__debug__)
         just_debug( "  YES release_QList                       %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );
      #endif
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "release_QList                       Object Allready deleted!" ) );
      #if defined(__debug__)
         just_debug( "  DEL release_QList" );
      #endif
   }
}

void * gcAllocate_QList( void * pObj )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QList;
   #if defined(__debug__)
      just_debug( "          new_QList                       %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );
   #endif
   return( p );
}

HB_FUNC( QT_QLIST )
{
   void * pObj = NULL;

   QList<void*>* list = NULL;
   pObj = ( QList<void*>* ) list ;

   hb_retptrGC( gcAllocate_QList( pObj ) );
}
/*
 * void append ( const T & value )
 */
HB_FUNC( QT_QLIST_APPEND )
{
   hbqt_par_QList( 1 )->append( hb_param( 2, HB_IT_ANY ) );
}

/*
 * const T & at ( int i ) const
 */
HB_FUNC( QT_QLIST_AT )
{
   hb_retptr( hbqt_par_QList( 1 )->at( hb_parni( 2 ) ) );
}

/*
 * T & back ()
 */
HB_FUNC( QT_QLIST_BACK )
{
   hb_retptr( hbqt_par_QList( 1 )->back() );
}

/*
 * const T & back () const
 */
HB_FUNC( QT_QLIST_BACK_1 )
{
   hb_retptr( hbqt_par_QList( 1 )->back() );
}

/*
 * void clear ()
 */
HB_FUNC( QT_QLIST_CLEAR )
{
   hbqt_par_QList( 1 )->clear();
}

/*
 * int count ( const T & value ) const
 */
HB_FUNC( QT_QLIST_COUNT )
{
   hb_retni( hbqt_par_QList( 1 )->count( hb_param( 2, HB_IT_ANY ) ) );
}

/*
 * int count () const
 */
HB_FUNC( QT_QLIST_COUNT_1 )
{
   hb_retni( hbqt_par_QList( 1 )->count() );
}

/*
 * bool empty () const
 */
HB_FUNC( QT_QLIST_EMPTY )
{
   hb_retl( hbqt_par_QList( 1 )->empty() );
}

/*
 * bool endsWith ( const T & value ) const
 */
HB_FUNC( QT_QLIST_ENDSWITH )
{
   hb_retl( hbqt_par_QList( 1 )->endsWith( hb_param( 2, HB_IT_ANY ) ) );
}

/*
 * T & first ()
 */
HB_FUNC( QT_QLIST_FIRST )
{
   hb_retptr( hbqt_par_QList( 1 )->first() );
}

/*
 * const T & first () const
 */
HB_FUNC( QT_QLIST_FIRST_1 )
{
   hb_retptr( hbqt_par_QList( 1 )->first() );
}

/*
 * T & front ()
 */
HB_FUNC( QT_QLIST_FRONT )
{
   hb_retptr( hbqt_par_QList( 1 )->front() );
}

/*
 * const T & front () const
 */
HB_FUNC( QT_QLIST_FRONT_1 )
{
   hb_retptr( hbqt_par_QList( 1 )->front() );
}

/*
 * int indexOf ( const T & value, int from = 0 ) const
 */
HB_FUNC( QT_QLIST_INDEXOF )
{
   hb_retni( hbqt_par_QList( 1 )->indexOf( hb_param( 2, HB_IT_ANY ), hb_parni( 3 ) ) );
}

/*
 * void insert ( int i, const T & value )
 */
HB_FUNC( QT_QLIST_INSERT )
{
   hbqt_par_QList( 1 )->insert( hb_parni( 2 ), hb_param( 3, HB_IT_ANY ) );
}

/*
 * bool isEmpty () const
 */
HB_FUNC( QT_QLIST_ISEMPTY )
{
   hb_retl( hbqt_par_QList( 1 )->isEmpty() );
}

/*
 * T & last ()
 */
HB_FUNC( QT_QLIST_LAST )
{
   hb_retptr( hbqt_par_QList( 1 )->last() );
}

/*
 * const T & last () const
 */
HB_FUNC( QT_QLIST_LAST_1 )
{
   hb_retptr( hbqt_par_QList( 1 )->last() );
}

/*
 * int lastIndexOf ( const T & value, int from = -1 ) const
 */
HB_FUNC( QT_QLIST_LASTINDEXOF )
{
   hb_retni( hbqt_par_QList( 1 )->lastIndexOf( hb_param( 2, HB_IT_ANY ), ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : -1 ) ) );
}

/*
 * int length () const
 */
HB_FUNC( QT_QLIST_LENGTH )
{
   hb_retni( hbqt_par_QList( 1 )->length() );
}

/*
 * void move ( int from, int to )
 */
HB_FUNC( QT_QLIST_MOVE )
{
   hbqt_par_QList( 1 )->move( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * void pop_back ()
 */
HB_FUNC( QT_QLIST_POP_BACK )
{
   hbqt_par_QList( 1 )->pop_back();
}

/*
 * void pop_front ()
 */
HB_FUNC( QT_QLIST_POP_FRONT )
{
   hbqt_par_QList( 1 )->pop_front();
}

/*
 * void prepend ( const T & value )
 */
HB_FUNC( QT_QLIST_PREPEND )
{
   hbqt_par_QList( 1 )->prepend( hb_param( 2, HB_IT_ANY ) );
}

/*
 * void push_back ( const T & value )
 */
HB_FUNC( QT_QLIST_PUSH_BACK )
{
   hbqt_par_QList( 1 )->push_back( hb_param( 2, HB_IT_ANY ) );
}

/*
 * void push_front ( const T & value )
 */
HB_FUNC( QT_QLIST_PUSH_FRONT )
{
   hbqt_par_QList( 1 )->push_front( hb_param( 2, HB_IT_ANY ) );
}

/*
 * int removeAll ( const T & value )
 */
HB_FUNC( QT_QLIST_REMOVEALL )
{
   hb_retni( hbqt_par_QList( 1 )->removeAll( hb_param( 2, HB_IT_ANY ) ) );
}

/*
 * void removeAt ( int i )
 */
HB_FUNC( QT_QLIST_REMOVEAT )
{
   hbqt_par_QList( 1 )->removeAt( hb_parni( 2 ) );
}

/*
 * void removeFirst ()
 */
HB_FUNC( QT_QLIST_REMOVEFIRST )
{
   hbqt_par_QList( 1 )->removeFirst();
}

/*
 * void removeLast ()
 */
HB_FUNC( QT_QLIST_REMOVELAST )
{
   hbqt_par_QList( 1 )->removeLast();
}

/*
 * bool removeOne ( const T & value )
 */
HB_FUNC( QT_QLIST_REMOVEONE )
{
   hb_retl( hbqt_par_QList( 1 )->removeOne( hb_param( 2, HB_IT_ANY ) ) );
}

/*
 * void replace ( int i, const T & value )
 */
HB_FUNC( QT_QLIST_REPLACE )
{
   hbqt_par_QList( 1 )->replace( hb_parni( 2 ), hb_param( 3, HB_IT_ANY ) );
}

/*
 * int size () const
 */
HB_FUNC( QT_QLIST_SIZE )
{
   hb_retni( hbqt_par_QList( 1 )->size() );
}

/*
 * bool startsWith ( const T & value ) const
 */
HB_FUNC( QT_QLIST_STARTSWITH )
{
   hb_retl( hbqt_par_QList( 1 )->startsWith( hb_param( 2, HB_IT_ANY ) ) );
}

/*
 * void swap ( int i, int j )
 */
HB_FUNC( QT_QLIST_SWAP )
{
   hbqt_par_QList( 1 )->swap( hb_parni( 2 ), hb_parni( 3 ) );
}

/*
 * T takeAt ( int i )
 */
HB_FUNC( QT_QLIST_TAKEAT )
{
   hb_retptr( hbqt_par_QList( 1 )->takeAt( hb_parni( 2 ) ) );
}

/*
 * T takeFirst ()
 */
HB_FUNC( QT_QLIST_TAKEFIRST )
{
   hb_retptr( hbqt_par_QList( 1 )->takeFirst() );
}

/*
 * T takeLast ()
 */
HB_FUNC( QT_QLIST_TAKELAST )
{
   hb_retptr( hbqt_par_QList( 1 )->takeLast() );
}

/*
 * T value ( int i ) const
 */
HB_FUNC( QT_QLIST_VALUE )
{
   hb_retptr( hbqt_par_QList( 1 )->value( hb_parni( 2 ) ) );
}

/*
 * T value ( int i, const T & defaultValue ) const
 */
HB_FUNC( QT_QLIST_VALUE_1 )
{
   hb_retptr( hbqt_par_QList( 1 )->value( hb_parni( 2 ), hb_param( 3, HB_IT_ANY ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
