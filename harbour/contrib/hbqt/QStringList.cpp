/*
 * $Id$
 */

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
#include "hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  Constructed[ 35/42 [ 83.33% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QList<QString> mid ( int pos, int length = -1 ) const
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


#include <QtCore/QStringList>


/*
 * QStringList ()
 * QStringList ( const QString & str )
 * QStringList ( const QStringList & other )
 * QStringList ( const QList<QString> & other )
 */
HB_FUNC( QT_QSTRINGLIST )
{
   hb_retptr( ( QStringList* ) new QStringList() );
}

/*
 * DESTRUCTOR
 */
HB_FUNC( QT_QSTRINGLIST_DESTROY )
{

}

/*
 * void append ( const QString & value )
 */
HB_FUNC( QT_QSTRINGLIST_APPEND )
{
   hbqt_par_QStringList( 1 )->append( hbqt_par_QString( 2 ) );
}

/*
 * QStringList filter ( const QString & str, Qt::CaseSensitivity cs = Qt::CaseSensitive ) const
 */
HB_FUNC( QT_QSTRINGLIST_FILTER )
{
   hb_retptr( new QStringList( hbqt_par_QStringList( 1 )->filter( hbqt_par_QString( 2 ), ( HB_ISNUM( 3 ) ? ( Qt::CaseSensitivity ) hb_parni( 3 ) : ( Qt::CaseSensitivity ) Qt::CaseSensitive ) ) ) );
}

/*
 * QStringList filter ( const QRegExp & rx ) const
 */
HB_FUNC( QT_QSTRINGLIST_FILTER_1 )
{
   hb_retptr( new QStringList( hbqt_par_QStringList( 1 )->filter( *hbqt_par_QRegExp( 2 ) ) ) );
}

/*
 * int indexOf ( const QString & value, int from = 0 ) const
 */
HB_FUNC( QT_QSTRINGLIST_INDEXOF )
{
   hb_retni( hbqt_par_QStringList( 1 )->indexOf( hbqt_par_QString( 2 ), hb_parni( 3 ) ) );
}

/*
 * int indexOf ( const QRegExp & rx, int from = 0 ) const
 */
HB_FUNC( QT_QSTRINGLIST_INDEXOF_1 )
{
   hb_retni( hbqt_par_QStringList( 1 )->indexOf( *hbqt_par_QRegExp( 2 ), hb_parni( 3 ) ) );
}

/*
 * int indexOf ( QRegExp & rx, int from = 0 ) const
 */
HB_FUNC( QT_QSTRINGLIST_INDEXOF_2 )
{
   hb_retni( hbqt_par_QStringList( 1 )->indexOf( *hbqt_par_QRegExp( 2 ), hb_parni( 3 ) ) );
}

/*
 * QString join ( const QString & separator ) const
 */
HB_FUNC( QT_QSTRINGLIST_JOIN )
{
   hb_retc( hbqt_par_QStringList( 1 )->join( hbqt_par_QString( 2 ) ).toAscii().data() );
}

/*
 * int lastIndexOf ( const QRegExp & rx, int from = -1 ) const
 */
HB_FUNC( QT_QSTRINGLIST_LASTINDEXOF )
{
   hb_retni( hbqt_par_QStringList( 1 )->lastIndexOf( *hbqt_par_QRegExp( 2 ), ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : -1 ) ) );
}

/*
 * int lastIndexOf ( const QString & value, int from = -1 ) const
 */
HB_FUNC( QT_QSTRINGLIST_LASTINDEXOF_1 )
{
   hb_retni( hbqt_par_QStringList( 1 )->lastIndexOf( hbqt_par_QString( 2 ), ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : -1 ) ) );
}

/*
 * int lastIndexOf ( QRegExp & rx, int from = -1 ) const
 */
HB_FUNC( QT_QSTRINGLIST_LASTINDEXOF_2 )
{
   hb_retni( hbqt_par_QStringList( 1 )->lastIndexOf( *hbqt_par_QRegExp( 2 ), ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : -1 ) ) );
}

/*
 * int removeDuplicates ()
 */
HB_FUNC( QT_QSTRINGLIST_REMOVEDUPLICATES )
{
   hb_retni( hbqt_par_QStringList( 1 )->removeDuplicates() );
}

/*
 * void sort ()
 */
HB_FUNC( QT_QSTRINGLIST_SORT )
{
   hbqt_par_QStringList( 1 )->sort();
}

/*
 * QString & at ( int i ) const
 */
HB_FUNC( QT_QSTRINGLIST_AT )
{
   hb_retc( hbqt_par_QStringList( 1 )->at( hb_parni( 2 ) ).toAscii().data() );
}

/*
 * QString & back ()
 */
HB_FUNC( QT_QSTRINGLIST_BACK )
{
   hb_retc( hbqt_par_QStringList( 1 )->back().toAscii().data() );
}

/*
 * int count ( const QString & value ) const
 */
HB_FUNC( QT_QSTRINGLIST_COUNT )
{
   hb_retni( hbqt_par_QStringList( 1 )->count( hbqt_par_QString( 2 ) ) );
}

/*
 * bool endsWith ( const QString & value ) const
 */
HB_FUNC( QT_QSTRINGLIST_ENDSWITH )
{
   hb_retl( hbqt_par_QStringList( 1 )->endsWith( hbqt_par_QString( 2 ) ) );
}

/*
 * QString & first ()
 */
HB_FUNC( QT_QSTRINGLIST_FIRST )
{
   hb_retc( hbqt_par_QStringList( 1 )->first().toAscii().data() );
}

/*
 * const QString & first () const
 */
HB_FUNC( QT_QSTRINGLIST_FIRST_1 )
{
   hb_retc( hbqt_par_QStringList( 1 )->first().toAscii().data() );
}

/*
 * QString & front ()
 */
HB_FUNC( QT_QSTRINGLIST_FRONT )
{
   hb_retc( hbqt_par_QStringList( 1 )->front().toAscii().data() );
}

/*
 * const QString & front () const
 */
HB_FUNC( QT_QSTRINGLIST_FRONT_1 )
{
   hb_retc( hbqt_par_QStringList( 1 )->front().toAscii().data() );
}

/*
 * void insert ( int i, const QString & value )
 */
HB_FUNC( QT_QSTRINGLIST_INSERT )
{
   hbqt_par_QStringList( 1 )->insert( hb_parni( 2 ), hbqt_par_QString( 3 ) );
}

/*
 * QString & last ()
 */
HB_FUNC( QT_QSTRINGLIST_LAST )
{
   hb_retc( hbqt_par_QStringList( 1 )->last().toAscii().data() );
}

/*
 * const QString & last () const
 */
HB_FUNC( QT_QSTRINGLIST_LAST_1 )
{
   hb_retc( hbqt_par_QStringList( 1 )->last().toAscii().data() );
}

/*
 * void prepend ( const QString & value )
 */
HB_FUNC( QT_QSTRINGLIST_PREPEND )
{
   hbqt_par_QStringList( 1 )->prepend( hbqt_par_QString( 2 ) );
}

/*
 * void push_back ( const QString & value )
 */
HB_FUNC( QT_QSTRINGLIST_PUSH_BACK )
{
   hbqt_par_QStringList( 1 )->push_back( hbqt_par_QString( 2 ) );
}

/*
 * void push_front ( const QString & value )
 */
HB_FUNC( QT_QSTRINGLIST_PUSH_FRONT )
{
   hbqt_par_QStringList( 1 )->push_front( hbqt_par_QString( 2 ) );
}

/*
 * int removeAll ( const QString & value )
 */
HB_FUNC( QT_QSTRINGLIST_REMOVEALL )
{
   hb_retni( hbqt_par_QStringList( 1 )->removeAll( hbqt_par_QString( 2 ) ) );
}

/*
 * bool removeOne ( const QString & value )
 */
HB_FUNC( QT_QSTRINGLIST_REMOVEONE )
{
   hb_retl( hbqt_par_QStringList( 1 )->removeOne( hbqt_par_QString( 2 ) ) );
}

/*
 * void replace ( int i, const QString & value )
 */
HB_FUNC( QT_QSTRINGLIST_REPLACE )
{
   hbqt_par_QStringList( 1 )->replace( hb_parni( 2 ), hbqt_par_QString( 3 ) );
}

/*
 * bool startsWith ( const QString & value ) const
 */
HB_FUNC( QT_QSTRINGLIST_STARTSWITH )
{
   hb_retl( hbqt_par_QStringList( 1 )->startsWith( hbqt_par_QString( 2 ) ) );
}

/*
 * QString takeAt ( int i )
 */
HB_FUNC( QT_QSTRINGLIST_TAKEAT )
{
   hb_retc( hbqt_par_QStringList( 1 )->takeAt( hb_parni( 2 ) ).toAscii().data() );
}

/*
 * QString takeFirst ()
 */
HB_FUNC( QT_QSTRINGLIST_TAKEFIRST )
{
   hb_retc( hbqt_par_QStringList( 1 )->takeFirst().toAscii().data() );
}

/*
 * QString takeLast ()
 */
HB_FUNC( QT_QSTRINGLIST_TAKELAST )
{
   hb_retc( hbqt_par_QStringList( 1 )->takeLast().toAscii().data() );
}

/*
 * QString value ( int i ) const
 */
HB_FUNC( QT_QSTRINGLIST_VALUE )
{
   hb_retc( hbqt_par_QStringList( 1 )->value( hb_parni( 2 ) ).toAscii().data() );
}

/*
 * QString value ( int i, const QString & defaultValue ) const
 */
HB_FUNC( QT_QSTRINGLIST_VALUE_1 )
{
   hb_retc( hbqt_par_QStringList( 1 )->value( hb_parni( 2 ), hbqt_par_QString( 3 ) ).toAscii().data() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
