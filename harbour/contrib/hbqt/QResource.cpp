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


#include <QtCore/QResource>


/*
 * QResource ( const QString & file = QString(), const QLocale & locale = QLocale() )
 * ~QResource ()
 */
HB_FUNC( QT_QRESOURCE )
{
   hb_retptr( ( QResource* ) new QResource() );
}

/*
 * DESTRUCTOR
 */
HB_FUNC( QT_QRESOURCE_DESTROY )
{
   hbqt_par_QResource( 1 )->~QResource();
}

/*
 * QString absoluteFilePath () const
 */
HB_FUNC( QT_QRESOURCE_ABSOLUTEFILEPATH )
{
   hb_retc( hbqt_par_QResource( 1 )->absoluteFilePath().toLatin1().data() );
}

/*
 * const uchar * data () const
 */
HB_FUNC( QT_QRESOURCE_DATA )
{
   hb_retptr( ( uchar* ) hbqt_par_QResource( 1 )->data() );
}

/*
 * QString fileName () const
 */
HB_FUNC( QT_QRESOURCE_FILENAME )
{
   hb_retc( hbqt_par_QResource( 1 )->fileName().toLatin1().data() );
}

/*
 * bool isCompressed () const
 */
HB_FUNC( QT_QRESOURCE_ISCOMPRESSED )
{
   hb_retl( hbqt_par_QResource( 1 )->isCompressed() );
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QRESOURCE_ISVALID )
{
   hb_retl( hbqt_par_QResource( 1 )->isValid() );
}

/*
 * QLocale locale () const
 */
HB_FUNC( QT_QRESOURCE_LOCALE )
{
   hb_retptr( new QLocale( hbqt_par_QResource( 1 )->locale() ) );
}

/*
 * void setFileName ( const QString & file )
 */
HB_FUNC( QT_QRESOURCE_SETFILENAME )
{
   hbqt_par_QResource( 1 )->setFileName( hbqt_par_QString( 2 ) );
}

/*
 * void setLocale ( const QLocale & locale )
 */
HB_FUNC( QT_QRESOURCE_SETLOCALE )
{
   hbqt_par_QResource( 1 )->setLocale( *hbqt_par_QLocale( 2 ) );
}

/*
 * qint64 size () const
 */
HB_FUNC( QT_QRESOURCE_SIZE )
{
   hb_retnint( hbqt_par_QResource( 1 )->size() );
}

/*
 * bool registerResource ( const QString & rccFileName, const QString & mapRoot = QString() )
 */
HB_FUNC( QT_QRESOURCE_REGISTERRESOURCE )
{
   hb_retl( hbqt_par_QResource( 1 )->registerResource( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) );
}

/*
 * QStringList searchPaths ()
 */
HB_FUNC( QT_QRESOURCE_SEARCHPATHS )
{
   hb_retptr( new QStringList( hbqt_par_QResource( 1 )->searchPaths() ) );
}

/*
 * bool unregisterResource ( const QString & rccFileName, const QString & mapRoot = QString() )
 */
HB_FUNC( QT_QRESOURCE_UNREGISTERRESOURCE )
{
   hb_retl( hbqt_par_QResource( 1 )->unregisterResource( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/

