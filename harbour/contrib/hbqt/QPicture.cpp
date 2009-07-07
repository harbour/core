/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
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


#include <QtGui/QPicture>


/*
 * QPicture ( int formatVersion = -1 )
 * QPicture ( const QPicture & pic )
 * ~QPicture ()
 */
HB_FUNC( QT_QPICTURE )
{
   hb_retptr( ( QPicture* ) new QPicture() );
}

/*
 * QRect boundingRect () const
 */
HB_FUNC( QT_QPICTURE_BOUNDINGRECT )
{
   hb_retptr( new QRect( hbqt_par_QPicture( 1 )->boundingRect() ) );
}

/*
 * const char * data () const
 */
HB_FUNC( QT_QPICTURE_DATA )
{
   hb_retc( hbqt_par_QPicture( 1 )->data() );
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QPICTURE_ISNULL )
{
   hb_retl( hbqt_par_QPicture( 1 )->isNull() );
}

/*
 * bool load ( const QString & fileName, const char * format = 0 )
 */
HB_FUNC( QT_QPICTURE_LOAD )
{
   hb_retl( hbqt_par_QPicture( 1 )->load( hbqt_par_QString( 2 ), hbqt_par_char( 3 ) ) );
}

/*
 * bool load ( QIODevice * dev, const char * format = 0 )
 */
HB_FUNC( QT_QPICTURE_LOAD_1 )
{
   hb_retl( hbqt_par_QPicture( 1 )->load( hbqt_par_QIODevice( 2 ), hbqt_par_char( 3 ) ) );
}

/*
 * bool play ( QPainter * painter )
 */
HB_FUNC( QT_QPICTURE_PLAY )
{
   hb_retl( hbqt_par_QPicture( 1 )->play( hbqt_par_QPainter( 2 ) ) );
}

/*
 * bool save ( const QString & fileName, const char * format = 0 )
 */
HB_FUNC( QT_QPICTURE_SAVE )
{
   hb_retl( hbqt_par_QPicture( 1 )->save( hbqt_par_QString( 2 ), hbqt_par_char( 3 ) ) );
}

/*
 * bool save ( QIODevice * dev, const char * format = 0 )
 */
HB_FUNC( QT_QPICTURE_SAVE_1 )
{
   hb_retl( hbqt_par_QPicture( 1 )->save( hbqt_par_QIODevice( 2 ), hbqt_par_char( 3 ) ) );
}

/*
 * void setBoundingRect ( const QRect & r )
 */
HB_FUNC( QT_QPICTURE_SETBOUNDINGRECT )
{
   hbqt_par_QPicture( 1 )->setBoundingRect( *hbqt_par_QRect( 2 ) );
}

/*
 * virtual void setData ( const char * data, uint size )
 */
HB_FUNC( QT_QPICTURE_SETDATA )
{
   hbqt_par_QPicture( 1 )->setData( hbqt_par_char( 2 ), hb_parni( 3 ) );
}

/*
 * uint size () const
 */
HB_FUNC( QT_QPICTURE_SIZE )
{
   hb_retni( hbqt_par_QPicture( 1 )->size() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/

