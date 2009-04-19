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


#include <QtCore/QTextBoundaryFinder>


/*
 * QTextBoundaryFinder ()
 * QTextBoundaryFinder ( const QTextBoundaryFinder & other )
 * QTextBoundaryFinder ( BoundaryType type, const QString & string )
 * QTextBoundaryFinder ( BoundaryType type, const QChar * chars, int length, unsigned char * buffer = 0, int bufferSize = 0 )
 * ~QTextBoundaryFinder ()
 */
HB_FUNC( QT_QTEXTBOUNDARYFINDER )
{
   hb_retptr( ( QTextBoundaryFinder* ) new QTextBoundaryFinder() );
}

/*
 * BoundaryReasons boundaryReasons () const
 */
HB_FUNC( QT_QTEXTBOUNDARYFINDER_BOUNDARYREASONS )
{
   hb_retni( ( QTextBoundaryFinder::BoundaryReasons ) hbqt_par_QTextBoundaryFinder( 1 )->boundaryReasons() );
}

/*
 * bool isAtBoundary () const
 */
HB_FUNC( QT_QTEXTBOUNDARYFINDER_ISATBOUNDARY )
{
   hb_retl( hbqt_par_QTextBoundaryFinder( 1 )->isAtBoundary() );
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QTEXTBOUNDARYFINDER_ISVALID )
{
   hb_retl( hbqt_par_QTextBoundaryFinder( 1 )->isValid() );
}

/*
 * int position () const
 */
HB_FUNC( QT_QTEXTBOUNDARYFINDER_POSITION )
{
   hb_retni( hbqt_par_QTextBoundaryFinder( 1 )->position() );
}

/*
 * void setPosition ( int position )
 */
HB_FUNC( QT_QTEXTBOUNDARYFINDER_SETPOSITION )
{
   hbqt_par_QTextBoundaryFinder( 1 )->setPosition( hb_parni( 2 ) );
}

/*
 * QString string () const
 */
HB_FUNC( QT_QTEXTBOUNDARYFINDER_STRING )
{
   hb_retc( hbqt_par_QTextBoundaryFinder( 1 )->string().toLatin1().data() );
}

/*
 * void toEnd ()
 */
HB_FUNC( QT_QTEXTBOUNDARYFINDER_TOEND )
{
   hbqt_par_QTextBoundaryFinder( 1 )->toEnd();
}

/*
 * int toNextBoundary ()
 */
HB_FUNC( QT_QTEXTBOUNDARYFINDER_TONEXTBOUNDARY )
{
   hb_retni( hbqt_par_QTextBoundaryFinder( 1 )->toNextBoundary() );
}

/*
 * int toPreviousBoundary ()
 */
HB_FUNC( QT_QTEXTBOUNDARYFINDER_TOPREVIOUSBOUNDARY )
{
   hb_retni( hbqt_par_QTextBoundaryFinder( 1 )->toPreviousBoundary() );
}

/*
 * void toStart ()
 */
HB_FUNC( QT_QTEXTBOUNDARYFINDER_TOSTART )
{
   hbqt_par_QTextBoundaryFinder( 1 )->toStart();
}

/*
 * BoundaryType type () const
 */
HB_FUNC( QT_QTEXTBOUNDARYFINDER_TYPE )
{
   hb_retni( ( QTextBoundaryFinder::BoundaryType ) hbqt_par_QTextBoundaryFinder( 1 )->type() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/

