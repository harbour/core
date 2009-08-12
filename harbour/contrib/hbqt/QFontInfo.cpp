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


#include <QtGui/QFontInfo>


/*
 * QFontInfo ( const QFont & font )
 * QFontInfo ( const QFontInfo & fi )
 * ~QFontInfo ()
 */
HB_FUNC( QT_QFONTINFO )
{
   hb_retptr( new QFontInfo( *hbqt_par_QFont( 1 ) ) );
}

/*
 * DESTRUCTOR
 */
HB_FUNC( QT_QFONTINFO_DESTROY )
{
   hbqt_par_QFontInfo( 1 )->~QFontInfo();
}

/*
 * bool bold () const
 */
HB_FUNC( QT_QFONTINFO_BOLD )
{
   hb_retl( hbqt_par_QFontInfo( 1 )->bold() );
}

/*
 * bool exactMatch () const
 */
HB_FUNC( QT_QFONTINFO_EXACTMATCH )
{
   hb_retl( hbqt_par_QFontInfo( 1 )->exactMatch() );
}

/*
 * QString family () const
 */
HB_FUNC( QT_QFONTINFO_FAMILY )
{
   hb_retc( hbqt_par_QFontInfo( 1 )->family().toLatin1().data() );
}

/*
 * bool fixedPitch () const
 */
HB_FUNC( QT_QFONTINFO_FIXEDPITCH )
{
   hb_retl( hbqt_par_QFontInfo( 1 )->fixedPitch() );
}

/*
 * bool italic () const
 */
HB_FUNC( QT_QFONTINFO_ITALIC )
{
   hb_retl( hbqt_par_QFontInfo( 1 )->italic() );
}

/*
 * int pixelSize () const
 */
HB_FUNC( QT_QFONTINFO_PIXELSIZE )
{
   hb_retni( hbqt_par_QFontInfo( 1 )->pixelSize() );
}

/*
 * int pointSize () const
 */
HB_FUNC( QT_QFONTINFO_POINTSIZE )
{
   hb_retni( hbqt_par_QFontInfo( 1 )->pointSize() );
}

/*
 * qreal pointSizeF () const
 */
HB_FUNC( QT_QFONTINFO_POINTSIZEF )
{
   hb_retnd( hbqt_par_QFontInfo( 1 )->pointSizeF() );
}

/*
 * bool rawMode () const
 */
HB_FUNC( QT_QFONTINFO_RAWMODE )
{
   hb_retl( hbqt_par_QFontInfo( 1 )->rawMode() );
}

/*
 * QFont::Style style () const
 */
HB_FUNC( QT_QFONTINFO_STYLE )
{
   hb_retni( ( QFont::Style ) hbqt_par_QFontInfo( 1 )->style() );
}

/*
 * QFont::StyleHint styleHint () const
 */
HB_FUNC( QT_QFONTINFO_STYLEHINT )
{
   hb_retni( ( QFont::StyleHint ) hbqt_par_QFontInfo( 1 )->styleHint() );
}

/*
 * int weight () const
 */
HB_FUNC( QT_QFONTINFO_WEIGHT )
{
   hb_retni( hbqt_par_QFontInfo( 1 )->weight() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
