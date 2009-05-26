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


#include <QtGui/QFont>

/*
 * QFont ()
 * QFont ( const QString & family, int pointSize = -1, int weight = -1, bool italic = false )
 * QFont ( const QFont & font, QPaintDevice * pd )
 * QFont ( const QFont & font )
 */
HB_FUNC( QT_QFONT )
{
   if( HB_ISCHAR( 1 ) )
   {
      hb_retptr( ( QFont* ) new QFont() );
   }
   else
   {
      hb_retptr( ( QFont* ) new QFont( hbqt_par_QString( 1 ),
                                       HB_ISNUM( 2 ) ? hb_parni( 2 ) : -1,
                                       HB_ISNUM( 3 ) ? hb_parni( 3 ) : -1,
                                       hb_parl( 4 ) ) );
   }
}

/*
 * bool bold () const
 */
HB_FUNC( QT_QFONT_BOLD )
{
   hb_retl( hbqt_par_QFont( 1 )->bold() );
}

/*
 * Capitalization capitalization () const
 */
HB_FUNC( QT_QFONT_CAPITALIZATION )
{
   hb_retni( ( QFont::Capitalization ) hbqt_par_QFont( 1 )->capitalization() );
}

/*
 * QString defaultFamily () const
 */
HB_FUNC( QT_QFONT_DEFAULTFAMILY )
{
   hb_retc( hbqt_par_QFont( 1 )->defaultFamily().toLatin1().data() );
}

/*
 * bool exactMatch () const
 */
HB_FUNC( QT_QFONT_EXACTMATCH )
{
   hb_retl( hbqt_par_QFont( 1 )->exactMatch() );
}

/*
 * QString family () const
 */
HB_FUNC( QT_QFONT_FAMILY )
{
   hb_retc( hbqt_par_QFont( 1 )->family().toLatin1().data() );
}

/*
 * bool fixedPitch () const
 */
HB_FUNC( QT_QFONT_FIXEDPITCH )
{
   hb_retl( hbqt_par_QFont( 1 )->fixedPitch() );
}

/*
 * bool fromString ( const QString & descrip )
 */
HB_FUNC( QT_QFONT_FROMSTRING )
{
   hb_retl( hbqt_par_QFont( 1 )->fromString( hbqt_par_QString( 2 ) ) );
}

/*
 * bool italic () const
 */
HB_FUNC( QT_QFONT_ITALIC )
{
   hb_retl( hbqt_par_QFont( 1 )->italic() );
}

/*
 * bool kerning () const
 */
HB_FUNC( QT_QFONT_KERNING )
{
   hb_retl( hbqt_par_QFont( 1 )->kerning() );
}

/*
 * QString key () const
 */
HB_FUNC( QT_QFONT_KEY )
{
   hb_retc( hbqt_par_QFont( 1 )->key().toLatin1().data() );
}

/*
 * QString lastResortFamily () const
 */
HB_FUNC( QT_QFONT_LASTRESORTFAMILY )
{
   hb_retc( hbqt_par_QFont( 1 )->lastResortFamily().toLatin1().data() );
}

/*
 * QString lastResortFont () const
 */
HB_FUNC( QT_QFONT_LASTRESORTFONT )
{
   hb_retc( hbqt_par_QFont( 1 )->lastResortFont().toLatin1().data() );
}

/*
 * qreal letterSpacing () const
 */
HB_FUNC( QT_QFONT_LETTERSPACING )
{
   hb_retnd( hbqt_par_QFont( 1 )->letterSpacing() );
}

/*
 * SpacingType letterSpacingType () const
 */
HB_FUNC( QT_QFONT_LETTERSPACINGTYPE )
{
   hb_retni( ( QFont::SpacingType ) hbqt_par_QFont( 1 )->letterSpacingType() );
}

/*
 * bool overline () const
 */
HB_FUNC( QT_QFONT_OVERLINE )
{
   hb_retl( hbqt_par_QFont( 1 )->overline() );
}

/*
 * int pixelSize () const
 */
HB_FUNC( QT_QFONT_PIXELSIZE )
{
   hb_retni( hbqt_par_QFont( 1 )->pixelSize() );
}

/*
 * int pointSize () const
 */
HB_FUNC( QT_QFONT_POINTSIZE )
{
   hb_retni( hbqt_par_QFont( 1 )->pointSize() );
}

/*
 * qreal pointSizeF () const
 */
HB_FUNC( QT_QFONT_POINTSIZEF )
{
   hb_retnd( hbqt_par_QFont( 1 )->pointSizeF() );
}

/*
 * bool rawMode () const
 */
HB_FUNC( QT_QFONT_RAWMODE )
{
   hb_retl( hbqt_par_QFont( 1 )->rawMode() );
}

/*
 * QString rawName () const
 */
HB_FUNC( QT_QFONT_RAWNAME )
{
   hb_retc( hbqt_par_QFont( 1 )->rawName().toLatin1().data() );
}

/*
 * void setBold ( bool enable )
 */
HB_FUNC( QT_QFONT_SETBOLD )
{
   hbqt_par_QFont( 1 )->setBold( hb_parl( 2 ) );
}

/*
 * void setCapitalization ( Capitalization caps )
 */
HB_FUNC( QT_QFONT_SETCAPITALIZATION )
{
   hbqt_par_QFont( 1 )->setCapitalization( ( QFont::Capitalization ) hb_parni( 2 ) );
}

/*
 * void setFamily ( const QString & family )
 */
HB_FUNC( QT_QFONT_SETFAMILY )
{
   hbqt_par_QFont( 1 )->setFamily( hbqt_par_QString( 2 ) );
}

/*
 * void setFixedPitch ( bool enable )
 */
HB_FUNC( QT_QFONT_SETFIXEDPITCH )
{
   hbqt_par_QFont( 1 )->setFixedPitch( hb_parl( 2 ) );
}

/*
 * void setItalic ( bool enable )
 */
HB_FUNC( QT_QFONT_SETITALIC )
{
   hbqt_par_QFont( 1 )->setItalic( hb_parl( 2 ) );
}

/*
 * void setKerning ( bool enable )
 */
HB_FUNC( QT_QFONT_SETKERNING )
{
   hbqt_par_QFont( 1 )->setKerning( hb_parl( 2 ) );
}

/*
 * void setLetterSpacing ( SpacingType type, qreal spacing )
 */
HB_FUNC( QT_QFONT_SETLETTERSPACING )
{
   hbqt_par_QFont( 1 )->setLetterSpacing( ( QFont::SpacingType ) hb_parni( 2 ), hb_parnd( 3 ) );
}

/*
 * void setOverline ( bool enable )
 */
HB_FUNC( QT_QFONT_SETOVERLINE )
{
   hbqt_par_QFont( 1 )->setOverline( hb_parl( 2 ) );
}

/*
 * void setPixelSize ( int pixelSize )
 */
HB_FUNC( QT_QFONT_SETPIXELSIZE )
{
   hbqt_par_QFont( 1 )->setPixelSize( hb_parni( 2 ) );
}

/*
 * void setPointSize ( int pointSize )
 */
HB_FUNC( QT_QFONT_SETPOINTSIZE )
{
   hbqt_par_QFont( 1 )->setPointSize( hb_parni( 2 ) );
}

/*
 * void setPointSizeF ( qreal pointSize )
 */
HB_FUNC( QT_QFONT_SETPOINTSIZEF )
{
   hbqt_par_QFont( 1 )->setPointSizeF( hb_parnd( 2 ) );
}

/*
 * void setRawMode ( bool enable )
 */
HB_FUNC( QT_QFONT_SETRAWMODE )
{
   hbqt_par_QFont( 1 )->setRawMode( hb_parl( 2 ) );
}

/*
 * void setRawName ( const QString & name )
 */
HB_FUNC( QT_QFONT_SETRAWNAME )
{
   hbqt_par_QFont( 1 )->setRawName( hbqt_par_QString( 2 ) );
}

/*
 * void setStretch ( int factor )
 */
HB_FUNC( QT_QFONT_SETSTRETCH )
{
   hbqt_par_QFont( 1 )->setStretch( hb_parni( 2 ) );
}

/*
 * void setStrikeOut ( bool enable )
 */
HB_FUNC( QT_QFONT_SETSTRIKEOUT )
{
   hbqt_par_QFont( 1 )->setStrikeOut( hb_parl( 2 ) );
}

/*
 * void setStyle ( Style style )
 */
HB_FUNC( QT_QFONT_SETSTYLE )
{
   hbqt_par_QFont( 1 )->setStyle( ( QFont::Style ) hb_parni( 2 ) );
}

/*
 * void setStyleHint ( StyleHint hint, StyleStrategy strategy = PreferDefault )
 */
HB_FUNC( QT_QFONT_SETSTYLEHINT )
{
   hbqt_par_QFont( 1 )->setStyleHint( ( QFont::StyleHint ) hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QFont::StyleStrategy ) hb_parni( 3 ) : ( QFont::StyleStrategy ) QFont::PreferDefault ) );
}

/*
 * void setStyleStrategy ( StyleStrategy s )
 */
HB_FUNC( QT_QFONT_SETSTYLESTRATEGY )
{
   hbqt_par_QFont( 1 )->setStyleStrategy( ( QFont::StyleStrategy ) hb_parni( 2 ) );
}

/*
 * void setUnderline ( bool enable )
 */
HB_FUNC( QT_QFONT_SETUNDERLINE )
{
   hbqt_par_QFont( 1 )->setUnderline( hb_parl( 2 ) );
}

/*
 * void setWeight ( int weight )
 */
HB_FUNC( QT_QFONT_SETWEIGHT )
{
   hbqt_par_QFont( 1 )->setWeight( hb_parni( 2 ) );
}

/*
 * void setWordSpacing ( qreal spacing )
 */
HB_FUNC( QT_QFONT_SETWORDSPACING )
{
   hbqt_par_QFont( 1 )->setWordSpacing( hb_parnd( 2 ) );
}

/*
 * int stretch () const
 */
HB_FUNC( QT_QFONT_STRETCH )
{
   hb_retni( hbqt_par_QFont( 1 )->stretch() );
}

/*
 * bool strikeOut () const
 */
HB_FUNC( QT_QFONT_STRIKEOUT )
{
   hb_retl( hbqt_par_QFont( 1 )->strikeOut() );
}

/*
 * Style style () const
 */
HB_FUNC( QT_QFONT_STYLE )
{
   hb_retni( ( QFont::Style ) hbqt_par_QFont( 1 )->style() );
}

/*
 * StyleHint styleHint () const
 */
HB_FUNC( QT_QFONT_STYLEHINT )
{
   hb_retni( ( QFont::StyleHint ) hbqt_par_QFont( 1 )->styleHint() );
}

/*
 * StyleStrategy styleStrategy () const
 */
HB_FUNC( QT_QFONT_STYLESTRATEGY )
{
   hb_retni( ( QFont::StyleStrategy ) hbqt_par_QFont( 1 )->styleStrategy() );
}

/*
 * QString toString () const
 */
HB_FUNC( QT_QFONT_TOSTRING )
{
   hb_retc( hbqt_par_QFont( 1 )->toString().toLatin1().data() );
}

/*
 * bool underline () const
 */
HB_FUNC( QT_QFONT_UNDERLINE )
{
   hb_retl( hbqt_par_QFont( 1 )->underline() );
}

/*
 * int weight () const
 */
HB_FUNC( QT_QFONT_WEIGHT )
{
   hb_retni( hbqt_par_QFont( 1 )->weight() );
}

/*
 * qreal wordSpacing () const
 */
HB_FUNC( QT_QFONT_WORDSPACING )
{
   hb_retnd( hbqt_par_QFont( 1 )->wordSpacing() );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/

