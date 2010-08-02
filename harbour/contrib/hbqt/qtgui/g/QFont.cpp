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

#include "hbqt.h"
#include "hbqtgui_garbage.h"
#include "hbqtcore_garbage.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum Capitalization { MixedCase, AllUppercase, AllLowercase, SmallCaps, Capitalize }
 *  enum SpacingType { PercentageSpacing, AbsoluteSpacing }
 *  enum Stretch { UltraCondensed, ExtraCondensed, Condensed, SemiCondensed, ..., UltraExpanded }
 *  enum Style { StyleNormal, StyleItalic, StyleOblique }
 *  enum StyleHint { AnyStyle, SansSerif, Helvetica, Serif, ..., System }
 *  enum StyleStrategy { PreferDefault, PreferBitmap, PreferDevice, PreferOutline, ..., PreferQuality }
 *  enum Weight { Light, Normal, DemiBold, Bold, Black }
 */

#include <QtCore/QPointer>

#include <QtCore/QStringList>
#include <QtGui/QFont>

/*
 * QFont ()
 * QFont ( const QString & family, int pointSize = -1, int weight = -1, bool italic = false )
 * QFont ( const QFont & font, QPaintDevice * pd )
 * QFont ( const QFont & font )
 * ~QFont ()
 */

typedef struct
{
   QFont * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QFont;

QT_G_FUNC( hbqt_gcRelease_QFont )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QFont   /.\\", p->ph ) );
         delete ( ( QFont * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QFont   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QFont    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QFont    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QFont( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QFont * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QFont;
   p->type = HBQT_TYPE_QFont;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QFont", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QFont", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QFONT )
{
   QFont * pObj = NULL;

   if(      hb_pcount() == 1 && HB_ISCHAR( 1 ) )
   {
      pObj =  new QFont( hbqt_par_QString( 1 ), -1, -1, false ) ;
   }
   else if( hb_pcount() == 2 && HB_ISCHAR( 1 ) && HB_ISNUM( 2 ) )
   {
      pObj =  new QFont( hbqt_par_QString( 1 ), hb_parni( 2 ), -1, false ) ;
   }
   else if( hb_pcount() == 3 && HB_ISCHAR( 1 ) && HB_ISNUM( 2 )  && HB_ISNUM( 3 ) )
   {
      pObj =  new QFont( hbqt_par_QString( 1 ), hb_parni( 2 ), hb_parni( 3 ), false ) ;
   }
   else if( hb_pcount() == 4 && HB_ISCHAR( 1 ) && HB_ISNUM( 2 )  && HB_ISNUM( 3 ) && HB_ISLOG( 4 ) )
   {
      pObj =  new QFont( hbqt_par_QString( 1 ), hb_parni( 2 ), hb_parni( 3 ), hb_parl( 4 ) ) ;
   }
   else if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj =  new QFont( *hbqt_par_QFont( 1 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISPOINTER( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj =  new QFont( *hbqt_par_QFont( 1 ), hbqt_par_QPaintDevice( 2 ) ) ;
   }
   else
   {
      pObj =  new QFont() ;
   }

   hb_retptrGC( hbqt_gcAllocate_QFont( ( void * ) pObj, true ) );
}

/*
 * bool bold () const
 */
HB_FUNC( QT_QFONT_BOLD )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      hb_retl( ( p )->bold() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_BOLD FP=hb_retl( ( p )->bold() ); p is NULL" ) );
   }
}

/*
 * Capitalization capitalization () const
 */
HB_FUNC( QT_QFONT_CAPITALIZATION )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      hb_retni( ( QFont::Capitalization ) ( p )->capitalization() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_CAPITALIZATION FP=hb_retni( ( QFont::Capitalization ) ( p )->capitalization() ); p is NULL" ) );
   }
}

/*
 * QString defaultFamily () const
 */
HB_FUNC( QT_QFONT_DEFAULTFAMILY )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      hb_retc( ( p )->defaultFamily().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_DEFAULTFAMILY FP=hb_retc( ( p )->defaultFamily().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * bool exactMatch () const
 */
HB_FUNC( QT_QFONT_EXACTMATCH )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      hb_retl( ( p )->exactMatch() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_EXACTMATCH FP=hb_retl( ( p )->exactMatch() ); p is NULL" ) );
   }
}

/*
 * QString family () const
 */
HB_FUNC( QT_QFONT_FAMILY )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      hb_retc( ( p )->family().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_FAMILY FP=hb_retc( ( p )->family().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * bool fixedPitch () const
 */
HB_FUNC( QT_QFONT_FIXEDPITCH )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      hb_retl( ( p )->fixedPitch() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_FIXEDPITCH FP=hb_retl( ( p )->fixedPitch() ); p is NULL" ) );
   }
}

/*
 * bool fromString ( const QString & descrip )
 */
HB_FUNC( QT_QFONT_FROMSTRING )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      hb_retl( ( p )->fromString( hbqt_par_QString( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_FROMSTRING FP=hb_retl( ( p )->fromString( hbqt_par_QString( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool italic () const
 */
HB_FUNC( QT_QFONT_ITALIC )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      hb_retl( ( p )->italic() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_ITALIC FP=hb_retl( ( p )->italic() ); p is NULL" ) );
   }
}

/*
 * bool kerning () const
 */
HB_FUNC( QT_QFONT_KERNING )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      hb_retl( ( p )->kerning() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_KERNING FP=hb_retl( ( p )->kerning() ); p is NULL" ) );
   }
}

/*
 * QString key () const
 */
HB_FUNC( QT_QFONT_KEY )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      hb_retc( ( p )->key().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_KEY FP=hb_retc( ( p )->key().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString lastResortFamily () const
 */
HB_FUNC( QT_QFONT_LASTRESORTFAMILY )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      hb_retc( ( p )->lastResortFamily().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_LASTRESORTFAMILY FP=hb_retc( ( p )->lastResortFamily().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString lastResortFont () const
 */
HB_FUNC( QT_QFONT_LASTRESORTFONT )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      hb_retc( ( p )->lastResortFont().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_LASTRESORTFONT FP=hb_retc( ( p )->lastResortFont().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * qreal letterSpacing () const
 */
HB_FUNC( QT_QFONT_LETTERSPACING )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      hb_retnd( ( p )->letterSpacing() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_LETTERSPACING FP=hb_retnd( ( p )->letterSpacing() ); p is NULL" ) );
   }
}

/*
 * SpacingType letterSpacingType () const
 */
HB_FUNC( QT_QFONT_LETTERSPACINGTYPE )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      hb_retni( ( QFont::SpacingType ) ( p )->letterSpacingType() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_LETTERSPACINGTYPE FP=hb_retni( ( QFont::SpacingType ) ( p )->letterSpacingType() ); p is NULL" ) );
   }
}

/*
 * bool overline () const
 */
HB_FUNC( QT_QFONT_OVERLINE )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      hb_retl( ( p )->overline() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_OVERLINE FP=hb_retl( ( p )->overline() ); p is NULL" ) );
   }
}

/*
 * int pixelSize () const
 */
HB_FUNC( QT_QFONT_PIXELSIZE )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      hb_retni( ( p )->pixelSize() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_PIXELSIZE FP=hb_retni( ( p )->pixelSize() ); p is NULL" ) );
   }
}

/*
 * int pointSize () const
 */
HB_FUNC( QT_QFONT_POINTSIZE )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      hb_retni( ( p )->pointSize() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_POINTSIZE FP=hb_retni( ( p )->pointSize() ); p is NULL" ) );
   }
}

/*
 * qreal pointSizeF () const
 */
HB_FUNC( QT_QFONT_POINTSIZEF )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      hb_retnd( ( p )->pointSizeF() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_POINTSIZEF FP=hb_retnd( ( p )->pointSizeF() ); p is NULL" ) );
   }
}

/*
 * bool rawMode () const
 */
HB_FUNC( QT_QFONT_RAWMODE )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      hb_retl( ( p )->rawMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_RAWMODE FP=hb_retl( ( p )->rawMode() ); p is NULL" ) );
   }
}

/*
 * QString rawName () const
 */
HB_FUNC( QT_QFONT_RAWNAME )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      hb_retc( ( p )->rawName().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_RAWNAME FP=hb_retc( ( p )->rawName().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * void setBold ( bool enable )
 */
HB_FUNC( QT_QFONT_SETBOLD )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      ( p )->setBold( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_SETBOLD FP=( p )->setBold( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setCapitalization ( Capitalization caps )
 */
HB_FUNC( QT_QFONT_SETCAPITALIZATION )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      ( p )->setCapitalization( ( QFont::Capitalization ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_SETCAPITALIZATION FP=( p )->setCapitalization( ( QFont::Capitalization ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFamily ( const QString & family )
 */
HB_FUNC( QT_QFONT_SETFAMILY )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      ( p )->setFamily( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_SETFAMILY FP=( p )->setFamily( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setFixedPitch ( bool enable )
 */
HB_FUNC( QT_QFONT_SETFIXEDPITCH )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      ( p )->setFixedPitch( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_SETFIXEDPITCH FP=( p )->setFixedPitch( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setItalic ( bool enable )
 */
HB_FUNC( QT_QFONT_SETITALIC )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      ( p )->setItalic( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_SETITALIC FP=( p )->setItalic( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setKerning ( bool enable )
 */
HB_FUNC( QT_QFONT_SETKERNING )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      ( p )->setKerning( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_SETKERNING FP=( p )->setKerning( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setLetterSpacing ( SpacingType type, qreal spacing )
 */
HB_FUNC( QT_QFONT_SETLETTERSPACING )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      ( p )->setLetterSpacing( ( QFont::SpacingType ) hb_parni( 2 ), hb_parnd( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_SETLETTERSPACING FP=( p )->setLetterSpacing( ( QFont::SpacingType ) hb_parni( 2 ), hb_parnd( 3 ) ); p is NULL" ) );
   }
}

/*
 * void setOverline ( bool enable )
 */
HB_FUNC( QT_QFONT_SETOVERLINE )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      ( p )->setOverline( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_SETOVERLINE FP=( p )->setOverline( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPixelSize ( int pixelSize )
 */
HB_FUNC( QT_QFONT_SETPIXELSIZE )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      ( p )->setPixelSize( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_SETPIXELSIZE FP=( p )->setPixelSize( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPointSize ( int pointSize )
 */
HB_FUNC( QT_QFONT_SETPOINTSIZE )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      ( p )->setPointSize( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_SETPOINTSIZE FP=( p )->setPointSize( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setPointSizeF ( qreal pointSize )
 */
HB_FUNC( QT_QFONT_SETPOINTSIZEF )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      ( p )->setPointSizeF( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_SETPOINTSIZEF FP=( p )->setPointSizeF( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setRawMode ( bool enable )
 */
HB_FUNC( QT_QFONT_SETRAWMODE )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      ( p )->setRawMode( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_SETRAWMODE FP=( p )->setRawMode( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setRawName ( const QString & name )
 */
HB_FUNC( QT_QFONT_SETRAWNAME )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      ( p )->setRawName( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_SETRAWNAME FP=( p )->setRawName( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setStretch ( int factor )
 */
HB_FUNC( QT_QFONT_SETSTRETCH )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      ( p )->setStretch( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_SETSTRETCH FP=( p )->setStretch( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setStrikeOut ( bool enable )
 */
HB_FUNC( QT_QFONT_SETSTRIKEOUT )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      ( p )->setStrikeOut( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_SETSTRIKEOUT FP=( p )->setStrikeOut( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setStyle ( Style style )
 */
HB_FUNC( QT_QFONT_SETSTYLE )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      ( p )->setStyle( ( QFont::Style ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_SETSTYLE FP=( p )->setStyle( ( QFont::Style ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setStyleHint ( StyleHint hint, StyleStrategy strategy = PreferDefault )
 */
HB_FUNC( QT_QFONT_SETSTYLEHINT )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      ( p )->setStyleHint( ( QFont::StyleHint ) hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QFont::StyleStrategy ) hb_parni( 3 ) : ( QFont::StyleStrategy ) QFont::PreferDefault ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_SETSTYLEHINT FP=( p )->setStyleHint( ( QFont::StyleHint ) hb_parni( 2 ), ( HB_ISNUM( 3 ) ? ( QFont::StyleStrategy ) hb_parni( 3 ) : ( QFont::StyleStrategy ) QFont::PreferDefault ) ); p is NULL" ) );
   }
}

/*
 * void setStyleStrategy ( StyleStrategy s )
 */
HB_FUNC( QT_QFONT_SETSTYLESTRATEGY )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      ( p )->setStyleStrategy( ( QFont::StyleStrategy ) hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_SETSTYLESTRATEGY FP=( p )->setStyleStrategy( ( QFont::StyleStrategy ) hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setUnderline ( bool enable )
 */
HB_FUNC( QT_QFONT_SETUNDERLINE )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      ( p )->setUnderline( hb_parl( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_SETUNDERLINE FP=( p )->setUnderline( hb_parl( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setWeight ( int weight )
 */
HB_FUNC( QT_QFONT_SETWEIGHT )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      ( p )->setWeight( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_SETWEIGHT FP=( p )->setWeight( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setWordSpacing ( qreal spacing )
 */
HB_FUNC( QT_QFONT_SETWORDSPACING )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      ( p )->setWordSpacing( hb_parnd( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_SETWORDSPACING FP=( p )->setWordSpacing( hb_parnd( 2 ) ); p is NULL" ) );
   }
}

/*
 * int stretch () const
 */
HB_FUNC( QT_QFONT_STRETCH )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      hb_retni( ( p )->stretch() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_STRETCH FP=hb_retni( ( p )->stretch() ); p is NULL" ) );
   }
}

/*
 * bool strikeOut () const
 */
HB_FUNC( QT_QFONT_STRIKEOUT )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      hb_retl( ( p )->strikeOut() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_STRIKEOUT FP=hb_retl( ( p )->strikeOut() ); p is NULL" ) );
   }
}

/*
 * Style style () const
 */
HB_FUNC( QT_QFONT_STYLE )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      hb_retni( ( QFont::Style ) ( p )->style() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_STYLE FP=hb_retni( ( QFont::Style ) ( p )->style() ); p is NULL" ) );
   }
}

/*
 * StyleHint styleHint () const
 */
HB_FUNC( QT_QFONT_STYLEHINT )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      hb_retni( ( QFont::StyleHint ) ( p )->styleHint() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_STYLEHINT FP=hb_retni( ( QFont::StyleHint ) ( p )->styleHint() ); p is NULL" ) );
   }
}

/*
 * StyleStrategy styleStrategy () const
 */
HB_FUNC( QT_QFONT_STYLESTRATEGY )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      hb_retni( ( QFont::StyleStrategy ) ( p )->styleStrategy() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_STYLESTRATEGY FP=hb_retni( ( QFont::StyleStrategy ) ( p )->styleStrategy() ); p is NULL" ) );
   }
}

/*
 * QString toString () const
 */
HB_FUNC( QT_QFONT_TOSTRING )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      hb_retc( ( p )->toString().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_TOSTRING FP=hb_retc( ( p )->toString().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * bool underline () const
 */
HB_FUNC( QT_QFONT_UNDERLINE )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      hb_retl( ( p )->underline() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_UNDERLINE FP=hb_retl( ( p )->underline() ); p is NULL" ) );
   }
}

/*
 * int weight () const
 */
HB_FUNC( QT_QFONT_WEIGHT )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      hb_retni( ( p )->weight() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_WEIGHT FP=hb_retni( ( p )->weight() ); p is NULL" ) );
   }
}

/*
 * qreal wordSpacing () const
 */
HB_FUNC( QT_QFONT_WORDSPACING )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      hb_retnd( ( p )->wordSpacing() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_WORDSPACING FP=hb_retnd( ( p )->wordSpacing() ); p is NULL" ) );
   }
}

/*
 * void cleanup ()
 */
HB_FUNC( QT_QFONT_CLEANUP )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      ( p )->cleanup();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_CLEANUP FP=( p )->cleanup(); p is NULL" ) );
   }
}

/*
 * void initialize ()
 */
HB_FUNC( QT_QFONT_INITIALIZE )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      ( p )->initialize();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_INITIALIZE FP=( p )->initialize(); p is NULL" ) );
   }
}

/*
 * void insertSubstitution ( const QString & familyName, const QString & substituteName )
 */
HB_FUNC( QT_QFONT_INSERTSUBSTITUTION )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      ( p )->insertSubstitution( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_INSERTSUBSTITUTION FP=( p )->insertSubstitution( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ); p is NULL" ) );
   }
}

/*
 * void insertSubstitutions ( const QString & familyName, const QStringList & substituteNames )
 */
HB_FUNC( QT_QFONT_INSERTSUBSTITUTIONS )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      ( p )->insertSubstitutions( hbqt_par_QString( 2 ), *hbqt_par_QStringList( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_INSERTSUBSTITUTIONS FP=( p )->insertSubstitutions( hbqt_par_QString( 2 ), *hbqt_par_QStringList( 3 ) ); p is NULL" ) );
   }
}

/*
 * void removeSubstitution ( const QString & familyName )
 */
HB_FUNC( QT_QFONT_REMOVESUBSTITUTION )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      ( p )->removeSubstitution( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_REMOVESUBSTITUTION FP=( p )->removeSubstitution( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * QString substitute ( const QString & familyName )
 */
HB_FUNC( QT_QFONT_SUBSTITUTE )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      hb_retc( ( p )->substitute( hbqt_par_QString( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_SUBSTITUTE FP=hb_retc( ( p )->substitute( hbqt_par_QString( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QStringList substitutes ( const QString & familyName )
 */
HB_FUNC( QT_QFONT_SUBSTITUTES )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->substitutes( hbqt_par_QString( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_SUBSTITUTES FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->substitutes( hbqt_par_QString( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * QStringList substitutions ()
 */
HB_FUNC( QT_QFONT_SUBSTITUTIONS )
{
   QFont * p = hbqt_par_QFont( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->substitutions() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONT_SUBSTITUTIONS FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->substitutions() ), true ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
