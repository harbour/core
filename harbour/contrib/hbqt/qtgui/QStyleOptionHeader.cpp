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
 *  enum SectionPosition { Beginning, Middle, End, OnlyOneSection }
 *  enum SelectedPosition { NotAdjacent, NextIsSelected, PreviousIsSelected, NextAndPreviousAreSelected }
 *  enum SortIndicator { None, SortUp, SortDown }
 *  enum StyleOptionType { Type }
 *  enum StyleOptionVersion { Version }
 */

#include <QtCore/QPointer>

#include <QtGui/QStyleOptionHeader>


/*
 * QStyleOptionHeader ()
 * QStyleOptionHeader ( const QStyleOptionHeader & other )
 */

QT_G_FUNC( release_QStyleOptionHeader )
{
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "release_QStyleOptionHeader          %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );  OutputDebugString( str );
#endif
   void * ph = ( void * ) Cargo;
   if( ph )
   {
      ( ( QStyleOptionHeader * ) ph )->~QStyleOptionHeader();
      ph = NULL;
   }
   else
   {
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "! ph____QStyleOptionHeader" );  OutputDebugString( str );
#endif
   }
}

HB_FUNC( QT_QSTYLEOPTIONHEADER )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), gcFuncs() );
   void * pObj = NULL;
#if defined(__debug__)
hb_snprintf( str, sizeof(str), "   GC:  new QStyleOptionHeader          %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );  OutputDebugString( str );
#endif

   pObj = ( QStyleOptionHeader* ) new QStyleOptionHeader() ;

#if defined(__debug__)
hb_snprintf( str, sizeof(str), "   GC:                                  %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );  OutputDebugString( str );
#endif
   p->ph = pObj;
   p->func = release_QStyleOptionHeader;

   hb_retptrGC( p );
}
/*
 * QIcon icon
 */
HB_FUNC( QT_QSTYLEOPTIONHEADER_ICON )
{
   hb_retptrGC( hbqt_ptrTOgcpointer( new QIcon( hbqt_par_QStyleOptionHeader( 1 )->icon ), release_QIcon ) );
}

/*
 * Qt::Alignment iconAlignment
 */
HB_FUNC( QT_QSTYLEOPTIONHEADER_ICONALIGNMENT )
{
   hb_retni( ( Qt::Alignment ) hbqt_par_QStyleOptionHeader( 1 )->iconAlignment );
}

/*
 * Qt::Orientation orientation
 */
HB_FUNC( QT_QSTYLEOPTIONHEADER_ORIENTATION )
{
   hb_retni( ( Qt::Orientation ) hbqt_par_QStyleOptionHeader( 1 )->orientation );
}

/*
 * SectionPosition position
 */
HB_FUNC( QT_QSTYLEOPTIONHEADER_POSITION )
{
   hb_retni( ( QStyleOptionHeader::SectionPosition ) hbqt_par_QStyleOptionHeader( 1 )->position );
}

/*
 * int section
 */
HB_FUNC( QT_QSTYLEOPTIONHEADER_SECTION )
{
   hb_retni( hbqt_par_QStyleOptionHeader( 1 )->section );
}

/*
 * SelectedPosition selectedPosition
 */
HB_FUNC( QT_QSTYLEOPTIONHEADER_SELECTEDPOSITION )
{
   hb_retni( ( QStyleOptionHeader::SelectedPosition ) hbqt_par_QStyleOptionHeader( 1 )->selectedPosition );
}

/*
 * SortIndicator sortIndicator
 */
HB_FUNC( QT_QSTYLEOPTIONHEADER_SORTINDICATOR )
{
   hb_retni( ( QStyleOptionHeader::SortIndicator ) hbqt_par_QStyleOptionHeader( 1 )->sortIndicator );
}

/*
 * QString text
 */
HB_FUNC( QT_QSTYLEOPTIONHEADER_TEXT )
{
   hb_retc( hbqt_par_QStyleOptionHeader( 1 )->text.toLatin1().data() );
}

/*
 * Qt::Alignment textAlignment
 */
HB_FUNC( QT_QSTYLEOPTIONHEADER_TEXTALIGNMENT )
{
   hb_retni( ( Qt::Alignment ) hbqt_par_QStyleOptionHeader( 1 )->textAlignment );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
