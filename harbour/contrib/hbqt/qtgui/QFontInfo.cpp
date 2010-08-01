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

#include "../hbqt.h"
#include "hbqtgui_garbage.h"
#include "hbqtcore_garbage.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include <QtGui/QFontInfo>


/*
 * QFontInfo ( const QFont & font )
 * QFontInfo ( const QFontInfo & fi )
 * ~QFontInfo ()
 */

typedef struct
{
   QFontInfo * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QFontInfo;

QT_G_FUNC( hbqt_gcRelease_QFontInfo )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QFontInfo   /.\\", p->ph ) );
         delete ( ( QFontInfo * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QFontInfo   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QFontInfo    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QFontInfo    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QFontInfo( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QFontInfo * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QFontInfo;
   p->type = HBQT_TYPE_QFontInfo;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QFontInfo", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QFontInfo", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QFONTINFO )
{
   QFontInfo * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QFontInfo( *hbqt_par_QFontInfo( 1 ) ) ;
   }
   else if( hb_pcount() == 2 && HB_ISCHAR( 1 ) && HB_ISPOINTER( 2 ) )
   {
      pObj = new QFontInfo( *hbqt_par_QFont( 2 ) ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QFontInfo( ( void * ) pObj, true ) );
}

/*
 * bool bold () const
 */
HB_FUNC( QT_QFONTINFO_BOLD )
{
   QFontInfo * p = hbqt_par_QFontInfo( 1 );
   if( p )
      hb_retl( ( p )->bold() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTINFO_BOLD FP=hb_retl( ( p )->bold() ); p is NULL" ) );
   }
}

/*
 * bool exactMatch () const
 */
HB_FUNC( QT_QFONTINFO_EXACTMATCH )
{
   QFontInfo * p = hbqt_par_QFontInfo( 1 );
   if( p )
      hb_retl( ( p )->exactMatch() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTINFO_EXACTMATCH FP=hb_retl( ( p )->exactMatch() ); p is NULL" ) );
   }
}

/*
 * QString family () const
 */
HB_FUNC( QT_QFONTINFO_FAMILY )
{
   QFontInfo * p = hbqt_par_QFontInfo( 1 );
   if( p )
      hb_retc( ( p )->family().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTINFO_FAMILY FP=hb_retc( ( p )->family().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * bool fixedPitch () const
 */
HB_FUNC( QT_QFONTINFO_FIXEDPITCH )
{
   QFontInfo * p = hbqt_par_QFontInfo( 1 );
   if( p )
      hb_retl( ( p )->fixedPitch() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTINFO_FIXEDPITCH FP=hb_retl( ( p )->fixedPitch() ); p is NULL" ) );
   }
}

/*
 * bool italic () const
 */
HB_FUNC( QT_QFONTINFO_ITALIC )
{
   QFontInfo * p = hbqt_par_QFontInfo( 1 );
   if( p )
      hb_retl( ( p )->italic() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTINFO_ITALIC FP=hb_retl( ( p )->italic() ); p is NULL" ) );
   }
}

/*
 * int pixelSize () const
 */
HB_FUNC( QT_QFONTINFO_PIXELSIZE )
{
   QFontInfo * p = hbqt_par_QFontInfo( 1 );
   if( p )
      hb_retni( ( p )->pixelSize() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTINFO_PIXELSIZE FP=hb_retni( ( p )->pixelSize() ); p is NULL" ) );
   }
}

/*
 * int pointSize () const
 */
HB_FUNC( QT_QFONTINFO_POINTSIZE )
{
   QFontInfo * p = hbqt_par_QFontInfo( 1 );
   if( p )
      hb_retni( ( p )->pointSize() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTINFO_POINTSIZE FP=hb_retni( ( p )->pointSize() ); p is NULL" ) );
   }
}

/*
 * qreal pointSizeF () const
 */
HB_FUNC( QT_QFONTINFO_POINTSIZEF )
{
   QFontInfo * p = hbqt_par_QFontInfo( 1 );
   if( p )
      hb_retnd( ( p )->pointSizeF() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTINFO_POINTSIZEF FP=hb_retnd( ( p )->pointSizeF() ); p is NULL" ) );
   }
}

/*
 * bool rawMode () const
 */
HB_FUNC( QT_QFONTINFO_RAWMODE )
{
   QFontInfo * p = hbqt_par_QFontInfo( 1 );
   if( p )
      hb_retl( ( p )->rawMode() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTINFO_RAWMODE FP=hb_retl( ( p )->rawMode() ); p is NULL" ) );
   }
}

/*
 * QFont::Style style () const
 */
HB_FUNC( QT_QFONTINFO_STYLE )
{
   QFontInfo * p = hbqt_par_QFontInfo( 1 );
   if( p )
      hb_retni( ( QFont::Style ) ( p )->style() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTINFO_STYLE FP=hb_retni( ( QFont::Style ) ( p )->style() ); p is NULL" ) );
   }
}

/*
 * QFont::StyleHint styleHint () const
 */
HB_FUNC( QT_QFONTINFO_STYLEHINT )
{
   QFontInfo * p = hbqt_par_QFontInfo( 1 );
   if( p )
      hb_retni( ( QFont::StyleHint ) ( p )->styleHint() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTINFO_STYLEHINT FP=hb_retni( ( QFont::StyleHint ) ( p )->styleHint() ); p is NULL" ) );
   }
}

/*
 * int weight () const
 */
HB_FUNC( QT_QFONTINFO_WEIGHT )
{
   QFontInfo * p = hbqt_par_QFontInfo( 1 );
   if( p )
      hb_retni( ( p )->weight() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QFONTINFO_WEIGHT FP=hb_retni( ( p )->weight() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
