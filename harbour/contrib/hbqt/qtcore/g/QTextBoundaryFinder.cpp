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

#include "hbqtcore.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  flags BoundaryReasons
 *  enum BoundaryReason { NotAtBoundary, StartWord, EndWord }
 *  enum BoundaryType { Grapheme, Word, Line, Sentence }
 */

#include <QtCore/QPointer>

#include <QtCore/QTextBoundaryFinder>


/*
 * QTextBoundaryFinder ()
 * QTextBoundaryFinder ( const QTextBoundaryFinder & other )
 * QTextBoundaryFinder ( BoundaryType type, const QString & string )
 * QTextBoundaryFinder ( BoundaryType type, const QChar * chars, int length, unsigned char * buffer = 0, int bufferSize = 0 )
 * ~QTextBoundaryFinder ()
 */

typedef struct
{
   QTextBoundaryFinder * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QTextBoundaryFinder;

QT_G_FUNC( hbqt_gcRelease_QTextBoundaryFinder )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QTextBoundaryFinder   /.\\", p->ph ) );
         delete ( ( QTextBoundaryFinder * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QTextBoundaryFinder   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QTextBoundaryFinder    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QTextBoundaryFinder    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextBoundaryFinder( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QTextBoundaryFinder * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextBoundaryFinder;
   p->type = HBQT_TYPE_QTextBoundaryFinder;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QTextBoundaryFinder", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QTextBoundaryFinder", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QTEXTBOUNDARYFINDER )
{
   QTextBoundaryFinder * pObj = NULL;

   pObj =  new QTextBoundaryFinder() ;

   hb_retptrGC( hbqt_gcAllocate_QTextBoundaryFinder( ( void * ) pObj, true ) );
}

/*
 * BoundaryReasons boundaryReasons () const
 */
HB_FUNC( QT_QTEXTBOUNDARYFINDER_BOUNDARYREASONS )
{
   QTextBoundaryFinder * p = hbqt_par_QTextBoundaryFinder( 1 );
   if( p )
      hb_retni( ( QTextBoundaryFinder::BoundaryReasons ) ( p )->boundaryReasons() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBOUNDARYFINDER_BOUNDARYREASONS FP=hb_retni( ( QTextBoundaryFinder::BoundaryReasons ) ( p )->boundaryReasons() ); p is NULL" ) );
   }
}

/*
 * bool isAtBoundary () const
 */
HB_FUNC( QT_QTEXTBOUNDARYFINDER_ISATBOUNDARY )
{
   QTextBoundaryFinder * p = hbqt_par_QTextBoundaryFinder( 1 );
   if( p )
      hb_retl( ( p )->isAtBoundary() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBOUNDARYFINDER_ISATBOUNDARY FP=hb_retl( ( p )->isAtBoundary() ); p is NULL" ) );
   }
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QTEXTBOUNDARYFINDER_ISVALID )
{
   QTextBoundaryFinder * p = hbqt_par_QTextBoundaryFinder( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBOUNDARYFINDER_ISVALID FP=hb_retl( ( p )->isValid() ); p is NULL" ) );
   }
}

/*
 * int position () const
 */
HB_FUNC( QT_QTEXTBOUNDARYFINDER_POSITION )
{
   QTextBoundaryFinder * p = hbqt_par_QTextBoundaryFinder( 1 );
   if( p )
      hb_retni( ( p )->position() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBOUNDARYFINDER_POSITION FP=hb_retni( ( p )->position() ); p is NULL" ) );
   }
}

/*
 * void setPosition ( int position )
 */
HB_FUNC( QT_QTEXTBOUNDARYFINDER_SETPOSITION )
{
   QTextBoundaryFinder * p = hbqt_par_QTextBoundaryFinder( 1 );
   if( p )
      ( p )->setPosition( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBOUNDARYFINDER_SETPOSITION FP=( p )->setPosition( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * QString string () const
 */
HB_FUNC( QT_QTEXTBOUNDARYFINDER_STRING )
{
   QTextBoundaryFinder * p = hbqt_par_QTextBoundaryFinder( 1 );
   if( p )
      hb_retc( ( p )->string().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBOUNDARYFINDER_STRING FP=hb_retc( ( p )->string().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * void toEnd ()
 */
HB_FUNC( QT_QTEXTBOUNDARYFINDER_TOEND )
{
   QTextBoundaryFinder * p = hbqt_par_QTextBoundaryFinder( 1 );
   if( p )
      ( p )->toEnd();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBOUNDARYFINDER_TOEND FP=( p )->toEnd(); p is NULL" ) );
   }
}

/*
 * int toNextBoundary ()
 */
HB_FUNC( QT_QTEXTBOUNDARYFINDER_TONEXTBOUNDARY )
{
   QTextBoundaryFinder * p = hbqt_par_QTextBoundaryFinder( 1 );
   if( p )
      hb_retni( ( p )->toNextBoundary() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBOUNDARYFINDER_TONEXTBOUNDARY FP=hb_retni( ( p )->toNextBoundary() ); p is NULL" ) );
   }
}

/*
 * int toPreviousBoundary ()
 */
HB_FUNC( QT_QTEXTBOUNDARYFINDER_TOPREVIOUSBOUNDARY )
{
   QTextBoundaryFinder * p = hbqt_par_QTextBoundaryFinder( 1 );
   if( p )
      hb_retni( ( p )->toPreviousBoundary() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBOUNDARYFINDER_TOPREVIOUSBOUNDARY FP=hb_retni( ( p )->toPreviousBoundary() ); p is NULL" ) );
   }
}

/*
 * void toStart ()
 */
HB_FUNC( QT_QTEXTBOUNDARYFINDER_TOSTART )
{
   QTextBoundaryFinder * p = hbqt_par_QTextBoundaryFinder( 1 );
   if( p )
      ( p )->toStart();
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBOUNDARYFINDER_TOSTART FP=( p )->toStart(); p is NULL" ) );
   }
}

/*
 * BoundaryType type () const
 */
HB_FUNC( QT_QTEXTBOUNDARYFINDER_TYPE )
{
   QTextBoundaryFinder * p = hbqt_par_QTextBoundaryFinder( 1 );
   if( p )
      hb_retni( ( QTextBoundaryFinder::BoundaryType ) ( p )->type() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTBOUNDARYFINDER_TYPE FP=hb_retni( ( QTextBoundaryFinder::BoundaryType ) ( p )->type() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
