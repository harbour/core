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
#include "hbqtgui.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  enum Shadow { Plain, Raised, Sunken }
 *  enum Shape { NoFrame, Box, Panel, StyledPanel, ..., WinPanel }
 *  enum StyleMask { Shadow_Mask, Shape_Mask }
 */

#include <QtCore/QPointer>

#include <QtGui/QFrame>


/*
 * QFrame ( QWidget * parent = 0, Qt::WindowFlags f = 0 )
 * ~QFrame ()
 */

typedef struct
{
   QPointer< QFrame > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QFrame;

HBQT_GC_FUNC( hbqt_gcRelease_QFrame )
{
   QFrame  * ph = NULL ;
   HBQT_GC_T_QFrame * p = ( HBQT_GC_T_QFrame * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QFrame   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QFrame   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QFrame          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QFrame    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QFrame    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QFrame( void * pObj, bool bNew )
{
   HBQT_GC_T_QFrame * p = ( HBQT_GC_T_QFrame * ) hb_gcAllocate( sizeof( HBQT_GC_T_QFrame ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QFrame >( ( QFrame * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QFrame;
   p->type = HBQT_TYPE_QFrame;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QFrame  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QFrame", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QFRAME )
{
   QFrame * pObj = NULL;

   pObj = new QFrame( hbqt_par_QWidget( 1 ), ( Qt::WindowFlags ) hb_parni( 2 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QFrame( ( void * ) pObj, true ) );
}

/*
 * QRect frameRect () const
 */
HB_FUNC( QT_QFRAME_FRAMERECT )
{
   QFrame * p = hbqt_par_QFrame( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QRect( new QRect( ( p )->frameRect() ), true ) );
   }
}

/*
 * Shadow frameShadow () const
 */
HB_FUNC( QT_QFRAME_FRAMESHADOW )
{
   QFrame * p = hbqt_par_QFrame( 1 );
   if( p )
   {
      hb_retni( ( QFrame::Shadow ) ( p )->frameShadow() );
   }
}

/*
 * Shape frameShape () const
 */
HB_FUNC( QT_QFRAME_FRAMESHAPE )
{
   QFrame * p = hbqt_par_QFrame( 1 );
   if( p )
   {
      hb_retni( ( QFrame::Shape ) ( p )->frameShape() );
   }
}

/*
 * int frameStyle () const
 */
HB_FUNC( QT_QFRAME_FRAMESTYLE )
{
   QFrame * p = hbqt_par_QFrame( 1 );
   if( p )
   {
      hb_retni( ( p )->frameStyle() );
   }
}

/*
 * int frameWidth () const
 */
HB_FUNC( QT_QFRAME_FRAMEWIDTH )
{
   QFrame * p = hbqt_par_QFrame( 1 );
   if( p )
   {
      hb_retni( ( p )->frameWidth() );
   }
}

/*
 * int lineWidth () const
 */
HB_FUNC( QT_QFRAME_LINEWIDTH )
{
   QFrame * p = hbqt_par_QFrame( 1 );
   if( p )
   {
      hb_retni( ( p )->lineWidth() );
   }
}

/*
 * int midLineWidth () const
 */
HB_FUNC( QT_QFRAME_MIDLINEWIDTH )
{
   QFrame * p = hbqt_par_QFrame( 1 );
   if( p )
   {
      hb_retni( ( p )->midLineWidth() );
   }
}

/*
 * void setFrameRect ( const QRect & )
 */
HB_FUNC( QT_QFRAME_SETFRAMERECT )
{
   QFrame * p = hbqt_par_QFrame( 1 );
   if( p )
   {
      ( p )->setFrameRect( *hbqt_par_QRect( 2 ) );
   }
}

/*
 * void setFrameShadow ( Shadow )
 */
HB_FUNC( QT_QFRAME_SETFRAMESHADOW )
{
   QFrame * p = hbqt_par_QFrame( 1 );
   if( p )
   {
      ( p )->setFrameShadow( ( QFrame::Shadow ) hb_parni( 2 ) );
   }
}

/*
 * void setFrameShape ( Shape )
 */
HB_FUNC( QT_QFRAME_SETFRAMESHAPE )
{
   QFrame * p = hbqt_par_QFrame( 1 );
   if( p )
   {
      ( p )->setFrameShape( ( QFrame::Shape ) hb_parni( 2 ) );
   }
}

/*
 * void setFrameStyle ( int style )
 */
HB_FUNC( QT_QFRAME_SETFRAMESTYLE )
{
   QFrame * p = hbqt_par_QFrame( 1 );
   if( p )
   {
      ( p )->setFrameStyle( hb_parni( 2 ) );
   }
}

/*
 * void setLineWidth ( int )
 */
HB_FUNC( QT_QFRAME_SETLINEWIDTH )
{
   QFrame * p = hbqt_par_QFrame( 1 );
   if( p )
   {
      ( p )->setLineWidth( hb_parni( 2 ) );
   }
}

/*
 * void setMidLineWidth ( int )
 */
HB_FUNC( QT_QFRAME_SETMIDLINEWIDTH )
{
   QFrame * p = hbqt_par_QFrame( 1 );
   if( p )
   {
      ( p )->setMidLineWidth( hb_parni( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
