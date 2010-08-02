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
#include "hbqtgui.h"
#include "hbqtcore_garbage.h"
#include "hbqtcore.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include <QtGui/QTextFrame>
#include <QtGui/QTextCursor>


/*
 * QTextFrame ( QTextDocument * document )
 * ~QTextFrame ()
 */

typedef struct
{
   QPointer< QTextFrame > ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QTextFrame;

QT_G_FUNC( hbqt_gcRelease_QTextFrame )
{
   QTextFrame  * ph = NULL ;
   QGC_POINTER_QTextFrame * p = ( QGC_POINTER_QTextFrame * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QTextFrame   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QTextFrame   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QTextFrame          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QTextFrame    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QTextFrame    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextFrame( void * pObj, bool bNew )
{
   QGC_POINTER_QTextFrame * p = ( QGC_POINTER_QTextFrame * ) hb_gcAllocate( sizeof( QGC_POINTER_QTextFrame ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QTextFrame >( ( QTextFrame * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextFrame;
   p->type = HBQT_TYPE_QTextFrame;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QTextFrame  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QTextFrame", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QTEXTFRAME )
{
   QTextFrame * pObj = NULL;

   pObj =  new QTextFrame( hbqt_par_QTextDocument( 1 ) ) ;

   hb_retptrGC( hbqt_gcAllocate_QTextFrame( ( void * ) pObj, true ) );
}

/*
 * QList<QTextFrame *> childFrames () const
 */
HB_FUNC( QT_QTEXTFRAME_CHILDFRAMES )
{
   QTextFrame * p = hbqt_par_QTextFrame( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QList( new QList<QTextFrame *>( ( p )->childFrames() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAME_CHILDFRAMES FP=hb_retptrGC( hbqt_gcAllocate_QList( new QList<QTextFrame *>( ( p )->childFrames() ), true ) ); p is NULL" ) );
   }
}

/*
 * QTextCursor firstCursorPosition () const
 */
HB_FUNC( QT_QTEXTFRAME_FIRSTCURSORPOSITION )
{
   QTextFrame * p = hbqt_par_QTextFrame( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( ( p )->firstCursorPosition() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAME_FIRSTCURSORPOSITION FP=hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( ( p )->firstCursorPosition() ), true ) ); p is NULL" ) );
   }
}

/*
 * int firstPosition () const
 */
HB_FUNC( QT_QTEXTFRAME_FIRSTPOSITION )
{
   QTextFrame * p = hbqt_par_QTextFrame( 1 );
   if( p )
      hb_retni( ( p )->firstPosition() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAME_FIRSTPOSITION FP=hb_retni( ( p )->firstPosition() ); p is NULL" ) );
   }
}

/*
 * QTextFrameFormat frameFormat () const
 */
HB_FUNC( QT_QTEXTFRAME_FRAMEFORMAT )
{
   QTextFrame * p = hbqt_par_QTextFrame( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextFrameFormat( new QTextFrameFormat( ( p )->frameFormat() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAME_FRAMEFORMAT FP=hb_retptrGC( hbqt_gcAllocate_QTextFrameFormat( new QTextFrameFormat( ( p )->frameFormat() ), true ) ); p is NULL" ) );
   }
}

/*
 * QTextCursor lastCursorPosition () const
 */
HB_FUNC( QT_QTEXTFRAME_LASTCURSORPOSITION )
{
   QTextFrame * p = hbqt_par_QTextFrame( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( ( p )->lastCursorPosition() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAME_LASTCURSORPOSITION FP=hb_retptrGC( hbqt_gcAllocate_QTextCursor( new QTextCursor( ( p )->lastCursorPosition() ), true ) ); p is NULL" ) );
   }
}

/*
 * int lastPosition () const
 */
HB_FUNC( QT_QTEXTFRAME_LASTPOSITION )
{
   QTextFrame * p = hbqt_par_QTextFrame( 1 );
   if( p )
      hb_retni( ( p )->lastPosition() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAME_LASTPOSITION FP=hb_retni( ( p )->lastPosition() ); p is NULL" ) );
   }
}

/*
 * QTextFrame * parentFrame () const
 */
HB_FUNC( QT_QTEXTFRAME_PARENTFRAME )
{
   QTextFrame * p = hbqt_par_QTextFrame( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextFrame( ( p )->parentFrame(), false ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAME_PARENTFRAME FP=hb_retptrGC( hbqt_gcAllocate_QTextFrame( ( p )->parentFrame(), false ) ); p is NULL" ) );
   }
}

/*
 * void setFrameFormat ( const QTextFrameFormat & format )
 */
HB_FUNC( QT_QTEXTFRAME_SETFRAMEFORMAT )
{
   QTextFrame * p = hbqt_par_QTextFrame( 1 );
   if( p )
      ( p )->setFrameFormat( *hbqt_par_QTextFrameFormat( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAME_SETFRAMEFORMAT FP=( p )->setFrameFormat( *hbqt_par_QTextFrameFormat( 2 ) ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
