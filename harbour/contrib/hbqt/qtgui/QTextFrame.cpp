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
 *  Constructed[ 7/10 [ 70.00% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QList<QTextFrame *> childFrames () const
 *
 *  *** Commented out protos which construct fine but do not compile ***
 *
 *  //iterator begin () const
 *  //iterator end () const
 */

#include <QtCore/QPointer>

#include <QtGui/QTextFrame>
#include <QtGui/QTextCursor>


/*
 * QTextFrame ( QTextDocument * document )
 * ~QTextFrame ()
 */

typedef struct
{
  void * ph;
  QT_G_FUNC_PTR func;
  QPointer< QTextFrame > pq;
} QGC_POINTER_QTextFrame;

QT_G_FUNC( release_QTextFrame )
{
   QGC_POINTER_QTextFrame * p = ( QGC_POINTER_QTextFrame * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QTextFrame                   p=%p", p));
   HB_TRACE( HB_TR_DEBUG, ( "release_QTextFrame                  ph=%p pq=%p", p->ph, (void *)(p->pq)));

   if( p && p->ph && p->pq )
   {
      const QMetaObject * m = ( ( QObject * ) p->ph )->metaObject();
      if( ( QString ) m->className() != ( QString ) "QObject" )
      {
         ( ( QTextFrame * ) p->ph )->~QTextFrame();
         p->ph = NULL;
         HB_TRACE( HB_TR_DEBUG, ( "release_QTextFrame                  Object deleted!" ) );
         #if defined( __HB_DEBUG__ )
            hbqt_debug( "  YES release_QTextFrame                  %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() );
         #endif
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "release_QTextFrame                  Object Name Missing!" ) );
         #if defined( __HB_DEBUG__ )
            hbqt_debug( "  NO  release_QTextFrame" );
         #endif
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "release_QTextFrame                  Object Allready deleted!" ) );
      #if defined( __HB_DEBUG__ )
         hbqt_debug( "  DEL release_QTextFrame" );
      #endif
   }
}

void * gcAllocate_QTextFrame( void * pObj )
{
   QGC_POINTER_QTextFrame * p = ( QGC_POINTER_QTextFrame * ) hb_gcAllocate( sizeof( QGC_POINTER_QTextFrame ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QTextFrame;
   new( & p->pq ) QPointer< QTextFrame >( ( QTextFrame * ) pObj );
   #if defined( __HB_DEBUG__ )
      hbqt_debug( "          new_QTextFrame                  %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() );
   #endif
   return( p );
}

HB_FUNC( QT_QTEXTFRAME )
{
   void * pObj = NULL;

   pObj = ( QTextFrame* ) new QTextFrame( hbqt_par_QTextDocument( 1 ) ) ;

   hb_retptrGC( gcAllocate_QTextFrame( pObj ) );
}
/*
 * QTextCursor firstCursorPosition () const
 */
HB_FUNC( QT_QTEXTFRAME_FIRSTCURSORPOSITION )
{
   hb_retptrGC( gcAllocate_QTextCursor( new QTextCursor( hbqt_par_QTextFrame( 1 )->firstCursorPosition() ) ) );
}

/*
 * int firstPosition () const
 */
HB_FUNC( QT_QTEXTFRAME_FIRSTPOSITION )
{
   hb_retni( hbqt_par_QTextFrame( 1 )->firstPosition() );
}

/*
 * QTextFrameFormat frameFormat () const
 */
HB_FUNC( QT_QTEXTFRAME_FRAMEFORMAT )
{
   hb_retptrGC( gcAllocate_QTextFrameFormat( new QTextFrameFormat( hbqt_par_QTextFrame( 1 )->frameFormat() ) ) );
}

/*
 * QTextCursor lastCursorPosition () const
 */
HB_FUNC( QT_QTEXTFRAME_LASTCURSORPOSITION )
{
   hb_retptrGC( gcAllocate_QTextCursor( new QTextCursor( hbqt_par_QTextFrame( 1 )->lastCursorPosition() ) ) );
}

/*
 * int lastPosition () const
 */
HB_FUNC( QT_QTEXTFRAME_LASTPOSITION )
{
   hb_retni( hbqt_par_QTextFrame( 1 )->lastPosition() );
}

/*
 * QTextFrame * parentFrame () const
 */
HB_FUNC( QT_QTEXTFRAME_PARENTFRAME )
{
   hb_retptr( ( QTextFrame* ) hbqt_par_QTextFrame( 1 )->parentFrame() );
}

/*
 * void setFrameFormat ( const QTextFrameFormat & format )
 */
HB_FUNC( QT_QTEXTFRAME_SETFRAMEFORMAT )
{
   hbqt_par_QTextFrame( 1 )->setFrameFormat( *hbqt_par_QTextFrameFormat( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
