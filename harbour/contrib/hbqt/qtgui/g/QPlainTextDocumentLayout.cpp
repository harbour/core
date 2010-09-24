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
 *  Constructed[ 4/4 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtGui/QPlainTextDocumentLayout>


/*
 * QPlainTextDocumentLayout ( QTextDocument * document )
 * ~QPlainTextDocumentLayout ()
 */

typedef struct
{
   QPointer< QPlainTextDocumentLayout > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QPlainTextDocumentLayout;

HBQT_GC_FUNC( hbqt_gcRelease_QPlainTextDocumentLayout )
{
   QPlainTextDocumentLayout  * ph = NULL ;
   HBQT_GC_T_QPlainTextDocumentLayout * p = ( HBQT_GC_T_QPlainTextDocumentLayout * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QPlainTextDocumentLayout   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QPlainTextDocumentLayout   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QPlainTextDocumentLayout          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QPlainTextDocumentLayout    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QPlainTextDocumentLayout    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QPlainTextDocumentLayout( void * pObj, bool bNew )
{
   HBQT_GC_T_QPlainTextDocumentLayout * p = ( HBQT_GC_T_QPlainTextDocumentLayout * ) hb_gcAllocate( sizeof( HBQT_GC_T_QPlainTextDocumentLayout ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QPlainTextDocumentLayout >( ( QPlainTextDocumentLayout * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QPlainTextDocumentLayout;
   p->type = HBQT_TYPE_QPlainTextDocumentLayout;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QPlainTextDocumentLayout  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QPlainTextDocumentLayout", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QPLAINTEXTDOCUMENTLAYOUT )
{
   QPlainTextDocumentLayout * pObj = NULL;

   if( hb_pcount() == 1 && HB_ISPOINTER( 1 ) )
   {
      pObj = new QPlainTextDocumentLayout( hbqt_par_QTextDocument( 1 ) ) ;
   }

   hb_retptrGC( hbqt_gcAllocate_QPlainTextDocumentLayout( ( void * ) pObj, true ) );
}

/*
 * int cursorWidth () const
 */
HB_FUNC( QT_QPLAINTEXTDOCUMENTLAYOUT_CURSORWIDTH )
{
   QPlainTextDocumentLayout * p = hbqt_par_QPlainTextDocumentLayout( 1 );
   if( p )
   {
      hb_retni( ( p )->cursorWidth() );
   }
}

/*
 * void ensureBlockLayout ( const QTextBlock & block ) const
 */
HB_FUNC( QT_QPLAINTEXTDOCUMENTLAYOUT_ENSUREBLOCKLAYOUT )
{
   QPlainTextDocumentLayout * p = hbqt_par_QPlainTextDocumentLayout( 1 );
   if( p )
   {
      ( p )->ensureBlockLayout( *hbqt_par_QTextBlock( 2 ) );
   }
}

/*
 * void requestUpdate ()
 */
HB_FUNC( QT_QPLAINTEXTDOCUMENTLAYOUT_REQUESTUPDATE )
{
   QPlainTextDocumentLayout * p = hbqt_par_QPlainTextDocumentLayout( 1 );
   if( p )
   {
      ( p )->requestUpdate();
   }
}

/*
 * void setCursorWidth ( int width )
 */
HB_FUNC( QT_QPLAINTEXTDOCUMENTLAYOUT_SETCURSORWIDTH )
{
   QPlainTextDocumentLayout * p = hbqt_par_QPlainTextDocumentLayout( 1 );
   if( p )
   {
      ( p )->setCursorWidth( hb_parni( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
