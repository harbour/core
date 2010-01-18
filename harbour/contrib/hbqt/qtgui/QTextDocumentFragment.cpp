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

#include <QtCore/QPointer>

#include <QtGui/QTextDocumentFragment>


/*
 * QTextDocumentFragment ()
 * QTextDocumentFragment ( const QTextDocument * document )
 * QTextDocumentFragment ( const QTextCursor & cursor )
 * QTextDocumentFragment ( const QTextDocumentFragment & other )
 * ~QTextDocumentFragment ()
 */

typedef struct
{
  void * ph;
  bool bNew;
  QT_G_FUNC_PTR func;
} QGC_POINTER_QTextDocumentFragment;

QT_G_FUNC( hbqt_gcRelease_QTextDocumentFragment )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QTextDocumentFragment * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QTextDocumentFragment      ph=%p %i B %i KB", p->ph, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_QTextDocumentFragment       Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_QTextDocumentFragment       Object not created with - new" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextDocumentFragment( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextDocumentFragment;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QTextDocumentFragment      ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QTEXTDOCUMENTFRAGMENT )
{
   void * pObj = NULL;

   pObj = ( QTextDocumentFragment* ) new QTextDocumentFragment() ;

   hb_retptrGC( hbqt_gcAllocate_QTextDocumentFragment( pObj, true ) );
}

/*
 * bool isEmpty () const
 */
HB_FUNC( QT_QTEXTDOCUMENTFRAGMENT_ISEMPTY )
{
   hb_retl( hbqt_par_QTextDocumentFragment( 1 )->isEmpty() );
}

/*
 * QString toHtml ( const QByteArray & encoding ) const
 */
HB_FUNC( QT_QTEXTDOCUMENTFRAGMENT_TOHTML )
{
   hb_retc( hbqt_par_QTextDocumentFragment( 1 )->toHtml( *hbqt_par_QByteArray( 2 ) ).toAscii().data() );
}

/*
 * QString toHtml () const
 */
HB_FUNC( QT_QTEXTDOCUMENTFRAGMENT_TOHTML_1 )
{
   hb_retc( hbqt_par_QTextDocumentFragment( 1 )->toHtml().toAscii().data() );
}

/*
 * QString toPlainText () const
 */
HB_FUNC( QT_QTEXTDOCUMENTFRAGMENT_TOPLAINTEXT )
{
   hb_retc( hbqt_par_QTextDocumentFragment( 1 )->toPlainText().toAscii().data() );
}

/*
 * QTextDocumentFragment fromHtml ( const QString & text )
 */
HB_FUNC( QT_QTEXTDOCUMENTFRAGMENT_FROMHTML )
{
   hb_retptrGC( hbqt_gcAllocate_QTextDocumentFragment( new QTextDocumentFragment( hbqt_par_QTextDocumentFragment( 1 )->fromHtml( hbqt_par_QString( 2 ) ) ), true ) );
}

/*
 * QTextDocumentFragment fromHtml ( const QString & text, const QTextDocument * resourceProvider )
 */
HB_FUNC( QT_QTEXTDOCUMENTFRAGMENT_FROMHTML_1 )
{
   hb_retptrGC( hbqt_gcAllocate_QTextDocumentFragment( new QTextDocumentFragment( hbqt_par_QTextDocumentFragment( 1 )->fromHtml( hbqt_par_QString( 2 ), hbqt_par_QTextDocument( 3 ) ) ), true ) );
}

/*
 * QTextDocumentFragment fromPlainText ( const QString & plainText )
 */
HB_FUNC( QT_QTEXTDOCUMENTFRAGMENT_FROMPLAINTEXT )
{
   hb_retptrGC( hbqt_gcAllocate_QTextDocumentFragment( new QTextDocumentFragment( hbqt_par_QTextDocumentFragment( 1 )->fromPlainText( hbqt_par_QString( 2 ) ) ), true ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
