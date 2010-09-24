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
#include "hbqtnetwork.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  Constructed[ 5/5 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtNetwork/QHttpResponseHeader>


/* QHttpResponseHeader ()
 * QHttpResponseHeader ( const QHttpResponseHeader & header )
 * QHttpResponseHeader ( const QString & str )
 * QHttpResponseHeader ( int code, const QString & text = QString(), int majorVer = 1, int minorVer = 1 )
 */

typedef struct
{
   QHttpResponseHeader * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QHttpResponseHeader;

HBQT_GC_FUNC( hbqt_gcRelease_QHttpResponseHeader )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QHttpResponseHeader   /.\\", p->ph ) );
         delete ( ( QHttpResponseHeader * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QHttpResponseHeader   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QHttpResponseHeader    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QHttpResponseHeader    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QHttpResponseHeader( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QHttpResponseHeader * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QHttpResponseHeader;
   p->type = HBQT_TYPE_QHttpResponseHeader;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QHttpResponseHeader", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QHttpResponseHeader", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QHTTPRESPONSEHEADER )
{
   QHttpResponseHeader * pObj = NULL;

   pObj = new QHttpResponseHeader() ;

   hb_retptrGC( hbqt_gcAllocate_QHttpResponseHeader( ( void * ) pObj, true ) );
}

/*
 * virtual int majorVersion () const
 */
HB_FUNC( QT_QHTTPRESPONSEHEADER_MAJORVERSION )
{
   QHttpResponseHeader * p = hbqt_par_QHttpResponseHeader( 1 );
   if( p )
   {
      hb_retni( ( p )->majorVersion() );
   }
}

/*
 * virtual int minorVersion () const
 */
HB_FUNC( QT_QHTTPRESPONSEHEADER_MINORVERSION )
{
   QHttpResponseHeader * p = hbqt_par_QHttpResponseHeader( 1 );
   if( p )
   {
      hb_retni( ( p )->minorVersion() );
   }
}

/*
 * QString reasonPhrase () const
 */
HB_FUNC( QT_QHTTPRESPONSEHEADER_REASONPHRASE )
{
   QHttpResponseHeader * p = hbqt_par_QHttpResponseHeader( 1 );
   if( p )
   {
      hb_retstr_utf8( ( p )->reasonPhrase().toUtf8().data() );
   }
}

/*
 * void setStatusLine ( int code, const QString & text = QString(), int majorVer = 1, int minorVer = 1 )
 */
HB_FUNC( QT_QHTTPRESPONSEHEADER_SETSTATUSLINE )
{
   QHttpResponseHeader * p = hbqt_par_QHttpResponseHeader( 1 );
   if( p )
   {
      void * pText;
      ( p )->setStatusLine( hb_parni( 2 ), hb_parstr_utf8( 3, &pText, NULL ), hb_parnidef( 4, 1 ), hb_parnidef( 5, 1 ) );
      hb_strfree( pText );
   }
}

/*
 * int statusCode () const
 */
HB_FUNC( QT_QHTTPRESPONSEHEADER_STATUSCODE )
{
   QHttpResponseHeader * p = hbqt_par_QHttpResponseHeader( 1 );
   if( p )
   {
      hb_retni( ( p )->statusCode() );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
