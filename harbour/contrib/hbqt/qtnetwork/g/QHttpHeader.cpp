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
 *  Constructed[ 18/19 [ 94.74% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  void setValues ( const QList<QPair<QString, QString> > & values )
 */

#include <QtCore/QPointer>

#include <QtNetwork/QHttpHeader>


/*
 * QHttpHeader ()
 * QHttpHeader ( const QHttpHeader & header )
 * QHttpHeader ( const QString & str )
 * virtual ~QHttpHeader ()
 */

typedef struct
{
   QHttpHeader * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QHttpHeader;

HBQT_GC_FUNC( hbqt_gcRelease_QHttpHeader )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QHttpHeader( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QHttpHeader * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QHttpHeader;
   p->type = HBQT_TYPE_QHttpHeader;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QHttpHeader", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QHttpHeader", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QHTTPHEADER )
{

}

/*
 * void addValue ( const QString & key, const QString & value )
 */
HB_FUNC( QT_QHTTPHEADER_ADDVALUE )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
      ( p )->addValue( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHTTPHEADER_ADDVALUE FP=( p )->addValue( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ); p is NULL" ) );
   }
}

/*
 * QStringList allValues ( const QString & key ) const
 */
HB_FUNC( QT_QHTTPHEADER_ALLVALUES )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->allValues( hbqt_par_QString( 2 ) ) ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHTTPHEADER_ALLVALUES FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->allValues( hbqt_par_QString( 2 ) ) ), true ) ); p is NULL" ) );
   }
}

/*
 * uint contentLength () const
 */
HB_FUNC( QT_QHTTPHEADER_CONTENTLENGTH )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
      hb_retni( ( p )->contentLength() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHTTPHEADER_CONTENTLENGTH FP=hb_retni( ( p )->contentLength() ); p is NULL" ) );
   }
}

/*
 * QString contentType () const
 */
HB_FUNC( QT_QHTTPHEADER_CONTENTTYPE )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
      hb_retc( ( p )->contentType().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHTTPHEADER_CONTENTTYPE FP=hb_retc( ( p )->contentType().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * bool hasContentLength () const
 */
HB_FUNC( QT_QHTTPHEADER_HASCONTENTLENGTH )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
      hb_retl( ( p )->hasContentLength() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHTTPHEADER_HASCONTENTLENGTH FP=hb_retl( ( p )->hasContentLength() ); p is NULL" ) );
   }
}

/*
 * bool hasContentType () const
 */
HB_FUNC( QT_QHTTPHEADER_HASCONTENTTYPE )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
      hb_retl( ( p )->hasContentType() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHTTPHEADER_HASCONTENTTYPE FP=hb_retl( ( p )->hasContentType() ); p is NULL" ) );
   }
}

/*
 * bool hasKey ( const QString & key ) const
 */
HB_FUNC( QT_QHTTPHEADER_HASKEY )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
      hb_retl( ( p )->hasKey( hbqt_par_QString( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHTTPHEADER_HASKEY FP=hb_retl( ( p )->hasKey( hbqt_par_QString( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QHTTPHEADER_ISVALID )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHTTPHEADER_ISVALID FP=hb_retl( ( p )->isValid() ); p is NULL" ) );
   }
}

/*
 * QStringList keys () const
 */
HB_FUNC( QT_QHTTPHEADER_KEYS )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->keys() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHTTPHEADER_KEYS FP=hb_retptrGC( hbqt_gcAllocate_QStringList( new QStringList( ( p )->keys() ), true ) ); p is NULL" ) );
   }
}

/*
 * virtual int majorVersion () const = 0
 */
HB_FUNC( QT_QHTTPHEADER_MAJORVERSION )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
      hb_retni( ( p )->majorVersion() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHTTPHEADER_MAJORVERSION FP=hb_retni( ( p )->majorVersion() ); p is NULL" ) );
   }
}

/*
 * virtual int minorVersion () const = 0
 */
HB_FUNC( QT_QHTTPHEADER_MINORVERSION )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
      hb_retni( ( p )->minorVersion() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHTTPHEADER_MINORVERSION FP=hb_retni( ( p )->minorVersion() ); p is NULL" ) );
   }
}

/*
 * void removeAllValues ( const QString & key )
 */
HB_FUNC( QT_QHTTPHEADER_REMOVEALLVALUES )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
      ( p )->removeAllValues( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHTTPHEADER_REMOVEALLVALUES FP=( p )->removeAllValues( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void removeValue ( const QString & key )
 */
HB_FUNC( QT_QHTTPHEADER_REMOVEVALUE )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
      ( p )->removeValue( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHTTPHEADER_REMOVEVALUE FP=( p )->removeValue( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setContentLength ( int len )
 */
HB_FUNC( QT_QHTTPHEADER_SETCONTENTLENGTH )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
      ( p )->setContentLength( hb_parni( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHTTPHEADER_SETCONTENTLENGTH FP=( p )->setContentLength( hb_parni( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setContentType ( const QString & type )
 */
HB_FUNC( QT_QHTTPHEADER_SETCONTENTTYPE )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
      ( p )->setContentType( hbqt_par_QString( 2 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHTTPHEADER_SETCONTENTTYPE FP=( p )->setContentType( hbqt_par_QString( 2 ) ); p is NULL" ) );
   }
}

/*
 * void setValue ( const QString & key, const QString & value )
 */
HB_FUNC( QT_QHTTPHEADER_SETVALUE )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
      ( p )->setValue( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHTTPHEADER_SETVALUE FP=( p )->setValue( hbqt_par_QString( 2 ), hbqt_par_QString( 3 ) ); p is NULL" ) );
   }
}

/*
 * virtual QString toString () const
 */
HB_FUNC( QT_QHTTPHEADER_TOSTRING )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
      hb_retc( ( p )->toString().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHTTPHEADER_TOSTRING FP=hb_retc( ( p )->toString().toAscii().data() ); p is NULL" ) );
   }
}

/*
 * QString value ( const QString & key ) const
 */
HB_FUNC( QT_QHTTPHEADER_VALUE )
{
   QHttpHeader * p = hbqt_par_QHttpHeader( 1 );
   if( p )
      hb_retc( ( p )->value( hbqt_par_QString( 2 ) ).toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QHTTPHEADER_VALUE FP=hb_retc( ( p )->value( hbqt_par_QString( 2 ) ).toAscii().data() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
