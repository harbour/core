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
 *  Constructed[ 5/5 [ 100.00% ] ]
 *
 *
 *  *** Commented out protostypes ***
 *
 *  //const QByteArray & buffer () const
 */

#include <QtCore/QPointer>

#include <QtCore/QBuffer>


/*
 * QBuffer ( QObject * parent = 0 )
 * QBuffer ( QByteArray * byteArray, QObject * parent = 0 )
 * ~QBuffer ()
 */

typedef struct
{
   QPointer< QBuffer > ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QBuffer;

HBQT_GC_FUNC( hbqt_gcRelease_QBuffer )
{
   QBuffer  * ph = NULL ;
   HBQT_GC_T_QBuffer * p = ( HBQT_GC_T_QBuffer * ) Cargo;

   if( p && p->bNew && p->ph )
   {
      ph = p->ph;
      if( ph )
      {
         const QMetaObject * m = ( ph )->metaObject();
         if( ( QString ) m->className() != ( QString ) "QObject" )
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QBuffer   /.\\   ", (void*) ph, (void*) p->ph ) );
            delete ( p->ph );
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p %p YES_rel_QBuffer   \\./   ", (void*) ph, (void*) p->ph ) );
            p->ph = NULL;
         }
         else
         {
            HB_TRACE( HB_TR_DEBUG, ( "ph=%p NO__rel_QBuffer          ", ph ) );
            p->ph = NULL;
         }
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QBuffer    :     Object already deleted!", ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QBuffer    :    Object not created with new=true", ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QBuffer( void * pObj, bool bNew )
{
   HBQT_GC_T_QBuffer * p = ( HBQT_GC_T_QBuffer * ) hb_gcAllocate( sizeof( HBQT_GC_T_QBuffer ), hbqt_gcFuncs() );

   new( & p->ph ) QPointer< QBuffer >( ( QBuffer * ) pObj );
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QBuffer;
   p->type = HBQT_TYPE_QBuffer;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QBuffer  under p->pq", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QBuffer", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QBUFFER )
{
   QBuffer * pObj = NULL;

   pObj = new QBuffer() ;

   hb_retptrGC( hbqt_gcAllocate_QBuffer( ( void * ) pObj, true ) );
}

/*
 * QByteArray & buffer ()
 */
HB_FUNC( QT_QBUFFER_BUFFER )
{
   QBuffer * p = hbqt_par_QBuffer( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->buffer() ), true ) );
   }
}

/*
 * const QByteArray & data () const
 */
HB_FUNC( QT_QBUFFER_DATA )
{
   QBuffer * p = hbqt_par_QBuffer( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QByteArray( new QByteArray( ( p )->data() ), true ) );
   }
}

/*
 * void setBuffer ( QByteArray * byteArray )
 */
HB_FUNC( QT_QBUFFER_SETBUFFER )
{
   QBuffer * p = hbqt_par_QBuffer( 1 );
   if( p )
   {
      ( p )->setBuffer( hbqt_par_QByteArray( 2 ) );
   }
}

/*
 * void setData ( const char * data, int size )
 */
HB_FUNC( QT_QBUFFER_SETDATA )
{
   QBuffer * p = hbqt_par_QBuffer( 1 );
   if( p )
   {
      ( p )->setData( hbqt_par_char( 2 ), hb_parni( 3 ) );
   }
}

/*
 * void setData ( const QByteArray & data )
 */
HB_FUNC( QT_QBUFFER_SETDATA_1 )
{
   QBuffer * p = hbqt_par_QBuffer( 1 );
   if( p )
   {
      ( p )->setData( *hbqt_par_QByteArray( 2 ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
