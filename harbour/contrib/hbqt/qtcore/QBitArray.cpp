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

#include <QtCore/QBitArray>


/* QBitArray ()
 * QBitArray ( int size, bool value = false )
 * QBitArray ( const QBitArray & other )
 */

typedef struct
{
  void * ph;
  bool bNew;
  QT_G_FUNC_PTR func;
} QGC_POINTER_QBitArray;

QT_G_FUNC( hbqt_gcRelease_QBitArray )
{
      QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         delete ( ( QBitArray * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "YES_rel_QBitArray                  ph=%p %i B %i KB", p->ph, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "DEL_rel_QBitArray                   Object already deleted!" ) );
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "PTR_rel_QBitArray                   Object not created with - new" ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QBitArray( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QBitArray;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "   _new_QBitArray                  ph=%p %i B %i KB", pObj, ( int ) hb_xquery( 1001 ), hbqt_getmemused() ) );
   }
   return p;
}

HB_FUNC( QT_QBITARRAY )
{
   void * pObj = NULL;

   pObj = new QBitArray() ;

   hb_retptrGC( hbqt_gcAllocate_QBitArray( pObj, true ) );
}
/*
 * bool at ( int i ) const
 */
HB_FUNC( QT_QBITARRAY_AT )
{
   hb_retl( hbqt_par_QBitArray( 1 )->at( hb_parni( 2 ) ) );
}

/*
 * void clear ()
 */
HB_FUNC( QT_QBITARRAY_CLEAR )
{
   hbqt_par_QBitArray( 1 )->clear();
}

/*
 * void clearBit ( int i )
 */
HB_FUNC( QT_QBITARRAY_CLEARBIT )
{
   hbqt_par_QBitArray( 1 )->clearBit( hb_parni( 2 ) );
}

/*
 * int count () const
 */
HB_FUNC( QT_QBITARRAY_COUNT )
{
   hb_retni( hbqt_par_QBitArray( 1 )->count() );
}

/*
 * int count ( bool on ) const
 */
HB_FUNC( QT_QBITARRAY_COUNT_1 )
{
   hb_retni( hbqt_par_QBitArray( 1 )->count( hb_parl( 2 ) ) );
}

/*
 * bool fill ( bool value, int size = -1 )
 */
HB_FUNC( QT_QBITARRAY_FILL )
{
   hb_retl( hbqt_par_QBitArray( 1 )->fill( hb_parl( 2 ), ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : -1 ) ) );
}

/*
 * void fill ( bool value, int begin, int end )
 */
HB_FUNC( QT_QBITARRAY_FILL_1 )
{
   hbqt_par_QBitArray( 1 )->fill( hb_parl( 2 ), hb_parni( 3 ), hb_parni( 4 ) );
}

/*
 * bool isEmpty () const
 */
HB_FUNC( QT_QBITARRAY_ISEMPTY )
{
   hb_retl( hbqt_par_QBitArray( 1 )->isEmpty() );
}

/*
 * bool isNull () const
 */
HB_FUNC( QT_QBITARRAY_ISNULL )
{
   hb_retl( hbqt_par_QBitArray( 1 )->isNull() );
}

/*
 * void resize ( int size )
 */
HB_FUNC( QT_QBITARRAY_RESIZE )
{
   hbqt_par_QBitArray( 1 )->resize( hb_parni( 2 ) );
}

/*
 * void setBit ( int i )
 */
HB_FUNC( QT_QBITARRAY_SETBIT )
{
   hbqt_par_QBitArray( 1 )->setBit( hb_parni( 2 ) );
}

/*
 * void setBit ( int i, bool value )
 */
HB_FUNC( QT_QBITARRAY_SETBIT_1 )
{
   hbqt_par_QBitArray( 1 )->setBit( hb_parni( 2 ), hb_parl( 3 ) );
}

/*
 * int size () const
 */
HB_FUNC( QT_QBITARRAY_SIZE )
{
   hb_retni( hbqt_par_QBitArray( 1 )->size() );
}

/*
 * bool testBit ( int i ) const
 */
HB_FUNC( QT_QBITARRAY_TESTBIT )
{
   hb_retl( hbqt_par_QBitArray( 1 )->testBit( hb_parni( 2 ) ) );
}

/*
 * bool toggleBit ( int i )
 */
HB_FUNC( QT_QBITARRAY_TOGGLEBIT )
{
   hb_retl( hbqt_par_QBitArray( 1 )->toggleBit( hb_parni( 2 ) ) );
}

/*
 * void truncate ( int pos )
 */
HB_FUNC( QT_QBITARRAY_TRUNCATE )
{
   hbqt_par_QBitArray( 1 )->truncate( hb_parni( 2 ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
