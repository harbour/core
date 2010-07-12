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

#include "../hbqt.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

#include <QtCore/QPointer>

#include <QtGui/QTextFragment>


/*
 * QTextFragment ()
 * QTextFragment ( const QTextFragment & other )
 */

typedef struct
{
   QTextFragment * ph;
   bool bNew;
   QT_G_FUNC_PTR func;
   int type;
} QGC_POINTER_QTextFragment;

QT_G_FUNC( hbqt_gcRelease_QTextFragment )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   if( p && p->bNew )
   {
      if( p->ph )
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _rel_QTextFragment   /.\\", p->ph ) );
         delete ( ( QTextFragment * ) p->ph );
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p YES_rel_QTextFragment   \\./", p->ph ) );
         p->ph = NULL;
      }
      else
      {
         HB_TRACE( HB_TR_DEBUG, ( "ph=%p DEL_rel_QTextFragment    :     Object already deleted!", p->ph ) );
         p->ph = NULL;
      }
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p PTR_rel_QTextFragment    :    Object not created with new=true", p->ph ) );
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QTextFragment( void * pObj, bool bNew )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), hbqt_gcFuncs() );

   p->ph = ( QTextFragment * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QTextFragment;
   p->type = HBQT_TYPE_QTextFragment;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QTextFragment", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QTextFragment", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QTEXTFRAGMENT )
{
   QTextFragment * pObj = NULL;

   pObj =  new QTextFragment() ;

   hb_retptrGC( hbqt_gcAllocate_QTextFragment( ( void * ) pObj, true ) );
}

/*
 * QTextCharFormat charFormat () const
 */
HB_FUNC( QT_QTEXTFRAGMENT_CHARFORMAT )
{
   QTextFragment * p = hbqt_par_QTextFragment( 1 );
   if( p )
      hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( ( p )->charFormat() ), true ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAGMENT_CHARFORMAT FP=hb_retptrGC( hbqt_gcAllocate_QTextCharFormat( new QTextCharFormat( ( p )->charFormat() ), true ) ); p is NULL" ) );
   }
}

/*
 * int charFormatIndex () const
 */
HB_FUNC( QT_QTEXTFRAGMENT_CHARFORMATINDEX )
{
   QTextFragment * p = hbqt_par_QTextFragment( 1 );
   if( p )
      hb_retni( ( p )->charFormatIndex() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAGMENT_CHARFORMATINDEX FP=hb_retni( ( p )->charFormatIndex() ); p is NULL" ) );
   }
}

/*
 * bool contains ( int position ) const
 */
HB_FUNC( QT_QTEXTFRAGMENT_CONTAINS )
{
   QTextFragment * p = hbqt_par_QTextFragment( 1 );
   if( p )
      hb_retl( ( p )->contains( hb_parni( 2 ) ) );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAGMENT_CONTAINS FP=hb_retl( ( p )->contains( hb_parni( 2 ) ) ); p is NULL" ) );
   }
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QTEXTFRAGMENT_ISVALID )
{
   QTextFragment * p = hbqt_par_QTextFragment( 1 );
   if( p )
      hb_retl( ( p )->isValid() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAGMENT_ISVALID FP=hb_retl( ( p )->isValid() ); p is NULL" ) );
   }
}

/*
 * int length () const
 */
HB_FUNC( QT_QTEXTFRAGMENT_LENGTH )
{
   QTextFragment * p = hbqt_par_QTextFragment( 1 );
   if( p )
      hb_retni( ( p )->length() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAGMENT_LENGTH FP=hb_retni( ( p )->length() ); p is NULL" ) );
   }
}

/*
 * int position () const
 */
HB_FUNC( QT_QTEXTFRAGMENT_POSITION )
{
   QTextFragment * p = hbqt_par_QTextFragment( 1 );
   if( p )
      hb_retni( ( p )->position() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAGMENT_POSITION FP=hb_retni( ( p )->position() ); p is NULL" ) );
   }
}

/*
 * QString text () const
 */
HB_FUNC( QT_QTEXTFRAGMENT_TEXT )
{
   QTextFragment * p = hbqt_par_QTextFragment( 1 );
   if( p )
      hb_retc( ( p )->text().toAscii().data() );
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "............................... F=QT_QTEXTFRAGMENT_TEXT FP=hb_retc( ( p )->text().toAscii().data() ); p is NULL" ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
