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
 *  enum Type { VariableLength, FixedLength, PercentageLength }
 */

#include <QtCore/QPointer>

#include <QtGui/QTextLength>


/*
 * QTextLength ()
 * QTextLength ( Type type, qreal value )
 */

QT_G_FUNC( release_QTextLength )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QTextLength                  p=%p", p ) );
   HB_TRACE( HB_TR_DEBUG, ( "release_QTextLength                 ph=%p", p->ph ) );

   if( p && p->ph )
   {
      delete ( ( QTextLength * ) p->ph );
      p->ph = NULL;
      HB_TRACE( HB_TR_DEBUG, ( "release_QTextLength                 Object deleted!" ) );
      #if defined( __HB_DEBUG__ )
         hbqt_debug( "  YES release_QTextLength                 %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() );
      #endif
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "release_QTextLength                 Object Allready deleted!" ) );
      #if defined( __HB_DEBUG__ )
         hbqt_debug( "  DEL release_QTextLength" );
      #endif
   }
}

void * gcAllocate_QTextLength( void * pObj )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QTextLength;
   #if defined( __HB_DEBUG__ )
      hbqt_debug( "          new_QTextLength                 %i B %i KB", ( int ) hb_xquery( 1001 ), hbqt_getmemused() );
   #endif
   return( p );
}

HB_FUNC( QT_QTEXTLENGTH )
{
   void * pObj = NULL;

   pObj = ( QTextLength* ) new QTextLength() ;

   hb_retptrGC( gcAllocate_QTextLength( pObj ) );
}
/*
 * qreal rawValue () const
 */
HB_FUNC( QT_QTEXTLENGTH_RAWVALUE )
{
   hb_retnd( hbqt_par_QTextLength( 1 )->rawValue() );
}

/*
 * Type type () const
 */
HB_FUNC( QT_QTEXTLENGTH_TYPE )
{
   hb_retni( ( QTextLength::Type ) hbqt_par_QTextLength( 1 )->type() );
}

/*
 * qreal value ( qreal maximumLength ) const
 */
HB_FUNC( QT_QTEXTLENGTH_VALUE )
{
   hb_retnd( hbqt_par_QTextLength( 1 )->value( hb_parnd( 2 ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
