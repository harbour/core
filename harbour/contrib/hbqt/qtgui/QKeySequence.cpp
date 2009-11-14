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
 *  enum SequenceFormat { NativeText, PortableText }
 *  enum SequenceMatch { NoMatch, PartialMatch, ExactMatch }
 *  enum StandardKey { AddTab, Back, Bold, Close, ..., ZoomOut }
 */

/*
 *  Constructed[ 6/7 [ 85.71% ] ]
 *
 *  *** Unconvered Prototypes ***
 *  -----------------------------
 *
 *  QList<QKeySequence> keyBindings ( StandardKey key )
 */

#include <QtCore/QPointer>

#include <QtGui/QKeySequence>


/*
 * QKeySequence ()
 * QKeySequence ( const QString & key )
 * QKeySequence ( int k1, int k2 = 0, int k3 = 0, int k4 = 0 )
 * QKeySequence ( const QKeySequence & keysequence )
 * QKeySequence ( StandardKey key )
 * ~QKeySequence ()
 */

QT_G_FUNC( release_QKeySequence )
{
   QGC_POINTER * p = ( QGC_POINTER * ) Cargo;

   HB_TRACE( HB_TR_DEBUG, ( "release_QKeySequence                 p=%p", p ) );
   HB_TRACE( HB_TR_DEBUG, ( "release_QKeySequence                ph=%p", p->ph ) );

   if( p && p->ph )
   {
      ( ( QKeySequence * ) p->ph )->~QKeySequence();
      p->ph = NULL;
      HB_TRACE( HB_TR_DEBUG, ( "release_QKeySequence                Object deleted!" ) );
      #if defined(__debug__)
         just_debug( "  YES release_QKeySequence                %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );
      #endif
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "release_QKeySequence                Object Allready deleted!" ) );
      #if defined(__debug__)
         just_debug( "  DEL release_QKeySequence" );
      #endif
   }
}

void * gcAllocate_QKeySequence( void * pObj )
{
   QGC_POINTER * p = ( QGC_POINTER * ) hb_gcAllocate( sizeof( QGC_POINTER ), gcFuncs() );

   p->ph = pObj;
   p->func = release_QKeySequence;
   #if defined(__debug__)
      just_debug( "          new_QKeySequence                %i B %i KB", ( int ) hb_xquery( 1001 ), hb_getMemUsed() );
   #endif
   return( p );
}

HB_FUNC( QT_QKEYSEQUENCE )
{
   void * pObj = NULL;

   if( HB_ISPOINTER( 1 ) )
      pObj = ( QKeySequence * ) new QKeySequence( *hbqt_par_QKeySequence( 1 ) ) ;
   else if( HB_ISCHAR( 1 ) )
      pObj = ( QKeySequence * ) new QKeySequence( hbqt_par_QString( 1 ) ) ;
   else if( HB_ISNUM( 1 ) )
      pObj = ( QKeySequence * ) new QKeySequence( hb_parni( 1 ) ) ;
   else
      pObj = ( QKeySequence * ) new QKeySequence() ;

   hb_retptrGC( gcAllocate_QKeySequence( pObj ) );
}
/*
 * uint count () const
 */
HB_FUNC( QT_QKEYSEQUENCE_COUNT )
{
   hb_retni( hbqt_par_QKeySequence( 1 )->count() );
}

/*
 * bool isEmpty () const
 */
HB_FUNC( QT_QKEYSEQUENCE_ISEMPTY )
{
   hb_retl( hbqt_par_QKeySequence( 1 )->isEmpty() );
}

/*
 * SequenceMatch matches ( const QKeySequence & seq ) const
 */
HB_FUNC( QT_QKEYSEQUENCE_MATCHES )
{
   hb_retni( ( QKeySequence::SequenceMatch ) hbqt_par_QKeySequence( 1 )->matches( *hbqt_par_QKeySequence( 2 ) ) );
}

/*
 * QString toString ( SequenceFormat format = PortableText ) const
 */
HB_FUNC( QT_QKEYSEQUENCE_TOSTRING )
{
   hb_retc( hbqt_par_QKeySequence( 1 )->toString( ( HB_ISNUM( 2 ) ? ( QKeySequence::SequenceFormat ) hb_parni( 2 ) : ( QKeySequence::SequenceFormat ) QKeySequence::PortableText ) ).toAscii().data() );
}

/*
 * QKeySequence fromString ( const QString & str, SequenceFormat format = PortableText )
 */
HB_FUNC( QT_QKEYSEQUENCE_FROMSTRING )
{
   hb_retptrGC( gcAllocate_QKeySequence( new QKeySequence( hbqt_par_QKeySequence( 1 )->fromString( hbqt_par_QString( 2 ), ( HB_ISNUM( 3 ) ? ( QKeySequence::SequenceFormat ) hb_parni( 3 ) : ( QKeySequence::SequenceFormat ) QKeySequence::PortableText ) ) ) ) );
}

/*
 * QKeySequence mnemonic ( const QString & text )
 */
HB_FUNC( QT_QKEYSEQUENCE_MNEMONIC )
{
   hb_retptrGC( gcAllocate_QKeySequence( new QKeySequence( hbqt_par_QKeySequence( 1 )->mnemonic( hbqt_par_QString( 2 ) ) ) ) );
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
