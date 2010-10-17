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
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/

#include "hbqtcore.h"

/*----------------------------------------------------------------------*/
#if QT_VERSION >= 0x040500
/*----------------------------------------------------------------------*/

/*
 *  Constructed[ 21/21 [ 100.00% ] ]
 *
 */

#include <QtCore/QPointer>

#include <QtCore/QMetaProperty>


/*
 * QMetaProperty ()
 *
 */

typedef struct
{
   QMetaProperty * ph;
   bool bNew;
   PHBQT_GC_FUNC func;
   int type;
} HBQT_GC_T_QMetaProperty;

HBQT_GC_FUNC( hbqt_gcRelease_QMetaProperty )
{
   HB_SYMBOL_UNUSED( Cargo );
   HBQT_GC_T * p = ( HBQT_GC_T * ) Cargo;

   if( p && p->bNew )
   {
      p->ph = NULL;
   }
}

void * hbqt_gcAllocate_QMetaProperty( void * pObj, bool bNew )
{
   HBQT_GC_T * p = ( HBQT_GC_T * ) hb_gcAllocate( sizeof( HBQT_GC_T ), hbqt_gcFuncs() );

   p->ph = ( QMetaProperty * ) pObj;
   p->bNew = bNew;
   p->func = hbqt_gcRelease_QMetaProperty;
   p->type = HBQT_TYPE_QMetaProperty;

   if( bNew )
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p    _new_QMetaProperty", pObj ) );
   }
   else
   {
      HB_TRACE( HB_TR_DEBUG, ( "ph=%p NOT_new_QMetaProperty", pObj ) );
   }
   return p;
}

HB_FUNC( QT_QMETAPROPERTY )
{
   // __HB_RETPTRGC__( new QMetaProperty() );
}

/*
 * QMetaEnum enumerator () const
 */
HB_FUNC( QT_QMETAPROPERTY_ENUMERATOR )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QMetaEnum( new QMetaEnum( ( p )->enumerator() ), true ) );
   }
}

/*
 * bool hasNotifySignal () const
 */
HB_FUNC( QT_QMETAPROPERTY_HASNOTIFYSIGNAL )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
   {
      hb_retl( ( p )->hasNotifySignal() );
   }
}

/*
 * bool isDesignable ( const QObject * object = 0 ) const
 */
HB_FUNC( QT_QMETAPROPERTY_ISDESIGNABLE )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
   {
      hb_retl( ( p )->isDesignable( hbqt_par_QObject( 2 ) ) );
   }
}

/*
 * bool isEnumType () const
 */
HB_FUNC( QT_QMETAPROPERTY_ISENUMTYPE )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
   {
      hb_retl( ( p )->isEnumType() );
   }
}

/*
 * bool isFlagType () const
 */
HB_FUNC( QT_QMETAPROPERTY_ISFLAGTYPE )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
   {
      hb_retl( ( p )->isFlagType() );
   }
}

/*
 * bool isReadable () const
 */
HB_FUNC( QT_QMETAPROPERTY_ISREADABLE )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
   {
      hb_retl( ( p )->isReadable() );
   }
}

/*
 * bool isResettable () const
 */
HB_FUNC( QT_QMETAPROPERTY_ISRESETTABLE )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
   {
      hb_retl( ( p )->isResettable() );
   }
}

/*
 * bool isScriptable ( const QObject * object = 0 ) const
 */
HB_FUNC( QT_QMETAPROPERTY_ISSCRIPTABLE )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
   {
      hb_retl( ( p )->isScriptable( hbqt_par_QObject( 2 ) ) );
   }
}

/*
 * bool isStored ( const QObject * object = 0 ) const
 */
HB_FUNC( QT_QMETAPROPERTY_ISSTORED )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
   {
      hb_retl( ( p )->isStored( hbqt_par_QObject( 2 ) ) );
   }
}

/*
 * bool isUser ( const QObject * object = 0 ) const
 */
HB_FUNC( QT_QMETAPROPERTY_ISUSER )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
   {
      hb_retl( ( p )->isUser( hbqt_par_QObject( 2 ) ) );
   }
}

/*
 * bool isValid () const
 */
HB_FUNC( QT_QMETAPROPERTY_ISVALID )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
   {
      hb_retl( ( p )->isValid() );
   }
}

/*
 * bool isWritable () const
 */
HB_FUNC( QT_QMETAPROPERTY_ISWRITABLE )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
   {
      hb_retl( ( p )->isWritable() );
   }
}

/*
 * const char * name () const
 */
HB_FUNC( QT_QMETAPROPERTY_NAME )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
   {
      hb_retc( ( p )->name() );
   }
}

/*
 * QMetaMethod notifySignal () const
 */
HB_FUNC( QT_QMETAPROPERTY_NOTIFYSIGNAL )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QMetaMethod( new QMetaMethod( ( p )->notifySignal() ), true ) );
   }
}

/*
 * int notifySignalIndex () const
 */
HB_FUNC( QT_QMETAPROPERTY_NOTIFYSIGNALINDEX )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
   {
      hb_retni( ( p )->notifySignalIndex() );
   }
}

/*
 * QVariant read ( const QObject * object ) const
 */
HB_FUNC( QT_QMETAPROPERTY_READ )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
   {
      hb_retptrGC( hbqt_gcAllocate_QVariant( new QVariant( ( p )->read( hbqt_par_QObject( 2 ) ) ), true ) );
   }
}

/*
 * bool reset ( QObject * object ) const
 */
HB_FUNC( QT_QMETAPROPERTY_RESET )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
   {
      hb_retl( ( p )->reset( hbqt_par_QObject( 2 ) ) );
   }
}

/*
 * QVariant::Type type () const
 */
HB_FUNC( QT_QMETAPROPERTY_TYPE )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
   {
      hb_retni( ( QVariant::Type ) ( p )->type() );
   }
}

/*
 * const char * typeName () const
 */
HB_FUNC( QT_QMETAPROPERTY_TYPENAME )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
   {
      hb_retc( ( p )->typeName() );
   }
}

/*
 * int userType () const
 */
HB_FUNC( QT_QMETAPROPERTY_USERTYPE )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
   {
      hb_retni( ( p )->userType() );
   }
}

/*
 * bool write ( QObject * object, const QVariant & value ) const
 */
HB_FUNC( QT_QMETAPROPERTY_WRITE )
{
   QMetaProperty * p = hbqt_par_QMetaProperty( 1 );
   if( p )
   {
      hb_retl( ( p )->write( hbqt_par_QObject( 2 ), *hbqt_par_QVariant( 3 ) ) );
   }
}


/*----------------------------------------------------------------------*/
#endif             /* #if QT_VERSION >= 0x040500 */
/*----------------------------------------------------------------------*/
