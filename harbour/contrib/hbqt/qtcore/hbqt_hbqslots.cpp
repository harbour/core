/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009 Marcos Antonio Gambeta (marcosgambeta at gmail dot com)
 * Copyright 2009 Pritpal Bedi (pritpal@vouchcac.com)
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
 * Copyright 2010 Francesco Perillo ()
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

#include "hbqt.h"

#include "hbapiitm.h"
#include "hbstack.h"
#include "hbvm.h"
#include "hbapierr.h"

#if QT_VERSION >= 0x040500

#include "hbqt_hbqslots.h"

/*----------------------------------------------------------------------*/

#include <QtCore/QStringList>

/*----------------------------------------------------------------------*/

static QList<QByteArray> s_argCombinations;
static QList<PHBQT_SLOT_FUNC> s_pCallback;

void hbqt_slots_register_callback( QByteArray sig, PHBQT_SLOT_FUNC pCallback )
{
   HB_TRACE( HB_TR_DEBUG, ( "callback %s", ( char * ) sig.data() ) );

   if( ! sig.isEmpty() && pCallback )
   {
      int iIndex = s_argCombinations.indexOf( sig );

      if( iIndex == -1 )
      {
         s_argCombinations << sig;
         s_pCallback << pCallback;
      }
      else
         s_pCallback[ iIndex ] = pCallback;
   }
}

void hbqt_slots_unregister_callback( QByteArray sig )
{
   if( ! sig.isEmpty() )
   {
      int iIndex = s_argCombinations.indexOf( sig );

      if( iIndex > -1 )
         s_pCallback.removeAt( iIndex );
   }
}

static int connect_signal( QString signal, QObject * object, HBQSlots * t_slots )
{
   HB_TRACE( HB_TR_DEBUG, ( "connect_signal: %s", ( char * ) signal.toAscii().data() ) );

   QByteArray theSignal = signal.toAscii();

   theSignal = QMetaObject::normalizedSignature( theSignal );

   HB_TRACE( HB_TR_DEBUG, ( "    normalized: %s", ( char * ) theSignal.data() ) );

   if( ! QMetaObject::checkConnectArgs( theSignal, theSignal ) )
   {
      HB_TRACE( HB_TR_ALWAYS, ( "NOT    checkConnectArgs: %s", ( char * ) theSignal.data() ) );
      return -1;
   }

   int signalId = object->metaObject()->indexOfSignal( theSignal );
   if( signalId == -1 )
   {
      HB_TRACE( HB_TR_ALWAYS, ( "NOT    indexOfSignal: %s", ( char * ) theSignal.data() ) );
      return -1;
   }

   int slotId = object->metaObject()->indexOfMethod( theSignal );
   if( slotId == -1 )
   {
      HB_TRACE( HB_TR_ALWAYS, ( "NOT    indexOfMethod: %s", ( char * ) theSignal.data() ) );
      return -1;
   }

   if( QMetaObject::connect( object, signalId, t_slots, slotId + QObject::staticMetaObject.methodCount(), Qt::AutoConnection ) )
   {
      HB_TRACE( HB_TR_DEBUG, ( "YES    connected: %i %i %s", signalId, slotId, ( char * ) theSignal.data() ) );
      return slotId;
   }
   return -1;
}

/*----------------------------------------------------------------------*/

static bool disconnect_signal( QObject * object, QString signal )
{
   QByteArray theSignal = signal.toAscii();

   theSignal = QMetaObject::normalizedSignature( theSignal ) ;

   int signalId = object->metaObject()->indexOfSignal( theSignal );
   if( signalId == -1 )
   {
      return false;
   }
   return QMetaObject::disconnect( object, signalId, 0, 0 );
}

/*----------------------------------------------------------------------*/

HBQSlots::HBQSlots( QObject* parent ) : QObject( parent )
{
}

HBQSlots::~HBQSlots()
{
   int i;

   for( i = 0; i < listBlock.size(); i++ )
   {
      if( listBlock[ i ] != NULL )
      {
         hb_itemRelease( listBlock.at( i ) );
         listBlock[ i ] = NULL;
      }
   }
   /* QUESTION: Should there be all remaining active slots disconnected at this point? */

   /*           Should be disconnected, but this is a responsibility of programmer as object is only known to the application */
   listBlock.clear();
}

int HBQSlots::qt_metacall( QMetaObject::Call c, int id, void **arguments )
{
   id = QObject::qt_metacall( c, id, arguments );

   if( id < 0 || c != QMetaObject::InvokeMetaMethod )
      return id;

   // Q_ASSERT(id < slotList.size());

   QObject * object = sender();
   const QMetaMethod meta = object->metaObject()->method( id );
   QList<QByteArray> arrayOfTypes = meta.parameterTypes();
   int parameterCount = arrayOfTypes.size();
   QStringList parList;

   HB_TRACE( HB_TR_DEBUG, ( "SlotsProxy signature %s", meta.signature() ) );

   for( int i = 0; i < parameterCount; i++ )
   {
      if( arrayOfTypes.at( i ).contains( "::" ) ) // if includes :: is a enum -> int
         parList += "int";
      else
      {
         if( arrayOfTypes.at( i ).contains( "*" ) )  // if includes * is a pointer -> pointer
            parList += "pointer";
         else
            parList += arrayOfTypes.at( i ); //
      }
   }

   QByteArray paramString = parList.join( "$" ).toAscii();
   HB_TRACE( HB_TR_DEBUG, ( "       SlotsProxy parList %s ", ( char * ) paramString.data() ) );

   if( object )
   {
      char szSlotName[ 20 ];
      hb_snprintf( szSlotName, sizeof( szSlotName ), "SLOT_%d", id );
      int i = object->property( szSlotName ).toInt();

      if( i > 0 && i <= this->listBlock.size() && hb_vmRequestReenter() )
      {
         if( parameterCount == 0 )
            hb_vmEvalBlockV( this->listBlock.at( i - 1 ), 0 );
         else
         {
            int paramId = s_argCombinations.indexOf( paramString );
            PHBQT_SLOT_FUNC pCallback;

            HB_TRACE( HB_TR_DEBUG, ( "  params=%s,  paramId=%d", ( char *) paramString.data(), paramId ) );

            pCallback = s_pCallback.at( paramId );
            if( pCallback )
               pCallback( ( PHB_ITEM * ) this->listBlock.at( i - 1 ), arguments );
         }
         hb_vmRequestRestore();
      }
   }
   return -1;
}

/*----------------------------------------------------------------------*/
/*
 * Harbour function to connect signals with slots
 */
HB_FUNC( __HBQT_SLOTS_CONNECT )
{
   HB_BOOL   bRet    = HB_FALSE;
   HBQSlots * t_slots = hbqt_par_HBQSlots( 1 );

   if( t_slots )
   {
      QObject * object = ( QObject * ) hbqt_pPtrFromObj( 2 );               /* get sender    */
      if( object )
      {
         int i = object->property( hb_parcx( 3 ) ).toInt();
         if( i == 0 )
         {
            QString signal = hb_parcx( 3 );                                 /* get signal    */
            int idSignal = connect_signal( signal, object, t_slots ) ;
            if( idSignal >= 0 )
            {
               PHB_ITEM pBlock = hb_itemNew( hb_param( 4, HB_IT_BLOCK ) );  /* get codeblock */
               if( pBlock )
               {
                  t_slots->listBlock << pBlock;

                  char szSlotName[ 20 ];
                  hb_snprintf( szSlotName, sizeof( szSlotName ), "SLOT_%d", idSignal );

                  object->setProperty( szSlotName, ( int ) t_slots->listBlock.size() );
                  object->setProperty( hb_parcx( 3 ), ( int ) t_slots->listBlock.size() );
                  bRet = HB_TRUE;
               }
            }
         }
      }
   }

   if( ! bRet )
      hb_errRT_BASE( EG_ARG, 1100, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );

   hb_retl( bRet );
}

/*
 * Harbour function to disconnect signals
 */
HB_FUNC( __HBQT_SLOTS_DISCONNECT )
{
   HB_BOOL bRet = HB_FALSE;
   HBQSlots * t_slots = hbqt_par_HBQSlots( 1 );

   if( t_slots )
   {
      QObject * object = ( QObject* ) hbqt_pPtrFromObj( 2 );
      if( object )
      {
         const char * slot = hb_parcx( 3 );

         int i = object->property( slot ).toInt();

         if( i > 0 && i <= t_slots->listBlock.size() )
         {
            object->setProperty( slot, QVariant() );

            bRet = ( disconnect_signal( object, ( QString ) slot ) == true );

            hb_itemRelease( t_slots->listBlock.at( i - 1 ) );
            t_slots->listBlock[ i - 1 ] = NULL;
         }
      }
   }
   hb_retl( bRet );
}

HB_FUNC( __HBQT_SLOTS_NEW )
{
   void * pObj = NULL;

   pObj = ( HBQSlots * ) new HBQSlots();

   hb_retptrGC( hbqt_gcAllocate_HBQSlots( pObj, true ) );
}

#endif
