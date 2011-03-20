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
#include "hbvm.h"

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
      {
         s_argCombinations.removeAt( iIndex );
         s_pCallback.removeAt( iIndex );
      }
   }
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
         HB_TRACE( HB_TR_DEBUG, ( "      HBQSlots::~HBQSlots() %d", i ) );
         hb_itemRelease( listBlock.at( i ) );
         listBlock[ i ] = NULL;
      }
   }
   /* QUESTION: Should there be all remaining active slots disconnected at this point? */

   /* Should be disconnected, but this is a responsibility of programmer as object is only known to the application */
   //listBlock.clear();
}

int HBQSlots::qt_metacall( QMetaObject::Call c, int id, void ** arguments )
{
   id = QObject::qt_metacall( c, id, arguments );

   if( id < 0 || c != QMetaObject::InvokeMetaMethod )
      return id;

   QObject * object = sender();
   if( object )
   {
      char szSlotName[ 20 ];
      hb_snprintf( szSlotName, sizeof( szSlotName ), "SLOT_%d", id );
      int i = object->property( szSlotName ).toInt();

      if( i > 0 && i <= this->listBlock.size() )
      {
         QByteArray paramString;
         const QMetaMethod meta = object->metaObject()->method( id );
         QList<QByteArray> arrayOfTypes = meta.parameterTypes();
         int parameterCount = arrayOfTypes.size();
         QStringList pList;

         if( parameterCount > 0 )
         {
            char szParams[ 20 ];
            hb_snprintf( szParams, sizeof( szParams ), "PARAM_%d", id );
            paramString = object->property( szParams ).toByteArray();

            char szPList[ 20 ];
            hb_snprintf( szPList, sizeof( szPList ), "PLIST_%d", id );
            pList = object->property( szPList ).toStringList();

            if( paramString.isNull() )
            {
               QStringList parList;
               HB_TRACE( HB_TR_DEBUG, ( "SlotsProxy signature %s", meta.signature() ) );

               for( int i = 0; i < parameterCount; i++ )
               {
                  if( arrayOfTypes.at( i ).contains( "::" ) )
                  {
                     parList += "int";
                     pList += "int";
                  }
                  else
                  {
                     parList += arrayOfTypes.at( i ).trimmed() ;
                     pList += arrayOfTypes.at( i ).trimmed().toUpper();
                  }
               }
               paramString = parList.join( "$" ).toAscii();
               object->setProperty( szParams, paramString );

               object->setProperty( szPList, pList );

               HB_TRACE( HB_TR_DEBUG, ( "       SlotsProxy parList %s ", ( char * ) paramString.data() ) );
            }
         }

         if( hb_vmRequestReenter() )
         {
            if( parameterCount == 0 )
               hb_evalBlock0( this->listBlock.at( i - 1 ) );
            else
            {
               int paramId = s_argCombinations.indexOf( paramString );
               PHBQT_SLOT_FUNC pCallback;

               HB_TRACE( HB_TR_DEBUG, ( "  params=%s,  paramId=%d", ( char * ) paramString.data(), paramId ) );

               pCallback = s_pCallback.at( paramId );
               if( pCallback )
                  pCallback( ( PHB_ITEM * ) this->listBlock.at( i - 1 ), arguments, pList );
            }
            hb_vmRequestRestore();
         }
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
   int nResult;
   HBQSlots * t_slots = hbqt_par_HBQSlots( 1 );

   if( t_slots )
   {
      QObject * object = ( QObject * ) hbqt_pPtrFromObj( 2 ); /* get sender */
      if( object )
      {
         PHB_ITEM pBlock = hb_itemNew( hb_param( 4, HB_IT_BLOCK ) );  /* get codeblock */
         if( pBlock )
         {
            const char * pszSignal = hb_parcx( 3 );

            int i = object->property( pszSignal ).toInt();
            if( i == 0 )
            {
               QString signal = pszSignal;
               QByteArray theSignal = QMetaObject::normalizedSignature( signal.toAscii() );

               if( QMetaObject::checkConnectArgs( theSignal, theSignal ) )
               {
                  int signalId = object->metaObject()->indexOfSignal( theSignal );
                  if( signalId != -1 )
                  {
                     int slotId = object->metaObject()->indexOfMethod( theSignal );
                     if( slotId != -1 )
                     {
                        if( QMetaObject::connect( object, signalId, t_slots, slotId + QObject::staticMetaObject.methodCount(), Qt::AutoConnection ) )
                        {
                           t_slots->listBlock << pBlock;

                           char szSlotName[ 20 ];
                           hb_snprintf( szSlotName, sizeof( szSlotName ), "SLOT_%d", slotId );

                           object->setProperty( szSlotName, ( int ) t_slots->listBlock.size() );
                           object->setProperty( pszSignal, ( int ) t_slots->listBlock.size() );

                           nResult = 0;
                        }
                        else
                           nResult = 8;
                     }
                     else
                        nResult = 7;
                  }
                  else
                     nResult = 6;
               }
               else
                  nResult = 5;
            }
            else
               nResult = 4;
         }
         else
            nResult = 3;
      }
      else
         nResult = 2;
   }
   else
      nResult = 1;

   hb_retni( nResult );
}

/*
 * Harbour function to disconnect signals
 */
HB_FUNC( __HBQT_SLOTS_DISCONNECT )
{
   int nResult;
   HBQSlots * t_slots = hbqt_par_HBQSlots( 1 );

   if( t_slots )
   {
      QObject * object = ( QObject * ) hbqt_pPtrFromObj( 2 );
      if( object )
      {
         const char * pszSignal = hb_parcx( 3 );

         int i = object->property( pszSignal ).toInt();
         if( i > 0 && i <= t_slots->listBlock.size() )
         {
            object->setProperty( pszSignal, QVariant() );

            QString signal = pszSignal;
            QByteArray theSignal = signal.toAscii();

            int signalId = object->metaObject()->indexOfSignal( QMetaObject::normalizedSignature( theSignal ) );
            if( signalId != -1 )
            {
               if( QMetaObject::disconnect( object, signalId, 0, 0 ) )
                  nResult = 0;
               else
                  nResult = 5;
            }
            else
               nResult = 4;

            if( t_slots->listBlock.at( i - 1 ) != NULL )
            {
               hb_itemRelease( t_slots->listBlock.at( i - 1 ) );
               t_slots->listBlock[ i - 1 ] = NULL;
            }
         }
         else
            nResult = 3;
      }
      else
         nResult = 2;
   }
   else
      nResult = 1;

   hb_retni( nResult );
}

HB_FUNC( __HBQT_SLOTS_NEW )
{
   hb_retptrGC( hbqt_gcAllocate_HBQSlots( ( HBQSlots * ) new HBQSlots(), true ) );
}

#endif
