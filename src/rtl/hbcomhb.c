/*
 * Serial communication port API wrapper functions
 *
 * Copyright 2010 Mindaugas Kavaliauskas <dbtopas / at / dbtopas.lt>
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
 * along with this program; see the file LICENSE.txt.  If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301 USA (or visit https://www.gnu.org/licenses/).
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

/*
 * hb_comClose( nPort )  --> lSuccess
 * hb_comDiscardChar( nPort, nChar | cChar ) --> lSuccess
 * hb_comErrorChar( nPort, nChar | cChar ) --> lSuccess
 * hb_comFlowChars( nPort, nXONchar | cXONchar, nXOFFchar | cXOFFchar ) --> lSuccess
 * hb_comFlowControl( nPort, @nOldFlow [, nNewFlow] ) --> lSuccess
 * hb_comFlowSet( nPort, nFlow ) --> lSuccess
 * hb_comFlush( nPort, [ nType = HB_COM_IOFLUSH ] ) --> lSuccess
 * hb_comGetDevice( nPort )  --> cDeviceName
 * hb_comGetDeviceHandle( nPort )  --> nHandle | F_ERROR
 * hb_comGetError( nPort ) --> nError
 * hb_comGetOSError( nPort ) --> nError
 * hb_comFindPort( cDeviceName [, lCreate = .F. ] ) --> nPort
 * hb_comInit( nPort, nBaud, cParity, nSize, nStop ) --> lSuccess
 * hb_comInputCount( nPort ) --> nCount
 * hb_comInputState( nPort ) --> nState
 * hb_comLastNum() --> nLastPortNumber
 * hb_comLSR( nPort, @nValue ) --> lSuccess
 * hb_comMCR( nPort, @nValue, nClear, nSet ) --> lSuccess
 * hb_comMSR( nPort, @nValue ) --> lSuccess
 * hb_comOpen( nPort ) --> lSuccess
 * hb_comOutputCount( nPort ) --> nCount
 * hb_comOutputState( nPort ) --> nState
 * hb_comSendBreak( nPort, [ nDuration = 50 ] ) --> lSuccess
 * hb_comSetDevice( nPort, cDeviceName ) --> lSuccess
 * hb_comSetError( nPort, nError ) --> NIL
 * hb_comRecv( nPort, @cBuffer, [ nLen = Len( cBuffer ) ], [ nTimeout = 0 ] ) --> nBytesRecv
 * hb_comSend( nPort, cBuffer, [ nLen = Len( cBuffer ) ], [ nTimeout = 0 ] ) --> nBytesSent
 */

#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbvm.h"
#include "hbapicom.h"


HB_FUNC( HB_COMCLOSE )
{
   hb_retl( hb_comClose( hb_parni( 1 ) ) == 0 );
}

HB_FUNC( HB_COMDISCARDCHAR )
{
   hb_retl( hb_comDiscardChar( hb_parni( 1 ), HB_ISCHAR( 2 ) ? hb_parc( 2 )[ 0 ] : hb_parni( 2 ) ) == 0 );
}

HB_FUNC( HB_COMERRORCHAR )
{
   hb_retl( hb_comErrorChar( hb_parni( 1 ), HB_ISCHAR( 2 ) ? hb_parc( 2 )[ 0 ] : hb_parni( 2 ) ) == 0 );
}

HB_FUNC( HB_COMFLOWCHARS )
{
   hb_retl( hb_comFlowChars( hb_parni( 1 ), HB_ISCHAR( 2 ) ? hb_parc( 2 )[ 0 ] : hb_parni( 2 ),
                             HB_ISCHAR( 3 ) ? hb_parc( 3 )[ 0 ] : hb_parni( 3 ) ) == 0 );
}

HB_FUNC( HB_COMFLOWCONTROL )
{
   int iValue = 0;

   hb_retl( hb_comFlowControl( hb_parni( 1 ), &iValue, hb_parnidef( 3, -1 ) ) == 0 );
   hb_storni( iValue, 2 );
}

HB_FUNC( HB_COMFLOWSET )
{
   hb_retl( hb_comFlowSet( hb_parni( 1 ), hb_parni( 2 ) ) == 0 );
}

HB_FUNC( HB_COMFLUSH )
{
   hb_retl( hb_comFlush( hb_parni( 1 ), hb_parnidef( 2, HB_COM_IOFLUSH ) ) == 0 );
}

HB_FUNC( HB_COMGETDEVICE )
{
   char buffer[ HB_COM_DEV_NAME_MAX ];
   const char * name = hb_comGetDevice( hb_parni( 1 ), buffer, sizeof( buffer ) );

   hb_retc( name );
}

HB_FUNC( HB_COMGETDEVICEHANDLE )
{
   hb_retnint( ( HB_NHANDLE ) hb_comGetDeviceHandle( hb_parni( 1 ) ) );
}

HB_FUNC( HB_COMGETERROR )
{
   hb_retni( hb_comGetError( hb_parni( 1 ) ) );
}

HB_FUNC( HB_COMGETOSERROR )
{
   hb_retni( hb_comGetOsError( hb_parni( 1 ) ) );
}

HB_FUNC( HB_COMFINDPORT )
{
   hb_retni( hb_comFindPort( hb_parc( 1 ), hb_parl( 2 ) ) );
}


HB_FUNC( HB_COMINIT )
{
   hb_retl( hb_comInit( hb_parni( 1 ), hb_parni( 2 ), HB_ISCHAR( 3 ) ? hb_parc( 3 )[ 0 ] : 0,
                        hb_parni( 4 ), hb_parni( 5 ) ) == 0 );
}

HB_FUNC( HB_COMINPUTCOUNT )
{
   hb_retni( hb_comInputCount( hb_parni( 1 ) ) );
}

HB_FUNC( HB_COMINPUTSTATE )
{
   hb_retni( hb_comInputState( hb_parni( 1 ) ) );
}

HB_FUNC( HB_COMLASTNUM )
{
   hb_retni( hb_comLastNum() );
}

HB_FUNC( HB_COMLSR )
{
   int iValue = 0;

   hb_retl( hb_comLSR( hb_parni( 1 ), &iValue ) == 0 );
   hb_storni( iValue, 2 );
}

HB_FUNC( HB_COMMCR )
{
   int iValue = 0;

   hb_retl( hb_comMCR( hb_parni( 1 ), &iValue, hb_parni( 3 ), hb_parni( 4 ) ) == 0 );
   hb_storni( iValue, 2 );
}

HB_FUNC( HB_COMMSR )
{
   int iValue = 0;

   hb_retl( hb_comMSR( hb_parni( 1 ), &iValue ) == 0 );
   hb_storni( iValue, 2 );
}

HB_FUNC( HB_COMOPEN )
{
   hb_retl( hb_comOpen( hb_parni( 1 ) ) == 0 );
}

HB_FUNC( HB_COMOUTPUTCOUNT )
{
   hb_retni( hb_comOutputCount( hb_parni( 1 ) ) );
}

HB_FUNC( HB_COMOUTPUTSTATE )
{
   hb_retni( hb_comOutputState( hb_parni( 1 ) ) );
}

HB_FUNC( HB_COMSENDBREAK )
{
   /* 50ms break is enough for baud-rate 300 and higher */
   hb_retl( hb_comSendBreak( hb_parni( 1 ), hb_parnidef( 2, 50 ) ) == 0 );
}

HB_FUNC( HB_COMSETDEVICE )
{
   hb_retl( hb_comSetDevice( hb_parni( 1 ), hb_parc( 2 ) ) == 0 );
}

HB_FUNC( HB_COMSETERROR )
{
   hb_comSetError( hb_parni( 1 ), hb_parni( 2 ) );
}

HB_FUNC( HB_COMRECV )
{
   PHB_ITEM pItem = hb_param( 2, HB_IT_STRING );
   char * pBuffer;
   HB_SIZE nLen;

   if( pItem && HB_ISBYREF( 2 ) && hb_itemGetWriteCL( pItem, &pBuffer, &nLen ) )
   {
      if( HB_ISNUM( 3 ) )
      {
         long lRead = hb_parnl( 3 );
         if( lRead >= 0 && lRead < ( long ) nLen )
            nLen = lRead;
      }
      hb_retnl( hb_comRecv( hb_parni( 1 ), pBuffer, ( long ) nLen, hb_parnint( 4 ) ) );
   }
   else
      hb_errRT_BASE_SubstR( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
}

HB_FUNC( HB_COMSEND )
{
   long  lLen = ( long ) hb_parclen( 2 );

   if( HB_ISNUM( 3 ) )
   {
      long lParam = hb_parnl( 3 );

      if( lParam >= 0 && lParam < lLen )
         lLen = lParam;
   }
   hb_retnl( hb_comSend( hb_parni( 1 ), hb_parc( 2 ), lLen, hb_parnint( 4 ) ) );
}
