/*
 * Harbour Project source code:
 *    CT3 serial communication com_*() functions
 *
 * Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
 * along with this software; see the file COPYING.txt.  If not, write to
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapicom.h"
#include "ctcom.ch"

static int hb_ctComCharParam( int iParam )
{
   const char * pszParam = hb_parc( iParam );

   if( pszParam )
   {
      if( hb_parclen( iParam ) > 0 )
         return ( unsigned char ) pszParam[ 0 ];
   }
   else if( HB_ISNUM( iParam ) )
      return ( unsigned char ) hb_parni( iParam );

   return -1;
}

static void hb_ctComTestMSR( int iLine )
{
   HB_BOOL fResult;
   int iMSR;

   if( hb_comMSR( hb_parni( 1 ), &iMSR ) != -1 )
      fResult = ( iMSR & iLine ) != 0;
   else
      fResult = HB_FALSE;

   hb_retl( fResult );
}

/* com_Count( <nComPort> ) -> <nCharactersInInputBuffer>
 */
HB_FUNC( COM_COUNT )
{
   hb_retni( hb_comInputCount( hb_parni( 1 ) ) );
}

/* com_SCount( <nComPort> ) -> <nCharactersInOutputBuffer>
 */
HB_FUNC( COM_SCOUNT )
{
   hb_retni( hb_comOutputCount( hb_parni( 1 ) ) );
}

/* com_Flush( <nComPort> ) -> <lInputBufferCleared>
 */
HB_FUNC( COM_FLUSH )
{
   hb_retl( hb_comFlush( hb_parni( 1 ), HB_COM_IFLUSH ) != -1 );
}

/* com_SFlush( <nComPort> ) -> <lOutputBufferCleared>
 */
HB_FUNC( COM_SFLUSH )
{
   hb_retl( hb_comFlush( hb_parni( 1 ), HB_COM_OFLUSH ) != -1 );
}

/* com_CTS( <nComPort> ) -> <lCTSActive>
 */
HB_FUNC( COM_CTS )
{
   hb_ctComTestMSR( HB_COM_MSR_CTS );
}

/* com_DCD( <nComPort> ) -> <lDCDActive>
 */
HB_FUNC( COM_DCD )
{
   hb_ctComTestMSR( HB_COM_MSR_DCD );
}

/* com_DSR( <nComPort> ) -> <lDSRActive>
 */
HB_FUNC( COM_DSR )
{
   hb_ctComTestMSR( HB_COM_MSR_DSR );
}

/* com_Ring( <nComPort> ) -> <lActiveRing>
 */
HB_FUNC( COM_RING )
{
   hb_ctComTestMSR( HB_COM_MSR_RI );
}

/* com_RTS( <nComPort>, [<lNewRTSStatus>] ) -> <lOldRTSStatus>
 */
HB_FUNC( COM_RTS )
{
   int iMCR, iClr = 0, iSet = 0;

   if( HB_ISLOG( 2 ) )
   {
      if( hb_parl( 2 ) )
         iSet = HB_COM_MCR_RTS;
      else
         iClr = HB_COM_MCR_RTS;
   }
   hb_comMCR( hb_parni( 1 ), &iMCR, iClr, iSet );
   hb_retl( ( iMCR & HB_COM_MCR_DTR ) != 0 );
}

/* com_DTR( <nComPort>, [<lNewDTRStatus>] ) -> <lOldDTRStatus>
 */
HB_FUNC( COM_DTR )
{
   int iMCR, iClr = 0, iSet = 0;

   if( HB_ISLOG( 2 ) )
   {
      if( hb_parl( 2 ) )
         iSet = HB_COM_MCR_DTR;
      else
         iClr = HB_COM_MCR_DTR;
   }
   hb_comMCR( hb_parni( 1 ), &iMCR, iClr, iSet );
   hb_retl( ( iMCR & HB_COM_MCR_DTR ) != 0 );
}

/* com_MCR( <nComPort>, [<nMCR>] ) -> <nMCR> (MCR_*)
 */
HB_FUNC( COM_MCR )
{
   int iMCR, iClr, iSet;

   if( HB_ISNUM( 2 ) )
   {
      iClr = 0xff;
      iSet = hb_parni( 2 ) & 0xff;
   }
   else
      iClr = iSet = 0;

   if( hb_comMCR( hb_parni( 1 ), &iMCR, iClr, iSet ) == -1 )
      iMCR = MCR_ERROR;

   hb_retni( iMCR );
}

/* com_MSR( <nComPort> ) -> <nMSR> (MSR_*)
 */
HB_FUNC( COM_MSR )
{
   int iMSR;

   if( hb_comMSR( hb_parni( 1 ), &iMSR ) == -1 )
      iMSR = MSR_ERROR;

   hb_retni( iMSR );
}

/* com_LSR( <nComPort> ) -> <nLSR> (LSR_*)
 */
HB_FUNC( COM_LSR )
{
   int iLSR;

   if( hb_comLSR( hb_parni( 1 ), &iLSR ) == -1 )
      iLSR = LSR_ERROR;

   hb_retni( iLSR );
}

/* com_Break( <nComPort>, <nDurationInMilliSecs>=100 ) -> <lSuccess>
 */
HB_FUNC( COM_BREAK )
{
   hb_retl( hb_comSendBreak( hb_parni( 1 ), hb_parnidef( 2, 100 ) ) != 0 );
}

/* com_Hard( <nComPort>, [<lNewHandshake>], [<lDTR/DSR>] ) -> <lOldHandshake>
 */
HB_FUNC( COM_HARD )
{
   int iPort = hb_parni( 1 ), iFlow, iMask;
   HB_BOOL fResult = HB_FALSE;

   if( hb_comFlowControl( iPort, &iFlow, -1 ) != -1 )
   {
      iMask = hb_parl( 3 ) ? ( HB_COM_FLOW_IDTRDSR | HB_COM_FLOW_ODTRDSR ) :
                             ( HB_COM_FLOW_IRTSCTS | HB_COM_FLOW_ORTSCTS );
      fResult = ( iFlow & iMask ) == iMask;

      if( HB_ISLOG( 2 ) )
      {
         iFlow &= ~( HB_COM_FLOW_IDTRDSR | HB_COM_FLOW_ODTRDSR |
                     HB_COM_FLOW_IRTSCTS | HB_COM_FLOW_ORTSCTS );
         if( hb_parl( 2 ) )
            iFlow |= iMask;
         hb_comFlowControl( iPort, NULL, iFlow );
      }
   }
   hb_retl( fResult );
}

/* com_Soft( <nComPort>, [<lNewHandshake>],
             [<cXONchar>], [<cXOFFchar>] ) -> <lOldHandshake>
 */
HB_FUNC( COM_SOFT )
{
   int iPort = hb_parni( 1 ), iFlow, iMask;
   HB_BOOL fResult = HB_FALSE;

   if( hb_comFlowControl( iPort, &iFlow, -1 ) != -1 )
   {
      iMask = ( HB_COM_FLOW_XON | HB_COM_FLOW_XOFF );
      fResult = ( iFlow & iMask ) == iMask;

      if( HB_ISLOG( 2 ) )
      {
         if( hb_parl( 2 ) )
            iFlow |= iMask;
         else
            iFlow &= ~iMask;
         hb_comFlowControl( iPort, NULL, iFlow );
      }
      if( hb_pcount() > 2 )
         hb_comFlowChars( iPort, hb_ctComCharParam( 3 ), hb_ctComCharParam( 4 ) );
   }
   hb_retl( fResult );
}

/* com_Soft_R( <nComPort>, [<lXOFFFlag>] ) -> <lXOFFFlag>
 */
HB_FUNC( COM_SOFT_R )
{
   HB_BOOL fResult = HB_FALSE;
   int iPort = hb_parni( 1 ), iMode;

   if( HB_ISLOG( 2 ) )
      hb_comFlowSet( iPort, HB_COM_FL_SOFT |
                            ( hb_parl( 2 ) ? HB_COM_FL_OOFF : HB_COM_FL_OON ) );

   iMode = hb_comOutputState( iPort );
   if( iMode > 0 )
      fResult = ( iMode & HB_COM_TX_XOFF ) != 0;

   hb_retl( fResult );
}

/* com_Soft_S( <nComPort> ) -> <lXOFFFlag>
 */
HB_FUNC( COM_SOFT_S )
{
   HB_BOOL fResult = HB_FALSE;
   int iMode = hb_comInputState( hb_parni( 1 ) );

   if( iMode > 0 )
      fResult = ( iMode & HB_COM_RX_XOFF ) != 0;

   hb_retl( fResult );
}

/* com_ErrChr( <nComPort>, [<nErrorCharacter|cErrorCharacter>] ) -> <lChanged>
 */
HB_FUNC( COM_ERRCHR )
{
   hb_retl( hb_comErrorChar( hb_parni( 1 ), hb_ctComCharParam( 2 ) ) != -1 );
}

/*  com_Remote( <nComPort>, [<nCharacter|cCharacter>] ) -> <lActive>
 */
HB_FUNC( COM_REMOTE )
{
   hb_retl( hb_comDiscardChar( hb_parni( 1 ), hb_ctComCharParam( 2 ) ) > 0 );
}

/* com_SMode( <nComPort> ) -> <nSendMode>
 */
HB_FUNC( COM_SMODE )
{
   int iMode = hb_comOutputState( hb_parni( 1 ) ), iResult = 0;

   if( iMode > 0 )
   {
      if( iMode & HB_COM_TX_EMPTY )
         iResult |= SMODE_EMPTY;
      if( iMode & HB_COM_TX_XOFF )
         iResult |= SMODE_SOFT;
      if( iMode & ( HB_COM_TX_CTS | HB_COM_TX_DSR | HB_COM_TX_DCD ) )
         iResult |= SMODE_HARD;
      if( iMode & HB_COM_TX_RFLUSH )
         iResult |= SMODE_RFLUSH;
   }

   hb_retni( iResult );
}

/* com_Event( <nComPort>, <nMode> ) -> <nCode>
 */
HB_FUNC( COM_EVENT )
{
   /* TODO: unsupported */
   hb_retni( 0 );
}

/* com_Key( <nComPort>, [<nKeyValue1>], [<nKeyValue2>] ) -> <lActive>
 */
HB_FUNC( COM_KEY )
{
   /* TODO: unsupported */
   hb_retl( HB_FALSE );
}

/* com_SKey( [<nComPort>], [<nKeyValue1|cKeyValue1>],
 *                         [<nKeyValue2|cKeyValue2>] ) -> <lActive>
 */
HB_FUNC( COM_SKEY )
{
   /* TODO: unsupported */
   hb_retl( HB_FALSE );
}

/* com_Init( <nComPort>, [<nBaudRate>=300], [<cParity:E,O,M,S,N>=N],
 *           [<nDataLength:7,8>=8], [<nStopBits:1,2>=1] ) -> <lInitialized>
 */
HB_FUNC( COM_INIT )
{
   int iPort = hb_parni( 1 ),
       iBaud = hb_parnidef( 2, 300 ),
       iParity = hb_parcx( 3 )[ 0 ],
       iSize = hb_parnidef( 4, 8 ),
       iStop = hb_parnidef( 5, 1 );

   hb_retl( hb_comInit( iPort, iBaud, iParity, iSize, iStop ) != -1 );
}

/* com_Open( <nComPort>, [<nBufferIn>=100] [, <nBufferOut>=0],
 *           [<lTrapMode>] ) -> <lStatus>
 */
HB_FUNC( COM_OPEN )
{
   int iPort = hb_parni( 1 );

   /* TODO: add support for <nBufferIn> */
   /* TODO: add support for <nBufferOut> */
   /* TODO: add support for <lTrapMode> */
   hb_comClose( iPort );
   hb_retl( hb_comOpen( iPort ) != -1 );
}

/* com_Close( <nComPort> ) -> <lClosed>
 */
HB_FUNC( COM_CLOSE )
{
   int iPort = hb_parni( 1 );

   hb_comFlush( iPort, HB_COM_IOFLUSH );
   hb_retl( hb_comClose( iPort ) != -1 );
}

/* com_Read( <nComPort>, [<nLength>], [<lNoDelete>] ) -> <cCharacterstring>
 */
HB_FUNC( COM_READ )
{
   char buffer[ 1024 ];
   char * data;
   long lLen, lRecv;
   int iPort = hb_parni( 1 );

   /* TODO: add support for <lNoDelete> */

   if( HB_ISNUM( 2 ) )
      lLen = hb_parnl( 2 );
   else
   {
      lLen = hb_comInputCount( iPort );
      if( lLen < ( long ) ( sizeof( buffer ) >> 1 ) )
         lLen = sizeof( buffer );
      else
         lLen <<= 2;
   }
   if( lLen <= ( long ) sizeof( buffer ) )
      data = buffer;
   else
      data = ( char * ) hb_xgrab( lLen + 1 );

   lRecv = hb_comRecv( iPort, buffer, lLen, 0 );
   if( lRecv < 0 )
      lRecv = 0;

   if( data == buffer )
      hb_retclen( data, lRecv );
   else if( lLen > 16 && ( lLen >> 2 ) > lRecv )
   {
      hb_retclen( data, lRecv );
      hb_xfree( data );
   }
   else
      hb_retclen_buffer( data, lRecv );
}

/* com_Send( <nComPort>, <cString|nChar> ) -> <nNotSendLength>
 */
HB_FUNC( COM_SEND )
{
   const char * data = hb_parc( 2 );
   long lLen = 0;
   char buffer;

   /* TODO: add automatic drain call for ports open without send buffer */

   if( data )
      lLen = ( long ) hb_parclen( 2 );
   else if( HB_ISNUM( 2 ) )
   {
      buffer = ( unsigned char ) hb_parni( 2 );
      data = &buffer;
      lLen = 1;
   }

   if( lLen )
   {
      long lResult = hb_comSend( hb_parni( 1 ), data, lLen, 0 );
      if( lResult > 0 )
         lLen -= lResult;
   }

   hb_retnl( lLen );
}

/* com_Num() -> <nMaxCom>
 */
HB_FUNC( COM_NUM )
{
   hb_retni( hb_comLastNum() );
}

/* com_GetIO( <nComPort> ) -> <nIOPort> | -1
 */
HB_FUNC( COM_GETIO )
{
   /* TODO! */
}

/* com_SetIO( <nComPort>, <nIOPort|cIOPort> ) -> <lChanged>
 */
HB_FUNC( COM_SETIO )
{
   /* TODO! */
}

/* com_GetIRQ( <nComPort> ) -> <nIRQ> | -1
 */
HB_FUNC( COM_GETIRQ )
{
   /* TODO! */
}

/* com_SetIRQ( <nComPort>, <nIRQ|cIRQ> ) -> <lChanged>
 */
HB_FUNC( COM_SETIRQ )
{
   /* TODO! */
}

/* com_DevName( <nComPort> [, <cNewName> ] ) -> <cPrevName>
 */
HB_FUNC( COM_DEVNAME )
{
   int iPort = hb_parni( 1 );
   const char * szDevName = hb_parc( 2 );
   char buffer[ HB_COM_DEV_NAME_MAX ];

   hb_retc( hb_comGetDevice( iPort, buffer, sizeof( buffer ) ) );
   if( szDevName )
      hb_comSetDevice( iPort, szDevName );
}
