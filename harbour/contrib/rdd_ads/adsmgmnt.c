/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Advantage Database Server RDD ( Management functions )
 *
 * Copyright 2001 Brian Hays  <bhays@abacuslaw.com>
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


#define HB_OS_WIN_32_USED

#include "rddads.h"


/*
               Advantage Managment API Examples
*/

ADSHANDLE hMgmtHandle = 0;

HB_FUNC( ADSMGCONNECT )
{
      /*   ulRetVal = AdsMgConnect( "\\\\server\\volume:", NULL, NULL, &hMgmtHandle );
      //   UNSIGNED32 ENTRYPOINT AdsMgConnect( UNSIGNED8   *pucServerName,
      //                                       UNSIGNED8   *pucUserName,
      //                                       UNSIGNED8   *pucPassword,
      //                                       ADSHANDLE   *phMgmtHandle );
      */

   hb_retnl( AdsMgConnect( (UNSIGNED8 *) hb_parc(1), (UNSIGNED8 *) hb_parc(2), (UNSIGNED8 *) hb_parc(3), &hMgmtHandle) );
}

HB_FUNC( ADSMGDISCONNECT )
{
   hb_retnl( AdsMgDisconnect( hMgmtHandle ) );
   hMgmtHandle = 0;
}

HB_FUNC( ADSMGGETINSTALLINFO )
{
   UNSIGNED32  ulRetVal;
   UNSIGNED16  usStructSize;
   ADS_MGMT_INSTALL_INFO   stInstallInfo;

   usStructSize = sizeof( ADS_MGMT_INSTALL_INFO );
   ulRetVal = AdsMgGetInstallInfo( hMgmtHandle, &stInstallInfo, &usStructSize );

   //if ( sizeof( ADS_MGMT_INSTALL_INFO ) < usStructSize )
   //   printf( "\nInstallation Information structure on server is larger." );
   //   printf( "\nMore possible info available." );

   if ( ulRetVal == AE_SUCCESS )
   {
      hb_reta( 8 );
      hb_stornl( stInstallInfo.ulUserOption               , -1, 1 );  /* User option purchased*/
      hb_storc ( (char *) stInstallInfo.aucRegisteredOwner, -1, 2 );  /* Registered owner     */
      hb_storc ( (char *) stInstallInfo.aucVersionStr     , -1, 3 );  /* Advantage version    */
      hb_storc ( (char *) stInstallInfo.aucInstallDate    , -1, 4 );  /* Install date string  */
      hb_storc ( (char *) stInstallInfo.aucOemCharName    , -1, 5 );  /* OEM char language    */
      hb_storc ( (char *) stInstallInfo.aucAnsiCharName   , -1, 6 );  /* ANSI char language   */
      hb_storc ( (char *) stInstallInfo.aucEvalExpireDate , -1, 7 );  /* Eval expiration date */
      hb_storc ( (char *) stInstallInfo.aucSerialNumber   , -1, 8 );  /* Serial number string */
   }
   else
     hb_ret( );
}

HB_FUNC( ADSMGGETACTIVITYINFO )
{
   UNSIGNED32  ulRetVal;
   UNSIGNED16  usStructSize;
   ADS_MGMT_ACTIVITY_INFO  stActivityInfo;
   UINT iOption = hb_parni( 1 );

   usStructSize = sizeof( ADS_MGMT_ACTIVITY_INFO );
   ulRetVal = AdsMgGetActivityInfo( hMgmtHandle, &stActivityInfo, &usStructSize );
      //   if ( sizeof( ADS_MGMT_ACTIVITY_INFO ) < usStructSize )
      //      printf( "\nActivity Information structure on server is larger." );
      //      printf( "\nMore possible info available." );

   hb_ret();                            /* default to NIL */

   if ( iOption && ulRetVal == AE_SUCCESS )
   {
      switch ( iOption )
      {
         case 1 :
            hb_retnl( stActivityInfo.ulOperations );     /* Number operations since started */
            break;

         case 2 :
            hb_retnl( stActivityInfo.ulLoggedErrors );   /* Number logged errors            */
            break;


         case 3 :
            hb_reta( 4 );                                /* Length of time ADS has been up  */
            hb_stornl( stActivityInfo.stUpTime.usDays,    -1, 1 );
            hb_stornl( stActivityInfo.stUpTime.usHours,   -1, 2 );
            hb_stornl( stActivityInfo.stUpTime.usMinutes, -1, 3 );
            hb_stornl( stActivityInfo.stUpTime.usSeconds, -1, 4 );
            break;


         case 4 :
            hb_reta( 3 );                                /* Users in use, max, rejected     */
            hb_stornl( stActivityInfo.stUsers.ulInUse,    -1, 1 );
            hb_stornl( stActivityInfo.stUsers.ulMaxUsed,  -1, 2 );
            hb_stornl( stActivityInfo.stUsers.ulRejected, -1, 3 );
            break;


         case 5 :
            hb_reta( 3 );                                /* Conns in use, max, rejected     */
            hb_stornl( stActivityInfo.stConnections.ulInUse,    -1, 1 );
            hb_stornl( stActivityInfo.stConnections.ulMaxUsed,  -1, 2 );
            hb_stornl( stActivityInfo.stConnections.ulRejected, -1, 3 );
            break;


         case 6 :
            hb_reta( 3 );                                /* WAs in use, max, rejected       */
            hb_stornl( stActivityInfo.stWorkAreas.ulInUse,    -1, 1 );
            hb_stornl( stActivityInfo.stWorkAreas.ulMaxUsed,  -1, 2 );
            hb_stornl( stActivityInfo.stWorkAreas.ulRejected, -1, 3 );
            break;


         case 7 :
            hb_reta( 3 );                                /* Tables in use, max, rejected    */
            hb_stornl( stActivityInfo.stTables.ulInUse,    -1, 1 );
            hb_stornl( stActivityInfo.stTables.ulMaxUsed,  -1, 2 );
            hb_stornl( stActivityInfo.stTables.ulRejected, -1, 3 );
            break;


         case 8 :
            hb_reta( 3 );                                /* Indexes in use, max, rejected   */
            hb_stornl( stActivityInfo.stIndexes.ulInUse,    -1, 1 );
            hb_stornl( stActivityInfo.stIndexes.ulMaxUsed,  -1, 2 );
            hb_stornl( stActivityInfo.stIndexes.ulRejected, -1, 3 );
            break;


         case 9 :
            hb_reta( 3 );                                /* Locks in use, max, rejected     */
            hb_stornl( stActivityInfo.stLocks.ulInUse,    -1, 1 );
            hb_stornl( stActivityInfo.stLocks.ulMaxUsed,  -1, 2 );
            hb_stornl( stActivityInfo.stLocks.ulRejected, -1, 3 );
            break;


         case 10 :
            hb_reta( 3 );                                /* TPS header elems in use, max    */
            hb_stornl( stActivityInfo.stTpsHeaderElems.ulInUse,    -1, 1 );
            hb_stornl( stActivityInfo.stTpsHeaderElems.ulMaxUsed,  -1, 2 );
            hb_stornl( stActivityInfo.stTpsHeaderElems.ulRejected, -1, 3 );
            break;


         case 11 :
            hb_reta( 3 );                                /* TPS vis elems in use, max       */
            hb_stornl( stActivityInfo.stTpsVisElems.ulInUse,    -1, 1 );
            hb_stornl( stActivityInfo.stTpsVisElems.ulMaxUsed,  -1, 2 );
            hb_stornl( stActivityInfo.stTpsVisElems.ulRejected, -1, 3 );
            break;


         case 12 :
            hb_reta( 3 );                                /* TPS memo elems in use, max      */
            hb_stornl( stActivityInfo.stTpsMemoElems.ulInUse,    -1, 1 );
            hb_stornl( stActivityInfo.stTpsMemoElems.ulMaxUsed,  -1, 2 );
            hb_stornl( stActivityInfo.stTpsMemoElems.ulRejected, -1, 3 );
            break;


         case 13 :
            hb_reta( 3 );                                /* Worker threads in use, max      */
            hb_stornl( stActivityInfo.stWorkerThreads.ulInUse,    -1, 1 );
            hb_stornl( stActivityInfo.stWorkerThreads.ulMaxUsed,  -1, 2 );
            hb_stornl( stActivityInfo.stWorkerThreads.ulRejected, -1, 3 );
            break;

      }
   }

}

HB_FUNC( ADSMGGETCOMMSTATS )
{
   UNSIGNED32  ulRetVal ;
   UNSIGNED16  usStructSize;
   ADS_MGMT_COMM_STATS     stCommStats;

   usStructSize = sizeof( ADS_MGMT_COMM_STATS );
   ulRetVal = AdsMgGetCommStats( hMgmtHandle, &stCommStats, &usStructSize );
      //   if ( sizeof( ADS_MGMT_COMM_STATS ) < usStructSize )
      //   {
      //      HB_TRACE(HB_TR_INFO, ("The Communication Statistics structure on the server is larger.
      //         \nMore info is available with the current ACE.H." ));
      //   }
   if ( ulRetVal == AE_SUCCESS )
   {
      hb_reta( 11 );
      hb_stornd( stCommStats.dPercentCheckSums,  -1, 1 );  /* % of pkts with checksum failures */
      hb_stornl( stCommStats.ulTotalPackets,     -1, 2 );  /* Total packets received           */
      hb_stornl( stCommStats.ulRcvPktOutOfSeq  , -1, 3 );  /* Receive packets out of sequence  */
      hb_stornl( stCommStats.ulNotLoggedIn     , -1, 4 );  /* Packet owner not logged in       */
      hb_stornl( stCommStats.ulRcvReqOutOfSeq  , -1, 5 );  /* Receive requests out of sequence */
      hb_stornl( stCommStats.ulCheckSumFailures, -1, 6 );  /* Checksum failures                */
      hb_stornl( stCommStats.ulDisconnectedUsers,-1, 7 );  /* Server initiated disconnects     */
      hb_stornl( stCommStats.ulPartialConnects , -1, 8 );  /* Removed partial connections      */
      hb_stornl( stCommStats.ulInvalidPackets  , -1, 9 );  /* Rcvd invalid packets (NT only)   */
      hb_stornl( stCommStats.ulRecvFromErrors  , -1, 10);  /* RecvFrom failed (NT only)        */
      hb_stornl( stCommStats.ulSendToErrors    , -1, 11);  /* SendTo failed (NT only)          */
   }
   else
     hb_ret(  );

}

HB_FUNC( ADSMGRESETCOMMSTATS )
{
   if ( hMgmtHandle )
      hb_retnl( AdsMgResetCommStats( hMgmtHandle ) );
   else
      hb_retnl( -1 );

}

HB_FUNC( ADSMGGETCONFIGINFO )
{
   UNSIGNED32 ulRetVal ;
   ADS_MGMT_CONFIG_PARAMS  stConfigValues;
   ADS_MGMT_CONFIG_MEMORY  stConfigMemory;
   UNSIGNED16 usConfigValuesStructSize;
   UNSIGNED16 usConfigMemoryStructSize;
   int iOption = ISNUM(1) ? hb_parni(1) : 1 ;  /* Pass 0 for Values, 1 for memory */

   usConfigValuesStructSize = sizeof( ADS_MGMT_CONFIG_PARAMS );
   usConfigMemoryStructSize = sizeof( ADS_MGMT_CONFIG_MEMORY );

   usConfigValuesStructSize = sizeof( ADS_MGMT_CONFIG_PARAMS );
   usConfigMemoryStructSize = sizeof( ADS_MGMT_CONFIG_MEMORY );

   ulRetVal = AdsMgGetConfigInfo( hMgmtHandle, &stConfigValues,
                                 &usConfigValuesStructSize,
                                 &stConfigMemory, &usConfigMemoryStructSize );

      //   if ( sizeof( ADS_MGMT_CONFIG_PARAMS ) < usConfigValuesStructSize )
      //      printf( "\nConfiguration Values structure on server is larger." );
      //   if ( sizeof( ADS_MGMT_CONFIG_MEMORY ) < usConfigMemoryStructSize )
      //      printf( "\nConfiguration Memory structure on server is larger." );
   if ( ulRetVal == AE_SUCCESS )
   {
      if ( iOption == 0 )
      {
         hb_reta( 26 );
         hb_stornl( stConfigValues.ulNumConnections       , -1, 1 );  /* number connections            */
         hb_stornl( stConfigValues.ulNumWorkAreas         , -1, 2 );  /* number work areas             */
         hb_stornl( stConfigValues.ulNumTables            , -1, 3 );  /* number tables                 */
         hb_stornl( stConfigValues.ulNumIndexes           , -1, 4 );  /* number indexes                */
         hb_stornl( stConfigValues.ulNumLocks             , -1, 5 );  /* number locks                  */
         hb_stornl( stConfigValues.ulUserBufferSize       , -1, 6 );  /* user buffer                   */
         hb_stornl( stConfigValues.ulStatDumpInterval     , -1, 7 );  /* statistics dump interval      */
         hb_stornl( stConfigValues.ulErrorLogMax          , -1, 8 );  /* max size of error log         */
         hb_stornl( stConfigValues.ulNumTPSHeaderElems    , -1, 9 );  /* number TPS header elems       */
         hb_stornl( stConfigValues.ulNumTPSVisibilityElems,-1, 10);  /* number TPS vis elems          */
         hb_stornl( stConfigValues.ulNumTPSMemoTransElems , -1, 11);  /* number TPS memo elems         */
         hb_stornl( stConfigValues.usNumReceiveECBs       , -1, 12);  /* number rcv ECBs (NLM only)    */
         hb_stornl( stConfigValues.usNumSendECBs          , -1, 13);  /* number send ECBs (NLM only)   */
         hb_stornd( stConfigValues.usNumBurstPackets      , -1, 14);  /* number packets per burst      */
         hb_stornl( stConfigValues.usNumWorkerThreads     , -1, 15);  /* number worker threads         */
         hb_stornl( stConfigValues.usSortBuffSize         , -1, 16);  /* index sort buffer size        */
         hb_storni( stConfigValues.ucReserved1            , -1, 17);  /* reserved                      */
         hb_storni( stConfigValues.ucReserved2            , -1, 18);  /* reserved                      */
         hb_storc ( (char *) stConfigValues.aucErrorLog   , -1, 19);  /* error log path         */
         hb_storc ( (char *) stConfigValues.aucSemaphore  , -1, 20);  /* semaphore file path    */
         hb_storc ( (char *) stConfigValues.aucTransaction, -1, 21);  /* TPS log file path      */
         hb_storni( stConfigValues.ucReserved3            , -1, 22);  /* reserved                      */
         hb_storni( stConfigValues.ucReserved4            , -1, 23);  /* reserved                      */
         hb_stornl( stConfigValues.usSendIPPort           , -1, 24);  /* NT Service IP send port #     */
         hb_stornl( stConfigValues.usReceiveIPPort        , -1, 25);  /* NT Service IP rcv port #      */
         hb_stornl( stConfigValues.usReserved5            , -1, 26);  /* reserved                      */

      }else
      if ( iOption == 1 )
      {
         hb_reta( 13 );
         hb_stornd( stConfigMemory.ulTotalConfigMem      , -1, 1 );  /* Total mem taken by cfg params */
         hb_stornl( stConfigMemory.ulConnectionMem       , -1, 2 );  /* memory taken by connections   */
         hb_stornl( stConfigMemory.ulWorkAreaMem         , -1, 3 );  /* memory taken by work areas    */
         hb_stornl( stConfigMemory.ulTableMem            , -1, 4 );  /* memory taken by tables        */
         hb_stornl( stConfigMemory.ulIndexMem            , -1, 5 );  /* memory taken by indexes       */
         hb_stornl( stConfigMemory.ulLockMem             , -1, 6 );  /* memory taken by locks         */
         hb_stornl( stConfigMemory.ulUserBufferMem       , -1, 7 );  /* memory taken by user buffer   */
         hb_stornl( stConfigMemory.ulTPSHeaderElemMem    , -1, 8 );  /* memory taken by TPS hdr elems */
         hb_stornl( stConfigMemory.ulTPSVisibilityElemMem, -1, 9 );  /* memory taken by TPS vis elems */
         hb_stornl( stConfigMemory.ulTPSMemoTransElemMem , -1, 10);  /* mem taken by TPS memo elems   */
         hb_stornl( stConfigMemory.ulReceiveEcbMem       , -1, 11);  /* mem taken by rcv ECBs (NLM)   */
         hb_stornl( stConfigMemory.ulSendEcbMem          , -1, 12);  /* mem taken by send ECBs (NLM)  */
         hb_stornl( stConfigMemory.ulWorkerThreadMem     , -1, 13);  /* mem taken by worker threads   */

      }
   }
   else
     hb_ret(  );

}

HB_FUNC( ADSMGGETUSERNAMES )   /* Return array of connected users */
{

   UNSIGNED32  ulRetVal ;
   UNSIGNED16  ulMaxUsers = 100 ;        /* needed for array memory allocation; caller can set with 2nd arg */
   UNSIGNED32  ulCount;
   UNSIGNED16  usStructSize = sizeof( ADS_MGMT_USER_INFO );
   ADS_MGMT_USER_INFO*  pastUserInfo;
//   ADS_MGMT_USER_INFO  astUserInfo[MAX_NUM_USERS];
// bh:  Enhancement:  Get # of tables from ADS_MGMT_ACTIVITY_INFO.stUsers instead of set size

   if ( ISNUM( 2 ) )
      ulMaxUsers = hb_parnl( 2 );

   pastUserInfo = (ADS_MGMT_USER_INFO *) hb_xgrab( sizeof( ADS_MGMT_USER_INFO ) * ulMaxUsers );
      //   AdsMgGetUserNames ( ADSHANDLE hMgmtConnect,
      //                      UNSIGNED8 *pucFileName,
      //                      ADS_MGMT_USER_INFO astUserInfo[],
      //                      UNSIGNED16 *pusArrayLen,
      //                      UNSIGNED16 *pusStructSize );

   ulRetVal = AdsMgGetUserNames( hMgmtHandle, ISCHAR( 1 ) ? (UNSIGNED8 *) hb_parc( 1 ) : NULL,
                                 pastUserInfo,
                                 &ulMaxUsers,
                                 &usStructSize );
      //if ( sizeof( ADS_MGMT_USER_INFO ) < usStructSize )
      //   {
      //      HB_TRACE(HB_TR_INFO, ("The \nUser Information structure on the server is larger.
      //         \nMore info is available with the current ACE.H." ));
      //   }

   if ( ulRetVal == AE_SUCCESS )
   {
      hb_reta( ulMaxUsers );
      for ( ulCount = 0; ulCount < ulMaxUsers; ulCount++ )
      {
         hb_storc ( (char *) pastUserInfo[ulCount].aucUserName , -1, ulCount+1);
      }
   }else
      hb_reta( 0 );

   hb_xfree( pastUserInfo );

}
/*

HB_FUNC( ADSMGGETOPENTABLES )
{
   UNSIGNED32  ulRetVal ;
   UNSIGNED32  ulMaxUsers = 100 ;        // needed for array memory allocation; caller can set with 2nd arg
   UNSIGNED32  ulCount;
   UNSIGNED16  usStructSize = sizeof( ADS_MGMT_USER_INFO );
   ADS_MGMT_USER_INFO*  pastUserInfo;
Get # of tables from ADS_MGMT_ACTIVITY_INFO
}

HB_FUNC( ADSMGGETOPENINDEXES )
{
   UNSIGNED32              ulRetVal = AE_SUCCESS;
   AdsMgGetOpenIndexes();
}

HB_FUNC( ADSMGGETLOCKS )
{
   UNSIGNED32              ulRetVal = AE_SUCCESS;
   AdsMgGetLocks();
}

HB_FUNC( ADSMGGETSERVERTYPE )
{
   UNSIGNED32              ulRetVal = AE_SUCCESS;
   AdsMgGetServerType();
}

HB_FUNC( ADSMGGETWORKERTHREADACTIVITY )
{
   UNSIGNED32              ulRetVal = AE_SUCCESS;
   AdsMgGetWorkerThreadActivity();
}

HB_FUNC( ADSMGGETLOCKOWNER )
{
   UNSIGNED32              ulRetVal = AE_SUCCESS;
   AdsMgGetLockOwner();
}

HB_FUNC( ADSMGKILLUSER )
{
   UNSIGNED32              ulRetVal = AE_SUCCESS;
   AdsMgKillUser();
}
*/
