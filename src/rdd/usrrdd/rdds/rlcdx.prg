/*
 * Harbour Project source code:
 *    RLCDX
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

/*
 * A simple RDD which introduce lock counters. It has full DBFCDX
 * functionality from which it inherits but if you execute dbRLock(100)
 * twice then you will have to also repeat call to dbRUnlock(100) to
 * really unlock the record 100. The same if for FLock()
 * This idea comes from one of messages sent by Mindaugas Kavaliauskas.
 */

#include "rddsys.ch"
#include "hbusrrdd.ch"

ANNOUNCE RLCDX

/*
 * methods: NEW and RELEASE receive pointer to work area structure
 * not work area number. It's necessary because the can be executed
 * before work area is allocated
 * these methods does not have to execute SUPER methods - these is
 * always done by low level USRRDD code
 */

STATIC FUNCTION RLCDX_NEW( pWA )

   LOCAL aWData := { 0, {} }

   /*
    * Set in our private AREA item the array with number of FLOCKs
    * recursively called and array with LOCKED records
    */

   USRRDD_AREADATA( pWA, aWData )

   RETURN HB_SUCCESS

STATIC FUNCTION RLCDX_LOCK( nWA, aLockInfo )

   LOCAL aWData, nResult, xRecId, i

   aWData := USRRDD_AREADATA( nWA )

   /* Convert EXCLUSIVE locks to DBLM_MULTIPLE */
   IF aLockInfo[ UR_LI_METHOD ] == DBLM_EXCLUSIVE

      aLockInfo[ UR_LI_METHOD ] := DBLM_MULTIPLE
      aLockInfo[ UR_LI_RECORD ] := RecNo()

   ENDIF

   IF aLockInfo[ UR_LI_METHOD ] == DBLM_MULTIPLE      /* RLOCK */

      IF aWData[ 1 ] > 0
         aLockInfo[ UR_LI_RESULT ] := .T.
         RETURN HB_SUCCESS
      ENDIF

      xRecID := aLockInfo[ UR_LI_RECORD ]
      IF Empty( xRecID )
         xRecID := RecNo()
      ENDIF

      IF aWData[ 1 ] > 0
         aLockInfo[ UR_LI_RESULT ] := .T.
         RETURN HB_SUCCESS
      ELSEIF ( i := AScan( aWData[ 2 ], {| x | x[ 1 ] == xRecID } ) ) != 0
         ++aWData[ 2, i, 2 ]
         aLockInfo[ UR_LI_RESULT ] := .T.
         RETURN HB_SUCCESS
      ENDIF

      nResult := UR_SUPER_LOCK( nWA, aLockInfo )
      IF nResult == HB_SUCCESS
         IF aLockInfo[ UR_LI_RESULT ]
            AAdd( aWData[ 2 ], { xRecID, 1 } )
         ENDIF
      ENDIF

      RETURN nResult

   ELSEIF aLockInfo[ UR_LI_METHOD ] == DBLM_FILE      /* FLOCK */

      IF aWData[ 1 ] > 0
         ++aWData[ 1 ]
         RETURN HB_SUCCESS
      ENDIF

      nResult := UR_SUPER_LOCK( nWA, aLockInfo )
      IF nResult == HB_SUCCESS

         /* FLOCK always first remove all RLOCKs, even if it fails */
         ASize( aWData[ 2 ], 0 )

         IF aLockInfo[ UR_LI_RESULT ]
            aWData[ 1 ] := 1
         ENDIF
      ENDIF

      RETURN nResult

   ENDIF

   aLockInfo[ UR_LI_RESULT ] := .F.

   RETURN HB_FAILURE

STATIC FUNCTION RLCDX_UNLOCK( nWA, xRecID )

   LOCAL aWData := USRRDD_AREADATA( nWA ), i

   IF HB_ISNUMERIC( xRecID ) .AND. xRecID > 0
      IF ( i := AScan( aWData[ 2 ], {| x | x[ 1 ] == xRecID } ) ) != 0
         IF --aWData[ 2, i, 2 ] > 0
            RETURN HB_SUCCESS
         ENDIF
         hb_ADel( aWData[ 2 ], i, .T. )
      ELSE
         RETURN HB_SUCCESS
      ENDIF
   ELSE
      IF aWData[ 1 ] > 1
         --aWData[ 1 ]
         RETURN HB_SUCCESS
      ENDIF
      aWData[ 1 ] := 0
      ASize( aWData[ 2 ], 0 )
   ENDIF

   RETURN UR_SUPER_UNLOCK( nWA, xRecID )

STATIC FUNCTION RLCDX_APPEND( nWA, lUnlockAll )

   LOCAL aWData, nResult, xRecId, i

   /* Never unlock other records, they have to be explicitly unlocked */

   lUnlockAll := .F.

   nResult := UR_SUPER_APPEND( nWA, lUnlockAll )
   IF nResult == HB_SUCCESS

      aWData := USRRDD_AREADATA( nWA )
      IF aWData[ 1 ] == 0
         xRecId := RecNo()
         /* Some RDDs may allow to set phantom locks with RLOCK so we should
            check if it's not the case and increase the counter when it is */
         IF ( i := AScan( aWData[ 2 ], {| x | x[ 1 ] == xRecID } ) ) != 0
            ++aWData[ 2, i, 2 ]
         ELSE
            AAdd( aWData[ 2 ], { xRecID, 1 } )
         ENDIF
      ENDIF
   ENDIF

   RETURN nResult

/* Force linking DBFCDX from which our RDD inherits */

REQUEST DBFCDX

/*
 * This function have to exist in all RDD and then name have to be in
 * format: <RDDNAME>_GETFUNCTABLE
 */

FUNCTION RLCDX_GETFUNCTABLE( pFuncCount, pFuncTable, pSuperTable, nRddID, pSuperRddID )

   LOCAL cSuperRDD := "DBFCDX" /* We are inheriting from DBFCDX */
   LOCAL aMethods[ UR_METHODCOUNT ]

   aMethods[ UR_NEW  ]   := ( @RLCDX_NEW()    )
   aMethods[ UR_LOCK ]   := ( @RLCDX_LOCK()   )
   aMethods[ UR_UNLOCK ] := ( @RLCDX_UNLOCK() )
   aMethods[ UR_APPEND ] := ( @RLCDX_APPEND() )

   RETURN USRRDD_GETFUNCTABLE( pFuncCount, pFuncTable, pSuperTable, nRddID, ;
      cSuperRDD, aMethods, pSuperRddID )

INIT PROCEDURE RLCDX_INIT()

   rddRegister( "RLCDX", RDT_FULL )

   RETURN
