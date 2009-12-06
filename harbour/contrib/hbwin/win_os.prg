/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 *     Copyright 2004 Peter Rees <peter@rees.co.nz>
 *                    Rees Software & Systems Ltd
 *
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option )
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.   If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/ ).
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
 * not apply to the code that you add in this way.   To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.

*/

/*
 * Operating system functions for Windows
 *
 * Program to check and set Windows Registry settings
 * for safe networking - all versions of Windows to XP SP2
 *
 * Also includes check for buggy VREDIR.VXD under Win95
 * and if the correct patch file is found - run it.
 */

#include "common.ch"
#include "directry.ch"
#include "hbwin.ch"

/* NOTE: To change any of these registry settings
         Administrator rights are required by default in Windows. [vszakats] */

FUNCTION WIN_OSNETREGOK( lSetIt, lDoVista )
   LOCAL bRetVal := .T.
   LOCAL cKeySrv
   LOCAL cKeyWks

   DEFAULT lSetIt TO .F.
   DEFAULT lDoVista TO .T.

   IF ! lDoVista .AND. win_osIsVistaOrUpper()
      /* do nothing */
   ELSEIF win_osIs9X()
      bRetVal := win_regQuery( HKEY_LOCAL_MACHINE, "System\CurrentControlSet\Services\VxD\VREDIR", "DiscardCacheOnOpen", 1, lSetIt )
   ELSE
      cKeySrv := "System\CurrentControlSet\Services\LanmanServer\Parameters"
      cKeyWks := "System\CurrentControlSet\Services\LanmanWorkStation\Parameters"

      IF lSetIt
         lSetIt := ! win_osIsNT() .OR. wapi_IsUserAnAdmin()
      ENDIF

      /* Server settings */
      bRetVal := bRetVal .AND. win_regQuery( HKEY_LOCAL_MACHINE, cKeySrv, "CachedOpenLimit", 0, lSetIt )
      bRetVal := bRetVal .AND. win_regQuery( HKEY_LOCAL_MACHINE, cKeySrv, "EnableOpLocks", 0, lSetIt ) /* Q124916 */
      bRetVal := bRetVal .AND. win_regQuery( HKEY_LOCAL_MACHINE, cKeySrv, "EnableOpLockForceClose", 1, lSetIt )
      bRetVal := bRetVal .AND. win_regQuery( HKEY_LOCAL_MACHINE, cKeySrv, "SharingViolationDelay", 0, lSetIt )
      bRetVal := bRetVal .AND. win_regQuery( HKEY_LOCAL_MACHINE, cKeySrv, "SharingViolationRetries", 0, lSetIt )

      IF win_osIsVistaOrUpper()
         /* If SMB2 is enabled turning off oplocks does not work, so SMB2 is required to be turned off on Server. */
         bRetVal := bRetVal .AND. win_regQuery( HKEY_LOCAL_MACHINE, cKeySrv, "SMB2", 0, lSetIt )
      ENDIF

      /* Workstation settings */
      bRetVal := bRetVal .AND. win_regQuery( HKEY_LOCAL_MACHINE, cKeyWks, "UseOpportunisticLocking", 0, lSetIt )
      bRetVal := bRetVal .AND. win_regQuery( HKEY_LOCAL_MACHINE, cKeyWks, "EnableOpLocks", 0, lSetIt )
      bRetVal := bRetVal .AND. win_regQuery( HKEY_LOCAL_MACHINE, cKeyWks, "EnableOpLockForceClose", 1, lSetIt )
      bRetVal := bRetVal .AND. win_regQuery( HKEY_LOCAL_MACHINE, cKeyWks, "UtilizeNtCaching", 0, lSetIt )
      bRetVal := bRetVal .AND. win_regQuery( HKEY_LOCAL_MACHINE, cKeyWks, "UseLockReadUnlock", 0, lSetIt )

      IF win_osis2000OrUpper()
         bRetVal := bRetVal .AND. win_regQuery( HKEY_LOCAL_MACHINE, "System\CurrentControlSet\Services\MRXSmb\Parameters", "OpLocksDisabled", 1, lSetIt )
      ENDIF
   ENDIF

   RETURN bRetVal

FUNCTION WIN_OSNETVREDIROK( /* @ */ nResult )
   LOCAL a

   nResult := 0

   IF win_osIs9X()
      a := Directory( hb_GetEnv( "WINDIR", "C:\WINDOWS" ) + "\SYSTEM\VREDIR.VXD" )  /* Check for faulty files. */
      IF ! Empty( a )
         IF a[ 1, F_SIZE ] == 156749 .AND. a[ 1, F_TIME ] == "11:11:10"
            nResult := 1111
         ELSEIF a[ 1, F_SIZE ] == 140343 .AND. a[ 1, F_TIME ] == "09:50:00"
            nResult := 950
         ENDIF
      ENDIF
   ENDIF

   RETURN Empty( nResult )
