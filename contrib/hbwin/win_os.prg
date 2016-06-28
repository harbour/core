/*
 * Windows OS - safe LAN networking
 *
 * Copyright 2004 Peter Rees <peter@rees.co.nz> Rees Software and Systems Ltd
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
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

/* Function to check and set Windows Registry settings
 * for safe networking - for all versions of Windows.
 *
 * Also includes check for buggy VREDIR.VXD under Win95
 * and if the correct patch file is found.
 */

#include "directry.ch"
#include "hbwin.ch"

/* NOTE: To change any of these registry settings
         Administrator rights are required by default in Windows. [vszakats] */

FUNCTION win_osNetRegOk( lSetIt, lDoVista )

   LOCAL bRetVal := .T.
   LOCAL cKeySrv
   LOCAL cKeyWks

   hb_default( @lSetIt, .F. )

   IF ! hb_defaultValue( lDoVista, .T. ) .AND. hb_osIsWinVista()
      /* do nothing */
   ELSEIF hb_osIsWin9x()
      bRetVal := win_regQuery( WIN_HKEY_LOCAL_MACHINE, "System\CurrentControlSet\Services\VxD\VREDIR", "DiscardCacheOnOpen", 1, lSetIt )
   ELSE
      cKeySrv := "System\CurrentControlSet\Services\LanmanServer\Parameters"
      cKeyWks := "System\CurrentControlSet\Services\LanmanWorkStation\Parameters"

      IF lSetIt
         lSetIt := ! hb_osIsWinNT() .OR. wapi_IsUserAnAdmin()
      ENDIF

      /* Server settings */
      bRetVal := bRetVal .AND. win_regQuery( WIN_HKEY_LOCAL_MACHINE, cKeySrv, "CachedOpenLimit", 0, lSetIt )
      bRetVal := bRetVal .AND. win_regQuery( WIN_HKEY_LOCAL_MACHINE, cKeySrv, "EnableOpLocks", 0, lSetIt ) /* Q124916 */
      bRetVal := bRetVal .AND. win_regQuery( WIN_HKEY_LOCAL_MACHINE, cKeySrv, "EnableOpLockForceClose", 1, lSetIt )
      bRetVal := bRetVal .AND. win_regQuery( WIN_HKEY_LOCAL_MACHINE, cKeySrv, "SharingViolationDelay", 0, lSetIt )
      bRetVal := bRetVal .AND. win_regQuery( WIN_HKEY_LOCAL_MACHINE, cKeySrv, "SharingViolationRetries", 0, lSetIt )

      IF hb_osIsWinVista()
         /* If SMB2 is enabled turning off oplocks does not work, so SMB2 is required to be turned off on Server. */
         bRetVal := bRetVal .AND. win_regQuery( WIN_HKEY_LOCAL_MACHINE, cKeySrv, "SMB2", 0, lSetIt )
      ENDIF

      /* Workstation settings */
      bRetVal := bRetVal .AND. win_regQuery( WIN_HKEY_LOCAL_MACHINE, cKeyWks, "UseOpportunisticLocking", 0, lSetIt )
      bRetVal := bRetVal .AND. win_regQuery( WIN_HKEY_LOCAL_MACHINE, cKeyWks, "EnableOpLocks", 0, lSetIt )
      bRetVal := bRetVal .AND. win_regQuery( WIN_HKEY_LOCAL_MACHINE, cKeyWks, "EnableOpLockForceClose", 1, lSetIt )
      bRetVal := bRetVal .AND. win_regQuery( WIN_HKEY_LOCAL_MACHINE, cKeyWks, "UtilizeNtCaching", 0, lSetIt )
      bRetVal := bRetVal .AND. win_regQuery( WIN_HKEY_LOCAL_MACHINE, cKeyWks, "UseLockReadUnlock", 0, lSetIt )

      IF hb_osIsWinVista()
         bRetVal := bRetVal .AND. win_regQuery( WIN_HKEY_LOCAL_MACHINE, cKeyWks, "FileInfoCacheLifetime", 0, lSetIt )
         bRetVal := bRetVal .AND. win_regQuery( WIN_HKEY_LOCAL_MACHINE, cKeyWks, "FileNotFoundCacheLifetime", 0, lSetIt )
         bRetVal := bRetVal .AND. win_regQuery( WIN_HKEY_LOCAL_MACHINE, cKeyWks, "DirectoryCacheLifetime", 0, lSetIt )
      ENDIF

      IF hb_osIsWin2K()
         bRetVal := bRetVal .AND. win_regQuery( WIN_HKEY_LOCAL_MACHINE, "System\CurrentControlSet\Services\MRXSmb\Parameters", "OpLocksDisabled", 1, lSetIt )
      ENDIF
   ENDIF

   RETURN bRetVal

FUNCTION win_osNetVRedirOk( /* @ */ nResult )

   LOCAL aFiles

   LOCAL nSize
   LOCAL cTime

   nResult := 0

   /* Check for faulty files */
   IF hb_osIsWin9x() .AND. ;
      Len( aFiles := hb_vfDirectory( hb_GetEnv( "WINDIR", "C:\WINDOWS" ) + "\SYSTEM\VREDIR.VXD" ) ) >= 1

      nSize := aFiles[ 1 ][ F_SIZE ]
      cTime := aFiles[ 1 ][ F_TIME ]

      DO CASE
      CASE nSize == 156749 .AND. cTime == "11:11:10"
         nResult := 1111
      CASE nSize == 140343 .AND. cTime == "09:50:00"
         nResult := 950
      ENDCASE
   ENDIF

   RETURN nResult == 0
