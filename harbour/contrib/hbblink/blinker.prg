/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Blinker compatibility functions.
 *
 * Copyright 2010 Viktor Szakats (harbour syenar.net)
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

#include "hbhrb.ch"
#include "hbmemory.ch"

#include "hbblink.ch"

STATIC s_cSerialNum := ""
STATIC s_cDemoDate := ""
STATIC s_nDemoMinutes := 0
STATIC s_nError := 0
STATIC s_cErrorParam := ""
STATIC s_lLibOverrides := .F.

THREAD STATIC t_nErrorLevel := 0
THREAD STATIC t_nErrorMaj := 0
THREAD STATIC t_nErrorMin := 0

PROCEDURE hb_BliVerNum( cString )

   IF HB_ISSTRING( cString )
      s_cSerialNum := cString
   ENDIF

   RETURN

PROCEDURE hb_BliDemDte( dDate )

   IF HB_ISDATE( dDate )
      s_cDemoDate := DToS( dDate )
   ENDIF

   RETURN

PROCEDURE hb_BliDemMin( nValue )

   IF HB_ISNUMERIC( nValue )
      s_nDemoMinutes := nValue
   ENDIF

   RETURN

FUNCTION BliDbgHan( nValue )

   STATIC s_nDebugHandle := 1
   LOCAL nOldValue := s_nDebugHandle

   IF HB_ISNUMERIC( nValue )
      s_nDebugHandle := nValue
   ENDIF

   RETURN nOldValue

FUNCTION BliDemDte()
   RETURN s_cDemoDate

FUNCTION BliDemDteBas()
   RETURN s_cDemoDate

FUNCTION BliDemMin( nValue )

   LOCAL nOldValue := s_nDemoMinutes

   IF HB_ISNUMERIC( nValue )
      s_nDemoMinutes := nValue
   ENDIF

   RETURN nOldValue

PROCEDURE BliDisFrg( fhnd )

   HB_SYMBOL_UNUSED( fhnd )

   RETURN

FUNCTION BliErrNum()
   RETURN s_nError

FUNCTION BliErrPrm()
   RETURN s_cErrorParam

FUNCTION BliErrPrmBas()
   RETURN s_cErrorParam

/* INCOMPATIBLE: Will return pointer instead of numeric. */
FUNCTION BliLibLib( cLib )
   RETURN iif( s_lLibOverrides, hb_hrbLoad( HB_HRB_BIND_OVERLOAD, cLib ), hb_hrbLoad( cLib ) )

/* INCOMPATIBLE: Will return array instead of numeric. */
FUNCTION BliFunHan( pLib, cFunction )
   RETURN hb_hrbGetFunSym( pLib, cFunction )

FUNCTION BliFunCal( ... )

   LOCAL aParams := hb_AParams()
   LOCAL pFunction

   IF Len( aParams ) > 0
      pFunction := ATail( aParams )
      ASize( aParams, Len( aParams ) - 1 )
      RETURN hb_hrbDo( pFunction, hb_ArrayToParams( aParams ) )
   ENDIF

   RETURN NIL

PROCEDURE BliLibFre( pLib )

   hb_hrbUnload( pLib )

   RETURN

PROCEDURE BliLibOvr( lValue )

   IF HB_ISLOGICAL( lValue )
      s_lLibOverrides := lValue
   ENDIF

   RETURN

PROCEDURE BliLstFrg( fhnd )

   HB_SYMBOL_UNUSED( fhnd )

   RETURN

FUNCTION BliMemAvl()
   RETURN Memory( HB_MEM_CHAR )

FUNCTION BliMemSiz()
   RETURN Memory( HB_MEM_CHAR )

FUNCTION BliMemBlk( nBlockSize )

   HB_SYMBOL_UNUSED( nBlockSize )

   RETURN Memory( HB_MEM_CHAR )

FUNCTION BliMemPak( nValue )

   STATIC s_nGCFrequency := 0
   LOCAL nOldValue := s_nDemoMinutes

   IF HB_ISNUMERIC( nValue )
      s_nGCFrequency := nValue
   ENDIF

   RETURN nOldValue

FUNCTION BliMemUse()
   RETURN Memory( HB_MEM_USED )

PROCEDURE BliOvlClr()
   RETURN

FUNCTION BliOvlOps()
   RETURN Memory( HB_MEM_CHAR )

PROCEDURE BliOvlRes()
   RETURN

FUNCTION BliOvlSiz()
   RETURN 0

PROCEDURE BliOvlSus( nValue )

   HB_SYMBOL_UNUSED( nValue )

   RETURN

FUNCTION BliPtrDec( nPointer, nValue )
   RETURN nPointer - nValue

FUNCTION BliPtrInc( nPointer, nValue )
   RETURN nPointer + nValue

FUNCTION BliSerNum()
   RETURN s_cSerialNum

FUNCTION BliSerNumBas()
   RETURN s_cSerialNum

FUNCTION BliStrFrg( cString )

   HB_SYMBOL_UNUSED( cString )

   RETURN ""

FUNCTION BliVerNum()
   RETURN 700

FUNCTION BliCpuRel()
   RETURN hb_ReleaseCPU()

FUNCTION BliMgrSts( nParam )

   SWITCH nParam
   CASE BliCacheLoc     ; RETURN BliCacheNone
   CASE BliCacheSize    ; RETURN 0
   CASE BliExtMemAvail  ; RETURN 0
   CASE BliHostMode     ; RETURN BliHostNone
   CASE BliMachineMode  ; RETURN BliMode286Prot
   CASE BliOverlayLoc   ; RETURN 0
   CASE BliOverlaySize  ; RETURN Memory( HB_MEM_CHAR )
   CASE BliRealMemAvail ; RETURN Memory( HB_MEM_CHAR )
   CASE BliVirMemAvail  ; RETURN 0
   ENDSWITCH

   RETURN 0

FUNCTION SwpAddEnv( nBytes )

   HB_SYMBOL_UNUSED( nBytes )

   RETURN 32768

FUNCTION SwpAddStr( cPID, cString )

   HB_SYMBOL_UNUSED( cPID )
   HB_SYMBOL_UNUSED( cString )

   RETURN 1

FUNCTION SwpAddStrBas( cPID, cString )

   HB_SYMBOL_UNUSED( cPID )
   HB_SYMBOL_UNUSED( cString )

   RETURN 1

FUNCTION SwpCurDir( lValue )

   HB_SYMBOL_UNUSED( lValue )

   RETURN .T.

FUNCTION SwpDisMsg( lValue )

   HB_SYMBOL_UNUSED( lValue )

   RETURN .F.

FUNCTION SwpEms320( lValue )

   HB_SYMBOL_UNUSED( lValue )

   RETURN .F.

FUNCTION SwpErrLev()
   RETURN t_nErrorLevel

FUNCTION SwpErrMaj()
   RETURN t_nErrorMaj

FUNCTION SwpErrMin()
   RETURN t_nErrorMin

FUNCTION SwpFreEms( nLimitKB )

   HB_SYMBOL_UNUSED( nLimitKB )

   RETURN 0

FUNCTION SwpUseEms( lValue )

   HB_SYMBOL_UNUSED( lValue )

   RETURN .F.

FUNCTION SwpFreXms( nLimitKB )

   HB_SYMBOL_UNUSED( nLimitKB )

   RETURN 0

FUNCTION SwpUseXms( lValue )

   HB_SYMBOL_UNUSED( lValue )

   RETURN .F.

FUNCTION SwpUseUmb( lValue )

   HB_SYMBOL_UNUSED( lValue )

   RETURN .F.

FUNCTION SwpGetKey( lValue )

   HB_SYMBOL_UNUSED( lValue )

   RETURN .F.

FUNCTION SwpGetPid( cIDString )

   HB_SYMBOL_UNUSED( cIDString )

   RETURN .F.

FUNCTION SwpVidMde( lValue )

   HB_SYMBOL_UNUSED( lValue )

   RETURN .F.

FUNCTION SwpGetStr()
   RETURN ""

FUNCTION SwpKeyBrd( cKeyString )

   HB_SYMBOL_UNUSED( cKeyString )

   RETURN 0

FUNCTION SwpKeyBrdBas( cKeyString )

   HB_SYMBOL_UNUSED( cKeyString )

   RETURN 0

FUNCTION SwpKeyClr( nValue )

   HB_SYMBOL_UNUSED( nValue )

   RETURN 0

FUNCTION SwpNobOot( lValue )

   HB_SYMBOL_UNUSED( lValue )

   RETURN .F.

FUNCTION SwpRunCmd( cCommand, nMem, cRunPath, cTempPath )

   LOCAL cShell

   HB_SYMBOL_UNUSED( nMem )
   HB_SYMBOL_UNUSED( cRunPath )
   HB_SYMBOL_UNUSED( cTempPath )

#if defined( __PLATFORM__UNIX )
   cShell := hb_GetEnv( "SHELL" )
#else
   cShell := hb_GetEnv( "COMSPEC" )
#endif

   IF ! Empty( cShell )
#if defined( __PLATFORM__UNIX )
      cCommand := cShell + " " + cCommand
#else
      cCommand := cShell + " /c " + cCommand
#endif
   ENDIF

   t_nErrorLevel := hb_run( cCommand )

   RETURN .T.

FUNCTION SwpSetEnv( cString )

   LOCAL cPair
   LOCAL tmp

   FOR EACH cPair IN hb_ATokens( cString, hb_BChar( 255 ) )
      IF ! Empty( cPair )
         tmp := At( "=", cPair )
         IF tmp > 0
            hb_SetEnv( Left( cPair, tmp - 1 ), SubStr( cPair, tmp + 1 ) )
         ENDIF
      ENDIF
   NEXT

   RETURN .T.

FUNCTION SwpSetPid( cIDString )

   HB_SYMBOL_UNUSED( cIDString )

   RETURN .T.

FUNCTION SwpSetPidBas( cIDString )

   HB_SYMBOL_UNUSED( cIDString )

   RETURN .T.

FUNCTION SwpSetStr( cString )

   HB_SYMBOL_UNUSED( cString )

   RETURN .T.

FUNCTION SwpSetStrBas( cString )

   HB_SYMBOL_UNUSED( cString )

   RETURN .T.
