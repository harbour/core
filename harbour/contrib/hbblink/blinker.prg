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

#include "common.ch"

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

PROCEDURE HB_BLIVERNUM( cString )
   IF ISCHARACTER( cString )
      s_cSerialNum := cString
   ENDIF
   RETURN

PROCEDURE HB_BLIDEMDTE( dDate )
   IF ISDATE( dDate )
      s_cDemoDate := DToS( dDate )
   ENDIF
   RETURN

PROCEDURE HB_BLIDEMMIN( nValue )
   IF ISNUMBER( nValue )
      s_nDemoMinutes := nValue
   ENDIF
   RETURN

FUNCTION BLIDBGHAN( nValue )
   STATIC s_nDebugHandle := 1
   LOCAL nOldValue := s_nDebugHandle
   IF ISNUMBER( nValue )
      s_nDebugHandle := nValue
   ENDIF
   RETURN nOldValue

FUNCTION BLIDEMDTE()
   RETURN s_cDemoDate

FUNCTION BLIDEMDTEBAS()
   RETURN s_cDemoDate

FUNCTION BLIDEMMIN( nValue )
   LOCAL nOldValue := s_nDemoMinutes
   IF ISNUMBER( nValue )
      s_nDemoMinutes := nValue
   ENDIF
   RETURN nOldValue

PROCEDURE BLIDISFRG( fhnd )
   HB_SYMBOL_UNUSED( fhnd )
   RETURN

FUNCTION BLIERRNUM()
   RETURN s_nError

FUNCTION BLIERRPRM()
   RETURN s_cErrorParam

FUNCTION BLIERRPRMBAS()
   RETURN s_cErrorParam

/* INCOMPATIBLE: Will return pointer instead of numeric. */
FUNCTION BLILIBLIB( cLib )
   RETURN iif( s_lLibOverrides, hb_hrbLoad( HB_HRB_BIND_OVERLOAD, cLib ), hb_hrbLoad( cLib ) )

/* INCOMPATIBLE: Will return array instead of numeric. */
FUNCTION BLIFUNHAN( pLib, cFunction )
   RETURN hb_hrbGetFunSym( pLib, cFunction )

FUNCTION BLIFUNCAL( ... )
   LOCAL aParams := hb_AParams()
   LOCAL pFunction
   IF Len( aParams ) > 0
      pFunction := ATail( aParams )
      ASize( aParams, Len( aParams ) - 1 )
      RETURN hb_hrbDo( pFunction, hb_arrayToParams( aParams ) )
   ENDIF
   RETURN NIL

PROCEDURE BLILIBFRE( pLib )
   hb_hrbUnLoad( pLib )
   RETURN

PROCEDURE BLILIBOVR( lValue )
   IF ISLOGICAL( lValue )
      s_lLibOverrides := lValue
   ENDIF
   RETURN

PROCEDURE BLILSTFRG( fhnd )
   HB_SYMBOL_UNUSED( fhnd )
   RETURN

FUNCTION BLIMEMAVL()
   RETURN Memory( HB_MEM_CHAR )

FUNCTION BLIMEMSIZ()
   RETURN Memory( HB_MEM_CHAR )

FUNCTION BLIMEMBLK( nBlockSize )
   HB_SYMBOL_UNUSED( nBlockSize )
   RETURN Memory( HB_MEM_CHAR )

FUNCTION BLIMEMPAK( nValue )
   STATIC s_nGCFrequency := 0
   LOCAL nOldValue := s_nDemoMinutes
   IF ISNUMBER( nValue )
      s_nGCFrequency := nValue
   ENDIF
   RETURN nOldValue

FUNCTION BLIMEMUSE()
   RETURN Memory( HB_MEM_USED )

PROCEDURE BLIOVLCLR()
   RETURN

FUNCTION BLIOVLOPS()
   RETURN Memory( HB_MEM_CHAR )

PROCEDURE BLIOVLRES()
   RETURN

FUNCTION BLIOVLSIZ()
   RETURN 0

PROCEDURE BLIOVLSUS( nValue )
   HB_SYMBOL_UNUSED( nValue )
   RETURN

FUNCTION BLIPTRDEC( nPointer, nValue )
   RETURN nPointer - nValue

FUNCTION BLIPTRINC( nPointer, nValue )
   RETURN nPointer + nValue

FUNCTION BLISERNUM()
   RETURN s_cSerialNum

FUNCTION BLISERNUMBAS()
   RETURN s_cSerialNum

FUNCTION BLISTRFRG( cString )
   HB_SYMBOL_UNUSED( cString )
   RETURN ""

FUNCTION BLIVERNUM()
   RETURN 700

FUNCTION BLICPUREL()
   RETURN hb_releaseCPU()

FUNCTION BLIMGRSTS( nParam )
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

FUNCTION SWPADDENV( nBytes )
   HB_SYMBOL_UNUSED( nBytes )
   RETURN 32768

FUNCTION SWPADDSTR( cPID, cString )
   HB_SYMBOL_UNUSED( cPID )
   HB_SYMBOL_UNUSED( cString )
   RETURN 1

FUNCTION SWPADDSTRBAS( cPID, cString )
   HB_SYMBOL_UNUSED( cPID )
   HB_SYMBOL_UNUSED( cString )
   RETURN 1

FUNCTION SWPCURDIR( lValue )
   HB_SYMBOL_UNUSED( lValue )
   RETURN .T.

FUNCTION SWPDISMSG( lValue )
   HB_SYMBOL_UNUSED( lValue )
   RETURN .F.

FUNCTION SWPEMS320( lValue )
   HB_SYMBOL_UNUSED( lValue )
   RETURN .F.

FUNCTION SWPERRLEV()
   RETURN t_nErrorLevel

FUNCTION SWPERRMAJ()
   RETURN t_nErrorMaj

FUNCTION SWPERRMIN()
   RETURN t_nErrorMin

FUNCTION SWPFREEMS( nLimitKB )
   HB_SYMBOL_UNUSED( nLimitKB )
   RETURN 0

FUNCTION SWPUSEEMS( lValue )
   HB_SYMBOL_UNUSED( lValue )
   RETURN .F.

FUNCTION SWPFREXMS( nLimitKB )
   HB_SYMBOL_UNUSED( nLimitKB )
   RETURN 0

FUNCTION SWPUSEXMS( lValue )
   HB_SYMBOL_UNUSED( lValue )
   RETURN .F.

FUNCTION SWPUSEUMB( lValue )
   HB_SYMBOL_UNUSED( lValue )
   RETURN .F.

FUNCTION SWPGETKEY( lValue )
   HB_SYMBOL_UNUSED( lValue )
   RETURN .F.

FUNCTION SWPGETPID( cIDString )
   HB_SYMBOL_UNUSED( cIDString )
   RETURN .F.

FUNCTION SWPVIDMDE( lValue )
   HB_SYMBOL_UNUSED( lValue )
   RETURN .F.

FUNCTION SWPGETSTR()
   RETURN ""

FUNCTION SWPKEYBRD( cKeyString )
   HB_SYMBOL_UNUSED( cKeyString )
   RETURN 0

FUNCTION SWPKEYBRDBAS( cKeyString )
   HB_SYMBOL_UNUSED( cKeyString )
   RETURN 0

FUNCTION SWPKEYCLR( nValue )
   HB_SYMBOL_UNUSED( nValue )
   RETURN 0

FUNCTION SWPNOBOOT( lValue )
   HB_SYMBOL_UNUSED( lValue )
   RETURN .F.

FUNCTION SWPRUNCMD( cCommand, nMem, cRunPath, cTempPath )
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
      #if defined( __PLATFORM__WINDOWS ) .OR. defined( __PLATFORM__DOS )
         cCommand := cShell + " /c " + cCommand
      #else
         cCommand := cShell + " " + cCommand
      #endif
   ENDIF

   t_nErrorLevel := hb_run( cCommand )

   RETURN .T.

FUNCTION SWPSETENV( cString )
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

FUNCTION SWPSETPID( cIDString )
   HB_SYMBOL_UNUSED( cIDString )
   RETURN .T.

FUNCTION SWPSETPIDBAS( cIDString )
   HB_SYMBOL_UNUSED( cIDString )
   RETURN .T.

FUNCTION SWPSETSTR( cString )
   HB_SYMBOL_UNUSED( cString )
   RETURN .T.

FUNCTION SWPSETSTRBAS( cString )
   HB_SYMBOL_UNUSED( cString )
   RETURN .T.
