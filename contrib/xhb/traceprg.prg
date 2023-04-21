/*
 * PRG Tracing System
 *
 * Copyright 2001 Ron Pinkas <ron@@ronpinkas.com>
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
 * As a special exception, xHarbour license gives permission for
 * additional uses of the text contained in its release of xHarbour.
 *
 * The exception is that, if you link the xHarbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the xHarbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released with this xHarbour
 * explicit exception.  If you add/copy code from other sources,
 * as the General Public License permits, the above exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for xHarbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "fileio.ch"

#define HB_SET_TRACESTACK_NONE    0
#define HB_SET_TRACESTACK_CURRENT 1
#define HB_SET_TRACESTACK_ALL     2

STATIC s_lSET_TRACE      := .T.
STATIC s_cSET_TRACEFILE  := "trace.log"
STATIC s_cSET_TRACEFILER := nil
STATIC s_nSET_TRACESTACK := HB_SET_TRACESTACK_ALL

FUNCTION xhb_SetTrace( xTrace )

   LOCAL lTrace := s_lSET_TRACE

   DO CASE
   CASE HB_ISLOGICAL( xTrace )
      s_lSET_TRACE := xTrace
   CASE HB_ISSTRING( xTrace )
      SWITCH Upper( xTrace )
      CASE "ON"
         s_lSET_TRACE := .T.
         EXIT
      CASE "OFF"
         s_lSET_TRACE := .F.
         EXIT
      ENDSWITCH
   ENDCASE

   RETURN lTrace

FUNCTION xhb_SetTraceFile( xFile, lAppend )

   LOCAL cTraceFile := s_cSET_TRACEFILE
   LOCAL hFile

   IF HB_ISSTRING( xFile ) .AND. ! Empty( xFile )
      s_cSET_TRACEFILER := s_cSET_TRACEFILE := s_RealPath( xFile )
      IF ! hb_defaultValue( lAppend, .F. ) .AND. ;
         ( hFile := hb_vfOpen( @s_cSET_TRACEFILER, FO_CREAT + FO_TRUNC + FO_WRITE ) ) != NIL
         hb_vfClose( hFile )
      ENDIF
   ENDIF

   RETURN cTraceFile

FUNCTION xhb_SetTraceStack( xLevel )

   LOCAL nTraceLevel := s_nSET_TRACESTACK

   DO CASE
   CASE HB_ISSTRING( xLevel )
      SWITCH xLevel
      CASE "NONE"
         s_nSET_TRACESTACK := HB_SET_TRACESTACK_NONE
         EXIT
      CASE "CURRENT"
         s_nSET_TRACESTACK := HB_SET_TRACESTACK_CURRENT
         EXIT
      CASE "ALL"
         s_nSET_TRACESTACK := HB_SET_TRACESTACK_ALL
         EXIT
      ENDSWITCH
   CASE HB_ISNUMERIC( xLevel )
      IF xLevel >= 0
         s_nSET_TRACESTACK := xLevel
      ENDIF
   ENDCASE

   RETURN nTraceLevel

/* --- */

FUNCTION TraceLog( ... )

   // Using PRIVATE instead of LOCALs so TraceLog() is DIVERT friendly.
   LOCAL hFile, nLevel, ProcName, xParam, cData

   IF s_lSET_TRACE

      cData := ""

      nLevel := s_nSET_TRACESTACK

      IF nLevel > 0
         cData += "[" + ProcFile( 1 ) + "->" + ProcName( 1 ) + "] (" + hb_ntos( ProcLine( 1 ) ) + ")"
      ENDIF

      IF nLevel > 1 .AND. ! ProcName( 2 ) == ""
         cData += " Called from:" + hb_eol()
         nLevel := 1
         DO WHILE ! ( ProcName := ProcName( ++nLevel ) ) == ""
            cData += Space( 30 ) + ProcFile( nLevel ) + "->" + ProcName + "(" + hb_ntos( ProcLine( nLevel ) ) + ")" + hb_eol()
         ENDDO
      ELSE
         cData += hb_eol()
      ENDIF

      FOR EACH xParam IN hb_AParams()
         cData += "Type: " + ValType( xParam ) + " >>>" + hb_CStr( xParam ) + "<<<" + hb_eol()
      NEXT

      cData += hb_eol()

      IF s_cSET_TRACEFILER == nil
         s_cSET_TRACEFILER := s_cSET_TRACEFILE := s_RealPath( s_cSET_TRACEFILE )
      ENDIF
      IF ( hFile := hb_vfOpen( @s_cSET_TRACEFILER, FO_CREAT + FO_WRITE ) ) != NIL
         hb_vfSeek( hFile, 0, FS_END )
         hb_vfWrite( hFile, cData )
         hb_vfClose( hFile )
      ENDIF
   ENDIF

   RETURN .T.

STATIC FUNCTION s_RealPath( cFilename )

   LOCAL cPath, cName, cExt, cDrv, cDir, nStart

   IF hb_vfIsLocal( cFilename )
      hb_FNameSplit( cFilename, @cPath, @cName, @cExt, @cDrv )
      IF Empty( hb_osDriveSeparator() )
         cDrv := ""
      ENDIF
      nStart := iif( Empty( cDrv ), 1, 3 )
      IF ! SubStr( cPath, nStart, 1 ) $ hb_osPathDelimiters()
         IF Empty( cDrv ) .OR. cDrv == DiskName()
            cPath := hb_cwd() + SubStr( cPath, nStart )
         ELSE
            #ifdef __PLATFORM__WINDOWS
               /* WIN API used by Harbour binds current directory with
                  process not with drive letters, due to side effects in
                  current Harbour code for MS-Windows using CurDir( cDrv )
                  changes current directory to root path [druzus] */
               cDir := ""
            #else
               IF ! Empty( cDir := CurDir( cDrv ) )
                  cDir += hb_ps()
               ENDIF
            #endif
            cPath := cDrv + hb_osDriveSeparator() + hb_ps() + cDir + ;
                     SubStr( cPath, nStart )
         ENDIF
         cFilename := hb_FNameMerge( cPath, cName, cExt )
      ELSEIF Empty( cDrv ) .AND. ! Empty( cDrv := DiskName() ) .AND. ;
             ! SubStr( cPath, 2, 1 ) $ hb_osPathDelimiters()
         cPath := cDrv + hb_osDriveSeparator() + cPath
         cFilename := hb_FNameMerge( cPath, cName, cExt )
      ENDIF
   ENDIF

   RETURN cFilename
