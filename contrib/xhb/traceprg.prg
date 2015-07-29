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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#define HB_SET_TRACESTACK_NONE     0
#define HB_SET_TRACESTACK_CURRENT  1
#define HB_SET_TRACESTACK_ALL      2

STATIC s_lSET_TRACE      := .T.
STATIC s_cSET_TRACEFILE  := "trace.log"
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

   IF HB_ISSTRING( xFile )
      s_cSET_TRACEFILE := xFile
      IF ! hb_defaultValue( lAppend, .F. ) .AND. ;
         ( hFile := hb_vfOpen( s_cSET_TRACEFILE, FO_CREAT + FO_TRUNC + FO_WRITE ) ) != NIL
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
   LOCAL hFile, nLevel, ProcName, xParam

   IF s_lSET_TRACE .AND. ;
      ( hFile := hb_vfOpen( s_cSET_TRACEFILE, FO_CREAT + FO_WRITE ) ) != NIL

      hb_vfSeek( hFile, 0, FS_END )

      nLevel := s_nSET_TRACESTACK

      IF nLevel > 0
         hb_vfWrite( hFile, "[" + ProcFile( 1 ) + "->" + ProcName( 1 ) + "] (" + hb_ntos( ProcLine( 1 ) ) + ")" )
      ENDIF

      IF nLevel > 1 .AND. !( ProcName( 2 ) == "" )
         hb_vfWrite( hFile, " Called from:" + hb_eol() )
         nLevel := 1
         DO WHILE !( ( ProcName := ProcName( ++nLevel ) ) == "" )
            hb_vfWrite( hFile, Space( 30 ) + ProcFile( nLevel ) + "->" + ProcName + "(" + hb_ntos( ProcLine( nLevel ) ) + ")" + hb_eol() )
         ENDDO
      ELSE
         hb_vfWrite( hFile, hb_eol() )
      ENDIF

      FOR EACH xParam IN hb_AParams()
         hb_vfWrite( hFile, "Type: " + ValType( xParam ) + " >>>" + hb_CStr( xParam ) + "<<<" + hb_eol() )
      NEXT

      hb_vfWrite( hFile, hb_eol() )
      hb_vfClose( hFile )
   ENDIF

   RETURN .T.
