/*
 * xHarbour Project source code:
 * PRG Tracing System
 *
 * Copyright 2001 Ron Pinkas <ron@@ronpinkas.com>
 * www - http://www.xharbour.org
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
      DO CASE
      CASE Upper( xTrace ) == "ON"
         s_lSET_TRACE := .T.
      CASE Upper( xTrace ) == "OFF"
         s_lSET_TRACE := .F.
      ENDCASE
   ENDCASE

   RETURN lTrace

FUNCTION xhb_SetTraceFile( xFile, lAppend )

   LOCAL cTraceFile := s_cSET_TRACEFILE

   IF HB_ISSTRING( xFile )
      s_cSET_TRACEFILE := xFile
      IF ! HB_ISLOGICAL( lAppend ) .OR. ! lAppend
         FClose( FCreate( s_cSET_TRACEFILE ) )
      ENDIF
   ENDIF

   RETURN cTraceFile

FUNCTION xhb_SetTraceStack( xLevel )

   LOCAL nTraceLevel := s_nSET_TRACESTACK

   DO CASE
   CASE HB_ISSTRING( xLevel )
      DO CASE
      CASE Upper( xLevel ) == "NONE"
         s_nSET_TRACESTACK := HB_SET_TRACESTACK_NONE
      CASE Upper( xLevel ) == "CURRENT"
         s_nSET_TRACESTACK := HB_SET_TRACESTACK_CURRENT
      CASE Upper( xLevel ) == "ALL"
         s_nSET_TRACESTACK := HB_SET_TRACESTACK_ALL
      ENDCASE
   CASE HB_ISNUMERIC( xLevel )
      IF xLevel >= 0
         s_nSET_TRACESTACK := xLevel
      ENDIF
   ENDCASE

   RETURN nTraceLevel

// -------------------------------------------------------------- //

FUNCTION TraceLog( ... )

   // Using PRIVATE instead of LOCALs so TraceLog() is DIVERT friendly.
   LOCAL cFile, FileHandle, nLevel, ProcName, xParam

   IF ! s_lSET_TRACE
      RETURN .T.
   ENDIF

   cFile := s_cSET_TRACEFILE
   nLevel := s_nSET_TRACESTACK

   /* hb_FileExists() and FOpen()/FCreate() make different assumptions rgdg path,
      so we have to make sure cFile contains path to avoid ambiguity */
   cFile := cWithPath( cFile )

   IF hb_FileExists( cFile )
      FileHandle := FOpen( cFile, FO_WRITE )
   ELSE
      FileHandle := FCreate( cFile )
   ENDIF

   IF FileHandle != F_ERROR

      FSeek( FileHandle, 0, FS_END )

      IF nLevel > 0
         FWrite( FileHandle, "[" + ProcFile( 1 ) + "->" + ProcName( 1 ) + "] (" + hb_ntos( ProcLine( 1 ) ) + ")" )
      ENDIF

      IF nLevel > 1 .AND. !( ProcName( 2 ) == "" )
         FWrite( FileHandle, " Called from:" + hb_eol() )
         nLevel := 1
         DO WHILE !( ( ProcName := ProcName( ++nLevel ) ) == "" )
            FWrite( FileHandle, Space( 30 ) + ProcFile( nLevel ) + "->" + ProcName + "(" + hb_ntos( ProcLine( nLevel ) ) + ")" + hb_eol() )
         ENDDO
      ELSE
         FWrite( FileHandle, hb_eol() )
      ENDIF

      FOR EACH xParam IN hb_AParams()
         FWrite( FileHandle, "Type: " + ValType( xParam ) + " >>>" + hb_CStr( xParam ) + "<<<" + hb_eol() )
      NEXT

      FWrite( FileHandle, hb_eol() )

      FClose( FileHandle )
   ENDIF

   RETURN .T.

/* Ensure cFilename contains path. If it doesn't, add current directory to the front of it */
STATIC FUNCTION cWithPath( cFilename )

   LOCAL cPath := hb_FNameDir( cFilename )

   RETURN iif( Empty( cPath ), "." + hb_ps(), "" ) + cFilename
