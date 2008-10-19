
/*
 * $Id: gd.prg 7887 2007-10-30 18:25:37Z lf_sfnet $
 */

/*
 * Harbour Project source code:
 * Debug Functions
 *
 * Copyright 2007 Francesco Saverio Giudice <info / at /fsgiudice.com>
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

#include "common.ch"
#include "fileio.ch"

REQUEST HB_SPRINTF

STATIC s_lToOutDebug   := TRUE
STATIC s_lToLogFile    := TRUE
STATIC s_lEmptyLogFile := TRUE

FUNCTION HB_ToOutDebugOnOff( lOnOff )
  LOCAL lOld := s_lToOutDebug
  IF HB_ISLOGICAL( lOnOff )
     s_lToOutDebug := lOnOff
  ENDIF
RETURN lOld

PROCEDURE HB_ToOutDebug( ... )
  //LOCAL cString
  IF !s_lToOutDebug
     RETURN
  ENDIF

  //cString := HB_ExecFromArray( "HB_SPRINTF", hb_aParams() )
  hb_OutDebug( hb_sprintf( ... ) )
RETURN

FUNCTION HB_ToLogFileOnOff( lOnOff )
  LOCAL lOld := s_lToLogFile
  IF HB_ISLOGICAL( lOnOff )
     s_lToLogFile := lOnOff
  ENDIF
RETURN lOld

FUNCTION HB_EmptyLogFileOnOff( lOnOff )
  LOCAL lOld := s_lEmptyLogFile
  IF HB_ISLOGICAL( lOnOff )
     s_lEmptyLogFile := lOnOff
  ENDIF
RETURN lOld

PROCEDURE HB_ToLogFile( cLogFile, ... )
  LOCAL nHandle

  IF !s_lToLogFile
     RETURN
  ENDIF

  DEFAULT cLogFile TO "logfile.log"

  IF cLogFile <> NIL

     IF !s_lEmptyLogFile .AND. FILE( cLogFile )
        nHandle := FOpen( cLogFile, FO_READWRITE + FO_SHARED)
     ELSE
        nHandle := FCreate( cLogFile )
        s_lEmptyLogFile := FALSE
        // Dopo che lo creato, lo richiudo immediatamente e lo riapro in modo condiviso
        // nel caso arrivasse una nuova scrittura
        IF Ferror() == 0 .AND. nHandle > 0
           FClose( nHandle )
           nHandle := FOpen( cLogFile, FO_READWRITE + FO_SHARED)
        ENDIF
        //__OutDebug( "Create ", nHandle )
     ENDIF

     // Writing
     IF nHandle > 0
        FSeek( nHandle, 0, FS_END )
        FWrite( nHandle, hb_sprintf( ... ) )
        FWrite( nHandle, HB_OSNewLine() )
        FClose( nHandle )
     ENDIF
  ENDIF
RETURN

