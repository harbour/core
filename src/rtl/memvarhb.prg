/*
 * MEMVAR save/restore functions with >10 long variable name support.
 *
 * Copyright 2010 Viktor Szakats (vszakats.net/harbour)
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

#pragma -gc0

#include "hbmemvar.ch"
#include "error.ch"
#include "fileio.ch"

/*
 * 0xC0, 'H', 'B', 'V' followed two-byte version number in Little Endian order.
 * Corresponding magic(5) rule:
 *
 *    0       string          \xc0HBV         Harbour variable dump file
 *    >4      leshort         x               version %d
 *
 * Until such time that the serialized format changes, and handling of
 * previously-saved files is required, only a naive approach of using
 * version 1 is taken.
 */

#define _HBMEM_EXT       ".hbv"

#define _HBMEM_SIG_LEN   6
#define _HBMEM_SIGNATURE ( ;
   hb_BChar( 0xC0 ) + ;
   hb_BChar( 0x48 ) + ;
   hb_BChar( 0x42 ) + ;
   hb_BChar( 0x56 ) + ;
   hb_BChar( 0x01 ) + ;
   hb_BChar( 0x00 ) )

PROCEDURE hb_mvSave( cFileName, cMask, lIncludeMask )

   LOCAL nCount
   LOCAL xValue
   LOCAL cName
   LOCAL nScope
   LOCAL lMatch

   LOCAL aVars
   LOCAL hFile
   LOCAL tmp

   LOCAL oError
   LOCAL nRetries

   IF HB_ISSTRING( cFileName )

      IF Set( _SET_DEFEXTENSIONS )
         cFileName := hb_FNameExtSetDef( cFileName, _HBMEM_EXT )
      ENDIF

      IF ! HB_ISSTRING( cMask ) .OR. ;
         Empty( cMask ) .OR. hb_LeftEq( cMask, "*" )
         cMask := "*"
      ENDIF

      hb_default( @lIncludeMask, .T. )

      aVars := {}

      FOR EACH nScope IN { HB_MV_PUBLIC, HB_MV_PRIVATE }
         nCount := __mvDbgInfo( nScope )
         FOR tmp := 1 TO nCount
            xValue := __mvDbgInfo( nScope, tmp, @cName )
            IF ValType( xValue ) $ "CNDTL"
               lMatch := hb_WildMatchI( cMask, cName )
               IF iif( lIncludeMask, lMatch, ! lMatch )
                  AAdd( aVars, { cName, xValue } )
               ENDIF
            ENDIF
         NEXT
      NEXT

      nRetries := 0
      DO WHILE .T.
         IF ( hFile := hb_vfOpen( cFileName, FO_CREAT + FO_TRUNC + FO_WRITE + FO_EXCLUSIVE ) ) == NIL
            oError := ErrorNew()

            oError:severity    := ES_ERROR
            oError:genCode     := EG_OPEN
            oError:subSystem   := "BASE"
            oError:subCode     := 2006
            oError:canRetry    := .T.
            oError:canDefault  := .T.
            oError:fileName    := cFileName
            oError:osCode      := FError()
            oError:tries       := ++nRetries

            IF hb_defaultValue( Eval( ErrorBlock(), oError ), .F. )
               LOOP
            ENDIF
         ENDIF
         EXIT
      ENDDO

      IF hFile != NIL
         hb_vfWrite( hFile, _HBMEM_SIGNATURE )
         hb_vfWrite( hFile, hb_Serialize( aVars ) )
         hb_vfClose( hFile )
      ENDIF
   ELSE
      oError := ErrorNew()

      oError:severity    := ES_ERROR
      oError:genCode     := EG_ARG
      oError:subSystem   := "BASE"
      oError:subCode     := 2008
      oError:canRetry    := .F.
      oError:canDefault  := .F.
      oError:Args        := hb_AParams()
      oError:operation   := ProcName()

      Eval( ErrorBlock(), oError )
   ENDIF

   RETURN

FUNCTION hb_mvRestore( cFileName, lAdditive, cMask, lIncludeMask )

   LOCAL item
   LOCAL cName
   LOCAL lMatch

   LOCAL aVars
   LOCAL cBuffer
   LOCAL xValue

   LOCAL hFile

   LOCAL oError
   LOCAL nRetries

   IF HB_ISSTRING( cFileName )

      IF ! hb_defaultValue( lAdditive, .T. )
         __mvClear()
      ENDIF

      IF Set( _SET_DEFEXTENSIONS )
         cFileName := hb_FNameExtSetDef( cFileName, _HBMEM_EXT )
      ENDIF

      IF ! HB_ISSTRING( cFileName ) .OR. ;
         Empty( cMask ) .OR. hb_LeftEq( cMask, "*" )
         cMask := "*"
      ENDIF

      hb_default( @lIncludeMask, .T. )

      nRetries := 0
      DO WHILE .T.

         IF ( hFile := hb_vfOpen( cFileName, FO_READ ) ) == NIL
            oError := ErrorNew()

            oError:severity    := ES_ERROR
            oError:genCode     := EG_OPEN
            oError:subSystem   := "BASE"
            oError:subCode     := 2005
            oError:canRetry    := .T.
            oError:canDefault  := .T.
            oError:fileName    := cFileName
            oError:osCode      := FError()
            oError:tries       := ++nRetries

            IF hb_defaultValue( Eval( ErrorBlock(), oError ), .F. )
               LOOP
            ENDIF
         ENDIF
         EXIT
      ENDDO

      IF hFile == NIL
         RETURN .F.
      ENDIF

      xValue := NIL

      IF hb_vfReadLen( hFile, _HBMEM_SIG_LEN ) == _HBMEM_SIGNATURE

         cBuffer := Space( hb_vfSize( hFile ) - _HBMEM_SIG_LEN )
         hb_vfSeek( hFile, _HBMEM_SIG_LEN, FS_SET )
         hb_vfRead( hFile, @cBuffer, hb_BLen( cBuffer ) )
         hb_vfClose( hFile )

         aVars := hb_Deserialize( cBuffer )
         cBuffer := NIL

         IF HB_ISARRAY( aVars )
            FOR EACH item IN aVars
               IF HB_ISARRAY( item ) .AND. Len( item ) == 2 .AND. ;
                  HB_ISSTRING( item[ 1 ] ) .AND. ;
                  ! Empty( item[ 1 ] )

                  cName := item[ 1 ]
                  lMatch := hb_WildMatchI( cMask, cName )
                  IF iif( lIncludeMask, lMatch, ! lMatch )
                     IF xValue == NIL
                        xValue := item[ 2 ]
                     ENDIF
                     __mvPut( cName, item[ 2 ] )
                  ENDIF
               ENDIF
            NEXT
            __mvSetBase()
         ENDIF
      ELSE
         hb_vfClose( hFile )
      ENDIF

      RETURN xValue
   ELSE
      oError := ErrorNew()

      oError:severity    := ES_ERROR
      oError:genCode     := EG_ARG
      oError:subSystem   := "BASE"
      oError:subCode     := 2007
      oError:canRetry    := .F.
      oError:canDefault  := .F.
      oError:Args        := hb_AParams()
      oError:operation   := ProcName()

      Eval( ErrorBlock(), oError )
   ENDIF

   RETURN NIL
