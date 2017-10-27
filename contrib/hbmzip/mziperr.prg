/*
 * mzip error strings
 *
 * Copyright 2011 Viktor Szakats (vszakats.net/harbour)
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

#include "hbmzip.ch"

FUNCTION hb_zipErrorStr( nStatus )

   IF ! HB_ISNUMERIC( nStatus )
      RETURN "ZIP_INVALID"
   ENDIF

   SWITCH nStatus
   CASE ZIP_OK                  ; RETURN "ZIP_OK"
   CASE ZIP_PARAMERROR          ; RETURN "ZIP_PARAMERROR"
   CASE ZIP_BADZIPFILE          ; RETURN "ZIP_BADZIPFILE"
   CASE ZIP_INTERNALERROR       ; RETURN "ZIP_INTERNALERROR"
   CASE 1                       ; RETURN "Z_STREAM_END"
   CASE 2                       ; RETURN "Z_NEED_DICT"
   CASE -1                      ; RETURN "Z_ERRNO"
   CASE -2                      ; RETURN "Z_STREAM_ERROR"
   CASE -3                      ; RETURN "Z_DATA_ERROR"
   CASE -4                      ; RETURN "Z_MEM_ERROR"
   CASE -5                      ; RETURN "Z_BUF_ERROR"
   CASE -6                      ; RETURN "Z_VERSION_ERROR"
   ENDSWITCH

   RETURN "ZIP_UNKNOWN_" + hb_ntos( nStatus )

FUNCTION hb_unzipErrorStr( nStatus )

   IF ! HB_ISNUMERIC( nStatus )
      RETURN "UNZ_INVALID"
   ENDIF

   SWITCH nStatus
   CASE UNZ_OK                  ; RETURN "UNZ_OK"
   CASE UNZ_END_OF_LIST_OF_FILE ; RETURN "UNZ_END_OF_LIST_OF_FILE"
   CASE UNZ_PARAMERROR          ; RETURN "UNZ_PARAMERROR"
   CASE UNZ_BADZIPFILE          ; RETURN "UNZ_BADZIPFILE"
   CASE UNZ_INTERNALERROR       ; RETURN "UNZ_INTERNALERROR"
   CASE UNZ_CRCERROR            ; RETURN "UNZ_CRCERROR"
   CASE 1                       ; RETURN "Z_STREAM_END"
   CASE 2                       ; RETURN "Z_NEED_DICT"
   CASE -1                      ; RETURN "Z_ERRNO"
   CASE -2                      ; RETURN "Z_STREAM_ERROR"
   CASE -3                      ; RETURN "Z_DATA_ERROR"
   CASE -4                      ; RETURN "Z_MEM_ERROR"
   CASE -5                      ; RETURN "Z_BUF_ERROR"
   CASE -6                      ; RETURN "Z_VERSION_ERROR"
   ENDSWITCH

   RETURN "UNZ_UNKNOWN_" + hb_ntos( nStatus )
