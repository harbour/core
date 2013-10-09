/*
 * Harbour Project source code:
 * Supplementary functions (error strings)
 *
 * Copyright 2013 Viktor Szakats (vszakats.net/harbour)
 * Copyright 2009 P.Chornyj <myorg63@mail.ru>
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

#include "hbsqlit3.ch"

FUNCTION hb_sqlite3_errstr_short( nError )

   IF ! HB_ISNUMERIC( nError )
      RETURN "HB_SQLITE_INVALID"
   ENDIF

   SWITCH nError
   CASE SQLITE_OK         ; RETURN "SQLITE_OK"
   CASE SQLITE_ERROR      ; RETURN "SQLITE_ERROR"
   CASE SQLITE_INTERNAL   ; RETURN "SQLITE_INTERNAL"
   CASE SQLITE_PERM       ; RETURN "SQLITE_PERM"
   CASE SQLITE_ABORT      ; RETURN "SQLITE_ABORT"
   CASE SQLITE_BUSY       ; RETURN "SQLITE_BUSY"
   CASE SQLITE_LOCKED     ; RETURN "SQLITE_LOCKED"
   CASE SQLITE_NOMEM      ; RETURN "SQLITE_NOMEM"
   CASE SQLITE_READONLY   ; RETURN "SQLITE_READONLY"
   CASE SQLITE_INTERRUPT  ; RETURN "SQLITE_INTERRUPT"
   CASE SQLITE_IOERR      ; RETURN "SQLITE_IOERR"
   CASE SQLITE_CORRUPT    ; RETURN "SQLITE_CORRUPT"
   CASE SQLITE_NOTFOUND   ; RETURN "SQLITE_NOTFOUND"
   CASE SQLITE_FULL       ; RETURN "SQLITE_FULL"
   CASE SQLITE_CANTOPEN   ; RETURN "SQLITE_CANTOPEN"
   CASE SQLITE_PROTOCOL   ; RETURN "SQLITE_PROTOCOL"
   CASE SQLITE_EMPTY      ; RETURN "SQLITE_EMPTY"
   CASE SQLITE_SCHEMA     ; RETURN "SQLITE_SCHEMA"
   CASE SQLITE_TOOBIG     ; RETURN "SQLITE_TOOBIG"
   CASE SQLITE_CONSTRAINT ; RETURN "SQLITE_CONSTRAINT"
   CASE SQLITE_MISMATCH   ; RETURN "SQLITE_MISMATCH"
   CASE SQLITE_MISUSE     ; RETURN "SQLITE_MISUSE"
   CASE SQLITE_NOLFS      ; RETURN "SQLITE_NOLFS"
   CASE SQLITE_AUTH       ; RETURN "SQLITE_AUTH"
   CASE SQLITE_FORMAT     ; RETURN "SQLITE_FORMAT"
   CASE SQLITE_RANGE      ; RETURN "SQLITE_RANGE"
   CASE SQLITE_NOTADB     ; RETURN "SQLITE_NOTADB"
   CASE SQLITE_ROW        ; RETURN "SQLITE_ROW"
   CASE SQLITE_DONE       ; RETURN "SQLITE_DONE"
   ENDSWITCH

   RETURN "HB_SQLITE_UNKNOWN_" + hb_ntos( nError )
