/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Reimplementation of xhb .zip functions. EXPERIMENTAL CODE. USE AT YOUR OWN RISK. NO GUARANTEES.
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

#include "error.ch"

#include "hbzlib.ch"

THREAD STATIC s_nLastError := HB_ZLIB_RES_OK

/****** COMPRESSOR WRAPPER *****
*  HB_COMPRESS(               cSource [,nSourceLen ] ) --> cDest
*  HB_COMPRESS( nComprFactor, cSource [,nSourceLen ] ) --> cDest
*  HB_COMPRESS(               cSource, nSourceLen, @cDest, @nDestLen ) --> nError
*  HB_COMPRESS( nComprFactor, cSource, nSourceLen, @cDest, @nDestLen ) --> nError
*/
FUNCTION HB_COMPRESS( xPar1, xPar2, xPar3, xPar4, xPar5 )
   LOCAL nComprFactor
   LOCAL cSource
   LOCAL nSourceLen
   LOCAL nDestLen

   LOCAL lReturnByRef

   LOCAL oError

   IF HB_ISNUMERIC( xPar1 )
      nComprFactor := xPar1
      cSource      := xPar2
      nSourceLen   := xPar3
      nDestLen     := xPar5
      lReturnByRef := PCount() >= 4
   ELSE
      nComprFactor := HB_ZLIB_COMPRESSION_DEFAULT
      cSource      := xPar1
      nSourceLen   := xPar2
      nDestLen     := xPar4
      lReturnByRef := PCount() >= 3
   ENDIF

   IF ! HB_ISSTRING( cSource )
      oError := ErrorNew()

      oError:severity    := ES_ERROR
      oError:genCode     := EG_ARG
      oError:subSystem   := "BASE"
      oError:subCode     := 3012
      oError:args        := { cSource }

      Eval( ErrorBlock(), oError )
      RETURN NIL
   ENDIF

   IF ! HB_ISNUMERIC( nDestLen )
      nDestLen := NIL
   ENDIF

   IF HB_ISNUMERIC( nSourceLen ) .AND. nSourceLen >= 0 .AND. nSourceLen < Len( cSource )
      cSource := Left( cSource, nSourceLen )
   ENDIF

   IF lReturnByRef
      IF HB_ISNUMERIC( xPar1 )
         xPar4 := HB_ZCOMPRESS( cSource, nDestLen, @s_nLastError, nComprFactor )
         IF ! HB_ISSTRING( xPar4 )
            xPar4 := ""
         ENDIF
         xPar5 := Len( xPar4 )
      ELSE
         xPar3 := HB_ZCOMPRESS( cSource, nDestLen, @s_nLastError, nComprFactor )
         IF ! HB_ISSTRING( xPar3 )
            xPar3 := ""
         ENDIF
         xPar4 := Len( xPar3 )
      ENDIF
      RETURN s_nLastError
   ENDIF

   RETURN HB_ZCOMPRESS( cSource, nDestLen, @s_nLastError, nComprFactor )

/****** DECOMPRESSOR WRAPPER *****
*  HB_UNCOMPRESS( nDestLen, cSource [, nSourceLen ] ) --> cDest
*  HB_UNCOMPRESS( nDestLen, cSource, nSourceLen, @cDest ) --> nError
*/
FUNCTION HB_UNCOMPRESS( nDestLen, cSource, nSourceLen, /* @ */ cDest )
   LOCAL oError

   IF ! HB_ISNUMERIC( nDestLen ) .OR. ;
      ! HB_ISSTRING( cSource )

      oError := ErrorNew()

      oError:severity    := ES_ERROR
      oError:genCode     := EG_ARG
      oError:subSystem   := "BASE"
      oError:subCode     := 3012
      oError:args        := { nDestLen }

      Eval( ErrorBlock(), oError )
      RETURN NIL
   ENDIF

   IF HB_ISNUMERIC( nSourceLen ) .AND. nSourceLen >= 0 .AND. nSourceLen < Len( cSource )
      cSource := Left( cSource, nSourceLen )
   ENDIF

   IF PCount() >= 4
      cDest := HB_ZUNCOMPRESS( cSource, nDestLen, @s_nLastError )
      RETURN s_nLastError
   ENDIF

   RETURN HB_ZUNCOMPRESS( cSource, nDestLen, @s_nLastError )

/*********************************
*  HB_COMPRESSERROR() --> nError
*/
FUNCTION HB_COMPRESSERROR()
   RETURN s_nLastError

/*********************************
*  HB_COMPRESSERRORDESC( nErrorCode ) --> cDesc
*/
FUNCTION HB_COMPRESSERRORDESC( nError )
   RETURN HB_ZERROR( nError )

/*******************************
*  HB_COMPRESSBUFLEN( nSrcLen ) --> nDestLen
*/
FUNCTION HB_COMPRESSBUFLEN( nSrcLen )
   LOCAL nRet

   IF ! HB_ISNUMERIC( nSrcLen )
      nSrcLen := 0
   ENDIF

   nRet := nSrcLen

   nRet += nRet / 100 * 15 + 12

   IF ( nSrcLen % 100 ) != 0
      nRet += 15
   ENDIF

   RETURN nRet
