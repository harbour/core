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

#include "error.ch"

#include "hbzlib.ch"

THREAD STATIC t_nLastError := HB_ZLIB_RES_OK

/****** COMPRESSOR WRAPPER
 *  hb_Compress(               cSource [,nSourceLen ] ) --> cDest
 *  hb_Compress( nComprFactor, cSource [,nSourceLen ] ) --> cDest
 *  hb_Compress(               cSource, nSourceLen, @cDest, @nDestLen ) --> nError
 *  hb_Compress( nComprFactor, cSource, nSourceLen, @cDest, @nDestLen ) --> nError
 */
FUNCTION hb_Compress( xPar1, xPar2, xPar3, xPar4, xPar5 )

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
         xPar4 := hb_ZCompress( cSource, nDestLen, @t_nLastError, nComprFactor )
         hb_default( @xPar4, "" )
         xPar5 := Len( xPar4 )
      ELSE
         xPar3 := hb_ZCompress( cSource, nDestLen, @t_nLastError, nComprFactor )
         hb_default( @xPar3, "" )
         xPar4 := Len( xPar3 )
      ENDIF
      RETURN t_nLastError
   ENDIF

   RETURN hb_ZCompress( cSource, nDestLen, @t_nLastError, nComprFactor )

/****** DECOMPRESSOR WRAPPER
 *  hb_Uncompress( nDestLen, cSource [, nSourceLen ] ) --> cDest
 *  hb_Uncompress( nDestLen, cSource, nSourceLen, @cDest ) --> nError
 */
FUNCTION hb_Uncompress( nDestLen, cSource, nSourceLen, /* @ */ cDest )

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
      cDest := hb_ZUncompress( cSource, nDestLen, @t_nLastError )
      RETURN t_nLastError
   ENDIF

   RETURN hb_ZUncompress( cSource, nDestLen, @t_nLastError )

/**
 *  hb_CompressError() --> nError
 */
FUNCTION hb_CompressError()
   RETURN t_nLastError

/**
 *  hb_CompressErrorDesc( nErrorCode ) --> cDesc
 */
FUNCTION hb_CompressErrorDesc( nError )
   RETURN hb_ZError( nError )

/**
 *  hb_CompressBufLen( nSrcLen ) --> nDestLen
 */
FUNCTION hb_CompressBufLen( nSrcLen )

   LOCAL nRet

   hb_default( @nSrcLen, 0 )

   nRet := nSrcLen

   nRet += nRet / 100 * 15 + 12

   IF ( nSrcLen % 100 ) != 0
      nRet += 15
   ENDIF

   RETURN nRet
