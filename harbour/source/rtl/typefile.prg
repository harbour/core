/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * __Typefile() function
 *
 * Copyright 2000 Luiz Rafael Culik <Culik@sl.conex.net>
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
#include "error.ch"
#include "fileio.ch"

#define BUFFER_LENGTH 2048

FUNCTION __TYPEFILE( cFile, lPrint )
   LOCAL nHandle, cBuffer
   LOCAL oErr, xRecover, nRetries
   LOCAL aSaveSet[ 2 ]
   LOCAL cDir, cName, cExt, cTmp, aPath, i

   IF !ISLOGICAL( lPrint )
      lPrint := .F.
   ENDIF

   IF !ISCHARACTER( cFile )
      oErr := ErrorNew()
      oErr:severity    := ES_ERROR
      oErr:genCode     := EG_OPEN
      oErr:subSystem   := "BASE"
      oErr:SubCode     := 2009
      oErr:Description := "Argument error: __TYPEFILE"
      Eval( ErrorBlock(), oErr )
   ENDIF

   // If no drive/dir specified, search the SET DEFAULT and PATH directories

   hb_FNameSplit( cFile, @cDir, @cName, @cExt )
   IF Empty( cDir )
      cTmp := SET( _SET_DEFAULT ) + ";" + SET( _SET_PATH )
      cTmp := StrTran( cTmp, ",", ";" )
      i := len( cTmp )
      IF substr( cTmp, i, 1 ) == ";"            // remove last ";"
         cTmp := substr( cTmp, 1, i - 1 )
      ENDIF
      aPath := aDvd( cTmp )
      FOR i := 1 TO len( aPath )
         cTmp := hb_FNameMerge( aPath[ i ], cName, cExt )
         IF file( cTmp )
            cFile := cTmp
            exit
         ENDIF
      NEXT
   ENDIF

   nRetries := 0
   DO WHILE ( nHandle := FOPEN( cFile, FO_READWRITE ) ) == F_ERROR
      oErr := ErrorNew()
      oErr:severity    := ES_ERROR
      oErr:genCode     := EG_OPEN
      oErr:subSystem   := "BASE"
      oErr:SubCode     := 2011
      oErr:Description := "Open Error: " + cFile
      oErr:canDefault  := .T.
      oErr:canRetry    := .T.
      oErr:OsCode      := FERROR()
      oErr:tries       := ++nRetries
      xRecover := Eval( ErrorBlock(), oErr )
      IF ISLOGICAL( xRecover ) .and. !xRecover      // user select "Default"
         RETURN NIL
      ENDIF
   ENDDO

   // NOTE: the NG say you should explicitly SET CONSOLE OFF if you wish to
   //       suppress output to screen. [ckedem]

   IF lPrint
      aSaveSet[ 1 ] := Set( _SET_DEVICE, "PRINTER" )
      aSaveSet[ 2 ] := Set( _SET_PRINTER, .T. )
   ENDIF

   // here we try to read a line at a time but I think we could just
   // display the whole buffer since it said: "without any headings or formating"

   cbuffer := SPACE( BUFFER_LENGTH )
   ?                                                      // start in a new line
   DO WHILE fread( nHandle, @cbuffer, BUFFER_LENGTH ) > 0
      ?? cBuffer
      cbuffer := SPACE( BUFFER_LENGTH )
   ENDDO

   FCLOSE( nHandle )

   IF lPrint
      Set( _SET_DEVICE,  aSaveSet[ 1 ] )
      Set( _SET_PRINTER, aSaveSet[ 2 ] )
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------------*/
/*         Function aDvd : Divide string to tokens and put tokens into array  */
/*         Parameters : cString - String to be splited          ( C )         */
/*                      cDelim  - Delimiter of tokens in string ( C )         */
/*         Return :     Array of tokens or empty array                        */
/*----------------------------------------------------------------------------*/

STATIC FUNCTION aDvd( cString, cDelim )
   LOCAL aProm := {}
   LOCAL nPos

   DEFAULT cDelim TO ";"

   DO WHILE ( nPos := at( cDelim, cString ) ) != 0
      AAdd( aProm, substr( cString, 1, nPos - 1 ) )
      cString := substr( cString, nPos + len( cDelim ) )
   ENDDO
   AAdd( aProm, cString )

   RETURN aProm
