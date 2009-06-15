/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * hbextern.ch generator
 *
 * Copyright 1999 Ryszard Glab <rglab@imid.med.pl>
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

/* NOTE: The process is not completely automatical, the generated output should
         be edited by hand after extraction. */

#include "directry.ch"
#include "fileio.ch"

// Remark this line when BEGINDUMP/ENDDUMP #pragma's
// are not used anymore in Harbour core and RTL .prg files:
// #define PRG_CAN_HAVE_HB_FUNC

#ifdef __HARBOUR__
   #define EOL  hb_OSNewLine()
#else
   #define EOL  ( Chr( 13 ) + Chr( 10 ) )
#endif

#ifdef __PLATFORM__UNIX
   #define PATH_SEPARATOR "/"
   #define BASE_DIR "../../source/"
#else
   #define PATH_SEPARATOR "\"
   #define BASE_DIR "..\..\source\"
#endif

// List of known files which does not contain any real public function.
// (always write the LOWERCASE file name)
STATIC s_aSkipList := { "profiler.prg" }

PROCEDURE MAIN()
   LOCAL aDirs :={ BASE_DIR + "debug", ;
                   BASE_DIR + "pp"   , ;
                   BASE_DIR + "rdd"  , ;
                   BASE_DIR + "rdd" + PATH_SEPARATOR + "dbfcdx", ;
                   BASE_DIR + "rdd" + PATH_SEPARATOR + "dbfdbt", ;
                   BASE_DIR + "rdd" + PATH_SEPARATOR + "dbffpt", ;
                   BASE_DIR + "rdd" + PATH_SEPARATOR + "dbfntx", ;
                   BASE_DIR + "rdd" + PATH_SEPARATOR + "hbsix" , ;
                   BASE_DIR + "rdd" + PATH_SEPARATOR + "hsx"   , ;
                   BASE_DIR + "rdd" + PATH_SEPARATOR + "nulsys", ;
                   BASE_DIR + "rtl"  , ;
                   BASE_DIR + "vm"     }
   LOCAL i, nOutput

   SET DATE FORMAT TO "yyyy-mm-dd"
   ? "Processing files:"
   nOutput := FCREATE( "hbextern.ch_" )
   IF nOutput > 0
      FWRITE( nOutput, "// NOTE: Machine generated on: " + DTOC( DATE() ) + EOL + ;
                       "//       This output should be edited by hand after extraction." + EOL + EOL )
      FOR i := 1 TO LEN( aDirs )
         FWRITE( nOutput, EOL + "// " + REPLICATE( "-", 60 ) + EOL )
         FWRITE( nOutput, "// Files from: " + aDirs[ i ] + EOL + EOL )
         ProcessDir( nOutput, aDirs[i] + PATH_SEPARATOR + "*.c"  , aDirs[ i ], .F. )
         ProcessDir( nOutput, aDirs[i] + PATH_SEPARATOR + "*.prg", aDirs[ i ], .T. )
      NEXT
      FWRITE( nOutput, EOL + "// " + REPLICATE( "-", 60 ) + EOL + EOL )
      FCLOSE( nOutput )
   ENDIF
   ? "Done."

   RETURN

STATIC PROCEDURE ProcessDir( nOutput, cFiles, cDir, lPRG )
   LOCAL i, nLen, aFiles

   aFiles := DIRECTORY( cFiles )
   IF ( nLen := LEN( aFiles ) ) > 0
      ASORT( aFiles,,, {|x,y| x[ F_NAME ] < y[ F_NAME ] } )
      FOR i := 1 TO nLen
         ProcessFile( nOutput, cDir + PATH_SEPARATOR + aFiles[ i ][ 1 ], lPRG )
      NEXT
   ENDIF

   RETURN

STATIC PROCEDURE ProcessFile( nOutput, cFile, lPRG )
   LOCAL nH

   // Skip known files which does not contain any real public function
   IF ASCAN( s_aSkipList, {|c| c $ lower( cFile ) } ) > 0
      RETURN
   ENDIF

   ? cFile
   FWRITE( nOutput, "//" + EOL + "// symbols from file: " + cFile + EOL + "//" + EOL )
   nH := FOPEN( cFile )
   IF nH > 0
      FILEEVAL( nH, 255, EOL, {|c| Processline( nOutput, c, lPRG ) } )
      FCLOSE( nH )
   ENDIF

   RETURN

STATIC PROCEDURE ProcessLine( nOutput, cLine, lPRG )
   LOCAL nPos, nLen

   cLine := AllTrim( cLine )
   IF empty( cLine )
      RETURN
   ENDIF

   IF lPRG                                // PRG source file (FUNC, PROC)

      IF upper( SubStr( cLine, 1, 4 ) ) == "FUNC" .or. upper( SubStr( cLine, 1, 4 ) ) == "PROC"
         IF ( nPos := AT( " ", cLine ) ) > 4
            cLine := LTrim( SubStr( cLine, nPos ) )
            nLen  := len( cLine )
            FOR nPos := 1 TO nLen
               IF SubStr( cLine, nPos, 1 ) $ " (;/&" + Chr( 9 )
                  --nPos
                  EXIT
               ENDIF
            NEXT
            WriteSymbol( nOutput, SubStr( cLine, 1, nPos ) )
         ENDIF
      ENDIF

#ifdef PRG_CAN_HAVE_HB_FUNC
   ENDIF
#else
   ELSE                                   // C source file (HB_FUNC)
#endif

      IF SubStr( cLine, 1, 8 ) == "HB_FUNC("
         cLine := SubStr( cLine, 9 )
         IF ( nPos := AT( ")", cLine ) ) > 0
            WriteSymbol( nOutput, AllTrim( SubStr( cLine, 1, nPos - 1 ) ) )
         ENDIF
      ENDIF

#ifndef PRG_CAN_HAVE_HB_FUNC
   ENDIF
#endif

   RETURN


STATIC PROCEDURE FileEval( nHandle, nMaxLine, cDelim, bBlock )
   LOCAL cBuffer := ""

   FSEEK( nHandle, 0 )

   DO WHILE FReadLn( nHandle, @cBuffer, nMaxLine, cDelim )
      EVAL( bBlock, cBuffer )
   ENDDO

   RETURN


STATIC FUNCTION FReadLn( nHandle, cBuffer, nMaxLine, cDelim )
   LOCAL cLine, nSavePos, nEol, nNumRead

   cLine := space( nMaxLine )
   cBuffer := ""
   nSavePos := FSEEK( nHandle, 0, FS_RELATIVE )
   nNumRead := FREAD( nHandle, @cLine, nMaxLine )
   IF ( nEol := AT( cDelim, substr( cLine, 1, nNumRead ) ) ) == 0
      cBuffer := cLine
   ELSE
      cBuffer := SubStr( cLine, 1, nEol - 1 )
      FSEEK( nHandle, nSavePos + nEol + 1, FS_SET )
   ENDIF

   RETURN nNumRead != 0


STATIC PROCEDURE WriteSymbol( nOutput, cLine )
STATIC s_aNames := { "MAIN" }   // Init with names you want to skip

   IF len( cLine ) > 0
      cLine := upper( cLine )
      IF ASCAN( s_aNames, {|c| c == cLine } ) == 0
         AADD( s_aNames, cLine )
         FWRITE( nOutput, "EXTERNAL " + cLine + EOL )
      ENDIF
   ENDIF

   RETURN
