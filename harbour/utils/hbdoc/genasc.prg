/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * GENASC support module for hbdoc document Extractor 
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
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

#ifdef __HARBOUR__
#define NANFOR
#endif

#include "directry.ch"
#include "fileio.ch"
#include "inkey.ch"

//  output lines on the screen

#define INFILELINE   10
#define MODULELINE   12
#define LINELINE     14
#define ERRORLINE    20
#define LONGLINE     100
#define LONGONELINE  86
#define CRLF HB_OSNewLine()
//  The delimiter
#define DELIM   "$"                 // keyword delimiter

#xtranslate UPPERLOWER(<exp>) => (UPPER(SUBSTR(<exp>,1,1))+LOWER(SUBSTR(<exp>,2)))
MEMVAR aDirList
MEMVAR aDocInfo
MEMVAR aWww
MEMVAR LCONTINUOUS
MEMVAR lAuthor
STATIC NWRITEHANDLE

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function ASCIIFiles()
*+
*+    Called from ( hbdoc.prg    )   2 - function main()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
FUNCTION ASCIIFiles()

   //
   //  This routine and all accompaning database structures are
   //  Copyright (C) 1992 Leo J. Letendre.
   //
   //  Purpose: Process each of the files in the directory making an ascii file
   //
   //  Modification History:
   //         Version    Date      Who       Notes
   //          V1.00     1/19/92   LJL       Initial Version
   //          V1.01     2/25/92   LJL       Trimmed spaces from see also and header
   //          V1.02     10/30/92  LJL       Added multi-line one liner check and
   //                                        insured blank line prior to and after
   //                                        category headers
   //          V1.03     10/30/93  LJL       added changable delimiter support,
   //                                        COMMANDNAME keyword and removed
   //                                        filename references in see alsos
   //
   //  Calling parameters: None
   //
   //  Notes: None
   // -
   //  LOCAL variables:

   LOCAL i
   LOCAL j
   LOCAL nFiles      := LEN( aDirList )
   LOCAL nCommentLen
   LOCAL lEof
   LOCAL lDoc
   LOCAL cBuffer
   LOCAL nEnd
   LOCAL nCount
   LOCAL cAuthor
   LOCAL cBar        := "哪哪哪哪哪哪哪哪哪哪哪哪哪哪哪哪哪哪哪哪哪哪哪哪哪哪哪哪哪哪哪哪哪哪哪哪哪哪哪" + CRLF
   LOCAL nMode
   LOCAL cFuncName
   LOCAL cOneLine
   LOCAL cCategory
   LOCAL cFileName
   LOCAL nLineCnt
   LOCAL cSeeAlso
   LOCAL cTemp
   LOCAL cChar
   LOCAL nDocCnt
   LOCAL lBlankLine  := .F.                // Blank line encountered and sent out
   LOCAL lAddBlank   := .F.                // Need to add a blank line if next line is not blank
   LOCAL lFunc       := .T.                // currently a function rather than a command
   LOCAL nReadHandle
   LOCAL cDoc        := DELIM + "DOC" + DELIM                  // DOC keyword
   LOCAL cEnd        := DELIM + "END" + DELIM                  // END keyword
   LOCAL cFunc       := DELIM + "FUNCNAME" + DELIM             // FUNCNAME keyword
   LOCAL cCat        := DELIM + "CATEGORY" + DELIM             // CATEGORY keyword
   LOCAL cOne        := DELIM + "ONELINER" + DELIM             // ONELINER keyword
   LOCAL cSyn        := DELIM + "SYNTAX" + DELIM               // SYNTAX keyword
   LOCAL cArg        := DELIM + "ARGUMENTS" + DELIM            // ARGUMENTS keyword
   LOCAL cRet        := DELIM + "RETURNS" + DELIM              // RETURNS keyword
   LOCAL cDesc       := DELIM + "DESCRIPTION" + DELIM          // DESCRIPTION keyword
   LOCAL cExam       := DELIM + "EXAMPLES" + DELIM             // EXAMPLES keyword
   LOCAL cSee        := DELIM + "SEEALSO" + DELIM              // SEEALSO keyword
   LOCAL cInc        := DELIM + "INCLUDE" + DELIM              // INCLUDE keyword
   LOCAL cComm       := DELIM + "COMMANDNAME" + DELIM          // COMMAND keyword
   LOCAL cCompl      := DELIM + "COMPLIANCE" + DELIM
   LOCAL cTest       := DELIM + 'TESTS' + DELIM
   LOCAL cStatus     := DELIM + 'STATUS' + DELIM
   LOCAL cPlat       := DELIM + 'PLATFORMS' + DELIM
   LOCAL cFiles      := DELIM + 'FILES' + DELIM
   LOCAL cSubCode    := DELIM + 'SUBCODE' + DELIM
   LOCAL cFunction   := DELIM + 'FUNCTION' + DELIM

#define D_NORMAL     1
#define D_ARG        2
#define D_SYNTAX     3
#define D_IGNORE     4
#define D_SEEALSO    5
#define D_INCLUDE    6
#define D_ONELINE    7
#define D_STATUS     8
#define D_TESTS      9
#define D_FUNCTIONS 10

   //
   //  Entry Point
   //
   //  Put up information labels
   @ INFILELINE, 20 SAY "Extracting: "          
   @ MODULELINE, 20 SAY "Documenting: "         
   //  loop through all of the files

   FOR i := 1 TO nFiles

      //  Open file for input

      nCommentLen := IIF( AT( ".ASM", UPPER( aDirList[ i, F_NAME ] ) ) > 0, 2, 3 )
      nReadHandle := FT_FUSE( aDirList[ i, F_NAME ] )
      @ INFILELINE, 33 CLEAR TO INFILELINE, MAXCOL()
      @ INFILELINE, 33 SAY PAD( aDirList[ i, F_NAME ], 47 )         
      @ MODULELINE, 33 CLEAR TO LINELINE, MAXCOL()
      @ LINELINE, 27   SAY "Line:"                                  

      nLineCnt := 0

      IF nReadHandle < 0
         WRITE_ERROR( "Can't open file: (Dos Error " + STR( FERROR() ) + ")",,,, aDirList[ i, F_NAME ] )
         @ ERRORLINE,  0 CLEAR TO ERRORLINE, MAXCOL()
         @ ERRORLINE, 20 SAY "Can't open file: (Dos Error " + STR( FERROR() ) + ") File=" + aDirList[ i, F_NAME ]         
         LOOP
      ENDIF
      lEof := .F.
      lDoc := .F.
      //  First find the author

      DO WHILE .NOT. lEof

         //  Read a line

         cBuffer := TRIM( SUBSTR( ReadLN( @lEof ), nCommentLen ) )
         nLineCnt ++
         IF nLineCnt % 10 = 0
            @ LINELINE, 33 SAY STR( nLineCnt, 5, 0 )         
         ENDIF

         //  check to see if we are in doc mode or getting out of doc mode

         IF AT( cDoc, cBuffer ) > 0
            IF lDoc
               WRITE_ERROR( cDoc + " encountered during extraction of Doc" ;
                            + " at line" + STR( nLinecnt, 5, 0 ),,,, aDirList[ i, F_NAME ] )
            ENDIF
            lDoc    := .T.
            cBuffer := TRIM( SUBSTR( ReadLN( @lEof ), ;
                             nCommentLen ) )
            nLineCnt ++
            cCategory := cFuncName := cSeeAlso := ""
            nMode     := D_IGNORE
         ELSEIF AT( cEnd, cBuffer ) > 0
            IF .NOT. lDoc
               WRITE_ERROR( cEnd + " encountered outside of Doc area at line" ;
                            + STR( nLinecnt, 5, 0 ),,,, aDirList[ i, F_NAME ] )

            ELSE
               //  Add a new entry to our list of files

               IF EMPTY( cCategory )
                  WRITE_ERROR( "Blank Category",,,, aDirList[ i, F_NAME ] )
                  cCategory := "Unknown"
               ENDIF
               IF EMPTY( cFuncName )
                  WRITE_ERROR( "Blank Function Name",,,, aDirList[ i, F_NAME ] )
                  cFuncName := "Unknown"
               ENDIF
               AADD( aDocInfo, { cCategory, cFuncName, cOneLine, cFileName } )
               //  Now close down this little piece
               lDoc := .F.
               IF nDocCnt > 60 .AND. .NOT. lContinuous
                  FWRITE( nWriteHandle, CHR( K_CTRL_L ) + CRLF )
                  nDocCnt := 0
               ENDIF
               FWRITE( nWriteHandle, CRLF )
               FWRITE( nWriteHandle, " Source: " + aDirList[ i, F_NAME ] + CRLF + CRLF )
               IF lAuthor
                  FWRITE( nWriteHandle, " Author: " + cAuthor + CRLF )
               ENDIF
               IF .NOT. EMPTY( cSeeAlso )
                  FWRITE( nWriteHandle, "See also: " + cSeeAlso + CRLF )
               ENDIF
               IF .NOT. lContinuous
                  FWRITE( nWriteHandle, CHR( K_CTRL_L ) + CRLF )
               ELSE
                  FWRITE( nWriteHandle, CRLF + CRLF )
               ENDIF
               nDocCnt := 0
               FCLOSE( nWriteHandle )
            ENDIF
            nMode := D_IGNORE
            @ MODULELINE, 33 CLEAR TO MODULELINE, MAXCOL()
         ENDIF

         //  Act on the input
         IF lDoc
            //  1) function name

            IF AT( cFunc, cBuffer ) > 0 .OR. AT( cComm, cBuffer ) > 0
               lFunc   := AT( cFunc, cBuffer ) > 0
               cBuffer := ReadLN( @lEof )
               nLineCnt ++
               //  Save the function name
               cFuncName := UPPER( ALLTRIM( SUBSTR( cBuffer, nCommentLen ) ) )
               @ MODULELINE, 33 CLEAR TO MODULELINE, MAXCOL()
               @ MODULELINE, 33 SAY cFuncName         

               nMode := D_NORMAL

               //  Open a new file
               IF AT( "FT_", cFuncName ) > 0
                  cTemp := SUBSTR( cFuncName, 4 )
               ELSE
                  cTemp := cFuncName
               ENDIF

               IF ( nEnd := AT( "(", cTemp ) ) > 0
                  cTemp := LEFT( cTemp, nEnd - 1 )
               ENDIF
               cFileName := ""

               //  Strip off any other non-alphabetic/numeric characters
               FOR j := 1 TO LEN( cTemp )
                  cChar := SUBSTR( cTemp, j, 1 )
                  IF ( cChar >= "0" .AND. cChar <= "9" ) .OR. ;
                       ( cChar >= "A" .AND. cChar <= "Z" ) .OR. cChar = "_"
                     cFileName += cChar
                  ENDIF
               NEXT

               //  See if file name is present already. If so then modify

               cFileName := LEFT( cFileName, 8 )
               nEnd      := 1
               nCount    := 0
               DO WHILE nEnd > 0
                  nEnd := ASCAN( aDocInfo, { | a | a[ 4 ] == cFileName + ".hdf" } )
                  IF nEnd > 0

                     //  This will break if there are more than 10 files with the same first
                     //  seven characters. We take our chances.

                     IF LEN( cFileName ) = 8
                        cFileName := STUFF( cFileName, 8, 1, STR( nCount, 1, 0 ) )
                     ELSE
                        cFileName += STR( nCount, 1, 0 )
                     ENDIF
                     nCount ++
                  ENDIF
               ENDDO
               //  Add on the extension

               cFileName := LEFT( cFileName, 8 ) + ".hdf"

               nWriteHandle := FCREATE( "hdf\" + cFileName )
               IF nWriteHandle < 1
                  ? "Error creating", cFileName, ".hdf"
                  WRITE_ERROR( "Error creating",,,, cFileName + ".hdf" )
               ENDIF
               //  2) Category
            ELSEIF AT( cCat, cBuffer ) > 0
               cBuffer := ReadLN( @lEof )
               nLineCnt ++
               //  get the category
               cCategory := UPPER( ALLTRIM( SUBSTR( cBuffer, nCommentLen ) ) )

               //  3) One line description

            ELSEIF AT( cOne, cBuffer ) > 0
               cBuffer := ReadLN( @lEof )
               nLineCnt ++
               cOneLine := ALLTRIM( SUBSTR( cBuffer, nCommentLen ) )
               IF LEN( cOneLine ) > LONGONELINE
                  WRITE_ERROR( "OneLine", cOneLine, nLineCnt, LONGONELINE, ;
                               aDirList[ i, F_NAME ] )
               ENDIF
               nMode := D_ONELINE

               //  Now start writing out what we know
               IF nDocCnt > 60 .AND. .NOT. lContinuous
                  FWRITE( nWriteHandle, CHR( K_CTRL_L ) + CRLF )
                  nDocCnt := 0
               ENDIF

               FWRITE( nWriteHandle, IIF( lFunc, "FUNCTION: ", "COMMAND: " ) ;
                       + cFuncName + CRLF )
               FWRITE( nWriteHandle, " " + cOneLine + CRLF )
               FWRITE( nWriteHandle, cBar )
               nDocCnt += 3
               //  4) all other stuff

            ELSE

               IF AT( cSyn, cBuffer ) > 0

                  IF nDocCnt > 62 .AND. .NOT. lContinuous
                     FWRITE( nWriteHandle, CHR( K_CTRL_L ) + CRLF )
                     nDocCnt := 0
                  ENDIF
                  FWRITE( nWriteHandle, " Syntax:" + CRLF )
                  nDocCnt ++
                  nMode := D_SYNTAX

               ELSEIF AT( cArg, cBuffer ) > 0

                  IF nDocCnt > 62 .AND. .NOT. lContinuous
                     FWRITE( nWriteHandle, CHR( K_CTRL_L ) + CRLF )
                     nDocCnt := 0
                  ELSEIF !lBlankLine
                     FWRITE( nWriteHandle, CRLF )
                     nDocCnt ++
                  ENDIF
                  FWRITE( nWriteHandle, " Arguments:" + CRLF )
                  nDocCnt ++
                  nMode     := D_ARG
                  lAddBlank := .T.

               ELSEIF AT( cRet, cBuffer ) > 0

                  IF nDocCnt > 62 .AND. .NOT. lContinuous
                     FWRITE( nWriteHandle, CHR( K_CTRL_L ) + CRLF )
                     nDocCnt := 0
                  ELSEIF !lBlankLine
                     FWRITE( nWriteHandle, CRLF )
                     nDocCnt ++
                  ENDIF
                  FWRITE( nWriteHandle, " Returns:" + CRLF )
                  nDocCnt ++
                  nMode     := D_ARG
                  lAddBlank := .T.

               ELSEIF AT( cDesc, cBuffer ) > 0

                  IF nDocCnt > 62 .AND. .NOT. lContinuous
                     FWRITE( nWriteHandle, CHR( K_CTRL_L ) + CRLF )
                     nDocCnt := 0
                  ELSEIF !lBlankLine
                     FWRITE( nWriteHandle, CRLF )
                     nDocCnt ++
                  ENDIF

                  FWRITE( nWriteHandle, " Description:" + CRLF )
                  nDocCnt ++
                  nMode     := D_NORMAL
                  lAddBlank := .T.

               ELSEIF AT( cExam, cBuffer ) > 0

                  IF nDocCnt > 62 .AND. .NOT. lContinuous
                     FWRITE( nWriteHandle, CHR( K_CTRL_L ) + CRLF )
                     nDocCnt := 0
                  ELSEIF !lBlankLine
                     FWRITE( nWriteHandle, CRLF )
                     nDocCnt ++
                  ENDIF

                  FWRITE( nWriteHandle, " Examples:" + CRLF )
                  nDocCnt ++
                  nMode     := D_NORMAL
                  lAddBlank := .T.
               ELSEIF AT( cTest, cBuffer ) > 0

                  IF nDocCnt > 62 .AND. .NOT. lContinuous
                     FWRITE( nWriteHandle, CHR( K_CTRL_L ) + CRLF )
                     nDocCnt := 0
                  ELSEIF !lBlankLine
                     FWRITE( nWriteHandle, CRLF )
                     nDocCnt ++
                  ENDIF

                  FWRITE( nWriteHandle, " Tests:" + CRLF )
                  nDocCnt ++
                  nMode     := D_NORMAL
                  lAddBlank := .T.

               ELSEIF AT( cStatus, cBuffer ) > 0
                  IF nDocCnt > 62 .AND. .NOT. lContinuous
                     FWRITE( nWriteHandle, CHR( K_CTRL_L ) + CRLF )
                     nDocCnt := 0
                  ELSEIF !lBlankLine
                     FWRITE( nWriteHandle, CRLF )
                     nDocCnt ++
                  ENDIF

                  FWRITE( nWriteHandle, " Status:" + CRLF )
                  nDocCnt ++
                  nMode     := D_NORMAL
                  lAddBlank := .T.

               ELSEIF AT( cCompl, cBuffer ) > 0
                  IF nDocCnt > 62 .AND. .NOT. lContinuous
                     FWRITE( nWriteHandle, CHR( K_CTRL_L ) + CRLF )
                     nDocCnt := 0
                  ELSEIF !lBlankLine
                     FWRITE( nWriteHandle, CRLF )
                     nDocCnt ++
                  ENDIF

                  FWRITE( nWriteHandle, " Compliance:" + CRLF )
                  nDocCnt ++
                  nMode     := D_NORMAL
                  lAddBlank := .T.

               ELSEIF AT( cPlat, cBuffer ) > 0
                  IF nDocCnt > 62 .AND. .NOT. lContinuous
                     FWRITE( nWriteHandle, CHR( K_CTRL_L ) + CRLF )
                     nDocCnt := 0
                  ELSEIF !lBlankLine
                     FWRITE( nWriteHandle, CRLF )
                     nDocCnt ++
                  ENDIF

                  FWRITE( nWriteHandle, " Platforms:" + CRLF )
                  nDocCnt ++
                  nMode     := D_NORMAL
                  lAddBlank := .T.
               ELSEIF AT( cFiles, cBuffer ) > 0
                  IF nDocCnt > 62 .AND. .NOT. lContinuous
                     FWRITE( nWriteHandle, CHR( K_CTRL_L ) + CRLF )
                     nDocCnt := 0
                  ELSEIF !lBlankLine
                     FWRITE( nWriteHandle, CRLF )
                     nDocCnt ++
                  ENDIF

                  FWRITE( nWriteHandle, " Files:" + CRLF )
                  nDocCnt ++
                  nMode     := D_NORMAL
                  lAddBlank := .T.

               ELSEIF AT( cSee, cBuffer ) > 0
                  nMode := D_SEEALSO
               ELSEIF AT( cInc, cBuffer ) > 0
                  nMode := D_INCLUDE

                  //  All other input is trimmed of comments and sent out

               ELSE
                  //  translate any \$ into $
                  cBuffer := STRTRAN( cBuffer, "\" + DELIM, DELIM )
                  IF nMode = D_SYNTAX
                     IF LEN( cBuffer ) > LONGLINE
                        WRITE_ERROR( "Syntax", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     IF nDocCnt > 62 .AND. .NOT. lContinuous
                        FWRITE( nWriteHandle, CHR( K_CTRL_L ) + CRLF )
                        nDocCnt   := 0
                        lAddBlank := .F.
                     ELSEIF lAddBlank
                        FWRITE( nWriteHandle, CRLF )
                        lAddBlank := .F.
                        nDocCnt ++
                     ENDIF

                     lBlankLine := EMPTY( cBuffer )
                     FWRITE( nWriteHandle, " " + cBuffer + CRLF )
                     nDocCnt ++
                  ELSEIF nMode = D_ARG
                     IF LEN( cBuffer ) > LONGLINE
                        WRITE_ERROR( "Arguments", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     IF nDocCnt > 62 .AND. .NOT. lContinuous
                        FWRITE( nWriteHandle, CHR( K_CTRL_L ) + CRLF )
                        nDocCnt   := 0
                        lAddBlank := .F.
                     ELSEIF lAddBlank
                        FWRITE( nWriteHandle, CRLF )
                        lAddBlank := .F.
                        nDocCnt ++
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     FWRITE( nWriteHandle, cBuffer + CRLF )
                     nDocCnt ++
                  ELSEIF nMode = D_NORMAL
                     IF LEN( cBuffer ) > LONGLINE
                        WRITE_ERROR( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     IF nDocCnt > 62 .AND. .NOT. lContinuous
                        FWRITE( nWriteHandle, CHR( K_CTRL_L ) + CRLF )
                        nDocCnt   := 0
                        lAddBlank := .F.
                     ELSEIF lAddBlank
                        FWRITE( nWriteHandle, CRLF )
                        lAddBlank := .F.
                        nDocCnt ++
                     ENDIF
                     FWRITE( nWriteHandle, cBuffer + CRLF )
                     nDocCnt ++
                  ELSEIF nMode = D_SEEALSO
                     IF .NOT. EMPTY( cBuffer )
                        cSeeAlso := StripFiles( ALLTRIM( cBuffer ) )
                     ENDIF
                  ELSEIF nMode = D_INCLUDE
                     IF .NOT. EMPTY( cBuffer )
                        IF nDocCnt > 62 .AND. .NOT. lContinuous
                           FWRITE( nWriteHandle, CHR( K_CTRL_L ) + CRLF )
                           nDocCnt := 0
                        ELSEIF !lBlankLine
                           FWRITE( nWriteHandle, CRLF )
                           lAddBlank := .F.
                           nDocCnt ++
                        ENDIF
                        FWRITE( nWriteHandle, " Header File: " ;
                                + ALLTRIM( cBuffer ) + CRLF )
                        nDocCnt ++
                     ENDIF
                  ELSEIF nMode = D_STATUS
                     IF !EMPTY( cBuffer )
                        FWRITE( nWriteHandle, " Status" + CRLF )
                     ENDIF
                     ProcStatus( nWriteHandle, cBuffer )

                  ELSE
                     //  unknown data from somewhere

                     WRITE_ERROR( "Unknown Data Type " + cBuffer,, ;
                                  nLineCnt, ;
                                  LONGONELINE, aDirList[ i, F_NAME ] )

                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDDO
      //  Close down the input file

      FT_FUSE()
   NEXT

RETURN NIL

*+ EOF: GENASC.PRG
