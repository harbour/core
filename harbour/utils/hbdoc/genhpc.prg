/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * GENHPC support module for hbdoc document Extractor 
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
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
#define LONGLINE     78
#define LONGONELINE  66
#define CRLF HB_OSNewLine()
//  The delimiter
#define DELIM   "$"                 // keyword delimiter

#xtranslate UPPERLOWER(<exp>) => (UPPER(SUBSTR(<exp>,1,1))+LOWER(SUBSTR(<exp>,2)))
MEMVAR aDirList,aDocInfo,aWww
STATIC NWRITEHANDLE

FUNCTION ProcessFiles()

   //
   //  This routine and all accompaning database structures are
   //  Copyright (C) 1992 Leo J. Letendre. Modified to generate
   //   HELPC Source code By Luiz Rafael Culik
   //
   //  Purpose: Process each of the files in the directory
   //
   //  Modification History:
   //         Version    Date      Who       Notes
   //          V1.00     1/19/92   LJL       Initial Version
   //          V1.01     2/25/92   LJL       Trimmed spaces from see also and header
   //          V1.02     10/17/92  LJL       Added multi-line one-liner check
   //          V1.03     10/22/92  LJL       Insured that there was a blank line
   //                                        before and after each catagory header
   //          V1.04     10/30/93  LJL       added changable delimiter support and
   //                                        COMMANDNAME keyword
   //
   //  Calling parameters: None
   //
   //  Notes: None
   // -
   //  LOCAL variables:

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

   LOCAL i
   LOCAL j
   LOCAL nFiles      := LEN( aDirList )
   LOCAL nCommentLen
   LOCAL lEof
   LOCAL lDoc
   LOCAL cBuffer
   LOCAL nEnd
   LOCAL nCount

   LOCAL cBar       := "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" + CRLF
   LOCAL nMode
   LOCAL cFuncName
   LOCAL cOneLine
   LOCAL cCategory
   LOCAL cFileName
   LOCAL nLineCnt
   LOCAL cSeeAlso
   LOCAL cTemp
   LOCAL cChar
   LOCAL lBlankLine := .F.                 // Blank line encountered and sent out
   LOCAL lAddBlank  := .F.                 // Need to add a blank line if next line is not blank
   LOCAL nReadHandle
   LOCAL cDoc      := DELIM + "DOC" + DELIM                   // DOC keyword
   LOCAL cEnd      := DELIM + "END" + DELIM                   // END keyword
   LOCAL cFunc     := DELIM + "FUNCNAME" + DELIM              // FUNCNAME keyword
   LOCAL cCat      := DELIM + "CATEGORY" + DELIM              // CATEGORY keyword
   LOCAL cOne      := DELIM + "ONELINER" + DELIM              // ONELINER keyword
   LOCAL cSyn      := DELIM + "SYNTAX" + DELIM                // SYNTAX keyword
   LOCAL cArg      := DELIM + "ARGUMENTS" + DELIM             // ARGUMENTS keyword
   LOCAL cRet      := DELIM + "RETURNS" + DELIM               // RETURNS keyword
   LOCAL cDesc     := DELIM + "DESCRIPTION" + DELIM           // DESCRIPTION keyword
   LOCAL cExam     := DELIM + "EXAMPLES" + DELIM              // EXAMPLES keyword
   LOCAL cSee      := DELIM + "SEEALSO" + DELIM               // SEEALSO keyword
   LOCAL cInc      := DELIM + "INCLUDE" + DELIM               // INCLUDE keyword
   LOCAL cComm     := DELIM + "COMMANDNAME" + DELIM           // COMMAND keyword
   LOCAL cCompl    := DELIM + "COMPLIANCE" + DELIM
   LOCAL cTest     := DELIM + 'TESTS' + DELIM
   LOCAL cStatus   := DELIM + 'STATUS' + DELIM
   LOCAL cPlat     := DELIM + 'PLATFORMS' + DELIM
   LOCAL cFiles    := DELIM + 'FILES' + DELIM
   LOCAL cSubCode  := DELIM + 'SUBCODE' + DELIM
   LOCAL cFunction := DELIM + 'FUNCTION' +DELIM
   
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
         write_error( "Can't open file: (Dos Error " + STR( FERROR() ) + ")",,,, aDirList[ i, F_NAME ] )
         @ ERRORLINE,  0 CLEAR TO ERRORLINE, MAXCOL()
         @ ERRORLINE, 20 SAY "Can't open file: (Dos Error " + STR( FERROR() ) + ") File=" + aDirList[ i, F_NAME ]
         LOOP
      ENDIF
      lEof := .F.
      lDoc := .F.
      //  First find the author

      //  loop to go through file

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
               write_error( cDoc + " encountered during extraction of Doc" ;
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
               write_error( cEnd + " encountered outside of Doc area at line" ;
                            + STR( nLinecnt, 5, 0 ),,,, aDirList[ i, F_NAME ] )
            ELSE
               //  Add a new entry to our list of files

               IF EMPTY( cCategory )
                  write_error( "Blank Category",,,, aDirList[ i, F_NAME ] )
                  cCategory := "Unknown"
               ENDIF
               IF EMPTY( cFuncName )
                  write_error( "Blank Function Name",,,, aDirList[ i, F_NAME ] )
                  cFuncName := "Unknown"
               ENDIF
               AADD( aDocInfo, { cCategory, cFuncName, cOneLine, cFileName } )
               //  Now close down this little piece
               lDoc := .F.
               IF .NOT. EMPTY( cSeeAlso )
                  FWRITE( nWriteHandle, "See Also " )
                  Proccalso( nWriteHandle, cSeealso )
               ENDIF
               FWRITE( nWriteHandle, ".end-topic" + CRLF )
               FCLOSE( nWriteHandle )
               nMode := D_IGNORE
            ENDIF

            @ MODULELINE, 33 CLEAR TO MODULELINE, MAXCOL()
         ENDIF

         //  Act on the input
         IF lDoc
            //  1) function name

            IF AT( cFunc, cBuffer ) > 0 .OR. AT( cComm, cBuffer ) > 0 .OR. AT(cSubCode,cBuffer) > 0
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
                  write_error( "Error creating",,,, cFileName + ".hdf" )
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
                  write_error( "OneLine", cOneLine, nLineCnt, LONGONELINE, ;
                               aDirList[ i, F_NAME ] )
               ENDIF

               nMode := D_ONELINE
               //  Now start writing out what we know
               FWRITE( nWriteHandle, '.topic ' + CheckTop( PAD( cFuncName, 20 ) ) + CRLF )
               //                    FWRITE(nWriteHandle,"!short: "+PAD(cFuncName,17)+cOneLine+CRLF)
               FWRITE( nWriteHandle, '.title ' + cFuncName + CRLF )
               //                  FWRITE(nWriteHandle," "+cFuncName+CRLF)
               FWRITE( nWriteHandle, ".par font 6 size 12 align center " + CRLF + cOneLine + CRLF + ".endpar" + CRLF )
               FWRITE( nWriteHandle, HB_OEMTOANSI( cBar ) )
               //  4) all other stuff

            ELSE

               IF AT( cSyn, cBuffer ) > 0
                  FWRITE( nWriteHandle, ".par bold on" + CRLF )
                  FWRITE( nWriteHandle, " Syntax" + CRLF )
                  FWRITE( nWriteHandle, ".endpar" + CRLF )
                  nMode     := D_SYNTAX
                  lAddBlank := .T.

               ELSEIF AT( cArg, cBuffer ) > 0

                  IF !lBlankLine
                     FWRITE( nWriteHandle, ".par bold on" + CRLF )
                     FWRITE( nWriteHandle, " Arguments" + CRLF )
                     FWRITE( nWriteHandle, ".endpar" + CRLF )
                  ENDIF

                  nMode     := D_ARG
                  lAddBlank := .T.

               ELSEIF AT( cRet, cBuffer ) > 0

                  IF !lBlankLine
                     FWRITE( nWriteHandle, CRLF )
                  ENDIF
                  FWRITE( nWriteHandle, ".par bold on" + CRLF )
                  FWRITE( nWriteHandle, " Returns" + CRLF )
                  FWRITE( nWriteHandle, ".endpar" + CRLF )
                  nMode     := D_ARG
                  lAddBlank := .T.

               ELSEIF AT( cDesc, cBuffer ) > 0

                  IF !lBlankLine
                     FWRITE( nWriteHandle, CRLF )
                  ENDIF
                  FWRITE( nWriteHandle, ".par bold on" + CRLF )

                  FWRITE( nWriteHandle, " Description" + CRLF )
                  FWRITE( nWriteHandle, ".endpar" + CRLF )
                  nMode     := D_NORMAL
                  lAddBlank := .T.

               ELSEIF AT( cExam, cBuffer ) > 0

                  IF !lBlankLine
                     FWRITE( nWriteHandle, CRLF )
                  ENDIF

                  FWRITE( nWriteHandle, ".par bold on" + CRLF )
                  FWRITE( nWriteHandle, " Examples" + CRLF )
                  FWRITE( nWriteHandle, ".endpar" + CRLF )

                  nMode     := D_NORMAL
                  lAddBlank := .T.
               ELSEIF AT( cTest, cBuffer ) > 0
                      nMode=D_TESTS
               ELSEIF AT( cCompl, cBuffer ) > 0

                  IF !lBlankLine
                     FWRITE( nWriteHandle, CRLF )
                  ENDIF
                  FWRITE( nWriteHandle, ".par bold on" + CRLF )
                  FWRITE( nWriteHandle, " Compliance" + CRLF )

                  FWRITE( nWriteHandle, ".endpar" + CRLF )

                  nMode     := D_NORMAL
                  lAddBlank := .T.
               ELSEIF AT( cPlat, cBuffer ) > 0

                  IF !lBlankLine
                     FWRITE( nWriteHandle, CRLF )
                  ENDIF
                  FWRITE( nWriteHandle, ".par bold on" + CRLF )
                  FWRITE( nWriteHandle, " Platforms" + CRLF )
                  FWRITE( nWriteHandle, ".endpar" + CRLF )

                  nMode     := D_NORMAL
                  lAddBlank := .T.
               ELSEIF AT( cFiles, cBuffer ) > 0

                  IF !lBlankLine
                     FWRITE( nWriteHandle, CRLF )
                  ENDIF
                  FWRITE( nWriteHandle, ".par bold on" + CRLF )
                  FWRITE( nWriteHandle, " Files" + CRLF )
                  FWRITE( nWriteHandle, ".endpar" + CRLF )

                  nMode     := D_NORMAL
                  lAddBlank := .T.
               ELSEIF AT( cFunction, cBuffer ) > 0

                  IF !lBlankLine
                     FWRITE( nWriteHandle, CRLF )
                  ENDIF
                  FWRITE( nWriteHandle, ".par bold on" + CRLF )
                  FWRITE( nWriteHandle, " Function" + CRLF )
                  FWRITE( nWriteHandle, ".endpar" + CRLF )

                  nMode     := D_NORMAL
                  lAddBlank := .T.

               ELSEIF AT( cStatus, cBuffer ) > 0
                  nMode := D_STATUS
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
                        write_error( "Syntax", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     IF lAddBlank
                        FWRITE( nWriteHandle, CRLF )
                        lAddBlank := .F.
                     ENDIF
                     FWRITE( nWriteHandle, cBuffer + CRLF )
                  ELSEIF nMode = D_ARG
                     IF LEN( cBuffer ) > LONGLINE
                        write_error( "Arguments", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     IF lAddBlank
                        FWRITE( nWriteHandle, CRLF )
                        lAddBlank := .F.
                     ENDIF
                     cBuffer := STRTRAN( cBuffer, "<", "<", 1 )
                     cBuffer := STRTRAN( cBuffer, ">", ">", 1 )
                     FWRITE( nWriteHandle, cBuffer + CRLF )
                  ELSEIF nMode = D_NORMAL
                     IF LEN( cBuffer ) > LONGLINE
                        write_error( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     IF lAddBlank
                        FWRITE( nWriteHandle, CRLF )
                        lAddBlank := .F.
                     ENDIF
                     FWRITE( nWriteHandle, StripNgControls( cBuffer ) + CRLF )
                  ELSEIF nMode = D_SEEALSO
                     IF .NOT. EMPTY( cBuffer )
                        cSeeAlso := StripFiles( ALLTRIM( cBuffer ) )
                     ENDIF
                  ELSEIF nMode = D_INCLUDE
                     //  read next line
                     IF .NOT. EMPTY( cBuffer )
                        IF !lBlankLine
                           FWRITE( nWriteHandle, CRLF )
                        ENDIF
                        FWRITE( nWriteHandle, " Header File: " ;
                                + ALLTRIM( cBuffer ) + CRLF )
                     ENDIF
                  ELSEIF nMode = D_STATUS
                     IF !EMPTY( cBuffer )
                        FWRITE( nWriteHandle, ".par bold on" + CRLF )
                        FWRITE( nWriteHandle, " Status" + CRLF )
                        FWRITE( nWriteHandle, ".endpar" + CRLF )
                     ENDIF
                     ProcStatus( nWriteHandle, StripNgControls( cBuffer ) )
                  ELSEIF    nMode=D_TESTS
                     IF !EMPTY(cBuffer)
                          FWRITE( nWriteHandle, ".par bold on" + CRLF )
                          FWRITE( nWriteHandle, " Tests" + CRLF )
                          FWRITE( nWriteHandle, ".endpar" + CRLF )
                          FWRITE( nWriteHandle, CRLF )

                  ENDIF
                          FWRITE( nWriteHandle,StripNgControls( cBuffer ) +CRLF)
                  ELSE

                     //  unknown data from somewhere

                     write_error( "Unknown Data Type " + cBuffer,, ;
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
#define CRLF chr(13)+chr(10)

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function Proccalso()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION Proccalso( nWriteHandle, cSeeAlso )

   LOCAL nPos
   LOCAL cTemp := ''
   LOCAL xTemp
   LOCAL nLen
   LOCAL xPos
   LOCAL tPos
   nLen := LEN( cSeeAlso )
   WHILE .t.
      nPos := AT( ",", cSeeAlso )

      IF nPos > 0
         xTemp := SUBSTR( ALLTRIM( cSeeAlso ), 1, nPos - 1 )
         tPos  := AT( "()", xTemp )
         IF tPos > 0
            nLen -= LEN( xTemp ) + 1

            xTemp += "~" + xTemp + "~ "
            cTemp := "~" + xTemp
         ELSE
            xPos := AT( " ", xTemp )
            IF xPos > 0
               nLen  -= LEN( xTemp ) + 3
               xTemp += "~" + SUBSTR( xTemp, 1, xPos - 1 ) + '_' + SUBSTR( xTemp, xPos + 1 ) + "~ "
               cTemp := "~" + xTemp
            ELSE
               nLen  -= LEN( xTemp ) + 2
               xTemp += "~" + xTemp + "~ "
               cTemp := "~" + xTemp
            END

         END
      ELSE
         xTemp := SUBSTR( cSeeAlso, 1 )
         tPos  := AT( "()", xTemp )

         IF tPos > 0
            nLen -= LEN( xTemp ) + 1

            xTemp += "~" + xTemp + "~ "
            cTemp := "~" + xTemp
         ELSE

            xPos := AT( " ", xTemp )
            IF xPos > 0
               nLen  -= LEN( xTemp ) + 3
               xTemp += "~" + SUBSTR( xTemp, 1, xPos - 1 ) + '_' + SUBSTR( xTemp, xPos + 1 ) + "~"
               cTemp := "~" + xTemp
            ELSE
               nLen  -= LEN( xTemp ) + 2
               xTemp += "~" + xTemp + "~ "
               cTemp := "~" + xTemp
            END
         END

      ENDIF
      FWRITE( nWriteHandle, ALLTRIM( cTemp ) + CRLF )
      cSeeAlso := SUBSTR( cSeeAlso, nPos + 1 )

      IF nLen == 0 .OR. nLen < 0
         EXIT
      END
   ENDDO
   RETURN nil

FUNCTION ProcStatus( nWriteHandle, cBuffer )
   IF LEN( ALLTRIM( cBuffer ) ) > 1
      FWRITE( nWriteHandle, cBuffer + CRLF )
   ELSEIF SUBSTR( ALLTRIM( cBuffer ), 1 ) == "R"
      FWRITE( nWriteHandle, "   Ready" + CRLF )
   ELSEIF SUBSTR( ALLTRIM( cBuffer ), 1 ) == "S"
      FWRITE( nWriteHandle, "   Started" + CRLF )
   ELSE
      FWRITE( nWriteHandle, "   Not Started" + CRLF )
   ENDIF
RETURN nil

FUNCTION CheckTop( cTop )

   LOCAL nPos
   LOCAL cTemp
   cTop := RTRIM( cTop )
   nPos := AT( " ", cTop )
   IF nPos > 0
      cTemp := SUBSTR( cTop, 1, nPos - 1 ) + '_' + SUBSTR( cTop, nPos + 1 )
   ELSE
      cTemp := cTop
   ENDIF
RETURN cTemp
