/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * GENRTF support module for hbdoc document Extractor
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
 */

#pragma -w2
#pragma linenumber=on

#include "directry.ch"
#include "fileio.ch"
#include "inkey.ch"
#include "common.ch"

#include "hbdocdef.ch"

#include "hbclass.ch"

#define CRLF HB_OSNewLine()
#xtranslate UPPERLOWER(<exp>) => (UPPER(SUBSTR(<exp>,1,1))+LOWER(SUBSTR(<exp>,2)))

#define DELIM   "$"                 // keyword delimiter

//  output lines on the screen
#define INFILELINE   10
#define MODULELINE   12
#define LINELINE     14
#define ERRORLINE    20
#define LONGLINE     600
#define LONGONELINE  86

MEMVAR aDirList
MEMVAR aDocinfo
MEMVAR aWWW
MEMVAR aResult

STATIC aFiTable    := {}
STATIC lIsTable    := .F.
STATIC nCommentLen
STATIC lEof
STATIC aCurDoc     := {}
STATIC nCurDoc     := 1

STATIC aColorTable := { { "aqua", "\cf2 " }, { "black", "\cf1 " }, { "fuchia", "\cf3 " }, { "grey", "\cf4 " }, { "green", "\cf5 " }, { "lime", "\cf6 " }, { "maroon", "\cf7 " }, { "navy", "\cf8 " }, { "olive", "\cf9 " }, { "purple", "\cf10 " }, { "red", "\cf11 " }, { "silver", "\cf12 " }, { "teal", "\cf13 " }, { "white", "\cf14 " }, { "yellow", "\cf15 " } }

*+--------------------------------------------------------------------
*+
*+    Function ProcessRtf()
*+
*+    Called from ( hbdoc.prg    )   2 - function main()
*+
*+--------------------------------------------------------------------
*+
FUNCTION ProcessRtf()

   //
   //  Copyright (C) 2000 Luiz Rafael Culik
   //
   //  Purpose: Process each of the files in the directory
   //
   //  Modification History:
   //         Version    Date        Who       Notes
   //          V1.00     1/08/2000   LRC       Initial Version
   //
   //  Calling parameters: None
   //
   //  Notes: None
   // -
   //  LOCAL variables:

   LOCAL i
   LOCAL j
   LOCAL nFiles         := LEN( aDirList )
   LOCAL lDoc
   LOCAL cBuffer
   LOCAL nEnd
   LOCAL nCount
   LOCAL xAddBlank
   LOCAL nNumTopics     := 0
   LOCAL nCurTopics     := 1
   LOCAL cBar           := " " + replicate( ")", 80 )
   LOCAL nMode
   LOCAL cFuncName
   LOCAL cOneLine
   LOCAL cCategory
   LOCAL cFileName
   LOCAL nLineCnt
   LOCAL cSeeAlso
   LOCAL cTemp
   LOCAL cTempx
   LOCAL cChar
   LOCAL lBlankLine     := .F.             // Blank line encountered and sent out
   LOCAL lAddBlank      := .F.             // Need to add a blank line if next line is not blank
   LOCAL lFirstArg      := .T.
   LOCAL lData          := .F.
   LOCAL lMethod        := .F.
   LOCAL oRtf
   LOCAL nReadHandle
   LOCAL lPar
   LOCAL lWrite         := .F.
   LOCAL lWasLine       := .F.
   LOCAL lIsDataLink    := .F.
   LOCAL lIsMethodLink  := .F.
   LOCAL cName
   LOCAL cDoc           := DELIM + "DOC" + DELIM               // DOC keyword
   LOCAL cEnd           := DELIM + "END" + DELIM               // END keyword
   LOCAL cFunc          := DELIM + "FUNCNAME" + DELIM          // FUNCNAME keyword
   LOCAL cCat           := DELIM + "CATEGORY" + DELIM          // CATEGORY keyword
   LOCAL cOne           := DELIM + "ONELINER" + DELIM          // ONELINER keyword
   LOCAL cSyn           := DELIM + "SYNTAX" + DELIM            // SYNTAX keyword
   LOCAL cArg           := DELIM + "ARGUMENTS" + DELIM         // ARGUMENTS keyword
   LOCAL cRet           := DELIM + "RETURNS" + DELIM           // RETURNS keyword
   LOCAL cDesc          := DELIM + "DESCRIPTION" + DELIM       // DESCRIPTION keyword
   LOCAL cExam          := DELIM + "EXAMPLES" + DELIM          // EXAMPLES keyword
   LOCAL cSee           := DELIM + "SEEALSO" + DELIM           // SEEALSO keyword
   LOCAL cInc           := DELIM + "INCLUDE" + DELIM           // INCLUDE keyword
   LOCAL cComm          := DELIM + "COMMANDNAME" + DELIM       // COMMAND keyword
   LOCAL cCompl         := DELIM + "COMPLIANCE" + DELIM
   LOCAL cTest          := DELIM + "TESTS" + DELIM
   LOCAL cStatus        := DELIM + "STATUS" + DELIM
   LOCAL cPlat          := DELIM + "PLATFORMS" + DELIM
   LOCAL cFiles         := DELIM + "FILES" + DELIM
   LOCAL cSubCode       := DELIM + "SUBCODE" + DELIM
   LOCAL cFunction      := DELIM + "FUNCTION" + DELIM
   LOCAL cConstruct     := DELIM + "CONSTRUCTOR" + DELIM
   LOCAL cDatalink      := DELIM + "DATALINK" + DELIM
   LOCAL cDatanolink    := DELIM + "DATANOLINK" + DELIM
   LOCAL cMethodslink   := DELIM + "METHODSLINK" + DELIM
   LOCAL cMethodsNolink := DELIM + "METHODSNOLINK" + DELIM
   LOCAL cData          := DELIM + "DATA" + DELIM
   LOCAL cMethod        := DELIM + "METHOD" + DELIM
   LOCAL cClassDoc      := DELIM + "CLASSDOC" + DELIM
   LOCAL cTable         := DELIM + "TABLE" + DELIM
   Local aAlso:={}
   lFirstArg     := .T.
   lData         := .F.
   lMethod       := .F.
   lIsDataLink   := .F.
   lIsMethodLink := .F.

   lWrite := .F.
   cTempx := ""
   //
   //  Entry Point
   //
   //  Put up information labels
   @ INFILELINE, 20 SAY "Extracting: "
   @ MODULELINE, 20 SAY "Documenting: "
   //  loop through all of the files
   oRtf := tRtf():new( "rtf\harbour.rtf" ):WriteHeader()
   FOR i := 1 TO nFiles

      //  Open file for input

      nCommentLen := IIF( AT( ".asm", Lower( aDirList[ i, F_NAME ] ) ) > 0, 2, 4 )
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
      lEof    := .F.
      lDoc    := .F.
      lData   := .F.
      lMethod := .F.
      lPar    := .T.
      //  First find the author
      ReadFromTop( nReadHandle )
      DO WHILE .NOT. lEof

         //  Read a line

         cBuffer := TRIM( SUBSTR( ReadLN( @lEof ), nCommentLen ) )
         nLineCnt ++
         IF nLineCnt % 10 == 0
            @ LINELINE, 33 SAY STR( nLineCnt, 5, 0 )
         ENDIF
         //  check to see if we are in doc mode or getting out of doc mode

         IF AT( cDoc, cBuffer ) > 0 .OR. AT( cClassDoc, cBuffer ) > 0
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
            nCurDoc ++
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
               IF .NOT. EMPTY( cSeeAlso )
                  oRtf:WritePar( "" )   //:endpar()
                  oRtf:WriteParBold( "See Also" )
                  oRtf:WritePar( "" )   //:endpar()
                  aAlso:=ListAsArray2(cSeeAlso,",")
//                  if Len(aAlso) <3
//                    ProcRtfalso( oRtf, cSeealso )
//                    oRtf:WriteKLink(aAlso,.F.)
//                  else
                  oRtf:WriteKLink(aAlso)
//                  endif
               ENDIF
               lDoc := .F.

               oRtf:EndPage()

               nMode := D_IGNORE
            ENDIF

            @ MODULELINE, 33 CLEAR TO MODULELINE, MAXCOL()
         ENDIF

         //  Act on the input
         IF lDoc
            IF AT( cFunc, cBuffer ) > 0 .OR. AT( cComm, cBuffer ) > 0 .OR. AT( cSubCode, cBuffer ) > 0
               cBuffer := ReadLN( @lEof )
               nLineCnt ++
               //  Save the function name
               cFuncName :=  ALLTRIM( SUBSTR( cBuffer, nCommentLen ) )
               @ MODULELINE, 33 CLEAR TO MODULELINE, MAXCOL()
               @ MODULELINE, 33 SAY cFuncName

               nMode := D_NORMAL

               //  Open a new file
               IF AT( "FT_", cFuncName ) > 0
                  cTemp := upper(SUBSTR( cFuncName, 4 ))
               ELSE
                  cTemp := upper(cFuncName)
               ENDIF

               IF ( nEnd := AT( "(", cTemp ) ) > 0
                  cTemp := LEFT( cTemp, nEnd - 1 )
               ENDIF
               cFileName := ""

               //  Strip off any other non-alphabetic/numeric characters
               FOR j := 1 TO LEN( cTemp )
                  cChar := SUBSTR( cTemp, j, 1 )
                  IF ( cChar >= "0" .AND. cChar <= "9" ) .OR. ;
                       ( cChar >= "A" .AND. cChar <= "Z" ) .OR. cChar == "_"
                     cFileName += cChar
                  ENDIF
               NEXT

               //  See if file name is present already. If so then modify

               cFileName := LEFT( cFileName, 21 )
               nEnd      := 1
               nCount    := 0
               DO WHILE nEnd > 0
                  nEnd := ASCAN( aDocInfo, { | a | a[ 4 ] == cFileName + ".rtf" } )
                  IF nEnd > 0

                     //  This will break if there are more than 10 files with the same first
                     //  seven characters. We take our chances.

                     IF LEN( cFileName ) == 40
                        cFileName := STUFF( cFileName, 40, 1, STR( nCount, 1, 0 ) )
                     ELSE
                        cFileName += STR( nCount, 1, 0 )
                     ENDIF
                     nCount ++
                  ENDIF
               ENDDO
               //  Add on the extension

               cFileName := LEFT( cFileName, 40 ) + ".rtf"

               IF oRtf:nHandle < 1
                  ? "Error creating", cFileName, ".rtf"
                  WRITE_ERROR( "Error creating",,,, cFileName + ".rtf" )
               ENDIF
            ELSEIF ( AT( cdata, cBuffer ) > 0 .AND. GetItem( cBuffer, nCurdoc ) ) .OR. ( AT( cmethod, cBuffer ) > 0 .AND. GetItem( cBuffer, nCurdoc ) )
               IF AT( cdata, cBuffer ) > 0
                  lData   := .T.
                  lMethod := .F.
               ELSEIF AT( cmethod, cBuffer ) > 0
                  lMethod := .T.
                  lData   := .F.
               ENDIF
               cBuffer := ReadLN( @lEof )
               nLineCnt ++
               //  Save the function name
               cFuncName :=  ALLTRIM( SUBSTR( cBuffer, nCommentLen ) )
               @ MODULELINE, 33 CLEAR TO MODULELINE, MAXCOL()
               @ MODULELINE, 33 SAY cFuncName

               nMode := D_NORMAL

               //  2) Category

               //  2) Category
            ELSEIF AT( cCat, cBuffer ) > 0
               cBuffer := ReadLN( @lEof )
               nLineCnt ++
               //  get the category
               cCategory :=  ALLTRIM( SUBSTR( cBuffer, nCommentLen ) )

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
               IF lData
                  oRtf:WriteJumpTitle( LEFT( cFilename, AT( ".", cFilename ) - 1 ) + cFuncName, "Data " + cFuncName )
               ELSEIF lMethod
                  oRtf:WriteJumpTitle( LEFT( cFilename, AT( ".", cFilename ) - 1 ) + cFuncName, LEFT( cFilename, AT( ".", cFilename ) - 1 )  + ":"+cFuncName )
               ELSE
                  oRtf:WriteTitle(  cFuncName, cFuncName, cOneLine,cCategory)
                  //               oRtf:WriteParBold( cOneLine )
                  //               oRtf:WriteParBox(  cBar  )
               ENDIF
               //  4) all other stuff

            ELSE

               IF AT( cSyn, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     IF !lBlankLine

                        oRtf:WritePar( "" )

                     ENDIF

                     oRtf:WriteParBold( " Syntax" )
                     oRtf:WritePar( "" )                    //:endpar()
                     nMode := D_SYNTAX
                     //                  oRtf:WritePar("") //:endpar()
                     lAddBlank := .T.
                  END
               ELSEIF AT( cConstruct, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     IF !lBlankLine

                        oRtf:WritePar( "" )

                     ENDIF

                     oRtf:WriteParBold( " Constructor syntax" )
                        oRtf:WritePar( "" )
                     nMode     := D_SYNTAX
                     lAddBlank := .T.

                  END
               ELSEIF AT( cArg, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     IF !lBlankLine

                        oRtf:WritePar( "" )

                     ENDIF
                     oRtf:WriteParBold( " Arguments" )
                     oRtf:WritePar( "" )                    //:endpar()
                     nMode     := D_ARG
                     lAddBlank := .T.
                     lPar      := .T.
                  END
               ELSEIF AT( cRet, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     IF !lBlankLine
                        oRtf:WritePar( "" )                 //:endpar()
                     ENDIF

                     oRtf:WriteParBold( " Returns" )
                     oRtf:WritePar( "" )                    //:endpar()
                     nMode     := D_RETURN
                     lAddBlank := .T.
                     lPar      := .T.
                  END
               ELSEIF AT( cDesc, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     oRtf:WritePar( "" )                    //:endpar()
                     oRtf:WriteParBold( " Description" )
                     oRtf:WritePar( "" )                    //:endpar()
                     nMode     := D_DESCRIPTION
                     lAddBlank := .T.
                     lPar      := .T.
                  END
               ELSEIF AT( cTable, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     nMode     := D_EXAMPLE
                     lAddBlank := .T.

                  END
               ELSEIF AT( cdatalink, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     IF !lBlankLine
                        oRtf:WritePar( "" )                 //:endpar()
                     ENDIF
                    oRtf:WritePar( "" )                 //:endpar()
                     oRtf:WriteParBold( " Data" )
                    oRtf:WritePar( "" )                 //:endpar()
                     nMode     := D_DATALINK
                     lAddBlank := .T.

                     lIsDataLink := .T.
                  END
               ELSEIF AT( cDatanolink, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     IF !lIsDataLink
                     oRtf:WritePar( "" )                 //:endpar()
                        oRtf:WriteParBold( " Data" )
//                     oRtf:WritePar( "" )                 //:endpar()
                     ENDIF
                     nMode     := D_NORMAL
                     lAddBlank := .T.

                     lPar := .T.
                  END
               ELSEIF AT( cMethodslink, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                  oRtf:WritePar( "" )                 //:endpar()
                     oRtf:WriteParBold( " Method" )
oRtf:WritePar( "" )                 //:endpar()
                     nMode     := D_METHODLINK
                     lAddBlank := .T.

                     lIsMethodLink := .T.
                  END
               ELSEIF AT( cMethodsnolink, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     IF !lIsMethodLink
                     oRtf:WritePar( "" )                 //:endpar()
                        oRtf:WriteParBold( " Methods" )
                        oRtf:WritePar( "" )                 //:endpar()
                     ENDIF
                        oRtf:WritePar( "" )                 //:endpar()
                     nMode     := D_NORMAL
                     lAddBlank := .T.
                     lPar      := .T.

                  END
               ELSEIF AT( cExam, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )

                     IF !lBlankLine
                        //ortf:endpar()
                        oRtf:WritePar("") //:endpar()
                        oRtf:WriteParBold( " Examples" )
                     ENDIF

                     nMode     := D_EXAMPLE
                     lAddBlank := .T.
                  END
               ELSEIF AT( cTest, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     IF !lBlankLine
                                                              oRtf:WritePar("") //:endpar()
                       oRtf:WriteParBold( " Tests" )
                        oRtf:WritePar( "" )                 //:endpar()
                     ENDIF

                     nMode     := D_EXAMPLE
                     lAddBlank := .T.
                     lPar      := .T.
                  END
               ELSEIF AT( cStatus, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     nMode := D_STATUS
                  END
               ELSEIF AT( cCompl, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     IF !lBlankLine
                        //ortf:endpar()
                        oRtf:WritePar( "" )                 //:endpar()
                        oRtf:WriteParBold( " Compliance" )
                        oRtf:WritePar( "" )                 //:endpar()
                     ENDIF

                     nMode     := D_COMPLIANCE
                     lAddBlank := .T.
                     lPar      := .T.
                  END
               ELSEIF AT( cPlat, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     IF !lBlankLine
                        //ortf:endpar()
                        oRtf:WritePar( "" )                 //:endpar()
                        oRtf:WriteParBold( " Platforms" )
                        oRtf:WritePar( "" )                 //:endpar()
                     ENDIF

                     nMode     := D_NORMAL
                     lAddBlank := .T.
                     lPar      := .T.
                  END
               ELSEIF AT( cFiles, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     IF !lBlankLine
                        oRtf:WritePar( "" )                 //:endpar()
                        oRtf:WriteParBold( " Files" )
                        oRtf:WritePar( "" )                 //:endpar()
                     ENDIF

                     lPar      := .T.
                     nMode     := D_NORMAL
                     lAddBlank := .T.
                  END
               ELSEIF AT( cFunction, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     IF !lBlankLine
                        oRtf:WritePar( "" )                 //:endpar()
                        oRtf:WriteParBold( " Functions" )
                        oRtf:WritePar( "" )                 //:endpar()
                     ENDIF

                     nMode     := D_NORMAL
                     lAddBlank := .T.
                     lPar      := .T.
                  END
               ELSEIF AT( cSee, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     nMode := D_SEEALSO
                  END
               ELSEIF AT( cInc, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     nMode := D_INCLUDE
                  END
                  //  All other input is trimmed of comments and sent out

               ELSE
                  //  translate any \$ into $
                  cBuffer := STRTRAN( cBuffer, "\" + DELIM, DELIM )
                  IF nMode == D_SYNTAX
                     IF LEN( cBuffer ) > LONGLINE
                        WRITE_ERROR( "Syntax", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     IF AT( "<par>", cBuffer ) > 0
                        STRTRAN( cBuffer, "<par>", "" )
                        STRTRAN( cBuffer, "</par>", "" )
                        cBuffer := ALLTRIM( cBuffer )
                        cbuFfer := "<par><b>" + cBuffer + "</b></par>"
                     ENDIF
                     procrtfdesc( cbuffer, oRtf, "Syntax" )
                     //                      oRtf:WritePar("") //:endpar()
                  ELSEIF nMode == D_RETURN

                     IF LEN( cBuffer ) > LONGLINE
                        WRITE_ERROR( "Arguments", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )

                     procrtfdesc( cbuffer, oRtf, "Arguments" )

                  ELSEIF nMode == D_ARG

                     IF LEN( cBuffer ) > LONGLINE
                        WRITE_ERROR( "Arguments", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )

                     procrtfdesc( cbuffer, oRtf, "Arguments" )
                  ELSEIF nMode == D_DATALINK
                     IF LEN( cBuffer ) > LONGLINE
                        WRITE_ERROR( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     IF lAddBlank
                        lAddBlank := .F.
                     ENDIF
                     cTemp   := SUBSTR( cBuffer, 1, AT( ":", cBuffer ) - 1 )
                     cBuffer := SUBSTR( cBuffer, AT( ":", cBuffer ) + 1 )
                     oRtf:WriteJumpLink1( LEFT( cfilename, AT( ".", cFilename ) - 1 ) + ALLTRIM( cTemp ), cTemp, cBuffer )
                  ELSEIF nMode == D_METHODLINK
                     IF LEN( cBuffer ) > LONGLINE
                        WRITE_ERROR( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     IF lAddBlank
                        lAddBlank := .F.
                     ENDIF
                     cTemp   := SUBSTR( cBuffer, 1, AT( "()", cBuffer ) + 1 )
                     cName   := SUBSTR( cBuffer, 1, AT( "()", cBuffer ) - 1 )
                     cBuffer := SUBSTR( cBuffer, AT( "()", cBuffer ) + 2 )
                     oRtf:WriteJumpLink( LEFT( cfilename, AT( ".", cFilename ) - 1 ) + ALLTRIM( cTemp ),ALLTRIM( cTemp ), cBuffer )

                  ELSEIF nMode == D_NORMAL
                     IF LEN( cBuffer ) > LONGLINE
                        WRITE_ERROR( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     procrtfdesc( cBuffer, oRtf )
                  ELSEIF nMode == D_COMPLIANCE
                     IF LEN( cBuffer ) > LONGLINE
                        WRITE_ERROR( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     procrtfdesc( cBuffer, oRtf, "Compliance" )

                  ELSEIF nMode == D_DESCRIPTION
                     IF LEN( cBuffer ) > LONGLINE
                        WRITE_ERROR( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     procrtfdesc( cBuffer, oRtf, "Description" )

                  ELSEIF nMode == D_EXAMPLE
                     IF LEN( cBuffer ) > LONGLINE
                        WRITE_ERROR( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )

                     IF lAddBlank
                        oRtf:WritePar( "" )                 //:endpar()
                        lAddBlank := .F.
                     ENDIF

                     procrtfdesc( cBuffer, oRtf, "Example" )
                  ELSEIF nMode == D_SEEALSO
                     IF .NOT. EMPTY( cBuffer )
                        cSeeAlso := StripFiles( ALLTRIM( cBuffer ) )
                     ENDIF
                  ELSEIF nMode == D_INCLUDE
                     //  read next line
                     IF .NOT. EMPTY( cBuffer )
                        IF !lBlankLine
                           oRtf:WritePar( "" )              //:endpar()
                        ENDIF
                     ENDIF
                  ELSEIF nMode == D_STATUS
                     IF !EMPTY( cBuffer )
                        oRtf:WritePar( "" )                 //:endpar()
                        oRtf:WriteParBold( "Status" )
                        oRtf:WritePar( "" )                 //:endpar()
                        xaddblank := .T.
                     ELSE
                        oRtf:WritePar( "" )                 //:endpar()
                        xAddBlank := .T.
                     ENDIF
                     procrtfstatus( oRtf, cBuffer )
                  ELSE

                     //  unknown data from somewhere

                     WRITE_ERROR( "Unknown Data Type " + cBuffer,, ;
                                  nLineCnt, ;
                                  LONGONELINE, aDirList[ i, F_NAME ] )

                  ENDIF
               ENDIF
            ENDIF
            //            ENDIF

         ENDIF

      ENDDO
      //  Close down the input file
      FT_FUSE()
      nCurDoc := 1
      aCurDoc := {}

   NEXT
   ortf:close()

RETURN oRtf:aIdh

*+--------------------------------------------------------------------
*+
*+    Function ProcRtfAlso()
*+
*+    Called from ( genrtf.prg   )   1 - function processrtf()
*+
*+--------------------------------------------------------------------
*+
FUNCTION ProcRtfAlso( nWriteHandle, cSeeAlso )

   LOCAL nPos
   LOCAL cTemp := ""
   LOCAL nLen
   LOCAL xPos
   LOCAL xTemp
   LOCAL tPos
   nLen := LEN( cSeeAlso )
   WHILE .T.
      nPos := AT( ",", cSeeAlso )

      IF nPos > 0
         xTemp := SUBSTR( ALLTRIM( cSeeAlso ), 1, nPos - 1 )
         tPos  := AT( "()", xTemp )
         IF tPos > 0
            nLen -= LEN( xTemp ) + 1

            cTemp := xTemp
         ELSE
            xPos := AT( " ", xTemp )
            IF xPos > 0
               nLen -= LEN( xTemp ) + 3

               cTemp := xTemp
            ELSE
               nLen -= LEN( xTemp ) + 2

               cTemp := xTemp
            END

         END
      ELSE
         xTemp := SUBSTR( cSeeAlso, 1 )
         tPos  := AT( "()", xTemp )

         IF tPos > 0
            nLen -= LEN( xTemp ) + 1

            cTemp := xTemp
         ELSE

            xPos := AT( " ", xTemp )
            IF xPos > 0
               nLen -= LEN( xTemp ) + 3

               cTemp := xTemp
            ELSE
               nLen -= LEN( xTemp ) + 2

               cTemp := xTemp
            END
         END

      ENDIF

      nWriteHandle:WriteLink( ALLTRIM( cTemp ) )
      cSeeAlso := SUBSTR( cSeeAlso, nPos + 1 )

      IF nLen == 0 .OR. nLen < 0
         EXIT
      END
   ENDDO
RETURN nil

*+--------------------------------------------------------------------
*+
*+    Function procrtfstatus()
*+
*+    Called from ( genrtf.prg   )   1 - function processrtf()
*+
*+--------------------------------------------------------------------
*+
FUNCTION procrtfstatus( nWriteHandle, cBuffer )

   IF LEN( ALLTRIM( cBuffer ) ) > 1
      nWriteHandle:WritePar( cBuffer )  //:endpar()
   ELSEIF SUBSTR( ALLTRIM( cBuffer ), 1 ) == "R"
      nWriteHandle:WritePar( "      Ready" )                //:endpar()
   ELSEIF SUBSTR( ALLTRIM( cBuffer ), 1 ) == "S"
      nWriteHandle:WritePar( "      Started" )              //:endpar()
   ELSE
      nWriteHandle:WritePar( "      Not Started" )          //:endpar()
   ENDIF
RETURN nil

*+--------------------------------------------------------------------
*+
*+    Function ProcRTFDesc()
*+
*+    Called from ( genrtf.prg   )   7 - function processrtf()
*+
*+--------------------------------------------------------------------
*+
FUNCTION ProcRTFDesc( cBuffer, oRtf, cStyle )

   LOCAL cLine       := ""
   LOCAL npos
   LOCAL CurPos      := 0
   LOCAL nColorPos
   LOCAL ccolor      := ""
   LOCAL creturn     := ""
   LOCAL NIDENTLEVEL
   LOCAL coline
   LOCAL lEndPar     := .F.

   LOCAL lEndFixed := .F.
   LOCAL lEndTable := .F.
   LOCAL lArgBold  := .F.
   DEFAULT cStyle TO "Default"
   IF AT( "<par>", cBuffer ) == 0 .AND. !EMPTY( cBuffer ) .AND. cstyle != "Example"
      cBuffer := "<par>" + cBuffer
   ENDIF

   IF EMPTY( cBuffer )
      oRtf:WritePar( "" )
   ENDIF

   IF cStyle != "Example" .AND. AT( "<table>", cBuffer ) == 0 .AND. AT( "<fixed>", cBuffer ) == 0
      IF AT( "<par>", cBuffer ) >= 0 .OR. AT( "</par>", cBuffer ) == 0 .AND. !EMPTY( cbuffer )
         IF AT( "<par>", cBuffer ) > 0 .AND. AT( "</par>", cBuffer ) > 0

            IF cStyle == "Arguments"

               creturn := cBuffer

               cReturn := STRTRAN( cReturn, "<par>", "" )
               cReturn := STRTRAN( cReturn, "</par>", "" )
               cReturn := ALLTRIM( cReturn )
               nPos    := AT( " ", cReturn )
               cOLine  := LEFT( cReturn, nPos - 1 )
               cReturn := STRTRAN( cReturn, coLine, "" )
               if "\" $ cReturn
                  cReturn := Strtran( cReturn, "\", "\\")
//                tracelog( cReturn )
               endif
               IF AT( "@", cOLine ) > 0 .OR. AT( "()", cOLine ) > 0 .OR. AT( "<", cOLine ) > 0 .OR. AT( "_", cOLine ) > 0
                  lArgBold := .T.
               ELSE
                  lArgBold := .F.
               ENDIF

               //            cBuffer:= strtran(cBuffer,"<par>","<par><b>")
               IF lArgBold
                  cReturn := "       <par><b>" + cOLine + "</b> " + cReturn + "    </par>"
               ELSE
                  cReturn := "       <par>" + cOLine + " " + cReturn + "    </par>"
               ENDIF

               cbuffer := cReturn
            ENDIF

         ELSE
            cBuffer := FormatrtfBuff( cBuffer, cStyle )
         ENDIF
      ENDIF
   ENDIF

   IF AT( "<par>", cBuffer ) > 0 .AND. AT( "</par>", cBuffer ) > 0
      if "\" $ cBuffer
                  cBuffer := Strtran(cBuffer, "\", "\\")
//                tracelog( cBuffer )
               endif

      cBuffer   := STRTRAN( cBuffer, "<par>", "" )
      cBuffer   := STRTRAN( cBuffer, "<b>", "\b " )
      cBuffer   := STRTRAN( cBuffer, "</b>", "\b0 " )
      cBuffer   := STRTRAN( cBuffer, "<em>", "\b\i " )
      cBuffer   := STRTRAN( cBuffer, "</em>", "\b0\i0 " )
      cBuffer   := STRTRAN( cBuffer, "<i>", "\i " )
      cBuffer   := STRTRAN( cBuffer, "</i>", "\i0 " )
      cBuffer   := STRTRAN( cBuffer, "</color>", "\cf1 " )
      nColorPos := AT( "<color:", cBuffer )
      IF ncolorpos > 0
         checkrtfcolor( @cbuffer, ncolorpos )
      ENDIF

      IF cStyle == "Description" .OR. cStyle == "Compliance"
         nIdentLevel := 6
         nPos        := 0
         IF AT( "</par>", cBuffer ) > 0
            cBuffer := STRTRAN( cBuffer, "</par>", "" )
         ENDIF
         IF !EMPTY( cBuffer )
            //             cBuffer:=SUBSTR(cBuffer,2)
            cBuffeR := ALLTRIM( cBuffer )
            oRtf:WritePar( "       " + cBuffer + " ", "\fi-426\li426 " )
         ENDIF

      ELSEIF cStyle == "Arguments"

         IF AT( "</par>", cBuffer ) > 0
            cBuffer := STRTRAN( cBuffer, "</par>", "" )
         ENDIF
         IF !EMPTY( cBuffer )
            cBuffeR := ALLTRIM( cBuffer )
            oRtf:WritePar( "       " + cBuffer + " ", "\fi-2272\li2272 " )
         ENDIF

      ELSEIF cStyle == "Syntax"
         IF AT( "</par>", cBuffer ) > 0
            cBuffer := STRTRAN( cBuffer, "</par>", "" )
         ENDIF
         IF !EMPTY( cBuffer )
            //                    cBuffer:=SUBSTR(cBuffer,2)
            cBuffeR := ALLTRIM( cBuffer )
            oRtf:WritePar( cBuffer + " ", "\fi-426\li426  " )
         ENDIF

      ELSEIF cStyle == "Default"
         IF AT( "</par>", cBuffer ) > 0
            cBuffer := STRTRAN( cBuffer, "</par>", "" )
         ENDIF
         IF !EMPTY( cBuffer )
            //                  cBuffer:=SUBSTR(cBuffer,2)
            cBuffeR := ALLTRIM( cBuffer )
            oRtf:WritePar( "       " + cBuffer, "\fi-426\li426 " )
         ENDIF

      ENDIF
   ENDIF
   IF AT( "<fixed>", cBuffer ) > 0 .OR. cStyle = "Example"
      IF AT( "<fixed>", cBuffer ) == 0 .OR. !EMPTY( cBuffer )
         cBuffer := STRTRAN( cBuffer, "<par>", "" )
         cBuffer := STRTRAN( cBuffer, "<fixed>", "" )
         oRtf:WriteParFixed( cBuffer )
      ENDIF
      DO WHILE !lendFixed
         cLine := TRIM( SUBSTR( ReadLN( @lEof ), nCommentLen ) )
         IF AT( "</fixed>", cLine ) > 0
            lendfixed := .T.
            cLine     := STRTRAN( cline, "</fixed>", "" )
         ENDIF
         IF AT( DELIM, cline ) > 0
            FT_FSKIP( - 1 )
            lEndfixed := .T.

         ENDIF
         IF AT( DELIM, cline ) == 0
            oRtf:WriteParFixed( cline )
         ENDIF
      ENDDO

   END
   IF AT( "<table>", cBuffer ) > 0
      DO WHILE !lendTable
         cBuffer := TRIM( SUBSTR( ReadLN( @lEof ), nCommentLen ) )
         IF AT( "</table>", cBuffer ) > 0
            lendTable := .T.
         ELSE
            procrtftable( cBuffer )
         ENDIF
      ENDDO
      IF lEndTable
         GenrtfTable( oRtf )
      ENDIF
   ENDIF

   //      If cStyle=="Description" .or. cStyle=="Compliance"
   //         oRtf:Writepar("")
   //      endif

RETURN nil

*+--------------------------------------------------------------------
*+
*+    Function ProcRtfTable()
*+
*+    Called from ( genrtf.prg   )   1 - function procrtfdesc()
*+
*+--------------------------------------------------------------------
*+
FUNCTION ProcRtfTable( cBuffer )

   LOCAL nPos
   LOCAL cItem
   LOCAL nColorpos
   LOCAL cColor
   IF AT( "<color:", cBuffer ) > 0
      nColorPos := AT( ":", cBuffer )
      cColor    := SUBSTR( cBuffer, nColorpos + 1 )
      nPos      := AT( ">", ccolor )
      cColor    := SUBSTR( ccolor, 1, nPos - 1 )

      cBuffer   := STRTRAN( cbuffer, "</color>", "\cf1" )
      cBuffer   := STRTRAN( cbuffer, "<color:", "" )
      cBuffer   := STRTRAN( cbuffer, ">", "" )
      cBuffer   := STRTRAN( cBuffer, ccolor, "" )
      nColorpos := ASCAN( aColorTable, { | x | UPPER( x[ 1 ] ) == UPPER( ccolor ) } )
      cColor    := aColortable[ nColorPos, 2 ]
   ENDIF
   IF !EMPTY( cBuffer )
      cItem := cBuffer
   ELSE
      cItem := ""
   ENDIF
   IF ccolor != NIL
      AADD( afiTable, ccolor + cItem )
   ELSE
      AADD( afiTable, cItem )
   ENDIF

RETURN Nil

*+--------------------------------------------------------------------
*+
*+    Function GenRtfTable()
*+
*+    Called from ( genrtf.prg   )   1 - function procrtfdesc()
*+
*+--------------------------------------------------------------------
*+
FUNCTION GenRtfTable( oRtf )

   LOCAL x
   LOCAL lCar       := .F.
   LOCAL nMax2
   LOCAL nPos2
   LOCAL nPos
   LOCAL aLensFItem := {}
   LOCAL aLensSItem := {}

   FOR X := 1 TO LEN( afitable )
      IF AT( "\cf", afitable[ x ] ) > 0
         AADD( aLensfItem, LEN( SUBSTR( STRTRAN( afitable[ x ], "\cf1", "" ), AT( " ", afitable[ x ] ) ) ) )
      ELSE
         AADD( aLensFItem, LEN( afiTable[ x ] ) )
      ENDIF
   NEXT
   ASORT( aLensFItem,,, { | x, y | x > y } )

//   oRtf:WritePar( "" )
   //  nMax2:=checkcar(aTable,1)+1
   nMax2 := alensfitem[ 1 ]
   nPos  := maxrtfelem( afitable )
   nPos2 := ASCAN( alensfitem, { | x | x == nPos } )

   oRtf:WriteParBox( "       " + replicate( CHR( 196 ), 80 ) )
   FOR x := 1 TO LEN( afiTable )
      ortf:WriteParFixed( iif( AT( "|", afiTable[ x ] ) > 0, STRTRAN( afiTable[ x ], "|", " " ), afiTable[ x ] ), "\fi-426\li426" )
   NEXT
   oRtf:WriteParBox( "       " + replicate( CHR( 196 ), 80 ) )
//   oRtf:WritePar( "" )
   afiTable := {}

RETURN Nil

*+--------------------------------------------------------------------
*+
*+    Function checkrtfcolor()
*+
*+    Called from ( genrtf.prg   )   1 - function procrtfdesc()
*+
*+--------------------------------------------------------------------
*+
FUNC checkrtfcolor( cbuffer, ncolorpos )

   LOCAL ncolorend
   LOCAL nreturn
   LOCAL cOldColorString
   LOCAL cReturn
   LOCAL ccolor

   DO WHILE AT( "<color:", cbuffer ) > 0
      nColorPos       := AT( "<color:", cBuffer )
      ccolor          := SUBSTR( cbuffer, ncolorpos + 7 )
      nColorend       := AT( ">", ccolor )
      ccolor          := SUBSTR( ccolor, 1, nColorend - 1 )
      cOldColorString := SUBSTR( cbuffer, ncolorpos )
      nColorend       := AT( ">", cOldColorString )
      cOldColorString := SUBSTR( cOldColorString, 1, nColorEnd )
      nreturn         := ASCAN( acolortable, { | x | UPPER( x[ 1 ] ) == UPPER( ccolor ) } )
      IF nreturn > 0
         creturn := "\cf" + acolortable[ nreturn, 2 ]
      ENDIF
      cBuffer := STRTRAN( cBuffer, cOldColorString, cReturn )
   ENDDO
RETURN cbuffer

*+--------------------------------------------------------------------
*+
*+    Function maxrtfelem()
*+
*+    Called from ( genos2.prg   )   1 - function genos2table()
*+                ( genrtf.prg   )   1 - function genrtftable()
*+
*+--------------------------------------------------------------------
*+
FUNC maxrtfelem( a )

   LOCAL nsize   := LEN( a )
   LOCAL max     := 0
   LOCAL tam     := 0
   LOCAL max2    := 0
   LOCAL nPos    := 1
   LOCAL cString
   LOCAL ncount
   FOR ncount := 1 TO nsize
      IF AT( "\cf", a[ ncount ] ) > 0
         cString := SUBSTR( STRTRAN( a[ ncount ], "\cf1", "" ), 6 )
         tam     := LEN( cString )
      ELSE
         tam := LEN( a[ ncount ] )
      ENDIF
      max := iif( tam > max, tam, max )
   NEXT
   nPos := ASCAN( a, { | x | LEN( x ) == max } )
RETURN max

*+--------------------------------------------------------------------
*+
*+    Function FormatrtfBuff()
*+
*+    Called from ( genrtf.prg   )   1 - function procrtfdesc()
*+
*+--------------------------------------------------------------------
*+
FUNCTION FormatrtfBuff( cBuffer, cStyle )

   LOCAL cReturn  := ""
   LOCAL cLine    := ""
   LOCAL cBuffend := ""
   LOCAL coline   := ""
   LOCAL lEndBuff := .F.
   LOCAL nPos
   LOCAL lArgBold := .F.
   creturn := cBuffer + " "
   IF AT( "</par>", creturn ) > 0 .OR. EMPTY( cBuffer )
      IF EMPTY( cbuffer )
         creturn := ""
      ENDIF
      RETURN creturn
   ENDIF
   IF cStyle != "Syntax" .AND. cStyle != "Arguments" .AND. cStyle != "Return"
      DO WHILE !lendBuff
         cLine := TRIM( SUBSTR( ReadLN( @lEof ), nCommentLen ) )
         IF AT( "</par>", cLine ) > 0
            lEndBuff := .T.
         ENDIF

         IF EMPTY( cLine )
            lEndBuff := .T.

            FT_FSKIP( - 1 )
         ENDIF
         IF AT( DELIM, cline ) > 0

            FT_FSKIP( - 1 )
            lEndBuff := .T.
         ENDIF
         IF AT( DELIM, cLine ) == 0
            cReturn += " " + ALLTRIM( cLine ) + " "
         ENDIF
      ENDDO
      creturn := STRTRAN( creturn, "<par>", "" )
      creturn := STRTRAN( creturn, "</par>", "" )

      cReturn := "<par>" + creturn + "    </par>"
   ELSEIF cStyle == "Syntax"

      cReturn := "<par><b>" + cReturn + " </b></par>"

   ELSEIF cStyle == "Arguments" .OR. cStyle == "Return"

      nPos    := 0
      cReturn := "<par>" + creturn
      IF AT( "<par>", cReturn ) > 0
         cReturn := STRTRAN( cReturn, "<par>", "" )
         cReturn := STRTRAN( cReturn, "</par>", "" )
         cReturn := ALLTRIM( cReturn )
         nPos    := AT( " ", cReturn )
         cOLine  := LEFT( cReturn, nPos - 1 )
         cReturn := STRTRAN( cReturn, coLine, "" )
         IF AT( "@", cOLine ) > 0 .OR. AT( "()", cOLine ) > 0 .OR. AT( "<", cOLine ) > 0 .OR. AT( "_", cOLine ) > 0
            lArgBold := .T.
         ELSE
            lArgBold := .F.
         ENDIF

      ENDIF
      DO WHILE !lEndBuff

         cLine := TRIM( SUBSTR( ReadLN( @lEof ), nCommentLen ) )
         IF AT( "</par>", cLine ) > 0
            lEndBuff := .T.
         ENDIF
         IF EMPTY( cLine )
            lEndBuff := .T.

            FT_FSKIP( - 1 )

         ENDIF
         IF AT( DELIM, cline ) > 0
            FT_FSKIP( - 1 )
            lEndBuff := .T.
         ENDIF
         IF AT( DELIM, cline ) == 0
            cReturn += " " + ALLTRIM( cLine ) + " "
         ENDIF
      ENDDO
      creturn := STRTRAN( creturn, "<par>", "" )
      creturn := STRTRAN( creturn, "</par>", "" )
      IF lArgBold
         cReturn := "       <par><b>" + cOLine + "</b> " + cReturn + "    </par>"
      ELSE
         cReturn := "       <par>" + cOLine + " " + cReturn + "    </par>"
      ENDIF
      lArgBold := .F.
   ENDIF

RETURN cReturn

*+--------------------------------------------------------------------
*+
*+    Static Function ReadFromTop()
*+
*+    Called from ( genrtf.prg   )   1 - function processrtf()
*+
*+--------------------------------------------------------------------
*+
STATIC FUNCTION ReadFromTop( nh )

   LOCAL cDoc      := DELIM + "DOC" + DELIM                    // DOC keyword
   LOCAL cEnd      := DELIM + "END" + DELIM                    // END keyword
   LOCAL cClassDoc := DELIM + "CLASSDOC" + DELIM
   LOCAL cBuffer   := ""
   LOCAL NPOS      := 0
   LOCAL aLocDoc   := {}
   DO WHILE FREADline( nH, @cBuffer, 4096 )
      cBuffer := TRIM( SUBSTR( cBuffer, nCommentLen ) )
      AADD( Alocdoc, CbUFFER )
      IF AT( cEnd, cBuffer ) > 0
         AADD( aCurdoc, aLocdoc )
         aLocDoc := {}
      ENDIF

   ENDDO
   lEof := .F.
   FT_FGOTOP()
RETURN nil

*+--------------------------------------------------------------------
*+
*+    Static Function GetItem()
*+
*+    Called from ( genrtf.prg   )  21 - function processrtf()
*+
*+--------------------------------------------------------------------
*+
STATIC FUNCTION GetItem( cItem, nCurdoc )

   LOCAL nPos
   LOCAL cCuritem
   LOCAL lReturn := .F.
   LOCAL xPos
   xPos := aCurdoc[ nCurdoc ]
   nPos := ASCAN( xPos, { | x | UPPER( ALLTRIM( x ) ) == UPPER( ALLTRIM( cItem ) ) } )
   IF nPos > 0
      cCuritem := xPos[ nPos ]
      IF AT( "$", xPos[ nPos + 1 ] ) > 0
         lReturn := .F.
      ELSE
         lReturn := .T.
      ENDIF

   ENDIF
RETURN lReturn

*+--------------------------------------------------------------------
*+
*+    Class TRTF
*+
*+--------------------------------------------------------------------
*+
CLASS TRTF

   DATA cFile
   DATA nHandle
   DATA aIdh init {}
   DATA lastId init 100
   METHOD WriteHeader()
   METHOD New( cFile )
   METHOD WritePar( cPar, cIden )
   METHOD WriteParFixed( cPar )
   METHOD WriteParText( cPar, lConv )
   METHOD WriteParNoIndent( cPar )
   METHOD WriteParBox( cPar )
   METHOD WriteLink( clink )
   METHOD WriteJumpLink( clink )
   METHOD WritekLink( aLink ,lAlink)
   METHOD WriteJumpLink1( cLink, cName, cText )
   METHOD CLOSE()
   METHOD WriteParBold( cPar, lCenter )
   METHOD WriteParBoldText( cPar, cText )
   METHOD WriteTitle( cTitle, cTopic )
   METHOD WriteJumpTitle( cTitle, cTopic )
   METHOD EndPar()
   METHOD EndPage()

ENDCLASS

METHOD new( cFile ) CLASS TRTF

   IF ISCHARACTER( cFile )
      Self:cFile   := LOWER( cFile )
      Self:nHandle := FCREATE( Self:cFile )
   ENDIF
   RETURN Self

METHOD WriteHeader() CLASS TRTF

   LOCAL cHeader := "{\rtf1\ansi\pard\fs20" + CRLF + ;
           "\deff5{\fonttbl" + CRLF + ;
           "{\f0\froman Tms Rmn;}" + CRLF + ;
           "{\f1\fdecor Symbol;}" + CRLF + ;
           "{\f2\fswiss Helv;}" + CRLF + ;
           "{\f3\fmodern LinePrinter;}" + CRLF + ;
           "{\f4\froman Terminal;}" + CRLF + ;
           "{\f5\froman Times New Roman;}" + CRLF + ;
           "{\f6\fswiss Arial;}" + CRLF + ;
           "{\f7\froman CG Times (WN);}" + CRLF + ;
           "{\f8\fmodern Courier;}" + CRLF + ;
           "{\f9\fmodern Modern;}" + CRLF + ;
           "{\f10\fscript Script;}" + CRLF + ;
           "{\f11\fswiss Univers (WN);}" + CRLF + ;
           "{\f12\fnil Wingdings;}" + CRLF + ;
           "{\f13\fswiss MS Sans Serif;}" + CRLF + ;
           "{\f14\fmodern\fcharset2 LotusWP Box;}" + CRLF + ;
           "}" + CRLF

   LOCAL cColortable := "{\colortbl;" + CRLF + ;
           "\red0\green0\blue0;" + CRLF + ;
           "\red0\green255\blue255;" + CRLF + ;
           "\red255\green0\blue255;" + CRLF + ;
           "\red128\green128\blue128;" + CRLF + ;
           "\red0\green128\blue0;" + CRLF + ;
           "\red0\green255\blue0;" + CRLF + ;
           "\red128\green0\blue0;" + CRLF + ;
           "\red0\green0\blue128;" + CRLF + ;
           "\red128\green128\blue0;" + CRLF + ;
           "\red128\green0\blue128;" + CRLF + ;
           "\red255\green0\blue0;" + CRLF + ;
           "\red192\green192\blue192;" + CRLF + ;
           "\red0\green128\blue128;" + CRLF + ;
           "\red255\green255\blue255;" + CRLF + ;
           "\red255\green255\blue0;" + CRLF + ;
           "}" + CRLF

   FWRITE( Self:nHandle, cHeader )

   FWRITE( Self:nHandle, cColorTable )

   RETURN Self

METHOD WritePar( cPar, cIden ) CLASS TRTF

   DEFAULT ciDen TO ""
   cPar := STRTRAN( cPar, "{", "\{" )
   cPar := STRTRAN( cPar, "}", "\}" )
   FWRITE( Self:nHandle, "\par" + CRLF + "\pard\cf1\f6\fs20\b0\i0" + cIden + HB_OEMTOANSI( cPar ) + CRLF )
   RETURN Self

METHOD WriteParNoIndent( cPar ) CLASS TRTF

   cPar := STRTRAN( cPar, "{", "\{" )
   cPar := STRTRAN( cPar, "}", "\}" )
   FWRITE( Self:nHandle, "\par" + CRLF + "\pard\cf1\f6\fs20\b0\i0" + HB_OEMTOANSI( cPar ) + CRLF )
   RETURN Self

METHOD WriteParBox( cPar ) CLASS TRTF

   cPar := STRTRAN( cPar, "{", "\{" )
   cPar := STRTRAN( cPar, "}", "\}" )
   FWRITE( Self:nHandle, "\par" + CRLF + "\pard\cf1\f4\b0\i0\fi-426\li426" + HB_OEMTOANSI( cPar ) + CRLF )
   RETURN Self

METHOD WriteParFixed( cPar ) CLASS TRTF

   cPar := STRTRAN( cPar, "{", "\{" )
   cPar := STRTRAN( cPar, "}", "\}" )
   FWRITE( Self:nHandle, "\par" + CRLF + "\pard\cf1\f8\b0\i0\keep" + cPar + CRLF )
   RETURN SELF

METHOD WriteParText( cPar, lConv ) CLASS TRTF

   DEFAULT lConv TO .T.
   cPar := STRTRAN( cPar, "{", "\{" )
   cPar := STRTRAN( cPar, "}", "\}" )
   IF lConv
      FWRITE( Self:nHandle, HB_OEMTOANSI( cPar ) )
   ELSE
      FWRITE( Self:nHandle, cPar )
   ENDIF
   RETURN Self

METHOD EndPar() CLASS TRTF

   FWRITE( Self:nHandle, "\par" + CRLF )
   RETURN Self

METHOD WriteParBold( cPar, lCenter ) CLASS TRTF

   DEFAULT lCenter TO .F.
   cPar := STRTRAN( cPar, "{", "\{" )
   cPar := STRTRAN( cPar, "}", "\}" )
   IF lCenter
      FWRITE( Self:nHandle, "\par \pard\qc\cf1\f6\fs30\i\b\fi-426\li426 " + ALLTRIM( HB_OEMTOANSI( cPar ) ) + CRLF )
   ELSE
      FWRITE( Self:nHandle, "\par \pard\cf1\f6\fs30\i0\b\fi-426\li426 " + ALLTRIM( HB_OEMTOANSI( cPar ) ) + CRLF )
   ENDIF
   RETURN Self

METHOD WriteParBoldText( cPar, cText ) CLASS TRTF

   cPar  := STRTRAN( cPar, "{", "\{" )
   cPar  := STRTRAN( cPar, "}", "\}" )
   cText := STRTRAN( cText, "{", "\{" )
   cText := STRTRAN( cText, "}", "\}" )

   FWRITE( Self:nHandle, "\par \pard\cf1\f6\fs20\i\b       " + ALLTRIM( HB_OEMTOANSI( cPar ) ) + " \b\cf1\f6\fs20\i0\b0\li300 " + ALLTRIM( HB_OEMTOANSI( cText ) ) + CRLF )
   RETURN Self

METHOD WriteTitle( cTitle, cTopic, cOne ,cCat) CLASS TRTF

   LOCAL cTemp
   LOCAL nPos
   LOCAL cWrite

   nPos := AT( "()", cTitle )

   IF nPos > 0
      cTemp := ALLTRIM( HB_OEMTOANSI( STRTRAN( cTitle, "()", "xx" ) ) )
   ELSE
      cTemp := HB_OEMTOANSI( ALLTRIM( cTitle ) )
      cTemp := STRTRAN( cTemp, "@", "x" )
   ENDIF
   nPos := AT( "#", cTitle )

   IF nPos > 0
      cTemp := ALLTRIM( HB_OEMTOANSI( STRTRAN( cTemp, "#", "\#" ) ) )
   ENDIF
   cTopic := ALLTRIM( HB_OEMTOANSI( cTopic ) )
   cTemp := StrTran( cTemp, " ","_")

   Aadd( ::aIdh, {"IDH_" + cTemp,::lastid++})
   cWrite := CRLF + ;
             "  {#{\footnote \pard\fs20 {" + "IDH_" + cTemp + " }}}" + CRLF + ;
             "  {${\footnote \pard\fs20 {" + ALLTRIM( cTopic ) + " }}}" + CRLF + ;
             "  {K{\footnote \pard\fs20 {" + UPPERLOWER(ALLTRIM( cTopic ))+";" + UPPERLOWER(ALLTRIM( cCat ))+ " }}}" + CRLF + ;
             "  {A{\footnote{A} " + UPPERLOWER(ALLTRIM( cTopic )) +" }}" + CRLF + ;
              CRLF



   /*"{\f6" + CRLF + ;*/
             /*" ; " + UPPERLOWER(cCat) +" , " +UPPERLOWER(ALLTRIM( strtran(cTopic,"()","" )))+ */
   aadd(aWww,{cTopic,"IDH_"+cTemp,cCat})
   nPos := ascan(aResult,{|a| UPPER(a) == UPPER(cCat)})
   if nPos==0
      aadd(aResult,cCat)
   endif
   FWRITE( Self:nHandle, cWrite )

   FWRITE( Self:nHandle, "\pard\cf1\f6\fs30\i0\b\keepn " + ALLTRIM( HB_OEMTOANSI( cTopic ) ) + CRLF )
   FWRITE( Self:nHandle, "\par" + CRLF + "\pard\cf1\f6\fs20\b\i0\keepn" + " " + CRLF )
   FWRITE( Self:nHandle, "\par \pard\cf1\f6\fs30\i0\b\keepn " + ALLTRIM( HB_OEMTOANSI( cOne ) ) + CRLF )
   RETURN Self

METHOD WriteJumpTitle( cTitle, cTopic ) CLASS TRTF

   LOCAL cTemp
   LOCAL nPos
   LOCAL cWrite

   nPos := AT( "()", cTitle )

   IF nPos > 0
      cTemp := ALLTRIM( HB_OEMTOANSI( STRTRAN( cTitle, "()", "xx" ) ) )
   ELSE
      cTemp := HB_OEMTOANSI( ALLTRIM( cTitle ) )
      cTemp := STRTRAN( cTemp, "@", "x" )
   ENDIF

   cTopic := ALLTRIM( HB_OEMTOANSI( cTopic ) )

   cWrite :=  CRLF + ;
             "  #{\footnote \pard\fs20 " + "IDH_" + cTemp + " }" + CRLF + ;
             "  ${\footnote \pard\fs20 " + ALLTRIM( cTopic ) + " }" + CRLF + ;
             CRLF

   FWRITE( Self:nHandle, cWrite )

   Self:WriteParBold( cTopic )

   RETURN Self

METHOD EndPage() CLASS TRTF

   FWRITE( Self:nHandle, "\par " + CRLF + "\page" + CRLF )
   RETURN Self

METHOD CLOSE() CLASS TRTF

   //   FWRITE( Self:nHandle, "\page" + CRLF )

   FWRITE( Self:nHandle, "}" + CRLF )

   FCLOSE( Self:nHandle )

   RETURN Self

METHOD WriteLink( cLink ) CLASS TRTF

   FWRITE( Self:nHandle, "\par \pard\cf1\fs20       {\f6\uldb " + ALLTRIM( HB_OEMTOANSI( cLink ) ) + "}{\v\f6 " + "IDH_" + iif( AT( "()", cLink ) > 0, ALLTRIM( HB_OEMTOANSI( STRTRAN( cLink, "()", "xx" ) ) ), ALLTRIM( HB_OEMTOANSI( STRTRAN( cLink, "@", "x" ) ) ) ) + "}" + CRLF )

   RETURN Self


METHOD WriteJumpLink( cLink, cName, cText ) CLASS TRTF

   FWRITE( Self:nHandle, "\par \pard\cf1\fs20       {\f6\uldb " + ALLTRIM( HB_OEMTOANSI( cName ) ) + "}{\v\f6 " + "IDH_" + iif( AT( "()", cLink ) > 0, ALLTRIM( HB_OEMTOANSI( STRTRAN( cLink, "()", "xx" ) ) ), ALLTRIM( HB_OEMTOANSI( STRTRAN( cLink, "@", "x" ) ) ) ) + "}" + cText + CRLF )

   RETURN Self

METHOD WriteJumpLink1( cLink, cName, cText ) CLASS TRTF

   FWRITE( Self:nHandle, "\par \pard\cf1\fs20       {\f6\ul " + ALLTRIM( HB_OEMTOANSI( cName ) ) + "}{\v\f6 " + "IDH_" + iif( AT( "()", cLink ) > 0, ALLTRIM( HB_OEMTOANSI( STRTRAN( cLink, "()", "xx" ) ) ), ALLTRIM( HB_OEMTOANSI( STRTRAN( cLink, "@", "x" ) ) ) ) + "}" + cText + CRLF )

   RETURN Self

METHOD WritekLink( aLink ,lAlink) CLASS TRTF
   Local cItem:=" "
   Local nPos:=0
   Local nSize:=Len(aLink)

   HB_SYMBOL_UNUSED( lAlink )

   if nSize >2
      For nPos:=1 to nSize
         if nPos==nSize
            cItem+= UPPERLOWER(aLink[nPos])
         else
            cItem+= UPPERLOWER(aLink[nPos])
            cItem+=";"
         endif
      next
      cItem:=Alltrim(cItem)
      FWRITE( Self:nHandle, "\par \pard\cf1\fs20       \{button , ALink("+UPPER(cItem) + ", 2) \}{\f6\uldb Related Topic }"+'{\v\f6 %!ALink(" '+cItem + '", 2) }'+ CRLF )
   else
      For nPos:=1 to nSize
         FWRITE( Self:nHandle, "\par \pard\cf1\fs20       {\f6\uldb "+aLink[nPos] +' }{\v\f6 !KLink(" '+UPPERLOWER(aLink[nPos]) + '", 2) }'+ CRLF )
      next
   endif
   RETURN Self
