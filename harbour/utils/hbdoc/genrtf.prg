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
#include 'hbdocdef.ch'
#include 'common.ch'
//  output lines on the screen

#define INFILELINE   10
#define MODULELINE   12
#define LINELINE     14
#define ERRORLINE    20
#define LONGLINE     600
#define LONGONELINE  86
MEMVAR aDirList
MEMVAR aDocinfo
STATIC aFiTable    := {}
STATIC lIsTable    := .F.
STATIC nCommentLen
STATIC lEof
STATIC aCurDoc     := {}
STATIC nCurDoc     := 1

STATIC aColorTable := { { 'aqua', '\cf2 ' }, { 'black', '\cf1 ' }, { 'fuchia', '\cf3 ' }, { 'grey', '\cf4 ' }, { 'green', '\cf5 ' }, { 'lime', '\cf6 ' }, { 'maroon', '\cf7 ' }, { 'navy', '\cf8 ' }, { 'olive', '\cf9 ' }, { 'purple', '\cf10 ' }, { 'red', '\cf11 ' }, { 'silver', '\cf12 ' }, { 'teal', '\cf13 ' }, { 'white', '\cf14 ' }, { 'yellow', '\cf15 ' } }

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function ProcessRtf()
*+
*+    Called from ( hbdoc.prg    )   2 - function main()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
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
   LOCAL cBar           := " " + REPL( ')', 80 )
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
   LOCAL lWrite         := .f.
   LOCAL lWasLine       := .F.
   LOCAL nPos
   LOCAL nEpos
   LOCAL nPosend
   LOCAL cBuffEnd
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
   LOCAL cTest          := DELIM + 'TESTS' + DELIM
   LOCAL cStatus        := DELIM + 'STATUS' + DELIM
   LOCAL cPlat          := DELIM + 'PLATFORMS' + DELIM
   LOCAL cFiles         := DELIM + 'FILES' + DELIM
   LOCAL cSubCode       := DELIM + 'SUBCODE' + DELIM
   LOCAL cFunction      := DELIM + 'FUNCTION' + DELIM
   LOCAL cConstruct     := DELIM + 'CONSTRUCTOR' + DELIM
   LOCAL cDatalink      := DELIM + 'DATALINK' + DELIM
   LOCAL cDatanolink    := DELIM + 'DATANOLINK' + DELIM
   LOCAL cMethodslink   := DELIM + 'METHODSLINK' + DELIM
   LOCAL cMethodsNolink := DELIM + 'METHODSNOLINK' + DELIM
   LOCAL cData          := DELIM + "DATA" + DELIM
   LOCAL cMethod        := DELIM + 'METHOD' + DELIM
   LOCAL cClassDoc      := DELIM + "CLASSDOC" + DELIM
   LOCAL cTable         := DELIM + "TABLE" + DELIM
   Local aAlso:={}
   lFirstArg     := .T.
   lData         := .F.
   lMethod       := .F.
   lIsDataLink   := .F.
   lIsMethodLink := .F.

   lWrite := .f.
   cTempx := ''
   //
   //  Entry Point
   //
   //  Put up information labels
   @ INFILELINE, 20 SAY "Extracting: "          
   @ MODULELINE, 20 SAY "Documenting: "         
   //  loop through all of the files
   oRtf := tRtf():new( "rtf\Harbour.rtf" ):WriteHeader()
   FOR i := 1 TO nFiles

      //  Open file for input

      nCommentLen := IIF( AT( ".ASM", UPPER( aDirList[ i, F_NAME ] ) ) > 0, 2, 4 )
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
         IF nLineCnt % 10 = 0
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
                  if Len(aAlso) <3
                    ProcRtfalso( oRtf, cSeealso )
                  else
                  oRtf:WriteKLink(aAlso)
                  endif  
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
                       ( cChar >= "A" .AND. cChar <= "Z" ) .OR. cChar = "_"
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

                     IF LEN( cFileName ) = 40
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
                  oRtf:WriteJumpTitle( LEFT( cFilename, AT( '.', cFilename ) - 1 ) + cFuncName, "Data " + cFuncName )
               ELSEIF lMethod
                  oRtf:WriteJumpTitle( LEFT( cFilename, AT( '.', cFilename ) - 1 ) + cFuncName, LEFT( cFilename, AT( '.', cFilename ) - 1 )  + ":"+cFuncName )
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
                     oRtf:WritePar( '' )                    //:endpar()
                     nMode := D_SYNTAX
                     //                  oRtf:WritePar('') //:endpar()
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
                     oRtf:WritePar( '' )                    //:endpar()
                     nMode     := D_ARG
                     lAddBlank := .T.
                     lPar      := .t.
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
                     lPar      := .t.
                  END
               ELSEIF AT( cDesc, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     oRtf:WritePar( '' )                    //:endpar()
                     oRtf:WriteParBold( " Description" )
                     oRtf:WritePar( '' )                    //:endpar()
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
                        oRtf:WritePar('') //:endpar()
                        oRtf:WriteParBold( " Examples" )
                     ENDIF

                     nMode     := D_EXAMPLE
                     lAddBlank := .T.
                  END
               ELSEIF AT( cTest, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     IF !lBlankLine
                                                              oRtf:WritePar('') //:endpar()
                       oRtf:WriteParBold( " Tests" )
                        oRtf:WritePar( '' )                 //:endpar()
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
                        oRtf:WritePar( '' )                 //:endpar()
                        oRtf:WriteParBold( " Compliance" )
                        oRtf:WritePar( '' )                 //:endpar()
                     ENDIF

                     nMode     := D_COMPLIANCE
                     lAddBlank := .T.
                     lPar      := .T.
                  END
               ELSEIF AT( cPlat, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     IF !lBlankLine
                        //ortf:endpar()
                        oRtf:WritePar( '' )                 //:endpar()
                        oRtf:WriteParBold( " Platforms" )
                        oRtf:WritePar( '' )                 //:endpar()
                     ENDIF

                     nMode     := D_NORMAL
                     lAddBlank := .T.
                     lPar      := .T.
                  END
               ELSEIF AT( cFiles, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     IF !lBlankLine
                        oRtf:WritePar( '' )                 //:endpar()
                        oRtf:WriteParBold( " Files" )
                        oRtf:WritePar( '' )                 //:endpar()
                     ENDIF

                     lPar      := .T.
                     nMode     := D_NORMAL
                     lAddBlank := .T.
                  END
               ELSEIF AT( cFunction, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     IF !lBlankLine
                        oRtf:WritePar( '' )                 //:endpar()
                        oRtf:WriteParBold( " Functions" )
                        oRtf:WritePar( '' )                 //:endpar()
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
                  IF nMode = D_SYNTAX
                     IF LEN( cBuffer ) > LONGLINE
                        WRITE_ERROR( "Syntax", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     IF AT( "<par>", cBuffer ) > 0
                        STRTRAN( cBuffer, "<par>", '' )
                        STRTRAN( cBuffer, "</par>", '' )
                        cBuffer := ALLTRIM( cBuffer )
                        cbuFfer := '<par><b>' + cBuffer + '</b></par>'
                     ENDIF
                     procrtfdesc( cbuffer, oRtf, "Syntax" )
                     //                      oRtf:WritePar('') //:endpar()
                  ELSEIF nMode = D_RETURN

                     IF LEN( cBuffer ) > LONGLINE
                        WRITE_ERROR( "Arguments", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )

                     procrtfdesc( cbuffer, oRtf, "Arguments" )

                  ELSEIF nMode = D_ARG

                     IF LEN( cBuffer ) > LONGLINE
                        WRITE_ERROR( "Arguments", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )

                     procrtfdesc( cbuffer, oRtf, "Arguments" )
                  ELSEIF nMode = D_DATALINK
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
                     oRtf:WriteJumpLink1( LEFT( cfilename, AT( '.', cFilename ) - 1 ) + ALLTRIM( cTemp ), cTemp, cBuffer )
                  ELSEIF nMode = D_METHODLINK
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
                     oRtf:WriteJumpLink( LEFT( cfilename, AT( '.', cFilename ) - 1 ) + ALLTRIM( cTemp ),ALLTRIM( cTemp ), cBuffer )

                  ELSEIF nMode = D_NORMAL
                     IF LEN( cBuffer ) > LONGLINE
                        WRITE_ERROR( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     procrtfdesc( cBuffer, oRtf )
                  ELSEIF nMode = D_COMPLIANCE
                     IF LEN( cBuffer ) > LONGLINE
                        WRITE_ERROR( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     procrtfdesc( cBuffer, oRtf, "Compliance" )

                  ELSEIF nMode = D_DESCRIPTION
                     IF LEN( cBuffer ) > LONGLINE
                        WRITE_ERROR( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     procrtfdesc( cBuffer, oRtf, "Description" )

                  ELSEIF nMode = D_EXAMPLE
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
                  ELSEIF nMode = D_SEEALSO
                     IF .NOT. EMPTY( cBuffer )
                        cSeeAlso := StripFiles( ALLTRIM( cBuffer ) )
                     ENDIF
                  ELSEIF nMode = D_INCLUDE
                     //  read next line
                     IF .NOT. EMPTY( cBuffer )
                        IF !lBlankLine
                           oRtf:WritePar( "" )              //:endpar()
                        ENDIF
                     ENDIF
                  ELSEIF nMode = D_STATUS
                     IF !EMPTY( cBuffer )
                        oRtf:WritePar( '' )                 //:endpar()
                        oRtf:WriteParBold( "Status" )
                        oRtf:WritePar( '' )                 //:endpar()
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

RETURN NIL

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function ProcRtfAlso()
*+
*+    Called from ( genrtf.prg   )   1 - function processrtf()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
FUNCTION ProcRtfAlso( nWriteHandle, cSeeAlso )

   LOCAL nPos
   LOCAL cTemp := ''
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

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function procrtfstatus()
*+
*+    Called from ( genrtf.prg   )   1 - function processrtf()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
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

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function ProcRTFDesc()
*+
*+    Called from ( genrtf.prg   )   7 - function processrtf()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
FUNCTION ProcRTFDesc( cBuffer, oRtf, cStyle )

   LOCAL cLine       := ''
   LOCAL npos
   LOCAL CurPos      := 0
   LOCAL nColorPos
   LOCAL ccolor      := ''
   LOCAL creturn     := ''
   LOCAL ncolorend
   LOCAL NIDENTLEVEL
   LOCAL coline
   LOCAL lEndPar     := .F.

   LOCAL lEndFixed := .F.
   LOCAL lEndTable := .F.
   LOCAL lArgBold  := .f.
   DEFAULT cStyle TO "Default"
   IF AT( '<par>', cBuffer ) == 0 .AND. !EMPTY( cBuffer ) .AND. cstyle <> "Example"
      cBuffer := '<par>' + cBuffer
   ENDIF

   IF EMPTY( cBuffer )
      oRtf:WritePar( "" )
   ENDIF

   IF cStyle <> "Example" .AND. AT( "<table>", cBuffer ) == 0 .AND. AT( "<fixed>", cBuffer ) = 0
      IF AT( "<par>", cBuffer ) >= 0 .OR. AT( "</par>", cBuffer ) = 0 .AND. !EMPTY( cbuffer )
         IF AT( "<par>", cBuffer ) > 0 .AND. AT( "</par>", cBuffer ) > 0

            IF cStyle == "Arguments"

               creturn := cBuffer

               cReturn := STRTRAN( cReturn, "<par>", "" )
               cReturn := STRTRAN( cReturn, "</par>", "" )
               cReturn := ALLTRIM( cReturn )
               nPos    := AT( " ", cReturn )
               cOLine  := LEFT( cReturn, nPos - 1 )
               cReturn := STRTRAN( cReturn, coLine, "" )
               IF AT( "@", cOLine ) > 0 .OR. AT( "()", cOLine ) > 0 .OR. AT( "<", cOLine ) > 0 .OR. AT( "_", cOLine ) > 0
                  lArgBold := .T.
               ELSE
                  lArgBold := .f.
               ENDIF

               //            cBuffer:= strtran(cBuffer,"<par>","<par><b>")
               IF lArgBold
                  cReturn := '       <par><b>' + cOLine + '</b> ' + cReturn + '    </par>'
               ELSE
                  cReturn := '       <par>' + cOLine + ' ' + cReturn + '    </par>'
               ENDIF

               cbuffer := cReturn
            ENDIF

         ELSE
            cBuffer := FormatrtfBuff( cBuffer, cStyle, ortf )
         ENDIF
      ENDIF
   ENDIF

   IF AT( '<par>', cBuffer ) > 0 .AND. AT( '</par>', cBuffer ) > 0
      cBuffer   := STRTRAN( cBuffer, '<par>', '' )
      cBuffer   := STRTRAN( cBuffer, '<b>', '\b ' )
      cBuffer   := STRTRAN( cBuffer, '</b>', '\b0 ' )
      cBuffer   := STRTRAN( cBuffer, '<em>', '\b\i ' )
      cBuffer   := STRTRAN( cBuffer, '</em>', '\b0\i0 ' )
      cBuffer   := STRTRAN( cBuffer, '<i>', '\i ' )
      cBuffer   := STRTRAN( cBuffer, '</i>', '\i0 ' )
      cBuffer   := STRTRAN( cBuffer, '</color>', '\cf1 ' )
      nColorPos := AT( '<color:', cBuffer )
      IF ncolorpos > 0
         checkrtfcolor( @cbuffer, ncolorpos )
      ENDIF

      IF cStyle == "Description" .OR. cStyle == "Compliance"
         nIdentLevel := 6
         nPos        := 0
         IF AT( '</par>', cBuffer ) > 0
            cBuffer := STRTRAN( cBuffer, "</par>", "" )
         ENDIF
         IF !EMPTY( cBuffer )
            //             cBuffer:=SUBSTR(cBuffer,2)
            cBuffeR := ALLTRIM( cBuffer )
            oRtf:WritePar( "       " + cBuffer + ' ', '\fi-426\li426 ' )
         ENDIF

      ELSEIF cStyle == "Arguments"

         IF AT( '</par>', cBuffer ) > 0
            cBuffer := STRTRAN( cBuffer, "</par>", "" )
         ENDIF
         IF !EMPTY( cBuffer )
            cBuffeR := ALLTRIM( cBuffer )
            oRtf:WritePar( "       " + cBuffer + ' ', '\fi-2272\li2272 ' )
         ENDIF

      ELSEIF cStyle == "Syntax"
         IF AT( '</par>', cBuffer ) > 0
            cBuffer := STRTRAN( cBuffer, "</par>", "" )
         ENDIF
         IF !EMPTY( cBuffer )
            //                    cBuffer:=SUBSTR(cBuffer,2)
            cBuffeR := ALLTRIM( cBuffer )
            oRtf:WritePar( cBuffer + ' ', '\fi-426\li426  ' )
         ENDIF

      ELSEIF cStyle == "Default"
         IF AT( '</par>', cBuffer ) > 0
            cBuffer := STRTRAN( cBuffer, "</par>", "" )
         ENDIF
         IF !EMPTY( cBuffer )
            //                  cBuffer:=SUBSTR(cBuffer,2)
            cBuffeR := ALLTRIM( cBuffer )
            oRtf:WritePar( "       " + cBuffer, '\fi-426\li426 ' )
         ENDIF

      ENDIF
   ENDIF
   IF AT( '<fixed>', cBuffer ) > 0 .OR. cStyle = "Example"
      IF AT( '<fixed>', cBuffer ) = 0 .OR. !EMPTY( cBuffer )
         cBuffer := STRTRAN( cBuffer, "<par>", "" )
         cBuffer := STRTRAN( cBuffer, "<fixed>", "" )
         oRtf:WriteParFixed( cBuffer )
      ENDIF
      DO WHILE !lendFixed
         cLine := TRIM( SUBSTR( ReadLN( @lEof ), nCommentLen ) )
         IF AT( "</fixed>", cLine ) > 0
            lendfixed := .t.
            cLine     := STRTRAN( cline, "</fixed>", "" )
         ENDIF
         IF AT( DELIM, cline ) > 0
            FT_FSKIP( - 1 )
            lEndfixed := .t.

         ENDIF
         IF AT( DELIM, cline ) == 0
            oRtf:WriteParFixed( cline )
         ENDIF
      ENDDO

   END
   IF AT( '<table>', cBuffer ) > 0
      DO WHILE !lendTable
         cBuffer := TRIM( SUBSTR( ReadLN( @lEof ), nCommentLen ) )
         IF AT( "</table>", cBuffer ) > 0
            lendTable := .t.
         ELSE
            procrtftable( cBuffer )
         ENDIF
      ENDDO
      IF lEndTable
         GenrtfTable( oRtf )
      ENDIF
   ENDIF

   //      If cStyle=="Description" .or. cStyle=="Compliance"
   //         oRtf:Writepar('')
   //      endif

RETURN nil

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function ProcRtfTable()
*+
*+    Called from ( genrtf.prg   )   1 - function procrtfdesc()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
FUNCTION ProcRtfTable( cBuffer )

   LOCAL nPos
   LOCAL cItem
   LOCAL cItem2
   LOCAL cItem3
   LOCAL xtype
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
      cBuffer   := STRTRAN( cBuffer, ccolor, '' )
      nColorpos := ASCAN( aColorTable, { | x, y | UPPER( x[ 1 ] ) == UPPER( ccolor ) } )
      cColor    := aColortable[ nColorPos, 2 ]
   ENDIF
   IF !EMPTY( cBuffer )
      cItem := cBuffer
   ELSE
      cItem := ''
   ENDIF
   IF ccolor <> NIL
      AADD( afiTable, ccolor + cItem )
   ELSE
      AADD( afiTable, cItem )
   ENDIF

RETURN Nil

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function GenRtfTable()
*+
*+    Called from ( genrtf.prg   )   1 - function procrtfdesc()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
FUNCTION GenRtfTable( oRtf )

   LOCAL y
   LOCAL nLen2
   LOCAL x
   LOCAL nMax
   LOCAL nSpace
   LOCAL lCar       := .f.
   LOCAL nMax2
   LOCAL nSpace2
   LOCAL nPos1
   LOCAL nPos2
   LOCAL LColor
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

   oRtf:WriteParBox( "       " + REPL( CHR( 196 ), 80 ) )
   FOR x := 1 TO LEN( afiTable )
      ortf:WriteParFixed( IF( AT( "|", afiTable[ x ] ) > 0, STRTRAN( afiTable[ x ], "|", " " ), afiTable[ x ] ), '\fi-426\li426' )
   NEXT
   oRtf:WriteParBox( "       " + REPL( CHR( 196 ), 80 ) )
//   oRtf:WritePar( "" )
   afiTable := {}

RETURN Nil

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function checkrtfcolor()
*+
*+    Called from ( genrtf.prg   )   1 - function procrtfdesc()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
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
      nreturn         := ASCAN( acolortable, { | x, y | UPPER( x[ 1 ] ) == UPPER( ccolor ) } )
      IF nreturn > 0
         creturn := "\cf" + acolortable[ nreturn, 2 ]
      ENDIF
      cBuffer := STRTRAN( cBuffer, cOldColorString, cReturn )
   ENDDO
RETURN cbuffer

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function maxrtfelem()
*+
*+    Called from ( genos2.prg   )   1 - function genos2table()
*+                ( genrtf.prg   )   1 - function genrtftable()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
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
      max := IF( tam > max, tam, max )
   NEXT
   nPos := ASCAN( a, { | x | LEN( x ) == max } )
RETURN max

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function FormatrtfBuff()
*+
*+    Called from ( genrtf.prg   )   1 - function procrtfdesc()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
FUNCTION FormatrtfBuff( cBuffer, cStyle, ongi )

   LOCAL cReturn  := ''
   LOCAL cLine    := ''
   LOCAL cBuffend := ''
   LOCAL cEnd
   LOCAL cStart
   LOCAL coline   := ''
   LOCAL lEndBuff := .f.
   LOCAL nPos
   LOCAL nPosEnd
   LOCAL lArgBold := .f.
   creturn := cBuffer + ' '
   IF AT( '</par>', creturn ) > 0 .OR. EMPTY( cBuffer )
      IF EMPTY( cbuffer )
         creturn := ''
      ENDIF
      RETURN creturn
   ENDIF
   IF cStyle != "Syntax" .AND. cStyle != "Arguments" .AND. cStyle != "Return"
      DO WHILE !lendBuff
         cLine := TRIM( SUBSTR( ReadLN( @lEof ), nCommentLen ) )
         IF AT( '</par>', cLine ) > 0
            lEndBuff := .t.
         ENDIF

         IF EMPTY( cLine )
            lEndBuff := .t.

            FT_FSKIP( - 1 )
         ENDIF
         IF AT( DELIM, cline ) > 0

            FT_FSKIP( - 1 )
            lEndBuff := .t.
         ENDIF
         IF AT( DELIM, cLine ) = 0
            cReturn += ' ' + ALLTRIM( cLine ) + ' '
         ENDIF
      ENDDO
      creturn := STRTRAN( creturn, "<par>", "" )
      creturn := STRTRAN( creturn, "</par>", "" )

      cReturn := '<par>' + creturn + '    </par>'
   ELSEIF cStyle == 'Syntax'

      cReturn := '<par><b>' + cReturn + ' </b></par>'

   ELSEIF cStyle == 'Arguments' .OR. cStyle == "Return"

      nPos    := 0
      cReturn := '<par>' + creturn
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
            lArgBold := .f.
         ENDIF

      ENDIF
      DO WHILE !lEndBuff

         cLine := TRIM( SUBSTR( ReadLN( @lEof ), nCommentLen ) )
         IF AT( "</par>", cLine ) > 0
            lEndBuff := .t.
         ENDIF
         IF EMPTY( cLine )
            lEndBuff := .t.

            FT_FSKIP( - 1 )

         ENDIF
         IF AT( DELIM, cline ) > 0
            FT_FSKIP( - 1 )
            lEndBuff := .t.
         ENDIF
         IF AT( DELIM, cline ) = 0
            cReturn += ' ' + ALLTRIM( cLine ) + ' '
         ENDIF
      ENDDO
      creturn := STRTRAN( creturn, "<par>", "" )
      creturn := STRTRAN( creturn, "</par>", "" )
      IF lArgBold
         cReturn := '       <par><b>' + cOLine + '</b> ' + cReturn + '    </par>'
      ELSE
         cReturn := '       <par>' + cOLine + ' ' + cReturn + '    </par>'
      ENDIF
      lArgBold := .F.
   ENDIF

RETURN cReturn

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Static Function ReadFromTop()
*+
*+    Called from ( genrtf.prg   )   1 - function processrtf()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
STATIC FUNCTION ReadFromTop( nh )

   LOCAL cDoc      := DELIM + "DOC" + DELIM                    // DOC keyword
   LOCAL cEnd      := DELIM + "END" + DELIM                    // END keyword
   LOCAL cClassDoc := DELIM + "CLASSDOC" + DELIM
   LOCAL cBuffer   := ''
   LOCAL NPOS      := 0
   LOCAL nlenpos
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

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Static Function GetItem()
*+
*+    Called from ( genrtf.prg   )  21 - function processrtf()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
STATIC FUNCTION GetItem( cItem, nCurdoc )

   LOCAL nPos
   LOCAL cCuritem
   LOCAL lReturn
   LOCAL x
   LOCAL xPos
   xPos := aCurdoc[ nCurdoc ]
   nPos := ASCAN( xPos, { | x, y | UPPER( ALLTRIM( x ) ) == UPPER( ALLTRIM( cItem ) ) } )
   IF nPos > 0
      cCuritem := xPos[ nPos ]
      IF AT( "$", xPos[ nPos + 1 ] ) > 0
         lReturn := .f.
      ELSE
         lReturn := .t.
      ENDIF

   ENDIF
RETURN lReturn

// /

*+ EOF: GENRTF.PRG
