/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * GENOS2 support module for hbdoc document Extractor 
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
#include "hbdocdef.ch"
#include 'common.ch'
//  output lines on the screen

#define INFILELINE   10
#define MODULELINE   12
#define LINELINE     14
#define ERRORLINE    20
#define LONGLINE     600
#define LONGONELINE  86
MEMVAR aDirlist
MEMVAR aDocInfo
STATIC aAlso
STATIC nCommentLen
STATIC lEof
STATIC aFiTable    := {}
STATIC aSiTable       := {}
STATIC aFoiTable      := {}
STATIC atiTable       := {}
STATIC nNumTableItems := 0

STATIC lIsTable    := .F.
STATIC aColorTable := { { 'aqua', '' }, { 'black', '' }, { 'fuchia', '' }, { 'grey', '' }, { 'green', '' }, { 'lime', '' }, { 'maroon', '' }, { 'navy', '' }, { 'olive', '' }, { 'purple', '' }, { 'red', '' }, { 'silver', '' }, { 'teal', '' }, { 'white', '' }, { 'yellow', '' } }
STATIC aCurDoc        := {}
STATIC nCurDoc := 1

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function ProcessOs2()
*+
*+    Called from ( hbdoc.prg    )   2 - function main()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION ProcessOs2()

   //

   //  Copyright (C) 2000 Luiz Rafael Culik
   //
   //  Purpose: Process each of the files in the directory
   //  and generate .tr file format output
   //  Modification History:
   //         Version    Date        Who       Notes
   //          V1.00     1/20/2000   LRC       Initial Version
   //
   //  Calling parameters: None
   //
   //  Notes: None
   // -
   //  LOCAL variables:

   LOCAL i
   LOCAL j
   LOCAL nFiles
   //   LOCAL nCommentLen
   //   LOCAL lEof
   LOCAL lDoc
   LOCAL cBuffer
   LOCAL nEnd
   LOCAL nCount
   LOCAL nAlso

   LOCAL cBar          := "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
   LOCAL nMode
   LOCAL cname
   LOCAL cFuncName
   LOCAL cOneLine
   LOCAL cCategory
   LOCAL cFileName
   LOCAL nLineCnt
   LOCAL cSeeAlso
   LOCAL cTemp
   LOCAL cChar
   LOCAL oOs2
   LOCAL lData         := .F.
   LOCAL lMethod       := .F.
   LOCAL nPos
   LOCAL nEpos
   LOCAL nPosend
   LOCAL cBuffEnd
   LOCAL lIsDataLink   := .F.
   LOCAL lIsMethodLink := .F.

   LOCAL lBlankLine     := .F.             // Blank line encountered and sent out
   LOCAL lAddBlank      := .F.             // Need to add a blank line if next line is not blank
   LOCAL nReadHandle
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

   lData         := .F.
   lMethod       := .F.
   lIsDataLink   := .F.
   lIsMethodLink := .F.

   nFiles := LEN( aDirList )
   //
   //  Entry Point
   //
   //  Put up information labels
   @ INFILELINE, 20 SAY "Extracting: "          
   @ MODULELINE, 20 SAY "Documenting: "         
   //  loop through all of the files
   oOs2 := tOS2():new( "ipf\Harbour.ipf" )
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
      lEof := .F.
      lDoc := .F.
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
               lDoc := .F.
               IF lData .OR. lmethod
                  oos2:writeText( ':efn.' )
               ENDIF
               IF .NOT. EMPTY( cSeeAlso )
                  oOs2:WriteText( ".br")
                  oOs2:WriteParBold( "See Also&colon." )
                  FOR nAlso := 1 TO LEN( aAlso )

                     IF nAlso == 1
                        oOs2:WriteLink( aAlso[ nAlso ] )
                     ELSE
                        oOs2:WriteLink( aAlso[ nAlso ] )
                     ENDIF
                  NEXT

               ENDIF

               nMode := D_IGNORE
            ENDIF

            @ MODULELINE, 33 CLEAR TO MODULELINE, MAXCOL()
         ENDIF

         //  Act on the input
         IF lDoc
            //  1) function name

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

               cFileName := LEFT( cFileName, 40 )
               nEnd      := 1
               nCount    := 0
               DO WHILE nEnd > 0
                  nEnd := ASCAN( aDocInfo, { | a | a[ 4 ] == cFileName + ".ipf" } )
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

               IF oOs2:nHandle < 1
                  ? "Error creating", cFileName, ".ipf"
                  WRITE_ERROR( "Error creating",,,, cFileName + ".ipf" )
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
                  oOs2:WriteJumpTitle( LEFT( cFilename, AT( '.', cFilename ) - 1 ) + cFuncName, "Data " + cFuncName )
               ELSEIF lMethod
                  oOs2:WriteJumpTitle( LEFT( cFilename, AT( '.', cFilename ) - 1 ) + cFuncName, "Method " + cFuncName )
               ELSE

                  oOs2:WriteTitle( PAD( cFuncName, 40 ), cFuncName ,cCategory)
                  oOs2:WriteParBold( cOneLine )

               ENDIF
               //  4) all other stuff

            ELSE

               IF AT( cSyn, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                  oOs2:WriteParBold( " Syntax" )

                  nMode     := D_SYNTAX
                  lAddBlank := .T.
end
               ELSEIF AT( cConstruct, cBuffer ) > 0
                     IF GetItem( cBuffer, nCurdoc )
                  oOs2:WriteParBold( " Constructor syntax" )

                  nMode     := D_SYNTAX
                  lAddBlank := .T.
end
               ELSEIF AT( cArg, cBuffer ) > 0
                     IF GetItem( cBuffer, nCurdoc )
                  IF !lBlankLine

                     oOs2:WriteParBold( " Arguments" )

                  ENDIF

                  nMode     := D_ARG
                  lAddBlank := .T.
   end
               ELSEIF AT( cRet, cBuffer ) > 0

                        IF GetItem( cBuffer, nCurdoc )
                                 IF !lBlankLine
                     oOs2:WriteText( ".br" )
                  ENDIF

                  oOs2:WriteParBold( " Returns" )

                  nMode     := D_RETURN
                  lAddBlank := .T.
                  end

               ELSEIF AT( cDesc, cBuffer ) > 0
                                 IF GetItem( cBuffer, nCurdoc )

                  IF !lBlankLine
                     oOs2:WriteText( ".br" )
                  ENDIF
                  oOs2:WriteParBold( " Description" )

                  nMode     := D_DESCRIPTION
                  lAddBlank := .T.
                  end
               ELSEIF AT( cdatalink, cBuffer ) > 0
                                 IF GetItem( cBuffer, nCurdoc )
                  IF !lBlankLine
                     oOs2:WriteText( ".br" )                    //:endpar()
                  ENDIF

                  oOs2:WriteParBold( " Data" )
                  nMode     := D_DATALINK
                  lAddBlank := .T.

                  lIsDataLink := .T.
               end
               ELSEIF AT( cDatanolink, cBuffer ) > 0
                                 IF GetItem( cBuffer, nCurdoc )
                  IF !lIsDataLink
                     oOs2:WriteParBold( " Data" )

                  ENDIF
                  nMode     := D_NORMAL
                  lAddBlank := .T.
                  end

               ELSEIF AT( cMethodslink, cBuffer ) > 0
               IF GetItem( cBuffer, nCurdoc )

                  oOs2:WriteParBold( " Method" )
                  nMode     := D_METHODLINK
                  lAddBlank := .T.

                  lIsMethodLink := .T.
               end
               ELSEIF AT( cMethodsnolink, cBuffer ) > 0
               IF GetItem( cBuffer, nCurdoc )
                  IF !lIsMethodLink
                     oOs2:WriteParBold( " Methods" )
                  ENDIF

                  nMode     := D_NORMAL
                  lAddBlank := .T.
               end
               ELSEIF AT( cExam, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                  IF !lBlankLine
                     oOs2:WriteText( ".br" )
                  ENDIF
                  oOs2:WriteParBold( " Examples" )
                  nMode     := D_EXAMPLE
                  lAddBlank := .T.
                  end
               ELSEIF AT( cTest, cBuffer ) > 0
                     IF GetItem( cBuffer, nCurdoc )
                  IF !lBlankLine
                     oOs2:WriteText( ".br" )
                  ENDIF

                  oOs2:WriteParBold( " Tests" )
                  nMode     := D_EXAMPLE
                  lAddBlank := .T.
                     end
               ELSEIF AT( cStatus, cBuffer ) > 0
                        IF GetItem( cBuffer, nCurdoc )
                  nMode := D_STATUS
                        end
               ELSEIF AT( cCompl, cBuffer ) > 0
                        IF GetItem( cBuffer, nCurdoc )
                  IF !lBlankLine
                     oOs2:WriteText( ".br" )
                  ENDIF
                  oOs2:WriteParBold( " Compliance" )
                  nMode     := D_COMPLIANCE
                  lAddBlank := .T.
                  end
               ELSEIF AT( cPlat, cBuffer ) > 0
                     IF GetItem( cBuffer, nCurdoc )
                  IF !lBlankLine
                     oOs2:WriteText( ".br" )
                  ENDIF
                  oOs2:WriteParBold( " Platforms" )
                  nMode     := D_NORMAL
                  lAddBlank := .T.
                  end
               ELSEIF AT( cFiles, cBuffer ) > 0
                     IF GetItem( cBuffer, nCurdoc )
                  IF !lBlankLine
                     oOs2:WriteText( ".br" )
                  ENDIF
                  oOs2:WriteParBold( " Files" )

                  nMode     := D_NORMAL
                  lAddBlank := .T.
                  end
               ELSEIF AT( cFunction, cBuffer ) > 0
                     IF GetItem( cBuffer, nCurdoc )
                  IF !lBlankLine
                     oOs2:WriteText( ".br" )
                  ENDIF
                  oOs2:WriteParBold( " Function" )

                  nMode     := D_NORMAL
                  lAddBlank := .T.
                     end
               ELSEIF AT( cSee, cBuffer ) > 0
               IF GetItem( cBuffer, nCurdoc )
                  nMode := D_SEEALSO
                  end
               ELSEIF AT( cInc, cBuffer ) > 0
               IF GetItem( cBuffer, nCurdoc )
                  nMode := D_INCLUDE
end
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
//                   cBuffer := oOs2:DosToOs2Text(cBuffer)
                     IF AT( "<par>", cBuffer ) > 0
                        STRTRAN( cBuffer, "<par>", '' )
                        STRTRAN( cBuffer, "</par>", '' )
                        cBuffer := ALLTRIM( cBuffer )
                        cbuFfer := '<par>' + cBuffer + '</par>'
                     ENDIF

                     procos2desc( cbuffer, oOs2, "Syntax" )

                  ELSEIF nMode = D_RETURN

                     IF LEN( cBuffer ) > LONGLINE
                        WRITE_ERROR( "Arguments", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
/*                     oOs2:dostoos2Text(cBuffer)*/
                     procos2desc( cbuffer, oOs2, "Arguments" )

                  ELSEIF nMode = D_ARG
                     IF LEN( cBuffer ) > LONGLINE
                        WRITE_ERROR( "Arguments", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
//                     oOs2:DosToOs2Text(cBuffer)
                     procos2desc( cbuffer, oOs2, "Arguments" )
                  ELSEIF nMode = D_NORMAL
                     IF LEN( cBuffer ) > LONGLINE
                        WRITE_ERROR( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
//                     oOs2:DosToOs2Text(cBuffer)
                     procos2desc( cBuffer, oOs2 )
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
                     oOs2:WriteJumpLink( LEFT( cfilename, AT( '.', cFilename ) - 1 ) + ALLTRIM( cTemp ), cBuffer )
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
                     oOs2:WriteJumpLink( LEFT( cfilename, AT( '.', cFilename ) - 1 ) + ALLTRIM( cTemp ), cTemp, cBuffer )

                  ELSEIF nMode = D_SEEALSO
                     IF .NOT. EMPTY( cBuffer )
                        cSeeAlso := ProcOs2Also( StripFiles( ALLTRIM( cBuffer ) ) )
                     ENDIF
                  ELSEIF nMode = D_INCLUDE
                     //  read next line
                     IF .NOT. EMPTY( cBuffer )
                        IF !lBlankLine
                           oOs2:WriteText( ".br" )
                        ENDIF
                     ENDIF
                  ELSEIF nMode = D_COMPLIANCE
                     IF LEN( cBuffer ) > LONGLINE
                        WRITE_ERROR( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
//      oOs2:DosToOs2Text(cBuffer)
                     procos2desc( cBuffer, oOs2, "Compliance" )

                  ELSEIF nMode = D_DESCRIPTION
                     IF LEN( cBuffer ) > LONGLINE
                        WRITE_ERROR( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
//      oOs2:DosToOs2Text(cBuffer)
                     procos2desc( cBuffer, oOs2, "Description" )

                  ELSEIF nMode = D_EXAMPLE
                     IF LEN( cBuffer ) > LONGLINE
                        WRITE_ERROR( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )

                     IF lAddBlank
                        oOs2:WriteText( ".br" )                 //:endpar()
                        lAddBlank := .F.
                     ENDIF
//      oOs2:DosToOs2Text(cBuffer)
                     procos2desc( cBuffer, oOs2, "Example" )

                  ELSEIF nMode = D_STATUS
                     IF !EMPTY( cBuffer )
                        oOs2:WriteParBold( "Status" )
                     ENDIF
                           oOs2:DosToOs2Text(cBuffer)
                     ProcStatusOs2( oOs2, cBuffer )

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
      nCurDoc := 1
      aCurDoc := {}

   NEXT
   oOs2:Close()
RETURN NIL

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function ProcStatusOs2()
*+
*+    Called from ( genos2.prg   )   1 - function processos2()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION ProcStatusOs2( nWriteHandle, cBuffer )

   IF LEN( ALLTRIM( cBuffer ) ) > 1
      nWriteHandle:WritePar( cBuffer )
   ELSEIF SUBSTR( ALLTRIM( cBuffer ), 1 ) == "R"
      nWriteHandle:WritePar( "Ready" )
   ELSEIF SUBSTR( ALLTRIM( cBuffer ), 1 ) == "S"
      nWriteHandle:WritePar( "Started" )
   ELSE
      nWriteHandle:WritePar( "Not Started" )
   ENDIF
RETURN nil

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function ProcOs2Also()
*+
*+    Called from ( genos2.prg   )   1 - function processos2()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION ProcOs2Also( cSeealso )

   aAlso := {}
   aAlso := ListAsArray2( cSeealso, "," )
RETURN aAlso

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function Formatos2Buff()
*+
*+    Called from ( genos2.prg   )   1 - function procos2desc()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION Formatos2Buff( cBuffer, cStyle, ongi )

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

   cReturn := cBuffer + ' '
   IF AT( '</par>', cReturn ) > 0 .OR. EMPTY( cBuffer )
      IF EMPTY( cbuffer )
         cReturn := ''
      ENDIF
      RETURN cReturn
   ENDIF
   IF cStyle != "Syntax" .AND. cStyle != "Arguments"
      DO WHILE !lEndBuff
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
            cReturn += " "+ ALLTRIM( cLine ) + ' '
         ENDIF
      ENDDO
      creturn := STRTRAN( creturn, "<par>", "" )
      creturn := STRTRAN( creturn, "</par>", "" )

      cReturn := '<par>' + creturn + '    </par>'

   ELSEIF cStyle == 'Syntax'
      cReturn := '<par> ' + cReturn + ' </par>'
   ELSEIF cStyle == 'Arguments'
      nPos := 0
      ? 'Estou em formatos2buff'
      ?  AT( "<par>", cReturn )
      IF AT( "<par>", cReturn ) > 0
         cReturn := STRTRAN( cReturn, "<par>", "" )
         cReturn := STRTRAN( cReturn, "</par>", "" )
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
         IF AT( DELIM, cline ) = 0
            cReturn += ' ' + ALLTRIM( cLine ) + ' '
         ENDIF
      ENDDO
      creturn := STRTRAN( creturn, "<par>", "" )
      creturn := STRTRAN( creturn, "</par>", "" )
      IF lArgBold
         cReturn := '<par><b>' + cOLine + '</b> ' + cReturn + '    </par>'
      ELSE
         cReturn := '<par>' + cOLine + ' ' + cReturn + '    </par>'
      ENDIF
      lArgBold := .F.

   ENDIF
RETURN cReturn

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function checkos2color()
*+
*+    Called from ( genos2.prg   )   1 - function procos2desc()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNC checkos2color( cbuffer, ncolorpos )

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
         creturn := + acolortable[ nreturn, 2 ]
      ENDIF
      cBuffer := STRTRAN( cBuffer, cOldColorString, cReturn )
   ENDDO
RETURN cbuffer

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function ProcOs2Table()
*+
*+    Called from ( genos2.prg   )   1 - function procos2desc()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION ProcOs2Table( cBuffer , nNum )

   LOCAL nPos
   LOCAL cItem
   LOCAL cItem2
   LOCAL cItem3
   LOCAL cItem4
   LOCAL xtype
   LOCAL nColorpos
   LOCAL cColor
   cBuffer := ALLTRIM( cBuffer )
   IF AT( "<color:", cBuffer ) > 0
      nColorPos := AT( ":", cBuffer )
      cColor    := SUBSTR( cBuffer, nColorpos + 1 )
      nPos      := AT( ">", ccolor )
      cColor    := SUBSTR( ccolor, 1, nPos - 1 )
      cBuffer   := STRTRAN( cbuffer, "</color>", "" )
      cBuffer   := STRTRAN( cbuffer, "<color:", "" )
      cBuffer   := STRTRAN( cbuffer, ">", "" )
      cBuffer   := STRTRAN( cBuffer, ccolor, '' )
      nColorpos := ASCAN( aColorTable, { | x, y | UPPER( x[ 1 ] ) == UPPER( ccolor ) } )
      cColor    := aColortable[ nColorPos, 2 ]
   ENDIF
   IF EMPTY( cBuffer )
      citem  := ''
      citem2 := ''
      citem3 := ''
      citem4 := ''
   ELSE
      cBuffer := STRTRAN( cBuffer, "<", "&lt." )
      cBuffer := STRTRAN( cBuffer, ">", "&gt." )

      cItem   := SUBSTR( cBuffer, 1, AT( SPACE( 3 ), cBuffer ) - 1 )
      cBuffer := ALLTRIM( STRTRAN( cBuffer, cItem, "" ) )
      IF nNum == 2
         cItem2 := SUBSTR( cBuffer, 1 )
      ELSE
         cItem2  := SUBSTR( cBuffer, 1, AT( SPACE( 3 ), cBuffer ) - 1 )
         cBuffer := ALLTRIM( STRTRAN( cBuffer, cItem2, "" ) )
      ENDIF

      IF nNum == 3
         cItem3 := SUBSTR( cBuffer, 1 )
      ELSEIF nNum > 3
         cItem3  := SUBSTR( cBuffer, 1, AT( SPACE( 3 ), cBuffer ) - 1 )
         cBuffer := ALLTRIM( STRTRAN( cBuffer, cItem3, "" ) )
         cItem4  := SUBSTR( cBuffer, 1 )
      ENDIF
   ENDIF
   IF cColor <> NIL
      AADD( afiTable, "<Font color=" + ccolor + ">" + RTRIM( LTRIM( cItem ) ) + '</font>' )
      AADD( asiTable, "<Font color=" + ccolor + ">" + cItem2 + '</font>' )
   ELSE
      AADD( afiTable, RTRIM( LTRIM( cItem ) ) )
      AADD( asiTable, cItem2 )
   ENDIF

   IF cColor <> NIL
      AADD( atiTable, "<Font color=" + ccolor + ">" + cItem3 + '</font>' )
   ELSE
      AADD( atiTable, cItem3 )
   ENDIF
   IF cColor <> NIL
      AADD( afoiTable, "<Font color=" + ccolor + ">" + cItem4 + '</font>' )
   ELSE
      AADD( afoiTable, cItem4 )
   ENDIF

RETURN Nil

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function maxos2elem()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNC maxos2elem( a )

   LOCAL nsize   := LEN( a )
   LOCAL max     := 0
   LOCAL tam     := 0
   LOCAL max2    := 0
   LOCAL nPos    := 1
   LOCAL cString
   LOCAL ncount
   FOR ncount := 1 TO nsize
      tam := LEN( a[ ncount ] )
      max := IF( tam > max, tam, max )
   NEXT
   nPos := ASCAN( a, { | x | LEN( x ) == max } )
RETURN max

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function Genos2Table()
*+
*+    Called from ( genos2.prg   )   1 - function procos2desc()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION Genos2Table( oOs2 )
    Local x
    Local cItem,cItem1,cItem2,cItem3
//   oos2:WritePar( "" )
    if nNumTableItems == 2
        citem  := str(maxos2elem(afitable),2)
        cItem1 := Str(maxos2elem(asitable),2)
       oos2:writeText(":table cols="+"'"+cItem  + " " + cItem1 +"'." )
    elseif nNumTableItems == 3         
       oos2:writeText(":table cols="+"'"+str(maxos2elem(afitable),2) + " " + str(maxos2elem(asitable),2) +  " " +str(maxos2elem(atitable),2) +"'." )
    elseif nNumTableItems == 4
       oos2:writeText(":table cols="+"'"+str(maxos2elem(afitable),2) + " " + str(maxos2elem(asitable),2) +  " " +str(maxos2elem(atitable),2) + ' ' +str(maxos2elem(afoitable),2)+"'." )
    endif


   FOR x := 1 TO LEN( asitable )
      IF !EMPTY( asitable[ x ] )
         IF nNumTableItems == 2
            oos2:Writetext(':row.')
            oos2:Writetext(':c.' + afitable[x])
            oos2:Writetext(':c.' + asitable[x])

         ELSEIF nNumTableItems == 3
            oos2:Writetext(':row.')
            oos2:Writetext(':c.' + afitable[x])
            oos2:Writetext(':c.' + asitable[x])
            oos2:Writetext(':c.' + atitable[x])
         ELSEIF nNumTableItems == 4
            oos2:Writetext(':row.')
            oos2:Writetext(':c.' + afitable[x])
            oos2:Writetext(':c.' + asitable[x])
            oos2:Writetext(':c.' + atitable[x])
            oos2:Writetext(':c.' + afoitable[x])
         ENDIF
      ELSE
            oos2:Writetext(':row.')
            oos2:Writetext(':c.')

      ENDIF
   NEXT

   oOs2:Writetext( ':etable.' )
   oos2:WriteText( ".br" )
   afiTable  := {}
   asitable  := {}
   atitable  := {}
   afoitable := {}


RETURN Nil

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function Procos2Desc()
*+
*+    Called from ( genos2.prg   )   7 - function processos2()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION Procos2Desc( cBuffer, oOs2, cStyle )

   LOCAL cLine       := ''
   LOCAL lArgBold    := .F.
   LOCAL lHasFixed   := .F.
   LOCAL npos
   LOCAL CurPos      := 0
   LOCAL nColorPos
   LOCAL ccolor      := ''
   LOCAL creturn     := ''
   LOCAL ncolorend
   LOCAL NIDENTLEVEL
   LOCAL coline
   LOCAL lEndPar     := .F.
   LOCAL LFstTableItem := .T.
   LOCAL lEndFixed := .F.
   LOCAL lEndTable := .F.

   DEFAULT cStyle TO "Default"
   lendfixed := .F.
   IF AT( '<par>', cBuffer ) == 0 .AND. !EMPTY( cBuffer ) .AND. cstyle <> "Example"
      cBuffer := '<par>' + cBuffer
   ENDIF
   IF EMPTY( cBuffer )
      oOs2:WriteText( ".br" )
   ENDIF

   IF cStyle <> "Example" .AND. AT( "<table>", cBuffer ) == 0 .AND. AT( "<fixed>", cBuffer ) = 0
      IF AT( "<par>", cBuffer ) >= 0 .OR. AT( "</par>", cBuffer ) = 0 .AND. !EMPTY( cbuffer )
         IF AT( "<par>", cBuffer ) > 0 .AND. AT( "</par>", cBuffer ) > 0
? " Estou Aqui"
            IF cStyle == "Arguments"
               creturn := cBuffer
                ? " Estou Aqui 2"
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
                  cReturn := '<par><b>' + cOLine + '</b> ' + cReturn + '    </par>'
               ELSE
                  cReturn := '<par>' + cOLine + ' ' + cReturn + '    </par>'
               ENDIF

               cbuffer := cReturn

            ENDIF

         ELSE
            cBuffer := Formatos2Buff( cBuffer, cStyle, oOs2 )
         ENDIF
      ENDIF
   ENDIF

   IF AT( '<par>', cBuffer ) > 0 .AND. AT( '</par>', cBuffer ) > 0

      cBuffer   := STRTRAN( cBuffer, '<par>', '' )

      cBuffer   := STRTRAN( cBuffer, '<em>', ':hp3. ' )
      cBuffer   := STRTRAN( cBuffer, '</em>', ':ehp3. ' )
      cBuffer   := STRTRAN( cBuffer, '<i>', ':hp1. ' )
      cBuffer   := STRTRAN( cBuffer, '</i>', ':ehp1. ' )
      cBuffer   := STRTRAN( cBuffer, '</color>', '' )
      nColorPos := AT( '<color:', cBuffer )
      IF ncolorpos > 0
         checkos2color( @cbuffer, ncolorpos )
      ENDIF

      IF cStyle == "Description" .OR. cStyle == "Compliance"
         nIdentLevel := 5
         nPos        := 0

         DO WHILE !lendPar
            IF nPos == 0
               cLine := SUBSTR( cBuffer, 1, 220 )
               nPos  := RAT( " ", cLine )
               IF nPos > 0
                  
                  cLine := SUBSTR( cBuffer, 1, nPos )
//                  cline:= Oos2:dostoos2text(cline)
               ENDIF

            ELSE
               cLine := SPACE( nidentLevel ) + SUBSTR( cBuffer, curPos, 220 )

               IF AT( '</par>', cLine ) > 0
                  lEndPar := .T.
                  cLine   := STRTRAN( cLine, " </par>", "" )
               ENDIF
               nPos := RAT( " ", cLine )
               IF nPos > 0
                  cLine := SPACE( nidentLevel ) + SUBSTR( cBuffer, curpos, nPos - nIdentlevel )
                  nPos  -= nIdentlevel
               ELSE
                  IF cLine == "</par>"
                     cLine := ''
                  ENDIF

               ENDIF
            ENDIF

            IF !EMPTY( cLine )
            if AT("<b>",cLine)>0 .or. AT("</b>",cLine)>0
              cLine   := STRTRAN( cLine, '<b>', '' )
              cLine   := STRTRAN( cLine, '</b>', '' )
              oOs2:WriteParBold(cLine,.f.)
            Else
              IF LEFT(CLINE,LEN(SPACE(nidentLevel ))) ==SPACE(nidentLevel )
               oos2:WriteText( cLine )
            Else
             oos2:WriteText(SPACE(nidentLevel )+ cLine )
            endif
            Endif
            oOs2:WriteTExt(".br")
            ENDIF
            curPos += nPos
         ENDDO


      ELSEIF cStyle == "Arguments"
      cBuffer   := STRTRAN( cBuffer, ':', "&colon." )
      cBuffer   := STRTRAN( cBuffer, '<b>', 'hp2.' )
      cBuffer   := STRTRAN( cBuffer, '</b>', 'ehp2.' )
         IF AT( '</par>', cBuffer ) > 0
            cBuffer := STRTRAN( cBuffer, "</par>", "" )
         ENDIF

      cBuffer := STRTRAN( cBuffer, "<", "&lt." )
      cBuffer := STRTRAN( cBuffer, ">", "&gt." )

         IF !EMPTY( cBuffer )
            cBuffer := SUBSTR( cBuffer, 2 )
              cBuffer   := STRTRAN( cBuffer, 'hp2.', ':hp2.' )
              cBuffer   := STRTRAN( cBuffer, 'ehp2.', ':ehp2.' )

            oos2:writetext(cbuffer+CRLF+'.br')

/*a            oOs2:writeText( cBuffer+CRLF+".br") */
         ENDIF

      ELSEIF cStyle == "Syntax"
         IF AT( '</par>', cBuffer ) > 0
            cBuffer := STRTRAN( cBuffer, "</par>", "" )
         ENDIF
         IF !EMPTY( cBuffer )
            cBuffer := SUBSTR( cBuffer, 2 )
            oOs2:WriteParBold( cBuffer,.f.)
            oOs2:writeText('.br')
            oOs2:writeText('.br')
         ENDIF
      ELSEIF cStyle == "Default"
         IF AT( '</par>', cBuffer ) > 0
            cBuffer := STRTRAN( cBuffer, "</par>", "" )
         ENDIF
         IF !EMPTY( cBuffer )
            cBuffer := SUBSTR( cBuffer, 2 )
            oOs2:WritePar( cBuffer )
         ENDIF

      ENDIF
   ENDIF
   IF AT( '<fixed>', cBuffer ) > 0 .OR. cStyle = "Example"

      IF AT( '<fixed>', cBuffer ) = 0 .OR. !EMPTY( cBuffer )
         if AT( '<fixed>', cBuffer ) > 0
            lHasFixed:=.T.
         else
            lHasFixed:=.F.
         Endif

         cBuffer := STRTRAN( cBuffer, "<par>", "" )
         cBuffer := STRTRAN( cBuffer, "<fixed>", "" )
       ENDIF
       if !lHasFixed
         oOs2:WritePar( cBuffer )
         oOs2:WriteTExt(".br")
      endif
      DO WHILE !lendFixed
         cLine := TRIM( SUBSTR( ReadLN( @lEof ), nCommentLen ) )
         IF AT( "</fixed>", cLine ) > 0
            lendfixed := .t.
            cLine     := STRTRAN( cLine, "</fixed>", "" )
         ENDIF
         IF AT( DELIM, cLine ) = 0
            cReturn += ALLTRIM( cLine ) + ' '
         ENDIF
         IF AT( DELIM, cLine ) > 0
            FT_FSKIP( - 1 )
            lEndfixed := .t.

         ENDIF
         IF AT( DELIM, cLine ) == 0  .and. !lendfixed
            oOs2:WritePar( cLine )
            oOs2:WriteTExt(".br")
         ENDIF
      ENDDO

   END
   IF AT( '<table>', cBuffer ) > 0
      DO WHILE !lendTable
         cLine := TRIM( SUBSTR( ReadLN( @lEof ), nCommentLen ) )
         IF AT( "</table>", cLine ) > 0
            lendTable := .t.
         ELSE
            IF LFstTableItem
               nNumTableItems := GetNumberofTableItems( cLine )
               procos2table( cline, nNumTableItems )
               LFstTableItem := .f.
            ELSE
               procos2table( cline, nNumTableItems )
            ENDIF

         ENDIF
      ENDDO
      IF lEndTable
         Genos2Table( oOs2 )
         LFstTableItem := .T.
      ENDIF
   ENDIF

RETURN nil

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

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Static Function GetItem()
*+
*+    Called from ( genng.prg    )  20 - function processing()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
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

*+ EOF: GENOS2.PRG
