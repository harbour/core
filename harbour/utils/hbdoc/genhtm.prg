/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * GENHTM support module for hbdoc document Extractor 
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
MEMVAR aDocInfo
MEMVAR aWww

STATIC aAlso
STATIC aFiTable       := {}
STATIC aSiTable       := {}
STATIC lIsTable       := .F.
STATIC nCommentLen
STATIC lEof
STATIC aFoiTable      := {}
STATIC atiTable       := {}
STATIC nNumTableItems := 0
STATIC aCurDoc        := {}
STATIC nCurDoc := 1
STATIC lWasTestExamples := .f.
STATIC aColorTable := { 'aqua', 'black', 'fuchia', 'grey', 'green', 'lime', 'maroon', 'navy', 'olive', 'purple', 'red', 'silver', 'teal', 'white', 'yellow' }

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function ProcessWww()
*+
*+    Called from ( hbdoc.prg    )   2 - function main()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
FUNCTION ProcessWww()

   //  Copyright (C) 2000 Luiz Rafael Culik
   //
   //  Purpose: Process each of the files in the directory
   //
   //  Modification History:
   //         Version    Date        Who       Notes
   //          V1.00     1/12/2000   LRC       Initial Version
   //
   //  Calling parameters: None
   //
   //  Notes: None
   // -
   //  LOCAL variables:

   LOCAL i
   LOCAL j
   LOCAL nFiles    := LEN( aDirList )
   LOCAL lDoc
   LOCAL lClassDoc
   LOCAL cBuffer
   LOCAL nEnd
   LOCAL nCount

   LOCAL cBar          := REPLICATE( "-", 80 ) + CRLF
   LOCAL nMode
   LOCAL cFuncName
   LOCAL cOneLine
   LOCAL cCategory
   LOCAL cFileName
   LOCAL nLineCnt
   LOCAL cSeeAlso
   LOCAL cTemp
   LOCAL cChar
   LOCAL nPos
   LOCAL lFirstSintax  := .T.
   LOCAL lAddEndPreTag := .F.
   LOCAL lEndDesc      := .F.
   LOCAL lEndArgs      := .F.
   LOCAL lEndSyntax    := .F.
   LOCAL lEndReturns   := .F.
   LOCAL lEndData      := .F.
   LOCAL lDataLink

   LOCAL lBlankLine     := .F.             // Blank line encountered and sent out
   LOCAL lAddBlank      := .F.             // Need to add a blank line if next line is not blank
   LOCAL oHtm
   LOCAL nReadHandle
   LOCAL lEndConstru    := .F.
   LOCAL lFirstPass     := .T.
   LOCAL lFirstArg      := .T.
   LOCAL lData          := .F.
   LOCAL lIsDataLink    := .F.
   LOCAL lIsMethodLink  := .F.
   LOCAL lMethod        := .F.
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
   //
   //  Entry Point
   //
   //  Put up information labels
   @ INFILELINE, 20 SAY "Extracting: "          
   @ MODULELINE, 20 SAY "Documenting: "         
   //  loop through all of the files
   lFirstArg    := .T.
   lFirstPass   := .T.
   lFirstSintax := .T.
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
      lEof      := .F.
      lDoc      := .F.
      lClassDoc := .F.
      //  First find the author
      ReadFromTop( nReadHandle )
      DO WHILE .NOT. lEof

         //  Read a line

         cBuffer := TRIM( SUBSTR( ReadLN( @lEof ), nCommentLen ) )
         cBuffer := STRTRAN( cBuffer, CHR( 10 ), '' )
         nLineCnt ++
         IF nLineCnt % 10 = 0
            @ LINELINE, 33 SAY STR( nLineCnt, 5, 0 )         
         ENDIF
         //  check to see if we are in doc mode or getting out of doc mode

         IF AT( cDoc, cBuffer ) > 0
            IF lDoc
//               WRITE_ERROR( cDoc + " encountered during extraction of Doc" ;
//                            + " at line" + STR( nLinecnt, 5, 0 ),,,, aDirList[ i, F_NAME ] )
            ENDIF
            lDoc    := .T.
            cBuffer := TRIM( SUBSTR( ReadLN( @lEof ), ;
                             nCommentLen ) )
            nLineCnt ++
            cCategory := cFuncName := cSeeAlso := ""
            nMode     := D_IGNORE
         ELSEIF AT( cClassDoc, cBuffer ) > 0
            IF lClassDoc
//               WRITE_ERROR( cDoc + " encountered during extraction of Doc" ;
  //                          + " at line" + STR( nLinecnt, 5, 0 ),,,, aDirList[ i, F_NAME ] )
            ENDIF
            lClassDoc := .T.
            cBuffer   := TRIM( SUBSTR( ReadLN( @lEof ), ;
                               nCommentLen ) )
            nLineCnt ++
            cCategory := cFuncName := cSeeAlso := ""
            nMode     := D_IGNORE

         ELSEIF AT( cEnd, cBuffer ) > 0
                     nCurDoc ++
            IF .NOT. lDoc .AND. !lClassDoc
//               WRITE_ERROR( cEnd + " encountered outside of Doc area at line" ;
  //                          + STR( nLinecnt, 5, 0 ),,,, aDirList[ i, F_NAME ] )
            ELSE
               //  Add a new entry to our list of files

               IF EMPTY( cCategory )
//                  WRITE_ERROR( "Blank Category",,,, aDirList[ i, F_NAME ] )
                  cCategory := "Unknown"
               ENDIF
               IF EMPTY( cFuncName )
                 // WRITE_ERROR( "Blank Function Name",,,, aDirList[ i, F_NAME ] )
                  cFuncName := "Unknown"
               ENDIF
               AADD( aDocInfo, { cCategory, cFuncName, cOneLine, cFileName } )
               //  Now close down this little piece
               IF .NOT. EMPTY( cSeeAlso )

                  oHtm:WriteParBold( "See Also " )
                  oHtm:WriteText( "<UL>" )
                  ProcWwwalso( oHtm, cSeealso )
                  oHtm:WriteText( "</UL></DL>" )
                  IF lDoc
                     oHtm:WriteText( "</DL>" )
                     oHtm:Close()
                  ENDIF
               Elseif empty(cSeeAlso)
                  IF lDoc
                     oHtm:WriteText( "</DL>" )
                     oHtm:Close()
                  ENDIF

               ENDIF
               lDoc      := .F.
               lClassDoc := .F.

               IF lEndReturns .AND. lClassDoc
                  lEndReturns := .f.
                  oHtm:WriteText( "</p></dd>" )
               ENDIF
               IF lEndArgs .AND. lClassDoc
                  lEndArgs := .f.
                  oHtm:WriteText( "</p></dd>" )
               ENDIF

               nMode := D_IGNORE
            ENDIF

            @ MODULELINE, 33 CLEAR TO MODULELINE, MAXCOL()
         ENDIF

         IF lDoc .OR. lClassDoc
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
                  cTemp := UPPER(SUBSTR( cFuncName, 4 ))
               ELSE
                  cTemp := UPPER(cFuncName)
               ENDIF

               IF ( nEnd := AT( "(", cTemp ) ) > 0
                  cTemp := LEFT( cTemp, nEnd - 1 )
               ENDIF
               cFileName := ""

               //  Strip off any other non-alphabetic/numeric characters
               FOR j := 1 TO LEN( cTemp )
                  cChar := SUBSTR( cTemp, j, 1 )
                  IF ( cChar >= "0" .AND. cChar <= "9" ) .OR. ;
                       ( cChar >= "A" .AND. cChar <= "Z" ) .OR. cChar = "_" .or. cchar ="@"
                     cFileName += cChar
                  ENDIF
               NEXT

               //  See if file name is present already. If so then modify

               cFileName := LEFT( cFileName, 36 )
               nEnd      := 1
               nCount    := 0
               DO WHILE nEnd > 0
                  nEnd := ASCAN( aDocInfo, { | a | a[ 4 ] == cFileName + ".htm" } )
                  IF nEnd > 0

                     //  This will break if there are more than 10 files with the same first
                     //  seven characters. We take our chances.

                     IF LEN( cFileName ) = 36
                        cFileName := STUFF( cFileName, 36, 1, STR( nCount, 1, 0 ) )
                     ELSE
                        cFileName += STR( nCount, 1, 0 )
                     ENDIF
                     nCount ++
                  ENDIF
               ENDDO
               //  Add on the extension

               cFileName := LEFT( cFileName, 36 ) + ".htm"
               IF lDoc
                  oHtm := THTML():New( 'htm\' + cFileName )
               ENDIF
               IF lFirstPass .AND. lClassDoc
                  lFirstPass := .F.
                  oHtm       := THTML():New( 'htm\' + cFileName )
               ENDIF
               IF ohtm:nHandle < 1
                  ? "Error creating", cFileName, ".htm"
//                  WRITE_ERROR( "Error creating",,,, cFileName + ".htm" )
               ENDIF

            ELSEIF (AT( cdata, cBuffer ) > 0 .and.  GetItem( cBuffer, nCurdoc ) ).OR. (AT( cmethod, cBuffer ) > 0 .and. GetItem( cBuffer, nCurdoc ) )
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
               IF AT( "(", cfuncname ) > 0
                  cFuncname := SUBSTR( cFuncName, 1, AT( "(", cFuncName ) - 1 )
               ENDIF
               IF lEndDesc .AND. lClassDoc
                  lEndDesc := .f.
                  oHtm:WriteText( "</p></dd>" )
               ENDIF
               ohtm:WriteText( '<br>' )
               ohtm:WriteText( '<br>' )
               ohtm:Writetext( '<hr>' )
               ohtm:WriteText( '<br>' )
               ohtm:WriteText( '<br>' )

               oHtm:WriteText( "<a NAME=" + '"' + ALLTRIM( cFuncname )  + '"' + "></a>" )

               //  2) Category
            ELSEIF AT( cCat, cBuffer ) > 0
               cBuffer := ReadLN( @lEof )
               nLineCnt ++
               //  get the category
               cCategory := ALLTRIM( SUBSTR( cBuffer, nCommentLen ) ) 

               //  3) One line description

            ELSEIF AT( cOne, cBuffer ) > 0
               cBuffer := ReadLN( @lEof )
               nLineCnt ++
               cOneLine := ALLTRIM( SUBSTR( cBuffer, nCommentLen ) )
               IF LEN( cOneLine ) > LONGONELINE
//                  WRITE_ERROR( "OneLine", cOneLine, nLineCnt, LONGONELINE, ;
//                               aDirList[ i, F_NAME ] )
               ENDIF

               nMode := D_ONELINE
               //  Now start writing out what we know

               IF lData
                  oHtm:WriteText( "<H1>DATA " + ALLTRIM(  cFuncName ) + "</H1>" )
                  oHtm:WriteText( "<p>" + cOneline + "</p>" + hb_osnewline() )
               ELSEIF lMethod
                  oHtm:WriteText( "<H1> METHOD " + ALLTRIM( cFuncName ) + "</H1>" )
                  oHtm:WriteText( "<p>" + cOneline + "</p>" + hb_osnewline() )
               ELSE
                  oHtm:WriteText( "<H1>" + ALLTRIM(  cFuncName ) + "</H1>" )
                  AADD( aWWW, { cFuncName, LEFT( cFileName, AT( ".", cFileName ) - 1 ) } )
                  oHtm:WriteText( "<p>" + cOneline + "</p>" + hb_osnewline() )
               ENDIF
               lFirstSintax := .T.
               //  4) all other stuff

            ELSE

               IF AT( cSyn, cBuffer ) > 0
                if GetItem( cBuffer, nCurdoc ) 
                  oHtm:WriteParBold( " Syntax", .f., .f. )
                  ohtm:WriteText( '<DD>' )
                  nMode      := D_SYNTAX
                  lAddBlank  := .T.
                  lEndSyntax := .T.
end
               ELSEIF AT( cConstruct, cBuffer ) > 0
               if GetItem( cBuffer, nCurdoc ) 
                  oHtm:WriteParBold( " Constructor syntax", .F., .f. )
                  ohtm:WriteText( '<DD>' )
                  nMode      := D_SYNTAX
                  lAddBlank  := .T.
                  lEndSyntax := .T.
               end
               ELSEIF AT( cArg, cBuffer ) > 0
                  if GetItem( cBuffer, nCurdoc ) 
                  oHtm:WriteParBold( " Arguments" )
                  ohtm:WriteText( '<DD>' )

                  nMode     := D_ARG
                  lAddBlank := .T.
                  lEndArgs  := .t.
                  end
               ELSEIF AT( cRet, cBuffer ) > 0
                     if GetItem( cBuffer, nCurdoc ) 
                  IF !lBlankLine
                     //                      oHtm:WritePar( "" )
                  ENDIF

                  oHtm:WriteParBold( " Returns" )
                  ohtm:WriteText( '<DD>' )
                  nMode       := D_ARG
                  lAddBlank   := .T.
                  lEndReturns := .t.
                  end
               ELSEIF AT( cDesc, cBuffer ) > 0
               if GetItem( cBuffer, nCurdoc ) 
                  IF !lBlankLine
                     oHtm:WriteText( "<br>" )
                  ENDIF

                  oHtm:WriteParBold( " Description" )
                  ohtm:WriteText( '<DD>' )

                  nMode     := D_DESCRIPTION
                  lAddBlank := .T.
                  lEndDesc  := .t.
               end
               ELSEIF AT( cExam, cBuffer ) > 0
                  if GetItem( cBuffer, nCurdoc ) 
                  IF !lBlankLine
                     //                      oHtm:WritePar( "" )
                  ENDIF
                  oHtm:WriteText( "</dl><b> Examples</b>" )
                  oHtm:WriteText( "<PRE>" )
                  nMode         := D_EXAMPLE
                  lAddBlank     := .T.
                  lAddEndPreTag := .T.
                  lWasTestExamples:=.t.
                  end
               ELSEIF AT( cTest, cBuffer ) > 0
                     if GetItem( cBuffer, nCurdoc ) 
                  IF !lBlankLine
                     //                     oHtm:WritePar( "" )
                  ENDIF
                  oHtm:WriteText( "</dl><b>Tests</b>" )
//                  oHtm:WriteText( "<DD>" )
                  nMode     := D_EXAMPLE
                  lAddBlank := .T.
                  lWasTestExamples:= .t.
                     end
               ELSEIF AT( cStatus, cBuffer ) > 0
                        if GetItem( cBuffer, nCurdoc ) 
                  nMode := D_STATUS
                        end
               ELSEIF AT( cCompl, cBuffer ) > 0
                         if GetItem( cBuffer, nCurdoc ) 
                  IF !lBlankLine
                     //                      oHtm:WritePar( "" )
                  ENDIF
                  oHtm:WriteParBold( " Compliance" )
                  oHtm:WriteText( "<DD>" )
                  nMode     := D_COMPLIANCE
                  lAddBlank := .T.
                         end
               ELSEIF AT( cPlat, cBuffer ) > 0
                            if GetItem( cBuffer, nCurdoc ) 
                  IF !lBlankLine
                     //    oHtm:WritePar( "" )
                  ENDIF
                  oHtm:WriteParBold( " Platforms" )
                  oHtm:WriteText( "<DD>" )
                  nMode     := D_NORMAL
                  lAddBlank := .T.
                  end
               ELSEIF AT( cFiles, cBuffer ) > 0
                     if GetItem( cBuffer, nCurdoc ) 
                  IF !lBlankLine
                     //    oHtm:WritePar( "" )
                  ENDIF
                  oHtm:WriteParBold( " Files" )
                  oHtm:WriteText( "<DD>" )
                  nMode     := D_NORMAL
                  lAddBlank := .T.
                  end
               ELSEIF AT( cFunction, cBuffer ) > 0
                     if GetItem( cBuffer, nCurdoc ) 
                  IF !lBlankLine
                     //    oHtm:WritePar( "" )
                  ENDIF
                  oHtm:WriteParBold( " Functions" )
                  oHtm:WriteText( "<DD>" )
                  nMode     := D_NORMAL
                  lAddBlank := .T.
                     end
               ELSEIF AT( cSee, cBuffer ) > 0
               if GetItem( cBuffer, nCurdoc ) 
                  nMode := D_SEEALSO
                  end
               ELSEIF AT( cInc, cBuffer ) > 0
               if GetItem( cBuffer, nCurdoc ) 
                  nMode := D_INCLUDE
                  end
                  //  All other input is trimmed of comments and sent out
               ELSE

                  //  translate any \$ into $
                  cBuffer := STRTRAN( cBuffer, "\" + DELIM, DELIM )
                  IF nMode = D_SYNTAX
                     IF LEN( cBuffer ) > LONGLINE
//                        WRITE_ERROR( "Syntax", cBuffer, nLineCnt, ;
 //                                    LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     IF lAddBlank
                        lAddBlank := .F.
                     ENDIF
                     prochtmdesc( cbuffer, oHtm, "Syntax" )

                  ELSEIF nMode = D_ARG
                     IF LEN( cBuffer ) > LONGLINE
//                        WRITE_ERROR( "Arguments", cBuffer, nLineCnt, ;
  //                                   LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     IF lAddBlank
                        lAddBlank := .F.
                     ENDIF

                     prochtmdesc( cbuffer, oHtm, "Arguments" )
                  ELSEIF nMode = D_NORMAL
                     IF LEN( cBuffer ) > LONGLINE
//                        WRITE_ERROR( "General", cBuffer, nLineCnt, ;
  //                                   LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )

                     IF lBlankLine
                        oHtm:WriteText( '<br>' )
                        lAddBlank := .F.
                     ENDIF

                     ProcHtmDesc( cBuffer, oHtm )
                  ELSEIF nMode = D_EXAMPLE
                     IF LEN( cBuffer ) > LONGLINE
  //                      WRITE_ERROR( "General", cBuffer, nLineCnt, ;
//                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     prochtmdesc( cBuffer, oHtm, "Example" )
                  ELSEIF nMode = D_DESCRIPTION
                     IF LEN( cBuffer ) > LONGLINE
//                        WRITE_ERROR( "General", cBuffer, nLineCnt, ;
  //                                   LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     prochtmdesc( cBuffer, oHtm, "Description" )

                  ELSEIF nMode = D_SEEALSO
                     IF .NOT. EMPTY( cBuffer )
                        cSeeAlso := StripFiles( ALLTRIM( cBuffer ) )
                     ENDIF
                  ELSEIF nMode = D_INCLUDE
                     //  read next line
                     IF .NOT. EMPTY( cBuffer )
                        IF !lBlankLine
                        ENDIF
                     ENDIF
                  ELSEIF nMode = D_DATALINK
                     IF LEN( cBuffer ) > LONGLINE
//                        WRITE_ERROR( "General", cBuffer, nLineCnt, ;
  //                                   LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     IF lAddBlank
                        lAddBlank := .F.
                     ENDIF
                     cTemp := ALLTRIM( SUBSTR( cBuffer, 1, AT( ":", cBuffer ) - 1 ) )
                     ohtm:WriteText( "<a href=" + cFileName + "#" +  cTemp  + ">" + cBuffer + '</a>' )

                  ELSEIF nMode = D_METHODLINK
                     IF LEN( cBuffer ) > LONGLINE
//                        WRITE_ERROR( "General", cBuffer, nLineCnt, ;
  //                                   LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     IF lAddBlank
                        lAddBlank := .F.
                     ENDIF
                     cTemp := ALLTRIM( SUBSTR( cBuffer, 1, AT( "(", cBuffer ) - 1 ) )
                     ohtm:WriteText( "<a href=" + cFileName + "#" +  cTemp  + ">" + cBuffer + '</a>' )

                  ELSEIF nMode = D_COMPLIANCE
                     IF LEN( cBuffer ) > LONGLINE
//                        WRITE_ERROR( "General", cBuffer, nLineCnt, ;
  //                                   LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     prochtmdesc( cBuffer, oHtm, "Compliance" )

                  ELSEIF nMode = D_STATUS
                     IF !EMPTY( cBuffer )
                        If lWasTestExamples
                           oHtm:WriteParBold( "Status",.t.,.f. )
                        Else
                        oHtm:WriteParBold( "Status" )
                        oHtm:WriteText( "<DD>" )
                        Endif
                        lWasTestExamples:=.f.
                     ENDIF
                     ProcStatusWww( oHtm, cBuffer )

                  ELSE

                     //  unknown data from somewhere

//                     WRITE_ERROR( "Unknown Data Type " + cBuffer,, ;
//                                  nLineCnt, ;
//                                  LONGONELINE, aDirList[ i, F_NAME ] )

                  ENDIF
               ENDIF

               ////////////////////

            ENDIF
         ENDIF

         IF !lClassDoc .AND. lEof
            IF VALTYPE( oHtm ) == "O"
               oHtm:WriteText( '</p></dd></dl>' )
               oHtm:Close()
            ENDIF

         ENDIF

      ENDDO
      //  Close down the input file

      FT_FUSE()
      IF lClassDoc
         oHtm:Close()
      ENDIF
      nCurDoc := 1
      aCurDoc := {}

   NEXT
RETURN nil
/***********************************
* Function ProcWwwBuf(cBuffer)   -> cTemp
* Parameter cBuffer  -> Strip the "<" and ">" symbols from the imput String
* Return    cTemp  Formated String to WWW output
*/

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function ProcWwwBuf()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
FUNCTION ProcWwwBuf( cPar )

   cPar := STRTRAN( cPar, "<", "&lt;" )
   cPar := STRTRAN( cPar, ">", "&gt;" )

RETURN cPar

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function ProcWwwAlso()
*+
*+    Called from ( genhtm.prg   )   1 - function processwww()
*+                ( genhtm1.prg  )   1 - function processwww()
*+                ( genhtm2.prg  )   1 - function processwww()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
FUNCTION ProcWwwAlso( nWriteHandle, cSeeAlso )

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

            cTemp := xTemp
         ELSE
            xPos := AT( " ", xTemp )
            IF xPos > 0
               nLen  -= LEN( xTemp ) + 3
               cTemp := xTemp
               xTemp := SUBSTR( xTemp, 1, xPos - 1 ) + SUBSTR( xTemp, xPos + 1 )

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
               nLen  -= LEN( xTemp ) + 3
               cTemp := xTemp
               xTemp := SUBSTR( xTemp, 1, xPos - 1 ) + SUBSTR( xTemp, xPos + 1 )

            ELSE
               nLen -= LEN( xTemp ) + 2

               cTemp := xTemp
            END
         END

      ENDIF

      nWriteHandle:WriteLink( ALLTRIM( xTemp ), cTemp )
      cSeeAlso := SUBSTR( cSeeAlso, nPos + 1 )

      IF nLen == 0 .OR. nLen < 0
         EXIT
      END
   ENDDO
RETURN nil

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function ProcStatusWww()
*+
*+    Called from ( genhtm.prg   )   1 - function processwww()
*+                ( genhtm1.prg  )   1 - function processwww()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
FUNCTION ProcStatusWww( nWriteHandle, cBuffer )

   IF LEN( ALLTRIM( cBuffer ) ) > 1
      nWriteHandle:Writepar( cBuffer )
   ELSEIF SUBSTR( ALLTRIM( cBuffer ), 1 ) == "R"
      nWriteHandle:Writepar( "   Ready" )
   ELSEIF SUBSTR( ALLTRIM( cBuffer ), 1 ) == "S"
      nWriteHandle:Writepar( "   Started" )
   ELSEIF SUBSTR( ALLTRIM( cBuffer ), 1 ) == "C"
      nWriteHandle:Writepar( "   Clipper" )
   ELSE
      nWriteHandle:Writepar( "   Not Started" )
   ENDIF

RETURN nil

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function FormatHtmBuff()
*+
*+    Called from ( genhtm.prg   )   1 - function prochtmdesc()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
FUNCTION FormatHtmBuff( cBuffer, cStyle, oHtm )

   LOCAL creturn    := ''
   LOCAL cline      := ''
   LOCAL cOldLine   := ''
   LOCAL cBuffend   := ''
   LOCAL lEndBuffer := .f.
   LOCAL lArgBold   := .f.
   LOCAL npos
   LOCAL nposend
   creturn := cBuffer + ' '
   IF AT( '</par>', creturn ) > 0 .OR. EMPTY( cBuffer )
      IF EMPTY( cbuffer )
         creturn := ''
      ENDIF
      RETURN creturn
   ENDIF
   IF cStyle != "Syntax" .AND. cStyle != "Arguments"
      DO WHILE !lEndBuffer
         cLine := TRIM( SUBSTR( ReadLN( @lEof ), nCommentLen ) )
         IF AT( '</par>', cLine ) > 0
            lEndBuffer := .t.
         ENDIF

         IF EMPTY( cLine )
            lEndBuffer := .t.
            //            TheBlank   := .t.
            FT_FSKIP( - 1 )
         ENDIF
         IF AT( DELIM, cLine ) > 0

            FT_FSKIP( - 1 )
            lEndBuffer := .t.
         ENDIF
         IF AT( DELIM, cLine ) = 0
            cReturn += ' ' + ALLTRIM( cLine ) + ' '
         ENDIF
      ENDDO
      cReturn := STRTRAN( cReturn, "<par>", "" )
      cReturn := STRTRAN( cReturn, "</par>", "" )

      cReturn := '<par>' + cReturn + '    </par>'

   ELSEIF cStyle == 'Syntax'
      cReturn := STRTRAN( cReturn, "<par>", "" )
      cReturn := STRTRAN( cReturn, "<", "&lt;" )
      cReturn := STRTRAN( cReturn, ">", "&gt;" )
      cReturn := AllTrim(cReturn)
      creturn := '<par><b>' + creturn + ' </b></par>'
   ELSEIF cStyle == 'Arguments'

      nPos := 0
      IF AT( "<par>", cReturn ) > 0 .and. at('<b>',cReturn)=0
         cReturn  := STRTRAN( cReturn, "<par>", "" )
         cReturn  := STRTRAN( cReturn, "</par>", "" )
         cReturn  := ALLTRIM( cReturn )
         nPos     := AT( " ", cReturn )
         cOldLine := LEFT( cReturn, nPos - 1 )
         cReturn  := STRTRAN( cReturn, cOldLine, "" )
         IF AT( "@", cOldLine ) > 0 .OR. AT( "()", cOldLine ) > 0 .OR. AT( "<", cOldLine ) > 0 .OR. AT( "_", cOldLine ) > 0
            cOldLine := STRTRAN( cOldLine, "<", "&lt;" )
            cOldLine := STRTRAN( cOldLine, ">", "&gt;" )
            lArgBold := .T.
         ENDIF
       elseif AT( "<par>", cReturn ) > 0 .and. at('<b>',cReturn)>0
         cReturn  := STRTRAN( cReturn, "<par>", "" )
         cReturn  := STRTRAN( cReturn, "</par>", "" )
         cReturn  := ALLTRIM( cReturn )
         nPos     := AT( '</b>', cReturn )
         cOldLine := LEFT( cReturn, nPos + 3 )
         cReturn  := STRTRAN( cReturn, cOldLine, "" )
         IF AT( "@", cOldLine ) > 0 .OR. AT( "()", cOldLine ) > 0 .OR. AT( "<", cOldLine ) > 0 .OR. AT( "_", cOldLine ) > 0
            cOldLine := STRTRAN( cOldLine, "<b>", "" )
            cOldLine := STRTRAN( cOldLine, "</b>", "" )
            cOldLine := STRTRAN( cOldLine, "<", "&lt;" )
            cOldLine := STRTRAN( cOldLine, ">", "&gt;" )
            lArgBold := .T.
         ENDIF
  
      ENDIF


      DO WHILE !lEndBuffer

         cLine := TRIM( SUBSTR( ReadLN( @lEof ), nCommentLen ) )
         IF AT( "</par>", cLine ) > 0
            lEndBuffer := .t.
         ENDIF
         IF EMPTY( cLine )
            lEndBuffer := .t.
            //            TheBlank   := .t.
            FT_FSKIP( - 1 )

         ENDIF
         IF AT( DELIM, cLine ) > 0
            FT_FSKIP( - 1 )
            lEndBuffer := .t.
         ENDIF
         IF AT( DELIM, cLine ) = 0
            cReturn += ' ' + ALLTRIM( cLine ) + ' '
         ENDIF
      ENDDO
      cReturn := STRTRAN( cReturn, "<par>", "" )
      cReturn := STRTRAN( cReturn, "</par>", "" )

      cReturn  := STRTRAN( cReturn, "<", "&lt;" )
      cReturn  := STRTRAN( cReturn, ">", "&gt;" )
      cOldLine := STRTRAN( cOldLine, "<", "&lt;" )
      cOldLine := STRTRAN( cOldLine, ">", "&gt;" )

      IF lArgBold
         cReturn := '       <par><b>' + cOldLine + '</b> ' + cReturn + '    </par>'
      ELSE
         cReturn := '       <par>' + cOldLine + ' ' + cReturn + '    </par>'
      ENDIF
      //   ENDIF
      lArgBold := .F.

   ENDIF

   //   endif
RETURN creturn

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function checkhtmcolor()
*+
*+    Called from ( genhtm.prg   )   1 - function prochtmdesc()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
FUNC checkhtmcolor( cbuffer, ncolorpos )

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
      nreturn         := ASCAN( acolortable, { | x, y | UPPER( x ) == UPPER( ccolor ) } )
      IF nreturn > 0
         creturn := '<font color=' + acolortable[ nreturn ] + '>'
      ENDIF
      cBuffer := STRTRAN( cBuffer, cOldColorString, cReturn )
   ENDDO
RETURN cbuffer

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function ProchtmDesc()
*+
*+    Called from ( genhtm.prg   )   6 - function processwww()
*+                ( genhtm1.prg  )   6 - function processwww()
*+                ( genhtm2.prg  )   6 - function processwww()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
FUNCTION ProchtmDesc( cBuffer, oHtm, cStyle )

   LOCAL cOldLine      := ''
   LOCAL npos
   LOCAL lHasFixed     := .F.
   LOCAL CurPos        :=  0
   LOCAL nColorPos
   LOCAL ccolor        := ''
   LOCAL creturn       := ''
   LOCAL ncolorend
   LOCAL nIdentLevel
   LOCAL lEndPar       := .F.
   LOCAL cLine         := ''
   LOCAL lEndFixed     := .F.
   LOCAL lArgBold      := .f.
   LOCAL LFstTableItem := .T.
   LOCAL lEndTable     := .F.
   LOCAL lEndBuffer    := .f.
   DEFAULT cStyle TO "Default"

   IF AT( '<par>', cBuffer ) == 0 .AND. !EMPTY( cBuffer ) .AND. cstyle <> "Example"
      cBuffer := '<par>' + cBuffer
   ENDIF

   IF EMPTY( cBuffer )
      oHtm:WriteText( "<dd><br></dd>" )
   ENDIF

   IF cStyle <> "Example" .AND. AT( "<table>", cBuffer ) == 0 .AND. AT( "<fixed>", cBuffer ) = 0
      IF AT( "<par>", cBuffer ) >= 0 .OR. AT( "</par>", cBuffer ) = 0 .AND. !EMPTY( cbuffer )
         IF AT( "<par>", cBuffer ) > 0 .AND. AT( "</par>", cBuffer ) > 0
            IF cStyle == "Arguments"

               creturn := cBuffer

               cReturn := STRTRAN( cReturn, "<par>", "" )
               cReturn := STRTRAN( cReturn, "</par>", "" )

               cReturn  := ALLTRIM( cReturn )
               nPos     := AT( " ", cReturn )
               cOldLine := LEFT( cReturn, nPos - 1 )
               cReturn  := STRTRAN( cReturn, cOldLine, "" )
               IF AT( "@", cOldLine ) > 0 .OR. AT( "()", cOldLine ) > 0 .OR. AT( "<", cOldLine ) > 0 .OR. AT( "_", cOldLine ) > 0
                  lArgBold := .T.
                  cOldLine := STRTRAN( cOldLine, "<", "&lt;" )
                  cOldLine := STRTRAN( cOldLine, ">", "&gt;" )

               ENDIF
               IF lArgBold
                  cReturn := '       <par><b>' + cOldLine + '</b> ' + cReturn + '    </par>'
               ELSE
                  cReturn := '       <par>' + cOldLine + ' ' + cReturn + '    </par>'
               ENDIF

               cbuffer := cReturn
            ENDIF
         ELSE
            cBuffer := FormathtmBuff( cBuffer, cStyle, oHtm )
         ENDIF
      ENDIF
   ENDIF

   IF AT( '<par>', cBuffer ) > 0 .AND. AT( '</par>', cBuffer ) > 0
      cBuffer   := STRTRAN( cBuffer, '<par>', '' )
      cBuffer   := STRTRAN( cBuffer, '</color>', '</font> ' )
      nColorPos := AT( '<color:', cBuffer )
      IF ncolorpos > 0
         checkhtmcolor( @cbuffer, ncolorpos )
      ENDIF
      //      Alltrim(cBuffer)
      IF cStyle == "Description" .OR. cStyle == "Compliance"
         nIdentLevel := 6
         nPos        := 0
         IF AT( '</par>', cBuffer ) > 0
            cBuffer := STRTRAN( cBuffer, "</par>", "" )
         ENDIF
         IF !EMPTY( cBuffer )
            //             cBuffer:=SUBSTR(cBuffer,2)
            cBuffer := STRTRAN( cBuffer, "<", "&lt;" )
            cBuffer := STRTRAN( cBuffer, ">", "&gt;" )

            cBuffeR := ALLTRIM( cBuffer )
            oHtm:WritePar( cBuffer )
         ENDIF

      ELSEIF cStyle == "Arguments"

         IF AT( '</par>', cBuffer ) > 0
            cBuffer := STRTRAN( cBuffer, "</par>", "" )
         ENDIF
         IF !EMPTY( cBuffer )
            cBuffeR := ALLTRIM( cBuffer )
            oHtm:WritePar( cBuffer )
         ENDIF

      ELSEIF cStyle == "Syntax"
         IF AT( '</par>', cBuffer ) > 0
            cBuffer := STRTRAN( cBuffer, "</par>", "" )
            cBuffer := STRTRAN( cBuffer, "<par>", "" )
         ENDIF
         IF !EMPTY( cBuffer )
            //                    cBuffer:=SUBSTR(cBuffer,2)
            cBuffeR := ALLTRIM( cBuffer )
            oHtm:WritePar( cBuffer )
         ENDIF

      ELSEIF cStyle == "Default"
         IF AT( '</par>', cBuffer ) > 0
            cBuffer := STRTRAN( cBuffer, "</par>", "" )
         ENDIF
         IF !EMPTY( cBuffer )
            //                  cBuffer:=SUBSTR(cBuffer,2)
            cBuffeR := ALLTRIM( cBuffer )
            oHtm:WritePar( cBuffer )
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

//         oHtm:WriteText( "<br>" )
         if !lHasFixed
         oHtm:WriteText( cBuffer )
         Endif
      ENDIF
      DO WHILE !lendFixed
         cOldLine := TRIM( SUBSTR( ReadLN( @lEof ), nCommentLen ) )
         IF AT( "</fixed>", cOldLine ) > 0
            lendfixed := .t.
            cOldLine  := Alltrim(STRTRAN( cOldLine, "</fixed>", "" ))          

         ENDIF
         IF AT( DELIM, cOldLine ) = 0
            cReturn += ALLTRIM( cOldLine ) + ' '
         ENDIF
         IF AT( DELIM, cOldLine ) > 0
            FT_FSKIP( - 1 )
            lEndfixed := .t.

         ENDIF

         IF AT( DELIM, cOldLine ) == 0  .and. !lendfixed
            oHtm:WriteText( cOldLine )
         ENDIF
      ENDDO
//      oHtm:WriteText( "</pre><br>" )
   lHasFixed:=.f.
   END
   IF AT( '<table>', cBuffer ) > 0
      DO WHILE !lendTable
         cLine := TRIM( SUBSTR( ReadLN( @lEof ), nCommentLen ) )
         IF AT( "</table>", cLine ) > 0
            lendTable := .t.
         ELSE
            IF LFstTableItem
               nNumTableItems := GetNumberofTableItems( cLine )
               prochtmtable( cline, nNumTableItems )
               LFstTableItem := .f.
            ELSE
               prochtmtable( cline, nNumTableItems )
            ENDIF

         ENDIF
      ENDDO

      IF lEndTable
         GenhtmTable( oHtm )
        LFstTableItem := .T.
      ENDIF
   ENDIF
RETURN nil

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function ProchtmTable()
*+
*+    Called from ( genhtm.prg   )   2 - function prochtmdesc()
*+                ( genhtm1.prg  )   1 - function prochtmdesc()
*+                ( genhtm2.prg  )   1 - function prochtmdesc()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
FUNCTION ProchtmTable( cBuffer, nNum )

   LOCAL nPos
   LOCAL cItem
   LOCAL cItem2
   LOCAL cItem3
   LOCAL nColorpos
   LOCAL cColor
   LOCAL cItem4

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
      nColorpos := ASCAN( aColorTable, { | x, y | UPPER( x ) == UPPER( ccolor ) } )
      cColor    := aColortable[ nColorPos ]
   ENDIF
   IF EMPTY( cBuffer )
      citem  := ''
      citem2 := ''
      citem3 := ''
      citem4 := ''
   ELSE
      cBuffer := STRTRAN( cBuffer, "<", "&lt;" )
      cBuffer := STRTRAN( cBuffer, ">", "&gt;" )

      cItem   := SUBSTR( cBuffer, 1, AT( SPACE( 3 ), cBuffer ) - 1 )
      cBuffer := ALLTRIM( STRTRAN( cBuffer, cItem, "" ,,1) )
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

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function GenhtmTable()
*+
*+    Called from ( genhtm.prg   )   1 - function prochtmdesc()
*+                ( genhtm1.prg  )   1 - function prochtmdesc()
*+                ( genhtm2.prg  )   1 - function prochtmdesc()
*+
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
FUNCTION GenhtmTable( oHtm )

   LOCAL x
/*   oHtm:WriteText( "<br>" )
   oHtm:WriteText( "<br>" )*/
   oHtm:WriteText( '<table border=1>' )                     //-4

   FOR x := 1 TO LEN( asitable )
      IF !EMPTY( asitable[ x ] )
         IF nNumTableItems == 2
            oHtm:WriteText( '<tr><td>' + afitable[ x ] + '</td><td>' + asitable[ x ] + '</td></tr> ' )
         ELSEIF nNumTableItems == 3
            oHtm:WriteText( '<tr><td>' + afitable[ x ] + '</td><td>' + asitable[ x ] + '</td><td>' + atitable[ x ] + '</td></tr> ' )
         ELSEIF nNumTableItems == 4
            oHtm:WriteText( '<tr><td>' + afitable[ x ] + '</td><td>' + asitable[ x ] + '</td><td>' + atitable[ x ] + '</td><td>' + afoitable[ x ] + '</td></tr> ' )
         ENDIF
      ELSE
         oHtm:WriteText( '<tr><td></td></tr> ' )
      ENDIF
   NEXT

   oHtm:Writetext( "</table>" )

   oHtm:WriteText( "<br>" )
   afiTable  := {}
   asitable  := {}
   atitable  := {}
   afoitable := {}

RETURN Nil

*+ EOF: GENHTM.PRG
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
*+    Called from ( genng.prg    )  20 - function processing()
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

*+ EOF: GENNG.PRG
