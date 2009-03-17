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

#pragma -w2
#pragma linenumber=on

#include "directry.ch"
#include "fileio.ch"
#include "inkey.ch"
#include "common.ch"

#include "hbdocdef.ch"

#include "hbclass.ch"

#define DELIM   "$"                 // keyword delimiter

#define CRLF HB_OSNewLine()

//  output lines on the screen
#define INFILELINE   10
#define MODULELINE   12
#define LINELINE     14
#define ERRORLINE    20
#define LONGLINE     600
#define LONGONELINE  86
MEMVAR aDirList
MEMVAR aDocInfo, aDocwwwInfo
MEMVAR aWww,aResult

STATIC aAlso
STATIC aFiTable       := {}
STATIC aSiTable       := {}
STATIC lIsTable       := .F.
STATIC nCommentLen
STATIC lEof
STATIC aFoiTable      := {}
STATIC atiTable       := {}
STATIC nNumTableItems := 0
STATIC aCurDoc        := {} // x
static cLastText      := ""
Static clastBuffer    := ""
STATIC nCurDoc := 1
STATIC lWasTestExamples := .F.
STATIC aColorTable := { "aqua", "black", "fuchia", "grey", "green", "lime", "maroon", "navy", "olive", "purple", "red", "silver", "teal", "white", "yellow" }

// Static variables added for the htm2 addition
STATIC cDocType       := ""
STATIC nArrayItem := 1      // Index used to loop through the array of the array
STATIC nArrayItemLine := 1  // Index used to loop through the items in the array of the array
STATIC oHtmClass
STATIC oHtmClassContent
STATIC cFileName := ""      // Stores filenames of files that will be created
STATIC cInherits := ""      // Stores the inheritance of a class (if known and present)

*+--------------------------------------------------------------------
*+
*+    Function ProcessWww()
*+
*+    Called from ( hbdoc.prg    )   2 - function main()
*+
*+--------------------------------------------------------------------
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

   LOCAL cBar          := REPLICATE( "-", 80 ) + hb_OSNewLine()
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
   LOCAL nDocs:=0
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

      nCommentLen := IIF( AT( ".asm", Lower( aDirList[ i, F_NAME ] ) ) > 0, 2, 4 )
      nReadHandle := FT_FUSE( aDirList[ i, F_NAME ] )
      #ifdef GAUGE
      if i == 1
        aGauge1:= Gauge():New( 5, 5, 7, MaxCol() - 5)
       Endif
     #endif
     #ifdef GAUGE
     aGauge1:Update(i/nFiles, "Current file: "+PAD(aDirList[ i, F_NAME ], 47))
     #else

      @ INFILELINE, 33 CLEAR TO INFILELINE, MAXCOL()
      @ INFILELINE, 33 SAY PAD( aDirList[ i, F_NAME ], 47 )
      @ MODULELINE, 33 CLEAR TO LINELINE, MAXCOL()
      @ LINELINE, 27   SAY "Line:"
      #endif
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
      #ifdef GAUGE
      if nDocs == 0

        aGauge:= Gauge():New( 8, 5, 10, MaxCol() - 5)

       Endif
       ReadFromTop( nReadHandle )
       nDocs:=CountDocs(aCurDoc)
      #else
            ReadFromTop( nReadHandle )
      #endif
      DO WHILE .NOT. lEof

         //  Read a line

         cBuffer := TRIM( SUBSTR( ReadLN( @lEof ), nCommentLen ) )
         cBuffer := STRTRAN( cBuffer, CHR( 10 ), "" )
         nLineCnt ++

         IF nLineCnt % 10 == 0
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
/*                     nDocs:=0*/
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
               nPos := ascan(aResult,{|a| UPPER(a) == UPPER(cCategory)})
               if nPos==0
                  if cCategory!="Document"
                    aadd(aResult,cCategory)
                  endif
               endif

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
               lData   := .F.
               lMethod := .F.
               IF lEndReturns .AND. lClassDoc
                  lEndReturns := .F.
                  oHtm:WriteText( "</pre>" )
               ENDIF
               IF lEndArgs .AND. lClassDoc
                  lEndArgs := .F.
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
               #ifdef GAUGE
                  aGauge:update(nCurdoc/nDocs,"Current Doc:"+cFuncName )
#else

               @ MODULELINE, 33 CLEAR TO MODULELINE, MAXCOL()
               @ MODULELINE, 33 SAY cFuncName
#endif
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
                       ( cChar >= "A" .AND. cChar <= "Z" ) .OR. cChar == "_"
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

                     IF LEN( cFileName ) == 36
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
                  oHtm := THTML():New( "htm\" + cFileName )
               ENDIF
               IF lFirstPass .AND. lClassDoc
                  lFirstPass := .F.
                  oHtm       := THTML():New( "htm\" + cFileName )
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
                  lEndDesc := .F.
                  if lWasTestExamples
                  oHtm:WriteText( "</pre>" )
                  else
                  oHtm:WriteText( "</p></dd>" )
                  lWasTestExamples:=.F.
                  endif

               ENDIF
               ohtm:WriteText( "<br>" )
               ohtm:WriteText( "<br>" )
               ohtm:Writetext( "<hr>" )
               ohtm:WriteText( "<br>" )
               ohtm:WriteText( "<br>" )

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
                  oHtm:WriteText( "<H1>"+ LEFT( cFileName, AT( ".", cFileName ) - 1 )+ ":" + ALLTRIM( cFuncName ) + "</H1>" )
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
                  oHtm:WriteParBold( " Syntax", .F., .F. )
                  ohtm:WriteText( "<DD>" )
                  nMode      := D_SYNTAX
                  lAddBlank  := .T.
                  lEndSyntax := .T.
                endif
               ELSEIF AT( cConstruct, cBuffer ) > 0
               if GetItem( cBuffer, nCurdoc )
                  oHtm:WriteParBold( " Constructor syntax", .F., .F. )
                  ohtm:WriteText( "<DD>" )
                  nMode      := D_SYNTAX
                  lAddBlank  := .T.
                  lEndSyntax := .T.
               end
               ELSEIF AT( cArg, cBuffer ) > 0
                  if GetItem( cBuffer, nCurdoc )
                  oHtm:WriteParBold( " Arguments" )
                  ohtm:WriteText( "<DD>" )

                  nMode     := D_ARG
                  lAddBlank := .T.
                  lEndArgs  := .T.
                  end
               ELSEIF AT( cRet, cBuffer ) > 0
                     if GetItem( cBuffer, nCurdoc )
                  IF !lBlankLine
                     //                      oHtm:WritePar( "" )
                  ENDIF

                  oHtm:WriteParBold( " Returns" )
                  ohtm:WriteText( "<DD>" )
                  nMode       := D_ARG
                  lAddBlank   := .T.
                  lEndReturns := .T.
                  end
               ELSEIF AT( cDesc, cBuffer ) > 0
               if GetItem( cBuffer, nCurdoc )
                  IF !lBlankLine
                     oHtm:WriteText( "<br>" )
                  ENDIF

                  oHtm:WriteParBold( " Description" )
                  ohtm:WriteText( "<DD>" )

                  nMode     := D_DESCRIPTION
                  lAddBlank := .T.
                  lEndDesc  := .T.
               end
               ELSEIF AT( cdatalink, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     IF !lBlankLine
oHtm:writeText("<br>")  //:endpar()
//                     oHtm:WriteParBold( " Data" )
                        oHtm:WriteText( "</dl><dl><dt><b>Data</b></dt>" )
//oHtm:writeText("<br>")  //:endpar()
                     endif
                     nMode     := D_DATALINK
  //                   lAddBlank := .T.

                     lIsDataLink := .T.
                  END
               ELSEIF AT( cDatanolink, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     IF !lIsDataLink
                    oHtm:writeText("<br>")                //:endpar()
                        oHtm:WriteText( "</dl><dl><dt><b>Data</b></dt>" )
//                    oHtm:writeText("<br>")                //:endpar()
                     ENDIF
                     nMode     := D_NORMAL
                     lAddBlank := .T.

                  END
               ELSEIF AT( cMethodslink, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                 oHtm:writeText("<br>")                //:endpar()
                     oHtm:WriteParBold( " Method" )
//oHtm:writeText("<br>")  //:endpar()
                     nMode     := D_METHODLINK
                     lAddBlank := .T.

                     lIsMethodLink := .T.
                  END
               ELSEIF AT( cMethodsnolink, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     IF !lIsMethodLink
                    oHtm:writeText("<br>")                //:endpar()
                        oHtm:WriteParBold( " Methods" )
                       oHtm:writepar("<br>")                //:endpar()
                     ENDIF
//                       oHtm:writeText("<br>")                //:endpar()
                     nMode     := D_NORMAL
                     lAddBlank := .T.

                  END

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
                  lWasTestExamples:=.T.
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
                  lWasTestExamples:= .T.
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
                  IF nMode == D_SYNTAX
                     IF LEN( cBuffer ) > LONGLINE
//                        WRITE_ERROR( "Syntax", cBuffer, nLineCnt, ;
 //                                    LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     IF lAddBlank
                        lAddBlank := .F.
                     ENDIF
                     prochtmdesc( cbuffer, oHtm, "Syntax" ,cFileName)

                  ELSEIF nMode == D_ARG
                     IF LEN( cBuffer ) > LONGLINE
//                        WRITE_ERROR( "Arguments", cBuffer, nLineCnt, ;
  //                                   LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     IF lAddBlank
                        lAddBlank := .F.
                     ENDIF

                     prochtmdesc( cbuffer, oHtm, "Arguments" )
                  ELSEIF nMode == D_NORMAL
                     IF LEN( cBuffer ) > LONGLINE
//                        WRITE_ERROR( "General", cBuffer, nLineCnt, ;
  //                                   LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )

                     IF lBlankLine
                        oHtm:WriteText( "<br>" )
                        lAddBlank := .F.
                     ENDIF

                     ProcHtmDesc( cBuffer, oHtm )
                  ELSEIF nMode == D_EXAMPLE
                     IF LEN( cBuffer ) > LONGLINE
  //                      WRITE_ERROR( "General", cBuffer, nLineCnt, ;
//                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     prochtmdesc( cBuffer, oHtm, "Example" )
                  ELSEIF nMode == D_DESCRIPTION
                     IF LEN( cBuffer ) > LONGLINE
//                        WRITE_ERROR( "General", cBuffer, nLineCnt, ;
  //                                   LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     prochtmdesc( cBuffer, oHtm, "Description" )

                  ELSEIF nMode == D_SEEALSO
                     IF .NOT. EMPTY( cBuffer )
                        cSeeAlso := StripFiles( ALLTRIM( cBuffer ) )
                     ENDIF
                  ELSEIF nMode == D_INCLUDE
                     //  read next line
                     IF .NOT. EMPTY( cBuffer )
                        IF !lBlankLine
                        ENDIF
                     ENDIF
                  ELSEIF nMode == D_DATALINK
                     IF LEN( cBuffer ) > LONGLINE
//                        WRITE_ERROR( "General", cBuffer, nLineCnt, ;
  //                                   LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     IF lAddBlank
                        lAddBlank := .F.
                     ENDIF
                     cTemp := ALLTRIM( SUBSTR( cBuffer, 1, AT( ":", cBuffer ) - 1 ) )
                     ohtm:WriteText( "<dd><a href=" + cFileName + "#" +  cTemp  + ">" + cBuffer + "</a></dd>" )
                     ohtm:writetext("<dd><br></dd>")
                  ELSEIF nMode == D_METHODLINK
                     IF LEN( cBuffer ) > LONGLINE
//                        WRITE_ERROR( "General", cBuffer, nLineCnt, ;
  //                                   LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     IF lAddBlank
                        lAddBlank := .F.
                     ENDIF
                     cTemp := ALLTRIM( SUBSTR( cBuffer, 1, AT( "(", cBuffer ) - 1 ) )
                     if !lBlankline
                     ohtm:WriteText( "<dd><a href=" + cFileName + "#" +  cTemp  + ">" + cBuffer + "</a></dd>" )
                     ohtm:writetext("<dd><br></dd>")
                    endif
                  ELSEIF nMode == D_COMPLIANCE
                     IF LEN( cBuffer ) > LONGLINE
//                        WRITE_ERROR( "General", cBuffer, nLineCnt, ;
  //                                   LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     prochtmdesc( cBuffer, oHtm, "Compliance" )

                  ELSEIF nMode == D_STATUS
                     IF !EMPTY( cBuffer )
                        If lWasTestExamples
                           oHtm:WriteParBold( "Status",.T.,.F. )
                        Else
                        oHtm:WriteParBold( "Status" )
                        oHtm:WriteText( "<DD>" )
                        Endif
                        lWasTestExamples:=.F.
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
            IF ISOBJECT( oHtm )
               oHtm:WriteText( "</p></dd></dl>" )
               oHtm:Close()
            ENDIF

         ENDIF

      ENDDO
      //  Close down the input file

      FT_FUSE()
        nDocs:=0
      IF lClassDoc
         oHtm:Close()
      ENDIF
      nCurDoc := 1
      aCurDoc := {}

   NEXT
RETURN nil


FUNCTION ProcessWww2()
   LOCAL aTempArray := {}         // Temporary array used for storing list of methods and properties
   LOCAL cTempString              // Temporary string used for storing random strings
   LOCAL cTempString2             // Temporary string used for storing random strings
   LOCAL nClassNamePos            // Stores the position in the array where the classname can be found
   LOCAL nFunctionNamePos         // Stores the position in the array where the functionname can be found
   LOCAL nReadHandle              // Stores the text file's workarea
   LOCAL nSubArrayItem := 1       // Index used to loop through the items of the array of the array
   LOCAL nTemp1 := 1              // Used to scroll through the DirList array
   LOCAL nTemp2 := 1              // Used to scroll through the aFunctionItems array
   LOCAL nPropertyListAnker       // Used to add an html-anker in the page
   LOCAL nMethodListAnker         // Used to add an html-anker in the page
   LOCAL aFunctionItems := {{"Category", "Category"}, {"Syntax", "Syntax"}, {"Arguments", "Arguments"}, ;
      + {"Returns", "Returns"}, {"Description", "Description"}, {"Examples", "Examples"}, {"Tests", "Tests"}, ;
      + {"Status", "Status"}, {"Compliance", "Compliance"}, {"Platforms", "Platforms"}, {"Files", "Files"}, ;
      + {"Seealso", "See also"}}
   LOCAL cTemp
   LOCAL nEnd
   LOCAL j
   LOCAL nCount
   LOCAL cChar

   FOR nTemp1 := 1 TO LEN(aDirList)
      // Calculate the amount of characters the source files uses each line
      nCommentLen := IIF(AT(".asm", Lower(aDirList[nTemp1, F_NAME])) > 0, 2, 4)
      // Select the file to read from
      nReadHandle := FT_FUSE(aDirList[nTemp1, F_NAME])

      IF nReadHandle < 0
         WRITE_ERROR("Can't open file: (Dos Error " + STR( FERROR() ) + ")",,,, aDirList[ nTemp1, F_NAME ])
         @ ERRORLINE,  0 CLEAR TO ERRORLINE, MAXCOL()
         @ ERRORLINE, 20 SAY "Can't open file: (Dos Error " + STR( FERROR() ) + ") File=" + aDirList[ nTemp1, F_NAME ]
         LOOP
      ENDIF

      // Read the entire file into the aCurDoc array
      ReadFromTop2(nReadHandle)

      // Generate a new document, based on the name of the class or function
      // Check if the file contains a class or function definition
      IF LEN(aCurDoc) > 0 .AND. nArrayItem <= LEN(aCurDoc)
         IF ASCAN(aCurDoc[nArrayItem], " $CLASSDOC$") > 0
            cDocType := "Class"
            nClassNamePos := ASCAN(aCurDoc[nArrayItem], " $CLASS$", ASCAN(aCurDoc[nArrayItem], " $CLASSDOC$") + 1) + 1
            cFileName := aCurDoc[nArrayItem][nClassNamePos]
         ELSEIF ASCAN(aCurDoc[nArrayItem], " $DOC$") > 0
            cDocType := "Function"
            nFunctionNamePos := ASCAN(aCurDoc[nArrayItem], " $FUNCNAME$") + 1
            cFileName := aCurDoc[nArrayItem][nFunctionNamePos]
         ELSE
            LOOP
         ENDIF

         // Check whether the function name has "FT_" in it, if so, ignore the first three characters
         IF AT("FT_", cFileName) > 0
            cTemp := UPPER(SUBSTR(cFileName, 4))
         ELSE
            cTemp := UPPER(cFileName)
         ENDIF

         // Check whether the function name has "(" in it, if so, ignore it + the following chars
         IF (nEnd := AT("(", cTemp)) > 0
            cTemp := LEFT(cTemp, nEnd - 1)
         ENDIF

         // Strip off any other non-alphabetical or -numeric characters
         cFileName := ""
         FOR j := 1 TO LEN( cTemp )
            cChar := SUBSTR( cTemp, j, 1 )
            IF ( cChar >= "0" .AND. cChar <= "9" ) .OR. ( cChar >= "A" .AND. cChar <= "Z" ) .OR. cChar == "_"
               cFileName += cChar
            ENDIF
         NEXT

         // Check whether the file name already exists, if so, modify the name
         cFileName := LEFT(cFileName, 36)
         nEnd := 1
         nCount := 0
         DO WHILE nEnd > 0
            nEnd := ASCAN(aDocInfo, { | a | a[ 2 ] == cFileName + ".htm" })
            IF nEnd > 0
               IF LEN( cFileName ) == 36
                  cFileName := STUFF( cFileName, 36, 1, STR( nCount, 1, 0 ) )
               ELSE
                  cFileName := cFileName + STR( nCount, 1, 0 )
               ENDIF
               nCount ++
            ENDIF
         ENDDO

         // Add the file extension
         cFileName := LEFT( cFileName, 36 ) + ".htm"
         oHtmClass := THTML():New( "htm\" + cFileName )

         // If file creation was successful
         IF oHtmClass:nHandle > 0
            // Add a title to the HTML
            nPropertyListAnker := .F.
            nMethodListAnker := .F.

            oHtmClass:WriteText("<title>Harbour - Reference Guide</title>")

            // Add CSS to the HTML
            oHtmClass:WriteText("<style>")
            oHtmClass:WriteText("body {font-family:arial;font-size:14px;line-height:18px;}")
            oHtmClass:WriteText(".classtitle {font-weight:bold;font-size:22px;padding-bottom:4px;}")
            oHtmClass:WriteText(".oneliner {font-style:italic;margin-bottom:12px;}")
            oHtmClass:WriteText(".itemtitle {font-weight:bold;margin-left:8px;padding-bottom:2px;}")
            oHtmClass:WriteText(".itemtext {margin-left:20px;padding-bottom:6px;}")
            oHtmClass:WriteText("</style></head><body>")

            // Loop through the first array (list with arrays)
            DO WHILE nArrayItem <= LEN(aCurDoc)
               nSubArrayItem := 1

               IF cDocType = "Class"
                  // Loop through the second array (list with methods, properties, ...)

                  IF ASCAN(aCurDoc[nArrayItem], " $CLASS$") > 0
                     // Write class name
                     nArrayItemLine := ASCAN(aCurDoc[nArrayItem], {|a| "CLASS" $ UPPER(a)}, 2) + 1
                     cTempString := HTMFormat(aCurDoc[nArrayItem][nArrayItemLine])

                     DO WHILE !( "$" $ aCurDoc[nArrayItem][nArrayItemLine + 1] )
                        nArrayItemLine ++
                        cTempString := cTempString + "<br>" + HTMFormat(aCurDoc[nArrayItem][nArrayItemLine])
                     ENDDO
                     oHtmClass:WriteText("<div class='classtitle'>Class " + cTempString + "</div>")

                     // Add classname and filename to the docinfo array
                     AADD( aDocInfo, { cTempString, cFileName, "C1" } )

                     // Write oneliner
                     nArrayItemLine := ASCAN(aCurDoc[nArrayItem], {|a| "ONELINER" $ UPPER(a)}) + 1
                     cTempString := HTMFormat(aCurDoc[nArrayItem][nArrayItemLine])
                     DO WHILE !( "$" $ aCurDoc[nArrayItem][nArrayItemLine + 1] )
                        nArrayItemLine ++
                        cTempString := cTempString + "<br>" + HTMFormat(aCurDoc[nArrayItem][nArrayItemLine])
                     ENDDO
                     oHtmClass:WriteText("<div class='oneliner'>" + cTempString + "</div>")

                     // Write inheritance
                     nArrayItemLine := ASCAN(aCurDoc[nArrayItem], {|a| "INHERITS" $ UPPER(a)}) + 1
                     IF nArrayItemLine - 1 != 0 .AND. !EMPTY(aCurDoc[nArrayItem][nArrayItemLine]) //.AND. FILE(aCurDoc[nArrayItem][nArrayItemLine] + ".txt")
                        cInherits := aCurDoc[nArrayItem][nArrayItemLine]
                        cTempString := "<a href='" + cInherits + ".htm'>" + HTMFormat(aCurDoc[nArrayItem][nArrayItemLine]) + "</a>"
                     ELSE
                        cTempString := "-"
                     ENDIF
                     oHtmClass:WriteText("<div class='itemtitle'>&raquo; " + "Inherits" + "</div>")
                     oHtmClass:WriteText("<div class='itemtext'>" + cTempString + "</div>")

                     WriteClass("Constructor", "Constructor")
                     WriteClass("Syntax", "Syntax")
                     WriteClass("Arguments", "Arguments")
                     WriteClass("Returns", "Returns")
                     WriteClassDataMethod("Method", "Methods", nArrayItem, nArrayItemLine)
                     WriteClassDataMethod("Data", "Properties", nArrayItem, nArrayItemLine)
                     WriteClass("Category", "Category")
                     WriteClass("Description", "Description")
                     WriteClass("Seealso", "See also")

                     oHtmClass:WriteText("</body></html>")

                     // Create new HTML for the properties and methods
                     IF LEN(aCurDoc) > 1
                        cFileName := LEFT(cFileName, LEN(cFileName) - 4) + "_content.htm"
                        oHtmClassContent := THTML():New( "htm\" + cFileName )

                        IF oHtmClassContent:nHandle > 0
                           // Add a title to the HTML
                           oHtmClassContent:WriteText("<title>Harbour - Reference Guide</title>")

                           // Add CSS to the HTML
                           oHtmClassContent:WriteText("<style>")
                           oHtmClassContent:WriteText("body {font-family:arial;font-size:14px;line-height:18px;}")
                           oHtmClassContent:WriteText(".classtitle {font-weight:bold;font-size:22px;padding-bottom:4px;}")
                           oHtmClassContent:WriteText(".title {font-weight:bold;font-size:18px;padding-bottom:2px;padding-top:8px;}")
                           oHtmClassContent:WriteText(".oneliner {font-style:italic;margin-bottom:8px;}")
                           oHtmClassContent:WriteText(".itemtitle {font-weight:bold;margin-left:8px;padding-bottom:2px;}")
                           oHtmClassContent:WriteText(".itemtext {margin-left:20px;padding-bottom:6px;}")
                           oHtmClassContent:WriteText("</style></head><body>")

                           // Write class name
                           nArrayItemLine := ASCAN(aCurDoc[nArrayItem], {|a| "CLASS" $ UPPER(a)}, 2) + 1
                           cTempString := HTMFormat(aCurDoc[nArrayItem][nArrayItemLine])
                           DO WHILE !( "$" $ aCurDoc[nArrayItem][nArrayItemLine + 1] )
                              nArrayItemLine ++
                              cTempString := cTempString + "<br>" + HTMFormat(aCurDoc[nArrayItem][nArrayItemLine])
                           ENDDO
                           oHtmClassContent:WriteText("<div class='classtitle'>Class " + cTempString + "</div>")
                        ENDIF
                     ENDIF
                  ELSEIF ASCAN(aCurDoc[nArrayItem], " $DATA$") > 0
                     IF oHtmClassContent:nHandle > 0
                        // Write property name
                        IF nPropertyListAnker == .F.
                           oHtmClassContent:WriteText("<a name='propertylist'>")
                           nPropertyListAnker := .T.
                        ENDIF

                        nArrayItemLine := ASCAN(aCurDoc[nArrayItem], {|a| "$DATA$" $ UPPER(a)}, 2) + 1
                        cTempString := HTMFormat(aCurDoc[nArrayItem][nArrayItemLine])
                        DO WHILE !( "$" $ aCurDoc[nArrayItem][nArrayItemLine + 1] )
                           nArrayItemLine ++
                           cTempString := cTempString + "<br>" + HTMFormat(aCurDoc[nArrayItem][nArrayItemLine])
                        ENDDO
                        oHtmClassContent:WriteText("<div class='title'><a name='" + cTempString + "'>Property " + cTempString + "</a></div>")

                        // Write property oneliner
                        nArrayItemLine := ASCAN(aCurDoc[nArrayItem], {|a| "ONELINER" $ UPPER(a)}, 2) + 1
                        cTempString := HTMFormat(aCurDoc[nArrayItem][nArrayItemLine])
                        DO WHILE !( "$" $ aCurDoc[nArrayItem][nArrayItemLine + 1] )
                           nArrayItemLine ++
                           cTempString := cTempString + "<br>" + HTMFormat(aCurDoc[nArrayItem][nArrayItemLine])
                        ENDDO
                        oHtmClassContent:WriteText("<div class='oneliner'>" + cTempString + "</div>")

                        WriteData("Datatype", "Property type")
                        WriteData("Category", "Category")
                        WriteData("Description", "Description")
                     ENDIF
                  ELSEIF ASCAN(aCurDoc[nArrayItem], " $METHOD$") > 0
                     IF oHtmClassContent:nHandle > 0
                        // Write method name
                        IF nMethodListAnker == .F.
                           oHtmClassContent:WriteText("<a name='methodlist'>")
                           nMethodListAnker := .T.
                        ENDIF

                        nArrayItemLine := ASCAN(aCurDoc[nArrayItem], {|a| "METHOD" $ UPPER(a)}, 2) + 1
                        cTempString := HTMFormat(aCurDoc[nArrayItem][nArrayItemLine])
                        DO WHILE !( "$" $ aCurDoc[nArrayItem][nArrayItemLine + 1] )
                           nArrayItemLine ++
                           cTempString := cTempString + "<br>" + HTMFormat(aCurDoc[nArrayItem][nArrayItemLine])
                        ENDDO
                        oHtmClassContent:WriteText("<div class='title'><a name='" + cTempString + "'>Method " + cTempString + "</a></div>")

                        // Write method oneliner
                        nArrayItemLine := ASCAN(aCurDoc[nArrayItem], {|a| "ONELINER" $ UPPER(a)}, 2) + 1
                        cTempString := HTMFormat(aCurDoc[nArrayItem][nArrayItemLine])
                        DO WHILE !( "$" $ aCurDoc[nArrayItem][nArrayItemLine + 1] )
                           nArrayItemLine ++
                           cTempString := cTempString + "<br>" + HTMFormat(aCurDoc[nArrayItem][nArrayItemLine])
                        ENDDO
                        oHtmClassContent:WriteText("<div class='oneliner'>" + cTempString + "</div>")

                        WriteMethod("Syntax", "Syntax")
                        WriteMethod("Arguments", "Arguments")
                        WriteMethod("Returns", "Returns")
                        WriteMethod("Description", "Description")
                     ENDIF
                  ELSE
                     // ...
                  ENDIF
               ELSE
                  // Write function name
                  nArrayItemLine := ASCAN(aCurDoc[nArrayItem], {|a| "FUNCNAME" $ UPPER(a)}, 2) + 1
                  cTempString := HTMFormat(aCurDoc[nArrayItem][nArrayItemLine])
                  DO WHILE !( "$" $ aCurDoc[nArrayItem][nArrayItemLine + 1] )
                     nArrayItemLine ++
                     cTempString := cTempString + "<br>" + HTMFormat(aCurDoc[nArrayItem][nArrayItemLine])
                  ENDDO
                  oHtmClass:WriteText("<div class='classtitle'>Function " + cTempString + "</div>")

                  // Add function name and filename to the docinfo array
                  nArrayItemLine := ASCAN(aCurDoc[nArrayItem], {|a| "CATEGORY" $ UPPER(a)}, 2) + 1
                  cTempString2 := aCurDoc[nArrayItem][nArrayItemLine]
                  IF cTempString2 $ "WinAPI"
                     AADD( aDocInfo, { cTempString, cFileName, "F2" } )
                  ELSE
                     AADD( aDocInfo, { cTempString, cFileName, "F1" } )
                  ENDIF

                  // Write function oneliner
                  nArrayItemLine := ASCAN(aCurDoc[nArrayItem], {|a| "ONELINER" $ UPPER(a)}, 2) + 1
                  cTempString := HTMFormat(aCurDoc[nArrayItem][nArrayItemLine])
                  DO WHILE !( "$" $ aCurDoc[nArrayItem][nArrayItemLine + 1] )
                     nArrayItemLine ++
                     cTempString := cTempString + "<br>" + HTMFormat(aCurDoc[nArrayItem][nArrayItemLine])
                  ENDDO
                  oHtmClass:WriteText("<div class='oneliner'>" + cTempString + "</div>")

                  // Write selected function items
                  DO WHILE nTemp2 <= LEN(aFunctionItems)
                     WriteFunction(aFunctionItems[nTemp2][1], aFunctionItems[nTemp2][2])
                     nTemp2 ++
                  ENDDO
               ENDIF

               nArrayItem ++
            ENDDO

            IF cDocType = "Class"
               oHtmClassContent:WriteText("</body></html>")
            ELSE
               oHtmClass:WriteText("</body></html>")
            ENDIF
         ELSE
            // HTML file could not be created, skip current source file
            LOOP
         ENDIF
      ENDIF

   NEXT
RETURN NIL


FUNCTION ReadFromTop2(nh)
   LOCAL cBuffer   := ""
   LOCAL aTempArray   := {}

   DO WHILE FReadLine(nH, @cBuffer, 4096)
      cBuffer := TRIM(SUBSTR(cBuffer, nCommentLen))

      AADD(aTempArray, cBuffer)
      IF AT(DELIM + "END" + DELIM, cBuffer) > 0
         AADD(aCurdoc, aTempArray)
         aTempArray := {}
      ENDIF
   ENDDO

   FT_FGOTOP()
RETURN NIL


FUNCTION WriteClass(cItem, cTitle)
   LOCAL cTempString
   nArrayItemLine := ASCAN(aCurDoc[nArrayItem], {|a| UPPER(cItem) $ UPPER(a)}) + 1
   IF nArrayItemLine - 1 != 0
      cTempString := HTMFormat(aCurDoc[nArrayItem][nArrayItemLine])
   ELSE
      cTempString := "-"
   ENDIF
   DO WHILE !( "$" $ aCurDoc[nArrayItem][nArrayItemLine + 1] )
      nArrayItemLine ++
      cTempString := cTempString + "<br>" + HTMFormat(aCurDoc[nArrayItem][nArrayItemLine])
   ENDDO
   IF EMPTY(cTempString)
      cTempString := "-"
   ENDIF

   oHtmClass:WriteText("<div class='itemtitle'>&raquo; " + cTitle + "</div>")
   oHtmClass:WriteText("<div class='itemtext'>" + cTempString + "</div>")
RETURN NIL


FUNCTION WriteClassDataMethod(cItem, cTitle, nArrayItem, nArrayItemLine)
   LOCAL nPrevArrayItem := nArrayItem
   LOCAL nPrevArrayItemLine := nArrayItemLine
   LOCAL aTmpArray := {}
   LOCAL nTmpCount := 1
   LOCAL cTempString

   nArrayItem ++
   cTempString := ""
   DO WHILE nArrayItem <= LEN(aCurDoc)
      nArrayItemLine := ASCAN(aCurDoc[nArrayItem], {|a| "$" + UPPER(cItem) + "$" $ UPPER(a)}) + 1
      IF nArrayItemLine -1 != 0
         AADD(aTmpArray, {"&bull; <a href='" + LOWER(LEFT(cFileName, LEN(cFileName) - 4)) + "_content.htm#" + aCurDoc[nArrayItem][nArrayItemLine] + "' target=_self>" + aCurDoc[nArrayItem][nArrayItemLine] + "</a><br>", aCurDoc[nArrayItem][nArrayItemLine]})
         nTmpCount ++
      ENDIF
      nArrayItem ++
   ENDDO

   ASORT(aTmpArray,,, {|x, y| UPPER(x[2]) < UPPER(y[2])})
   FOR nTmpCount := 1 TO LEN(aTmpArray)
      cTempString := cTempString + aTmpArray[nTmpCount][1]
   NEXT
   IF EMPTY(cTempString)
      cTempString := "-"
   ENDIF

   oHtmClass:WriteText("<div class='itemtitle'>&raquo; " + cTitle + "</div>")
   oHtmClass:WriteText("<div class='itemtext'>" + cTempString + "</div>")

   nArrayItem := nPrevArrayItem
   nArrayItemLine := nPrevArrayItemLine

   LinkInheritance(cItem)
RETURN NIL


FUNCTION LinkInheritance(cItem)
   LOCAL nTemp1 := 1

   DO WHILE nTemp1 <= LEN(aDirList)
      IF UPPER(aDirList[nTemp1][1]) == UPPER(cInherits + ".txt")
         oHtmClass:WriteText("<div class='itemtext'>&bull; <a href='" + cInherits + "_content.htm#" + IIF(UPPER(cItem) = "DATA", "propertylist", "methodlist") + "' target=_self>" + IIF(cItem ="Data", "Properties", "Methods") + " inherited via " + cInherits + "</a></div>")
      ENDIF
      nTemp1 ++
   ENDDO
RETURN NIL


FUNCTION WriteData(cItem, cTitle)
   LOCAL cTempString
   nArrayItemLine := ASCAN(aCurDoc[nArrayItem], {|a| UPPER(cItem) $ UPPER(a)}) + 1
   cTempString := HTMFormat(aCurDoc[nArrayItem][nArrayItemLine])
   DO WHILE !( "$" $ aCurDoc[nArrayItem][nArrayItemLine + 1] )
      nArrayItemLine ++
      cTempString := cTempString + "<br>" + HTMFormat(aCurDoc[nArrayItem][nArrayItemLine])
   ENDDO
   IF EMPTY(cTempString)
      cTempString := "-"
   ENDIF

   oHtmClassContent:WriteText("<div class='itemtitle'>&raquo; " + cTitle + "</div>")
   oHtmClassContent:WriteText("<div class='itemtext'>" + cTempString + "</div>")
RETURN NIL


FUNCTION WriteMethod(cItem, cTitle)
   LOCAL cTempString
   nArrayItemLine := ASCAN(aCurDoc[nArrayItem], {|a| UPPER(cItem) $ UPPER(a)}) + 1
   cTempString := HTMFormat(aCurDoc[nArrayItem][nArrayItemLine])
   DO WHILE !( "$" $ aCurDoc[nArrayItem][nArrayItemLine + 1] )
      nArrayItemLine ++
      cTempString := cTempString + "<br>" + HTMFormat(aCurDoc[nArrayItem][nArrayItemLine])
   ENDDO
   IF EMPTY(cTempString)
      cTempString := "-"
   ENDIF

   oHtmClassContent:WriteText("<div class='itemtitle'>&raquo; " + cTitle + "</div>")
   oHtmClassContent:WriteText("<div class='itemtext'>" + cTempString + "</div>")
RETURN NIL


FUNCTION WriteFunction(cItem, cTitle)
   LOCAL cTempString
   nArrayItemLine := ASCAN(aCurDoc[nArrayItem], {|a| UPPER(cItem) $ UPPER(a)}) + 1
   cTempString := aCurDoc[nArrayItem][nArrayItemLine]
   DO WHILE !( "$" $ aCurDoc[nArrayItem][nArrayItemLine + 1] )
      nArrayItemLine ++
      cTempString := cTempString + "<br>" + HTMFormat(aCurDoc[nArrayItem][nArrayItemLine])
   ENDDO
   IF EMPTY(cTempString)
      cTempString := "-"
   ENDIF

   oHtmClass:WriteText("<div class='itemtitle'>&raquo; " + cTitle + "</div>")
   oHtmClass:WriteText("<div class='itemtext'>" + cTempString + "</div>")
RETURN NIL


FUNCTION HTMFormat(cTempString)
   LOCAL cTmpString1 := cTempString
   LOCAL cTmpString2 := ""
   LOCAL nTmpCount := 1
   LOCAL cTmpChar

   DO WHILE nTmpCount <= LEN(cTmpString1)
      cTmpChar := SUBSTR(cTmpString1, nTmpCount, 1)

      DO CASE
      CASE cTmpChar == "<"
         cTmpString2 := cTmpString2 + "&lt;"
      CASE cTmpChar == ">"
         cTmpString2 := cTmpString2 + "&gt;"
      OTHERWISE
         cTmpString2 := cTmpString2 + cTmpChar
      ENDCASE

      nTmpCount ++
   ENDDO
RETURN cTmpString2



/***********************************
* Function ProcWwwBuf(cBuffer)   -> cTemp
* Parameter cBuffer  -> Strip the "<" and ">" symbols from the imput String
* Return    cTemp  Formated String to WWW output
*/

*+--------------------------------------------------------------------
*+
*+    Function ProcWwwBuf()
*+
*+--------------------------------------------------------------------
*+
FUNCTION ProcWwwBuf( cPar )

   cPar := STRTRAN( cPar, "<", "&lt;" )
   cPar := STRTRAN( cPar, ">", "&gt;" )

RETURN cPar

*+--------------------------------------------------------------------
*+
*+    Function ProcWwwAlso()
*+
*+    Called from ( genhtm.prg   )   1 - function processwww()
*+                ( genhtm1.prg  )   1 - function processwww()
*+                ( genhtm2.prg  )   1 - function processwww()
*+
*+--------------------------------------------------------------------
*+
FUNCTION ProcWwwAlso( nWriteHandle, cSeeAlso )

   LOCAL nPos
   LOCAL cTemp := ""
   LOCAL xTemp
   LOCAL nLen
   LOCAL xPos
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

*+--------------------------------------------------------------------
*+
*+    Function ProcStatusWww()
*+
*+    Called from ( genhtm.prg   )   1 - function processwww()
*+                ( genhtm1.prg  )   1 - function processwww()
*+
*+--------------------------------------------------------------------
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

*+--------------------------------------------------------------------
*+
*+    Function FormatHtmBuff()
*+
*+    Called from ( genhtm.prg   )   1 - function prochtmdesc()
*+
*+--------------------------------------------------------------------
*+
FUNCTION FormatHtmBuff( cBuffer, cStyle )

   LOCAL creturn    := ""
   LOCAL cline      := ""
   LOCAL cOldLine   := ""
   LOCAL cBuffend   := ""
   LOCAL lEndBuffer := .F.
   LOCAL lArgBold   := .F.
   LOCAL npos
   creturn := cBuffer + " "
   IF AT( "</par>", creturn ) > 0 .OR. EMPTY( cBuffer )
      IF EMPTY( cbuffer )
         creturn := ""
      ENDIF
      RETURN creturn
   ENDIF
   IF cStyle != "Syntax" .AND. cStyle != "Arguments"
      DO WHILE !lEndBuffer
         cLine := TRIM( SUBSTR( ReadLN( @lEof ), nCommentLen ) )
         IF AT( "</par>", cLine ) > 0
            lEndBuffer := .T.
         ENDIF

         IF EMPTY( cLine )
            lEndBuffer := .T.
            //            TheBlank   := .T.
            FT_FSKIP( - 1 )
         ENDIF
         IF AT( DELIM, cLine ) > 0

            FT_FSKIP( - 1 )
            lEndBuffer := .T.
         ENDIF
         IF AT( DELIM, cLine ) == 0
            cReturn += " " + ALLTRIM( cLine ) + " "
         ENDIF
      ENDDO
      cReturn := STRTRAN( cReturn, "<par>", "" )
      cReturn := STRTRAN( cReturn, "</par>", "" )

      cReturn := "<par>" + cReturn + "    </par>"

   ELSEIF cStyle == "Syntax"
      cReturn := STRTRAN( cReturn, "<par>", "" )
      cReturn := STRTRAN( cReturn, "<", "&lt;" )
      cReturn := STRTRAN( cReturn, ">", "&gt;" )
      cReturn := AllTrim(cReturn)
      creturn := "<par><b>" + creturn + " </b></par>"
   ELSEIF cStyle == "Arguments"

      nPos := 0
      IF AT( "<par>", cReturn ) > 0 .and. at("<b>",cReturn)=0
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
       elseif AT( "<par>", cReturn ) > 0 .and. at("<b>",cReturn)>0
         cReturn  := STRTRAN( cReturn, "<par>", "" )
         cReturn  := STRTRAN( cReturn, "</par>", "" )
         cReturn  := ALLTRIM( cReturn )
         nPos     := AT( "</b>", cReturn )
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
            lEndBuffer := .T.
         ENDIF
         IF EMPTY( cLine )
            lEndBuffer := .T.
            //            TheBlank   := .T.
            FT_FSKIP( - 1 )

         ENDIF
         IF AT( DELIM, cLine ) > 0
            FT_FSKIP( - 1 )
            lEndBuffer := .T.
         ENDIF
         IF AT( DELIM, cLine ) == 0
            cReturn += " " + ALLTRIM( cLine ) + " "
         ENDIF
      ENDDO
      cReturn := STRTRAN( cReturn, "<par>", "" )
      cReturn := STRTRAN( cReturn, "</par>", "" )

      cReturn  := STRTRAN( cReturn, "<", "&lt;" )
      cReturn  := STRTRAN( cReturn, ">", "&gt;" )
      cOldLine := STRTRAN( cOldLine, "<", "&lt;" )
      cOldLine := STRTRAN( cOldLine, ">", "&gt;" )

      IF lArgBold
         cReturn := "       <par><b>" + cOldLine + "</b> " + cReturn + "    </par>"
      ELSE
         cReturn := "       <par>" + cOldLine + " " + cReturn + "    </par>"
      ENDIF
      //   ENDIF
      lArgBold := .F.

   ENDIF

   //   endif
RETURN creturn

*+--------------------------------------------------------------------
*+
*+    Function checkhtmcolor()
*+
*+    Called from ( genhtm.prg   )   1 - function prochtmdesc()
*+
*+--------------------------------------------------------------------
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
      nreturn         := ASCAN( acolortable, { | x | UPPER( x ) == UPPER( ccolor ) } )
      IF nreturn > 0
         creturn := "<font color=" + acolortable[ nreturn ] + ">"
      ENDIF
      cBuffer := STRTRAN( cBuffer, cOldColorString, cReturn )
   ENDDO
RETURN cbuffer

*+--------------------------------------------------------------------
*+
*+    Function ProchtmDesc()
*+
*+    Called from ( genhtm.prg   )   6 - function processwww()
*+                ( genhtm1.prg  )   6 - function processwww()
*+                ( genhtm2.prg  )   6 - function processwww()
*+
*+--------------------------------------------------------------------
*+
FUNCTION ProchtmDesc( cBuffer, oHtm, cStyle ,cFileName)

   LOCAL cOldLine      := ""
   LOCAL npos
   LOCAL lHasFixed     := .F.
   LOCAL CurPos        :=  0
   LOCAL nColorPos
   LOCAL ccolor        := ""
   LOCAL creturn       := ""
   LOCAL nIdentLevel
   LOCAL lEndPar       := .F.
   LOCAL cLine         := ""
   LOCAL lEndFixed     := .F.
   LOCAL lArgBold      := .F.
   LOCAL LFstTableItem := .T.
   LOCAL lEndTable     := .F.
   LOCAL lEndBuffer    := .F.

   DEFAULT cStyle TO "Default"
   DEFAULT cFileName TO NIL
   iF cStyle == "Syntax"
//   tracelog("cBuffer",cBuffer)
   clastBuffer += cBuffer
   endif
   IF AT( "<par>", cBuffer ) == 0 .AND. !EMPTY( cBuffer ) .AND. cstyle != "Example"
      cBuffer := "<par>" + cBuffer
   ENDIF

   IF EMPTY( cBuffer )
      oHtm:WriteText( "<dd><br></dd>" )
   ENDIF

   IF cStyle != "Example" .AND. AT( "<table>", cBuffer ) == 0 .AND. AT( "<fixed>", cBuffer ) == 0
      IF AT( "<par>", cBuffer ) >= 0 .OR. AT( "</par>", cBuffer ) == 0 .AND. !EMPTY( cbuffer )
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
                  cReturn := "       <par><b>" + cOldLine + "</b> " + cReturn + "    </par>"
               ELSE
                  cReturn := "       <par>" + cOldLine + " " + cReturn + "    </par>"
               ENDIF

               cbuffer := cReturn
            ENDIF
         ELSE
            cBuffer := FormathtmBuff( cBuffer, cStyle )
         ENDIF
      ENDIF
   ENDIF

   IF AT( "<par>", cBuffer ) > 0 .AND. AT( "</par>", cBuffer ) > 0
      cBuffer   := STRTRAN( cBuffer, "<par>", "" )
      cBuffer   := STRTRAN( cBuffer, "</color>", "</font> " )
      nColorPos := AT( "<color:", cBuffer )
      IF ncolorpos > 0
         checkhtmcolor( @cbuffer, ncolorpos )
      ENDIF
      //      Alltrim(cBuffer)
      IF cStyle == "Description" .OR. cStyle == "Compliance"
         nIdentLevel := 6
         nPos        := 0
         IF AT( "</par>", cBuffer ) > 0
            cBuffer := STRTRAN( cBuffer, "</par>", "" )
         ENDIF
         IF !EMPTY( cBuffer )
            //             cBuffer:=SUBSTR(cBuffer,2)
            cBuffer := STRTRAN( cBuffer, "<b>", "bold" )
            cBuffer := STRTRAN( cBuffer, "</b>", "negrito" )
            cBuffer := STRTRAN( cBuffer, "<", "&lt;" )
            cBuffer := STRTRAN( cBuffer, ">", "&gt;" )
            cBuffer := STRTRAN( cBuffer, "bold", "<b>" )
            cBuffer := STRTRAN( cBuffer, "negrito", "</b>" )

            cBuffeR := ALLTRIM( cBuffer )
            oHtm:WritePar( cBuffer )
         ENDIF

      ELSEIF cStyle == "Arguments"

         IF AT( "</par>", cBuffer ) > 0
            cBuffer := STRTRAN( cBuffer, "</par>", "" )
         ENDIF
         IF !EMPTY( cBuffer )
            cBuffeR := ALLTRIM( cBuffer )
            oHtm:WritePar( cBuffer )
         ENDIF

      ELSEIF cStyle == "Syntax"
         IF AT( "</par>", cBuffer ) > 0
            cBuffer := STRTRAN( cBuffer, "</par>", "" )
            cBuffer := STRTRAN( cBuffer, "<par>", "" )
         ENDIF
         IF !EMPTY( cBuffer )
            //                    cBuffer:=SUBSTR(cBuffer,2)
            cBuffeR := ALLTRIM( cBuffer )
            cLastText += cBuffer
            if At("->" ,clastBuffer) >0
               aadd(aDocWwwInfo, { cLastText,cFileName})
               clastBuffer:=""
               cLastText:=""
            endif
//          tracelog(cLastBuffer)
            oHtm:WritePar( cBuffer )
         ENDIF

      ELSEIF cStyle == "Default"
         IF AT( "</par>", cBuffer ) > 0
            cBuffer := STRTRAN( cBuffer, "</par>", "" )
         ENDIF
         IF !EMPTY( cBuffer )
            //                  cBuffer:=SUBSTR(cBuffer,2)
            cBuffeR := ALLTRIM( cBuffer )
            oHtm:WritePar( cBuffer )
         ENDIF

      ENDIF
   ENDIF
   IF AT( "<fixed>", cBuffer ) > 0 .OR. cStyle = "Example"
      IF AT( "<fixed>", cBuffer ) == 0 .OR. !EMPTY( cBuffer )
         if AT( "<fixed>", cBuffer ) > 0
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
            lendfixed := .T.
            cOldLine  := Alltrim(STRTRAN( cOldLine, "</fixed>", "" ))

         ENDIF
         IF AT( DELIM, cOldLine ) == 0
            cReturn += ALLTRIM( cOldLine ) + " "
         ENDIF
         IF AT( DELIM, cOldLine ) > 0
            FT_FSKIP( - 1 )
            lEndfixed := .T.

         ENDIF

         IF AT( DELIM, cOldLine ) == 0  .and. !lendfixed
            oHtm:WriteText( cOldLine )
         ENDIF
      ENDDO
//      oHtm:WriteText( "</pre><br>" )
   lHasFixed:=.F.
   END
   IF AT( "<table>", cBuffer ) > 0
      DO WHILE !lendTable
         cLine := TRIM( SUBSTR( ReadLN( @lEof ), nCommentLen ) )
         IF AT( "</table>", cLine ) > 0
            lendTable := .T.
         ELSE
            IF LFstTableItem
               nNumTableItems := GetNumberofTableItems( cLine )
               prochtmtable( cline, nNumTableItems )
               LFstTableItem := .F.
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

*+--------------------------------------------------------------------
*+
*+    Function ProchtmTable()
*+
*+    Called from ( genhtm.prg   )   2 - function prochtmdesc()
*+                ( genhtm1.prg  )   1 - function prochtmdesc()
*+                ( genhtm2.prg  )   1 - function prochtmdesc()
*+
*+--------------------------------------------------------------------
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
      cBuffer   := STRTRAN( cBuffer, ccolor, "" )
      nColorpos := ASCAN( aColorTable, { | x | UPPER( x ) == UPPER( ccolor ) } )
      cColor    := aColortable[ nColorPos ]
   ENDIF
   IF EMPTY( cBuffer )
      citem  := ""
      citem2 := ""
      citem3 := ""
      citem4 := ""
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
   IF cColor != NIL
      AADD( afiTable, "<Font color=" + ccolor + ">" + RTRIM( LTRIM( cItem ) ) + "</font>" )
      AADD( asiTable, "<Font color=" + ccolor + ">" + cItem2 + "</font>" )
   ELSE
      AADD( afiTable, RTRIM( LTRIM( cItem ) ) )
      AADD( asiTable, cItem2 )
   ENDIF

   IF cColor != NIL
      AADD( atiTable, "<Font color=" + ccolor + ">" + cItem3 + "</font>" )
   ELSE
      AADD( atiTable, cItem3 )
   ENDIF
   IF cColor != NIL
      AADD( afoiTable, "<Font color=" + ccolor + ">" + cItem4 + "</font>" )
   ELSE
      AADD( afoiTable, cItem4 )
   ENDIF

RETURN Nil

*+--------------------------------------------------------------------
*+
*+    Function GenhtmTable()
*+
*+    Called from ( genhtm.prg   )   1 - function prochtmdesc()
*+                ( genhtm1.prg  )   1 - function prochtmdesc()
*+                ( genhtm2.prg  )   1 - function prochtmdesc()
*+
*+--------------------------------------------------------------------
*+
FUNCTION GenhtmTable( oHtm )

   LOCAL x
/*   oHtm:WriteText( "<br>" )
   oHtm:WriteText( "<br>" )*/
   oHtm:WriteText( "<table border=1 aling=center>" )                     //-4

   FOR x := 1 TO LEN( asitable )
      IF !EMPTY( asitable[ x ] )
         IF nNumTableItems == 2
            oHtm:WriteText( "<tr aling=center ><td>" + afitable[ x ] + "</td><td>" + asitable[ x ] + "</td></tr> " )
         ELSEIF nNumTableItems == 3
            oHtm:WriteText( "<tr aling=center><td>" + afitable[ x ] + "</td><td>" + asitable[ x ] + "</td><td>" + atitable[ x ] + "</td></tr> " )
         ELSEIF nNumTableItems == 4
            oHtm:WriteText( "<tr aling=center><td>" + afitable[ x ] + "</td><td>" + asitable[ x ] + "</td><td>" + atitable[ x ] + "</td><td>" + afoitable[ x ] + "</td></tr> " )
         ENDIF
      ELSE
         oHtm:WriteText( "<tr><td></td></tr> " )
      ENDIF
   NEXT

   oHtm:Writetext( "</table>" )

//   oHtm:WriteText( "<br>" )
   afiTable  := {}
   asitable  := {}
   atitable  := {}
   afoitable := {}

RETURN Nil

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
*+    Called from ( genng.prg    )  20 - function processing()
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

#ifdef GAUGE
function CountDocs(aCurDoc)
   return len(aCurdoc)
#endif


*+--------------------------------------------------------------------
*+
*+    Class THTML
*+
*+--------------------------------------------------------------------
*+
CLASS THTML

   DATA nHandle INIT -1
   DATA cFile
   METHOD New( cFile ,aMetaContents)
   METHOD WriteTitle( cTitle )
   METHOD WritePar( cPar )
   METHOD WriteParBold( cPar )
   METHOD WriteLink( cLink ,cName)
   METHOD WriteLinkTable( cLink ,cName)
   METHOD WriteChmLink( cLink ,cName)
   METHOD WriteText( cText )
   METHOD WriteMetaTag(cTag,cDescription)
   METHOD CLOSE()
   // The Follow methods is for html source files for .chm help
   METHOD NewChm( cFile ,aMetaContents,cFuncName)
   METHOD ADDoBJECT(cType,cClassid)
   METHOD ADDPARAM(cName,cValue)
   METHOD EndOBJect()
   METHOD NewContent(cFile)
   METHOD ListItem()
ENDCLASS

METHOD New( cFile, aMetaContents ) CLASS THTML

   Local nCount
   IF Self:nHandle > 0
      FCLOSE( Self:nHandle )
   ENDIF

   IF ISCHARACTER( cFile )
      Self:cFile   := LOWER( cFile )
      Self:nHandle := FCREATE( Self:cFile )
   ENDIF
   FWRITE( Self:nHandle, "<HTML>" + CRLF )
   FWRITE( Self:nHandle, "<HEAD>" + CRLF )
   if ISARRAY( aMetaContents )
      For nCount:=1 to len(aMetaContents)
         Self:WriteMetaTag(aMetaContents[nCount,1],aMetaContents[nCount,2])
      NEXT
   Endif

   RETURN Self

METHOD WriteTitle( cTitle ) CLASS THTML

   FWRITE( Self:nHandle, "<TITLE>" + CRLF + cTitle + CRLF + "</Title>" + CRLF + "</HEAD>" + CRLF  )
   FWRITE( Self:nHandle, "<BODY>" + CRLF )
   RETURN Self

METHOD WritePar( cPar ) CLASS THTML

   //   cPar:=STRTRAN(cPar,"<","&lt;")
   //   cPar:=STRTRAN(cPar,">","&gt;")
   FWRITE( Self:nHandle, "<dd>" + ALLTRIM( cPar ) + "</dd>" + CRLF )
   RETURN Self

METHOD WriteText( cPar ) CLASS THTML
   FWRITE( Self:nHandle, cPar + CRLF )
   RETURN Self

METHOD WriteParBold( cPar, lEndDl, lPar ) CLASS THTML

   DEFAULT lEnddl TO .T.
   DEFAULT lPar TO .T.
   IF lEndDl .AND. lPar
      FWRITE( Self:nHandle, "</P></dd>" + CRLF + "</DL>" + CRLF + "<DL>" + CRLF + "<dt><b>" + ALLTRIM( cPar ) + "</b></dt><p>" + CRLF )
   ELSEIF !lPar .AND. !lEnddl
      FWRITE( Self:nHandle, "<DL>" + CRLF + "<dt><b>" + ALLTRIM( cPar ) + "</b></dt><p>" + CRLF )
   ELSEIF !lPar .AND. lEnddl
      FWRITE( Self:nHandle, "</PRE>" + CRLF + "</DL>" + CRLF + "<DL>" + CRLF + "<dt><b>" + ALLTRIM( cPar ) + "</b></dt><p>" + CRLF )
   ELSEIF lPar .AND. !lEnddl
      FWRITE( Self:nHandle, "</P></dd>" + CRLF + "<DL>" + CRLF + "<dt><b>" + ALLTRIM( cPar ) + "</b></dt><p>" + CRLF )

   ENDIF
   RETURN Self

METHOD CLOSE() CLASS THTML

   FWRITE( Self:nHandle, "</body>" + CRLF )
   FWRITE( Self:nHandle, "</html>" + CRLF )
   FCLOSE( Self:nHandle )

   RETURN Self

METHOD WriteLink( cLink, cName ) CLASS THTML

   LOCAL nPos
   LOCAL cTemp := ""

   nPos := AT( "()", cLink )
   IF nPos > 0
      if AT(".htm",cLink)=0
      cTemp := SUBSTR( cLink, 1, nPos - 1 ) + ".htm"
      else
      cTemp := SUBSTR( cLink, 1, nPos - 1 )
      endif
   ELSE
     if AT(".htm",cLink)=0
      cTemp := ALLTRIM( cLink ) + ".htm"
        else
     cTemp := ALLTRIM( cLink )
      endif
   ENDIF
   IF cName != Nil
      cLink := cName
   ENDIF
   cTemp := STRTRAN( cTemp, "@...", "" )
   cTemp := STRTRAN( cTemp, " ", "" )

   FWRITE( Self:nHandle, "<LI><a href=" + LOWER( cTemp ) + ">" + cLink + "</a></LI>" + CRLF )

   RETURN Self

METHOD WriteLinkTable( cLink, cName,cInfo ) CLASS THTML

   LOCAL nPos
   LOCAL cTemp

   nPos := AT( "()", cLink )
   IF nPos > 0
      if AT(".htm",cLink)=0
      cTemp := SUBSTR( cLink, 1, nPos - 1 ) + ".htm"
      else
      cTemp := SUBSTR( cLink, 1, nPos - 1 )
      endif
   ELSE
         if AT(".htm",cLink)=0
      cTemp := ALLTRIM( cLink ) + ".htm"
        else
     cTemp := ALLTRIM( cLink )
      endif
   ENDIF
   IF cName != Nil
      cLink := cName
   ENDIF
   cTemp := STRTRAN( cTemp, " ", "" )
   FWRITE( Self:nHandle, "<tr><td><a href=" + LOWER( cTemp ) + ">" + cLink + "</a></td><td>" +cinfo +"</td></tr>"+ CRLF )

   RETURN Self

METHOD WriteMetaTag(cTag,cDescription) Class THtml
   fWrite(Self:nHandle,'<META NAME="'+cTag+'" CONTENT="'+cDescription+'">'+CRLF)
   return Self

/////////////////////Method for .chm html source files support////////////////
METHOD NewChm( cFile, aMetaContents, cFuncName ) CLASS THTML

   Local nCount
   IF Self:nHandle > 0
      FCLOSE( Self:nHandle )
   ENDIF

   IF ISCHARACTER( cFile )
      Self:cFile   := LOWER( cFile )
      Self:nHandle := FCREATE( Self:cFile )
   ENDIF
   FWRITE( Self:nHandle, "<HTML>" + CRLF +"<HEAD>" +CRLF)
   if ISARRAY( aMetaContents )
   For nCount:=1 to len(aMetaContents)
      Self:WriteMetaTag(aMetaContents[nCount,1],aMetaContents[nCount,2])
   NEXT
    Endif
   ::WriteTitle(cFuncName)

   FWRITE( Self:nHandle, '<BODY BGCOLOR="#FFFFFF" TEXT="#000000">' + CRLF )
   ::AddObject("application/x-oleobject","clsid:1e2a7bd0-dab9-11d0-b93a-00c04fc99f9e")
   ::ADDPARAM("Keyword",cFuncName)
   ::AddParam("ALink Name",cFuncName)
   ::ENDOBJECT()
   RETURN Self

method ADDOBJECT(cType,cClassId) Class THTML
   IF ISCHARACTER( cClassId )
      FWRITE( Self:nHandle,'<Object type="'+cType+'" classid="'+cClassId+'">'+CRLF)
   ELSE
      FWRITE( Self:nHandle,'<Object type="'+ cType +'">'+CRLF)
   ENDIF
   RETURN Self

METHOD  ENDOBJECT() Class THTML
   FWRITE( Self:nHandle,"</OBJECT>"+CRLF)
   RETURN Self

METHOD ADDPARAM(cType,cValue) Class THTML
   FWRITE( Self:nHandle,'<param name="'+cType+ '" value="'+cValue +'">'  +CRLF)
   RETURN Self

METHOD NewContent( cFile ) CLASS THTML

   IF Self:nHandle > 0
      FCLOSE( Self:nHandle )
   ENDIF

   IF ISCHARACTER( cFile )
      Self:cFile   := LOWER( cFile )
      Self:nHandle := FCREATE( Self:cFile )
   ENDIF
   FWRITE( Self:nHandle, "<HTML>" + CRLF )
   RETURN Self

METHOD ListItem() CLASS tHtml
   FWRITE( Self:nHandle, "<LI>" )
   RETURN SELF

METHOD WriteChmLink( cLink, cName ) CLASS THTML

   LOCAL nPos
   LOCAL cTemp := ""

   nPos := AT( "()", cLink )
   IF nPos > 0
      if AT(".htm",cLink)=0
      cTemp := SUBSTR( cLink, 1, nPos - 1 ) + ".htm"
      else
      cTemp := SUBSTR( cLink, 1, nPos - 1 )
      endif
   ELSE
         if AT(".htm",cLink)=0
      cTemp := ALLTRIM( cLink ) + ".htm"
        else
     cTemp := ALLTRIM( cLink )
      endif
   ENDIF
   IF cName != Nil
      cLink := cName
   ENDIF
   cTemp := STRTRAN( cTemp, "@...", "" )
   cTemp := STRTRAN( cTemp, " ", "" )
   FWRITE( Self:nHandle, "<a href=" + LOWER( cTemp ) + ">" + cLink + "</a><br>" + CRLF )
   Return Self
