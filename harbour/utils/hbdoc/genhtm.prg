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
MEMVAR aDirList,aDocInfo,aWww

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

#define D_NORMAL  1
#define D_ARG     2
#define D_SYNTAX  3
#define D_IGNORE  4
#define D_SEEALSO 5
#define D_INCLUDE 6
#define D_ONELINE 7
#define D_STATUS  8
#define D_EXAMPLE 9
#define D_DATALINK 10
#define D_METHODLINK 11
   LOCAL i
   LOCAL j
   LOCAL nFiles      := LEN( aDirList )
   LOCAL nCommentLen
   LOCAL lEof
   LOCAL lDoc
   LOCAL lClassDoc
   LOCAL cBuffer
   LOCAL nEnd
   LOCAL nCount

   LOCAL cBar       := REPLICATE( "-", 80 ) + CRLF
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
   LOCAL lFirstSintax := .T.
   LOCAL lAddEndPreTag := .F.
   LOCAL lEndDesc := .F.
   LOCAL lEndArgs :=.F.
   LOCAL lEndSyntax := .F.
   LOCAL lEndReturns := .F.
   LOCAL lEndData := .F.
   LOCAL lDataLink
       
   LOCAL lBlankLine := .F.                 // Blank line encountered and sent out
   LOCAL lAddBlank  := .F.                 // Need to add a blank line if next line is not blank
   LOCAL oHtm
   LOCAL nReadHandle
   LOCAL lEndConstru := .F.
   LOCAL lFirstPass:= .T.
   LOCAL lFirstArg := .T.
   LOCAL lData := .F.
   LOCAL lEndDataLink := .F.
   LOCAL lEndMethodLink := .F.
   LOCAL lMethod := .F.
   LOCAL cDoc       := DELIM + "DOC" + DELIM                   // DOC keyword
   LOCAL cEnd       := DELIM + "END" + DELIM                   // END keyword
   LOCAL cFunc      := DELIM + "FUNCNAME" + DELIM              // FUNCNAME keyword
   LOCAL cCat       := DELIM + "CATEGORY" + DELIM              // CATEGORY keyword
   LOCAL cOne       := DELIM + "ONELINER" + DELIM              // ONELINER keyword
   LOCAL cSyn       := DELIM + "SYNTAX" + DELIM                // SYNTAX keyword
   LOCAL cArg       := DELIM + "ARGUMENTS" + DELIM             // ARGUMENTS keyword
   LOCAL cRet       := DELIM + "RETURNS" + DELIM               // RETURNS keyword
   LOCAL cDesc      := DELIM + "DESCRIPTION" + DELIM           // DESCRIPTION keyword
   LOCAL cExam      := DELIM + "EXAMPLES" + DELIM              // EXAMPLES keyword
   LOCAL cSee       := DELIM + "SEEALSO" + DELIM               // SEEALSO keyword
   LOCAL cInc       := DELIM + "INCLUDE" + DELIM               // INCLUDE keyword
   LOCAL cComm      := DELIM + "COMMANDNAME" + DELIM           // COMMAND keyword
   LOCAL cCompl     := DELIM + "COMPLIANCE" + DELIM
   LOCAL cTest      := DELIM + 'TESTS' + DELIM
   LOCAL cStatus    := DELIM + 'STATUS' + DELIM
   LOCAL cPlat      := DELIM + 'PLATFORMS' + DELIM
   LOCAL cFiles     := DELIM + 'FILES' + DELIM
   LOCAL cSubCode   := DELIM + 'SUBCODE' + DELIM
   LOCAL cFunction  := DELIM + 'FUNCTION' +DELIM
   LOCAL cConstruct := DELIM + 'CONSTRUCTOR' + DELIM
   LOCAL cDatalink  := DELIM + 'DATALINK' + DELIM
   LOCAL cDatanolink  := DELIM + 'DATANOLINK' + DELIM
   LOCAL cMethodslink := DELIM + 'METHODSLINK' + DELIM
   LOCAL cMethodsNolink := DELIM + 'METHODSNOLINK' + DELIM
   LOCAL cData      := DELIM +"DATA"+ DELIM
   LOCAL cMethod    := DELIM +'METHOD' +DELIM
   LOCAL cClassDoc  := DELIM+ "CLASSDOC" + DELIM
   //
   //  Entry Point
   //
   //  Put up information labels
   @ INFILELINE, 20 SAY "Extracting: "
   @ MODULELINE, 20 SAY "Documenting: "
   //  loop through all of the files
   lFirstArg:=.T.
   lFirstPass:=.T.
   lFirstSintax:=.T.
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
      lClassDoc:= .F.
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
               write_error( cDoc + " encountered during extraction of Doc" ;
                            + " at line" + STR( nLinecnt, 5, 0 ),,,, aDirList[ i, F_NAME ] )
            ENDIF
            lDoc    := .T.
            cBuffer := TRIM( SUBSTR( ReadLN( @lEof ), ;
                             nCommentLen ) )
            nLineCnt ++
            cCategory := cFuncName := cSeeAlso := ""
            nMode     := D_IGNORE
         ELSEIF AT( cClassDoc, cBuffer ) > 0
            IF lClassDoc
               write_error( cDoc + " encountered during extraction of Doc" ;
                            + " at line" + STR( nLinecnt, 5, 0 ),,,, aDirList[ i, F_NAME ] )
            ENDIF
            lClassDoc    := .T.
            cBuffer := TRIM( SUBSTR( ReadLN( @lEof ), ;
                             nCommentLen ) )
            nLineCnt ++
            cCategory := cFuncName := cSeeAlso := ""
            nMode     := D_IGNORE

         ELSEIF AT( cEnd, cBuffer ) > 0
            IF .NOT. lDoc .and. !lClassDoc
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
               IF .NOT. EMPTY( cSeeAlso )

                  oHtm:WriteParBold( "See Also " )
                  oHtm:WriteText("<UL>")
                  ProcWwwalso( oHtm, cSeealso )
                  oHtm:WriteText("</UL></DL>")
                  if lDoc
                     oHtm:WriteText("</DL>")
                     oHtm:Close()
                  Endif

                ENDIF
               lDoc := .F.
               lClassDoc := .F.

                  if lEndReturns   .and. lClassDoc
                    lEndReturns:=.f.
                    oHtm:WriteText("</p></dd>")
                  endif
                  if lEndArgs   .and. lClassDoc
                    lEndArgs:=.f.
                    oHtm:WriteText("</p></dd>")
                  endif
                

  //             if lClassDoc
//                 Rename htm\temp.htm to (cFileName)
 //                 Erase htm\temp.htm
    //           ENDIF

               nMode := D_IGNORE
            ENDIF

            @ MODULELINE, 33 CLEAR TO MODULELINE, MAXCOL()
         ENDIF







         IF lDoc .or. lClassDoc
            //  1) function name

            IF AT( cFunc, cBuffer ) > 0 .OR. AT( cComm, cBuffer ) > 0 .OR. AT( cSubCode, cBuffer ) >0 

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

               cFileName := LEFT( cFileName, 21 )
               nEnd      := 1
               nCount    := 0
               DO WHILE nEnd > 0
                  nEnd := ASCAN( aDocInfo, { | a | a[ 4 ] == cFileName + ".htm" } )
                  IF nEnd > 0

                     //  This will break if there are more than 10 files with the same first
                     //  seven characters. We take our chances.

                     IF LEN( cFileName ) = 21
                        cFileName := STUFF( cFileName, 21, 1, STR( nCount, 1, 0 ) )
                     ELSE
                        cFileName += STR( nCount, 1, 0 )
                     ENDIF
                     nCount ++
                  ENDIF
               ENDDO
               //  Add on the extension

               cFileName := LEFT( cFileName, 21 ) + ".htm"
               if lDoc 
                oHtm:=THTML():New('htm\'+cFileName)
               endif
                IF lFirstPass .and. lClassDoc
                  lFirstPass:=.F.
                  oHtm:=THTML():New('htm\'+cFileName)
                ENDIF
               IF ohtm:nHandle < 1
                  ? "Error creating", cFileName, ".htm"
                  write_error( "Error creating",,,, cFileName + ".htm" )
               ENDIF

            ELSEIF AT( cdata, cBuffer ) > 0 .OR. AT( cmethod, cBuffer ) > 0 .and. lClassDoc
                   if AT( cdata, cBuffer ) > 0
                      lData := .T.
                      lMethod := .F.
                   ELSEIF AT( cmethod, cBuffer ) > 0
                      lMethod := .T.
                      lData:= .F.
                   ENDIF

               cBuffer := ReadLN( @lEof )
               nLineCnt ++
               //  Save the function name
               cFuncName := UPPER( ALLTRIM( SUBSTR( cBuffer, nCommentLen ) ) )
               @ MODULELINE, 33 CLEAR TO MODULELINE, MAXCOL()
               @ MODULELINE, 33 SAY cFuncName

               nMode := D_NORMAL
               if AT("(",cfuncname)>0
               cFuncname:=substr(cFuncName,1,AT("(",cFuncName)-1)
               endif
                  if lEndDesc .and. lClassDoc
                    lEndDesc:=.f.
                    oHtm:WriteText("</p></dd>")
                  endif

               oHtm:WriteText("<a NAME="+'"'+alltrim(UPPERLOWER(cFuncname))+'"'+"></a>")
  
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

                 if lData
                    oHtm:WriteText("<H1>DATA "+ alltrim(PAD( cFuncName, 21 )) + "</H1>")
                    oHtm:WriteText("<p>"+cOneline+"</p>"+ hb_osnewline())
                 ELSEIF lMethod
                    oHtm:WriteText("<H1> METHOD "+ alltrim(PAD( cFuncName, 21 )) + "</H1>")
                    oHtm:WriteText("<p>"+cOneline+"</p>"+ hb_osnewline())
                 ELSE
                    oHtm:WriteText("<H1>"+ alltrim(PAD( cFuncName, 21 )) + "</H1>")
                    AADD( aWWW,{ cFuncName,LEFT(cFileName,AT(".",cFileName)-1)} )
                    oHtm:WriteText("<p>"+cOneline+"</p>"+ hb_osnewline())
                 Endif
                 lFirstSintax := .T.
               //  4) all other stuff

            ELSE
              if lDoc

               IF AT( cSyn, cBuffer ) > 0
                  oHtm:WriteParBold( " Syntax" ,.f.,.f.)
                  ohtm:WriteText('<DD><P>')
                  nMode     := D_SYNTAX
                  lAddBlank := .T.
                  lEndSyntax := .T.

               ELSEIF AT( cArg, cBuffer ) > 0

                  oHtm:WriteParBold( " Arguments" )
                  ohtm:WriteText('<DD><P>') 

                  nMode     := D_ARG
                  lAddBlank := .T.
                  lEndArgs:=.t.
               ELSEIF AT( cRet, cBuffer ) > 0

                  IF !lBlankLine
*                     oHtm:WritePar( "" )
                  ENDIF

                  oHtm:WriteParBold( " Returns" )
                  ohtm:WriteText('<DD><P>')
                  nMode     := D_ARG
                  lAddBlank := .T.
                  lEndReturns:=.t.
               ELSEIF AT( cDesc, cBuffer ) > 0
                  IF !lBlankLine
                     oHtm:WriteText( "<br>" )
                  ENDIF

                  oHtm:WriteParBold( " Description" )
                  ohtm:WriteText('<DD><P>')

                  nMode     := D_NORMAL
                  lAddBlank := .T.
                  lEndDesc:=.t.

               ELSEIF AT( cExam, cBuffer ) > 0

                  IF !lBlankLine
*                     oHtm:WritePar( "" )
                  ENDIF
                  oHtm:WriteParBold( " Examples" )
                  oHtm:WriteText("<DD><PRE>")
                  nMode     := D_EXAMPLE
                  lAddBlank := .T.
                  lAddEndPreTag:=.T.                     
                  

               ELSEIF AT( cTest, cBuffer ) > 0

                  IF !lBlankLine
 *                    oHtm:WritePar( "" )
                  ENDIF
                   oHtm:WriteParBold( " Tests", .t.,.f.)
                   oHtm:WriteText("<DD><P>")
                  nMode     := D_NORMAL
                  lAddBlank := .T.

               ELSEIF AT( cStatus, cBuffer ) > 0

                  nMode := D_STATUS

               ELSEIF AT( cCompl, cBuffer ) > 0

                  IF !lBlankLine
*                     oHtm:WritePar( "" )
                  ENDIF
                  oHtm:WriteParBold( " Compliance" )
                  oHtm:WriteText("<DD><P>")
                  nMode     := D_NORMAL
                  lAddBlank := .T.
               ELSEIF AT( cPlat, cBuffer ) > 0

                  IF !lBlankLine
                  *   oHtm:WritePar( "" )
                  ENDIF
                  oHtm:WriteParBold( " Platforms" )
                  oHtm:WriteText("<DD><P>")
                  nMode     := D_NORMAL
                  lAddBlank := .T.
               ELSEIF AT( cFiles, cBuffer ) > 0

                  IF !lBlankLine
                  *   oHtm:WritePar( "" )
                  ENDIF
                  oHtm:WriteParBold( " Files" )
                  oHtm:WriteText("<DD><P>")
                  nMode     := D_NORMAL
                  lAddBlank := .T.
               ELSEIF AT( cFunction, cBuffer ) > 0

                  IF !lBlankLine
                  *   oHtm:WritePar( "" )
                  ENDIF
                  oHtm:WriteParBold( " Functions" )
                  oHtm:WriteText("<DD><P>")
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
                        write_error( "Syntax", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     IF lAddBlank
                        lAddBlank := .F.
                     ENDIF
                     cBuffer := ProcwwwBuf( cBuffer )
                     if lFirstSintax
                       oHtm:WriteText( cBuffer )
                       lFirstSintax:=.f.
                     Else
                        cBuffer:="<Br>"+cBuffer
                        oHtm:WriteText( cBuffer )
                     Endif
                     lFirstArg := .T.
                  ELSEIF nMode = D_ARG
                     IF LEN( cBuffer ) > LONGLINE
                        write_error( "Arguments", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     IF lAddBlank
                        lAddBlank := .F.
                     ENDIF
                     IF AT("<",Alltrim(cBuffer))> 0
                        nPos := AT("<",ALLTRIM(cBuffer))
                        IF nPos <= 3 
                          cBuffer := STRTRAN( cBuffer, "<", "&lt;" )                     
                          cBuffer := STRTRAN( cBuffer, ">", "&gt;" )
                          IF lFirstArg
                            cBuffer:= "<B>" + Substr(cBuffer,At("&lt;",cBuffer)-1,At("&gt;",cBuffer)-2)+"</b>"+Substr(cBuffer,At("&gt;",cBuffer)+4)
                            lFirstArg:=.F.
                          ELSE
                            cBuffer:= "<br><B>" + Substr(cBuffer,At("&lt;",cBuffer)-1,At("&gt;",cBuffer)-2)+"</b>"+Substr(cBuffer,At("&gt;",cBuffer)+4)
                          ENDIF
                        ELSE
                          cBuffer := STRTRAN( cBuffer, "<", "&lt;" )                     
                          cBuffer := STRTRAN( cBuffer, ">", "&gt;" )                        
                        ENDIF
                     ENDIF 
                                         
                     oHtm:WriteText( cBuffer )

                  ELSEIF nMode = D_NORMAL
                     IF LEN( cBuffer ) > LONGLINE
                        write_error( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )

                     IF lBlankLine
                        oHtm:WriteText('<br>')
                        lAddBlank := .F.
                     ENDIF
                     ohtm:WriteText(Alltrim(StripNgControls( cBuffer )))

                  ELSEIF nMode = D_EXAMPLE
                     IF LEN( cBuffer ) > LONGLINE
                        write_error( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     IF lAddBlank
                        lAddBlank := .F.
                     ENDIF
                     oHtm:WriteText( strtran(StripNgControls( cBuffer ),space(6),"") )

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
                  ELSEIF nMode = D_STATUS
                     IF !EMPTY( cBuffer )
                        oHtm:WriteParBold( "Status" )
                        oHtm:WriteText("<DD><P>")
                     ENDIF
                     ProcStatusWww( oHtm, cBuffer )

                  ELSE

                     //  unknown data from somewhere

                     write_error( "Unknown Data Type " + cBuffer,, ;
                                  nLineCnt, ;
                                  LONGONELINE, aDirList[ i, F_NAME ] )

                  ENDIF
               ENDIF
              ELSEIF lClassDoc
               IF AT( cSyn, cBuffer ) > 0 .or. AT( cConstruct, cBuffer ) > 0
                  IF AT( cSyn, cBuffer )>0                  
                     oHtm:WriteParBold( " Syntax",.F.,.f. )
                  ELSEIF AT( cConstruct, cBuffer ) > 0
                     oHtm:WriteParBold( " Constructor syntax", .F.,.f.)
                  ENDIF
                  ohtm:WriteText('<DD><P>')
                  nMode     := D_SYNTAX
                  lAddBlank := .T.
                  lEndSyntax := .T.
                
               ELSEIF AT( cArg, cBuffer ) > 0

                  oHtm:WriteParBold( " Arguments" )
                  ohtm:WriteText('<DD><P>') 

                  nMode     := D_ARG
                  lAddBlank := .T.
                  lEndArgs:=.t.
               ELSEIF AT( cRet, cBuffer ) > 0

                  IF !lBlankLine
*                     oHtm:WritePar( "" )
                  ENDIF

                  oHtm:WriteParBold( " Returns" )
                  ohtm:WriteText('<DD><P>')
                  nMode     := D_ARG
                  lAddBlank := .T.
                  lEndReturns := .T.
               ELSEIF AT( cDesc, cBuffer ) > 0
                  IF !lBlankLine
                     oHtm:WriteText( "<br>" )
                  ENDIF


                  oHtm:WriteParBold( " Description" )
                  ohtm:WriteText('<DD><P>')
                  nMode     := D_NORMAL
                  lAddBlank := .T.
                  lEndDesc := .T.
               ELSEIF AT( cdatalink, cBuffer ) > 0

                  oHtm:WriteParBold( " Data" )
                  ohtm:WriteText('<DD><P>')
                  nMode     := D_DATALINK
                  lAddBlank := .T.
                  lEnddatalink := .T.
                  lIsDataLink := .T.

               ELSEIF AT( cDatanolink, cBuffer ) > 0
                  if !lisdatalink
                  oHtm:WriteParBold( " Data" )

                  ohtm:WriteText('<DD><P>')
                  endif
                  nMode     := D_NORMAL
                  lAddBlank := .T.
                  lEndDatalink:=.t.

               ELSEIF AT(  cMethodslink, cBuffer ) > 0

                  oHtm:WriteParBold( " Method" )
                  ohtm:WriteText('<DD><P>')
                  nMode     := D_METHODLINK
                  lAddBlank := .T.
                  lEndMethodlink := .T.
                  lIsMethodLink := .T.

               ELSEIF AT(  cMethodsnolink, cBuffer ) > 0
                  if !lisdatalink
                  oHtm:WriteParBold( " METHOD" )

                  ohtm:WriteText('<DD><P>')
                  endif

                  nMode     := D_NORMAL
                  lAddBlank := .T.
                  lEndMethodlink := .T.


               ELSEIF AT( cExam, cBuffer ) > 0

                  IF !lBlankLine
*                     oHtm:WritePar( "" )
                  ENDIF
                  oHtm:WriteParBold( " Examples" )
                  oHtm:WriteText("<DD><PRE>")
                  nMode     := D_EXAMPLE
                  lAddBlank := .T.
                  lAddEndPreTag:=.T.                     
                  

               ELSEIF AT( cTest, cBuffer ) > 0

                  IF !lBlankLine
 *                    oHtm:WritePar( "" )
                  ENDIF
                   oHtm:WriteParBold( " Tests" )
                   oHtm:WriteText("<dd><p>")
                  nMode     := D_NORMAL
                  lAddBlank := .T.

               ELSEIF AT( cStatus, cBuffer ) > 0

                  nMode := D_STATUS

               ELSEIF AT( cCompl, cBuffer ) > 0

                  IF !lBlankLine
*                     oHtm:WritePar( "" )
                  ENDIF
                  oHtm:WriteParBold( " Compliance" )
                  oHtm:WriteText("<dd><p>")
                  nMode     := D_NORMAL
                  lAddBlank := .T.
               ELSEIF AT( cPlat, cBuffer ) > 0

                  IF !lBlankLine
                  *   oHtm:WritePar( "" )
                  ENDIF
                  oHtm:WriteParBold( " Platforms" )
                  oHtm:WriteText("<dd><p>")
                  nMode     := D_NORMAL
                  lAddBlank := .T.
               ELSEIF AT( cFiles, cBuffer ) > 0

                  IF !lBlankLine
                  *   oHtm:WritePar( "" )
                  ENDIF
                  oHtm:WriteParBold( " Files" )
                  oHtm:WriteText("<dd><p>")
                  nMode     := D_NORMAL
                  lAddBlank := .T.
               ELSEIF AT( cFunction, cBuffer ) > 0
                  if lAddEndPreTag
                     oHtm:WriteText("</PRE></DD>")
                     lAddEndPreTag:=.f.                     
                  Endif

                  IF !lBlankLine
                  *   oHtm:WritePar( "" )
                  ENDIF
                  oHtm:WriteParBold( " Functions" )
                  oHtm:WriteText("<dd><p>")
                  nMode     := D_NORMAL
                  lAddBlank := .T.

               ELSEIF AT( cSee, cBuffer ) > 0
                  if lAddEndPreTag
                     oHtm:WriteText("</PRE></DD>")
                     lAddEndPreTag:=.f.                     
                  Endif

                  nMode := D_SEEALSO
               ELSEIF AT( cInc, cBuffer ) > 0
                  if lAddEndPreTag
                     oHtm:WriteText("</PRE></DD>")
                     lAddEndPreTag:=.f.                     
                  Endif

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
                        lAddBlank := .F.
                     ENDIF
                     cBuffer := ProcwwwBuf( cBuffer )

                     oHtm:WriteText( cBuffer )
                     lFirstArg := .T.
                  ELSEIF nMode = D_ARG
                     IF LEN( cBuffer ) > LONGLINE
                        write_error( "Arguments", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     IF lAddBlank
                        lAddBlank := .F.
                     ENDIF
                     IF AT("<",Alltrim(cBuffer))> 0
                        nPos := AT("<",ALLTRIM(cBuffer))
                        IF nPos <= 3 
                          cBuffer := STRTRAN( cBuffer, "<", "&lt;" )                     
                          cBuffer := STRTRAN( cBuffer, ">", "&gt;" )
                          IF lFirstArg
                            cBuffer:= "<B>" + Substr(cBuffer,At("&lt;",cBuffer)-1,At("&gt;",cBuffer)-2)+"</b>"+Substr(cBuffer,At("&gt;",cBuffer)+4)
                            lFirstArg:=.F.
                          ELSE
                            cBuffer:= "<br><B>" + Substr(cBuffer,At("&lt;",cBuffer)-1,At("&gt;",cBuffer)-2)+"</b>"+Substr(cBuffer,At("&gt;",cBuffer)+4)
                          ENDIF
                        ELSE
                          cBuffer := STRTRAN( cBuffer, "<", "&lt;" )                     
                          cBuffer := STRTRAN( cBuffer, ">", "&gt;" )                        
                        ENDIF
                     ENDIF 
                                         
                     oHtm:WriteText( cBuffer )

                  ELSEIF nMode = D_NORMAL
                     IF LEN( cBuffer ) > LONGLINE
                        write_error( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     IF lAddBlank
                        lAddBlank := .F.
                     ENDIF
                     ohtm:WriteText(Alltrim(StripNgControls( cBuffer )))

                  ELSEIF nMode = D_DATALINK
                     IF LEN( cBuffer ) > LONGLINE
                        write_error( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     IF lAddBlank
                        lAddBlank := .F.
                     ENDIF
                     cTemp:=ALLTRIM(Substr(cBuffer,1,AT(":",cBuffer)-1))
                     ohtm:WriteText("<a href="+cFileName+"#"+UPPERLOWER(cTemp)+">"+ cBuffer+'</a>')

                  ELSEIF nMode = D_METHODLINK
                     IF LEN( cBuffer ) > LONGLINE
                        write_error( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     IF lAddBlank
                        lAddBlank := .F.
                     ENDIF
                     cTemp:=ALLTRIM(Substr(cBuffer,1,AT("(",cBuffer)-1))
                     ohtm:WriteText("<a href="+cFileName+"#"+UPPERLOWER(cTemp)+">"+ cBuffer+'</a>')


                  ELSEIF nMode = D_EXAMPLE
                     IF LEN( cBuffer ) > LONGLINE
                        write_error( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     IF lAddBlank
                        lAddBlank := .F.
                     ENDIF
                     oHtm:WriteText( strtran(StripNgControls( cBuffer ),space(6),"") )

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
                  ELSEIF nMode = D_STATUS
                     IF !EMPTY( cBuffer )
                        oHtm:WriteParBold( "Status" )
                        oHtm:WriteText("<DD><P>")
                     ENDIF
                     ProcStatusWww( oHtm, cBuffer )

                  ELSE

                     //  unknown data from somewhere

                     write_error( "Unknown Data Type " + cBuffer,, ;
                                  nLineCnt, ;
                                  LONGONELINE, aDirList[ i, F_NAME ] )

                  ENDIF
               ENDIF

            ENDIF
////////////////////
            ENDIF
         ENDIF


         if !lClassDoc .and. lEof
            if valtype(oHtm)=="O"
              oHtm:WriteText('</p></dd></dl>')
              oHtm:Close()
            Endif

          
         ENDIF

      ENDDO
      //  Close down the input file

      FT_FUSE()
               if lClassDoc
               oHtm:Close()
               Endif

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

   cPar:=STRTRAN(cPar,"<","&lt;")
   cPar:=STRTRAN(cPar,">","&gt;")

RETURN cPar

*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
*+    Function ProcWwwAlso()
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
               nLen -= LEN( xTemp ) + 3
               cTemp := xTemp
               xTemp:=Substr(xTemp,1,xPos-1 ) + Substr(xTemp,xPos+1)
               
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
                xTemp:=Substr(xTemp,1,xPos-1 )+Substr(xTemp,xPos+1)
               
            ELSE
               nLen -= LEN( xTemp ) + 2

               cTemp := xTemp
            END
         END

      ENDIF

      nWriteHandle:WriteLink( ALLTRIM( xTemp ),cTemp )
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
*+北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北北
*+
FUNCTION ProcStatusWww( nWriteHandle, cBuffer )
   IF LEN( ALLTRIM( cBuffer ) ) >1
      nWriteHandle:WriteText( cBuffer)
   ELSEIF SUBSTR( ALLTRIM( cBuffer ), 1 ) == "R"
      nWriteHandle:WriteText( "   Ready" )
   ELSEIF SUBSTR( ALLTRIM( cBuffer ), 1 ) == "S"
      nWriteHandle:WriteText( "   Started" )
   ELSEIF SUBSTR( ALLTRIM( cBuffer ), 1 ) == "C"
      nWriteHandle:WriteText( "   Clipper" )
   ELSE
      nWriteHandle:WriteText( "   Not Started" )
   ENDIF

RETURN nil
