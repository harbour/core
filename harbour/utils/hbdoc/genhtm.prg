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
#define LONGLINE     100
#define LONGONELINE  86
MEMVAR aDirList,aDocInfo,aWww

STATIC aAlso
STATIC aFiTable := {}
STATIC aSiTable := {}
STATIC lIsTable :=.F.
STATIC nCommentLen
STATIC lEof
STATIC aFoiTable      := {}
STATIC atiTable       := {}
STATIC nNumTableItems := 0

STATIC aColorTable:={'aqua','black','fuchia','grey','green','lime','maroon','navy','olive','purple','red','silver','teal','white','yellow'}

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
   LOCAL nFiles      := LEN( aDirList )
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
   LOCAL lIsDataLink := .F.
   LOCAL lIsMethodLink := .F.
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

      nCommentLen := IIF( AT( ".ASM", UPPER( aDirList[ i, F_NAME ] ) ) > 0, 2, 4 )
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
         cBuffer := STRTRAN( cBuffer,chr(10),'')
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

            ELSEIF AT( cdata, cBuffer ) > 0 .OR. AT( cmethod, cBuffer ) > 0 
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
               ohtm:WriteText('<br>')
               ohtm:WriteText('<br>')
               ohtm:Writetext('<hr>')
               ohtm:WriteText('<br>')
               ohtm:WriteText('<br>')

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
        
               IF AT( cSyn, cBuffer ) > 0
                  oHtm:WriteParBold( " Syntax" ,.f.,.f.)
                  ohtm:WriteText('<DD><P>')
                  nMode     := D_SYNTAX
                  lAddBlank := .T.
                  lEndSyntax := .T.

               ELSEIF  AT( cConstruct, cBuffer ) > 0
                     oHtm:WriteParBold( " Constructor syntax", .F.,.f.)
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

                  nMode     := D_DESCRIPTION
                  lAddBlank := .T.
                  lEndDesc:=.t.
               
               ELSEIF AT( cExam, cBuffer ) > 0

                  IF !lBlankLine
*                     oHtm:WritePar( "" )
                  ENDIF
                  oHtm:WriteParBold( " Examples" )
                  oHtm:WriteText("<PRE>")
                  nMode     := D_EXAMPLE
                  lAddBlank := .T.
                  lAddEndPreTag:=.T.                     
                  
               
               ELSEIF AT( cTest, cBuffer ) > 0

                  IF !lBlankLine
 *                    oHtm:WritePar( "" )
                  ENDIF
                   oHtm:WriteParBold( " Tests", .t.,.f.)
                   oHtm:WriteText("<DD><P>")
                  nMode     := D_EXAMPLE
                  lAddBlank := .T.

               ELSEIF AT( cStatus, cBuffer ) > 0

                  nMode := D_STATUS

               ELSEIF AT( cCompl, cBuffer ) > 0

                  IF !lBlankLine
*                     oHtm:WritePar( "" )
                  ENDIF
                  oHtm:WriteParBold( " Compliance" )
                  oHtm:WriteText("<DD><P>")
                  nMode     := D_COMPLIANCE
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
                      prochtmdesc(cbuffer,oHtm,"Syntax")  

                  ELSEIF nMode = D_ARG
                     IF LEN( cBuffer ) > LONGLINE
                        write_error( "Arguments", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     IF lAddBlank
                        lAddBlank := .F.
                     ENDIF

          prochtmdesc(cbuffer,oHtm,"Arguments")
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

                   ProcHtmDesc(cBuffer,oHtm)
                  ELSEIF nMode = D_EXAMPLE
                     IF LEN( cBuffer ) > LONGLINE
                        write_error( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     prochtmdesc(cBuffer,oHtm,"Example")
                  ELSEIF nMode = D_DESCRIPTION
                     IF LEN( cBuffer ) > LONGLINE
                        write_error( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     prochtmdesc(cBuffer,oHtm,"Description")

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


                  ELSEIF nMode = D_COMPLIANCE
                     IF LEN( cBuffer ) > LONGLINE
                        write_error( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     prochtmdesc(cBuffer,oHtm,"Compliance")

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
function FormatHtmBuff(cBuffer,cStyle,oHtm)

Local creturn:=''
local cline:=''
LOCAL cOldLine:=''
local cBuffend:=''
local lEndBuffer:=.f.
LOCAL lArgBold:= .f.
local npos,nposend
      creturn :=cBuffer+' '
      if at('</par>',creturn)>0 .or. empty(cBuffer)
         if empty(cbuffer)
         creturn:=''
         endif
         return creturn
      endif
   if cStyle != "Syntax" .AND. cStyle !="Arguments"
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

  elseif cStyle=='Syntax'
            cReturn := STRTRAN( cReturn, "<par>","")
            cReturn := STRTRAN( cReturn, "<", "&lt;" )                     
            cReturn := STRTRAN( cReturn, ">", "&gt;" )
        
         creturn:='<par><b>'+creturn+' </b></par>'
  ELSEIF cStyle=='Arguments'

      nPos    := 0
      IF AT( "<par>", cReturn ) > 0
         cReturn  := STRTRAN( cReturn, "<par>", "" )
         cReturn  := STRTRAN( cReturn, "</par>", "" )
         cReturn  := ALLTRIM( cReturn )
         nPos     := AT( " ", cReturn )
         cOldLine := LEFT( cReturn, nPos - 1 )
         cReturn  := STRTRAN( cReturn, cOldLine, "" )
         IF AT( "@", cOldLine ) > 0 .OR. AT( "()", cOldLine ) > 0 .OR. AT( "<", cOldLine ) > 0 .OR. AT( "_", cOldLine ) > 0
             cOldLine:=STRTRAN(cOldLine,"<","&lt;")
             cOldLine:=STRTRAN(cOldLine,">","&gt;")
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
      
       cReturn:=STRTRAN(cReturn,"<","&lt;")
       cReturn:=STRTRAN(cReturn,">","&gt;")
       cOldLine:=STRTRAN(cOldLine,"<","&lt;")
       cOldLine:=STRTRAN(cOldLine,">","&gt;")

      IF lArgBold
         cReturn := '       <par><b>' + cOldLine + '</b> ' + cReturn + '    </par>'
      ELSE
         cReturn := '       <par>' + cOldLine + ' ' + cReturn + '    </par>'
      ENDIF
//   ENDIF
    lArgBold:=.F.

   ENDIF

//   endif
return creturn


  
func checkhtmcolor(cbuffer,ncolorpos)
local ncolorend,nreturn,cOldColorString,cReturn,ccolor

do while at("<color:",cbuffer)>0
          nColorPos:=AT("<color:",cBuffer)
          ccolor:=substr(cbuffer,ncolorpos+7)
          nColorend:=AT(">",ccolor)
          ccolor:=substr(ccolor,1,nColorend-1)
          cOldColorString:=Substr(cbuffer,ncolorpos)
          nColorend:=AT(">",cOldColorString)
          cOldColorString:=Substr(cOldColorString,1,nColorEnd)
nreturn:=ascan(acolortable,{|x,y| upper(x)==upper(ccolor)})
if nreturn >0
  creturn:='<font color='+acolortable[nreturn]+'>'
endif
cBuffer:=strtran(cBuffer,cOldColorString,cReturn)
enddo
return cbuffer

FUNCTION  ProchtmDesc(cBuffer,oHtm,cStyle)
local cOldLine:=''
Local npos,CurPos:=0
LOCAL nColorPos,ccolor:='',creturn:='',ncolorend,nIdentLevel
LOCAL lEndPar:= .F.
LOCAL cLine:=''
LOCAL lEndFixed:=.F.
LOCAL lArgBold:=.f.
LOCAL LFstTableItem := .T.
LOCAL lEndTable:=.F.
LOCAL lEndBuffer :=.f.
Default cStyle to "Default"

if at('<par>',cBuffer)==0 .and. !empty(cBuffer) .and. cstyle<>"Example"
    cBuffer:='<par>'+cBuffer
endif

if empty(cBuffer)
oHtm:WriteText("<dd><br></dd>")
endif

if cStyle<>"Example" .and. at("<table>",cBuffer)==0 .and. AT("<fixed>",cBuffer)=0
   if AT("<par>",cBuffer)>=0 .or. AT("</par>",cBuffer)=0   .and. !empty(cbuffer) 
      If AT("<par>",cBuffer)>0 .and. AT("</par>",cBuffer)>0
         if cStyle=="Arguments"

            creturn:=cBuffer

            cReturn:=STRTRAN(cReturn,"<par>","")
            cReturn:=STRTRAN(cReturn,"</par>","")

            cReturn:=alltrim(cReturn)
            nPos:=AT(" ",cReturn)
            cOldLine:=left(cReturn,nPos-1)
            cReturn:=STRTRAN(cReturn,cOldLine,"")
             IF AT( "@", cOldLine ) > 0 .OR. AT( "()", cOldLine ) > 0 .OR. AT( "<", cOldLine ) > 0 .OR. AT( "_", cOldLine ) > 0
                lArgBold := .T.
                cOldLine:=STRTRAN(cOldLine,"<","&lt;")
                cOldLine:=STRTRAN(cOldLine,">","&gt;")
         

             ENDIF
    if lArgBold
        cReturn:='       <par><b>'+cOldLine+'</b> '+cReturn+'    </par>'
      else
            cReturn:='       <par>'+cOldLine+' '+cReturn+'    </par>'
      endif

      cbuffer:=cReturn
      endif
      else
      cBuffer:=FormathtmBuff(cBuffer,cStyle,oHtm)
      endif
endif
endif


If AT('<par>',cBuffer)>0 .and. AT('</par>',cBuffer)>0
      cBuffer:=Strtran(cBuffer,'<par>','')
      cBuffer:=strtran(cBuffer,'</color>','</font> ')
      nColorPos:=at('<color:',cBuffer)
      if ncolorpos>0
      checkhtmcolor(@cbuffer,ncolorpos)
      endif
//      Alltrim(cBuffer)
      If cStyle=="Description" .or. cStyle=="Compliance"
          nIdentLevel:=6
          nPos:=0
          if AT('</par>',cBuffer)>0
             cBuffer:=strtran(cBuffer,"</par>","")
          endif
          if  !empty(cBuffer)
//             cBuffer:=SUBSTR(cBuffer,2)
             cBuffeR:=Alltrim(cBuffer)
             oHtm:WritePar(cBuffer)
          endif

      ELSEIf cStyle=="Arguments"

         if AT('</par>',cBuffer)>0
            cBuffer:=strtran(cBuffer,"</par>","")
         endif
         if  !empty(cBuffer)
                      cBuffeR:=Alltrim(cBuffer)
             oHtm:WritePar(cBuffer)
         endif

      ELSEIf cStyle=="Syntax"
          if AT('</par>',cBuffer)>0
             cBuffer:=strtran(cBuffer,"</par>","")
             cBuffer:=strtran(cBuffer,"<par>","")
          endif
          if  !empty(cBuffer)
//                    cBuffer:=SUBSTR(cBuffer,2)
                                 cBuffeR:=Alltrim(cBuffer)
             oHtm:WritePar(cBuffer)
          endif

Elseif cStyle=="Default"
          if AT('</par>',cBuffer)>0
             cBuffer:=strtran(cBuffer,"</par>","")
          endif
          if  !empty(cBuffer)
  //                  cBuffer:=SUBSTR(cBuffer,2)
             cBuffeR:=Alltrim(cBuffer)
             oHtm:WritePar(cBuffer)
          endif


endif
endif
If AT('<fixed>',cBuffer)>0 .or. cStyle="Example"
      IF AT( '<fixed>', cBuffer ) = 0 .OR. !EMPTY( cBuffer )
         cBuffer := STRTRAN( cBuffer, "<par>", "" )
         cBuffer := STRTRAN( cBuffer, "<fixed>", "" )
        
        oHtm:WriteText("<br>")
         oHtm:WritePar( cBuffer )
      ENDIF
      DO WHILE !lendFixed
         cOldLine := TRIM( SUBSTR( ReadLN( @lEof ), nCommentLen ) )
         IF AT( "</fixed>", cOldLine ) > 0
            lendfixed := .t.
            cOldLine     := STRTRAN( cOldLine, "</fixed>", "" )
         ENDIF
         IF AT( DELIM, cOldLine ) = 0
            cReturn += ALLTRIM( cOldLine ) + ' '
         ENDIF
         IF AT( DELIM, cOldLine ) > 0
            FT_FSKIP( - 1 )
            lEndfixed := .t.

         ENDIF
         IF AT( DELIM, cOldLine ) == 0
            oHtm:WritePar( cOldLine )
         ENDIF
      ENDDO
        oHtm:WriteText( "</pre><br>")
end
if AT('<table>',cBuffer)>0
    do while !lendTable
        cLine :=  TRIM(SUBSTR( ReadLN( @lEof ), nCommentLen ) )
        if  at("</table>",cLine)>0
          lendTable:=.t.
        else
            IF LFstTableItem
               nNumTableItems := GetNumberofTableItems( cLine )
               prochtmtable( cline, nNumTableItems )
               LFstTableItem := .f.
            ELSE
               prochtmtable( cline, nNumTableItems )
            ENDIF
          
    endif
    enddo

if lEndTable
    GenhtmTable(oHtm)
endif
endif
return nil
Function ProchtmTable(cBuffer,nNum)

Local nPos,cItem,cItem2,cItem3,nColorpos,cColor,cItem4

      cBuffer:=alltrim(cBuffer)           
      if AT("<color:",cBuffer)>0
         nColorPos:=AT(":",cBuffer)
         cColor:=SubStr(cBuffer,nColorpos+1)
         nPos:=at(">",ccolor)
            cColor:=substr(ccolor,1,nPos-1)

         cBuffer:=strtran(cbuffer,"</color>","")
         cBuffer:=STRTRAn(cbuffer,"<color:","")
         cBuffer:=STRTRAn(cbuffer,">","")
         cBuffer:=Strtran(cBuffer,ccolor,'')
         nColorpos:=ASCAn(aColorTable,{|x,y| upper(x)==upper(ccolor)})
         cColor:=aColortable[nColorPos]
      Endif
      if empty(cBuffer)
      citem:=''
      citem2:=''
      citem3:=''
      citem4:=''
   else
                cBuffer:=STRTRAN(cBuffer,"<","&lt;")
                cBuffer:=STRTRAN(cBuffer,">","&gt;")
         

   cItem   := SUBSTR( cBuffer, 1, AT( SPACE( 3 ), cBuffer ) - 1 )
   cBuffer := ALLTRIM( STRTRAN( cBuffer, cItem, "" ) )
   if nNum==2
       cItem2 := SUBSTR( cBuffer, 1 )
   else       
   cItem2  := SUBSTR( cBuffer, 1, AT( SPACE( 3 ), cBuffer ) - 1 )
   cBuffer := ALLTRIM( STRTRAN( cBuffer, cItem2, "" ) )
    endif

   IF nNum == 3
      cItem3 := SUBSTR( cBuffer, 1 )
   ELSEIF nNum > 3
      cItem3  := SUBSTR( cBuffer, 1, AT( SPACE( 3 ), cBuffer ) - 1 )
      cBuffer := ALLTRIM( STRTRAN( cBuffer, cItem3, "" ) )
      cItem4  := SUBSTR( cBuffer, 1 )
   ENDIF
endif
        if cColor<>NIL
        AADD(afiTable,"<Font color="+ccolor+">"+rtrim(ltrim(cItem))+'</font>')
        AADD(asiTable,"<Font color="+ccolor+">"+cItem2+'</font>')
        Else
        AADD(afiTable,rtrim(ltrim(cItem)))
        AADD(asiTable,cItem2)
        endif


      if cColor <>NIL
          AADD(atiTable,"<Font color="+ccolor+">"+cItem3+'</font>')
      ELSE  
      AADD( atiTable, cItem3 )
      Endif
         if cColor <>NIL
                AADD(afoiTable,"<Font color="+ccolor+">"+cItem4+'</font>')
         ELSE
          AADD( afoiTable, cItem4 )
      Endif



Return Nil
Function GenhtmTable(oHtm)
LOCAL x
        oHtm:WriteText("<br>")
        oHtm:WriteText("<br>")
        oHtm:WriteText('<table border=1>') //-4

FOR x:=1 to len(asitable)
        If !empty(asitable[x])
            if nNumTableItems ==2
                oHtm:WriteText('<tr><td>'+afitable[x]+'</td><td>' +asitable[x]+'</td></tr> ')
            elseif nNumTableItems ==3
               oHtm:WriteText('<tr><td>'+afitable[x]+'</td><td>' +asitable[x]+'</td><td>'+atitable[x]+'</td></tr> ')
            elseif nNumTableItems ==4
                oHtm:WriteText('<tr><td>'+afitable[x]+'</td><td>' +asitable[x]+'</td><td>'+atitable[x]+'</td><td>'+afoitable[x]+'</td></tr> ')
            Endif
        Else
            oHtm:WriteText('<tr><td></td></tr> ')
        endif
Next

  oHtm:Writetext("</table>")

oHtm:WriteText("<br>")
afiTable:={}
asitable:={}
atitable  := {}
afoitable := {}

Return Nil


