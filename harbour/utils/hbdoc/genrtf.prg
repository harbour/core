
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
MEMVAR aDirList,aDocInfo

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

#define D_NORMAL  1
#define D_ARG     2
#define D_SYNTAX  3
#define D_IGNORE  4
#define D_SEEALSO 5
#define D_INCLUDE 6
#define D_ONELINE 7
#define D_STATUS  8
#define D_DATALINK 10
#define D_METHODLINK 11
#define D_EXAMPLE 12
   LOCAL i
   LOCAL j
   LOCAL nFiles      := LEN( aDirList )
   LOCAL nCommentLen
   LOCAL lEof
   LOCAL lDoc
   LOCAL cBuffer
   LOCAL nEnd
   LOCAL nCount
   LOCAL xAddBlank
   LOCAL nNumTopics :=0
   LOCAL nCurTopics :=1
   LOCAL cBar       := "컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�"
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
   LOCAL lBlankLine := .F.                 // Blank line encountered and sent out
   LOCAL lAddBlank  := .F.                 // Need to add a blank line if next line is not blank
   LOCAL lFirstArg := .T.
   LOCAL lData := .F.
   LOCAL lMethod := .F.
   LOCAL oRtf
   LOCAL nReadHandle
   LOCAL lPar
   LOCAL lWrite :=.f.
   LOCAL lWasLine := .F.
   LOCAL nPos,nEpos
   LOCAL lIsDataLink := .F. 
   LOCAL lIsMethodLink := .F.
   LOCAL cName
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
   LOCAL cConstruct := DELIM + 'CONSTRUCTOR' + DELIM
   LOCAL cDatalink  := DELIM + 'DATALINK' + DELIM
   LOCAL cDatanolink  := DELIM + 'DATANOLINK' + DELIM
   LOCAL cMethodslink := DELIM + 'METHODSLINK' + DELIM
   LOCAL cMethodsNolink := DELIM + 'METHODSNOLINK' + DELIM
   LOCAL cData      := DELIM +"DATA"+ DELIM
   LOCAL cMethod    := DELIM +'METHOD' +DELIM
   LOCAL cClassDoc  := DELIM+ "CLASSDOC" + DELIM
   LOCAL cTable     := DELIM +"TABLE" + DELIM
   lFirstArg:=.T.
   lData := .F.
   lMethod := .F.
   lIsDataLink := .F.
   lIsMethodLink := .F.

   lWrite:=.f.
   cTempx:=''
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
      lData:= .F.
      lMethod := .F. 
      lPar:=.T.
      //  First find the author

      DO WHILE .NOT. lEof

         //  Read a line

         cBuffer := TRIM( SUBSTR( ReadLN( @lEof ), nCommentLen ) )
         nLineCnt ++
         IF nLineCnt % 10 = 0
            @ LINELINE, 33 SAY STR( nLineCnt, 5, 0 )
         ENDIF
         //  check to see if we are in doc mode or getting out of doc mode

         IF AT( cDoc, cBuffer ) > 0 .or.  AT( cClassDoc, cBuffer ) > 0
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
               IF .NOT. EMPTY( cSeeAlso ) 
                  oRtf:WritePar("") //:endpar()
                  oRtf:WriteParBold( "See Also" )
                  ProcRtfalso( oRtf, cSeealso )
               Endif
               lDoc := .F.

                  oRtf:EndPage()

               nMode := D_IGNORE
            ENDIF

            @ MODULELINE, 33 CLEAR TO MODULELINE, MAXCOL()
         ENDIF

         //  Act on the input
         IF lDoc
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
                  nEnd := ASCAN( aDocInfo, { | a | a[ 4 ] == cFileName + ".rtf" } )
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

               cFileName := LEFT( cFileName, 21 ) + ".rtf"

               IF oRtf:nHandle < 1
                  ? "Error creating", cFileName, ".rtf"
                  write_error( "Error creating",,,, cFileName + ".rtf" )
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
  
               //  2) Category

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
                    oRtf:WriteJumpTitle( left(cFilename,At('.',cFilename)-1)+ cFuncName, "Data "+cFuncName )
               Elseif lMethod
                    oRtf:WriteJumpTitle( left(cFilename,At('.',cFilename)-1)+cFuncName, "Method " +cFuncName )
               Else
               oRtf:WriteTitle( PAD( cFuncName, 21 ), cFuncName )
               oRtf:WriteParBold( cOneLine,.t. )
               oRtf:WriteParBold(  cBar,.t.  )
               ENDIF
               //  4) all other stuff

            ELSE

               IF AT( cSyn, cBuffer ) > 0

                  oRtf:WriteParBold( " Syntax" )

                  nMode     := D_SYNTAX

                  lAddBlank := .T.
               ELSEIF AT( cConstruct, cBuffer ) > 0

                     oRtf:WriteParBold( " Constructor syntax" )

                  nMode     := D_SYNTAX
                  lAddBlank := .T.



               ELSEIF AT( cArg, cBuffer ) > 0

                  IF !lBlankLine

                     oRtf:WriteParBold( " Arguments" )

                  ENDIF

                  nMode     := D_ARG
                  lAddBlank := .T.
                  lPar:=.t.
               ELSEIF AT( cRet, cBuffer ) > 0

                  IF !lBlankLine
                     oRtf:WritePar( "" ) //:endpar()
                  ENDIF

                  oRtf:WriteParBold( " Returns" )

                  nMode     := D_ARG
                  lAddBlank := .T.
                           lPar:=.t.
               ELSEIF AT( cDesc, cBuffer ) > 0
                  oRtf:WritePar('') //:endpar()
                  oRtf:WriteParBold( " Description" )
                  oRtf:WritePar('') //:endpar()
                  nMode     := D_NORMAL
                  lAddBlank := .T.
                  lPar:= .T.
               ELSEIF AT( cTable, cBuffer ) > 0
                  nMode     := D_EXAMPLE
                  lAddBlank := .T.


               ELSEIF AT( cdatalink, cBuffer ) > 0
                  IF !lBlankLine
                     oRtf:WritePar( "" ) //:endpar()
                  ENDIF
        
                  oRtf:WriteParBold( " Data" )
                  nMode     := D_DATALINK
                  lAddBlank := .T.

                  lIsDataLink := .T.

               ELSEIF AT( cDatanolink, cBuffer ) > 0
                  if !lIsDataLink
                    oRtf:WriteParBold( " Data" )
  
                  endif
                  nMode     := D_NORMAL
                  lAddBlank := .T.
//                  lEndDatalink:=.T.
                  lPar:= .T.
               ELSEIF AT(  cMethodslink, cBuffer ) > 0

                  oRtf:WriteParBold( " Method" )
                  nMode     := D_METHODLINK
                  lAddBlank := .T.

                  lIsMethodLink := .T.
                  
               ELSEIF AT(  cMethodsnolink, cBuffer ) > 0
                  if !lIsMethodLink
                  oRtf:WriteParBold( " Methods" )
                  endif

                  nMode     := D_NORMAL
                  lAddBlank := .T.
                  lPar:= .T.


               ELSEIF AT( cExam, cBuffer ) > 0

                  IF !lBlankLine
                                    //ortf:endpar()
                                    oRtf:WritePar('') //:endpar()
                                    oRtf:WriteParBold( " Examples" )
                  ENDIF

                  nMode     := D_EXAMPLE
                  lAddBlank := .T.

               ELSEIF AT( cTest, cBuffer ) > 0

                  IF !lBlankLine
                                      oRtf:WritePar('') //:endpar()   
                     oRtf:WriteParBold( " Tests" )
                                         oRtf:WritePar('') //:endpar()   
                  ENDIF

                  
                  nMode     := D_NORMAL
                  lAddBlank := .T.
                  lPar:= .T.
               ELSEIF AT( cStatus, cBuffer ) > 0

                  nMode := D_STATUS

               ELSEIF AT( cCompl, cBuffer ) > 0

                  IF !lBlankLine
                    //ortf:endpar()   
                    oRtf:WritePar('') //:endpar()   
                    oRtf:WriteParBold( " Compliance" )
                    oRtf:WritePar('') //:endpar()   
                  ENDIF

                  nMode     := D_NORMAL
                  lAddBlank := .T.
                  lPar:= .T.
               ELSEIF AT( cPlat, cBuffer ) > 0

                  IF !lBlankLine
                    //ortf:endpar()   
                    oRtf:WritePar('') //:endpar()
                    oRtf:WriteParBold( " Platforms" )
                    oRtf:WritePar('') //:endpar()   
                  ENDIF

                  nMode     := D_NORMAL
                  lAddBlank := .T.
                  lPar:= .T.
               ELSEIF AT( cFiles, cBuffer ) > 0

                  IF !lBlankLine
                        oRtf:WritePar('') //:endpar()
                                       oRtf:WriteParBold( " Files" )
                        oRtf:WritePar('') //:endpar()
                  ENDIF


                  lPar:= .T.
                        nMode     := D_NORMAL
                  lAddBlank := .T.

               ELSEIF AT( cFunction, cBuffer ) > 0

                  IF !lBlankLine
                  oRtf:WritePar('') //:endpar()
                                                            oRtf:WriteParBold( " Functions" )
                                                            oRtf:WritePar('') //:endpar()
                  ENDIF


                  nMode     := D_NORMAL
                  lAddBlank := .T.
                                    lPar:= .T.
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
                        oRtf:WritePar( "" ) //:endpar()
                        lAddBlank := .F.
                     ENDIF
                     /*    nNonBlank:=FirstNB(cBuffer)
                        cBuffer=STUFF(cBuffer,nNonBlank,0,"^a1f ")*/
                     oRtf:WritePar( cBuffer ) //:endpar()
                     oRtf:WritePar("") //:endpar()

                  ELSEIF nMode = D_ARG
                     IF LEN( cBuffer ) > LONGLINE
                        write_error( "Arguments", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     IF lAddBlank
                        oRtf:WritePar( "" ) //:endpar()
                        lAddBlank := .F.

                     ENDIF
                     IF AT("_SET_",cBuffer)>0
                            nPos:=AT("_SET_",cBuffer)
                            if nPos<23
                              oRtf:WritePar("\line "+cBuffer+ " \line ")
                            Else
                              oRtf:WriteParText(  " "+ ALLTRIM(cBuffer) +" ")                             
                            endif
                     ELSEIF   StrPos(cBuffer)=12
                          cBuffer:=Substr(cBuffer,6)
                          oRtf:WriteParNoIndent(cBuffer+" ")

                     ELSEIF AT("<",cBuffer)> 0
                        nPos := AT("<",cBuffer)
                        if  nPos>0   .and. nPos<12
                            nEpos:=AT(">",cBuffer)
                            oRtf:WriteParBoldText( substr(cBuffer,1,nEpos ),substr(cBuffer,nEpos+1)+" ")
                        else
                        oRtf:WriteParText(  " "+ ALLTRIM(cBuffer) )
                       endif
                     ELSEIF AT("()",Cbuffer)>0
                        nPos:=AT("()",Cbuffer)
                        nEpos:=AT(")",cBuffer)
                        if nPos>0 .and. nPos<22
                         
                            oRtf:WriteParBoldText( substr(cBuffer,1,nEpos ),substr(cBuffer,nEpos+1)+" ")
                        ELSE
                            oRtf:WriteParText(  " "+ ALLTRIM(cBuffer) +" ")
                        Endif
                      ELSEIf AT('*',cBuffer)>0
                            nPos := AT("*",cBuffer)
                            oRtf:WritePar(Strtran(cBuffer,"*",""))
                            lWasLine:=.T.
                     ELSEIF at("===",cBuffer)>0 .or. at("---",cBuffer)>0
                            oRtf:WritePar(cBuffer+" ")
                     ELSEIF lBlankline
                        //ortf:endpar()
                        ortf:writepar('') //:endpar()
                     ELSE
                            if lWasLine
                                oRtf:WritePar(cBuffer+" ")
                                lWasLine:=.F.
                             ELSE
                                oRtf:WriteParText(  ALLTRIM(cBuffer) +" " )
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
                     cTemp:=Substr(cBuffer,1,AT(":",cBuffer)-1)
                     cBuffer:=Substr(cBuffer,AT(":",cBuffer)+1)
                     oRtf:WriteJumpLink(Left(cfilename,At('.',cFilename)-1)+alltrim(cTemp),cTemp,cBuffer)



                  ELSEIF nMode = D_METHODLINK
                     IF LEN( cBuffer ) > LONGLINE
                        write_error( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     IF lAddBlank
                        lAddBlank := .F.
                     ENDIF
                     cTemp:=Substr(cBuffer,1,AT("()",cBuffer)+1)
                     cName:=Substr(cBuffer,1,AT("()",cBuffer)-1)
                     cBuffer:=Substr(cBuffer,AT("()",cBuffer)+2)
                     oRtf:WriteJumpLink(Left(cfilename,At('.',cFilename)-1)+alltrim(cTemp) ,cTemp,cBuffer)

                  ELSEIF nMode = D_NORMAL
                     IF LEN( cBuffer ) > LONGLINE
                        write_error( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )

                     ProcRtfDesc(cBuffer,lBlankLine,oRtf,@lPar)

                  ELSEIF nMode = D_EXAMPLE
                     IF LEN( cBuffer ) > LONGLINE
                        write_error( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )

                     IF lAddBlank
                        oRtf:WritePar( "" ) //:endpar()
                        lAddBlank := .F.
                     ENDIF
                     oRtf:WritePar(  cBuffer  ) //:endpar()

                  ELSEIF nMode = D_SEEALSO
                     IF .NOT. EMPTY( cBuffer )
                        cSeeAlso := StripFiles( ALLTRIM( cBuffer ) )
                     ENDIF
                  ELSEIF nMode = D_INCLUDE
                     //  read next line
                     IF .NOT. EMPTY( cBuffer )
                        IF !lBlankLine
                           oRtf:WritePar( "" ) //:endpar()
                        ENDIF
                     ENDIF
                  ELSEIF nMode = D_STATUS
                     IF !EMPTY( cBuffer )
                        oRtf:WritePar('') //:endpar()
                        oRtf:WriteParBold( "Status" )
                        oRtf:WritePar('') //:endpar()
                        xaddblank:=.T.
                     ELSE
                        oRtf:WritePar("") //:endpar()
                        xAddBlank:=.T.
                     ENDIF
                     ProcStatusRtf( oRtf, cBuffer )
                  ELSE

                     //  unknown data from somewhere

                     write_error( "Unknown Data Type " + cBuffer,, ;
                                  nLineCnt, ;
                                  LONGONELINE, aDirList[ i, F_NAME ] )

                  ENDIF
                 endif
               ENDIF
//            ENDIF

         ENDIF


      ENDDO
      //  Close down the input file
      FT_FUSE()
   NEXT
                    ortf:close()
RETURN NIL

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

FUNCTION ProcStatusRTF( nWriteHandle, cBuffer )
   IF LEN( ALLTRIM(cBuffer) ) >1
      nWriteHandle:WritePar( cBuffer ) //:endpar()
   ELSEIF SUBSTR( ALLTRIM( cBuffer ), 1 ) == "R"
      nWriteHandle:WritePar( "      Ready" ) //:endpar()
   ELSEIF SUBSTR( ALLTRIM( cBuffer ), 1 ) == "S"
      nWriteHandle:WritePar( "      Started" ) //:endpar()
   ELSE
      nWriteHandle:WritePar( "      Not Started" ) //:endpar()
   ENDIF
RETURN nil
/*
func filesize(cfile)
  
  nretval := fseek(cfile,0,2)
  

return nretval
*/
FUNCTION  ProcRtfDesc(cBuffer,lBlankLine,oRtf,lPar)
          IF   StrPos(cBuffer)=12
              cBuffer:=Substr(cBuffer,6)
              oRtf:WriteParNoIndent(cBuffer+" ")
              lPar:=.f.
          ELSEIF lPar
              oRtf:WritePar(cBuffer+" ")
              lPar:=.F.
          ELSEIF  AT("==",cBuffer)>0 .or. at("--",cBuffer)>0
              lPar:=.T.
              oRtf:WritePar(cBuffer+" ")
          ELSEIF lBlankLine
              oRtf:WritePar("")
              lPar:=.T.
          ELSE    
              oRtf:WriteParText(" "+alltrim(cBuffer))
          ENDIF

RETURN lPar

/*                      if !lBlankLine
                        if lPar
                           if at("==",cBuffer)>0 .or. at("--",cBuffer)>0 .or. at("*",cBuffer)>0
                                if at("*",cBuffer)>0
                                  cbuffer:=Strtran(cBuffer,"*","")
                                 endif
                                oRtf:WritePar(cBuffer+" ")
                              lPar:=.t.
                           else
                               oRtf:WritePar(cBuffer+" ")
                               lPar:=.f.
                          endif
                        else
                           if at("==",cBuffer)>0 .or. at("--",cBuffer)>0 
                                oRtf:WritePar(cBuffer+" ")
                            
                            else
                              oRtf:WriteParText(" "+alltrim(cBuffer))
                              endif
                        endif
                      Endif
                      if lBlankline
                         oRtf:WritePar("")
                         lPar:=.T.
                      endif

*/
FUNCTION StrPos(cBuffer)
LOCAL nPos,x,cChar
      FOR x:=1 to LEN(cBuffer)
          cChar:=SubStr(cBuffer,x,1)
          if cChar>=chr(64) .and. cChar <=Chr(90) .or. cChar>=chr(97) .and. cChar <=Chr(122) .or. cChar>=Chr(48)  .and. cChar <=chr(57)
             nPos=x
             Exit
          ENDIF
      NEXT
return nPos
