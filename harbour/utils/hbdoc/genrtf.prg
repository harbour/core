
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
MEMVAR aDirList,aDocinfo
STATIC aFiTable := {}
STATIC lIsTable :=.F.
STATIC nCommentLen
STATIC lEof

STATIC aColorTable:={{'aqua','\cf2 '},{'black','\cf1 '},{'fuchia','\cf3 '},{'grey','\cf4 '},{'green','\cf5 '},{'lime','\cf6 '},{'maroon','\cf7 '},{'navy','\cf8 '},{'olive','\cf9 '},{'purple','\cf10 '},{'red','\cf11 '},{'silver','\cf12 '},{'teal','\cf13 '},{'white','\cf14 '},{'yellow','\cf15 '}}

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
   LOCAL nFiles := LEN( aDirList )
   LOCAL lDoc
   LOCAL cBuffer
   LOCAL nEnd
   LOCAL nCount
   LOCAL xAddBlank
   LOCAL nNumTopics :=0
   LOCAL nCurTopics :=1
   LOCAL cBar       := " "+ repl(')',80)
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
   LOCAL nPos,nEpos,nPosend,cBuffEnd
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
               oRtf:WriteTitle( PAD( cFuncName, 21 ), cFuncName ,cOneLine)
//               oRtf:WriteParBold( cOneLine )
//               oRtf:WriteParBox(  cBar  )
               ENDIF
               //  4) all other stuff

            ELSE

               IF AT( cSyn, cBuffer ) > 0
                  IF !lBlankLine

                     oRtf:WritePar( "" )

                  ENDIF

                  oRtf:WriteParBold( " Syntax" )
                  oRtf:WritePar('') //:endpar()
                  nMode     := D_SYNTAX
//                  oRtf:WritePar('') //:endpar()
                  lAddBlank := .T.
               ELSEIF AT( cConstruct, cBuffer ) > 0
                  IF !lBlankLine

                     oRtf:WritePar( "" )

                  ENDIF

                     oRtf:WriteParBold( " Constructor syntax" )

                  nMode     := D_SYNTAX
                  lAddBlank := .T.



               ELSEIF AT( cArg, cBuffer ) > 0

                  IF !lBlankLine

                     oRtf:WritePar( "" )

                  ENDIF
                     oRtf:WriteParBold( " Arguments" )
                                       oRtf:WritePar('') //:endpar()
                  nMode     := D_ARG
                  lAddBlank := .T.
                  lPar:=.t.
               ELSEIF AT( cRet, cBuffer ) > 0

                  IF !lBlankLine
                     oRtf:WritePar( "" ) //:endpar()
                  ENDIF

                  oRtf:WriteParBold( " Returns" )
                  oRtf:WritePar( "" ) //:endpar()
                  nMode     := D_RETURN
                  lAddBlank := .T.
                           lPar:=.t.
               ELSEIF AT( cDesc, cBuffer ) > 0
                  oRtf:WritePar('') //:endpar()
                  oRtf:WriteParBold( " Description" )
                  oRtf:WritePar('') //:endpar()
                  nMode     := D_DESCRIPTION
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
  //                                  oRtf:WritePar('') //:endpar()
                                    oRtf:WriteParBold( " Examples" )
                  ENDIF

                  nMode     := D_EXAMPLE
                  lAddBlank := .T.

               ELSEIF AT( cTest, cBuffer ) > 0

                  IF !lBlankLine
//                                      oRtf:WritePar('') //:endpar()   
                     oRtf:WriteParBold( " Tests" )
                                         oRtf:WritePar('') //:endpar()   
                  ENDIF

                  
                  nMode     := D_EXAMPLE
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

                  nMode     := D_COMPLIANCE
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
                     if At("<par>",cBuffer)>0
                         strtran(cBuffer,"<par>",'')
                         strtran(cBuffer,"</par>",'')
                         cBuffer:=Alltrim(cBuffer)
                         cbuFfer:='<par><b>'+cBuffer+'</b></par>'
                     endif
                      procrtfdesc(cbuffer,oRtf,"Syntax")  
//                      oRtf:WritePar('') //:endpar()
                  ELSEIF nMode = D_RETURN

                     IF LEN( cBuffer ) > LONGLINE
                        write_error( "Arguments", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )

                  procrtfdesc(cbuffer,oRtf,"Arguments")

                  ELSEIF nMode = D_ARG

                     IF LEN( cBuffer ) > LONGLINE
                        write_error( "Arguments", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )

                  procrtfdesc(cbuffer,oRtf,"Arguments")
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
                             procrtfdesc(cBuffer,oRtf)
                  ELSEIF nMode = D_COMPLIANCE
                     IF LEN( cBuffer ) > LONGLINE
                        write_error( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     procrtfdesc(cBuffer,oRtf,"Compliance")


                  ELSEIF nMode = D_DESCRIPTION
                     IF LEN( cBuffer ) > LONGLINE
                        write_error( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     procrtfdesc(cBuffer,oRtf,"Description")
        

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

                     procrtfdesc(cBuffer,oRtf,"Example")
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
//                        oRtf:WritePar('') //:endpar()
                        oRtf:WriteParBold( "Status" )
                        oRtf:WritePar('') //:endpar()
                        xaddblank:=.T.
                     ELSE
                        oRtf:WritePar("") //:endpar()
                        xAddBlank:=.T.
                     ENDIF
                     procrtfstatus( oRtf, cBuffer )
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

FUNCTION procrtfstatus( nWriteHandle, cBuffer )
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
FUNCTION  ProcRTFDesc(cBuffer,oRtf,cStyle)
LOCAL cLine:=''
LOCAL npos,CurPos:=0
LOCAL nColorPos,ccolor:='',creturn:='',ncolorend,NIDENTLEVEL,coline
LOCAL lEndPar:= .F.

LOCAL lEndFixed:=.F.
LOCAL lEndTable:=.F.
LOCAL lArgBold:=.f.
default cStyle to "Default"
if at('<par>',cBuffer)==0 .and. !empty(cBuffer) .and. cstyle<>"Example"
    cBuffer:='<par>'+cBuffer
endif

if empty(cBuffer)
oRtf:WritePar("")
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
            cOLine:=left(cReturn,nPos-1)
            cReturn:=STRTRAN(cReturn,coLine,"")
            if at("@",cOLine)>0 .or. at("()",cOLine)>0 .or. at("<",cOLine)>0 .or. at("_",cOLine)>0
             lArgBold:=.T.
            else
            lArgBold:= .f.
           endif

        //            cBuffer:= strtran(cBuffer,"<par>","<par><b>")
    if lArgBold
        cReturn:='       <par><b>'+cOLine+'</b> '+cReturn+'    </par>'
      else
            cReturn:='       <par>'+cOLine+' '+cReturn+'    </par>'
      endif

      cbuffer:=cReturn
      endif
 
      else
      cBuffer:=FormatrtfBuff(cBuffer,cStyle,ortf)
      endif
endif
endif


If AT('<par>',cBuffer)>0 .and. AT('</par>',cBuffer)>0
      cBuffer:=Strtran(cBuffer,'<par>','')
      cBuffer:=StrTran(cBuffer,'<b>','\b ')
      cBuffer:=StrTran(cBuffer,'</b>','\b0 ')
      cBuffer:=StrTran(cBuffer,'<em>','\b\i ')
      cBuffer:=StrTran(cBuffer,'</em>','\b0\i0 ')
      cBuffer:=StrTran(cBuffer,'<i>','\i ')
      cBuffer:=StrTran(cBuffer,'</i>','\i0 ')
      cBuffer:=Strtran(cBuffer,'</color>','\cf1 ')
      nColorPos:=at('<color:',cBuffer)
      if ncolorpos>0
      checkrtfcolor(@cbuffer,ncolorpos)
      endif

      If cStyle=="Description" .or. cStyle=="Compliance"
          nIdentLevel:=6
          nPos:=0
          if AT('</par>',cBuffer)>0
             cBuffer:=strtran(cBuffer,"</par>","")
          endif
          if  !empty(cBuffer)
//             cBuffer:=SUBSTR(cBuffer,2)
             cBuffeR:=Alltrim(cBuffer)
             oRtf:WritePar("       "+cBuffer+' ','\fi-426\li426 ')
          endif

      ELSEIf cStyle=="Arguments"

         if AT('</par>',cBuffer)>0
            cBuffer:=strtran(cBuffer,"</par>","")
         endif
         if  !empty(cBuffer)
                      cBuffeR:=Alltrim(cBuffer)
             oRtf:WritePar("       "+cBuffer+' ','\fi-2272\li2272 ')
         endif

      ELSEIf cStyle=="Syntax"
          if AT('</par>',cBuffer)>0
             cBuffer:=strtran(cBuffer,"</par>","")
          endif
          if  !empty(cBuffer)
//                    cBuffer:=SUBSTR(cBuffer,2)
                                 cBuffeR:=Alltrim(cBuffer)
             oRtf:WritePar(cBuffer+' ','\fi-426\li426  ')
          endif

Elseif cStyle=="Default"
          if AT('</par>',cBuffer)>0
             cBuffer:=strtran(cBuffer,"</par>","")
          endif
          if  !empty(cBuffer)
  //                  cBuffer:=SUBSTR(cBuffer,2)
             cBuffeR:=Alltrim(cBuffer)
             oRtf:WritePar("       "+cBuffer,'\fi-426\li426 ')
          endif


endif
endif
If AT('<fixed>',cBuffer)>0 .or. cStyle="Example"
         if at('<fixed>',cBuffer)=0 .or. !empty(cBuffer)
            cBuffer:=Strtran(cBuffer,"<par>","")
            cBuffer:=Strtran(cBuffer,"<fixed>","")
            oRtf:WriteParFixed(cBuffer)
         endif                
    do while !lendFixed
                cLine :=  TRIM(SUBSTR( ReadLN( @lEof ), nCommentLen ) )
        if at("</fixed>",cLine)>0
          lendfixed:=.t.
          cLine:=Strtran(cline,"</fixed>","")
        endif
      if At(DELIM,cline)>0 
        ft_fskip(-1)
        lEndfixed:=.t.

    endif
    if at(DELIM,cline)==0
        oRtf:WriteParFixed(cline)
        endif
    enddo

end
if AT('<table>',cBuffer)>0
    do while !lendTable
        cBuffer :=  TRIM(SUBSTR( ReadLN( @lEof ), nCommentLen ) )
        if  at("</table>",cBuffer)>0
          lendTable:=.t.
        else
          procrtftable(cBuffer)
    endif
    enddo
    if lEndTable
      GenrtfTable(oRtf)
    endif
endif

//      If cStyle=="Description" .or. cStyle=="Compliance"
//         oRtf:Writepar('')
//      endif

return nil


Function ProcRtfTable(cBuffer)

LOCAL nPos,cItem,cItem2,cItem3,xtype,nColorpos,cColor
      if AT("<color:",cBuffer)>0
         nColorPos:=AT(":",cBuffer)
         cColor:=SubStr(cBuffer,nColorpos+1)
         nPos:=at(">",ccolor)
            cColor:=substr(ccolor,1,nPos-1)

         cBuffer:=strtran(cbuffer,"</color>","\cf1")
         cBuffer:=STRTRAn(cbuffer,"<color:","")
         cBuffer:=STRTRAn(cbuffer,">","")
         cBuffer:=Strtran(cBuffer,ccolor,'')
         nColorpos:=ASCAn(aColorTable,{|x,y| upper(x[1])==upper(ccolor)})
         cColor:=aColortable[nColorPos,2]
      Endif
      cItem:=cBuffer
      if ccolor<>NIL
        AADD(afiTable,ccolor+cItem)
      else
        AADD(afiTable,cItem)
      endif

Return Nil          

Function GenRtfTable(oRtf)
LOCAL y,nLen2,x,nMax,nSpace,lCar:=.f.,nMax2,nSpace2,nPos1,nPos2,LColor,nPos
LOCAL aLensFItem:={}
LOCAL aLensSItem:={}

  FOR X:=1 to LEN(afitable)
  if AT("\cf",afitable[x])>0
      aadd(aLensfItem,len(Substr(strtran(afitable[x],"\cf1",""),at(" ",afitable[x]))))  
  else
     AADD(aLensFItem,Len(afiTable[x]))
     endif
  NEXT
  ASORT(aLensFItem,,,{|x,y| x > y})



        oRtf:WritePar("")
//  nMax2:=checkcar(aTable,1)+1
 nMax2:=alensfitem[1]
 nPos:=maxrtfelem(afitable)
 nPos2:=ascan(alensfitem,{|x| x==nPos})  


oRtf:WriteParBox("       "+repl(chr(196),80))
FOR x:=1 to len(afiTable)
  ortf:WriteParFixed(IF(at("|",afiTable[x])>0,Strtran(afiTable[x],"|"," "),afiTable[x]),'\fi-426\li426')
Next
oRtf:WriteParBox("       "+repl(chr(196),80))
 oRtf:WritePar("")
afiTable:={}

Return Nil

func checkrtfcolor(cbuffer,ncolorpos)
LOCAL ncolorend,nreturn,cOldColorString,cReturn,ccolor

do while at("<color:",cbuffer)>0
          nColorPos:=AT("<color:",cBuffer)
          ccolor:=substr(cbuffer,ncolorpos+7)
          nColorend:=AT(">",ccolor)
          ccolor:=substr(ccolor,1,nColorend-1)
          cOldColorString:=Substr(cbuffer,ncolorpos)
          nColorend:=AT(">",cOldColorString)
          cOldColorString:=Substr(cOldColorString,1,nColorEnd)
nreturn:=ascan(acolortable,{|x,y| upper(x[1])==upper(ccolor)})
if nreturn >0
  creturn:="\cf"+acolortable[nreturn,2]
endif
cBuffer:=strtran(cBuffer,cOldColorString,cReturn)
enddo
return cbuffer
func maxrtfelem(a)
LOCAL nsize:=len(a)
LOCAL max:=0
LOCAL tam:=0,max2:=0
LOCAL nPos:=1
LOCAL cString
LOCAL ncount
for ncount:=1 to nsize
    if  AT("\cf",a[ncount])>0
      cString:=Substr(strtran(a[ncount],"\cf1",""),6)
      tam:=len(cString)
    else
      tam:=len(a[ncount])
    endif
    max:=if(tam>max,tam,max)
next
nPos:=ascan(a,{|x| Len(x)==max})
return max
FUNCTION FormatrtfBuff(cBuffer,cStyle,ongi)

LOCAL cReturn:=''
LOCAL cLine:=''
LOCAL cBuffend:=''
LOCAL cEnd,cStart ,coline:=''
LOCAL lEndBuff:=.f.
LOCAL nPos,nPosEnd
LOCAL lArgBold:=.f.
      creturn :=cBuffer+' '
      if at('</par>',creturn)>0 .or. empty(cBuffer)
         if empty(cbuffer)
         creturn:=''
         endif
         return creturn
      endif
   if cStyle != "Syntax" .AND. cStyle !="Arguments" .and. cStyle !="Return"
       do while !lendBuff
                cLine :=  TRIM(SUBSTR( ReadLN( @lEof ), nCommentLen ) )
      IF AT('</par>',cLine)>0 
         lEndBuff:=.t.
      ENDIF

      IF EMPTY(cLine)
         lEndBuff:=.t.

            ft_fskip(-1)
      ENDIF
      if At(DELIM,cline)>0
        
          FT_Fskip(-1)
        lEndBuff:=.t.
      endif  
      if at(DELIM,cLine)=0
      cReturn+=' '+alltrim(cLine)+ ' '
      endif
    enddo
    creturn:=strtran(creturn,"<par>","")
    creturn:=strtran(creturn,"</par>","")
    
    cReturn:='<par>'+creturn+'    </par>'
  elseif cStyle=='Syntax'

         cReturn:='<par><b>'+cReturn+' </b></par>'

  ELSEIF cStyle=='Arguments' .or. cStyle=="Return"

  nPos:=0
        cReturn:='<par>'+creturn
    if at("<par>",cReturn)>0
            cReturn:=STRTRAN(cReturn,"<par>","")
            cReturn:=STRTRAN(cReturn,"</par>","")
            cReturn:=alltrim(cReturn)
            nPos:=AT(" ",cReturn)
            cOLine:=left(cReturn,nPos-1)
            cReturn:=STRTRAN(cReturn,coLine,"")
            if at("@",cOLine)>0 .or. at("()",cOLine)>0 .or. at("<",cOLine)>0 .or. at("_",cOLine)>0
             lArgBold:=.T.
            else
            lArgBold:= .f.
           endif

    endif
       DO WHILE !lEndBuff

                cLine :=  TRIM(SUBSTR( ReadLN( @lEof ), nCommentLen ) )
    if aT("</par>",cLine)>0
                 lEndBuff:=.t.
    endif
      IF EMPTY(cLine)
         lEndBuff:=.t.

                 ft_fskip(-1)

      ENDIF
      if At(DELIM,cline)>0
        ft_fskip(-1)
        lEndBuff:=.t.
      endif
      if at(DELIM,cline)=0
      cReturn+=' '+alltrim(cLine)+ ' '
      endif
    enddo
    creturn:=strtran(creturn,"<par>","")
    creturn:=strtran(creturn,"</par>","")
    if lArgBold
        cReturn:='       <par><b>'+cOLine+'</b> '+cReturn+'    </par>'
      else
            cReturn:='       <par>'+cOLine+' '+cReturn+'    </par>'
      endif
    lArgBold:=.F.
   ENDIF

Return cReturn

*/
