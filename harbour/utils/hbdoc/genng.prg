
/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * GENNG support module for hbdoc document Extractor 
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


#include "directry.ch"
#include "fileio.ch"
#include "inkey.ch"
#include 'common.ch'
#include 'hbdocdef.ch'
//  output lines on the screen

#define INFILELINE   10
#define MODULELINE   12
#define LINELINE     14
#define ERRORLINE    20
#define LONGLINE     78
#define LONGONELINE  66
MEMVAR aDirlist,aDocInfo
STATIC aAlso
STATIC aFiTable := {}
STATIC aSiTable := {}
STATIC lIsTable :=.F.
STATIC nCommentLen
STATIC lEof

STATIC aColorTable:={{'aqua','1B'},{'black','10'},{'fuchia','1D'},{'grey','18'},{'green','12'},{'lime','1A'},{'maroon','16'},{'navy','19'},{'olive','12'},{'purple','15'},{'red','1C'},{'silver','17'},{'teal','13 '},{'white','1F'},{'yellow','1E'}}
FUNCTION ProcessiNg()

   //

   //  Copyright (C) 2000 Luiz Rafael Culik
   //
   //  Purpose: Process each of the files in the directory
   //  and Gerenate norton guide source code
   //  Modification History:
   //         Version    Date        Who       Notes
   //          V1.00     1/16/2000   LRC       Initial Version
   //
   //  Calling parameters: None
   //
   //  Notes: None
   // -
   //  LOCAL variables:

   LOCAL i
   LOCAL j
   LOCAL nFiles   := LEN( aDirList )
   LOCAL lDoc
   
   LOCAL cBuffer
   LOCAL nEnd
   LOCAL nCount   ,nAlso

   LOCAL lData := .F.
   LOCAL lMethod := .F.
   LOCAL CBUFFEND
   LOCAL nPos,NPOSEND
   LOCAL lIsDataLink := .F. 
   LOCAL lIsMethodLink := .F.

   LOCAL cBar       := "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
   LOCAL nMode
   LOCAL cFuncName
   LOCAL cOneLine
   LOCAL cCategory
   LOCAL cFileName
   LOCAL nLineCnt
   LOCAL cSeeAlso
   LOCAL cTemp
   LOCAL lPar
   LOCAL cChar
   LOCAL lBlankLine := .F.                 // Blank line encountered and sent out
   LOCAL lAddBlank  := .F.                 // Need to add a blank line if next line is not blank
   LOCAL oNgi
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
   LOCAL cConstruct := DELIM + 'CONSTRUCTOR' + DELIM
   LOCAL cDatalink  := DELIM + 'DATALINK' + DELIM
   LOCAL cDatanolink  := DELIM + 'DATANOLINK' + DELIM
   LOCAL cMethodslink := DELIM + 'METHODSLINK' + DELIM
   LOCAL cMethodsNolink := DELIM + 'METHODSNOLINK' + DELIM
   LOCAL cData      := DELIM +"DATA"+ DELIM
   LOCAL cMethod    := DELIM +'METHOD' +DELIM
   LOCAL cClassDoc  := DELIM+ "CLASSDOC" + DELIM


   lData := .F.
   lMethod := .F.
   lIsDataLink := .F.
   lIsMethodLink := .F.

   lPar := .T.
   //
   //  Entry Point
   //
   //  Put up information labels
   @ INFILELINE, 20 SAY "Extracting: "
   @ MODULELINE, 20 SAY "Documenting: "
   //  loop through all of the files


   FOR i := 1 TO nFiles
    afiTable:={}
    asiTable:={}
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

      DO WHILE .NOT. lEof

         //  Read a line

         cBuffer :=  TRIM(SUBSTR( ReadLN( @lEof ), nCommentLen ) )
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
               lDoc := .F.
               IF .NOT. EMPTY( cSeeAlso )

                  FOR nAlso := 1 TO LEN( aalso )
                     IF nAlso == 1
                        oNgi:WriteLink( "!seealso: " + aalso[ nAlso ] )
                     ELSE
                        oNgi:WriteLink( "," + aalso[ nAlso ] )
                     ENDIF
                  NEXT

                  oNgi:WritePar( CRLF )
               ENDIF

               oNgi:Close()
               nMode := D_IGNORE
            ENDIF

            @ MODULELINE, 33 CLEAR TO MODULELINE, MAXCOL()
         ENDIF

         //  Act on the input
         IF lDoc
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
                  nEnd := ASCAN( aDocInfo, { | a | a[ 4 ] == cFileName + ".ngi" } )
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

               cFileName := LEFT( cFileName, 21 ) + ".ngi"

               oNgi := TNortonGuide():new( "ngi\" + cFileName )
               IF oNgi:nHandle < 1
                  ? "Error creating", cFileName, ".ngi"
                  write_error( "Error creating",,,, cFileName + ".ngi" )
               ENDIF
               //  2) Category
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
                    oNgi:WriteJumpTitle( left(cFilename,At('.',cFilename)-1)+ cFuncName, "Data "+cFuncName )
               Elseif lMethod
                    oNgi:WriteJumpTitle( left(cFilename,At('.',cFilename)-1)+cFuncName, "Method " +cFuncName )
               Else

               oNgi:WriteTitle( PAD( cFuncName, 21 ) + cOneLine, cFuncName )
               oNgi:WritePar( cOneLine )
               oNgi:WritePar(  cBar )
               //  4) all other stuff
               endif
            ELSE

               IF AT( cSyn, cBuffer ) > 0

                  oNgi:WriteParBold( " Syntax" )

                  nMode     := D_SYNTAX
                  lAddBlank := .T.
               ELSEIF AT( cConstruct, cBuffer ) > 0

                     oNgi:WriteParBold( " Constructor syntax" )

                  nMode     := D_SYNTAX
                  lAddBlank := .T.


               ELSEIF AT( cArg, cBuffer ) > 0

                  IF !lBlankLine

                     oNgi:WriteParBold( " Arguments" )

                  ENDIF

                  nMode     := D_ARG
                  lAddBlank := .T.

               ELSEIF AT( cRet, cBuffer ) > 0

//                  IF !lBlankLine
//                     oNgi:WritePar( "" )
//                  ENDIF

                  oNgi:WriteParBold( " Returns" )

                  nMode     := D_ARG
                  lAddBlank := .T.

               ELSEIF AT( cDesc, cBuffer ) > 0

                  oNgi:WriteParBold( " Description" )

                  nMode     := D_DESCRIPTION
                  lAddBlank := .T.
                  lPar := .T.
               ELSEIF AT( cdatalink, cBuffer ) > 0
                  IF !lBlankLine
                     oNgi:WritePar( "" ) //:endpar()
                  ENDIF
        
                  oNgi:WriteParBold( " Data" )
                  nMode     := D_DATALINK
                  lAddBlank := .T.

                  lIsDataLink := .T.

               ELSEIF AT( cDatanolink, cBuffer ) > 0
                  if !lIsDataLink
                    oNgi:WriteParBold( " Data" )
  
                  endif
                  nMode     := D_NORMAL
                  lAddBlank := .T.

                  lPar:= .T.
               ELSEIF AT(  cMethodslink, cBuffer ) > 0

                  oNgi:WriteParBold( " Method" )
                  nMode     := D_METHODLINK
                  lAddBlank := .T.

                  lIsMethodLink := .T.
                  
               ELSEIF AT(  cMethodsnolink, cBuffer ) > 0
                  if !lIsMethodLink
                  oNgi:WriteParBold( " Methods" )
                  endif

                  nMode     := D_NORMAL
                  lAddBlank := .T.
                  lPar:= .T.


               ELSEIF AT( cExam, cBuffer ) > 0

                  IF !lBlankLine
//                     oNgi:WritePar( "" )
                  oNgi:WriteParBold( " Examples" )
                  ENDIF

                  nMode     := D_EXAMPLE
                  lAddBlank := .T.
                  
               ELSEIF AT( cTest, cBuffer ) > 0

                  IF !lBlankLine

                  oNgi:WriteParBold( " Tests" )
                  ENDIF


                  nMode     := D_NORMAL
                  lAddBlank := .T.
                                    lPar:=.t.
               ELSEIF AT( cStatus, cBuffer ) > 0

                  nMode := D_STATUS

               ELSEIF AT( cCompl, cBuffer ) > 0

                  IF !lBlankLine
                    oNgi:WriteParBold( " Compliance" )
                  ENDIF

                  nMode     := D_COMPLIANCE
                  lAddBlank := .T.
                                    lPar:=.t.
               ELSEIF AT( cPlat, cBuffer ) > 0

                  IF !lBlankLine
                    oNgi:WriteParBold( " Platforms" )
                  ENDIF

                  nMode     := D_NORMAL
                  lAddBlank := .T.
                                    lPar:=.t.
               ELSEIF AT( cFiles, cBuffer ) > 0

                  IF !lBlankLine
                    oNgi:WriteParBold( " Files" )
                  ENDIF

                  nMode     := D_NORMAL
                  lAddBlank := .T.
                                    lPar:=.t.
               ELSEIF AT( cFunction, cBuffer ) > 0

//                  IF !lBlankLine
//                     oNgi:WritePar( "" )
//                  ENDIF
                  oNgi:WriteParBold( " Functions" )

                                    lPar:=.t.
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
                     if AT("<par>",cBuffer)>0
                      nPos:=At("->",cBuffer)
                      if nPos>0
                      nPosend:=AT("</par>",cBuffer)
                              
                      cBuffend:=Substr(cBuffer,nPos+2,nPosend-2)
                      cBuffEnd:=Strtran(cBuffend,"</par>","")
                      cBuffer:=SubStr(cBuffer,1,nPos+2)

                      cBuffer:=cBuffer+'<color:aqua> '+cBuffend+'</color> </par>'
                      endif
                      Endif
                      procngdesc(cbuffer,oNgi,"Syntax")  
                  ELSEIF nMode = D_ARG
                     IF LEN( cBuffer ) > LONGLINE
                        write_error( "Arguments", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     IF lAddBlank
                        oNgi:WritePar( "" )
                        lAddBlank := .F.
                     ENDIF
                     
          procngdesc(cbuffer,oNgi,"Arguments")
                  ELSEIF nMode = D_EXAMPLE
                     IF LEN( cBuffer ) > LONGLINE
                        write_error( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     ProcNgDesc(cBuffer,oNgi,"Example")
                  ELSEIF nMode = D_DESCRIPTION
                     IF LEN( cBuffer ) > LONGLINE
                        write_error( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     ProcNgDesc(cBuffer,oNgi,"Description")

                  ELSEIF nMode = D_NORMAL
                     IF LEN( cBuffer ) > LONGLINE
                        write_error( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     ProcNgDesc(cBuffer,oNgi)
                  ELSEIF nMode = D_COMPLIANCE
                     IF LEN( cBuffer ) > LONGLINE
                        write_error( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     ProcNgDesc(cBuffer,oNgi,"Compliance")

                  ELSEIF nMode = D_SEEALSO
                     IF .NOT. EMPTY( cBuffer )
                        cSeeAlso := ProcNgiAlso( StripFiles( ALLTRIM( cBuffer ) ) )
                     ENDIF
                  ELSEIF nMode = D_INCLUDE
                     //  read next line
                  ELSEIF nMode = D_STATUS
                     IF !EMPTY( cBuffer )
                        oNgi:WriteParBold( "Status" )
                     ENDIF
                     ProcStatusNg( oNgi, cBuffer )

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
   Return Nil
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function ProcNgiAlso()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION ProcNgiAlso( cSeealso )
   aAlso:={}
   aAlso := ListAsArray( cSeealso, "," )

RETURN aAlso

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function ProcNgiInput()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION ProcNgiInput()

   LOCAL aFiles   := {}
   LOCAL aFuncs   := {}
   LOCAL aFuncsam := {}
   LOCAL aFuncsn_ := {}
   LOCAL acfiles  := {}
   LOCAL aComms   := {}

   LOCAL cName,cT,cTs,cFile
   LOCAL x,nPos,nAlso,y

   LOCAL nXhandle := FCREATE( 'ngi\funcam.txt' )
   LOCAL nYhandle := FCREATE( 'ngi\funcn_.txt' )
   LOCAL xY       := "!Short:"
   LOCAL cSee     := "!seealso:"
   LOCAL lEof     := .f.
   LOCAL cBuffer
   SET CONSOLE ON
   afiles := DIRECTORY( "ngi\*.*" )
   ASORT( aFiles,,, { | x, y | UPPER( x[ 1 ] ) < UPPER( y[ 1 ] ) } )

   FOR x := 1 TO LEN( aFiles )
      Ft_FUse( "ngi\" + aFiles[ x, 1 ] )
      WHILE !lEof
         cBuffer := ReadLn( @lEof )
         cT      := LEFT( cBuffer, 7 )
         IF xY == cT
            cName := SUBSTR( cBuffer, 9 )
            cName := SUBSTR( cName, 1, 21 )
            nPos  := AT( "()", cName )
            IF nPoS > 0
               AADD( aFuncs, cName )
               IF LEFT( cname, 1 ) < "N"
                  AADD( afuncsam, aFiles[ x, 1 ] )
               ELSE
                  AADD( afuncsn_, aFiles[ x, 1 ] )
               ENDIF
            ELSE
               AADD( aComms, cName )
               AADD( aCfiles, aFiles[ x, 1 ] )

            ENDIF
         ENDIF
      END
      lEof := .f.
      Ft_FUse()
   NEXT

   @ INFILELINE, 21 SAY "Extracting: "

   FOR x := 1 TO LEN( afuncsam )
      cFile := afuncsam[ x ]

      @ INFILELINE, 33 SAY PAD( cfile, 47 )

      Ft_FUse( "ngi\" + cFile )
      aAlso := {}
      WHILE !lEof
         cBuffer := ReadLn( @lEof )
         cT      := LEFT( cBuffer, 7 )
         IF xY == cT
            cName := SUBSTR( cBuffer, 9 )
            cName := SUBSTR( cName, 1, AT( ' ', cName ) - 1 )
         ENDIF
         cTs := SUBSTR( cBuffer, 1, 9 )
         IF UPPER( cSee ) == UPPER( cTs )
            cTs := SUBSTR( cBuffer, 11 )

            aAlso   := procngialso2( cTs )
            cBuffer := ''
         ENDIF

         FWRITE( nXhandle, cBuffer + CRLF )

      ENDDO
      IF LEN( aAlso ) > 0
         cBuffer := "!SeeAlso: "
         FOR nAlso := 1 TO LEN( aAlso )
            cBuffer += aAlso[ nalso ] + " "
         NEXT
      ENDIF

      FWRITE( nXhandle, cBuffer + CRLF )

      lEof  := .f.
      aalso := {}
      FT_fuse()
   NEXT
   FCLOSE( nXhandle )
   FOR x := 1 TO LEN( AFUNCSN_ )
      cFile := afuncsn_[ x ]

      @ INFILELINE, 33 SAY PAD( cfile, 47 )

      Ft_FUse( "ngi\" + cFile )
      aAlso := {}
      WHILE !lEof
         cBuffer := ReadLn( @lEof )
         cT      := LEFT( cBuffer, 7 )
         IF xY == cT
            cName := SUBSTR( cBuffer, 9 )
            cName := SUBSTR( cName, 1, AT( ' ', cName ) - 1 )
         ENDIF
         cTs := SUBSTR( cBuffer, 1, 9 )
         IF UPPER( cSee ) == UPPER( cTs )
            cTs := SUBSTR( cBuffer, 11 )

            aAlso   := procngialso2( cTs )
            cBuffer := ''
         ENDIF

         FWRITE( nYhandle, cBuffer + CRLF )

      ENDDO

      IF LEN( aAlso ) > 0
         cBuffer := "!SeeAlso: "
         FOR nAlso := 1 TO LEN( aAlso )
            cBuffer += aAlso[ nalso ] + " "
         NEXT
      ENDIF

      FWRITE( nYhandle, cBuffer + CRLF )

      lEof  := .f.
      aAlso := {}

      lEof := .f.
      Ft_FUse()
   NEXT

   FCLOSE( nYhandle )
   lEof := .f.

   y := FCREATE( 'ngi\comm.txt' )
   ASORT( acfiles )
   FOR x := 1 TO LEN( acfiles )
      cFile := acfiles[ x ]
      IF upper(LEFT( cFile, AT( '.', cFile ) - 1 )) <> "LICENSE";
         .AND. upper(LEFT( cFile, AT( '.', cFile ) - 1 )) <> "OVERVIEW" ;
         .AND. upper(LEFT( cFile, AT( '.', cFile ) - 1 )) <> "COMPILEROPTIONS" ;
         .AND. upper(LEFT( cFile, AT( '.', cFile ) - 1 )) <> "GNULICENSE" ;
         .AND. upper(LEFT( cFile, AT( '.', cFile ) - 1 )) <> "GNULICENSEPART2" 

         @ INFILELINE, 33 SAY PAD( cfile, 47 )

         Ft_FUse( "ngi\" + acfiles[ x ] )
         aAlso := {}
         WHILE !lEof
            cBuffer := ReadLn( @lEof )
            cT      := LEFT( cBuffer, 7 )
            IF xY == cT
               cName := SUBSTR( cBuffer, 9 )
               cName := SUBSTR( cName, 1, AT( ' ', cName ) - 1 )
            ENDIF
            cTs := SUBSTR( cBuffer, 1, 9 )
            IF UPPER( cSee ) == UPPER( cTs )
               cTs := SUBSTR( cBuffer, 11 )

               aAlso   := procngialso2( cTs )
               cBuffer := ''
            ENDIF

            FWRITE( y, cBuffer + CRLF )

         ENDDO

         IF LEN( aAlso ) > 0
            cBuffer := "!SeeAlso: "
            FOR nAlso := 1 TO LEN( aAlso )
               cBuffer += aAlso[ nalso ]
            NEXT
            FWRITE( y, cBuffer + CRLF )

         ENDIF
      ENDIF
      lEof  := .f.
      aAlso := {}
      Ft_FUse()
   NEXT
   lEof := .f.
   FCLOSE( y )
RETURN NIL

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function procngialso2()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION procngialso2( cSeealso )

   LOCAL nPos
   LOCAL aAlso:={}
   LOCAL cTemp   := ''
   LOCAL xAlso   := {}
   LOCAL hPos    := 0
   aAlso := {}
   xAlso := ListAsArray2( cSeeAlso )
   FOR hPos := 1 TO LEN( xAlso )

      cTemp := SUBSTR( xAlso[ hPos ], 2, 1 )

      IF cTemp >= "A" .AND. cTemp < "_"
         nPos := AT( "()", xAlso[ hPos ] )
         IF nPos > 0
            AADD( aAlso, "funcam.ngo:" + ALLTRIM( xAlso[ hPos ] ) + ' ' )
         ELSEIF nPos = 0 .AND. UPPER(xAlso[ hPos ]) <> "LICENSE" .AND. UPPER(xAlso[ hPos ]) <> "OVERVIEW" .AND. !EMPTY( xAlso[ hPos ] )
            AADD( aAlso, "Comm.ngo:" + ALLTRIM( xAlso[ hPos ] ) + ' ' )
         ENDIF
      ENDIF
   NEXT
RETURN aAlso

FUNCTION ProcStatusng( nWriteHandle, cBuffer )
   IF LEN( ALLTRIM( cBuffer ) ) >1
      nWriteHandle:WritePar( cBuffer)
   ELSEIF SUBSTR( ALLTRIM( cBuffer ), 1 ) == "R"
      nWriteHandle:WritePar( "   Ready" )
   ELSEIF SUBSTR( ALLTRIM( cBuffer ), 1 ) == "S"
      nWriteHandle:WritePar( "   Started" )
   ELSEIF SUBSTR( ALLTRIM( cBuffer ), 1 ) == "C"
      nWriteHandle:WritePar( "   Clipper" )
   ELSE
      nWriteHandle:WritePar( "   Not Started" )
   ENDIF

RETURN nil


Function GenNgTable(oNgi)
LOCAL y,nLen2,x,nMax,nSpace,lCar:=.f.,nMax2,nSpace2,nPos1,nPos2,LColor,npos
LOCAL aLensFItem:={}
LOCAL aLensSItem:={}

  FOR X:=1 to LEN(afitable)
  if AT("^",afitable[x])>0
      aadd(aLenssItem,len(Substr(strtran(afitable[x],"^n",""),5)))  
  else
     AADD(aLensFItem,Len(afiTable[x]))
     endif
  NEXT
  FOR X:=1 to LEN(asiTable)
    if AT("^",asitable[x])>0
    aadd(aLenssItem,len(Substr(strtran(asitable[x],"^n",""),5)))
    else
     AADD(aLensSItem,Len(asiTable[x]))
     endif
  NEXT
  ASORT(aLensFItem,,,{|x,y| x > y})
  ASORT(aLensSItem,,,{|x,y| x > y})

nMax:=alenssitem[1]
nPos:=maxelem(asitable)
  nPos1:=ascan(aLenssItem,{|x| x==nPos})
        oNgi:WritePar("")
//  nMax2:=checkcar(aTable,1)+1
 nMax2:=alensfitem[1]
 nPos:=maxelem(afitable)
  nPos2:=ascan(alensfitem,{|x| x==nPos})  


        oNgi:WritePar("   É"+REPL("Í",aLensFitem[nPos2]+2)+"Ë"+REPL("Í",alensSitem[nPos1]+2)+"»",.F.) //-4

FOR x:=1 to len(asitable)
      nSpace:=nMax-Len(asitable[x])
      nSpace2:=nMax2-Len(afitable[x])
      oNgi:WriteParBox( "   º "+  afiTable[x]+space(nSpace2)+ " º "+ IF(asiTable[x]=="|",Strtran(asiTable[x],"|"," "),asiTable[x]) +space(nspace)+" º"+HB_OsNEWLINE())
Next


  oNgi:WritePar("   È"+REPL("Í",aLensFitem[nPos2]+2)+"Ê"+REPL("Í",aLenssitem[npos1]+2)+"¼",.F.) // -4

oNgi:WritePar("")
afiTable:={}
asitable:={}
Return Nil

Function ProcNgTable(cBuffer)

Local nPos,cItem,cItem2,cItem3,xtype,nColorpos,cColor
      if AT("<color:",cBuffer)>0
         nColorPos:=AT(":",cBuffer)
         cColor:=SubStr(cBuffer,nColorpos+1)
         nPos:=at(">",ccolor)
            cColor:=substr(ccolor,1,nPos-1)

         cBuffer:=strtran(cbuffer,"</color>","")
         cBuffer:=STRTRAn(cbuffer,"<color:","")
         cBuffer:=STRTRAn(cbuffer,">","")
         cBuffer:=Strtran(cBuffer,ccolor,'')
         nColorpos:=ASCAn(aColorTable,{|x,y| upper(x[1])==upper(ccolor)})
         cColor:=aColortable[nColorPos,2]
      Endif
      cItem:=SubStr(cBuffer,1,22)
      xtype=valtype(citem)
      cBuffer:=StrTran(cBuffer,cItem,Space(len(cItem)))
      nPos:=STRPos(cBuffer)
      IF nPos=23
          cItem2:=SubStr(cBuffer,nPos)
      Endif
        AADD(afiTable,rtrim(ltrim(cItem)))
        AADD(asiTable,cItem2)

Return Nil
FUNCTION  ProcNGDesc(cBuffer,oNgi,cStyle)
local cLine:=''
Local npos,CurPos:=0
LOCAL nColorPos,ccolor:='',creturn:='',ncolorend,nIdentLevel
LOCAL lEndPar:= .F.

LOCAL lEndFixed:=.F.
LOCAL lEndTable:=.F.
Default cStyle to "Default"



if cStyle<>"Example" .and. at("<table>",cBuffer)==0 .and. AT("<fixed>",cBuffer)=0
   if AT("<par>",cBuffer)>=0 .or. AT("</par>",cBuffer)=0   .and. !empty(cbuffer) 
      If AT("<par>",cBuffer)>0 .and. AT("</par>",cBuffer)>0
      
         if cStyle=="Arguments"
            cBuffer:= strtran(cBuffer,"<par>","<par><b>")
//            ? cBuffer
         if at(") ",cBuffer)>0
            cBuffer:= strtran(cBuffer,") ",")</b>")
         elseif at("> ",cBuffer)>0
            cBuffer:= strtran(cBuffer,"> ","></b>")
         endif
         endif
 
      else
      cBuffer:=FormatngBuff(cBuffer,cStyle,ongi)
      endif
endif
endif

if empty(cBuffer)
oNgi:WritePar("")
endif

If AT('<par>',cBuffer)>0 .and. AT('</par>',cBuffer)>0
      cBuffer:=Strtran(cBuffer,'<par>','')
      cBuffer:=StrTran(cBuffer,'<b>',' ^b')
      cBuffer:=StrTran(cBuffer,'</b>', '^n ')
      cBuffer:=Strtran(cBuffer,'</color>', '^n ')
      cBuffer:=StrTran(cBuffer,'<em>','')
      cBuffer:=StrTran(cBuffer,'</em>','')
      cBuffer:=StrTran(cBuffer,'<i>','')
      cBuffer:=StrTran(cBuffer,'</i>','')

      nColorPos:=at('<color:',cBuffer)
      if ncolorpos>0
      checkngcolor(@cbuffer,ncolorpos)
      endif
//      Alltrim(cBuffer)
      If cStyle=="Description" .or. cStyle=="Compliance"
      nIdentLevel:=6
      nPos:=0
      do while !lendPar
         if nPos==0
           cLine:=SubStr(cBuffer,1,76)
           nPos:=RAT(" ",cLine)
              if nPos>0

              cLine:=SubStr(cBuffer,1,nPos)
              endif

         else
           cLine:=space(nidentLevel)+SubStr(cBuffer,curPos,69)

              if AT('</par>',cline)>0
                  lEndPar:=.T.
                  cline:=strtran(cline," </par>","")
              endif
              nPos:=RAT(" ",cLine)
              if nPos>0
              cLine:=space(nidentLevel)+ SubStr(cBuffer,curpos,nPos-nIdentlevel)
              npos-=nIdentlevel
              else
                if cLine=="</par>"
                    cLine:=''
               endif

              endif
         endif
         if  !empty(cline)
         oNgi:WritePar(space(nidentLevel)+Alltrim(cline))
         endif
          curPos:=Curpos+nPos
      Enddo

      ELSEIf cStyle=="Arguments"
      nIdentLevel:=18
      nPos:=0

      do while !lendPar
         if nPos==0
           cLine:=SubStr(cBuffer,1,76)
           nPos:=RAT(" ",cLine)
              if nPos>0

              cLine:=SubStr(cBuffer,1,nPos)
              endif
         if  !empty(cline)
         oNgi:WritePar(cline)
         endif

         else
           cLine:=space(nidentLevel)+SubStr(cBuffer,curPos,58) //60

              if AT('</par>',cline)>0
                  lEndPar:=.T.
                  cline:=strtran(cline," </par>","")
              endif
              nPos:=RAT(" ",cLine)
              if nPos>0
              cLine:=space(nidentLevel)+ SubStr(cBuffer,curpos,nPos-nIdentlevel)
              npos-=nIdentlevel
              else
                if cLine=="</par>"
                    cLine:=''
               endif

              endif
         if  !empty(cline)
         oNgi:WritePar(space(nidentLevel)+Alltrim(cline))
         endif

         endif
          curPos:=Curpos+nPos
      Enddo


      ELSEIf cStyle=="Syntax"
      nIdentLevel:=6
      nPos:=0
      do while !lendPar
         if nPos==0
           cLine:=SubStr(cBuffer,1,76)
           nPos:=RAT(" ",cLine)
              if nPos>0

              cLine:=SubStr(cBuffer,1,nPos)
              endif

         else
           cLine:=space(nidentLevel)+SubStr(cBuffer,curPos,69)

              if AT('</par>',cline)>0
                  lEndPar:=.T.
                  cline:=strtran(cline," </par>","")
              endif
              nPos:=RAT(" ",cLine)
              if nPos>0
              cLine:=space(nidentLevel)+ SubStr(cBuffer,curpos,nPos-nIdentlevel)
              npos-=nIdentlevel
              else
                if cLine=="</par>"
                    cLine:=''
               endif

              endif
         endif
         if  !empty(cline)
         oNgi:WritePar(space(nidentLevel)+Alltrim(cline))
         endif
          curPos:=Curpos+nPos
      Enddo


Elseif cStyle=="Default"
      nIdentLevel:=6
      nPos:=0
      do while !lendPar
         if nPos==0
           cLine:=SubStr(cBuffer,1,76)
           nPos:=RAT(" ",cLine)
              if nPos>0
              cLine:=SubStr(cBuffer,1,nPos)
              endif

         else
           cLine:=space(nidentLevel)+SubStr(cBuffer,curPos,69)
           nPos:=RAT(" ",cLine)
              if AT('</par>',cline)>0
                  lEndPar:=.T.
                  cline:=strtran(cline,"</par>","")
              endif
              if nPos>0
              cLine:=space(nidentLevel)+SubStr(cBuffer,curpos,nPos-nIdentlevel)
              npos-=nIdentlevel
              else
                if cLine=="</par>"
                    cLine:=''
               endif

              endif
         endif
         if  !empty(cline)
         oNgi:WritePar(rtrim(cline))
         endif
          curPos:=Curpos+nPos
      Enddo
endif
endif
If AT('<fixed>',cBuffer)>0
    do while !lendFixed
                cLine :=  TRIM(SUBSTR( ReadLN( @lEof ), nCommentLen ) )
        if at("</fixed>",cLine)>0
          lendfixed:=.t.
        else

        oNgi:WritePar(cline)
    endif
    enddo
end
if AT('<table>',cBuffer)>0
    do while !lendTable
        cLine :=  TRIM(SUBSTR( ReadLN( @lEof ), nCommentLen ) )
        if  at("</table>",cLine)>0
          lendTable:=.t.
        else
          procngtable(cline)
    endif
    enddo
if lEndTable
    GenNgTable(oNgi)
endif
endif
//      If cStyle=="Description" .or. cStyle=="Compliance"
 //        oNgi:Writepar('')
 //     endif
return nil


func checkngcolor(cbuffer,ncolorpos)
local ncolorend,nreturn,cOldColorString,cReturn,ccolor

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
  creturn:="^a"+acolortable[nreturn,2]
endif
cBuffer:=strtran(cBuffer,cOldColorString,cReturn)
enddo
return cbuffer

func maxelem(a)
local nsize:=len(a)
local max:=0
Local tam:=0
local max2:=0
LOCAL nPos:=1
LOCAL cString
local ncount
for ncount:=1 to nsize
    tam:=len(a[ncount])
    max:=if(tam>max,tam,max)
next
nPos:=ascan(a,{|x| Len(x)==max})
return max

function FormatNgBuff(cBuffer,cStyle,ongi)

Local creturn:=''
local cline:='',coline:=''
local cBuffend:=''
local lendbuff:=.f.
local npos,nposend
      creturn :=cBuffer+' '
      if at('</par>',creturn)>0 .or. empty(cBuffer)
         if empty(cbuffer)
         creturn:=''
         endif
         return creturn
      endif
   if cStyle != "Syntax" .AND. cStyle !="Arguments"
       do while !lendBuff
                cLine :=  TRIM(SUBSTR( ReadLN( @lEof ), nCommentLen ) )
      if at('</par>',cLine)>0 .or. empty(cline)
         lendBuff:=.t.
      endif
      cReturn+=alltrim(cLine)+ ' '
    enddo                      
    cReturn:='<par>'+creturn+' </par>'
  elseif cStyle=='Syntax'
      nPos:=At("-->",cBuffer)
      if nPos>0
         cBuffend:=Substr(cReturn,nPos+3)
         cReturn:=SubStr(cReturn,1,nPos+3)
         cReturn:=cReturn+'<color:aqua>'+cBuffend+' </color>'
         creturn:='<par>'+creturn+' </par>'
    ELSE
         cReturn:='<par>'+cReturn+' </par>'
      ENDIF
  ELSEIF cStyle=='Arguments'
  nPos:=0
    if at("<par>",cReturn)>0
            cReturn:=STRTRAN(cReturn,"<par>","")
            cReturn:=STRTRAN(cReturn,"</par>","")
            cReturn:=alltrim(cReturn)
            nPos:=AT(" ",cReturn)
            cOLine:=left(cReturn,nPos-1)
            cReturn:=STRTRAN(cReturn,coLine,"")
            cReturn:=STRTRAN(cReturn,">","></b>  ")         
            cReturn:=STRTRAN(cReturn," <","<b> <")

    endif
       DO WHILE !lEndBuff
                cLine :=  TRIM(SUBSTR( ReadLN( @lEof ), nCommentLen ) )
      IF AT('</par>',cLine)>0 .OR. EMPTY(cLine)
         lEndBuff:=.t.
      ENDIF
      cReturn+=alltrim(cLine)+ ' '
    enddo
      cReturn:='       <par><b>'+cOLine+'</b> '+cReturn+'</par>'
   ENDIF

//   endif
return creturn


  
