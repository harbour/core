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
#define LONGLINE     600
#define LONGONELINE  66
MEMVAR aDirlist
MEMVAR aDocInfo
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

STATIC aColorTable := { { 'aqua', '1B' }, { 'black', '10' }, { 'fuchia', '1D' }, { 'grey', '18' }, { 'green', '12' }, { 'lime', '1A' }, { 'maroon', '16' }, { 'navy', '19' }, { 'olive', '12' }, { 'purple', '15' }, { 'red', '1C' }, { 'silver', '17' }, { 'teal', '13 ' }, { 'white', '1F' }, { 'yellow', '1E' } }

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function ProcessiNg()
*+
*+    Called from ( hbdoc.prg    )   2 - function main()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
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
   LOCAL nFiles := LEN( aDirList )
   LOCAL lDoc

   LOCAL cBuffer
   LOCAL nEnd
   LOCAL nCount
   LOCAL nAlso

   LOCAL lData         := .F.
   LOCAL lMethod       := .F.
   LOCAL cBuffEnd
   LOCAL nPos
   LOCAL nPosEND
   LOCAL lIsDataLink   := .F.
   LOCAL lIsMethodLink := .F.

   LOCAL cBar           := "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
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
   LOCAL lBlankLine     := .F.             // Blank line enCountered and sent out
   LOCAL lAddBlank      := .F.             // Need to add a blank line if next line is not blank
   LOCAL oNgi
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

   lData         := .F.
   lMethod       := .F.
   lIsDataLink   := .F.
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
      afiTable := {}
      asiTable := {}
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
         cBuffer := STRTRAN( cBuffer, CHR( 10 ), "" )
         nLineCnt ++
         IF nLineCnt % 10 = 0
            @ LINELINE, 33 SAY STR( nLineCnt, 5, 0 )         
         ENDIF
         //  check to see if we are in doc mode or getting out of doc mode

         IF AT( cDoc, cBuffer ) > 0 .OR. AT( cClassDoc, cBuffer ) > 0
            IF lDoc
               WRITE_ERROR( cDoc + " enCountered during extraction of Doc" ;
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
               WRITE_ERROR( cEnd + " enCountered outside of Doc area at line" ;
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
                  WRITE_ERROR( "Error creating",,,, cFileName + ".ngi" )
               ENDIF
               //  2) Category
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
               //          endif
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
                  oNgi:WriteJumpTitle( LEFT( cFilename, AT( '.', cFilename ) - 1 ) + cFuncName, "Data " + cFuncName )
               ELSEIF lMethod
                  oNgi:WriteJumpTitle( LEFT( cFilename, AT( '.', cFilename ) - 1 ) + cFuncName, "Method " + cFuncName )
               ELSE
                  IF LEN( cFuncname ) < 22
                     oNgi:WriteTitle( PAD( cFuncName, 21 ) + cOneLine, cFuncName )
                  ELSE
                     oNgi:WriteTitle( PAD( cFuncName, 24 ) + cOneLine, cFuncName )
                  ENDIF
                  oNgi:WritePar( cOneLine )
                  oNgi:WritePar( cBar )
                  //  4) all other stuff
               ENDIF
            ELSE

               IF AT( cSyn, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )

                     oNgi:WriteParBold( " Syntax" )

                     nMode     := D_SYNTAX
                     lAddBlank := .T.
                  ENDIF
               ELSEIF AT( cConstruct, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )

                     oNgi:WriteParBold( " Constructor syntax" )

                     nMode     := D_SYNTAX
                     lAddBlank := .T.
                  ENDIF
               ELSEIF AT( cArg, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )

                     IF !lBlankLine

                        oNgi:WriteParBold( " Arguments" )

                     ENDIF

                     nMode     := D_ARG
                     lAddBlank := .T.
                  ENDIF
               ELSEIF AT( cRet, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )

                     //                  IF !lBlankLine
                     //                     oNgi:WritePar( "" )
                     //                  ENDIF

                     oNgi:WriteParBold( " Returns" )

                     nMode     := D_ARG
                     lAddBlank := .T.
                  ENDIF
               ELSEIF AT( cDesc, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )

                     oNgi:WriteParBold( " Description" )

                     nMode     := D_DESCRIPTION
                     lAddBlank := .T.
                     lPar      := .T.
                  ENDIF
               ELSEIF AT( cdatalink, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     IF !lBlankLine
                        oNgi:WritePar( "" )                 //:endpar()
                     ENDIF

                     oNgi:WriteParBold( " Data" )
                     nMode     := D_DATALINK
                     lAddBlank := .T.

                     lIsDataLink := .T.
                  ENDIF
               ELSEIF AT( cDatanolink, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     IF !lIsDataLink
                        oNgi:WriteParBold( " Data" )

                     ENDIF
                     nMode     := D_NORMAL
                     lAddBlank := .T.

                     lPar := .T.
                  ENDIF
               ELSEIF AT( cMethodslink, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )

                     oNgi:WriteParBold( " Method" )
                     nMode     := D_METHODLINK
                     lAddBlank := .T.

                     lIsMethodLink := .T.
                  ENDIF
               ELSEIF AT( cMethodsnolink, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     IF !lIsMethodLink
                        oNgi:WriteParBold( " Methods" )
                     ENDIF

                     nMode     := D_NORMAL
                     lAddBlank := .T.
                     lPar      := .T.
                  ENDIF

               ELSEIF AT( cExam, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )

                     IF !lBlankLine
                        //                     oNgi:WritePar( "" )
                        oNgi:WriteParBold( " Examples" )
                     ENDIF

                     nMode     := D_EXAMPLE
                     lAddBlank := .T.
                  ENDIF

               ELSEIF AT( cTest, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )

                     IF !lBlankLine

                        oNgi:WriteParBold( " Tests" )
                     ENDIF

                     nMode     := D_EXAMPLE
                     lAddBlank := .T.
                     lPar      := .t.
                  ENDIF
               ELSEIF AT( cStatus, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )

                     nMode := D_STATUS
                  ENDIF

               ELSEIF AT( cCompl, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )

                     IF !lBlankLine
                        oNgi:WriteParBold( " Compliance" )
                     ENDIF

                     nMode     := D_COMPLIANCE
                     lAddBlank := .T.
                     lPar      := .t.
                  ENDIF
               ELSEIF AT( cPlat, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )

                     IF !lBlankLine
                        oNgi:WriteParBold( " Platforms" )
                     ENDIF

                     nMode     := D_NORMAL
                     lAddBlank := .T.
                     lPar      := .t.
                  ENDIF
               ELSEIF AT( cFiles, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )

                     IF !lBlankLine
                        oNgi:WriteParBold( " Files" )
                     ENDIF

                     nMode     := D_NORMAL
                     lAddBlank := .T.
                     lPar      := .t.
                  ENDIF
               ELSEIF AT( cFunction, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )

                     //                  IF !lBlankLine
                     //                     oNgi:WritePar( "" )
                     //                  ENDIF
                     oNgi:WriteParBold( " Functions" )

                     lPar      := .t.
                     nMode     := D_NORMAL
                     lAddBlank := .T.
                  ENDIF
               ELSEIF AT( cSee, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     nMode := D_SEEALSO
                  ENDIF
               ELSEIF AT( cInc, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     nMode := D_INCLUDE
                  ENDIF
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
                        cBuffer := STRTRAN( cBuffer, SPACE( 6 ), "" )
                        cbuFfer := '<par><b>' + cBuffer + '</b></par>'
                     ENDIF
                     procngdesc( cbuffer, oNgi, "Syntax" )
                  ELSEIF nMode = D_ARG
                     IF LEN( cBuffer ) > LONGLINE
                        WRITE_ERROR( "Arguments", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )

                     procngdesc( cbuffer, oNgi, "Arguments" )
                  ELSEIF nMode = D_EXAMPLE
                     IF LEN( cBuffer ) > LONGLINE
                        WRITE_ERROR( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     ProcNgDesc( cBuffer, oNgi, "Example" )
                  ELSEIF nMode = D_DESCRIPTION
                     IF LEN( cBuffer ) > LONGLINE
                        WRITE_ERROR( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     ProcNgDesc( cBuffer, oNgi, "Description" )

                  ELSEIF nMode = D_NORMAL
                     IF LEN( cBuffer ) > LONGLINE
                        WRITE_ERROR( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     ProcNgDesc( cBuffer, oNgi )
                  ELSEIF nMode = D_COMPLIANCE
                     IF LEN( cBuffer ) > LONGLINE
                        WRITE_ERROR( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     ProcNgDesc( cBuffer, oNgi, "Compliance" )

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
RETURN Nil

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function ProcNgiAlso()
*+
*+    Called from ( genhtm2.prg  )   1 - function processwww()
*+                ( genng.prg    )   1 - function processing()
*+                ( genng1.prg   )   1 - function processing()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION ProcNgiAlso( cSeealso )

   aAlso := {}
   aAlso := ListAsArray( cSeealso, "," )

RETURN aAlso

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function ProcNgiInput()
*+
*+    Called from ( hbdoc.prg    )   1 - function main()
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

   LOCAL cName
   LOCAL cT
   LOCAL cTs
   LOCAL cFile
   LOCAL x
   LOCAL nPos
   LOCAL nAlso
   LOCAL y

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
      FT_FUSE( "ngi\" + aFiles[ x, 1 ] )
      WHILE !lEof
         cBuffer := ReadLn( @lEof )
         cBuffer := STRTRAN( cBuffer, CHR( 10 ), "" )
         cT      := LEFT( cBuffer, 7 )

         IF xY == cT
            cName := SUBSTR( cBuffer, 9 )
            cName := SUBSTR( cName, 1, 21 )
            nPos  := AT( "()", cName )
            IF nPos > 0
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
      FT_FUSE()
   NEXT

   @ INFILELINE, 21 SAY "Extracting: "         

   FOR x := 1 TO LEN( afuncsam )
      cFile := afuncsam[ x ]

      @ INFILELINE, 33 SAY PAD( cfile, 47 )         

      FT_FUSE( "ngi\" + cFile )
      aAlso := {}
      WHILE !lEof
         cBuffer := ReadLn( @lEof )
         cBuffer := STRTRAN( cbuffer, CHR( 10 ), '' )
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

         FWRITE( nXhandle, cBuffer + pCRLF )

      ENDDO
      IF LEN( aAlso ) > 0
         cBuffer := "!SeeAlso: "
         FOR nAlso := 1 TO LEN( aAlso )
            cBuffer += aAlso[ nalso ] + " "
         NEXT
      ENDIF
      //    cBuffer:=strtran(cBuffer,chr(10),"")
      FWRITE( nXhandle, cBuffer + pCRLF )

      lEof  := .f.
      aalso := {}
      FT_FUSE()
   NEXT
   FCLOSE( nXhandle )
   FOR x := 1 TO LEN( AFUNCSN_ )
      cFile := afuncsn_[ x ]

      @ INFILELINE, 33 SAY PAD( cfile, 47 )         

      FT_FUSE( "ngi\" + cFile )
      aAlso := {}
      WHILE !lEof
         cBuffer := ReadLn( @lEof )
         cBuffer := STRTRAN( cbuffer, CHR( 10 ), '' )
         cT      := LEFT( cBuffer, 7 )
         IF xY == cT
            cName := SUBSTR( cBuffer, 9 )
            cName := SUBSTR( cName, 1, AT( ' ', cName ) - 1 )
         ENDIF
         //         if at(chr(10),cBuffer)>0
         //            cBuffer:=Strtran(cbuffer,chr(10),'')
         //        endif

         cTs := SUBSTR( cBuffer, 1, 9 )
         IF UPPER( cSee ) == UPPER( cTs )
            cTs := SUBSTR( cBuffer, 11 )

            aAlso   := procngialso2( cTs )
            cBuffer := ''
         ENDIF
         //    cBuffer:=strtran(cBuffer,chr(10),"")
         FWRITE( nYhandle, cBuffer + pCRLF )

      ENDDO

      IF LEN( aAlso ) > 0
         cBuffer := "!SeeAlso: "
         FOR nAlso := 1 TO LEN( aAlso )
            cBuffer += aAlso[ nalso ] + " "
         NEXT
      ENDIF
      //    cBuffer:=strtran(cBuffer,chr(10),"")
      FWRITE( nYhandle, cBuffer + pCRLF )

      lEof  := .f.
      aAlso := {}

      lEof := .f.
      FT_FUSE()
   NEXT

   FCLOSE( nYhandle )
   lEof := .f.

   y := FCREATE( 'ngi\comm.txt' )
   ASORT( acfiles )
   FOR x := 1 TO LEN( acfiles )
      cFile := acfiles[ x ]
      IF UPPER( LEFT( cFile, AT( '.', cFile ) - 1 ) ) <> "LICENSE" ;
                .AND. UPPER( LEFT( cFile, AT( '.', cFile ) - 1 ) ) <> "OVERVIEW" ;
                .AND. UPPER( LEFT( cFile, AT( '.', cFile ) - 1 ) ) <> "COMPILEROPTIONS" ;
                .AND. UPPER( LEFT( cFile, AT( '.', cFile ) - 1 ) ) <> "GNULICENSE" ;
                .AND. UPPER( LEFT( cFile, AT( '.', cFile ) - 1 ) ) <> "GNULICENSEPART2"

         @ INFILELINE, 33 SAY PAD( cfile, 47 )         

         FT_FUSE( "ngi\" + acfiles[ x ] )
         aAlso := {}
         WHILE !lEof
            cBuffer := ReadLn( @lEof )
            cT      := LEFT( cBuffer, 7 )
            cBuffer := STRTRAN( cbuffer, CHR( 10 ), '' )
            IF xY == cT
               cName := SUBSTR( cBuffer, 9 )
               cName := SUBSTR( cName, 1, AT( ' ', cName ) - 1 )
            ENDIF
            cTs := SUBSTR( cBuffer, 1, 9 )
            //             if at(chr(10),cBuffer)>0
            //                cBuffer:=Strtran(cbuffer,chr(10),'')
            //            endif

            IF UPPER( cSee ) == UPPER( cTs )
               cTs := SUBSTR( cBuffer, 11 )

               aAlso   := procngialso2( cTs )
               cBuffer := ''
            ENDIF
            //            cBuffer:=strtran(cBuffer,chr(10),"")
            FWRITE( y, cBuffer + pCRLF )

         ENDDO

         IF LEN( aAlso ) > 0
            cBuffer := "!SeeAlso: "
            FOR nAlso := 1 TO LEN( aAlso )
               cBuffer += aAlso[ nalso ]
            NEXT
            FWRITE( y, cBuffer + pCRLF )

         ENDIF
      ENDIF
      lEof  := .f.
      aAlso := {}
      FT_FUSE()
   NEXT
   lEof := .f.
   FCLOSE( y )
RETURN NIL

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function procngialso2()
*+
*+    Called from ( genng.prg    )   3 - function procngiinput()
*+                ( genng1.prg   )   3 - function procngiinput()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION procngialso2( cSeealso )

   LOCAL nPos
   LOCAL aAlso := {}
   LOCAL cTemp := ''
   LOCAL xAlso := {}
   LOCAL hPos  := 0
   aAlso := {}
   xAlso := ListAsArray2( cSeeAlso )
   FOR hPos := 1 TO LEN( xAlso )

      cTemp := SUBSTR( xAlso[ hPos ], 2, 1 )

      IF cTemp >= "A" .AND. cTemp < "_"
         nPos := AT( "()", xAlso[ hPos ] )
         IF nPos > 0
            AADD( aAlso, "funcam.ngo:" + ALLTRIM( xAlso[ hPos ] ) + ' ' )
         ELSEIF nPos = 0 .AND. UPPER( xAlso[ hPos ] ) <> "LICENSE" .AND. UPPER( xAlso[ hPos ] ) <> "OVERVIEW" .AND. !EMPTY( xAlso[ hPos ] )
            AADD( aAlso, "Comm.ngo:" + ALLTRIM( xAlso[ hPos ] ) + ' ' )
         ENDIF
      ENDIF
   NEXT
RETURN aAlso

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function ProcStatusng()
*+
*+    Called from ( genhtm2.prg  )   1 - function processwww()
*+                ( genng.prg    )   1 - function processing()
*+                ( genng1.prg   )   1 - function processing()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION ProcStatusng( nWriteHandle, cBuffer )

   IF LEN( ALLTRIM( cBuffer ) ) > 1
      nWriteHandle:WritePar( cBuffer )
   ELSEIF SUBSTR( ALLTRIM( cBuffer ), 1 ) == "R"
      nWriteHandle:WritePar( "      Ready" )
   ELSEIF SUBSTR( ALLTRIM( cBuffer ), 1 ) == "S"
      nWriteHandle:WritePar( "      Started" )
   ELSEIF SUBSTR( ALLTRIM( cBuffer ), 1 ) == "C"
      nWriteHandle:WritePar( "      Clipper" )
   ELSE
      nWriteHandle:WritePar( "      Not Started" )
   ENDIF

RETURN nil

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function GenNgTable()
*+
*+    Called from ( genng.prg    )   1 - function procngdesc()
*+                ( genng1.prg   )   1 - function procngdesc()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION GenNgTable( oNgi )

   LOCAL y
   LOCAL nLen2
   LOCAL x
   LOCAL nMax
   LOCAL nSpace
   LOCAL lCar        := .f.
   LOCAL nMax2
   LOCAL nSpace2
   LOCAL nPos1
   LOCAL nPos2
   LOCAL LColor
   LOCAL nPos
   LOCAL aLensFItem  := {}
   LOCAL aLensSItem  := {}
   LOCAL cMaxItem    := ''
   LOCAL nmax3
   LOCAL nmax4
   LOCAL npos3
   LOCAL npos4
   LOCAL nSpace3
   LOCAL nSpace4
   LOCAL aLensTItem  := {}
   LOCAL aLensfoItem := {}
   LOCAL nLen
   FOR X := 1 TO LEN( afitable )
      IF !EMPTY( afiTable[ x ] )
         AADD( aLensFItem, LEN( afiTable[ x ] ) )
      END
   NEXT
   FOR X := 1 TO LEN( asiTable )
      IF !EMPTY( asiTable[ x ] )
         AADD( aLensSItem, LEN( asiTable[ x ] ) )
      END
   NEXT
   IF LEN( afoitable ) > 0

      FOR X := 1 TO LEN( afoitable )
         IF !EMPTY( afoiTable[ x ] )
            AADD( aLensfoItem, LEN( afoiTable[ x ] ) )
         END
      NEXT
   ENDIF
   IF LEN( atitable ) > 0
      FOR X := 1 TO LEN( atitable )
         IF !EMPTY( atiTable[ x ] )
            AADD( aLenstItem, LEN( atiTable[ x ] ) )
         END
      NEXT
   ENDIF

   ASORT( aLensFItem,,, { | x, y | x > y } )
   ASORT( aLensSItem,,, { | x, y | x > y } )
   IF LEN( afoitable ) > 0 .AND. nNumTableItems == 4
      ASORT( alenstitem,,, { | x, y | x > y } )
      nmax3 := alenstitem[ 1 ]
      npos  := maxelem( atitable )
      nPos3 := ASCAN( alenstitem, { | x | x == nPos } )

      ASORT( aLensFoItem,,, { | x, y | x > y } )
      nmax4 := alensfoitem[ 1 ]
      nPos  := maxelem( afoitable )
      nPos4 := ASCAN( alensfoitem, { | x | x == nPos } )
   ENDIF
   IF LEN( atitable ) > 0 .AND. nNumTableItems == 3
      ASORT( alenstitem,,, { | x, y | x > y } )
      nmax3 := alenstitem[ 1 ]
      npos  := maxelem( atitable )
      nPos3 := ASCAN( alenstitem, { | x | x == nPos } )

   ENDIF

   nMax  := aLenssItem[ 1 ]
   nPos  := maxelem( asitable )
   nPos1 := ASCAN( aLenssItem, { | x | x == nPos } )

/*   oNgi:WritePar( "" )*/
   //  nMax2:=checkcar(aTable,1)+1
   nMax2 := alensfitem[ 1 ]
   nPos  := maxelem( afitable )
   nPos2 := ASCAN( alensfitem, { | x | x == nPos } )
   IF nNumTableItems == 2
      cMaxItem := '      ' + "É" + REPL( "Í", aLensFitem[ nPos2 ] + 2 ) + "Ë" + REPL( "Í", alensSitem[ nPos1 ] + 2 ) + "»"
      IF LEN( cMaxItem ) < 76
         oNgi:WritePar( "      É" + REPL( "Í", aLensFitem[ nPos2 ] + 2 ) + "Ë" + REPL( "Í", alensSitem[ nPos1 ] + 2 ) + "»", .F. )          //-4
      ELSE
         oNgi:WritePar( "É" + REPL( "Í", aLensFitem[ nPos2 ] + 2 ) + "Ë" + REPL( "Í", alensSitem[ nPos1 ] + 2 ) + "»", .F. )                //-4
      ENDIF
   ELSEIF nNumTableItems == 3
      cMaxItem := '      ' + "É" + REPL( "Í", aLensFitem[ nPos2 ] + 2 ) + "Ë" + REPL( "Í", alensSitem[ nPos1 ] + 2 ) + "Ë" + REPL( "Í", alensTitem[ nPos3 ] + 2 ) + "»"
      IF LEN( cMaxItem ) < 76
         oNgi:WritePar( "      É" + REPL( "Í", aLensFitem[ nPos2 ] + 2 ) + "Ë" + REPL( "Í", alensSitem[ nPos1 ] + 2 ) + "Ë" + REPL( "Í", alensTitem[ nPos3 ] + 2 ) + "»", .F. )     //-4
      ELSE
         oNgi:WritePar( "É" + REPL( "Í", aLensFitem[ nPos2 ] + 2 ) + "Ë" + REPL( "Í", alensSitem[ nPos1 ] + 2 ) + "Ë" + REPL( "Í", alensTitem[ nPos3 ] + 2 ) + "»", .F. )           //-4
      ENDIF
   ELSEIF nNumTableItems == 4
      cMaxItem := '      ' + "É" + REPL( "Í", aLensFitem[ nPos2 ] + 2 ) + "Ë" + REPL( "Í", alensSitem[ nPos1 ] + 2 ) + "Ë" + REPL( "Í", alensTitem[ nPos3 ] + 2 ) + "Ë" + REPL( "Í", alensfoitem[ nPos4 ] + 2 ) + "»"
      IF LEN( cMaxItem ) < 76
         oNgi:WritePar( "      É" + REPL( "Í", aLensFitem[ nPos2 ] + 2 ) + "Ë" + REPL( "Í", alensSitem[ nPos1 ] + 2 ) + "Ë" + REPL( "Í", alensTitem[ nPos3 ] + 2 ) + "Ë" + REPL( "Í", alensfoitem[ nPos4 ] + 2 ) + "»", .F. )                   //-4
      ELSE
         oNgi:WritePar( "É" + REPL( "Í", aLensFitem[ nPos2 ] + 2 ) + "Ë" + REPL( "Í", alensSitem[ nPos1 ] + 2 ) + "Ë" + REPL( "Í", alensTitem[ nPos3 ] + 2 ) + "Ë" + REPL( "Í", alensfoitem[ nPos4 ] + 2 ) + "»", .F. )     //-4
      ENDIF
   ENDIF
   FOR x := 1 TO LEN( asitable )
      IF !EMPTY( asitable[ x ] )
         nSpace  := nMax - LEN( asitable[ x ] )
         nSpace2 := nMax2 - LEN( afitable[ x ] )

         IF nNumTableItems == 2
            IF LEN( cMaxItem ) < 76
               oNgi:WriteParBox( "      º " + afiTable[ x ] + SPACE( nSpace2 ) + " º " + IF( asiTable[ x ] == "|", STRTRAN( asiTable[ x ], "|", " " ), asiTable[ x ] ) + SPACE( nspace ) + " º" + HB_OSNEWLINE() )
            ELSE
               oNgi:WriteParBox( "º " + afiTable[ x ] + SPACE( nSpace2 ) + " º " + IF( asiTable[ x ] == "|", STRTRAN( asiTable[ x ], "|", " " ), asiTable[ x ] ) + SPACE( nspace ) + " º" + HB_OSNEWLINE() )
            ENDIF
         ELSEIF nNumTableItems == 3
            nSpace3 := nMax3 - LEN( atitable[ x ] )
            IF LEN( cMaxItem ) < 76
               oNgi:WriteParBox( "      º " + afiTable[ x ] + SPACE( nSpace2 ) + " º " + IF( asiTable[ x ] == "|", STRTRAN( asiTable[ x ], "|", " " ), asiTable[ x ] ) + SPACE( nspace ) + " º " + atiTable[ x ] + SPACE( nspace3 ) + " º" + HB_OSNEWLINE() )
            ELSE
               oNgi:WriteParBox( "º " + afiTable[ x ] + SPACE( nSpace2 ) + " º " + IF( asiTable[ x ] == "|", STRTRAN( asiTable[ x ], "|", " " ), asiTable[ x ] ) + SPACE( nspace ) + " º " + atiTable[ x ] + SPACE( nspace3 ) + " º" + HB_OSNEWLINE() )
            ENDIF
         ELSEIF nNumTableItems == 4
            nSpace3 := nMax3 - LEN( atitable[ x ] )
            nSpace4 := nMax4 - LEN( afoitable[ x ] )
            IF LEN( cMaxItem ) < 76
               oNgi:WriteParBox( "      º " + afiTable[ x ] + SPACE( nSpace2 ) + " º " + IF( asiTable[ x ] == "|", STRTRAN( asiTable[ x ], "|", " " ), asiTable[ x ] ) + SPACE( nspace ) + " º " + atiTable[ x ] + SPACE( nspace3 ) + " º " + afoiTable[ x ] + SPACE( nspace4 ) + " º" + HB_OSNEWLINE() )
            ELSE
               oNgi:WriteParBox( "º " + afiTable[ x ] + SPACE( nSpace2 ) + " º " + IF( asiTable[ x ] == "|", STRTRAN( asiTable[ x ], "|", " " ), asiTable[ x ] ) + SPACE( nspace ) + " º " + atiTable[ x ] + SPACE( nspace3 ) + " º " + afoiTable[ x ] + SPACE( nspace4 ) + " º" + HB_OSNEWLINE() )
            ENDIF
         ENDIF
      ELSE
         IF nNumTableItems == 2
            IF LEN( cMaxItem ) < 76
               oNgi:WritePar( "      Ì" + REPL( "Í", aLensFitem[ nPos2 ] + 2 ) + "Î" + REPL( "Í", alensSitem[ nPos1 ] + 2 ) + "¹", .F. )
            ELSE
               oNgi:WritePar( "Ì" + REPL( "Í", aLensFitem[ nPos2 ] + 2 ) + "Î" + REPL( "Í", alensSitem[ nPos1 ] + 2 ) + "¹", .F. )
            ENDIF
         ELSEIF nNumTableItems == 3
            IF LEN( cMaxItem ) < 76
               oNgi:WritePar( "      Ì" + REPL( "Í", aLensFitem[ nPos2 ] + 2 ) + "Î" + REPL( "Í", alensSitem[ nPos1 ] + 2 ) + "Î" + REPL( "Í", alensTitem[ nPos3 ] + 2 ) + "¹", .F. )                   //-4
            ELSE
               oNgi:WritePar( "Ì" + REPL( "Í", aLensFitem[ nPos2 ] + 2 ) + "Î" + REPL( "Í", alensSitem[ nPos1 ] + 2 ) + "Î" + REPL( "Í", alensTitem[ nPos3 ] + 2 ) + "¹", .F. )     //-4
            ENDIF

         ELSEIF nNumTableItems == 4
            IF LEN( cMaxItem ) < 76
               oNgi:WritePar( "      Ì" + REPL( "Í", aLensFitem[ nPos2 ] + 2 ) + "Î" + REPL( "Í", alensSitem[ nPos1 ] + 2 ) + "Î" + REPL( "Í", alensTitem[ nPos3 ] + 2 ) + "Î" + REPL( "Í", alensfoitem[ nPos4 ] + 2 ) + "¹", .F. )             //-4
            ELSE
               oNgi:WritePar( "Ì" + REPL( "Í", aLensFitem[ nPos2 ] + 2 ) + "Î" + REPL( "Í", alensSitem[ nPos1 ] + 2 ) + "Î" + REPL( "Í", alensTitem[ nPos3 ] + 2 ) + "Î" + REPL( "Í", alensfoitem[ nPos4 ] + 2 ) + "¹", .F. )                   //-4
            ENDIF

         ENDIF

      ENDIF
   NEXT

   IF nNumTableItems == 2
      IF LEN( cMaxItem ) < 76
         oNgi:WritePar( "      È" + REPL( "Í", aLensFitem[ nPos2 ] + 2 ) + "Ê" + REPL( "Í", alensSitem[ nPos1 ] + 2 ) + "¼", .F. )          //-4
      ELSE
         oNgi:WritePar( "È" + REPL( "Í", aLensFitem[ nPos2 ] + 2 ) + "Ê" + REPL( "Í", alensSitem[ nPos1 ] + 2 ) + "¼", .F. )                //-4
      ENDIF

   ELSEIF nNumTableItems == 3
      IF LEN( cMaxItem ) < 76

         oNgi:WritePar( "      È" + REPL( "Í", aLensFitem[ nPos2 ] + 2 ) + "Ê" + REPL( "Í", alensSitem[ nPos1 ] + 2 ) + "Ê" + REPL( "Í", alensTitem[ nPos3 ] + 2 ) + "¼", .F. )     //-4
      ELSE
         oNgi:WritePar( "      È" + REPL( "Í", aLensFitem[ nPos2 ] + 2 ) + "Ê" + REPL( "Í", alensSitem[ nPos1 ] + 2 ) + "Ê" + REPL( "Í", alensTitem[ nPos3 ] + 2 ) + "¼", .F. )     //-4
      ENDIF

   ELSEIF nNumTableItems == 4
      IF LEN( cMaxItem ) < 76
         oNgi:WritePar( "      È" + REPL( "Í", aLensFitem[ nPos2 ] + 2 ) + "Ê" + REPL( "Í", alensSitem[ nPos1 ] + 2 ) + "Ê" + REPL( "Í", alensTitem[ nPos3 ] + 2 ) + "Ê" + REPL( "Í", alensfoitem[ nPos4 ] + 2 ) + "¼", .F. )                   //-4
      ELSE
         oNgi:WritePar( "È" + REPL( "Í", aLensFitem[ nPos2 ] + 2 ) + "Ê" + REPL( "Í", alensSitem[ nPos1 ] + 2 ) + "Ê" + REPL( "Í", alensTitem[ nPos3 ] + 2 ) + "Ê" + REPL( "Í", alensfoitem[ nPos4 ] + 2 ) + "¼", .F. )     //-4
      ENDIF
   ENDIF

/*   oNgi:WritePar( "" )*/
   afiTable  := {}
   asitable  := {}
   atitable  := {}
   afoitable := {}

RETURN Nil

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function ProcNgTable()
*+
*+    Called from ( genng.prg    )   2 - function procngdesc()
*+                ( genng1.prg   )   2 - function procngdesc()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION ProcNgTable( cBuffer, nNum )

   LOCAL nPos
   LOCAL cItem     := ''
   LOCAL cItem2    := ''
   LOCAL cItem3    := ''
   LOCAL cItem4    := ''
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
   /*
   If !empty(cBuffer)
       cItem   := SUBSTR( cBuffer, 1, AT( SPACE( 3 ), cBuffer ) - 1 )
       cBuffer := ALLTRIM( STRTRAN( cBuffer, cItem, "" ,,1) )
   else
       citem:=''
   endif

   if nNum==2
       If !empty(cBuffer)
       cItem2 := SUBSTR( cBuffer, 1 )
       else
        citem2:=''
       endif
   elseif nNum ==3
          If !empty(cBuffer)
        cItem2  := SUBSTR( cBuffer, 1, AT( SPACE( 3 ), cBuffer ) - 1 )
        cBuffer := ALLTRIM( STRTRAN( cBuffer, cItem2, "" ) )
        cItem3 := SUBSTR( cBuffer, 1 )
       else
        citem2:=''
        citem3:=''
       endif

  ELSEIF nNum > 3
         If !empty(cBuffer)
        cItem2  := SUBSTR( cBuffer, 1, AT( SPACE( 3 ), cBuffer ) - 1 )
        cBuffer := ALLTRIM( STRTRAN( cBuffer, cItem2, "" ) )

        cItem3  := SUBSTR( cBuffer, 1, AT( SPACE( 3 ), cBuffer ) - 1 )
        cBuffer := ALLTRIM( STRTRAN( cBuffer, cItem3, "" ) )

        cItem4  := SUBSTR( cBuffer, 1 )
       else
        citem2:=''
        citem3:=''
        citem4:=''
       endif

   ENDIF
   AADD( afiTable, RTRIM( LTRIM( cItem ) ) )
   AADD( asiTable, cItem2 )
      AADD( atiTable, cItem3 )
      AADD( afoiTable, cItem4 )

  */
   IF !EMPTY( cBuffer )
      cItem   := SUBSTR( cBuffer, 1, AT( SPACE( 3 ), cBuffer ) - 1 )
      cBuffer := ALLTRIM( STRTRAN( cBuffer, cItem, "" ) )
   ELSE
      citem := ''
   ENDIF

   IF nNum == 2
      IF !EMPTY( cBuffer )
         cItem2 := SUBSTR( cBuffer, 1 )
      ELSE
         citem2 := ''
      ENDIF
   ELSEIF nNum == 3
      IF !EMPTY( cBuffer )
         cItem2  := SUBSTR( cBuffer, 1, AT( SPACE( 3 ), cBuffer ) - 1 )
         cBuffer := ALLTRIM( STRTRAN( cBuffer, cItem2, "" ) )
         cItem3  := SUBSTR( cBuffer, 1 )
      ELSE
         citem2 := ''
         citem3 := ''
      ENDIF

   ELSEIF nNum > 3
      IF !EMPTY( cBuffer )
         cItem2  := SUBSTR( cBuffer, 1, AT( SPACE( 3 ), cBuffer ) - 1 )
         cBuffer := ALLTRIM( STRTRAN( cBuffer, cItem2, "" ) )

         cItem3  := SUBSTR( cBuffer, 1, AT( SPACE( 3 ), cBuffer ) - 1 )
         cBuffer := ALLTRIM( STRTRAN( cBuffer, cItem3, "" ) )

         cItem4 := SUBSTR( cBuffer, 1 )
      ELSE
         citem2 := ''
         citem3 := ''
         citem4 := ''
      ENDIF

   ENDIF
   AADD( afiTable, RTRIM( LTRIM( cItem ) ) )
   AADD( asiTable, cItem2 )
   //   IF !EMPTY( cItem3 )
   AADD( atiTable, cItem3 )
   //   ENDIF
   //   IF !EMPTY( cItem4 )
   AADD( afoiTable, cItem4 )
   //  ENDIF

RETURN Nil

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function ProcNGDesc()
*+
*+    Called from ( genng.prg    )   6 - function processing()
*+                ( genng1.prg   )   6 - function processing()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION ProcNGDesc( cBuffer, oNgi, cStyle )

   LOCAL cLine       := ''
   LOCAL nPos
   LOCAL CurPos      := 0
   LOCAL nColorPos
   LOCAL ccolor      := ''
   LOCAL cReturn     := ''
   LOCAL ncolorend
   LOCAL nIdentLevel
   LOCAL cOldLine
   LOCAL lEndPar     := .F.

   LOCAL lEndFixed     := .F.
   LOCAL lEndTable     := .F.
   LOCAL LFstTableItem := .T.
   LOCAL lArgBold      := .F.
   DEFAULT cStyle TO "Default"

   IF AT( '<par>', cBuffer ) == 0 .AND. !EMPTY( cBuffer ) .AND. cstyle <> "Example"
      cBuffer := '<par>' + cBuffer
   ENDIF

   IF EMPTY( cBuffer )
      oNgi:WritePar( "" )

   ENDIF

   IF cStyle <> "Example" .AND. AT( "<table>", cBuffer ) == 0 .AND. AT( "<fixed>", cBuffer ) = 0
      IF AT( "<par>", cBuffer ) >= 0 .OR. AT( "</par>", cBuffer ) = 0 .AND. !EMPTY( cbuffer )
         IF AT( "<par>", cBuffer ) > 0 .AND. AT( "</par>", cBuffer ) > 0
            IF cStyle == "Arguments"

               cReturn := cBuffer

               cReturn  := STRTRAN( cReturn, "<par>", "" )
               cReturn  := STRTRAN( cReturn, "</par>", "" )
               cReturn  := ALLTRIM( cReturn )
               nPos     := AT( " ", cReturn )
               cOldLine := LEFT( cReturn, nPos - 1 )
               cReturn  := STRTRAN( cReturn, cOldLine, "" )
               IF AT( "@", cOldLine ) > 0 .OR. AT( "()", cOldLine ) > 0 .OR. AT( "<", cOldLine ) > 0 .OR. AT( "_", cOldLine ) > 0
                  lArgBold := .T.
               ELSE
                  lArgBold := .f.
               ENDIF

               //            cBuffer:= strtran(cBuffer,"<par>","<par><b>")
               IF lArgBold
                  cReturn := '       <par><b>' + cOldLine + '</b> ' + cReturn + '    </par>'
               ELSE
                  cReturn := '       <par>' + cOldLine + ' ' + cReturn + '    </par>'
               ENDIF

               cbuffer := cReturn
            ENDIF
         ELSE
            cBuffer := FormatngBuff( cBuffer, cStyle, ongi )
         ENDIF
      ENDIF
   ENDIF

   IF AT( '<par>', cBuffer ) > 0 .AND. AT( '</par>', cBuffer ) > 0
      cBuffer := STRTRAN( cBuffer, '<par>', '' )
      cBuffer := STRTRAN( cBuffer, '<b>', ' ^b' )
      cBuffer := STRTRAN( cBuffer, '</b>', '^n ' )
      cBuffer := STRTRAN( cBuffer, '</color>', '^n ' )
      cBuffer := STRTRAN( cBuffer, '<em>', '' )
      cBuffer := STRTRAN( cBuffer, '</em>', '' )
      cBuffer := STRTRAN( cBuffer, '<i>', '' )
      cBuffer := STRTRAN( cBuffer, '</i>', '' )

      nColorPos := AT( '<color:', cBuffer )
      IF ncolorpos > 0
         checkngcolor( @cbuffer, ncolorpos )
      ENDIF
      //      Alltrim(cBuffer)
      IF cStyle == "Description" .OR. cStyle == "Compliance"
         nIdentLevel := 6
         nPos        := 0
         DO WHILE !lendPar
            IF nPos == 0
               cLine := SUBSTR( cBuffer, 1, 76 )
               nPos  := RAT( " ", cLine )
               IF nPos > 0

                  cLine := SUBSTR( cBuffer, 1, nPos )
               ENDIF

            ELSE
               cLine := SPACE( nidentLevel ) + SUBSTR( cBuffer, curPos, 69 )

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
               oNgi:WritePar( SPACE( nidentLevel ) + ALLTRIM( cLine ) )
            ENDIF
            curPos += nPos
         ENDDO

      ELSEIF cStyle == "Arguments"
         nIdentLevel := 18
         nPos        := 0

         DO WHILE !lendPar
            IF nPos == 0
               cLine := SUBSTR( cBuffer, 1, 76 )
               nPos  := RAT( " ", cLine )
               IF nPos > 0

                  cLine := SUBSTR( cBuffer, 1, nPos )
               ENDIF
               IF !EMPTY( cLine )
                  oNgi:WritePar( cLine )
               ENDIF

            ELSE
               cLine := SPACE( nidentLevel ) + SUBSTR( cBuffer, curPos, 58 )    //60

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
               IF !EMPTY( cLine )
                  oNgi:WritePar( SPACE( nidentLevel ) + ALLTRIM( cLine ) )
               ENDIF

            ENDIF
            curPos += nPos
         ENDDO

      ELSEIF cStyle == "Syntax"
         nIdentLevel := 6
         nPos        := 0
         DO WHILE !lendPar
            IF nPos == 0
               cLine := SUBSTR( cBuffer, 1, 76 )
               nPos  := RAT( " ", cLine )
               IF nPos > 0

                  cLine := SUBSTR( cBuffer, 1, nPos )
               ENDIF

            ELSE
               cLine := SPACE( nidentLevel ) + SUBSTR( ALLTRIM( cBuffer ), curPos, 69 )

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
               oNgi:WritePar( SPACE( nidentLevel ) + ALLTRIM( cLine ) )
            ENDIF
            curPos += nPos
         ENDDO

      ELSEIF cStyle == "Default"
         nIdentLevel := 6
         nPos        := 0
         DO WHILE !lendPar
            IF nPos == 0
               cLine := SUBSTR( cBuffer, 1, 76 )
               nPos  := RAT( " ", cLine )
               IF nPos > 0
                  cLine := SUBSTR( cBuffer, 1, nPos )
               ENDIF

            ELSE
               cLine := SPACE( nidentLevel ) + SUBSTR( cBuffer, curPos, 69 )
               nPos  := RAT( " ", cLine )
               IF AT( '</par>', cLine ) > 0
                  lEndPar := .T.
                  cLine   := STRTRAN( cLine, "</par>", "" )
               ENDIF
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
               oNgi:WritePar( RTRIM( cLine ) )
            ENDIF
            curPos += nPos
         ENDDO
      ENDIF
   ENDIF

   IF AT( '<fixed>', cBuffer ) > 0 .OR. cStyle = "Example"
      IF AT( '<fixed>', cBuffer ) = 0 .OR. !EMPTY( cBuffer )
         cBuffer := STRTRAN( cBuffer, "<par>", "" )
         cBuffer := STRTRAN( cBuffer, "<fixed>", "" )

         oNgi:WritePar( cBuffer )
      ENDIF
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
         IF AT( DELIM, cLine ) == 0
            oNgi:WritePar( cLine )
         ENDIF
      ENDDO

   END
   IF AT( '<table>', cBuffer ) > 0
      DO WHILE !lendTable
         cLine := TRIM( SUBSTR( ReadLN( @lEof ), nCommentLen ) )
         IF AT( "</table>", cLine ) > 0 .or. AT( "</TABLE>", cLine ) > 0
            lendTable := .t.
         ELSE
            IF LFstTableItem
               nNumTableItems := GetNumberofTableItems( cLine )
               procngtable( cline, nNumTableItems )
               LFstTableItem := .f.
            ELSE
               procngtable( cline, nNumTableItems )
            ENDIF

         ENDIF
      ENDDO
      IF lEndTable
         GenNgTable( oNgi )
         LFstTableItem:=.T.
      ENDIF
   ENDIF
   //      If cStyle=="Description" .or. cStyle=="Compliance"
   //        oNgi:Writepar('')
   //     endif
RETURN nil

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function checkngcolor()
*+
*+    Called from ( genhtm1.prg  )   1 - function prochtmdesc()
*+                ( genhtm2.prg  )   1 - function prochtmdesc()
*+                ( genng.prg    )   1 - function procngdesc()
*+                ( genng1.prg   )   1 - function procngdesc()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNC checkngcolor( cbuffer, ncolorpos )

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
         cReturn := "^a" + acolortable[ nreturn, 2 ]
      ENDIF
      cBuffer := STRTRAN( cBuffer, cOldColorString, cReturn )
   ENDDO
RETURN cbuffer

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function maxelem()
*+
*+    Called from ( genng.prg    )   5 - function genngtable()
*+                ( genng1.prg   )   4 - function genngtable()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNC maxelem( a )

   LOCAL nSize   := LEN( a )
   LOCAL max     := 0
   LOCAL tam     := 0
   LOCAL nMax2   := 0
   LOCAL nPos    := 1
   LOCAL cString

   LOCAL nCount
   FOR nCount := 1 TO nSize

      tam := LEN( a[ nCount ] )
      max := IF( tam > max, tam, max )
   NEXT
   nPos := ASCAN( a, { | x | LEN( x ) == max } )
RETURN max

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function FormatNgBuff()
*+
*+    Called from ( genhtm1.prg  )   1 - function prochtmdesc()
*+                ( genhtm2.prg  )   1 - function prochtmdesc()
*+                ( genng.prg    )   1 - function procngdesc()
*+                ( genng1.prg   )   1 - function procngdesc()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION FormatNgBuff( cBuffer, cStyle, ongi )

   LOCAL cReturn       := ''
   LOCAL cLine         := ''
   LOCAL cOldLine      := ''
   LOCAL cBuffEnd      := ''
   LOCAL lEndBuffer    := .f.
   LOCAL nPos
   LOCAL nPosend
   LOCAL lArgBold      := .f.
   LOCAL LFstTableItem := .t.
   cReturn := cBuffer + ' '
   IF AT( '</par>', cReturn ) > 0 .OR. EMPTY( cBuffer )
      IF EMPTY( cbuffer )
         cReturn := ''
      ENDIF
      RETURN cReturn
   ENDIF
   IF cStyle != "Syntax" .AND. cStyle != "Arguments" .AND. cStyle != "Return"
      DO WHILE !lEndBuffer
         cLine := TRIM( SUBSTR( ReadLN( @lEof ), nCommentLen ) )
         IF AT( '</par>', cLine ) > 0
            lEndBuffer := .t.
         ENDIF

         IF EMPTY( cLine )
            lEndBuffer := .t.

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
      cReturn := STRTRAN( cReturn, SPACE( 4 ), "" )
      cReturn := '<par><b>' + cReturn + ' </b></par>'

   ELSEIF cStyle == 'Arguments' .OR. cStyle == "Return"

      nPos    := 0
      cReturn := '<par>' + cReturn
      IF AT( "<par>", cReturn ) > 0
         cReturn  := STRTRAN( cReturn, "<par>", "" )
         cReturn  := STRTRAN( cReturn, "</par>", "" )
         cReturn  := ALLTRIM( cReturn )
         nPos     := AT( " ", cReturn )
         cOldLine := LEFT( cReturn, nPos - 1 )
         cReturn  := STRTRAN( cReturn, cOldLine, "" )
         IF AT( "@", cOldLine ) > 0 .OR. AT( "()", cOldLine ) > 0 .OR. AT( "<", cOldLine ) > 0 .OR. AT( "_", cOldLine ) > 0
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
      IF lArgBold
         cReturn := '       <par><b>' + cOldLine + '</b> ' + cReturn + '    </par>'
      ELSE
         cReturn := '       <par>' + cOldLine + ' ' + cReturn + '    </par>'
      ENDIF
   ENDIF
   lArgBold := .F.
   //   endif
RETURN cReturn

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Static Function ReadFromTop()
*+
*+    Called from ( genng.prg    )   1 - function processing()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
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

*+ EOF: GENNG.PRG
