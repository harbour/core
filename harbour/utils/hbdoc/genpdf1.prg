/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * GENPdf support module for hbdoc document Extractor 
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
MEMVAR aDocInfo,awww,aResult
STATIC aAlso
STATIC lIsTable       := .F.
STATIC nCommentLen
STATIC lEof
STATIC aFiTable       := {}
STATIC aSiTable       := {}
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
FUNCTION ProcessPdf(lMemory)

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
   LOCAL oPdf
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
local hhh
   DEFAULT lMemory to .F.
   lData         := .F.
   lMethod       := .F.
   lIsDataLink   := .F.
   lIsMethodLink := .F.
   if lMemory
      aadd(aResult,"Document")
   endif
   lPar := .T.
   //
   //  Entry Point
   //
   //  Put up information labels
   @ INFILELINE, 20 SAY "Extracting: "          
   @ MODULELINE, 20 SAY "Documenting: "         
   //  loop through all of the files
   if lmemory
//   oPdf := tPdf():new( "pdf\temp.pdf" )
   HB_PDFNEW("pdf\temp.pdf")
   hb_pdfnewpage("Harbour Guide",'Harbour Guide')
   hb_pdfendpage()
   else
   ? 'im here'
   HB_PDFNEW("pdf\harbour.pdf")
   hb_pdfnewpage("Harbour Guide",'Harbour Guide')
   hb_pdfinitbook(aResult)
   hb_pdfendpage()
   hhh:=fcreate('ssss.sss')
   endif

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
         cBuffer:=ReadLN( @lEof )
         if !lmemory
            ? valtype(cBuffer)
            ? cBuffer
            fWrite(hhh,cBuffer+hb_osnewline())
//            cBuffer:=" "
         Endif
         if !lmemory
         cBuffer := TRIM( SUBSTR( cBuffer, nCommentLen ) )
         else
         cBuffer :=  SUBSTR( cBuffer, nCommentLen )
         endif
         cBuffer := STRTRAN( cBuffer, CHR( 10 ), "" )
         nLineCnt ++
//         IF nLineCnt % 10 = 0
            @ LINELINE, 33 SAY STR( nLineCnt, 5, 0 )         
  //       ENDIF
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
               if lMemory
               nPos := ascan(aResult,{|a| UPPER(a) == UPPER(cCategory)})
               if nPos==0
                  aadd(aResult,cCategory)
               endif
               endif
               //  Now close down this little piece
               lDoc := .F.
               IF .NOT. EMPTY( cSeeAlso )
                  hb_pdfwritetext(" ")
                  hb_pdfwriteBoldtext1( "See Also:" )
                  hb_pdfwritetext(" ")
                  FOR nAlso := 1 TO LEN( aalso )

                     IF nAlso == 1
                        nPos:=ascan(awww,{|a,b| Upper(a[1])== upper(aAlso[nAlso])})
                        if nPos>0
                           HB_PDFADDLINK(awww[ npos,1 ],aWww[nPos,2] )
                        else
                           HB_PDFADDLINK(awww[ 1,1 ],aWww[1,2] )
                        endif
                     ELSE
                          nPos:=ascan(awww,{|a,b| Upper(a[1])== upper(aAlso[nAlso])})
                        if nPos>0
                           HB_PDFADDLINK(awww[ npos,1 ],aWww[nPos,2] )
                        else
                           HB_PDFADDLINK(awww[ 1,1 ],aWww[1,2] )
                        endif

                     ENDIF

                  NEXT

                  hb_pdfWriteText( " " )
               ENDIF

               hb_pdfendpage()
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

//               oPdf := TNortonGuide():new( "ngi\" + cFileName )
/*               IF oPdf:nHandle < 1
                  ? "Error creating", cFileName, ".ngi"
                  WRITE_ERROR( "Error creating",,,, cFileName + ".ngi" )
               ENDIF*/
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
                  if lMemory
//                  oPdf:WriteTitle( PAD( cFuncName, 40 ), cFuncName ,cCategory)
                  HB_PDFNEWPAGE( PAD( cFuncName, 40 ), cFuncName)
                  hb_pdfWritetext(" ")
                  aadd(aWww,{alltrim(cFuncname),HB_GETPAGE()})
                  else
//                  oPdf:WriteTitle( PAD( cFuncName, 40 ), cFuncName ,cCategory,.t.)
                  HB_PDFNEWPAGE( PAD( cFuncName, 40 ), cFuncName)
                  HB_PDFBOOKMARK(  cCategory,cFuncName,{|x| UPPER(x[1])==UPPER(cCategory )} )
//                  HB_PDFINITBOOK(awww)
                  endif
                  hb_pdfwriteBoldtext( cOneLine )
                  hb_pdfWritetext(" ")
            ELSE

               IF AT( cSyn, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )

                     hb_pdfwriteBoldtext1( " Syntax" )
                     hb_pdfWritetext( " " )
                     nMode     := D_SYNTAX
                     lAddBlank := .T.
                  ENDIF
               ELSEIF AT( cConstruct, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     hb_pdfWritetext( " " )
                     hb_pdfwriteBoldtext1( " Constructor syntax" )
                     hb_pdfWritetext( " " )
                     nMode     := D_SYNTAX
                     lAddBlank := .T.
                  ENDIF
               ELSEIF AT( cArg, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )

                     IF !lBlankLine
                        hb_pdfWritetext( " " )
                        hb_pdfwriteBoldtext1( " Arguments" )
                        hb_pdfWritetext( " " )
                     ENDIF

                     nMode     := D_ARG
                     lAddBlank := .T.
                  ENDIF
               ELSEIF AT( cRet, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )

                     //                  IF !lBlankLine
                                                hb_pdfWriteText( " " )
                     //                  ENDIF

                     hb_pdfwriteBoldtext1( " Returns" )
                     hb_pdfWritetext( " " )
                     nMode     := D_ARG
                     lAddBlank := .T.
                  ENDIF
               ELSEIF AT( cDesc, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     hb_pdfWritetext( " " )
                     hb_pdfwriteBoldtext1( " Description" )
                     hb_pdfWritetext( " " )
                     nMode     := D_DESCRIPTION
                     lAddBlank := .T.
                     lPar      := .T.
                  ENDIF
               ELSEIF AT( cdatalink, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     IF !lBlankLine
                              hb_pdfWriteText( "" )                 //:endpar()
                     ENDIF

                     hb_pdfwriteBoldtext1( " Data")
                     hb_pdfWritetext( " " )
                     nMode     := D_DATALINK
                     lAddBlank := .T.

                     lIsDataLink := .T.
                  ENDIF
               ELSEIF AT( cDatanolink, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     IF !lIsDataLink
                                          hb_pdfWritetext( " " )
                        hb_pdfwriteBoldtext1( " Data")
                                          hb_pdfWritetext( " " )
                     ENDIF
                     nMode     := D_NORMAL
                     lAddBlank := .T.

                     lPar := .T.
                  ENDIF
               ELSEIF AT( cMethodslink, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                                          hb_pdfWritetext( " " )
                     hb_pdfwriteBoldtext1( " Method" )
                                          hb_pdfWritetext( " " )
                     nMode     := D_METHODLINK
                     lAddBlank := .T.

                     lIsMethodLink := .T.
                  ENDIF
               ELSEIF AT( cMethodsnolink, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )
                     IF !lIsMethodLink
                                          hb_pdfWritetext( " " )
                        hb_pdfwriteBoldtext1( " Methods" )
                                             hb_pdfWritetext( " " )
                     ENDIF

                     nMode     := D_NORMAL
                     lAddBlank := .T.
                     lPar      := .T.
                  ENDIF

               ELSEIF AT( cExam, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )

                     IF !lBlankLine
                                                   hb_pdfWriteText( "" )
                        hb_pdfwriteBoldtext1( " Examples" )
                                             hb_pdfWritetext( " " )
                     ENDIF

                     nMode     := D_EXAMPLE
                     lAddBlank := .T.
                  ENDIF

               ELSEIF AT( cTest, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )

                     IF !lBlankLine
                                             hb_pdfWritetext( " " )
                        hb_pdfwriteBoldtext1( " Tests" )
                                             hb_pdfWritetext( " " )
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
                                          hb_pdfWritetext( " " )
                        hb_pdfwriteBoldtext1( " Compliance" )
                                             hb_pdfWritetext( " " )
                     ENDIF

                     nMode     := D_COMPLIANCE
                     lAddBlank := .T.
                     lPar      := .t.
                  ENDIF
               ELSEIF AT( cPlat, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )

                     IF !lBlankLine
                                          hb_pdfWritetext( " " )
                        hb_pdfwriteBoldtext1( " Platforms")
                                             hb_pdfWritetext( " " )
                     ENDIF

                     nMode     := D_NORMAL
                     lAddBlank := .T.
                     lPar      := .t.
                  ENDIF
               ELSEIF AT( cFiles, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )

                     IF !lBlankLine
                     hb_pdfWritetext( " " )
                     hb_pdfwriteBoldtext1( " Files" )
                     hb_pdfWritetext( " " )
                     ENDIF

                     nMode     := D_NORMAL
                     lAddBlank := .T.
                     lPar      := .t.
                  ENDIF
               ELSEIF AT( cFunction, cBuffer ) > 0
                  IF GetItem( cBuffer, nCurdoc )

                     //                  IF !lBlankLine
                                                hb_pdfWriteText( "" )
                     //                  ENDIF
                     hb_pdfwriteBoldtext1( " Functions" )
                     hb_pdfWritetext( " " )

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
                     ProcPdfDesc( cbuffer, oPdf, "Syntax" )
                  ELSEIF nMode = D_ARG
                     IF LEN( cBuffer ) > LONGLINE
                        WRITE_ERROR( "Arguments", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )

                     ProcPdfDesc( cbuffer, oPdf, "Arguments" )
                  ELSEIF nMode = D_EXAMPLE
                     IF LEN( cBuffer ) > LONGLINE
                        WRITE_ERROR( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     ProcPdfDesc( cBuffer, oPdf, "Example" )
                  ELSEIF nMode = D_DESCRIPTION
                     IF LEN( cBuffer ) > LONGLINE
                        WRITE_ERROR( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     ProcPdfDesc( cBuffer, oPdf, "Description" )

                  ELSEIF nMode = D_NORMAL
                     IF LEN( cBuffer ) > LONGLINE
                        WRITE_ERROR( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     ProcPdfDesc( cBuffer, oPdf )
                  ELSEIF nMode = D_COMPLIANCE
                     IF LEN( cBuffer ) > LONGLINE
                        WRITE_ERROR( "General", cBuffer, nLineCnt, ;
                                     LONGLINE, aDirList[ i, F_NAME ] )
                     ENDIF
                     lBlankLine := EMPTY( cBuffer )
                     ProcPdfDesc( cBuffer, oPdf, "Compliance" )

                  ELSEIF nMode = D_SEEALSO
                     IF .NOT. EMPTY( cBuffer )
                        cSeeAlso := ProcPdfAlso( StripFiles( ALLTRIM( cBuffer ) ) )
                     ENDIF
                  ELSEIF nMode = D_INCLUDE
                     //  read next line
                  ELSEIF nMode = D_STATUS
                     IF !EMPTY( cBuffer )
                        hb_pdfwritetext('')
                        hb_pdfwriteBoldtext1( " Status" )
                        hb_pdfwritetext('')
                     ENDIF
                     ProcStatusPdf( oPdf, cBuffer )

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
/*   oPdf:Close()*/
HB_PDFCLOSE()
   if lmemory
//      ferase('pdf\temp.pdf')
fclose(hhh)
   endif

RETURN Nil

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function ProcPdfAlso()
*+
*+    Called from ( genhtm2.prg  )   1 - function processwww()
*+                ( genng.prg    )   1 - function processing()
*+                ( genng1.prg   )   1 - function processing()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION ProcPdfAlso( cSeealso )

   aAlso := {}
   aAlso := ListAsArray2( cSeealso, "," )

RETURN aAlso
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function ProcStatusPdf()
*+
*+    Called from ( genhtm2.prg  )   1 - function processwww()
*+                ( genng.prg    )   1 - function processing()
*+                ( genng1.prg   )   1 - function processing()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION ProcStatusPdf( nWriteHandle, cBuffer )

   IF LEN( ALLTRIM( cBuffer ) ) > 1
      hb_pdfWriteText( cBuffer )
   ELSEIF SUBSTR( ALLTRIM( cBuffer ), 1 ) == "R"
      hb_pdfWriteText( "       Ready" )
   ELSEIF SUBSTR( ALLTRIM( cBuffer ), 1 ) == "S"
      hb_pdfWriteText( "       Started" )
   ELSEIF SUBSTR( ALLTRIM( cBuffer ), 1 ) == "C"
      hb_pdfWriteText( "       Clipper" )
   ELSE
      hb_pdfWriteText( "       Not Started" )
   ENDIF

RETURN nil

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function GenPdfTable()
*+
*+    Called from ( genng.prg    )   1 - function ProcPdfDesc()
*+                ( genng1.prg   )   1 - function ProcPdfDesc()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+

FUNCTION GenPdfTable( oPdf ,nNumTableItems)

if nNumTableItems <3
hb_pdfWriteText( " " )
HB_PDFTABLE(aFitable,aSitable)
elseif nNumTableItems<4
hb_pdfWriteText( " " )
HB_PDFTABLE(aFitable,aSitable,aTitable)
else
hb_pdfWriteText( " " )
HB_PDFTABLE(aFitable,aSitable,aTitable,aFoiTable)
endif

      
   afiTable  := {}
   asitable  := {}
   atitable  := {}
   afoitable := {}

RETURN Nil

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function ProcPdfTable()
*+
*+    Called from ( genng.prg    )   2 - function ProcPdfDesc()
*+                ( genng1.prg   )   2 - function ProcPdfDesc()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+

FUNCTION ProcPdfTable( cBuffer, nNum )

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
   Formattablestring(cItem ,cItem2 ,cItem3 ,cItem4 ,nNum )



RETURN Nil

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function ProcPdfDesc()
*+
*+    Called from ( genng.prg    )   6 - function processing()
*+                ( genng1.prg   )   6 - function processing()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION ProcPdfDesc( cBuffer, oPdf, cStyle )

   LOCAL cLine       := ''
   LOCAL nPos
   LOCAL cBold       := ''
   LOCAL cRemove     := ''
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
            hb_pdfWriteText( "" )

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
                  cReturn := '<par><b>' + cOldLine + '</b> ' + cReturn + '    </par>'
               ELSE
                  cReturn := '<par>' + cOldLine + ' ' + cReturn + '    </par>'
               ENDIF

               cbuffer := cReturn
            ENDIF
         ELSE
            cBuffer := FormatPdfBuff( cBuffer, cStyle, oPdf )
         ENDIF
      ENDIF
   ENDIF

   IF AT( '<par>', cBuffer ) > 0 .AND. AT( '</par>', cBuffer ) > 0
      nColorPos := AT( '<color:', cBuffer )
      IF ncolorpos > 0
         CheckPdfColor( @cbuffer, ncolorpos )
      ENDIF
      //      Alltrim(cBuffer)
      IF cStyle == "Description" .OR. cStyle == "Compliance"
         nIdentLevel := 7
         nPos        := 0
         cBuffer:=strtran(cbuffer,"<par>",'')

         DO WHILE !lendPar
            IF nPos == 0
               cLine := SUBSTR( cBuffer, 1, 85 )
               nPos  := RAT( " ", cLine )
               IF nPos > 0

                  cLine := SUBSTR( cBuffer, 1, nPos )
               ENDIF

            ELSE
               cLine := SPACE( nidentLevel ) + SUBSTR( cBuffer, curPos, 85 )

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
            IF !EMPTY( cLine ) .and. at("<b>",cLine)==0 .and. at("</b>",cLine)==0
                     hb_pdfWriteText( SPACE( nidentLevel ) + ALLTRIM( cLine ) )
            elseif !EMPTY( cLine ) .and. at("<b>",cLine)>=0 .and. at("</b>",cLine)>=0
                     cLine:=strtran(cLine,"<b>","")
                     cLine:=strtran(cLine,"</b>","")
                     hb_pdfWriteBoldText( SPACE( nidentLevel )+ALLTRIM( cLine ) )
            ENDIF

            curPos += nPos
         ENDDO

      ELSEIF cStyle == "Arguments"
         nIdentLevel := 7
         nPos        := 0
         cBuffer:=strtran(cbuffer,"<par>",'')
         DO WHILE !lendPar
            IF nPos == 0
               cLine := SUBSTR( cBuffer, 1, 85 )
               nPos  := RAT( " ", cLine )
               IF nPos > 0
                  cLine := SUBSTR( cBuffer, 1, nPos )
               ENDIF
               IF !EMPTY( cLine ) .and. at("<b>",cLine)>=0 .and. at("</b>",cLine)>=0

                      if at("<b>",cline) >0
                         cLine:=strtran(cLine,"<b>","")
                         cBold:=subStr(cLine,1,at("</b>",cLine)-1)
                         cRemove:=cBold+"</b>"
                         cLine:=strtran(cLine,cRemove,"")
                         HB_PDFWRITEARG(SPACE( nidentLevel )+cBold,cLine)
                   ENDIF
            Endif
            ELSE
               cLine := SPACE( nidentLevel ) + SUBSTR( cBuffer, curPos, 85 )    //60

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
                     hb_pdfWriteText( SPACE( nidentLevel ) + ALLTRIM( cLine ) )
               ENDIF

            ENDIF
            curPos += nPos
         ENDDO            

      ELSEIF cStyle == "Syntax"
         nIdentLevel := 6
         nPos        := 0

         cBuffer:=strtran(cbuffer,"<par>",'')
         cBuffer:=strtran(cbuffer,"<b>",'')
         cBuffer:=strtran(cbuffer,"</b>",'')
         cBuffer:=alltrim(cbuffer)
         DO WHILE !lendPar
            IF nPos == 0
               cLine := SUBSTR( cBuffer, 1, 85 )
               nPos  := RAT( " ", cLine )
               IF nPos > 0

                  cLine := SUBSTR( cBuffer, 1, nPos )
               ENDIF

            ELSE
               cLine := SPACE( nidentLevel ) + SUBSTR( cBuffer, curPos, 85 )

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
                     hb_pdfWriteBoldText( SPACE( nidentLevel ) + ALLTRIM( cLine ) ,.f.)
            ENDIF
            curPos += nPos
         ENDDO

      ELSEIF cStyle == "Default"
         nIdentLevel := 7
         nPos        := 0
//         hb_pdfwritetext(cbuffer)
       cBuffer:=strtran(cbuffer,"<par>",'')
         
         DO WHILE !lendPar
            IF nPos == 0
               cLine := SUBSTR( cBuffer, 1, 85 )
               nPos  := RAT( " ", cLine )
               IF nPos > 0

                  cLine := SUBSTR( cBuffer, 1, nPos )
               ENDIF

            ELSE
               cLine := SPACE( nidentLevel ) + SUBSTR( cBuffer, curPos, 85 )

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
                     hb_pdfWriteText( SPACE( nidentLevel ) + ALLTRIM( cLine ) )
            ENDIF
            curPos += nPos
         ENDDO


      ENDIF
   ENDIF

   IF AT( '<fixed>', cBuffer ) > 0 .OR. cStyle = "Example"
      IF AT( '<fixed>', cBuffer ) = 0 .OR. !EMPTY( cBuffer )
         cBuffer := STRTRAN( cBuffer, "<par>", "" )
         cBuffer := STRTRAN( cBuffer, "<fixed>", "" )
               hb_pdfWriteText( cBuffer )
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
                  hb_pdfWriteText( cLine )
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
               ProcPdfTable( cline, nNumTableItems )
               LFstTableItem := .f.
            ELSE
               ProcPdfTable( cline, nNumTableItems )  
            ENDIF

         ENDIF
      ENDDO
      IF lEndTable
         GenPdfTable( oPdf ,nNumTableItems) 
         LFstTableItem:=.T.
      ENDIF
   ENDIF
RETURN nil

*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
*+    Function CheckPdfColor()
*+
*+    Called from ( genhtm1.prg  )   1 - function prochtmdesc()
*+                ( genhtm2.prg  )   1 - function prochtmdesc()
*+                ( genng.prg    )   1 - function ProcPdfDesc()
*+                ( genng1.prg   )   1 - function ProcPdfDesc()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNC CheckPdfColor( cbuffer, ncolorpos )

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
*+    Function MaxElemPdf()
*+
*+    Called from ( genng.prg    )   5 - function GenPdfTable()
*+                ( genng1.prg   )   4 - function GenPdfTable()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNC MaxElemPdf( a )

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
*+    Function FormatPdfBuff()
*+
*+    Called from ( genhtm1.prg  )   1 - function prochtmdesc()
*+                ( genhtm2.prg  )   1 - function prochtmdesc()
*+                ( genng.prg    )   1 - function ProcPdfDesc()
*+                ( genng1.prg   )   1 - function ProcPdfDesc()
*+
*+±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
*+
FUNCTION FormatPdfBuff( cBuffer, cStyle, oPdf )

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
         cReturn := '<par><b>' + cOldLine + '</b> ' + cReturn + '    </par>'
      ELSE
         cReturn := '<par>' + cOldLine + ' ' + cReturn + '    </par>'
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

static Function Formattablestring(cItem,cItem2,cItem3,cItem4,nItem)
Local aItems:={}
Local aItems2:={}
Local aItems3:={}
      
Local aItems4:={}

if nItem <3
   aadd(aitems,citem)
   aadd(aItems2,citem2)
elseif nItem <4
   aadd(aitems,cItem)
   aadd(aitems2,citem2)
   aadd(aitems3,citem3)
else
   aadd(aitems,cItem)
   aadd(aitems2,citem2)
   aadd(aitems3,citem3)
   aadd(aItems4,citem4)
endif
aadd(aFiTable,aItems)
aadd(aSiTable,aItems2)
aadd(aTitable,aItems3)
aadd(aFoiTable,aItems4)

return nil
static function getArray(aItems,aItems2,Aitems3,aItems4)
local nSize := 0
nSize:=Len(aItems)
if nSize<Len(aitems2)
nSize:=Len(aItems2)   
endif
if nSize<Len(aitems3)
nSize:=Len(aItems3)   
endif
if nSize<Len(aitems4)
nSize:=Len(aItems4)   
endif
Return nSize


*+ EOF: GENNG.PRG
