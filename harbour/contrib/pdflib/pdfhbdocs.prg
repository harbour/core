/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * PDF api Api for multiple porpurse
 *
 * Copyright 2000-2004 Luiz Rafael Culik <culikr /at/ brturbo.com>
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


#pragma begindump
/* declare for later use on C functions */
#include "hbapi.h"
#include "hbfast.h"
#pragma enddump

#include "pdfhbdoc.ch"
#include "common.ch"
#INCLUDE "pdf.ch"

/*Variaveis Staticas */
GLOBAL lLandScape := .F.
STATIC fontSizePoints     := 10.0 /*10.0*/
STATIC NumofRows
STATIC LEAD               := 10
STATIC iRow               := 800
STATIC fCurrentRowSetting := 800
STATIC fOldPos
STATIC iWidth
STATIC iCol
STATIC sziFontBold        := 0
STATIC fFootBottom        := 40
STATIC fColumn            := 10
STATIC sziFont            := 0
STATIC fPageWidth         := a4_width
STATIC fPageHeigth        := a4_height
STATIC iPage              := 1
STATIC uiLen
STATIC uiCount
STATIC bTItems
STATIC bFItems
STATIC szUserpass
STATIC iLastLinkPos       := 0
STATIC oPdf

STATIC FUNCTION setBox( irow, icol, h, iw )

   opdf:PDF_RECT( irow, icol, h, iw )

   opdf:PDF_STROKE()
RETURN NIL

FUNCTION HB_PDFNEW( szFileResult, fontsize, iStartRow, iBottom, iStartCol, iEndBottom, szMasterPass, szPass )

   DEFAULT fontsize TO fontSizePoints
   DEFAULT iStartRow TO fPageWidth
   DEFAULT iBottom TO fPageHeigth
   DEFAULT iStartCol TO fColumn
   DEFAULT iEndBottom TO fFootBottom
   DEFAULT szMasterPass TO ""
   DEFAULT szPass TO ""
   szFileResult := Lower( szFileResult )

   oPdf := TPdf():New( szFileResult,, iStartRow, iBottom )
   IF oPdf == NIL
      RETURN .f.
   ENDIF

   fColumn := iStartCol
   oPdf:SetTop( iRow )                  // top
   oPdf:SetLeft( fColumn )              // left & right
   oPdf:SetBottom( iEndBottom )

   fPageWidth  := iStartRow
   fPageHeigth := iBottom

   fontSizePoints := fontsize
   LEAD           := fontSizePoints

   /* Ajusta Coluna inicial/rodapé e o numero atual de linhas por pagina */

   iRow               := iStartRow - iEndBottom
   fCurrentRowSetting := iRow
   fFootBottom        := iEndBottom
   fColumn            := iStartCol
   NumofRows          := iRow / fontSizePoints

RETURN .T.

FUNCTION HB_PDFENDPAGE

   iRow := fCurrentRowSetting
   opdf:PDFCLOSEpage() /* close page       */
   iPage ++
   iLastLinkPos := 0
RETURN NIL

FUNCTION HB_PDFNEWPAGE( szTitleT, szFile )

LOCAL image
LOCAL scale_x
LOCAL scale_y

   oPdf:pdfNewPage( "A4", Iif( fPageHeigth < fPageWidth, "P", "L" ),, "Courier", 0 )
   IF Ischaracter( szFile )
      IF lLandScape .OR. opdf:aReport[ PAGEORIENT ] == "L"
         oPdf:pdfImage( szFile, 490 / 0.4, 60, 126, 126,, 0.4, 0.4 )
      ELSE
         oPdf:pdfImage( szFile, 755 / 0.4, 20, 126, 126,, 0.4, 0.4 )
      ENDIF
   ENDIF
   oPdF:PDFsetfont( "Courier", 1, FONTSIZEBOLD )
   oPdf:pdfAtSay( szTitleT, iRow -= 15, fColumn, .T. )

RETURN nil

FUNC HB_PDFWRITETEXT( cText )

   IF ( iRow <= fFootBottom )
      iRow := fCurrentRowSetting
      opdf:PDFCLOSEpage()
      //PDF_begin_page(szPDFFile, fPageWidth, fPageHeigth) /* start a new page */
      oPdf:pdfNewPage( "A4", Iif( fPageHeigth < fPageWidth, "P", "L" ),, "Courier", 0 )
      iLastLinkPos := 0
      iPage ++
   ENDIF

   opdf:PDFsetfont( FONTNORMAL, 0, fontSizePoints )

   opdf:pdfAtSay( cText, iRow -= LEAD, fColumn, .T. )
RETURN nil

FUNC HB_PDFWRITEBOLDTEXT( ctext )

   IF ( iRow <= fFootBottom )
      iRow := fCurrentRowSetting
      opdf:PDFCLOSEpage()
      //PDF_begin_page(szPDFFile, fPageWidth, fPageHeigth) /* start a new page */
      oPdf:pdfNewPage( "A4", Iif( fPageHeigth < fPageWidth, "P", "L" ),, "Courier", 1 )
      iLastLinkPos := 0
      iPage ++
   ENDIF

   opdf:PDFsetfont( FONTNORMAL, 1, fontSizePoints )
   opdf:pdfAtSay( cText, iRow -= LEAD, fColumn, .T. )

RETURN nil

FUNC HB_PDFWRITETEXTBOLD1( cText )

   IF ( iRow <= fFootBottom )
      iRow := fCurrentRowSetting
      opdf:PDFCLOSEpage()
      //PDF_begin_page(szPDFFile, fPageWidth, fPageHeigth) /* start a new page */
      oPdf:pdfNewPage( "A4", Iif( fPageHeigth < fPageWidth, "P", "L" ),, "Courier", 1 )
      iLastLinkPos := 0
      iPage ++
   ENDIF

   opdf:PDFsetfont( FONTNORMAL, 1, FONTSIZEBOLD )
   opdf:pdfAtSay( cText, iRow -= LEAD, fColumn, .T. )
   //      PDFsetfont(FONTNORMAL,0, fontSizePoints)

RETURN nil

FUNCTION HB_PDFCLOSE

   opdf:pdfclose()
   fontSizePoints     := 10.0 /*10.0*/
   LEAD               := 10
   iRow               := 800
   fCurrentRowSetting := 800
   lLandScape         := .F.

   RETURN nil

   /* FUNCAO PDFMAXLINES()
   Essa funcao retorna o numero maximo de linhas por pagina de
   acordo com a fonte setada.
*/

FUNCTION PDFMAXLINES

   RETURN NumofRows

   /* FUNCAO PDFMAXCOLS()

   Essa funcao retorna o numero maximo de colunas por pagina de
   acordo com a fonte setada.
*/

FUNCTION PDFMAXCOLS

RETURN ( ( fPageWidth - 2 * fColumn ) / fontSizePoints )

FUNCTION PDFWRITETEXTAT( cText, iThisRow, iThisCol )

LOCAL currow
   iThisRow := Iif( Isnumber( iThisRow ), fCurrentRowSetting - RowtoPoint( iThisRow ), iRow )
   iThisCol := RowtoPoint( iThisCol )
   currow   := iRow
   opdf:PDFsetfont( FONTNORMAL, 0, fontSizePoints )
   opdf:pdfAtSay( cText, iThisRow, iThisCol, .T. )

RETURN Nil

FUNCTION POINTTOROW( iPt )

LOCAL thisRow

   thisRow := iPt / FONTSIZEPOINTS

RETURN NUMOFROWS - thisRow

FUNCTION ROWTOPOINT( ThisRow )

   RETURN thisRow * FONTSIZEPOINTS

   /* FUNCAO PDFWRITETABLE(nRow, nCol , aTableCols, aLenCol,nNumLines, nStyle)
   Parametros
   nRow        -> Linha onde tabela inicia
   nCol        -> Coluna onde tabela inicia
   aTableCols  -> Array de Items da coluna
   aLenCol     -> Array com as larguras da coluna
   nNumLines   -> Numero de linhas da tabela
   nStyle      -> Stilo da tabela
*/

FUNCTION PDFWRITETABLE( nRow, nCol, pArray, pCol, fNumLines, nStyle )

LOCAL ulTempPos
LOCAL ulTempPos1
LOCAL fRow       := GraphRowtoPoint( nRow ) - 0.5 * fontSizePoints
LOCAL fCol       := RowtoPoint( nCol )
LOCAL nNumLines  := fNumLines
LOCAL fThisRow   := RowtoPoint( nRow )
LOCAL fCurRow    := fNumLines + 0.5 * fontSizePoints
LOCAL fLinesDiff := fNumLines - fRow

LOCAL DiffFromRow := fRow - iRow
LOCAL fLen        := 0
LOCAL fCurrRow    := fRow
LOCAL fTemp
LOCAL fOldCol     := fCol
LOCAL pTemp
LOCAL nLineGen
LOCAL fBoxSize
LOCAL fLastRow
LOCAL cItem

   DEFAULT pArray TO NIL

   SWITCH( nStyle )

CASE 0

   FOR Each pTemp in pCol
      fLen += RowtoPoint( pTemp )

   NEXT

   setBox( fCol, fRow, fLen, - fCurRow )

   FOR EACH pTemp IN pArray
      fRow -= LEAD

      IF Isarray( pTemp )
         fCol := fOldCol
         FOR each cItem IN pTemp
            ultempPos1 := hb_enumindex()
            opdf:pdfAtSay( cItem, fRow, fCol + 1, .T. )
            fCol += RowtoPoint( pCol[ ulTempPos1 ] )
         NEXT
      ENDIF

   NEXT

   iRow -= fNumLines + fontSizePoints

   EXIT

CASE 1

   fTemp := - fCurRow

   fCol := fOldCol

   FOR each pTemp in pCol
      setBox( fCol, fRow, RowtoPoint( pTemp ), - fCurRow + ( 1 * fontSizePoints ) )
      fCol += RowtoPoint( pTemp )

   NEXT

   FOR each pTemp IN pArray

      IF Isarray( pTemp )
         fRow -= LEAD
         fCol := fOldCol

         FOR each cItem in pTemp
            ulTempPos1 := hb_enumindex()
            opdf:pdfAtSay( cItem, fRow, fCol + 1, .T. )
            fCol += RowtoPoint( pCol[ ulTempPos1 ] )
         NEXT

      ENDIF

   NEXT

   iRow -= fNumLines + fontSizePoints

   EXIT

CASE 2

   nLineGen := 0
   fBoxSize := 0
   fLastRow := fRow

   fTemp := - fCurRow
   fLen  := fRow

   FOR each pTemp in pCol
      fBoxSize += RowtoPoint( pTemp )
   NEXT

   WHILE ( nLineGen < nNumLines )

      setBox( fCol, fLen, fBoxSize, - fontSizePoints * 1.5 )
      fLen     -= fontSizePoints * 1.5
      nLineGen += 2
   ENDDO

   fRow     := fCurrRow
   fLen     := fRow
   nLineGen := 0

   FOR EACH pTemp IN pCol

      WHILE ( nLineGen < nNumLines )

         setBox( fCol, fLen, RowtoPoint( pTemp ), - fontSizePoints * 1.5 )
         fLen     -= fontSizePoints * 1.5
         nLineGen += 2

      ENDDO

      fCol     += RowtoPoint( pTemp )
      fLen     := fRow
      nLineGen := 0
   NEXT

   fCol := fOldCol

   fRow := fLastRow
   fRow -= LEAD

   FOR EACH pTemp in pArray
      ulTempPos := hb_enumindex()

      IF Isarray( pTemp )

         //if ( hb_arrayGet(pArray, ulTempPos,pTemp))

         fCol := fOldCol

         FOR each cItem in pTemp

            //               PDF_show_xy( szPDFFile, hb_arrayGetCPtr( pTemp , ulTempPos1 ), fCol+1 , fRow )
            opdf:pdfAtSay( cItem, fRow, fCol + 1, .T. )
            fCol += RowtoPoint( pCol[ hb_enumindex() ] )
         NEXT

         fRow -= LEAD

         IF ( ulTempPos == 1 )
            fRow += LEAD / 2
         ELSE
            fRow -= LEAD / 2
         ENDIF
      ENDIF

      IF ( ulTempPos == 1 )
         fRow -= LEAD
      ENDIF

   NEXT

   iRow -= fNumLines + fontSizePoints
   EXIT

CASE 3

   fBoxSize := 0
   fLastRow := fRow

   fTemp := - fCurRow
   fLen  := fRow

   FOR EACH pTemp in pCol
      fBoxSize += RowtoPoint( pTemp )
   NEXT

   fTemp := - fCurRow
   setBox( fCol, fRow, fBoxSize, - ( 1.5 * fontSizePoints ) )
   fTemp     := fCurRow
   fNumLines -= RowtoPoint( 3 )
   fCurRow   := fNumLines + 0.5 * fontSizePoints
   setBox( fCol, fRow - 1.5 * fontSizePoints, fBoxSize, - fCurRow + ( 1.0 * fontSizePoints ) )
   fRow := fCurrRow
   fCol := fOldCol

   FOR EACH pTemp In pCol

      //         if (ulTempPos == 1 ||ulTempPos == 2 )
      setBox( fCol, fRow, RowtoPoint( pTemp ), - ( 1.5 * fontSizePoints ) )
      //         else
      setBox( fCol, fRow - 1.5 * fontSizePoints, RowtoPoint( pTemp ), - fCurRow + ( 1.0 * fontSizePoints ) )

      fCol += RowtoPoint( pTemp )

   NEXT

   fRow -= LEAD

   FOR EACH pTemp in pArray
      ulTempPos := hb_enumindex()

      IF Isarray( ptemp )

         fCol := fOldCol

         FOR each cItem in pTemp
            ulTempPos1 := hb_enumindex()

            IF ( ulTempPos == 1 )
               opdf:pdfAtSay( cItem, fRow, fCol + 1, .T. )
            ELSE
               opdf:pdfAtSay( cItem, fRow, fCol + 1, .T. )
            ENDIF

            fCol += RowtoPoint( pCol[ ulTempPos1 ] )

         NEXT

         fRow -= LEAD

         IF ( ulTempPos == 1 )
            fRow += LEAD / 2
         ENDIF

      ENDIF

      IF ( ulTempPos == 1 )
         fRow -= LEAD
      ENDIF
   NEXT
   iRow -= fNumLines + fontSizePoints

   EXIT

CASE 4

   // printf("estou case 3 \n")
   fBoxSize := 0
   fLastRow := fRow

   fTemp := - fCurRow
   FOR each pTemp in pCol
      fBoxSize += RowtoPoint( pTemp )
   NEXT

   setBox( fCol, fRow, fBoxSize, - ( fontSizePoints * 1.5 ) )
   setBox( fCol, fRow - ( fNumLines + 0.5 * fontSizePoints ) + ( 1.5 * fontSizePoints ), fBoxSize, + ( fontSizePoints * 1.5 ) )
   fCol := fOldCol

   FOR EACH pTemp IN pCol
      setBox( fCol, fRow, RowtoPoint( pTemp ), - fCurRow + ( 1.5 * fontSizePoints ) )
      fCol += RowtoPoint( pTemp )
   NEXT

   FOR each pTemp in pArray
      ulTempPos := hb_enumindex()

      IF Isarray( pTemp )
         fRow -= LEAD
         fCol := fOldCol
         FOR each cItem in pTemp
            ulTempPos1 := hb_enumindex()
            IF ulTempPos == Len( pTemp )
               opdf:pdfAtSay( cItem, fRow - ( 0.5 * fontSizePoints ), fCol + 1, .T. )
            ELSEIF ulTempPos == 1
               opdf:pdfAtSay( cItem, fRow, fCol + 1, .T. )
            ELSE
               opdf:pdfAtSay( cItem, fRow - ( 0.5 * fontSizePoints ), fCol + 1, .T. )
            ENDIF
            fCol += RowtoPoint( pCol[ ulTempPos1 ] )
         NEXT
         fRow -= LEAD
      ENDIF

   NEXT
   iRow -= fNumLines + fontSizePoints
   EXIT

CASE 5

   fBoxSize := 0
   FOR each pTemp in pCol
      fBoxSize += RowtoPoint( pTemp )
   NEXT
   setBox( fCol, fRow, fBoxSize, - ( 1.5 * fontSizePoints ) )

   fNumLines -= RowtoPoint( 3 )
   fCurRow   := fNumLines + 0.5 * fontSizePoints
   setBox( fCol, fRow - 1.5 * fontSizePoints, fBoxSize, - fCurRow + ( 2 * fontSizePoints ) )
   fRow -= LEAD

   FOR EACH pTemp in pArray

      ulTempPos := hb_enumindex()

      IF Isarray( ptemp )

         fCol := fOldCol
         FOR each cItem in pTemp
            ulTempPos1 := hb_enumindex()
            opdf:pdfAtSay( cItem, fRow, fCol + 1, .T. )
            fCol += RowtoPoint( pCol[ ulTempPos1 ] )
         NEXT
         fRow -= LEAD
         IF ( ulTempPos == 1 )
            fRow += LEAD / 2
         ENDIF
      ENDIF

      IF ( ulTempPos == 1 )
         fRow -= LEAD
      ENDIF
   NEXT
   iRow -= fNumLines + fontSizePoints
   EXIT

CASE 6

   nLineGen := 0
   fBoxSize := 0

   fTemp := - fCurRow
   fLen  := fRow

   FOR each pTemp in pCol
      fBoxSize += RowtoPoint( pTemp )
   NEXT
   WHILE ( nLineGen < nNumLines )

      setBox( fCol, fLen, fBoxSize, - fontSizePoints * 1.5 )
      fLen     -= fontSizePoints * 1.5
      nLineGen += 2
   ENDDO

   fRow -= LEAD

   FOR each pTemp in pArray
      ulTempPos := hb_enumindex()

      IF Isarray( ptemp )

         fCol := fOldCol

         FOR each cItem in pTemp

            ulTempPos1 := hB_enumIndex()
            opdf:pdfAtSay( cItem, fRow, fCol + 1, .T. )

            fCol += RowtoPoint( pCol[ ulTempPos1 ] )
         NEXT

         fRow -= LEAD

         IF ( ulTempPos == 1 )
            fRow += LEAD / 2
         ELSE
            fRow -= LEAD / 2
         ENDIF
      ENDIF

      IF ( ulTempPos == 1 )
         fRow -= LEAD
      ENDIF

   NEXT
   iRow -= fNumLines + fontSizePoints
   EXIT

   END
RETURN 0

FUNCTION GraphRowtoPoint( thisRow )

RETURN fCurrentRowSetting - ( thisRow * fontSizePoints )

STATIC FUNCTION inverty( y )

RETURN fCurrentRowSetting - y

FUNCTION PDFTEXTBOXED2( pText, pEnd, pStart, lDecRow, lBold )

LOCAL iSize
LOCAL sLastSize := 0
LOCAL ulTempPos := 0
LOCAL cText
   DEFAULT lBold TO .F.

   iSize := PdfMaxBoxSize(  pText, Iif( lBold, 1, 0 ), pEnd, iRow, pStart )

   FOR each cText in pText
      setText(  cText, pStart[ hb_enumindex() ], iRow, iSize, pEnd[ hb_enumindex() ], lBold )
   NEXT

   IF ( lDecRow )
      iRow -= iSize
   ENDIF

   RETURN nil

   /*  FUNCAO PDFNEW( Nome,Tamanho fonte,largura pagina, altura pagina,coluna inicial,linha final,Senhamestre,SenhaNormal, aInfo)
   Parametros:
   Nome  -> Nome do arquivo a ser criado
   Tamanho fonte  -> Tamanho da fonte a ser usada nesse documento
   largura pagina -> Tamanho da largura da pagina
   altura paginas -> Tamanho da altura da pagina
   coluna inicial -> Coluna inicial do texto
   linha final    -> Linha final do texto (
   Senhamestre    -> senha mestre de protecao
   SenhaNormal    -> senha do arquivo
   aInfo          -> Array de caracteres contendo informacoes do arquivo
   Array nesse Formato -> { Keywords,Subject,Title,Creator,Author}
   retorna verdadeiro se o arquivo foi criado, senao falso
*/

FUNCTION Pdfnew( szFileresult, fontsize, iStartRow, iBottom, iStartCol, iEndBottom, szMasterPass, szPass, ainfo )

   DEFAULT fontsize TO fontSizePoints
   DEFAULT iStartRow TO fPageWidth
   DEFAULT iBottom TO fPageHeigth
   DEFAULT iStartCol TO fColumn
   DEFAULT iEndBottom TO fFootBottom
   DEFAULT szMasterPass TO Nil
   DEFAULT szPass TO Nil

   oPdf := TPdf():New( szFileResult,, iStartRow, iBottom )
   IF oPdf == NIL
      RETURN 0
   ENDIF
   /* Ajusta fonte da geração e numero de linhas para pulos */
   fontSizePoints := fontsize
   LEAD           := fontSizePoints

   /* Ajusta Coluna inicial/rodapé e o numero atual de linhas por pagina */
   fPageWidth  := iStartRow
   fPageHeigth := iBottom

   iRow := iStartRow - iEndBottom
   IF ( iRow > 800 )
      iRow := 800
   ENDIF
   NumofRows          := iRow / fontSizePoints
   fCurrentRowSetting := iRow
   fFootBottom        := iEndBottom
   fColumn            := iStartCol

   IF Isarray( aInfo )
      opdf:PDFINFO( aInfo[ 1 ], aInfo[ 5 ], aInfo[ 2 ], aInfo[ 4 ] )
   ENDIF
RETURN 1

FUNCTION PDFEND

   opdf:PDFclose() /* close PDF document   */

   fontSizePoints     := 10.0 /*10.0*/
   LEAD               := 10
   iRow               := 800
   fCurrentRowSetting := 800
   sziFontBold        := 0
   fFootBottom        := 40
   fColumn            := 10
   sziFont            := 0
   fPageWidth         := a4_width
   fPageHeigth        := a4_height
   iPage              := 1
   lLandScape         := .F.

   RETURN nil

   /* PDFSETUNDERLINE( lUnder )
   Parametros
   lUnder -> Flag the informa se e para usar underline ou nao
   retorna o ultimo setting
*/

FUNCTION PDFSETUNDERLINE( lSet )

   RETURN opdf:SetUnderline( lSet )
   /* PDFSETOVERLINE( lUnder )
   Parametros
   lUnder -> Flag the informa se e para usar overline ou nao
   retorna o ultimo setting
*/
FUNCTION PDFSETOVERLINE( l )

   RETURN opdf:SetOverline( l )

   /* FUNCAO PDFSTARTPAGE( aHead,llShowHeader)
PARAMETROS
   aHead     -> Array que contem os cabecalhos das colunas
   llShowHeader -> flag que indica de e para criar uma nova pagina
*/

FUNCTION PDFSTARTPAGE( aPage, bOpenPage )

LOCAL cItem
LOCAL i
   DEFAULT bOpenPage TO .f.

   IF ( bOpenPage )
      opdf:pdfNewPage( "A4", Iif( fPageHeigth < fPageWidth, "P", "L" ),, "Courier", 0 )
   ENDIF
   FOR each citem in aPage
      i := hb_enumindex()

      IF ( i == 1 )
         opdf:PDFsetfont( "Courier", 1, FONTSIZEBOLD )
         opdf:pdfAtSay( cItem, iRow -= 15, fColumn, .T. )

      ELSE

         opdf:PDFsetfont( "Courier", 0, fontSizePoints )
         opdf:pdfAtSay( cItem, iRow -= LEAD, fColumn, .T. )
      ENDIF
   NEXT

   RETURN nil

   /* FUNCAO PDFDRAWPAGE( aPage)
   Parametros
   aPage  -> Array com items a escrever na PDF
   bOnNewPage -> Code block para evaluar se pagina acabar
*/

FUNCTION PDFDRAWPAGE( aPage, bOnNewPage )

LOCAL cItem
LOCAL i

   FOR each cItem in aPage

      /* testa se a pagina acabou */
      IF ( iRow <= fFootBottom )

         iRow := fCurrentRowSetting
         opdf:pdfClosePage()
         iLastLinkPos := 0
         opdf:pdfNewPage( "A4", Iif( fPageHeigth < fPageWidth, "P", "L" ),, "Courier", 0 )
         iPage ++
         IF isblock( bOnNewPage )
            Eval( bOnNewPage )
         ENDIF

      ENDIF
      opdf:PDFsetfont( "Courier", 0, fontSizePoints )
      opdf:pdfAtSay( cItem, iRow -= LEAD, fColumn, .T. )

   NEXT

   RETURN nil

   /* FUNCAO PDFENDPAGE()
   fecha a pagina em criacao
*/

FUNCTION PDFENDPAGE

   iRow := fCurrentRowSetting
   opdf:PDFclosepage() /* close page       */
   iPage ++
   iLastLinkPos := 0
RETURN nil

STATIC FUNCTION SetText( szText, irow, icol, h, iw, bBold )

   opdf:PDFsetfont( "Courier", Iif( bBold, 1, 0 ), fontSizePoints )
   opdf:PDF_rect( irow, icol, iw + 5, h )
   opdf:PDF_stroke()
   //   PDF_show_boxed(p, szText, irow, icol, iw, h, "center", "")
   opdf:pdfText( szText, iRow, iCol, iw, 0, 2 )
RETURN nil

FUNCTION getText( szText, iFont, irow, icol, iw )

LOCAL h
LOCAL w
LOCAL c
   h := fontSizePoints * 2.1
   c := opdf:pdfTextCount( szText, irow, icol, iw, 0, 2 )
   h += fontSizePoints * c
RETURN h

FUNCTION PdfMaxBoxSize( pText, iFont, pEnd, iw, pStart )

LOCAL fMax         := 0
LOCAL fCurrentSize
LOCAL ulTempPos
   FOR ulTempPos := 1 TO Len( pText )

      fCurrentSize := getText( pText[ ulTempPos ], iFont, pStart[ ulTempPos ], iRow, pEnd[ ulTempPos ] )
      fMax         := Iif( fCurrentSize > fMax, fCurrentSize, fMax )
   NEXT
RETURN fMax

FUNCTION PDFHR( fPos, fStart )

   DEFAULT fPos TO Iif( lLandScape, fPageWidth - fColumn, fPageHeigth - fColumn )
   DEFAULT fStart TO fColumn
   opdf:PDFmoveto( fStart * fontSizePoints, iRow - LEAD / 2 )
   opdf:PDFlineto( fPos * fontSizePoints, iRow - LEAD / 2 )
   iRow -= LEAD
   opdf:PDF_stroke( )

   /*
FUNCAO PDFPAGENUMBER()
   Retorna o numero da pagina atual sendo construida
*/

FUNCTION PDFPAGENUMBER

   RETURN iPage

   /* FUNCAO PDFCURRENTLINE()
retorna a linha atual
*/
FUNCTION PDFCURRENTLINE

   RETURN PointtoRow( iRow )

   /* FUNCAO PDFNEWLINE()
incremanta a linha atual
*/

PROCEDURE PDFNEWLINE

   iRow -= LEAD
RETURN
FUNCTION PDFPLACEIMAGE( szfile, row, col, scale_y )

   //    int image;
LOCAL scale_x
   row := RowtoPoint( row )
   col := RowtoPoint( col )
   col := inverty( col )

   scale_x := 0.40
   scale_y := Iif( Isnumber( scale_y ), scale_y / 100, 0.40 )

   opdf:PdfImage( szfile, row / scale_y, col )

RETURN nil
FUNCTION HB_SETLANDSCAPE( lLand )

   lLandScape := lLand
   //   oPdf:pdfPageOrient( Iif( lLand, "L", "P" ) )
RETURN nil

FUNCTION Pdfcomplevel( X )

   opdf:PdfCompLevel( x )
RETURN NIL

   // inline c functions

#pragma begindump
   int Bin2i( BYTE * pszString )
   {

   ULONG ulLen = strlen( pszString ) ;
                         int i = HB_MKSHORT( ( ulLen >= 1 ) ? ( BYTE ) pszString[ 0 ]:0,
   ( ulLen >= 2 ) ? ( BYTE ) pszString[ 1 ] :0 ) ;
     //int i;
   //    i = *((short *) pszString);
   return i ;
    }
   const char szFontTable[ ] = {
   "\xfa\0\xfa\0\xfa\0\xfa\0\x4d\x01\x4d\x01\x4d\x01\x85\x01"
   "\x98\x01\x2b\x02\xa4\x01\x2b\x02\xf4\x01\xf4\x01\xf4\x01\xf4\x01"
   "\xf4\x01\xf4\x01\xf4\x01\xf4\x01\x41\x03\xe8\x03\x41\x03\x41\x03"
   "\x0a\x03\x41\x03\x0a\x03\x0a\x03\x4d\x01\x4d\x01\x4d\x01\x4d\x01"
   "\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01"
   "\xf4\x01\xf4\x01\xf4\x01\xf4\x01\x34\x02\x3a\x02\xa3\x02\x3a\x02"
   "\xfa\0\xfa\0\xfa\0\xfa\0\x4d\x01\x4d\x01\x4d\x01\x4d\x01"
   "\xfa\0\xfa\0\xfa\0\xfa\0\x16\x01\x16\x01\x16\x01\x16\x01"
   "\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01"
   "\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01"
   "\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01"
   "\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01"
   "\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01"
   "\x16\x01\x4d\x01\x4d\x01\x4d\x01\x16\x01\x4d\x01\x4d\x01\x4d\x01"
   "\x34\x02\x3a\x02\xa3\x02\x3a\x02\x34\x02\x3a\x02\xa3\x02\x3a\x02"
   "\x34\x02\x3a\x02\xa3\x02\x3a\x02\xbc\x01\xf4\x01\xf4\x01\xf4\x01"
   "\x99\x03\xa2\x03\x98\x03\x40\x03\xd2\x02\xd2\x02\x63\x02\x9b\x02"
   "\x9b\x02\x9b\x02\x63\x02\x9b\x02\x9b\x02\xd2\x02\x9b\x02\x9b\x02"
   "\xd2\x02\xd2\x02\xd2\x02\xd2\x02\x63\x02\x9b\x02\x63\x02\x9b\x02"
   "\x2c\x02\x63\x02\x63\x02\x9b\x02\xd2\x02\x0a\x03\xd2\x02\xd2\x02"
   "\xd2\x02\x0a\x03\xd2\x02\x0a\x03\x4d\x01\x85\x01\x4d\x01\x85\x01"
   "\x85\x01\xf4\x01\xbc\x01\xf4\x01\xd2\x02\x0a\x03\x9b\x02\x9b\x02"
   "\x63\x02\x9b\x02\x2c\x02\x63\x02\x79\x03\xb0\x03\x41\x03\x79\x03"
   "\xd2\x02\xd2\x02\x9b\x02\xd2\x02\xd2\x02\x0a\x03\xd2\x02\xd2\x02"
   "\x2c\x02\x63\x02\x63\x02\x63\x02\xd2\x02\x0a\x03\xd2\x02\xd2\x02"
   "\x9b\x02\xd2\x02\x63\x02\x9b\x02\x2c\x02\x2c\x02\xf4\x01\x2c\x02"
   "\x63\x02\x9b\x02\x2c\x02\x63\x02\xd2\x02\xd2\x02\xd2\x02\xd2\x02"
   "\xd2\x02\xd2\x02\x63\x02\x9b\x02\xb0\x03\xe8\x03\x41\x03\x79\x03"
   "\xd2\x02\xd2\x02\x63\x02\x9b\x02\xd2\x02\xd2\x02\x2c\x02\x63\x02"
   "\x63\x02\x9b\x02\x2c\x02\x63\x02\x4d\x01\x4d\x01\x85\x01\x4d\x01"
   "\x16\x01\x16\x01\x16\x01\x16\x01\x4d\x01\x4d\x01\x85\x01\x4d\x01"
   "\xd5\x01\x45\x02\xa6\x01\x3a\x02\xf4\x01\xf4\x01\xf4\x01\xf4\x01"
   "\x4d\x01\x4d\x01\x4d\x01\x4d\x01\xbc\x01\xf4\x01\xf4\x01\xf4\x01"
   "\xf4\x01\x2c\x02\xf4\x01\xf4\x01\xbc\x01\xbc\x01\xbc\x01\xbc\x01"
   "\xf4\x01\x2c\x02\xf4\x01\xf4\x01\xbc\x01\xbc\x01\xbc\x01\xbc\x01"
   "\x4d\x01\x4d\x01\x16\x01\x4d\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01"
   "\xf4\x01\x2c\x02\xf4\x01\x2c\x02\x16\x01\x16\x01\x16\x01\x16\x01"
   "\x16\x01\x4d\x01\x16\x01\x16\x01\xf4\x01\x2c\x02\xbc\x01\xf4\x01"
   "\x16\x01\x16\x01\x16\x01\x16\x01\x0a\x03\x41\x03\xd2\x02\x0a\x03"
   "\xf4\x01\x2c\x02\xf4\x01\x2c\x02\xf4\x01\xf4\x01\xf4\x01\xf4\x01"
   "\xf4\x01\x2c\x02\xf4\x01\xf4\x01\xf4\x01\x2c\x02\xf4\x01\xf4\x01"
   "\x4d\x01\xbc\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01\x85\x01"
   "\x16\x01\x4d\x01\x16\x01\x16\x01\xf4\x01\x2c\x02\xf4\x01\x2c\x02"
   "\xf4\x01\xf4\x01\xbc\x01\xbc\x01\xd2\x02\xd2\x02\x9b\x02\x9b\x02"
   "\xf4\x01\xf4\x01\xbc\x01\xf4\x01\xf4\x01\xf4\x01\xbc\x01\xbc\x01"
   "\xbc\x01\xbc\x01\x85\x01\x85\x01\xe0\x01\x8a\x01\x90\x01\x5c\x01"
   "\xc8\0\xdc\0\x13\x01\xdc\0\xe0\x01\x8a\x01\x90\x01\x5c\x01"
   "\x1d\x02\x08\x02\x1d\x02\x3a\x02\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\x4d\x01\x4d\x01\x85\x01\x85\x01"
   "\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01"
   "\xa7\0\xa7\0\xa7\0\xa7\0\xf4\x01\xf4\x01\xf4\x01\xf4\x01"
   "\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01"
   "\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xb4\0\x16\x01\xd6\0\x16\x01"
   "\xbc\x01\xf4\x01\x2c\x02\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01"
   "\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01"
   "\x2c\x02\x2c\x02\xf4\x01\x2c\x02\x2c\x02\x2c\x02\xf4\x01\x2c\x02"
   "\0\0\0\0\0\0\0\0\xf4\x01\xf4\x01\xf4\x01\xf4\x01"
   "\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01"
   "\xfa\0\xfa\0\xfa\0\xfa\0\0\0\0\0\0\0\0\0"
   "\xc5\x01\x1c\x02\x0b\x02\xf4\x01\x5e\x01\x5e\x01\x5e\x01\x5e\x01"
   "\x4d\x01\x4d\x01\x4d\x01\x4d\x01\xbc\x01\xf4\x01\x2c\x02\xf4\x01"
   "\xbc\x01\xf4\x01\x2c\x02\xf4\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01"
   "\xe8\x03\xe8\x03\x79\x03\xe8\x03\xe8\x03\xe8\x03\xe8\x03\xe8\x03"
   "\0\0\0\0\0\0\0\0\xbc\x01\xf4\x01\xf4\x01\xf4\x01"
   "\0\0\0\0\0\0\0\0\x4d\x01\x4d\x01\x4d\x01\x4d\x01"
   "\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01"
   "\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01"
   "\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01"
   "\x4d\x01\x4d\x01\x4d\x01\x4d\x01\0\0\0\0\0\0\0\0"
   "\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01"
   "\0\0\0\0\0\0\0\0\x4d\x01\x4d\x01\x4d\x01\x4d\x01"
   "\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01"
   "\xe8\x03\xe8\x03\x79\x03\xe8\x03\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\x79\x03\xe8\x03\x79\x03\xb0\x03"
   "\0\0\0\0\0\0\0\0\x14\x01\x2c\x01\x14\x01\x0a\x01"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\x63\x02\x9b\x02\x2c\x02\x63\x02\xd2\x02\x0a\x03\xd2\x02\xd2\x02"
   "\x79\x03\xe8\x03\xb0\x03\xb0\x03\x36\x01\x4a\x01\x36\x01\x2c\x01"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\x9b\x02\xd2\x02\x9b\x02\xd2\x02"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\x16\x01\x16\x01\x16\x01\x16\x01"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\x16\x01\x16\x01\x16\x01\x16\x01\xf4\x01\xf4\x01\xf4\x01\xf4\x01"
   "\xd2\x02\xd2\x02\x9b\x02\xd2\x02\xf4\x01\x2c\x02\xf4\x01\xf4\x01"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\x16\x01\x16\x01\x16\x01\x16\x01\x16\x01\x4d\x01\x16\x01\x4d\x01"
   "\x63\x01\xda\x01\x63\x01\xda\x01\x2c\x02\x2c\x02\x2c\x02\x2c\x02"
   "\x2c\x02\x2c\x02\x2c\x02\x2c\x02\x79\x03\x79\x03\x79\x03\x79\x03"
   "\x9b\x02\xd2\x02\x9b\x02\xd2\x02\xde\0\x16\x01\xde\0\x16\x01"
   "\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01"
   "\x85\x01\x85\x01\x85\x01\x85\x01\x48\x02\x48\x02\x48\x02\x48\x02"
   "\x16\x01\x16\x01\x16\x01\x16\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01"
   "\x16\x01\x16\x01\x16\x01\x16\x01\x16\x01\x16\x01\x16\x01\x16\x01"
   "\x2c\x02\x2c\x02\x2c\x02\x2c\x02\x2c\x02\x2c\x02\x2c\x02\x2c\x02"
   "\x2c\x02\x2c\x02\x2c\x02\x2c\x02\x2c\x02\x2c\x02\x2c\x02\x2c\x02"
   "\x2c\x02\x2c\x02\x2c\x02\x2c\x02\x2c\x02\x2c\x02\x2c\x02\x2c\x02"
   "\x2c\x02\x2c\x02\x2c\x02\x2c\x02\x2c\x02\x2c\x02\x2c\x02\x2c\x02"
   "\x2c\x02\x2c\x02\x2c\x02\x2c\x02\x2c\x02\x2c\x02\x2c\x02\x2c\x02"
   "\x16\x01\x4d\x01\x16\x01\x4d\x01\x16\x01\x4d\x01\x16\x01\x4d\x01"
   "\x48\x02\x48\x02\x48\x02\x48\x02\x48\x02\x48\x02\x48\x02\x48\x02"
   "\x48\x02\x48\x02\x48\x02\x48\x02\x2c\x02\x63\x02\x2c\x02\x63\x02"
   "\xf7\x03\xcf\x03\xf7\x03\xcf\x03\x9b\x02\xd2\x02\x9b\x02\xd2\x02"
   "\x9b\x02\xd2\x02\x9b\x02\xd2\x02\xd2\x02\xd2\x02\xd2\x02\xd2\x02"
   "\xd2\x02\xd2\x02\xd2\x02\xd2\x02\x9b\x02\x9b\x02\x9b\x02\x9b\x02"
   "\x63\x02\x63\x02\x63\x02\x63\x02\x0a\x03\x0a\x03\x0a\x03\x0a\x03"
   "\xd2\x02\xd2\x02\xd2\x02\xd2\x02\x16\x01\x16\x01\x16\x01\x16\x01"
   "\xf4\x01\x2c\x02\xf4\x01\x2c\x02\x9b\x02\xd2\x02\x9b\x02\xd2\x02"
   "\x2c\x02\x63\x02\x2c\x02\x63\x02\x41\x03\x41\x03\x41\x03\x41\x03"
   "\xd2\x02\xd2\x02\xd2\x02\xd2\x02\x0a\x03\x0a\x03\x0a\x03\x0a\x03"
   "\x9b\x02\x9b\x02\x9b\x02\x9b\x02\x0a\x03\x0a\x03\x0a\x03\x0a\x03"
   "\xd2\x02\xd2\x02\xd2\x02\xd2\x02\x9b\x02\x9b\x02\x9b\x02\x9b\x02"
   "\x63\x02\x63\x02\x63\x02\x63\x02\xd2\x02\xd2\x02\xd2\x02\xd2\x02"
   "\x9b\x02\x9b\x02\x9b\x02\x9b\x02\xb0\x03\xb0\x03\xb0\x03\xb0\x03"
   "\x9b\x02\x9b\x02\x9b\x02\x9b\x02\x9b\x02\x9b\x02\x9b\x02\x9b\x02"
   "\x63\x02\x63\x02\x63\x02\x63\x02\x16\x01\x4d\x01\x16\x01\x4d\x01"
   "\x16\x01\x16\x01\x16\x01\x16\x01\x16\x01\x4d\x01\x16\x01\x4d\x01"
   "\xd5\x01\x48\x02\xd5\x01\x48\x02\x2c\x02\x2c\x02\x2c\x02\x2c\x02"
   "\xde\0\x16\x01\xde\0\x16\x01\x2c\x02\x2c\x02\x2c\x02\x2c\x02"
   "\x2c\x02\x63\x02\x2c\x02\x63\x02\xf4\x01\x2c\x02\xf4\x01\x2c\x02"
   "\x2c\x02\x63\x02\x2c\x02\x63\x02\x2c\x02\x2c\x02\x2c\x02\x2c\x02"
   "\x16\x01\x4d\x01\x16\x01\x4d\x01\x2c\x02\x63\x02\x2c\x02\x63\x02"
   "\x2c\x02\x63\x02\x2c\x02\x63\x02\xde\0\x16\x01\xde\0\x16\x01"
   "\xde\0\x16\x01\xde\0\x16\x01\xf4\x01\x2c\x02\xf4\x01\x2c\x02"
   "\xde\0\x16\x01\xde\0\x16\x01\x41\x03\x79\x03\x41\x03\x79\x03"
   "\x2c\x02\x63\x02\x2c\x02\x63\x02\x2c\x02\x63\x02\x2c\x02\x63\x02"
   "\x2c\x02\x63\x02\x2c\x02\x63\x02\x2c\x02\x63\x02\x2c\x02\x63\x02"
   "\x4d\x01\x85\x01\x4d\x01\x85\x01\xf4\x01\x2c\x02\xf4\x01\x2c\x02"
   "\x16\x01\x4d\x01\x16\x01\x4d\x01\x2c\x02\x63\x02\x2c\x02\x63\x02"
   "\xf4\x01\x2c\x02\xf4\x01\x2c\x02\xd2\x02\x0a\x03\xd2\x02\x0a\x03"
   "\xf4\x01\x2c\x02\xf4\x01\x2c\x02\xf4\x01\x2c\x02\xf4\x01\x2c\x02"
   "\xf4\x01\xf4\x01\xf4\x01\xf4\x01\x4e\x01\x85\x01\x4e\x01\x85\x01"
   "\x04\x01\x18\x01\x04\x01\x18\x01\x4e\x01\x85\x01\x4e\x01\x85\x01"
   "\x48\x02\x48\x02\x48\x02\x48\x02\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\x4d\x01\x4d\x01\x4d\x01\x4d\x01"
   "\x2c\x02\x2c\x02\x2c\x02\x2c\x02\x2c\x02\x2c\x02\x2c\x02\x2c\x02"
   "\xa7\0\xa7\0\xa7\0\xa7\0\x2c\x02\x2c\x02\x2c\x02\x2c\x02"
   "\x2c\x02\x2c\x02\x2c\x02\x2c\x02\x2c\x02\x2c\x02\x2c\x02\x2c\x02"
   "\x2c\x02\x2c\x02\x2c\x02\x2c\x02\xbf\0\xee\0\xbf\0\xee\0"
   "\x4d\x01\xf4\x01\x4d\x01\xf4\x01\x2c\x02\x2c\x02\x2c\x02\x2c\x02"
   "\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01"
   "\xf4\x01\x63\x02\xf4\x01\x63\x02\xf4\x01\x63\x02\xf4\x01\x63\x02"
   "\0\0\0\0\0\0\0\0\x2c\x02\x2c\x02\x2c\x02\x2c\x02"
   "\x2c\x02\x2c\x02\x2c\x02\x2c\x02\x2c\x02\x2c\x02\x2c\x02\x2c\x02"
   "\x16\x01\x16\x01\x16\x01\x16\x01\0\0\0\0\0\0\0\0"
   "\x19\x02\x2c\x02\x19\x02\x2c\x02\x5e\x01\x5e\x01\x5e\x01\x5e\x01"
   "\xde\0\x16\x01\xde\0\x16\x01\x4d\x01\xf4\x01\x4d\x01\xf4\x01"
   "\x4d\x01\xf4\x01\x4d\x01\xf4\x01\x2c\x02\x2c\x02\x2c\x02\x2c\x02"
   "\xe8\x03\xe8\x03\xe8\x03\xe8\x03\xe8\x03\xe8\x03\xe8\x03\xe8\x03"
   "\0\0\0\0\0\0\0\0\x63\x02\x63\x02\x63\x02\x63\x02"
   "\0\0\0\0\0\0\0\0\x4d\x01\x4d\x01\x4d\x01\x4d\x01"
   "\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01"
   "\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01"
   "\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01"
   "\x4d\x01\x4d\x01\x4d\x01\x4d\x01\0\0\0\0\0\0\0\0"
   "\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01"
   "\0\0\0\0\0\0\0\0\x4d\x01\x4d\x01\x4d\x01\x4d\x01"
   "\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01\x4d\x01"
   "\xe8\x03\xe8\x03\xe8\x03\xe8\x03\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\xe8\x03\xe8\x03\xe8\x03\xe8\x03"
   "\0\0\0\0\0\0\0\0\x72\x01\x72\x01\x72\x01\x72\x01"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\x2c\x02\x63\x02\x2c\x02\x63\x02\x0a\x03\x0a\x03\x0a\x03\x0a\x03"
   "\xe8\x03\xe8\x03\xe8\x03\xe8\x03\x6d\x01\x6d\x01\x6d\x01\x6d\x01"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\x79\x03\x79\x03\x79\x03\x79\x03"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\x16\x01\x16\x01\x16\x01\x16\x01"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\xde\0\x16\x01\xde\0\x16\x01\x63\x02\x63\x02\x63\x02\x63\x02"
   "\xb0\x03\xb0\x03\xb0\x03\xb0\x03\x63\x02\x63\x02\x63\x02\x63\x02"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   "\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02\x58\x02"
   } ;
           const int szFontTable_size = 5376 ;

   HB_FUNC( CALCDATA )
   {
   int rowsperstrip = hb_parnd( 1 ) ;
                                float y = hb_parnd( 2 ) ;
                                float h = hb_parnd( 3 ) ;
                                int row, res ;
                                int p ;
                                for ( row = 0 ;   row < h ;   row += rowsperstrip )
   {
   int height ;
           height = row + rowsperstrip > h ? h - row :rowsperstrip ;
           p = ( h - row - height ) ;
           res = y + 1 * p ;
           }
   hb_retni( res ) ;
             }
#pragma enddump

   /*FUNCAO PDFBOXCOLOR(nRed,nGreen,nBlue)
   Parametros
   nRed   -> Quantidade de vermelho na composicao da cor ( valor entre 0 a 255)
   nGreen -> Quantidade de verde na composicao da cor    ( valor entre 0 a 255)
   nBlue  -> Quantidade de azul na composicao da cor     ( valor entre 0 a 255)
*/

FUNCTION PDFBOXCOLOR( R, G, B )

   oPdf:PDFsetrgbcolor( R / 255, G / 255, B / 255 )
RETURN NIL

FUNCTION PDFTEXTCOLOR( R, G, B )

   oPdf:PDFsetrgbcolor( R / 255, G / 255, B / 255 )
RETURN NIL

