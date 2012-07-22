/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2011 Fausto Di Creddo Trautwein, ftwein@yahoo.com.br
 * www - http://www.xharbour.org http://harbour-project.org
 *
 * Thanks TO Robert F Greer, PHP original version
 * http://sourceforge.net/projects/excelwriterxml/
 *
 * This program is free software; you can redistribute it AND/OR modify
 * it under the terms of the GNU General PUBLIC License as published by
 * the Free Software Foundation; either version 2, OR( at your option )
 * any later version.
 *
 * This program is distributed IN the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General PUBLIC License FOR more details.
 *
 * You should have received a copy of the GNU General PUBLIC License
 * along WITH this software; see the file COPYING.  IF NOT, write TO
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA( OR visit the web site http://www.gnu.org/ ).
 *
 * As a special exception, the Harbour Project gives permission FOR
 * additional uses of the text contained IN its release of Harbour.
 *
 * The exception is that, IF you link the Harbour libraries WITH other
 * files TO produce an executable, this does NOT by itself cause the
 * resulting executable TO be covered by the GNU General PUBLIC License.
 * Your use of that executable is IN no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does NOT however invalidate any other reasons why
 * the executable file might be covered by the GNU General PUBLIC License.
 *
 * This exception applies only TO the code released by the Harbour
 * Project under the name Harbour.  IF you copy code FROM other
 * Harbour Project OR Free Software Foundation releases into a copy of
 * Harbour, as the General PUBLIC License permits, the exception does
 * NOT apply TO the code that you add IN this way.  TO avoid misleading
 * anyone as TO the status of such modified files, you must delete
 * this exception notice FROM them.
 *
 * IF you write modifications of your own FOR Harbour, it is your choice
 * whether TO permit this exception TO apply TO your modifications.
 * IF you DO NOT wish that, delete this exception notice.
 *
 */
/*----------------------------------------------------------------------*/

FUNCTION main()
   LOCAL oXml, oSheet, xarquivo := "example.xml"
   LOCAL i, xqtddoc, xttotnot, xtbascal, xtvlricm, xtbasipi, xtvlripi, aDoc, nLinha
   LOCAL xEmpresa
   LOCAL xDataImp
   LOCAL xTitulo
   LOCAL xPeriodo
   LOCAL xOrdem

   SET DATE ANSI

   oXml:= ExcelWriterXML():New(xarquivo)
   oXml:setOverwriteFile(.t.)

   WITH OBJECT oXml:addStyle('textLeft')
      :alignHorizontal('Left')
      :alignVertical('Center')
      :fontSize(10)
   END WITH

   WITH OBJECT oXml:addStyle('textLeftWrap')
      :alignHorizontal('Left')
      :alignVertical('Center')
      :alignWraptext()
      :fontSize(10)
   END WITH
   WITH OBJECT oXml:addStyle('textLeftBold')
      :alignHorizontal('Left')
      :alignVertical('Center')
      :fontSize(10)
      :setFontBold()
   END WITH

   WITH OBJECT oXml:addStyle('textLeftBoldCor')
      :alignHorizontal('Left')
      :alignVertical('Center')
      :fontSize(10)
      :setFontBold()
      :bgColor('lightblue')
      :alignWraptext()
   END WITH

   WITH OBJECT oXml:addStyle('textRight')
      :alignHorizontal('Right')
      :alignVertical('Center')
      :fontSize(10)
   END WITH

   WITH OBJECT oXml:addStyle('textRightBold')
      :alignHorizontal('Right')
      :alignVertical('Center')
      :fontSize(10)
      :setFontBold()
   END WITH

   WITH OBJECT oXml:addStyle('textRightBoldCor')
      :alignHorizontal('Right')
      :alignVertical('Center')
      :fontSize(10)
      :setFontBold()
      :bgColor('lightblue')
      :alignWraptext()
   END WITH

   WITH OBJECT oXml:addStyle('numberRight')
      :alignHorizontal('Right')
      :alignVertical('Center')
      :setNumberFormat('#,##0.00')
      :fontSize(10)
   END WITH

   WITH OBJECT oXml:addStyle('numberRightBold')
      :alignHorizontal('Right')
      :alignVertical('Center')
      :setNumberFormat('#,##0.00')
      :fontSize(10)
      :setFontBold()
   END WITH

   WITH OBJECT oXml:addStyle('numberRightBoldCor')
      :alignHorizontal('Right')
      :alignVertical('Center')
      :setNumberFormat('#,##0.00')
      :fontSize(10)
      :setFontBold()
      :bgColor('lightblue')
   END WITH

   WITH OBJECT oXml:addStyle('numberRightZero')
      :alignHorizontal('Right')
      :alignVertical('Center')
      :setNumberFormat('#,##0.00;[Red]-#,##0.00;;@') //'#,###.00')
      :fontSize(10)
      :setFontBold()
   END WITH

   WITH OBJECT oXml:addStyle('Cabec')
      :alignHorizontal('Left')
      :alignVertical('Center')
      :fontSize(12)
      :setFontBold()
   END WITH

   WITH OBJECT oXml:addStyle('CabecRight')
      :alignHorizontal('Right')
      :alignVertical('Center')
      :fontSize(12)
      :setFontBold()
   END WITH

   oSheet := oXml:addSheet('Plan1')

   WITH OBJECT oSheet
      :columnWidth( 1,  70 ) // N.Fiscal
      :columnWidth( 2,  20 ) // TM
      :columnWidth( 3,  70 ) // Data Movto
      :columnWidth( 4,  70 ) // Data Emis.
      :columnWidth( 5,  50 ) // CFOP
      :columnWidth( 6,  50 ) // Cod. Cliente/Fornecedor
      :columnWidth( 7, 300 ) // Nome Cliente/Fornecedor
      :columnWidth( 8,  20 ) // UF
      :columnWidth( 9,  80 ) // Vlr.Tot.
      :columnWidth(10,  80 ) // Base Calc.
      :columnWidth(11,  80 ) // Vlr ICMS
      :columnWidth(12,  80 ) // Base IPI
      :columnWidth(13,  80 ) // Valor IPI

      xEmpresa:= "EMPRESA DEMONSTRACAO LTDA"
      xDataImp:= "22.03.2011"
      xTitulo := "RELATORIO PARA DEMONSTRAR XML EXCEL"
      xPeriodo:= "01.02.2011 a 28.02.2011"
      xOrdem  := "DATA DE EMISSAO"

      nLinha:= 0

      :writeString(++nLinha,1,xEmpresa ,'Cabec')
      :cellMerge(    nLinha,1, 5, 0)
      :writeString(  nLinha,12,"Data:"+xDataImp ,'CabecRight')
      :cellMerge(    nLinha,12, 1, 0)
      :writeString(++nLinha,1,xTitulo  ,'Cabec')
      :cellMerge(    nLinha,1, 5, 0)
      :writeString(++nLinha,1,xPeriodo ,'Cabec')
      :cellMerge(    nLinha,1, 5, 0)
      :writeString(++nLinha,1,xOrdem   ,'Cabec')
      :cellMerge(    nLinha,1, 5, 0)
   END WITH

   WITH OBJECT oSheet
      :writeString(++nLinha, 1,"N.Fiscal"          ,'textLeftBoldCor' )
      :writeString(  nLinha, 2,"TM"                ,'textLeftBoldCor' )
      :writeString(  nLinha, 3,"Data Movto"        ,'textLeftBoldCor' )
      :writeString(  nLinha, 4,"Data Emis."        ,'textLeftBoldCor' )
      :writeString(  nLinha, 5,"CFOP"              ,'textLeftBoldCor' )
      :writeString(  nLinha, 6,"Codigo"            ,'textLeftBoldCor' )
      :writeString(  nLinha, 7,"Cliente/Fornecedor",'textLeftBoldCor' )
      :writeString(  nLinha, 8,"UF"                ,'textLeftBoldCor' )
      :writeString(  nLinha, 9,"Vlr.Tot."          ,'textRightBoldCor')
      :writeString(  nLinha,10,"Base Calc."        ,'textRightBoldCor')
      :writeString(  nLinha,11,"Vlr ICMS"          ,'textRightBoldCor')
      :writeString(  nLinha,12,"Base IPI"          ,'textRightBoldCor')
      :writeString(  nLinha,13,"Valor IPI"         ,'textRightBoldCor')
   END WITH

   aDoc:= {}
   FOR i:= 1 TO 40
      AADD( aDoc,;
           { STRZERO(i,8),;
             "VE",;
             DATE()-49-i,;
             DATE()-50-i,;
             "5.102",;
             STRZERO(i,5),;
             "NOME DO CLIENTE TESTE "+ALLTRIM(STR(i,5,0)),;
             "PR",;
             i*100,;
             i*100*0.90,;
             i*100*0.90*0.12,;
             i*100,;
             i*100*0.10 } )
   NEXT

   xqtddoc:= xttotnot:= xtbascal:= xtvlricm:= xtbasipi:= xtvlripi:= 0

   FOR i:= 1 TO 40
      WITH OBJECT oSheet
         :writeString(++nLinha, 1,aDoc[i,1],'textLeft')
         :writeString(  nLinha, 2,aDoc[i,2],'textLeft')
         :writeString(  nLinha, 3,DTOC(aDoc[i,3]),'textLeft')
         :writeString(  nLinha, 4,DTOC(aDoc[i,4]),'textLeft')
         :writeString(  nLinha, 5,aDoc[i,5],'textLeft')
         :writeString(  nLinha, 6,aDoc[i,6],'textLeft')
         :writeString(  nLinha, 7,aDoc[i,7],'textLeft')
         :writeString(  nLinha, 8,aDoc[i,8],'textLeft')
         :writeNumber(  nLinha, 9,aDoc[i,9],'numberRight')
         :writeNumber(  nLinha,10,aDoc[i,10],'numberRight')
         :writeNumber(  nLinha,11,aDoc[i,11],'numberRight')
         :writeNumber(  nLinha,12,aDoc[i,12],'numberRight')
         :writeNumber(  nLinha,13,aDoc[i,13],'numberRight')
      END WITH
      xqtddoc++
      xttotnot+= aDoc[i,9]
      xtbascal+= aDoc[i,10]
      xtvlricm+= aDoc[i,11]
      xtbasipi+= aDoc[i,12]
      xtvlripi+= aDoc[i,13]
   NEXT

   WITH OBJECT oSheet
      :writeString(++nLinha, 1,"",'textLeft')
      :writeString(  nLinha, 2,"",'textLeft')
      :writeString(  nLinha, 3,"",'textLeft')
      :writeString(  nLinha, 4,"",'textLeft')
      :writeString(  nLinha, 5,"",'textLeft')
      :writeString(  nLinha, 6,"",'textLeft')
      :writeString(  nLinha, 7,"TOTAL ==> "+STR(xqtddoc,5)+" documentos",'textLeftBold')
      :writeString(  nLinha, 8,"",'textLeft')
      :writeFormula('Number',nLinha,9,'=SUM(R[-40]C:R[-1]C)','numberRightBold')
      //:writeNumber(  nLinha, 9,xttotnot,'numberRightBold')
      :writeNumber(  nLinha,10,xtbascal,'numberRightBold')
      :writeNumber(  nLinha,11,xtvlricm,'numberRightBold')
      :writeNumber(  nLinha,12,xtbasipi,'numberRightBold')
      :writeNumber(  nLinha,13,xtvlripi,'numberRightBold')
   END WITH

   oXml:writeData(xarquivo)

   RETURN NIL

/*----------------------------------------------------------------------*/
