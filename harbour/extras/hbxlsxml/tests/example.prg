/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2011 Fausto Di Creddo Trautwein, ftwein@yahoo.com.br
 * www - http://harbour-project.org
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
 * along WITH this software; see the file COPYING.txt.  IF NOT, write TO
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

#require "hbxlsxml"

PROCEDURE Main()

   LOCAL oXml, oSheet, xarquivo := "example.xml"
   LOCAL i, xqtddoc, xttotnot, xtbascal, xtvlricm, xtbasipi, xtvlripi, aDoc, nLinha
   LOCAL xEmpresa
   LOCAL xDataImp
   LOCAL xTitulo
   LOCAL xPeriodo
   LOCAL xOrdem
   LOCAL oObj

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   oXml := ExcelWriterXML():New( xarquivo )
   oXml:setOverwriteFile( .T. )

   oObj := oXml:addStyle( "textLeft" )
   oObj:alignHorizontal( "Left" )
   oObj:alignVertical( "Center" )
   oObj:fontSize( 10 )

   oObj := oXml:addStyle( "textLeftWrap" )
   oObj:alignHorizontal( "Left" )
   oObj:alignVertical( "Center" )
   oObj:alignWraptext()
   oObj:fontSize( 10 )

   oObj := oXml:addStyle( "textLeftBold" )
   oObj:alignHorizontal( "Left" )
   oObj:alignVertical( "Center" )
   oObj:fontSize( 10 )
   oObj:setFontBold()

   oObj := oXml:addStyle( "textLeftBoldCor" )
   oObj:alignHorizontal( "Left" )
   oObj:alignVertical( "Center" )
   oObj:fontSize( 10 )
   oObj:setFontBold()
   oObj:bgColor( "lightblue" )
   oObj:alignWraptext()

   oObj := oXml:addStyle( "textRight" )
   oObj:alignHorizontal( "Right" )
   oObj:alignVertical( "Center" )
   oObj:fontSize( 10 )

   oObj := oXml:addStyle( "textRightBold" )
   oObj:alignHorizontal( "Right" )
   oObj:alignVertical( "Center" )
   oObj:fontSize( 10 )
   oObj:setFontBold()

   oObj := oXml:addStyle( "textRightBoldCor" )
   oObj:alignHorizontal( "Right" )
   oObj:alignVertical( "Center" )
   oObj:fontSize( 10 )
   oObj:setFontBold()
   oObj:bgColor( "lightblue" )
   oObj:alignWraptext()

   oObj := oXml:addStyle( "numberRight" )
   oObj:alignHorizontal( "Right" )
   oObj:alignVertical( "Center" )
   oObj:setNumberFormat( "#,##0.00" )
   oObj:fontSize( 10 )

   oObj := oXml:addStyle( "numberRightBold" )
   oObj:alignHorizontal( "Right" )
   oObj:alignVertical( "Center" )
   oObj:setNumberFormat( "#,##0.00" )
   oObj:fontSize( 10 )
   oObj:setFontBold()

   oObj := oXml:addStyle( "numberRightBoldCor" )
   oObj:alignHorizontal( "Right" )
   oObj:alignVertical( "Center" )
   oObj:setNumberFormat( "#,##0.00" )
   oObj:fontSize( 10 )
   oObj:setFontBold()
   oObj:bgColor( "lightblue" )

   oObj := oXml:addStyle( "numberRightZero" )
   oObj:alignHorizontal( "Right" )
   oObj:alignVertical( "Center" )
   oObj:setNumberFormat( "#,##0.00;[Red]-#,##0.00;;@" ) //"#,###.00")
   oObj:fontSize( 10 )
   oObj:setFontBold()

   oObj := oXml:addStyle( "Cabec" )
   oObj:alignHorizontal( "Left" )
   oObj:alignVertical( "Center" )
   oObj:fontSize( 12 )
   oObj:setFontBold()

   oObj := oXml:addStyle( "CabecRight" )
   oObj:alignHorizontal( "Right" )
   oObj:alignVertical( "Center" )
   oObj:fontSize( 12 )
   oObj:setFontBold()

   oSheet := oXml:addSheet( "Plan1" )

   oObj := oSheet
   oObj:columnWidth(  1,  70 ) // N.Fiscal
   oObj:columnWidth(  2,  20 ) // TM
   oObj:columnWidth(  3,  70 ) // Data Movto
   oObj:columnWidth(  4,  70 ) // Data Emis.
   oObj:columnWidth(  5,  50 ) // CFOP
   oObj:columnWidth(  6,  50 ) // Cod. Cliente/Fornecedor
   oObj:columnWidth(  7, 300 ) // Nome Cliente/Fornecedor
   oObj:columnWidth(  8,  20 ) // UF
   oObj:columnWidth(  9,  80 ) // Vlr.Tot.
   oObj:columnWidth( 10,  80 ) // Base Calc.
   oObj:columnWidth( 11,  80 ) // Vlr ICMS
   oObj:columnWidth( 12,  80 ) // Base IPI
   oObj:columnWidth( 13,  80 ) // Valor IPI

   xEmpresa := "EMPRESA DEMONSTRACAO LTDA"
   xDataImp := "2011-03-22"
   xTitulo := "RELATORIO PARA DEMONSTRAR XML EXCEL"
   xPeriodo := "2011-02-01 a 2011-02-28"
   xOrdem  := "DATA DE EMISSAO"

   nLinha := 0

   oObj:writeString( ++nLinha, 1, xEmpresa , "Cabec" )
   oObj:cellMerge(     nLinha, 1, 5, 0 )
   oObj:writeString(   nLinha, 12, "Data:" + xDataImp , "CabecRight" )
   oObj:cellMerge(     nLinha, 12, 1, 0 )
   oObj:writeString( ++nLinha, 1, xTitulo  , "Cabec" )
   oObj:cellMerge(     nLinha, 1, 5, 0 )
   oObj:writeString( ++nLinha, 1, xPeriodo , "Cabec" )
   oObj:cellMerge(     nLinha, 1, 5, 0 )
   oObj:writeString( ++nLinha, 1, xOrdem   , "Cabec" )
   oObj:cellMerge(     nLinha, 1, 5, 0 )

   oObj := oSheet
   oObj:writeString( ++nLinha,  1, "N.Fiscal"          , "textLeftBoldCor" )
   oObj:writeString(   nLinha,  2, "TM"                , "textLeftBoldCor" )
   oObj:writeString(   nLinha,  3, "Data Movto"        , "textLeftBoldCor" )
   oObj:writeString(   nLinha,  4, "Data Emis."        , "textLeftBoldCor" )
   oObj:writeString(   nLinha,  5, "CFOP"              , "textLeftBoldCor" )
   oObj:writeString(   nLinha,  6, "Codigo"            , "textLeftBoldCor" )
   oObj:writeString(   nLinha,  7, "Cliente/Fornecedor", "textLeftBoldCor" )
   oObj:writeString(   nLinha,  8, "UF"                , "textLeftBoldCor" )
   oObj:writeString(   nLinha,  9, "Vlr.Tot."          , "textRightBoldCor" )
   oObj:writeString(   nLinha, 10, "Base Calc."        , "textRightBoldCor" )
   oObj:writeString(   nLinha, 11, "Vlr ICMS"          , "textRightBoldCor" )
   oObj:writeString(   nLinha, 12, "Base IPI"          , "textRightBoldCor" )
   oObj:writeString(   nLinha, 13, "Valor IPI"         , "textRightBoldCor" )

   aDoc := {}
   FOR i := 1 TO 40
      AAdd( aDoc, ;
         { StrZero( i, 8 ), ;
         "VE", ;
         Date() - 49 - i, ;
         Date() - 50 - i, ;
         "5.102", ;
         StrZero( i, 5 ), ;
         "NOME DO CLIENTE TESTE " + hb_ntos( i ), ;
         "PR", ;
         i * 100, ;
         i * 100 * 0.90, ;
         i * 100 * 0.90 * 0.12, ;
         i * 100, ;
         i * 100 * 0.10 } )
   NEXT

   xqtddoc := xttotnot := xtbascal := xtvlricm := xtbasipi := xtvlripi := 0

   FOR i := 1 TO 40
      oObj := oSheet
      oObj:writeString( ++nLinha, 1, aDoc[ i, 1 ], "textLeft" )
      oObj:writeString( nLinha, 2, aDoc[ i, 2 ], "textLeft" )
      oObj:writeString( nLinha, 3, DToC( aDoc[ i, 3 ] ), "textLeft" )
      oObj:writeString( nLinha, 4, DToC( aDoc[ i, 4 ] ), "textLeft" )
      oObj:writeString( nLinha, 5, aDoc[ i, 5 ], "textLeft" )
      oObj:writeString( nLinha, 6, aDoc[ i, 6 ], "textLeft" )
      oObj:writeString( nLinha, 7, aDoc[ i, 7 ], "textLeft" )
      oObj:writeString( nLinha, 8, aDoc[ i, 8 ], "textLeft" )
      oObj:writeNumber( nLinha, 9, aDoc[ i, 9 ], "numberRight" )
      oObj:writeNumber( nLinha, 10, aDoc[ i, 10 ], "numberRight" )
      oObj:writeNumber( nLinha, 11, aDoc[ i, 11 ], "numberRight" )
      oObj:writeNumber( nLinha, 12, aDoc[ i, 12 ], "numberRight" )
      oObj:writeNumber( nLinha, 13, aDoc[ i, 13 ], "numberRight" )

      xqtddoc++
      xttotnot += aDoc[ i, 9 ]
      xtbascal += aDoc[ i, 10 ]
      xtvlricm += aDoc[ i, 11 ]
      xtbasipi += aDoc[ i, 12 ]
      xtvlripi += aDoc[ i, 13 ]
   NEXT

   oObj := oSheet
   oObj:writeString( ++nLinha,  1, "", "textLeft" )
   oObj:writeString(   nLinha,  2, "", "textLeft" )
   oObj:writeString(   nLinha,  3, "", "textLeft" )
   oObj:writeString(   nLinha,  4, "", "textLeft" )
   oObj:writeString(   nLinha,  5, "", "textLeft" )
   oObj:writeString(   nLinha,  6, "", "textLeft" )
   oObj:writeString(   nLinha,  7, "TOTAL ==> " + hb_ntos( xqtddoc ) + " document(s)", "textLeftBold" )
   oObj:writeString(   nLinha,  8, "", "textLeft" )
   oObj:writeFormula( "Number", nLinha, 9, "=SUM(R[-40]C:R[-1]C)", "numberRightBold" )
#if 0
   oObj:writeNumber(   nLinha,  9, xttotnot, "numberRightBold" )
#endif
   oObj:writeNumber(   nLinha, 10, xtbascal, "numberRightBold" )
   oObj:writeNumber(   nLinha, 11, xtvlricm, "numberRightBold" )
   oObj:writeNumber(   nLinha, 12, xtbasipi, "numberRightBold" )
   oObj:writeNumber(   nLinha, 13, xtvlripi, "numberRightBold" )

   oXml:writeData( xarquivo )

   RETURN
