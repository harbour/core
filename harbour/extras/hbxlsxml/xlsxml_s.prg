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


#include "hbclass.ch"

CREATE CLASS ExcelWriterXML_Sheet

   VAR    id
   VAR    cells                                   INIT { => }
   VAR    colWidth                                INIT { => }
   VAR    rowHeight                               INIT { => }
   VAR    URLs                                    INIT { => }
   VAR    mergeCells                              INIT { => }
   VAR    comments                                INIT { => }
   VAR    formatErrors                            INIT { => }
   VAR    ldisplayRightToLeft                     INIT .F.

   METHOD new( id )
   METHOD getID()
   METHOD addError( cFunction, cMessage )
   METHOD getErrors()
   METHOD writeFormula( dataType, row, column, xData, style )
   METHOD writeString( row, column, xData, style )
   METHOD writeNumber( row, column, xData, style )
   METHOD writeDateTime( row, column, xData, style )
   METHOD writeData( type, row, column, xData, style, formula )
   METHOD displayRightToLeft()
   METHOD getSheetXML( handle )
   METHOD cellWidth( row, col, width )
   METHOD columnWidth( col, width )
   METHOD cellHeight( row, col, height )
   METHOD setRowHeight( row, height )
   METHOD cellMerge( row, col, width, height )
   METHOD addComment( row, col, comment, author )

ENDCLASS

METHOD ExcelWriterXML_Sheet:new( id )

   ::id := id

   RETURN SELF

METHOD ExcelWriterXML_Sheet:getID()

   RETURN ::id

METHOD ExcelWriterXML_Sheet:addError( cFunction, cMessage )

   LOCAL tmp

   tmp := { "sheet"      => ::id, ;
      "FUNCTION"   => cFunction, ;
      "MESSAGE"    => cMessage }

   ::formatErrors += tmp

   RETURN NIL

METHOD ExcelWriterXML_Sheet:getErrors()

   RETURN ::formatErrors

METHOD ExcelWriterXML_Sheet:writeFormula( dataType, row, column, xData, style )

   HB_SYMBOL_UNUSED( dataType )

   ::writeData( "String", row, column, "", style, xData )

   RETURN NIL

METHOD ExcelWriterXML_Sheet:writeString( row, column, xData, style )

   ::writeData( "String", row, column, xData, style )

   RETURN NIL

METHOD ExcelWriterXML_Sheet:writeNumber( row, column, xData, style )

   IF ! HB_ISNUMERIC( xData )
      ::writeData( "String", row, column, xData, style )
   ELSE
      ::writeData( "Number", row, column, AllTrim( Str( xData, 18, 6 ) ), style )
   ENDIF

   RETURN NIL

METHOD ExcelWriterXML_Sheet:writeDateTime( row, column, xData, style )

   IF HB_ISDATE( xData )
      ::writeData( "DateTime", row, column, DToC( xData ), style )
   ELSE
      ::writeData( "String", row, column, xData, style )
   ENDIF

   RETURN NIL

METHOD ExcelWriterXML_Sheet:writeData( type, row, column, xData, style, formula )

   LOCAL hcol, cell, styleID

   IF style != NIL
      IF HB_ISOBJECT( style )
         styleID := style:getID()
      ELSE
         styleID := style
      ENDIF
   ELSE
      styleID := NIL
   ENDIF

   cell := { ;
      "type"      => type, ;
      "style"     => styleID, ;
      "data"      => xData, ;
      "formula"   => formula }
   IF hb_HPos( ::cells, row ) > 0
      hcol := ::cells[ row ]
      hcol[ column ] := cell
      ::cells[ row ] := hcol
   ELSE
      hcol := { => }
      hcol[ column ] := cell
      ::cells[ row ] := hcol
   ENDIF

   RETURN NIL

METHOD ExcelWriterXML_Sheet:displayRightToLeft()

   ::ldisplayRightToLeft := .T.

   RETURN NIL

METHOD ExcelWriterXML_Sheet:getSheetXML( handle )

   LOCAL displayRightToLeft, ir, ic, xml, url
   LOCAL column, cell, xData, type, mergecell, comment, style, colIndex, colWidth
   LOCAL row, rowData, rowHeight, formula

   displayRightToLeft := iif( ::ldisplayRightToLeft, 'ss:RightToLeft="1"', "" )

   xml := '<Worksheet ss:Name="' + ::id + '" ' + displayRightToLeft + ">" + hb_eol()
   xml += "   <Table>" + hb_eol()

   FWrite( handle, xml )
   xml := ""

   IF Len( ::colWidth ) > 0
      FOR ic := 1 TO Len( ::colWidth )
         colIndex := hb_HKeyAt( ::colWidth, ic )
         colWidth := hb_HValueAt( ::colWidth, ic )
         colIndex := hb_ntos( colIndex )
         colWidth := hb_ntos( colWidth )
         xml += '      <Column ss:Index="' + colIndex + '" ss:AutoFitWidth="0" ss:Width="' + colWidth + '"/>' + hb_eol()
      NEXT
   ENDIF

   FWrite( handle, xml )
   xml := ""

   IF Len( ::cells ) > 0
      FOR ir := 1 TO Len( ::cells )
         row     := hb_HKeyAt( ::cells, ir )
         rowData := hb_HValueAt( ::cells, ir )

         IF hb_HPos( ::rowHeight, row ) > 0
            rowHeight := 'ss:AutoFitHeight="0" ss:Height="' + AllTrim( Str( ::rowHeight[ row ], 14, 2 ) ) + '"'
         ELSE
            rowHeight := ""
         ENDIF

         xml += '      <Row ss:Index="' + hb_ntos( row ) + '" ' + rowHeight + " >" + hb_eol()
         FOR ic := 1 TO Len( rowData )
            column := hb_HKeyAt( rowData, ic )
            cell   := hb_HValueAt( rowData, ic )
            IF !Empty( cell[ "formula" ] )
               formula := 'ss:Formula="' + cell[ "formula" ] + '"'
            ELSE
               formula := ""
            ENDIF
            IF !Empty( cell[ "style" ] )
               style := 'ss:StyleID="' + cell[ "style" ] + '"'
            ELSE
               style := ""
            ENDIF
            URL := ""
            mergeCell := ""
            IF hb_HPos( ::mergeCells, row ) > 0
               IF hb_HPos( ::mergeCells[ row ], column ) > 0
                  mergeCell := 'ss:MergeAcross="' + hb_ntos( ::mergeCells[ row ][ column ][ "width" ] ) + '" ss:MergeDown="' + hb_ntos( ::mergeCells[ row ][ column ][ "height" ] ) + '"'
               ENDIF
            ENDIF
            comment := ""
            IF hb_HPos( ::comments, row ) > 0
               IF hb_HPos( ::comments[ row ], column ) > 0
                  comment := '               <Comment ss:Author="' + ::comments[ row ][ column ][ "author" ] + '">' + hb_eol()
                  comment += '               <ss:Data xmlns="http://www.w3.org/TR/REC-html40">' + hb_eol()
                  comment += '               <B><Font html:Face="Tahoma" x:CharSet="1" html:Size="8" html:Color="#000000">' + ::comments[ row ][ column ][ "author" ] + ":</Font></B>" + hb_eol()
                  comment += '               <Font html:Face="Tahoma" x:CharSet="1" html:Size="8" html:Color="#000000">' + ::comments[ row ][ column ][ "comment" ] + "</Font>" + hb_eol()
                  comment += "               </ss:Data>" + hb_eol()
                  comment += "               </Comment>" + hb_eol()
               ENDIF
            ENDIF
            comment := ""
            type  := cell[ "type" ]
            xData := cell[ "data" ]

            xml += "         <Cell " + style + ' ss:Index="' + hb_ntos( column ) + '" ' + URL + " " + mergeCell + " " + formula + ">" + hb_eol()
            xml += '            <Data ss:Type="' + type + '">'
            xml += StrToHtmlEspecial( xData )
            xml += "</Data>" + hb_eol()
            xml += comment
            xml += "         </Cell>" + hb_eol()

         NEXT
         xml += "      </Row>" + hb_eol()

         FWrite( handle, xml )
         xml := ""
      NEXT
   ENDIF
   xml += "   </Table>" + hb_eol()
   xml += "</Worksheet>" + hb_eol()

   FWrite( handle, xml )
   xml := ""

   RETURN xml

METHOD ExcelWriterXML_Sheet:cellWidth( row, col, width )

   HB_SYMBOL_UNUSED( row )
   HB_SYMBOL_UNUSED( col )

   IF width == NIL
      width := 48
   ENDIF
   ::columnWidth( col, width )

   RETURN NIL

METHOD ExcelWriterXML_Sheet:columnWidth( col, width )

   IF width == NIL
      width := 48
   ENDIF
   ::colWidth[ col ] := width

   RETURN NIL

METHOD ExcelWriterXML_Sheet:cellHeight( row, col, height )

   HB_SYMBOL_UNUSED( col )

   IF height == NIL
      height := 12.5
   ENDIF
   ::setRowHeight( row, height )

   RETURN NIL

METHOD ExcelWriterXML_Sheet:setRowHeight( row, height )

   IF height == NIL
      height := 12.5
   ENDIF
   ::rowHeight[ row ] := height

   RETURN NIL

METHOD ExcelWriterXML_Sheet:cellMerge( row, col, width, height )

   LOCAL haux := { => }

   IF hb_HPos( ::mergeCells, row ) > 0
      haux := ::mergeCells[ row ]
   ENDIF
   haux[ col ] := { "width"   => width, ;
      "height"  => height }

   ::mergeCells[ row ] := haux

   RETURN NIL

METHOD ExcelWriterXML_Sheet:addComment( row, col, comment, author )

   LOCAL haux := { => }

   haux[ col ] := { "comment"  => comment, ;
      "author"   => author  }

   ::comments[ row ] := haux

   RETURN NIL
