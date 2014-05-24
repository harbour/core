 /*
 * Harbour Project source code:
 *
 * Copyright 2011 Fausto Di Creddo Trautwein, ftwein@yahoo.com.br
 * www - http://harbour-project.org
 *
 * Thanks TO Robert F Greer, PHP original version
 * https://sourceforge.net/projects/excelwriterxml/
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

   ::formatErrors += { ;
      "sheet"      => ::id, ;
      "FUNCTION"   => cFunction, ;
      "MESSAGE"    => cMessage }

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

   IF HB_ISNUMERIC( xData )
      ::writeData( "Number", row, column, AllTrim( Str( xData, 18, 6 ) ), style )
   ELSE
      ::writeData( "String", row, column, xData, style )
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

   IF row $ ::cells
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

   FOR EACH ic IN ::colWidth
      colIndex := ic:__enumKey()
      colWidth := ic:__enumValue()
      colIndex := hb_ntos( colIndex )
      colWidth := hb_ntos( colWidth )
      xml += '      <Column ss:Index="' + colIndex + '" ss:AutoFitWidth="0" ss:Width="' + colWidth + '"/>' + hb_eol()
   NEXT

   FWrite( handle, xml )
   xml := ""

   FOR EACH ir IN ::cells
      row     := ir:__enumKey()
      rowData := ir:__enumValue()

      IF row $ ::rowHeight
         rowHeight := 'ss:AutoFitHeight="0" ss:Height="' + AllTrim( Str( ::rowHeight[ row ], 14, 2 ) ) + '"'
      ELSE
         rowHeight := ""
      ENDIF

      xml += '      <Row ss:Index="' + hb_ntos( row ) + '" ' + rowHeight + " >" + hb_eol()
      FOR EACH ic IN rowData
         column := ic:__enumKey()
         cell   := ic:__enumValue()
         IF Empty( cell[ "formula" ] )
            formula := ""
         ELSE
            formula := 'ss:Formula="' + cell[ "formula" ] + '"'
         ENDIF
         IF Empty( cell[ "style" ] )
            style := ""
         ELSE
            style := 'ss:StyleID="' + cell[ "style" ] + '"'
         ENDIF
         URL := ""
         mergeCell := ""
         IF row $ ::mergeCells
            IF column $ ::mergeCells[ row ]
               mergeCell := 'ss:MergeAcross="' + hb_ntos( ::mergeCells[ row ][ column ][ "width" ] ) + '" ss:MergeDown="' + hb_ntos( ::mergeCells[ row ][ column ][ "height" ] ) + '"'
            ENDIF
         ENDIF
         comment := ""
         IF row $ ::comments
            IF column $ ::comments[ row ]
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

         xml += "         <Cell " + AllTrim( style + ' ss:Index="' + hb_ntos( column ) + '" ' + URL + " " + mergeCell + " " + formula ) + ">" + hb_eol()
         xml += '            <Data ss:Type="' + type + '">'
         xml += StrToHtmlSpecial( xData )
         xml += "</Data>" + hb_eol()
         xml += comment
         xml += "         </Cell>" + hb_eol()

      NEXT
      xml += "      </Row>" + hb_eol()

      FWrite( handle, xml )
      xml := ""
   NEXT

   xml += "   </Table>" + hb_eol()
   xml += "</Worksheet>" + hb_eol()

   FWrite( handle, xml )
   xml := ""

   RETURN xml

METHOD ExcelWriterXML_Sheet:cellWidth( row, col, width )

   HB_SYMBOL_UNUSED( row )

   ::columnWidth( col, hb_defaultValue( width, 48 ) )

   RETURN NIL

METHOD ExcelWriterXML_Sheet:columnWidth( col, width )

   ::colWidth[ col ] := hb_defaultValue( width, 48 )

   RETURN NIL

METHOD ExcelWriterXML_Sheet:cellHeight( row, col, height )

   HB_SYMBOL_UNUSED( col )

   ::setRowHeight( row, hb_defaultValue( height, 12.5 ) )

   RETURN NIL

METHOD ExcelWriterXML_Sheet:setRowHeight( row, height )

   ::rowHeight[ row ] := hb_defaultValue( height, 12.5 )

   RETURN NIL

METHOD ExcelWriterXML_Sheet:cellMerge( row, col, width, height )

   LOCAL haux := { => }

   IF row $ ::mergeCells
      haux := ::mergeCells[ row ]
   ENDIF

   haux[ col ] := { ;
      "width"   => width, ;
      "height"  => height }

   ::mergeCells[ row ] := haux

   RETURN NIL

METHOD ExcelWriterXML_Sheet:addComment( row, col, comment, author )

   LOCAL haux := { => }

   haux[ col ] := { ;
      "comment"  => comment, ;
      "author"   => author }

   ::comments[ row ] := haux

   RETURN NIL
