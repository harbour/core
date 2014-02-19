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

#require "hbxlsxml"

PROCEDURE Main()

   LOCAL xml, format1, format2, format3, format4
   LOCAL sheet1, sheet2, sheet4

   xml := ExcelWriterXML():new( "my file.xml" )

   /**
    * Add some general properties to the document
    */
   xml:docTitle( "My Demo Doc" )
   xml:docAuthor( "Robert F Greer" )
   xml:docCompany( "Greers Org" )
   xml:docManager( "Wife" )

   /**
    * Choose to show any formatting/input errors on a seperate sheet
    */
#if 0
   xml:showErrorSheet( .T. )
#endif

   /**
    * Show the style options
    */
   format1 := xml:addStyle( "left_rotate60_big" )
   format1:alignRotate( 60 )
   format1:alignHorizontal( "Left" )
   format1:setFontSize( "18" )

   format2 := xml:addStyle( "verticaltext_left" )
   format2:alignVerticaltext( 45 )
   format2:alignHorizontal( "Left" )

   format3 := xml:addStyle( "wraptext_top" )
   format3:alignWraptext()
   format3:alignVertical( "Top" )

   /**
    * Create a new sheet with the XML document
    */
   sheet1 := xml:addSheet( "Alignment" )
   /**
    * Add three new cells of type String with difference alignment values.
    * Notice that the style of the each cell can be explicity named or the style
    * reference can be passed.
    */
   sheet1:writeString( 1, 1, "left_rotate45", format1 )
   sheet1:writeString( 1, 2, "vertical left", "verticaltext_left" )
   sheet1:writeString( 1, 3, "this text has been wrapped and is aligned at the top", "wraptext_top" )
#if 0
   sheet1:writeString( 1, 4, "No style applied" )
#endif

   sheet2 := xml:addSheet( "Formulas" )
   /**
    * Wrote three numbers.
    * Rows 4 and 5 show the formulas in R1C1 notation using the writeFormula()
    * function.
    * Also see how comments are added.
    */
   sheet2:columnWidth( 1, 100 )
   sheet2:writeString( 1, 1, "Number" )
   sheet2:writeNumber( 1, 2, 50 )
   sheet2:writeString( 2, 1, "Number" )
   sheet2:writeNumber( 2, 2, 30 )
   sheet2:writeString( 3, 1, "Number" )
   sheet2:writeNumber( 3, 2, 20 )
   sheet2:writeString( 4, 1, "=SUM(R[-3]C:R[-1]C)" )
   sheet2:writeFormula( "Number", 4, 2, "=SUM(R[-3]C:R[-1]C)" )
#if 0
   sheet2:addComment( 4, 2, "Here is my formula: =SUM(R[-3]C:R[-1]C)","My NAME" )
#endif
   sheet2:writeString( 5, 1, "=SUM(R1C2:R3C2)" )
   sheet2:writeFormula( "Number", 5, 2, "=SUM(R1C1:R3C2)" )
#if 0
   sheet2:addComment( 5, 2, "Here is my formula: =SUM(R1C1:R3C2)" )
#endif

   sheet4 := xml:addSheet( "more formatting" )
   format4 := xml:addStyle( "my style" )
   format4:setFontBold()
   format4:setFontItalic()
   format4:setFontUnderline( "DoubleAccounting" )
   format4:bgColor( "Black" )
   format4:setFontColor( "White" )
   format4:setNumberFormatDateTime()
#if 0
   mydate := sheet4:convertMysqlDateTime( "2008-02-14 19:30:00" )
#endif
   sheet4:writeDateTime( 1, 1, DToC( Date() ), format4 )
   // Change the row1 height to 30 pixels
   sheet4:rowHeight( 1, "30" )
   sheet4:writeString( 2, 1, "formatted text + cell color + merged + underlined", format4 )
   // Merge (2,1) with 4 columns to the right and 2 rows down
   sheet4:cellMerge( 2, 1, 4, 2 )

   /**
    * Send the headers, then output the data
    */
#if 0
   xml:sendHeaders()
#endif
   xml:writeData( "example2.xml" )

   RETURN
