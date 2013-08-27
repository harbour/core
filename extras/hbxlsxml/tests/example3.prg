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
 * Boston, MA 02111-1307 USA( OR visit the web site https://www.gnu.org/ ).
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

   LOCAL xml, sheet1, format4

   xml := ExcelWriterXML():New( "my file.xml" )

   sheet1 := xml:addSheet( "Plan 1" )

   format4 := xml:addStyle( "my style" )
   format4:setFontSize( 20 )
   format4:setFontColor( "yellow" )
   format4:bgColor( "blue" )

   sheet1:columnWidth( 1, 150 )
   sheet1:columnWidth( 2, 150 )
   sheet1:columnWidth( 3, 150 )
   sheet1:writeString( 2, 3, "celula 2_3", format4 )
   sheet1:writeString( 2, 2, "celula 2_2", format4 )
   sheet1:writeString( 2, 1, "celula 2_1", format4 )

   sheet1:writeString( 1, 1, "celula 1_1", format4 )
#if 0
   sheet1:writeString( 1, 2, "celula 1_2", format4 )
#endif
   sheet1:writeString( 1, 3, "celula 1_3", format4 )
   sheet1:cellMerge( 1, 1, 1, 0 )

   xml:writeData( "example3.xml" )

   RETURN
