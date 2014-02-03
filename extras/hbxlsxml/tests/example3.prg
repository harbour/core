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
