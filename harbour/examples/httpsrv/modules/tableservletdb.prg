/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    xml table servlet
 *
 * Copyright 2009 Francesco Saverio Giudice <info / at / fsgiudice.com>
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

#include "common.ch"
#include "hbclass.ch"

#define CRLF ( chr(13)+chr(10) )
#ifdef __PLATFORM__WINDOWS
#define TABLE_NAME_PATH "..\..\..\tests\test.dbf"
#else
#define TABLE_NAME_PATH "../../../tests/test.dbf"
#endif
#define SIMULATE_SLOW_REPLY

MEMVAR _REQUEST // defined in uHTTPD

FUNCTION HRBMAIN()

   LOCAL cXml, cPage, cCount, nCount
   LOCAL oTM
   LOCAL hGets

   hGets := _REQUEST

   DEFAULT hGets TO { => }

   IF HB_HHasKey( hGets, "page" )

      cPage := hGets[ "page" ]

      oTM  := TableManager():New()

      IF ( oTM:Open() )

         oTM:Read()
         cXml := oTM:getXmlData( Val( cPage ) )

         oTM:Close()

      ENDIF

   ELSEIF HB_HHasKey( hGets, "count" )

      cCount := hGets[ "count" ]

      IF cCount == "true"

         oTM  := TableManager():New()

         IF ( oTM:Open() )

            nCount := oTM:getLastRec()
            cXml := oTM:getXmlCount( nCount )

            oTM:Close()

         ENDIF

      ENDIF
   ENDIF


   IF !Empty( cXml )

      uhttpd_SetHeader( "Content-Type", "text/xml" )
      // cache control
      uhttpd_SetHeader( "Cache-Control", "no-cache, must-revalidate" )
      uhttpd_SetHeader( "Expires", "Mon, 26 Jul 1997 05:00:00 GMT" )

      uhttpd_Write( cXml )

   ELSE

      uhttpd_SetHeader("Content-Type", "text/xml")
      uhttpd_Write( '<?xml version="1.0" encoding="ISO-8859-1"?>' )
      uhttpd_Write( '<pages><page>No Data</page></pages>' )

   ENDIF

RETURN TRUE // I Handle HTML Output

/*
  TableManager
*/

CLASS TableManager

    CLASSVAR ROWS_PER_PAGE INIT 23

    VAR aData               INIT {}

    VAR cTable              INIT TABLE_NAME_PATH
    VAR lOpened             INIT FALSE

    METHOD New()
    METHOD Open()
    METHOD Close()          INLINE IIF( ::lOpened, ( table->( dbCloseArea() ), ::lOpened := FALSE ), )
    METHOD Read()
    METHOD getLastRec()     INLINE table->( LastRec() )
    METHOD getXmlData( page )
    METHOD getXmlCount( ncount )
    METHOD xmlEncode( input )
ENDCLASS

METHOD New() CLASS TableManager
RETURN Self

METHOD Open() CLASS TableManager
   LOCAL cDBF := ::cTable

   //hb_ToOutDebug( "CurPath = %s", hb_CurDrive() + hb_osDriveSeparator() + hb_ps() + CurDir() )

   //hb_ToOutDebug( "before: cDBF = %s, Used() = %s\n", cDBF, Used() )

   IF !::lOpened

      CLOSE ALL
      USE ( cDBF ) ALIAS table SHARED NEW
      //hb_ToOutDebug( "after: cDBF = %s, Used() = %s\n", cDBF, Used() )
      ::lOpened := USED()

   ENDIF

RETURN ::lOpened

METHOD Read() CLASS TableManager
   LOCAL hMap, lOk := FALSE

#ifdef SIMULATE_SLOW_REPLY
   // force slow connection to simulate long reply
   HB_IDLESLEEP(0.5)
#endif

   IF ::lOpened

      table->( dbGoTop() )
      //n := 0
      DO WHILE table->( !Eof() ) //.AND. ++n < 50

         hMap := { => }
         hMap[ "recno"   ] := StrZero( table->( RecNo() ), 4 )
         hMap[ "name"    ] := RTrim( table->first ) + " " + RTrim( table->last )
         hMap[ "address" ] := RTrim( table->street )
         hMap[ "city"    ] := RTrim( table->city )
         hMap[ "state"   ] := table->state
         hMap[ "zip"     ] := table->zip
         aAdd( ::aData, hMap )
         table->( dbSkip() )
      ENDDO

      lOk := TRUE

   ENDIF

RETURN lOK

/**
 * Builds a <code>String</code> of XML representing the aData for the
 * request table.
 *
 * For simplicity, we are using a hard-coded data set. In a production
 * system, you may wish to use DAOs to query a database for specific table
 * data. This may require additional parameters (e.g., the name of the
 * table, which could be used to look up instructions on retrieving the
 * necessary data).
 *
 * The returned XML will be formatted as follows:
 * &lt;table&gt;<br />
 *   &lt;header&gt;<br />
 *     &lt;cell key="address"&gt;Address&lt;/cell&gt;<br />
 *   &lt;/header&gt;<br />
 *   &lt;row&gt;<br />
 *     &lt;cell key="name"&gt;Hank&lt;/cell&gt;<br />
 *     &lt;cell key="address"&gt;1B Something Street&lt;/cell&gt;<br />
 *     &lt;cell key="city"&gt;Marietta&lt;/cell&gt;<br />
 *     &lt;cell key="state"&gt;GA&lt;/cell&gt;<br />
 *     &lt;cell key="zip"&gt;30339&lt;/cell&gt;<br />
 *   &lt;/row&gt;<br />
 *   ...<br />
 * &lt;/table&gt;
 *
 * @param page
 *            the page number to retrieve data for
 * @return a <code>String</code> of XML representing data for the
 *         requested table
 * @throws IllegalArgumentException
 */

METHOD getXmlData( page ) CLASS TableManager
   LOCAL startIndex, stopIndex
   LOCAL xml, i, map, key, cString

   /*
    * For simplicity, we are creating XML as a String. In a production
    * system, you should create an XML document (org.w3c.dom.Document) to
    * ensure compliance with the DOM Level 2 Core Specification.
    */

   // Calculate the start and end indexes of the table data.
   startIndex := (page - 1) * ::ROWS_PER_PAGE
   stopIndex  := startIndex + ::ROWS_PER_PAGE
   stopIndex  := Min( Len( ::aData ), stopIndex )

   // Check the validity of the page index.
   IF ( startIndex < 0 .OR. startIndex >= stopIndex )
       //throw new IllegalArgumentException("Page index is out of bounds.");
   ENDIF

   xml := BasicXML():New()

   xml:append( '<?xml version="1.0" encoding="ISO-8859-1"?>' )

   // Add the opening <table> tag
   xml:append( "<table>" )

   // Add nodes describing the table columns
   xml:append( "<header>" )
   xml:append( '<cell key="recno">RecNo</cell>')
   xml:append( '<cell key="name">Name</cell>')
   xml:append( '<cell key="address">Address</cell>' )
   xml:append( '<cell key="city">City</cell>' )
   xml:append( '<cell key="state">State</cell>' )
   xml:append( '<cell key="zip">Zip</cell>' )
   xml:append( "</header>" )

   // Add nodes for each row.
   FOR i := startIndex + 1 TO stopIndex
       map := ::aData[ i ]

       // Add the opening <row> tag
       xml:append( "<row>" )

       // For each entry in the HashMap, add a node
       // e.g., <address>123 four street</address>
       FOR EACH key IN map:Keys

           cString := '<cell key="' + key + '">'
           cString += ::xmlEncode( hb_cStr( map[ key ] ) )
           cString += "</cell>"

           xml:append( cString )

       NEXT

       // Add the closing </row> tag
       xml:append( "</row>" )

   NEXT

   // Add the closing </table> tag
   xml:append( "</table>" )

RETURN xml:toString()

METHOD getXmlCount( nCount ) CLASS TableManager
   LOCAL xml, n
   LOCAL nPages := nCount / ::ROWS_PER_PAGE

   IF Int( nPages ) < nPages
      nPages++
   ENDIF

   xml := BasicXML():New()

   xml:append( '<?xml version="1.0" encoding="ISO-8859-1"?>' )

   xml:append( "<pages>" )
   FOR n := 1 TO nPages
       xml:append( "<page>" + LTrim( Str( n ) ) + "</page>" )
   NEXT
   xml:append( "</pages>" )

RETURN xml:toString()

    /**
     * Replaces characters commonly used in XML with symbolic representations
     * such that they are interpretted correctly by XML parsers.
     *
     * @param input
     *            the string to encode.
     * @return the encoded version of the specified string
     */
METHOD xmlEncode( input ) CLASS TableManager

   LOCAL out, i, c

   IF input == NIL
      RETURN input
   ENDIF

   // Go through the input string and replace the following
   // characters:
   //  & &amp;
   //  ' &apos;
   //  " &quot;
   //  < &lt;
   //  > &gt;
   //  [any non-ascii character] &#[character code];

   out := ""

   FOR i := 1 TO Len( input )
       c := SubStr( input, i, 1 )
       switch ( c )
       case '&'
           out += "&amp;"
           exit
       case "'"
           out += "&apos;"
           exit
       case '"'
           out += "&quot;"
           exit
       case '<'
           out += "&lt;"
           exit
       case '>'
           out += "&gt;"
           exit
       //case ' '
       //    out += "&nbsp;"
       //    exit
       case Chr( 9 ) //E'\t'
       case Chr( 13 ) //E'\r'
       case Chr( 10 ) //E'\n'
           out += c
           exit
       OTHERWISE
           // All non-ascii
           if ( Asc( c ) <= 0x1F .OR. Asc( c ) >= 0x80 )
               out += "&#x" + hb_NumToHex( Asc( c ) ) + ";"
           else
               out += c
           endif
           exit
       end
   NEXT

RETURN out

CLASS BasicXML
   VAR aData         INIT {}

   METHOD New()              CONSTRUCTOR
   METHOD Append( cString )  INLINE aAdd( ::aData, cString )
   METHOD ToString()

ENDCLASS

METHOD New() CLASS BasicXML

RETURN Self

METHOD ToString() CLASS BasicXML
  LOCAL s := ""

  aEval( ::aData, {|c| s += c + IIF( Right( c, 1 ) == ">", CRLF, "" ) } )

RETURN s
