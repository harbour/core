

/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Ini Class
 *
 * Copyright 2000 Manos Aspradakis <maspr@otenet.gr>
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
/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 *    Porting this library to Harbour
 *
 * See doc/license.txt for licensing terms.
 *
 */

/*
**
**    oIni.prg
**    --------
**
**    This implementation of an .ini file reader presents a different
**    approach to the problem as it creates a two dimensional array to
**    store and analyse data.
**
**    The first dimension keeps all sections and the second dimension
**    their entries. This way we avoid misinterpretation of entries (an
**    entry could be mistaken for section and vice versa) and keep
**    a clearer view of the whole structure in memory.
**
**    Entries are separated in entry tags, data and comments, e.g.
**
**    TestTag="Data"  ; comment
**
**    Header comments are supported only if preceded with "/*", "*" or "/*"
**    and only at the beginning of the .ini file before any sections.
**
**
**    Organisation of the file in memory (two dimensional array):
**    -----------------------------------------------------------
**
**      [section]
**                    entry=data
**                    entry=data
**                    ...
**      [section]
**                    ...
**
**
**    organisation of the file on disk:
**    ---------------------------------
**      /*
**      **  Header
**      */
**
**      [section]
**      entry=data   ; comment
**      entry=data
**      [section]
**      ...
**      ; comment
**
**
*/

#include "hbclass.ch"
#include "default.ch"

#ifdef TEST
PROC TestMe()
LOCAL o := oINI():New( "Test.ini" )
//o:Open()
o:Read()
objectViewer( o )
? "handle",o:handle
? o:Get( "methods", "method45", 45 )
? o:Get( "methods", "method455", .t. )
? o:Put( "methods", "method8", .f. )
? o:Get( "Test", "Test1", 1234567890 )
o:DumpSections()
? "handle",o:handle
o:Save(";"+CRLF()+;
       ";TEST.INI"+CRLF()+;
       ";Created on "+DTOC(DATE())+" - " + TIME() +CRLF()+;
       ";"+CRLF())
o:close()
RETURN

#endif


//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
 CLASS oIni FROM FileBase
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
DATA aLines    INIT {}
DATA aSections INIT {}
DATA lineSize  INIT 200

METHOD New( cFile )
METHOD Read()
METHOD Get( cSection, cEntry, uDefault )
METHOD Put( cSection, cEntry, uValue )
METHOD Save(c)
METHOD DumpSections()
ENDCLASS



//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
METHOD New( cFile ) CLASS oIni 
//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴

Super():New( cFile )
::Name      := cFile
::aLines    := {}
::aSections := {}
::handle    := -1
RETURN Self



/*
**  ::Read()
**
**  Reads the .ini file in memory, separating [sections] from entries
**
*/

METHOD Read()       CLASS oIni 
LOCAL nSection := 0

//? "ohandle",::handle
IF ::handle == NIL .OR. ::Handle <= 0

   IF ::Open()

       ::aLines := {} ;  ::aSections := {}
       ::Size()
       ::GoTop()

       WHILE !::EOF()

            ::ReadLine( ::lineSize )

            // ::aLines keep anything before the first section
            // like comments, headers etc.
            IF !EMPTY( ::Buffer ) .and. nSection == 0
                 IF ::Buffer = "/*" .or. ;
                    ::Buffer = "**" .or. ;
                    ::Buffer = "*"  .or. ;
                    ::Buffer = "*/"

                    AADD( ::aLines, ::Buffer )

                 ENDIF
            ENDIF

            IF LEFT(ALLTRIM( ::Buffer ),1) == "["
               nSection++
               AADD( ::aSections, { lower(ALLTRIM(::Buffer)), {} } )
            ENDIF

            if nSection > 0
                 if !EMPTY( ::Buffer )
                     AADD( ::aSections[nSection,2], ::buffer )
                 ENDIF
            ENDIF

            IF ::EOF()
               EXIT
            ENDIF

       ENDDO

       AEVAL( ::aSections, ;
              {|e| ADEL(e[2],1), ASIZE( e[2], LEN( e[2] ) -1 ) } )

   ENDIF  // ::open()

ENDIF  // ::handle

RETURN Self


/*
**  ::Get( cSection, cEntry, uDefault )
**
**  Retrieves an entry from memory. If it doesn't exist it creates it with
**  default data. Also it creates the section if it doesn't exist.
*/

METHOD Get( cSection, cEntry, uDefault ) CLASS oIni 
LOCAL cRet, nPos, nSection, nEntry

DEFAULT uDefault := ""

cSection := "["+lower( ALLTRIM(cSection) )+"]"
nSection := ASCAN( ::aSections, {|e| e[1] = cSection  } )

IF nSection > 0

   nEntry := ASCAN( ::aSections[nSection,2], ;
                    {|e| UPPER(cEntry) == UPPER( ALLTRIM(LEFT( e, AT("=",e)-1 )) ) })

   IF nEntry > 0   // Found entry. Get value. Cast to data type

       cRet := STR2ANY( GetEntryData( ::aSections[nSection,2,nEntry] ), ;
                        uDefault )

   ELSE           // Entry not found. Insert new entry

       AADD( ::aSections[nSection,2], cEntry + "=" + ANY2STR( uDefault ) )
       cRet := uDefault
   ENDIF

ELSE              // insert new section and entry...
     AADD( ::aSections, { cSection, ;
                        { cEntry + "=" + ANY2STR( uDefault ) } } )
     cRet := uDefault

ENDIF

RETURN( cRet )


/*
**  ::Put()
**
**
**
*/

METHOD Put( cSection, cEntry, uValue ) CLASS oIni 
LOCAL cRet := ""
LOCAL cComment, nPos, nSection, nEntry

DEFAULT uValue := ""

cSection := "["+Lower( ALLTRIM(cSection) )+"]"
nSection := ASCAN( ::aSections, {|e| e[1] = cSection })  //$ e[1] } )
IF nSection > 0

   nEntry := ASCAN( ::aSections[nSection,2], ;
                    {|e| UPPER(cEntry) == UPPER( LEFT( e, AT("=",e)-1 ) ) })

   IF nEntry > 0   // Found entry. Get value. Cast to data type

       // return old value
       cRet := STR2ANY( GetEntryData( ::aSections[nSection,2,nEntry] ), ;
                        uValue )

       cComment := GetEntryComment( ::aSections[nSection,2,nEntry] )

       // put new value
       ::aSections[nSection,2,nEntry] := cEntry + "=" + ;
                                         ANY2STR( uValue ) +;
                                         SPACE(2)+;
                                         cComment

   ELSE           // Entry not found. Insert new entry.

       AADD( ::aSections[nSection,2], cEntry + "=" + ANY2STR( uValue ) )
       cRet := uValue
   ENDIF

ELSE              // Section not found. Insert section and entry.

     AADD( ::aSections, { cSection, ;
                        { cEntry + "=" + ANY2STR( uValue ) } } )
     cRet := uValue

ENDIF

RETURN( cRet )


/*
**  ::Save()
**
**  Stores the .ini file back to disk from memory. All new changes are
**  saved, including comments...
*/

METHOD Save( cComment ) CLASS oIni 
LOCAL i, j

DEFAULT cComment := ""

::Close()

// Delete and create file

IF ::Create( ::Name )

   ::Write( cComment + CRLF() )

   AEVAL( ::aLines, {|e| FWrite( ::Handle, e + chr(13)+chr(10) ) } )

   ::Write( CRLF() )

   FOR i=1 to LEN(::aSections)

       //alert( ::aSections[i,1]  )

       ::Write( ::aSections[i,1] + CRLF() )
       FOR j=1 TO LEN( ::aSections[i,2] )
           ::Write( ::aSections[i,2,j] +CRLF() )
       NEXT
       ::Write( CRLF() )
   NEXT

ENDIF

::Close()

RETURN Self

METHOD DumpSections() cLASS OINI
LOCAL i, j
for i=1 to len(::aSections)
    outStd( ::aSections[i,1] + CRLF() )
    for j=1 to len( ::aSections[i,2] )
        OUTSTD( SPACE(5) + ::aSections[i,2,j] +CRLF() )
    next
next 
RETURN Self

/*
**  stripSection()
**
**  Removes "[]" characters from a section entry
*/
STATIC FUNCTION StripSection(cSection)
cSection := LOWER( ALLTRIM( cSection ) )
DO WHILE "[" $ cSection
   cSection := STUFF(cSection, AT("[", cSection), 1 )
ENDDO
DO WHILE "]" $ cSection
   cSection := STUFF(cSection, AT("]", cSection), 1)
ENDDO
RETURN "["+cSection+"]"


/*
**  GetEntryData( cEntry )
**
**  Retrieves the data part of an .ini entry. Supports comments to the
**  right of the data.
*/

STATIC FUNCTION GetEntryData( cEntry )
LOCAL cRet      := ""
LOCAL nPos      := 0
LOCAL isComment := ( nPos := AT( ";", cEntry ) ) > 0

IF isComment
   cRet := SUBSTR( cEntry, AT( "=", cEntry )+1 , LEN(cEntry)-nPos-1 )  //AT( ";", cEntry )-1 )
ELSE
   cRet := SUBSTR( cEntry, AT( "=", cEntry )+1 )
ENDIF

RETURN  alltrim( cRet )


/*
**  GetEntryComment( cEntry )
**
**  Retrieves the comment of an .ini entry.
*/

STATIC FUNCTION GetEntryComment( cEntry )
LOCAL cRet      := ""
LOCAL nPos      := 0
LOCAL isComment := ( nPos := AT( ";", cEntry ) ) > 0

IF isComment
   cRet := SUBSTR( cEntry, AT( ";", cEntry ), LEN( cEntry ) )
ELSE
   cRet := ""
ENDIF

RETURN  alltrim( cRet )


/*
**  ::DumpSections()
**
**  Display [sections] and entries from memory ( debug method )
**
*/




/****
*
*     UTILITIES
*     ---------
*
*/

//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
STATIC FUNCTION Any2Str( xVal )
LOCAL cRet  := ""
LOCAL cType := VALTYPE( xVal )
   DO CASE
      CASE cType == "D" ; cRet := DTOC( xVal )
      CASE cType == "N" ; cRet := ALLTRIM(STR( xVal ))
      CASE cType == "L" ; cRet := IF( xVal == .T., ".T.", ".F." )
      CASE cType == "B" ; cRet := "{|| ... }"
      CASE cType == "O" ; cRet := xVal:ClassName()
      CASE cType == "U" ; cRet := ""
      OTHERWISE         ; cRet := xVal
   ENDCASE
RETURN cRet


//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
STATIC FUNCTION Str2Any( cVal, uType )
LOCAL  bError
LOCAL uRet  := ""
LOCAL cType := VALTYPE( uType )
bError:= ERRORBLOCK( {|o| BREAK(o) } )
cVal := ALLTRIM( cVal )

BEGIN SEQUENCE

   DO CASE
      CASE cType == "D" ; uRet := CTOD( cVal )
      CASE cType == "N" ; uRet := IF( !EMPTY(cVal), VAL( cVal ), 0 )
      CASE cType == "L" ; uRet := IF( cVal == ".T.", .T., .F. )
      CASE cType == "B" ; uRet := {|| .T. }
      CASE cType == "O" ; uRet := NIL
      CASE cType == "U" ; uRet := NIL
      OTHERWISE         ; uRet := cVal
   ENDCASE

RECOVER
   uRet := cVal
END SEQUENCE

ERRORBLOCK( bError )

RETURN uRet


//컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
STATIC FUNCTION Token(cStr, cDelim, nToken)

LOCAL nPos, cToken, nCounter := 1

DEFAULT nToken := 1

WHILE .T.

     IF (nPos := AT(cDelim, cStr)) == 0

          IF nCounter == nToken
             cToken := cStr
          endif

          EXIT

     ENDIF

     IF ++nCounter > nToken
          cToken := LEFT(cStr, nPos - 1)
          EXIT
     ENDIF

     cStr := SUBSTR(cStr, nPos + 1)

ENDDO

RETURN cToken

