/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * __Typefile() function
 *
 * Copyright 2000 Luiz Rafael Culik <Culik@sl.conex.net>
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
 * Copyright 2000 Chen Kedem <niki@actcom.co.il>
 *    __TYPEFILE() / TYPE documentation
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include 'common.ch'
#include 'error.ch'
#include 'fileio.ch'

#define pBUFFER_LENGTH 2048
// #define EOL HB_OSNEWLINE()

#xtranslate FTELL(<nhandle>) => FSEEK(<nhandle>,0,FS_RELATIVE)

#ifdef TEST
function main
type typefile.prg
return nil
#endif

/*  $DOC$
 *  $FUNCNAME$
 *      __TYPEFILE()
 *  $CATEGORY$
 *      Data input and output
 *  $ONELINER$
 *      Show the content of a file on the console and/or printer
 *  $SYNTAX$
 *      __TYPEFILE( <cFile>, [<lPrint>] ) --> NIL
 *  $ARGUMENTS$
 *      <cFile> is a name of the file to display. If the file have an
 *      extension, it must be specified (there is no default value).
 *
 *      <lPrint> is an optional logical value that specifies whether the
 *      output should go only to the screen (.F.) or to both the screen and
 *      printer (.T.), the default is (.F.).
 *  $RETURNS$
 *      __TYPEFILE() always return NIL.
 *  $DESCRIPTION$
 *      __TYPEFILE() function type the content of a text file on the screen
 *      with an option to send this information also to the printer. The
 *      file is displayed as is without any headings or formating.
 *
 *      If <cFile> contain no path, __TYPEFILE() try to find the file first
 *      in the SET DEFAULT directory and then in search all of the SET PATH
 *      directories. If <cFile> can not be found a run-time error occur.
 *
 *      Use SET CONSOLE OFF to suppress screen output.
 *      You can pause the output using Ctrl-S, press any key to resume.
 *
 *      __TYPEFILE() function is used in the preprocessing of the TYPE
 *      command.
 *  $EXAMPLES$
 *      The following examples assume a file name MyText.DAT exist in all
 *      specified paths, a run-time error would displayed if it does not
 *
 *      // display MyText.DAT file on screen
 *      __TYPEFILE( "MyText.DAT" )
 *
 *      // display MyText.DAT file on screen and printer
 *      __TYPEFILE( "MyText.DAT", .T. )
 *
 *      // display MyText.DAT file on printer only
 *      SET CONSOLE OFF
 *      __TYPEFILE( "MyText.DAT", .T. )
 *      SET CONSOLE ON
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      __TYPEFILE() works exactly like CA-Clipper's __TYPEFILE()
 *  $PLATFORMS$
 *  $FILES$
 *  $SEEALSO$
 *      "COPY FILE","SET DEFAULT","SET PATH","SET PRINTER",TYPE
 *  $END$
 */

FUNCTION __TYPEFILE( cFile, lPrint )
LOCAL nHandle, cBuffer
LOCAL oErr, xRecover, nRetries
LOCAL aSaveSet[2]
LOCAL cDir, cName, cExt, cTmp, aPath, i

IF !ISLOGICAL( lPrint )
    lPrint := .F.
ENDIF

IF !ISCHARACTER( cFile )
    oErr := ErrorNew()
    oErr:severity    := ES_ERROR
    oErr:genCode     := EG_OPEN
    oErr:subSystem   := "BASE"
    oErr:SubCode     := 2009
    oErr:Description := "Argument error: __TYPEFILE"
    Eval( ErrorBlock(), oErr )
ENDIF

// If no drive/dir specified, search the SET DEFAULT and PATH directories

hb_FNameSplit( cFile, @cDir, @cName, @cExt )
IF Empty( cDir )
    cTmp := SET( _SET_DEFAULT ) + ";" + SET( _SET_PATH )
    cTmp := StrTran( cTmp, ",", ";" )
    i := len( cTmp )
    IF substr( cTmp, i, 1 ) == ";"            // remove last ";"
        cTmp := substr( cTmp, 1, i - 1 )
    ENDIF
    aPath := aDvd( cTmp )
    FOR i = 1 TO len( aPath )
        cTmp := hb_FNameMerge( aPath[ i ], cName, cExt )
        IF file( cTmp )
            cFile := cTmp
            exit
        ENDIF
    NEXT
ENDIF

nRetries := 0
DO WHILE ( nHandle := FOPEN( cFile, FO_READWRITE ) ) == F_ERROR
    oErr := ErrorNew()
    oErr:severity    := ES_ERROR
    oErr:genCode     := EG_OPEN
    oErr:subSystem   := "BASE"
    oErr:SubCode     := 2011
    oErr:Description := "Open Error: " + cFile
    oErr:canDefault  := .T.
    oErr:canRetry    := .T.
    oErr:OsCode      := FERROR()
    oErr:tries       := ++nRetries
    xRecover := Eval( ErrorBlock(), oErr )
    IF ISLOGICAL( xRecover ) .and. !xRecover      // user select "Default"
        RETURN NIL
    ENDIF
ENDDO

// NOTE: the NG say you should explicitly SET CONSOLE OFF if you wish to
//       suppress output to screen. [ckedem]

IF lPrint
    aSaveSet[ 1 ] := Set( _SET_DEVICE, "PRINTER" )
    aSaveSet[ 2 ] := Set( _SET_PRINTER, .T. )
/*  SET CONSOLE OFF */
ENDIF

// here we try to read a line at a time but I think we could just
// display the whole buffer since it said: "without any headings or formating"

// DO WHILE Freadln( nHandle, @cbuffer, pBUFFER_LENGTH )
//    ? cBuffer
// ENDDO

?                                                      // start in a new line
DO WHILE fread( nHandle, @cbuffer, pBUFFER_LENGTH ) > 0
    ?? cBuffer
ENDDO


FCLOSE( nHandle )

IF lPrint
    Set( _SET_DEVICE,  aSaveSet[ 1 ] )
    Set( _SET_PRINTER, aSaveSet[ 2 ] )
/*  SET CONSOLE ON */
ENDIF

RETURN NIL

/*
STATIC FUNCTION Freadln( nH, cB, nMaxLine )
LOCAL cLine, nSavePos, nEol, nNumRead
cLine := space( nMaxLine )
cB := ''
nSavePos := FTELL( nH )
nNumRead := fread( nH, @cLine, nMaxLine )
IF ( nEol := at( EOL, substr( cLine, 1, nNumRead ) ) ) == 0
    cB := cLine
ELSE
    cB := substr( cLine, 1, nEol - 1 )
    fseek( nH, nSavePos + nEol + 1, FS_SET )
ENDIF
RETURN nNumRead != 0
*/

/*----------------------------------------------------------------------------*/
/*         Function aDvd : Divide string to tokens and put tokens into array  */
/*         Parameters : cString - String to be splited          ( C )         */
/*                      cDelim  - Delimiter of tokens in string ( C )         */
/*         Return :     Array of tokens or empty array                        */
/*----------------------------------------------------------------------------*/
STATIC FUNCTION aDvd( cString, cDelim )
LOCAL aProm:={}, nPos
iif( cDelim == NIL, cDelim := ";", )
DO WHILE ( nPos := at( cDelim, cString ) ) != 0
    aadd( aProm, substr( cString, 1, nPos - 1 ) )
    cString := substr( cString, nPos + len( cDelim ) )
ENDDO
aadd( aProm, cString )
RETURN aProm

/*  $DOC$
 *  $FUNCNAME$
 *      TYPE
 *  $CATEGORY$
 *      Data input and output
 *  $ONELINER$
 *      Show the content of a file on the console, printer or file
 *  $SYNTAX$
 *      TYPE <xcFile> [TO PRINTER] [TO FILE <xcDestFile>]
 *  $ARGUMENTS$
 *      <xcFile> is a name of the file to display. If the file have an
 *      extension, it must be specified (there is no default value).
 *      It can be specified as literal file name or as a character
 *      expression enclosed in parentheses.
 *
 *      TO PRINTER is an optional keyword that specifies that the output
 *      should go to both the screen and printer.
 *
 *      TO FILE <xcDestFile> copy the source <xcFile> also to a file. If no
 *      extension is given (.txt) is added to the output file name.
 *      <xcDestFile> can be specified as literal file name or as a character
 *      expression enclosed in parentheses.
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TYPE command type the content of a text file on the screen
 *      with an option to send this information also to the printer or to
 *      an alternate file. The file is displayed as is without any headings
 *      or formating.
 *
 *      If <xcFile> contain no path, TYPE try to find the file first in the
 *      SET DEFAULT directory and then in search all of the SET PATH
 *      directories. If <xcFile> can not be found a run-time error occur.
 *
 *      If <xcDestFile> contain no path it is created in the SET DEFAULT
 *      directory.
 *
 *      Use SET CONSOLE OFF to suppress screen output.
 *      You can pause the output using Ctrl-S, press any key to resume.
 *  $EXAMPLES$
 *      The following examples assume a file name MyText.DAT exist in all
 *      specified paths, a run-time error would displayed if it does not
 *
 *      // display MyText.DAT file on screen
 *      TYPE MyText.DAT
 *
 *      // display MyText.DAT file on screen and printer
 *      TYPE MyText.DAT TO PRINTER
 *
 *      // display MyText.DAT file on printer only
 *      SET CONSOLE OFF
 *      TYPE MyText.DAT" TO PRINTER
 *      SET CONSOLE ON
 *
 *      // display MyText.DAT file on screen and into a file MyReport.txt
 *      TYPE MyText.DAT TO FILE MyReport
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      TYPE works exactly like CA-Clipper's TYPE
 *  $PLATFORMS$
 *  $FILES$
 *  $SEEALSO$
 *      "COPY FILE","SET DEFAULT","SET PATH","SET PRINTER",__TYPEFILE()
 *  $END$
 */
