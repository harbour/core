/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour level definition of characters mapping
 *
 * Copyright 2003 Ryszard Glab <rglab@imid.med.pl>
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

/*
  Load user specified character mappings which was specified as 
  a command line argument
  //CHARMAP:full_path_to_file
  
  <input_code>:<output_code>
  
  Example:

  //checkbox mark
  251:'x'
  //o
  254:74
  
  codes can contain:
  	octal codes \012 (three digits)
  or: 
  	hex codes \x0F 
  or:
  	decimal code
  or:
   string with a single character eg. 'x' "z"
   
   The output_code can contain '+' character at the ending - in this case the
   ALTERNATE attribute will be enbled for this mapping
   eg.:
   3:3+
*/

PROCEDURE HB_GT_CHARMAP( cFile )
LOCAL cMap, i, nLines, cLine, nPos
LOCAL nIn, cOut, nOut

	IF( EMPTY(cFile) )
		cFile = HB_ARGSTRING( "CHARMAP" )
	ENDIF
	IF( !EMPTY(cFile) )
		cMap := MEMOREAD( cFile )
		IF( !EMPTY(cMap) )
			nLines := MLCOUNT( cMap )
			FOR i:=1 TO nLines
				cLine := ALLTRIM( MEMOLINE( cMap, 196, i ) )
				IF( !EMPTY(cLine) )
					IF( cLine = '//' )
					
					ELSEIF((nPos:=AT(':',cLine)) > 0 )
						cOut := SUBSTR( cLine, nPos+1 )
                  nIn  := CharConv( LEFT( cLine, nPos-1 ) )
                  nOut := CharConv( cOut )
                  IF( nIn >= 0 .AND. nOut >= 0 )
							HB_GT_ADDCHARMAP( nIn, nOut, RIGHT(cOut,1)=='+' )
                  ENDIF
					ENDIF
				ENDIF
			NEXT
		ENDIF
	ENDIF
	
RETURN

STATIC FUNCTION CharConv( cTxt )
LOCAL cChar, cChar2
LOCAL nAsc:=-1
LOCAL lExact:=SET(_SET_EXACT, .F. )

	cChar2 := SUBSTR( cTxt, 2, 1 )
	IF( cTxt = '\' )
      IF( cChar2 $ '0123' )
		   //OCTAL NUMBER < 377 (255 decimal)
		   nAsc := Octal2Num(SUBSTR(cTxt,2,3) )

	   ELSEIF( cChar2 $ 'xX' )
		   //Hex number
		   nAsc := Hex2Num( SUBSTR(cTxt, 3, 2 ) )
	   ELSEIF( cChar2 == '\' )
		   nAsc := ASC('\')
      ENDIF
      
	ELSEIF( (cTxt = "'" .OR. cTxt = '"') .AND. LEN(cTxt)==3 )
   	nAsc := ASC( SUBSTR(cTxt, 2, 1) )
      
   ELSE
      nAsc := VAL( cTxt )
	ENDIF
   SET(_SET_EXACT, lExact)

RETURN nAsc

//
STATIC FUNCTION Octal2Num( cOctal )
LOCAL i, c, nMult:=1, nVal:=0

	FOR i:=0 TO 2
		c := SUBSTR( cOctal, 3-i, 1 )
		IF( c $ '01234567' )
			nVal += ((ASC( c ) - ASC( '0' )) * nMult)
			nMult *= 8
		ELSE
			RETURN -1
		ENDIF
	NEXT
	
RETURN nVal

STATIC FUNCTION Hex2Num( cOctal )
LOCAL i, c, nMult:=1, nVal:=0

	FOR i:=0 TO 1
		c := SUBSTR( cOctal, 2-i, 1 )
		IF( c $ '01234567' )
			nVal += ((ASC( c ) - ASC( '0' )) * nMult)
		ELSEIF( c $ 'ABCDEF' )
			nVal += ((ASC( c ) - ASC( 'A' ) + 10 ) * nMult)
		ELSE
			RETURN -1
		ENDIF
		nMult *= 16
	NEXT
	
RETURN nVal
