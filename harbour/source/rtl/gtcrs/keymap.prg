/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour level definition of keyboard mapping
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
  Load user specified keymap which was specified as a command line argument
  //KEYMAP:full_path_to_file
  
  The keymap file should contine definitions of byte sequences that should be
  translated into a single INKEY() value - a single line for a single 
  inkey code (see include/inkey.ch for valid INKEY codes\\\\\0.
  Additionally it can contain "CLEAR' keyword to clear default keymaps
  
  Format of file
  [CLEAR]
  <inkey_code>:<sequence>
  
  Example:
  CLEAR
  //Ctrl-Pgup
  31:\033]5^
  //Ctrl-PgDn
  30:\033]6^  
  
  Keycodes can contain:
  	octal codes \012 (three digits)
  or: 
  	hex codes \x0F 
  or:
  	if you need a backslash then use '\\'
*/

PROCEDURE HB_GT_KEYMAP( cFile )
LOCAL cKeymap, i, nKeys, cLine, nPos, nKey, nLen
LOCAL j, cCode, cChar, cChar2, nAsc

	IF( EMPTY(cFile) )
		cFile = HB_ARGSTRING( "KEYMAP" )
	ENDIF
	IF( !EMPTY(cFile) )
		cKeymap := MEMOREAD( cFile )
		IF( !EMPTY(cKeymap) )
			nKeys := MLCOUNT( cKeymap )
			FOR i:=1 TO nKeys
				cLine := ALLTRIM( MEMOLINE( cKeymap, 196, i ) )
				IF( !EMPTY(cLine) )
					IF( cLine = '//' )
					
					ELSEIF( UPPER(cLine) = 'CLEAR' )
						HB_GT_ADDKEYMAP(0)		// Clear all default keymaps
					
					ELSEIF((nPos:=AT(':',cLine)) > 0 )
						nKey := VAL( LEFT( cLine, nPos-1 ) )
						cLine := SUBSTR( cLine, nPos+1 )
						j :=1
						nLen := LEN(cLine)
						cCode := ""
						DO WHILE( j <= nLen )
							cChar := SUBSTR( cLine, j, 1 )
							cChar2 := SUBSTR(cLine,j+1,1)
							IF( cChar == '\' .and. cChar2 $ '0123' )
								//OCTAL NUMBER < 377 (255 decimal)
								nAsc := Octal2Num(SUBSTR(cLine,j+1,3) )
								IF( nAsc >= 0 )
									cCode += CHR( nAsc )
								ELSE
									cCode := ""
									EXIT	//ignore errorneous code
								ENDIF
								j += 4
							ELSEIF( cChar2 == 'X' .OR. cChar2 == 'x' )
								//Hex number
								nAsc := Hex2Num( SUBSTR(cLine, j+2, 2 ) )
								IF( nAsc >=0 )
									cCode += CHR( nAsc )
								ELSE
									cCode := ""
									EXIT	//ignore errorneous code
								ENDIF
								j += 4
							ELSEIF( cChar2 == '\' )
								cCode += '\'
								j += 2
							ELSE
								cCode += cChar
								++j
							ENDIF
						ENDDO
						IF( !EMPTY(cCode) .AND. nKey != 0 )
							HB_GT_ADDKEYMAP( nKey, cCode )
						ENDIF
					ENDIF
				ENDIF
			NEXT
		ENDIF
	ENDIF
	
RETURN

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
