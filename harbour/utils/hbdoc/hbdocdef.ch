/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file common definition of HBDOc
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
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
#ifndef _HBDOCDEF_CH_
#define _HBDOCDEF_CH_

#define CRLF HB_OSNewLine()
//  The delimiter
#define DELIM   "$"                 // keyword delimiter

#xtranslate UPPERLOWER(<exp>) => (UPPER(SUBSTR(<exp>,1,1))+LOWER(SUBSTR(<exp>,2)))
#define D_NORMAL  1
#define D_ARG     2
#define D_SYNTAX  3
#define D_IGNORE  4
#define D_SEEALSO 5
#define D_INCLUDE 6
#define D_ONELINE 7
#define D_STATUS  8
#define D_DATALINK 10
#define D_METHODLINK 11
#define D_EXAMPLE 12
#define D_DESCRIPTION 13
#define D_RETURN 14
#define D_COMPLIANCE 15
#define pDOS_HANDLE 1
#define pDOS_FILE   2
#define pDOS_PATH   3
#define pARRAY     "A"
#define pBLOCK     "B"
#define pCHARACTER "C"
#define pDATE      "D"
#define pLOGICAL   "L"
#define pMEMO      "M"
#define pNUMERIC   "N"
#define pOBJECT    "O"
#define pTRUE .t.
#define pFALSE .f.
#define pCRLF HB_OSNEWLINE()

#xtranslate DOSFILENAME(<c>) => substr( <c>, rat("\",<c>)+1 )


#xcommand IF <var> IS <type>          => if valtype(<var>) = <type>
#xcommand IF <var> IS NOT <type>      => if valtype(<var>) != <type>
#xcommand IF <ele> IS IN <array>      => if !(len( <array> ) \< <ele> )
#xcommand IF <ele> IS NOT IN <array>  => if len( <array> ) \< <ele>
#xtranslate (<var> IS <type> )     => (  valtype(<var>) = <type> )
#xtranslate (<var> IS NOT <type>)  => (  valtype(<var>) != <type> )
#define pBUFFER_LENGTH 4096
#endif
