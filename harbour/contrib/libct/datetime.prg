/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 Date & Time functions: - BOM() / EOM()
 *                              - BOQ() / EOQ()
 *                              - BOY() / EOY()
 *                              - STOD()
 *
 * Copyright 1999-2001 Marek Horodyski <homar@altkom.com.pl>
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


/*  $DOC$
 *  $FUNCNAME$
 *      BOM()
 *  $CATEGORY$
 *      CT3 date and time functions
 *  $ONELINER$
 *      _B_egin _O_f _M_onth
 *  $SYNTAX$
 *      BOM ([<dDate>]) -> dDateBeginOfMonth
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      BOM() is compatible with CT3's BOM().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is datetime.prg, library is libct.
 *  $SEEALSO$
 *      EOM(),BOQ(),EOQ(),BOY(),EOY()
 *  $END$
 */
Function BOM( date)
 local yyyy
 date := If( ValType( date) == 'D', date, Date())
 if (empty(date))
   return (date)
 endif
 yyyy := Str( Year( date), 4, 0)
 Return StoD( SubStr( DtoS( date), 1, 6) + '01')


/*  $DOC$
 *  $FUNCNAME$
 *      EOM()
 *  $CATEGORY$
 *      CT3 date and time functions
 *  $ONELINER$
 *      _E_nd _O_f _M_onth
 *  $SYNTAX$
 *      EOM ([<dDate>]) -> dDateEndOfMonth
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      EOM() is compatible with CT3's EOM().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is datetime.prg, library is libct.
 *  $SEEALSO$
 *      BOM(),BOQ(),EOQ(),BOY(),EOY()
 *  $END$
 */
Function EOM( date)
 Local m 
 date := If( ValType( date) == 'D', date, Date())
 if (empty(date))
   return (date)
 endif
 m := Month( date)
 While Month( ++date) == m
 End
 Return --date


/*  $DOC$
 *  $FUNCNAME$
 *      BOQ()
 *  $CATEGORY$
 *      CT3 date and time functions
 *  $ONELINER$
 *      _B_egin _O_f _Q_uarter
 *  $SYNTAX$
 *      BOQ ([<dDate>]) -> dDateBeginOfQuarter
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      BOQ() is compatible with CT3's BOQ().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is datetime.prg, library is libct.
 *  $SEEALSO$
 *      BOM(),EOM(),EOQ(),BOY(),EOY()
 *  $END$
 */
Function BOQ( date)
 Local boq, m, yyyy 
 date := If( ValType( date) == 'D', date, Date())
 if (empty(date))
   return (date)
 endif
 yyyy := Str( Year( date), 4, 0)
 If     ( m := Month( date)) <= 3
  boq := StoD( yyyy + '0101')
 ElseIf m <= 6
  boq := StoD( yyyy + '0301')
 ElseIf m <= 9
  boq := StoD( yyyy + '0901')
 Else
  boq := StoD( yyyy + '1201')
 End
Return boq


/*  $DOC$
 *  $FUNCNAME$
 *      EOQ()
 *  $CATEGORY$
 *      CT3 date and time functions
 *  $ONELINER$
 *      _E_nd _O_f _Q_uarter
 *  $SYNTAX$
 *      EOQ ([<dDate>]) -> dDateEndOfQuarter
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      EOQ() is compatible with CT3's EOQ().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is datetime.prg, library is libct.
 *  $SEEALSO$
 *      BOM(),EOM(),BOQ(),BOY(),EOY()
 *  $END$
 */
Function EOQ( date)
 Local m, eoq, yyyy
 date := If( ValType( date) == 'D', date, Date())
 if (empty(date))
  return (date)
 endif
 If     ( m := Month( date)) <= 12
  eoq := StoD( yyyy + '1231')
 ElseIf m <= 9
  eoq := StoD( yyyy + '0930')
 ElseIf m <= 6
  eoq := StoD( yyyy + '0630')
 Else
  eoq := StoD( yyyy + '0331')
 End
 Return eoq


/*  $DOC$
 *  $FUNCNAME$
 *      BOY()
 *  $CATEGORY$
 *      CT3 date and time functions
 *  $ONELINER$
 *      _B_egin _O_f _Y_ear
 *  $SYNTAX$
 *      BOY ([<dDate>]) -> dDateBeginOfYear
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      BOY() is compatible with CT3's BOY().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is datetime.prg, library is libct.
 *  $SEEALSO$
 *      BOM(),EOM(),BOQ(),EOQ(),EOY()
 *  $END$
 */
Function BOY( date)
 date := If( ValType( date) == 'D', date, Date())
 if (empty(date))
  return (date)
 endif
 Return StoD( Str( Year( date), 4, 0) + '0101')


/*  $DOC$
 *  $FUNCNAME$
 *      EOY()
 *  $CATEGORY$
 *      CT3 date and time functions
 *  $ONELINER$
 *      _E_nd _O_f _Y_ear
 *  $SYNTAX$
 *      EOY ([<dDate>]) -> dDateEndOfYear
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      EOY() is compatible with CT3's EOY().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is datetime.prg, library is libct.
 *  $SEEALSO$
 *      BOM(),EOM(),BOQ(),EOQ(),BOY()
 *  $END$
 */
Function EOY( date)
 date := If( ValType( date) == 'D', date, Date())
 if (empty(date))
  return (date)
 endif
 Return StoD( Str( Year( date), 4, 0) + '1231')


/*  $DOC$
 *  $FUNCNAME$
 *      STOD()
 *  $CATEGORY$
 *      CT3 date and time functions
 *  $ONELINER$
 *      Convert ANSI date string to Harbour date
 *  $SYNTAX$
 *      STOD ([<cDate>]) -> dDate
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      STOD() is compatible with CT3's STOD().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is datetime.prg, library is libct.
 *  $SEEALSO$
 *  $END$
 */
Function StoD( cdate)
 Local ofd := Set( _SET_DATEFORMAT, 'dd.mm.yyyy'), rvd 
 cdate := If( ValType( cdate) == 'C', cdate, DtoS( Date()))
 rvd := CtoD( SubStr( cDate, 7, 2) + '.' + SubStr( cDate, 5, 2)  + '.' + SubStr( cDate, 1, 4))
 Set( _SET_DATEFORMAT, ofd)
 Return rvd




