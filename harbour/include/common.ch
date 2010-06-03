/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for common macros
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
 * www - http://harbour-project.org
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

#ifndef HB_COMMON_CH_
#define HB_COMMON_CH_

/* Friendly logical aliases */
#define TRUE                    .T.
#define FALSE                   .F.
#define YES                     .T.
#define NO                      .F.

/* Type checking macros */
#translate ISNIL( <xValue> )       => ( <xValue> == NIL )
#translate ISARRAY( <xValue> )     => hb_isArray( <xValue> )
#translate ISBLOCK( <xValue> )     => hb_isBlock( <xValue> )
#translate ISCHARACTER( <xValue> ) => hb_isString( <xValue> )
#translate ISDATE( <xValue> )      => hb_isDate( <xValue> )
#translate ISLOGICAL( <xValue> )   => hb_isLogical( <xValue> )
#translate ISMEMO( <xValue> )      => hb_isMemo( <xValue> )
#translate ISNUMBER( <xValue> )    => hb_isNumeric( <xValue> )
#translate ISOBJECT( <xValue> )    => hb_isObject( <xValue> )

/* DEFAULT and UPDATE commands */
#xcommand DEFAULT <v1> TO <x1> [, <vn> TO <xn> ] => ;
                                IF <v1> == NIL ; <v1> := <x1> ; END ;
                                [; IF <vn> == NIL ; <vn> := <xn> ; END ]

#command UPDATE <v1> IF <exp> TO <v2> => ;
                                IF <exp> ; <v1> := <v2> ; END

/* HASH autoadd options */
#define HB_HAUTOADD_NEVER       0x00
#define HB_HAUTOADD_ACCESS      0x01
#define HB_HAUTOADD_ASSIGN      0x02
#define HB_HAUTOADD_ALWAYS      ( HB_HAUTOADD_ACCESS + HB_HAUTOADD_ASSIGN )
#define HB_HAUTOADD_REFERENCE   HB_HAUTOADD_ALWAYS

/* HB_HMERGE() modes */
#define HB_HMERGE_UNION         0  /* logical OR  on items in two hash tables (default) */
#define HB_HMERGE_INTERSECT     1  /* logical AND on items in two hash tables */
#define HB_HMERGE_DIFFERENCE    2  /* logical XOR on items in two hash tables */
#define HB_HMERGE_REMOVE        3  /* h1 & ( h1 ^ h2 ) */

#endif /* HB_COMMON_CH_ */
