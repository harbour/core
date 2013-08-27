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

#ifndef HB_COMMON_CH_
#define HB_COMMON_CH_

#if defined( HB_LEGACY_LEVEL4 )
#  include "hbhash.ch"
#endif

/* Friendly logical aliases */
#define TRUE                    .T.
#define FALSE                   .F.
#define YES                     .T.
#define NO                      .F.

/* Type checking macros */
#translate ISNIL( <xValue> )       => ( <xValue> == NIL )
#translate ISARRAY( <xValue> )     => HB_ISARRAY( <xValue> )
#translate ISBLOCK( <xValue> )     => HB_ISBLOCK( <xValue> )
#translate ISCHARACTER( <xValue> ) => HB_ISSTRING( <xValue> )
#translate ISDATE( <xValue> )      => HB_ISDATE( <xValue> )
#translate ISLOGICAL( <xValue> )   => HB_ISLOGICAL( <xValue> )
#translate ISMEMO( <xValue> )      => HB_ISMEMO( <xValue> )
#translate ISNUMBER( <xValue> )    => HB_ISNUMERIC( <xValue> )
#translate ISOBJECT( <xValue> )    => HB_ISOBJECT( <xValue> )

/* DEFAULT and UPDATE commands */
#xcommand DEFAULT <v1> TO <x1> [, <vn> TO <xn> ] => ;
                                IF <v1> == NIL ; <v1> := <x1> ; END ;
                                [; IF <vn> == NIL ; <vn> := <xn> ; END ]

#command UPDATE <v1> IF <exp> TO <v2> => ;
                                IF <exp> ; <v1> := <v2> ; END

#endif /* HB_COMMON_CH_ */
