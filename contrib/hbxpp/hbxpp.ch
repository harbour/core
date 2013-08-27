/*
 * Harbour Project source code:
 * Xbase++ compatibility header
 *
 * Copyright 2009 Viktor Szakats (harbour syenar.net)
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

/* NOTE: Add this header before regular Harbour headers. */

#ifdef __XPP__

/* Translations for Harbour programs compiled with Xbase++ compiler */

#else
#ifdef __HARBOUR__

/* Translations for Xbase++ programs compiled with Harbour compiler */

#include "hbclass.ch"

#if ! defined( XPP_OP_OVERLOAD_OFF )
   #pragma -ks+
   REQUEST xpp_op_Overload
#endif

#xtranslate IsPrinter( [<x>] )     => hb_IsPrinter( <x> )
#xtranslate DispOutAt( <x,...> )   => hb_DispOutAt( <x> )
#xtranslate AAdd( <a>, <v>, <p> )  => hb_AIns( <a>, <v>, <p>, .T. )

#xtranslate TBColumn()             => xpp_TBColumn()
#xtranslate TBrowse()              => xpp_TBrowse()
#xtranslate Get()                  => xpp_Get()

/* NOTE: Several other Xbase++ functions are currently implemented using
         original Xbase++ names. */

/* box.ch extensions */

#define B_THIN          ( Chr( 219 ) + Chr( 223 ) + Chr( 219 ) + Chr( 219 ) + ;
                          Chr( 219 ) + Chr( 220 ) + Chr( 219 ) + Chr( 219 ) )

#define B_FAT           ( Chr( 219 ) + Chr( 219 ) + Chr( 219 ) + Chr( 219 ) + ;
                          Chr( 219 ) + Chr( 219 ) + Chr( 219 ) + Chr( 219 ) )

#define HB_B_THIN_UNI   hb_UTF8ToStrBox( "█▀███▄██" )
#define HB_B_FAT_UNI    hb_UTF8ToStrBox( "████████" )

/* TEXT INTO <varname> [WRAP [<cEOL>]] [TRIMMED]  */
#xcommand TEXT INTO <v> [<wrp:WRAP>] [<trm:TRIMMED>] => ;
            #pragma __text | <v> += iif( <.trm.>, LTrim( %s ), %s ) +;
                                    iif( <.wrp.>, hb_eol(), "" ); <v> := ""
#xcommand TEXT INTO <v> WRAP [<EOL>] [<trm:TRIMMED>] => ;
            #pragma __text | <v> += iif( <.trm.>, LTrim( %s ), %s ) +;
                                    iif( <.EOL.>, <EOL>, hb_eol() ); <v> := ""

#endif
#endif
