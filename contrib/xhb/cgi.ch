/*
 * Harbour Project source code:
 * common includes for cgi lib
 *
 * Copyright 2000 Manos Aspradakis <maspr@otenet.gr>
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
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 *    Porting this library to Harbour
 *
 * See COPYING.txt for licensing terms.
 *
 */

#xtranslate Default( <p>, <v> )  => <p> := iif( <p> == NIL, <v>, <p> )
#xtranslate HTMLSpace( <n> )     => Replicate( "&nbsp;", <n> )  //"&#32;"
#xtranslate CRLF()               => Chr( 13 ) + Chr( 10 )

#define STD_IN       0
#define STD_OUT      1

#define _WHITE_BLUE_STYLE   "<!-- "+;
                                " A:visited {text-decoration:none;color:blue;background:none;} "+;
                                " A:link {text-decoration:none;color:blue;background:none;} "+;
                                " A:active {text-decoration:none;color:blue;background:none;} "+;
                                " A:hover {text-decoration:none;color:white;background:blue;} "+;
                                "-->"
#define _BLUE_WHITE_STYLE   "<!-- "+;
                                 "A:visited {text-decoration:none;color:white;background:none;} "+;
                                 "A:link {text-decoration:none;color:white;background:none;} "+;
                                 "A:active {text-decoration:none;color:white;background:none;} "+;
                                 "A:hover {text-decoration:none;color:blue;background:white;}  "+;
                                 "-->"
#define _WHITE_RED_STYLE    "<!-- "+;
                                 "A:visited {text-decoration:none;color:red;background:none;} "+;
                                 "A:link {text-decoration:none;color:red;background:none;} "+;
                                 "A:active {text-decoration:none;color:red;background:none;} "+;
                                 "A:hover {text-decoration:none;color:white;background:red;} "+;
                                 "-->"
#define _WHITE_BLACK_STYLE  "<!-- "+;
                                 "A:visited {text-decoration:none;color:black;background:none;}"+;
                                 "A:link {text-decoration:none;color:black;background:none;} "+;
                                 "A:active {text-decoration:none;color:black;background:none;}"+;
                                 "A:hover {text-decoration:none;color:white;background:black;} "+;
                                 " -->"

#define _HTML_SPACE       "&nbsp;"

#define CLR_LIGHT_YELLOW  "#fffffc0"
#define CLR_DARK_YELLOW   "#fffffcc"
#define CLR_DARKER_YELLOW "#fffff80"
#define CLR_LIGHT_BLUE    "#DEEFEF"
#define CLR_MAGENTA       "#FFD0FF"
#define CLR_CYAN          "#D0FFFFF"
#define CLR_LIGHT_GRAY    "#F0F0F0"

#define CLR_BLACK         "black"
#define CLR_MAROON        "maroon"
#define CLR_GREEN         "green"
#define CLR_OLIVE         "olive"
#define CLR_NAVY          "navy"
#define CLR_PURPLE        "purple"
#define CLR_TEAL          "teal"
#define CLR_GRAY          "gray"
#define CLR_SILVER        "silver"
#define CLR_RED           "red"
#define CLR_LIME          "lime"
#define CLR_YELLOW        "yellow"
#define CLR_BLUE          "blue"
#define CLR_FUCHSIA       "fuchsia"
#define CLR_AQUA          "aqua"
#define CLR_WHITE         "white"
