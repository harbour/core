/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * cgi.ch common includes for cgi lib
 *
 * Copyright 2000 Manos Aspradakis <maspr@otenet.gr>
 * www - http://harbour-project.org
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
 * www - http://harbour-project.org
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 *    Porting this library to Harbour
 *
 * See doc/license.txt for licensing terms.
 *
 */

#xtranslate DEFAULT( <p>, <v> )  => <p> := iif( <p> == NIL, <v>, <p> )
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

#define _HTML_SPACE  Chr( 38 ) + "nbsp;"

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
