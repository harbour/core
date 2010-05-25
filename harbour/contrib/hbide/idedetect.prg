/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 1999-2010 Viktor Szakats (harbour.01 syenar.hu)
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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

FUNCTION hbmk2_plugin_show_target( hbmk2 ) 

   SWITCH hbmk2[ "cSTATE" ] 
   CASE "post_all" 

      IF hbmk2[ "nErrorLevel" ] == 0 
         hbmk2_OutStd( hbmk2, "@@ TARGET: " + hbmk2[ "cTARGETTYPE" ] + " : " + hbmk2[ "cTARGETNAME" ] ) 
         hbmk2_OutStd( hbmk2, "@@ TARGET (ABSOLUTE): " + hbmk2_PathMakeAbsolute( hbmk2[ "cTARGETNAME" ], hbmk2_Macro( hbmk2, "${hb_curdir}" ) ) ) 
      ENDIF 
      EXIT 

   ENDSWITCH 

   RETURN "" 

