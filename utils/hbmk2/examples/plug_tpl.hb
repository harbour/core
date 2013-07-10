/*
 * hbmk2 plugin example
 *
 * Copyright 2010-2013 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
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
 * their web site at https://www.gnu.org/).
 *
 */

/* NOTE: Code must be written in reentrant way. IOW do not use
         PUBLIC, PRIVATE and STATIC variables. Also do not use
         temporary disk files with non-random names. [vszakats] */

#pragma -w3

#if defined( __HBSCRIPT__HBMK_PLUGIN )

FUNCTION hbmk_plugin_tpl( hbmk )

   LOCAL tmp

   IF hbmk[ "lTRACE" ]
      hbmk_OutStd( hbmk, "@@ Entered plugin: " + hbmk[ "cSTATE" ] )
   ENDIF

   SWITCH hbmk[ "cSTATE" ]
   CASE "pre_all"

      FOR EACH tmp IN hbmk[ "params" ]
         hbmk_OutStd( hbmk, hb_StrFormat( "Parameter #%1$d: '%2$s'", tmp:__enumIndex(), tmp ) )
      NEXT
      EXIT

   CASE "pre_c"
      hbmk[ "vars" ][ "MyVar" ] := "Hello world!"
      EXIT

   CASE "post_all"
      hbmk_OutStd( hbmk, "POST_ALL: " + hbmk[ "vars" ][ "MyVar" ] )
      EXIT

   ENDSWITCH

   RETURN NIL

#else

PROCEDURE Main()

   ?? "Cannot be run in standalone mode. Use it with -plugin= option of hbmk2."
   ?

   RETURN

#endif
