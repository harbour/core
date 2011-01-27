/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour NETIO server management cmdline tool
 *
 * Copyright 2009-2011 Viktor Szakats (harbour.01 syenar.hu)
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
 * their web site at http://www.gnu.org/).
 *
 */

#define _NETIOMGM_IPV4_DEF  "127.0.0.1"
#define _NETIOMGM_PORT_DEF  2940

PROCEDURE Main( ... )
   LOCAL cParam

   LOCAL cIP := _NETIOMGM_IPV4_DEF
   LOCAL nPort := _NETIOMGM_PORT_DEF
   LOCAL cPassword := ""

   SET DATE ANSI
   SET CENTURY ON
   SET CONFIRM ON
   SET SCOREBOARD OFF

   HB_Logo()

   FOR EACH cParam IN { ... }
      DO CASE
      CASE Lower( Left( cParam, 6 ) ) == "-addr="
         hbnetiocon_IPPortSplit( SubStr( cParam, 7 ), @cIP, @nPort )
         IF Empty( nPort )
            nPort := _NETIOMGM_PORT_DEF
         ENDIF
      CASE Lower( Left( cParam, 6 ) ) == "-pass="
         cPassword := SubStr( cParam, 7 )
         hb_StrClear( @cParam )
      CASE Lower( cParam ) == "--version"
         RETURN
      CASE Lower( cParam ) == "-help" .OR. ;
           Lower( cParam ) == "--help"
         HB_Usage()
         RETURN
      OTHERWISE
         OutStd( "Warning: Unkown parameter ignored: " + cParam + hb_eol() )
      ENDCASE
   NEXT

   hbnetiocon_cmdUI( cIP, nPort, cPassword )

   RETURN

STATIC PROCEDURE HB_Logo()

   OutStd( "Harbour NETIO Server Management Console " + StrTran( Version(), "Harbour " ) + hb_eol() +;
           "Copyright (c) 2009-2011, Viktor Szakats" + hb_eol() + ;
           "http://harbour-project.org/" + hb_eol() +;
           hb_eol() )

   RETURN

STATIC PROCEDURE HB_Usage()

   OutStd(               "Syntax:"                                                                                , hb_eol() )
   OutStd(                                                                                                          hb_eol() )
   OutStd(               "  netiocui [options]"                                                                   , hb_eol() )
   OutStd(                                                                                                          hb_eol() )
   OutStd(               "Options:"                                                                               , hb_eol() )
   OutStd(                                                                                                          hb_eol() )
   OutStd(               "  -addr=<ip[:port]>  connect to netio server on IPv4 address <ip:port>"                 , hb_eol() )
   OutStd( hb_StrFormat( "                     Default: %1$s:%2$d", _NETIOMGM_IPV4_DEF, _NETIOMGM_PORT_DEF )      , hb_eol() )
   OutStd(               "  -pass=<passwd>     connect to netio server with password"                             , hb_eol() )
   OutStd(                                                                                                          hb_eol() )
   OutStd(               "  --version          display version header only"                                       , hb_eol() )
   OutStd(               "  -help|--help       this help"                                                         , hb_eol() )

   RETURN
