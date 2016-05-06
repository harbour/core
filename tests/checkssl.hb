#!/usr/bin/env hbmk2

/* Copyright 2014-2016 Viktor Szakats (vszakats.net/harbour) */

/* Fetch all http URLs from source base and check
   whether the same URL is available via https.

   REQUIRES: GNU grep and curl (built with SSL) in PATH

   NOTE: curl may accept a different set of certificates
         than web browsers. */

procedure Main( cURLList )

   local url
   local outs, outp, err
   local cnts := 0, cntp := 0
   local f
   local tmp
   local lDebug := .F.

   hb_default( @cURLList, "_url" )

   hb_vfErase( "_cookie" )

   if ! hb_vfExists( cURLList )
      hb_run( 'grep -R -H -I -n -o "http://[a-zA-Z0-9_/\.\~\%\?&\+=@:-]*" * > ' + cURLList )
   endif

   for each url in hb_ATokens( hb_MemoRead( cURLList ), .T. )
      if Len( url := hb_ATokens( url, ":" ) ) >= 4
         if ! Empty( url[ 4 ] ) .and. ;
            ! "apple.com/DTDs/" $ url[ 4 ] .and. ;
            ! Right( url[ 4 ], Len( ".xsd" ) ) == ".xsd" .and. ;
            ! url[ 4 ] $ { "//" => } .and. ;
            ( ! "/3rd/" $ url[ 1 ] .or. "Makefile" $ url[ 1 ] .or. ".hb" $ url[ 1 ] )

            f := StrTran( hb_MemoRead( "." + hb_ps() + url[ 1 ] ), Chr( 13 ) )

            if "xmlns" $ SubStr( f, At( url[ 4 ], f ) - 20, 20 + Len( url[ 4 ] ) )
               if lDebug
                  OutErr( "! XMLNS DETECTED", hb_ValToExp( url ) + hb_eol() )
               endif
               loop
            endif

            switch url[ 3 ]
            case "http"
               if lDebug
                  OutErr( "Checking...", hb_StrToExp( "https:" + url[ 4 ] ) + hb_eol() )
               endif
               hb_processRun( "curl -fsS -v -L --max-redirs 10 --cookie-jar _cookie --connect-timeout 2 " + '"' + "https:" + url[ 4 ] + '"',, @outs, @err )
               if Empty( outs )
                  ++cntp
               else
                  hb_processRun( "curl -fsS -v -L --max-redirs 10 --cookie-jar _cookie --connect-timeout 2 " + '"' + "http:" + url[ 4 ] + '"',, @outp, @err )
                  OutStd( url[ 1 ], "->", "https:" + url[ 4 ], iif( outs == outp, "[OK]", "[Different content - verify manually]" ) + hb_eol() )
                  if outs == outp
                     ++cnts
                  else
                     ++cntp
                  endif
               endif
               exit
            case "https"
               ++cnts
               exit
            endswitch
         endif
      endif
      if lDebug .and. url:__enumIndex() > 10
         exit
      endif
   next

   hb_vfErase( "_cookie" )

   OutStd( "https:", hb_ntos( cnts ) + hb_eol() )
   OutStd( "http-only:", hb_ntos( cntp ) + hb_eol() )

   return
