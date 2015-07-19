/* Copyright 2014 Viktor Szakats (vszakats.net/harbour) */

/* Fetch all http URLs from source base and check
   whether the same URL is available via https.

   REQUIRES: GNU grep and curl (built with SSL) in PATH

   NOTE: curl may accept a different set of certificates
         than web browsers. */

procedure Main( cURLList )

   local url
   local outs, outp, err
   local cnts := 0, cntp := 0

   hb_default( @cURLList, "_url" )

   if ! hb_vfExists( cURLList )
      hb_run( 'grep -R -H -I -n -o "http://[a-zA-Z0-9/\.\~\%\?&\+=@:-]*" * > ' + cURLList )
   endif

   for each url in hb_ATokens( hb_MemoRead( cURLList ), .T. )
      if Len( url := hb_ATokens( url, ":" ) ) >= 4
         if ! Empty( url[ 4 ] ) .and. ;
            ! url[ 4 ] $ { "//" => } .and. ;
            ( ! "/3rd/" $ url[ 1 ] .or. "Makefile" $ url[ 1 ] .or. ".hb" $ url[ 1 ] )

            switch url[ 3 ]
            case "http"
               OutErr( "Checking...", hb_StrToExp( "https:" + url[ 4 ] ) + hb_eol() )
               hb_processRun( "curl -v -L --connect-timeout 2 " + '"' + "https:" + url[ 4 ] + '"',, @outs, @err )
               if Empty( outs )
                  ++cntp
               else
                  hb_processRun( "curl -v -L --connect-timeout 2 " + '"' + "http:" + url[ 4 ] + '"',, @outp, @err )
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
   next

   OutStd( "https:", hb_ntos( cnts ) + hb_eol() )
   OutStd( "http-only:", hb_ntos( cntp ) + hb_eol() )

   return
