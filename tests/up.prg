/* Copyright 2016 Przemyslaw Czerpak <druzus / at / priv.onet.pl> */

announce HB_GTSYS
request HB_GT_CGI_DEFAULT

procedure Main()

   local cBuffer, cOut, nRead, nWritten

   cBuffer := Space( 1024 )

   while .t.

      // read data from stdin
      nRead := FRead( hb_GetStdIn(), @cBuffer, hb_BLen( cBuffer ) )
      do case
      case nRead == -1
         OutErr( "stdin read error: " + hb_ntos( FError() ) + hb_eol() )
         ErrorLevel( Max( FError(), 1 ) )
         quit
      case nRead == 0  // end of input stream
         exit
      endcase

      // convert data read from stdin
      cOut := Upper( hb_BLeft( cBuffer, nRead ) )

      // write output to stdout
      if hb_BLen( cOut ) > 0 .and. ;
         ( nWritten := FWrite( hb_GetStdOut(), cOut ) ) < hb_BLen( cOut )

         OutErr( "stdout write error: " + hb_ntos( FError() ) + hb_eol() )
         ErrorLevel( Max( FError(), 1 ) )
         quit
      endif
   enddo

   return
