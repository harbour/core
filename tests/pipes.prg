/*
 * demonstration/test code for communication
 * between processes by unnamed pipes.
 *
 * Copyright 2016 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *
 */

#xcommand ? [<cmd,...>]   => msg( <cmd> )
request HB_GT_CGI_DEFAULT

static s_lChild := .F.

procedure Main( xChild )

   local hProcess, hStdIn, hStdOut
   local n, nTimeout, nWrLim, nSent, nRead
   local cToSend, cBuffer, cProg
   local cSrvData, cCliData, cRead, cError

   s_lChild := ! Empty( xChild )

   nTimeout := 1000

#if defined( __PLATFORM__UNIX )
   n := 10000
   nWrLim := 100000
#else
   n := 1000
   nWrLim := 10000
// nWrLim := 4096
#endif
   cToSend := "ABCDEFGHIJKLMNOP"
   cSrvData := Replicate( Left( cToSend, 16 - Len( hb_eol() ) ) + hb_eol(), n )
   cCliData := Lower( cSrvData  )
   cBuffer := Space( 10000000 )

   if s_lChild
      ? "process started"
      if xChild == "STDOUT"
         ? "this is OUTPUT only test"
         OutStd( "Child is happy to send this data :-)" + hb_eol() )
         OutStd( "Bye Bye" + hb_eol() )
         ErrorLevel( 123 )
         return
      endif
      ? "waiting..."
      hb_idleSleep( 2 )

      ? "reading..."
      cRead := ""
      nRead := 0
      while .t.
         if ( n := hb_PRead( 0, @cBuffer,, nTimeout ) ) == -1
            ? "error during reading:", FError()
            exit
         else
            cRead += hb_BLeft( cBuffer, n )
            nRead += n
            ? "bytes read:", n, ", total:", nRead, ", error:", FError()
         endif
      enddo
      ? "READ DONE =>", iif( cRead == cSrvData, "OK", "ERR" )
      ? "writing..."
      cToSend := cCliData
      nSent := 0
      while ! HB_ISNULL( cToSend )
         if ( n := hb_PWrite( 1, cToSend, nWrLim, nTimeout ) ) == -1
            ? "error during writing:", FError()
            exit
         else
            nSent += n
            ? "bytes sent:", n, ", total:", nSent, ", error:", FError()
            if n > 0
               cToSend := hb_BSubStr( cToSend, n + 1 )
            endif
            if nWrLim != NIL
               hb_idleSleep( 0.1 )
            endif
         endif
      enddo
   else
      cProg := hb_ProgName() + " x"

      ? Replicate( "=", 40 )

      ? "process started"
      ? "opening child process:", cProg
      if ( hProcess := hb_processOpen( cProg, @hStdIn, @hStdOut ) ) == -1
         ? "Cannot create child process..."
      else
         ? "child started, PID =", hProcess, ", stdin =", hStdIn, ", stdout =", hStdOut
         ? "writing..."
         cToSend := cSrvData
         nSent := 0
         while ! HB_ISNULL( cToSend )
            if ( n := hb_PWrite( hStdIn, cToSend,, nTimeout ) ) == -1
               ? "error during writing:", FError()
               exit
            else
               nSent += n
               ? "bytes sent:", n, ", total:", nSent, ", error:", FError()
               if n > 0
                  cToSend := hb_BSubStr( cToSend, n + 1 )
               endif
            endif
         enddo
         FClose( hStdIn )
         ? "WRITE DONE"
         ? "reading..."
         cRead := ""
         nRead := 0
         while .t.
            if ( n := hb_PRead( hStdOut, @cBuffer,, nTimeout ) ) == -1
               ? "error during reading:", FError()
               exit
            else
               cRead += hb_BLeft( cBuffer, n )
               nRead += n
               ? "bytes read:", n, ", total:", nRead, ", error:", FError()
            endif
         enddo
         ? "READ DONE =>", iif( cRead == cCliData, "OK", "ERR" )
         FClose( hStdOut )
         ? "process result:", hb_processValue( hProcess )
      endif

      ? Replicate( "=", 40 )

      ? "running child process:", cProg
      ? "result:", hb_processRun( cProg, cSrvData, @cRead )
      ? "READ DONE =>", iif( cRead == cCliData, "OK", "ERR" )

      ? Replicate( "=", 40 )

      ? "running child process with error redirection:", cProg
      n := hb_processRun( cProg, cSrvData, @cRead, @cError )
      OutErr( cError )
      ? "result:", n
      ? "READ DONE =>", iif( cRead == cCliData, "OK", "ERR" )

      ? Replicate( "=", 40 )

      ? "running child process with mixed redirection:", cProg
      n := hb_processRun( cProg, cSrvData, @cRead, @cRead )
      /* divide stdout and stderr lines */
      cBuffer := cRead
      cRead := cError := ""
      for each cBuffer in hb_ATokens( cBuffer, .T. )
         if " CHILD: " $ cBuffer
            cError += cBuffer + hb_eol()
         elseif ! cBuffer == "" .or. ! cBuffer:__enumIsLast()
            cRead += cBuffer + hb_eol()
         endif
      next
      OutErr( cError )
      ? "result:", n
      ? "READ DONE =>", iif( cRead == cCliData, "OK", "ERR" )

      ? Replicate( "=", 40 )

      cProg := hb_ProgName() + " STDOUT"
      ? "running child process only with stdout redirected:", cProg
      ? "result:", hb_processRun( cProg,, @cRead )
      ? "STDOUT =>"
      ? cRead

      ? Replicate( "=", 40 )

      ? "running child process with stdout and stderr redirected:", cProg
      ? "result:", hb_processRun( cProg,, @cRead, @cError )
      ? "STDOUT =>"
      ? cRead
      ? "STDERR =>"
      ? cError

      ? Replicate( "=", 40 )
   endif

   return

static procedure msg( ... )

   local param, cPar, cMsg

   cMsg := hb_TSToStr( hb_DateTime() ) + " " + iif( s_lChild, "CHILD: ", "PARENT:" )
   for each param in { ... }
      cPar := hb_CStr( param )
      cMsg += iif( hb_LeftEq( cPar, "," ), "", " " ) + LTrim( cPar )
   next
   OutErr( cMsg + hb_eol() )

   return
