PROCEDURE Main()

   LOCAL lIgnored

#ifdef __PLATFORM__LINUX
   lIgnored := hb_defaultValue( unix_sigactionget( "SIGPIPE" ), .T. )
   ? "SIGPIPE was ignored:", lIgnored
   IF ! lIgnored
#endif
      IF posix_signal( "PIPE", .T. ) == NIL
         IF posix_errno() <> 0
            ? "posix errno:", posix_errno()
         ENDIF
      ENDIF
#ifdef __PLATFORM__LINUX
   ENDIF
#endif

   ? posix_kill( posix_getpid(), "PIPE" )
   ? "survived"

   RETURN
