/*
 * Harbour Project source code:
 *
 * Copyright 2010 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
 *
 */

#require "hbwin"

PROCEDURE Main()

   ? win_ReportEvent( NIL, "Application", WIN_EVENTLOG_SUCCESS, 0, 0, "hello" )
   ? win_ReportEvent( NIL, "Application", WIN_EVENTLOG_SUCCESS, 0, 0, { "hello", "world" } )

   RETURN
