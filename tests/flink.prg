/*
 * Harbour Project source code:
 *
 * Copyright 2010 Viktor Szakats (vszakats.net/harbour)
 * www - http://harbour-project.org
 *
 */

#include "simpleio.ch"

PROCEDURE Main()

   hb_MemoWrit( "_hb_h.tmp", ">h.tmp<" )

   ? hb_FLink()
   ? FError()

   ? hb_FLinkRead()
   ? FError()

   ? hb_FLinkRead( "_hb_h.tmp" )
   ? FError()

   ? hb_FLink( "_hb_h.tmp", "_hb_hlnk.tmp" )
   ? FError()

   hb_MemoWrit( "_hb_s.tmp", ">s.tmp<" )

   /* Requires special rights on Windows platform,
      by default Administrators are allowed. */

   ? hb_FLinkSym()
   ? FError()

   ? hb_FLinkSym( "_hb_s.tmp", "_hb_slnk.tmp" )
   ? FError()

   ? hb_FLinkRead( "_hb_slnk.tmp" )
   ? FError()

   hb_DirCreate( "_hb_d" )

   ? hb_FLinkSym( "_hb_d", "_hb_dlnk" )
   ? FError()

   ? hb_FLinkRead( "_hb_dlnk" )
   ? FError()

   RETURN
