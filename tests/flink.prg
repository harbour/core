/* Copyright 2010 Viktor Szakats (vszakats.net/harbour) */

#include "simpleio.ch"

PROCEDURE Main()

   hb_MemoWrit( "_hb_h.tmp", ">h.tmp<" )

   ? hb_vfLink()
   ? FError()

   ? hb_vfLinkRead()
   ? FError()

   ? hb_vfLinkRead( "_hb_h.tmp" )
   ? FError()

   ? hb_vfLink( "_hb_h.tmp", "_hb_hlnk.tmp" )
   ? FError()

   hb_MemoWrit( "_hb_s.tmp", ">s.tmp<" )

   /* Requires special rights on Windows platform,
      by default Administrators are allowed. */

   ? hb_vfLinkSym()
   ? FError()

   ? hb_vfLinkSym( "_hb_s.tmp", "_hb_slnk.tmp" )
   ? FError()

   ? hb_vfLinkRead( "_hb_slnk.tmp" )
   ? FError()

   hb_vfDirMake( "_hb_d" )

   ? hb_vfLinkSym( "_hb_d", "_hb_dlnk" )
   ? FError()

   ? hb_vfLinkRead( "_hb_dlnk" )
   ? FError()

   RETURN
