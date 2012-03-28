/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2010 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
 *
 */

#include "simpleio.ch"

PROCEDURE Main()

   hb_MemoWrit( "_hb_h.tmp", ">h.tmp<" )

   ? HB_FLINK()
   ? FERROR()

   ? HB_FLINKREAD()
   ? FERROR()

   ? HB_FLINKREAD( "_hb_h.tmp" )
   ? FERROR()

   ? HB_FLINK( "_hb_h.tmp", "_hb_hlnk.tmp" )
   ? FERROR()

   hb_MemoWrit( "_hb_s.tmp", ">s.tmp<" )

   /* Requires special rights on Windows system,
      by default Administrators are allowed. */

   ? HB_FLINKSYM()
   ? FERROR()

   ? HB_FLINKSYM( "_hb_s.tmp", "_hb_slnk.tmp" )
   ? FERROR()

   ? HB_FLINKREAD( "_hb_slnk.tmp" )
   ? FERROR()

   hb_DirCreate( "_hb_d" )

   ? HB_FLINKSYM( "_hb_d", "_hb_dlnk" )
   ? FERROR()

   ? HB_FLINKREAD( "_hb_dlnk" )
   ? FERROR()

   RETURN
