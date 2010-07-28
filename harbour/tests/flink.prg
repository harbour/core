/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2010 Viktor Szakats (harbour.01 syenar.hu)
 * www - http://harbour-project.org
 *
 */

#include "simpleio.ch"

PROCEDURE Main()

   hb_MemoWrit( "_hb_h.txt", ">h.txt<" )

   ? HB_FLINK()
   ? FERROR(), DOSERROR()

   ? HB_FLINK( "_hb_h.txt", "_hb_hlnk.txt" )
   ? FERROR(), DOSERROR()

   hb_MemoWrit( "_hb_s.txt", ">s.txt<" )

   ? HB_FLINKSYM()
   ? FERROR(), DOSERROR()

   ? HB_FLINKSYM( "_hb_s.txt", "_hb_slnk.txt" )
   ? FERROR(), DOSERROR()

   RETURN
