#!/usr/bin/env hbmk2

hb_vfCopyFile( "_pp_test.prg", "_pp_hb.prg" )
hb_vfCopyFile( "_pp_test.prg", "_pp_cl.prg" )

hb_run( "harbour _pp_hb.prg -p -s -n" )
hb_run( "clipper.exe _pp_cl.prg /p /s /n" )

hb_vfErase( "_pp_hb.prg" )
hb_vfErase( "_pp_cl.prg" )

hb_run( "diff -u -w _pp_hb.ppo _pp_cl.ppo > pp_test.dif" )

hb_vfErase( "_pp_hb.ppo" )
hb_vfErase( "_pp_cl.ppo" )
