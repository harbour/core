hb_FCopy( "_pp_test.prg", "_pp_hb.prg" )
hb_FCopy( "_pp_test.prg", "_pp_cl.prg" )

hb_run( "harbour -p -s -n _pp_hb.prg" )
hb_run( "clipper.exe _pp_cl.prg /p /s /n" )

FErase( "_pp_hb.prg" )
FErase( "_pp_cl.prg" )

hb_run( "diff -u -w _pp_hb.ppo _pp_cl.ppo > pp_test.dif" )

FErase( "_pp_hb.ppo" )
FErase( "_pp_cl.ppo" )
