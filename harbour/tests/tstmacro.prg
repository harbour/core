Function nMyFunc( )

	LOCAL n := 'cVar1'

	PRIVATE cVar1

	&n = 'Simple '
	? M->cVar1

	&( 'cVar' + '1' ) := 'Macro'
	?? M->cVar1

	M->&n = 'Aliased'
	? M->cVar1

	MEMVAR->&( 'cVar' + '1' ) := ' Macro'
	?? M->cVar1

return NIL
