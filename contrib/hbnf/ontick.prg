/* This is an original work from 2011 by Viktor Szakats (vszakats.net/harbour)
   and is placed in the public domain. */

THREAD STATIC t_bOnTick
THREAD STATIC t_nTickInterval := 0
THREAD STATIC t_nLastCheck := 0
THREAD STATIC t_hIdle

STATIC PROCEDURE __ft_OnTick()

   IF hb_MilliSeconds() >= ( t_nLastCheck + t_nTickInterval )
      t_nLastCheck := hb_MilliSeconds()
      Eval( t_bOnTick )
   ENDIF

   RETURN

PROCEDURE ft_OnTick( bOnTick, nTickInterval )

   /* HB_EXTENSION: Harbour will also accept function pointers */
   IF HB_ISEVALITEM( bOnTick )
      t_bOnTick := bOnTick
      IF HB_ISNUMERIC( nTickInterval )
         t_nTickInterval := ( 1 / 18.20648 ) * nTickInterval * 1000
      ENDIF
      t_nLastCheck := hb_MilliSeconds()
      IF Empty( t_hIdle )
         t_hIdle := hb_idleAdd( {|| __ft_OnTick() } )
      ENDIF
   ELSE
      t_bOnTick := NIL
      t_nTickInterval := 0
      IF ! Empty( t_hIdle )
         hb_idleDel( t_hIdle )
         t_hIdle := NIL
      ENDIF
   ENDIF

   RETURN
