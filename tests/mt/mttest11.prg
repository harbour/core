/*
 * Harbour Project source code:
 *    demonstration/test code for asynchronous screen updating without
 *    breaking foreground screen operations.
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */

proc main()
   local getList := {}
   local cVar := space( 20 )

   CLS

   if ! hb_mtvm()
      ? "No MT support in HVM. Clock will not be shown."
      WAIT
   else
      hb_threadStart( @thFunc() )
   endif

   @ 10, 10 SAY "Insert cVar:" GET cVar
   READ
   SetPos( 12, 0 )
   ? "Result -> [" + cVar + "]"
   WAIT

return

func thFunc()
   local cTime
   while .T.
      cTime := dtoc( date() ) + " " + time()
      /* use hb_dispOutAt() which does not change current default
       * color and cursor position so can be executed without bad
       * side effects for other threads which updates screen.
       * This functions also accepts colors as numeric values.
       * Similar functionality have hb_dispBox() and hb_scroll().
       * All these functions changes only screen buffer but do not
       * touch cursor position and current color settings.
       */
      hb_dispOutAt( 0, maxcol() - len( cTime ) + 1, cTime, "GR+/N" )
      hb_idleSleep( 1 )
   enddo
return nil
