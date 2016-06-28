/*
 * Demonstration/test code for asynchronous screen updating without
 * breaking foreground screen operations.
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *
 */

procedure Main()

   local GetList := {}
   local cVar := Space( 20 )

   cls

   if hb_mtvm()
      hb_threadStart( @thFunc() )
   else
      ? "No MT support in HVM. Clock will not be shown."
      WAIT
   endif

   @ 10, 10 SAY "Insert cVar:" GET cVar
   READ
   SetPos( 12, 0 )
   ? "Result -> [" + cVar + "]"
   wait

   return

static procedure thFunc()

   local cTime

   while .T.
      cTime := DToC( Date() ) + " " + Time()
      /* use hb_DispOutAt() which does not change current default
       * color and cursor position so can be executed without bad
       * side effects for other threads which updates screen.
       * This functions also accepts colors as numeric values.
       * Similar functionality have hb_DispBox() and hb_Scroll().
       * All these functions changes only screen buffer but do not
       * touch cursor position and current color settings.
       */
      hb_DispOutAt( 0, MaxCol() - Len( cTime ) + 1, cTime, "GR+/N" )
      hb_idleSleep( 1 )
   enddo

   return
