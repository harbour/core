function main()

   CLS
   @ MAXROW(), 0 SAY "If the GT API was linked in, the rest of the screen should be blank now."
   Pause()
   CLS
   @ 0,0  SAY "01234567890123456789012345678901"
   @ 1,0  SAY "01234567890123456789012345678901"
   @ 2,0  SAY "01234567890123456789012345678901"
   @ 3,0  SAY "01234                      78901"
   @ 4,0  SAY "01234 This is a test.      78901"
   @ 5,0  SAY "01234 This is only a test. 78901"
   @ 6,0  SAY "01234 Had this been a real 78901"
   @ 7,0  SAY "01234 emergency, you would 78901"
   @ 8,0  SAY "01234 be dead now.         78901"
   @ 9,0  SAY "01234                      78901"
   @ 10,0 SAY "01234567890123456789012345678901"
   @ 11,0 SAY "01234567890123456789012345678901"
   @ 12,0 SAY "01234567890123456789012345678901"
   Pause()
   Scroll (1, 1, 11, 30, -2, -5)
   pause()
   Scroll (1, 1, 11, 30, 2, 5)
   pause()
   Scroll (1, 1, 11, 30, -5, 2)
   pause()
   Scroll (1, 1, 11, 30, 7, -12)
   pause()
   Scroll (1, 1, 11, 30)
   pause()

return nil

function pause()
   DevPos (MAXROW() - 2, 0)
   __ACCEPT ("pause: ")
return nil
