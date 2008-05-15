//
// $Id$
//

// Test program for new libmisc twirler class
/* Harbour Project source code
   http://www.Harbour-Project.org/
   Donated to the public domain on 2001-03-15 by David G. Holm <dholm@jsd-llc.com>
*/

PROCEDURE Main( cFile )
LOCAL counter := 0, start_time, stop_time
LOCAL twirl1 := Twirler():new( 2,  5, "One: " )
LOCAL twirl2 := Twirler():new( 2, 15, "Two: ", , 0.1 )
LOCAL twirl3 := Twirler():new( 2, 30, "Three: ", , 0.25 )
LOCAL twirl4 := Twirler():new( 3,  5, "Four: ", "^)v(", 0.2 )
LOCAL twirl5 := Twirler():new( 3, 15, "Five: ", "^)v(", 0.2 )
LOCAL twirl6 := Twirler():new( 4,  5, "Six: ", ".oOo", 0.2 )
LOCAL twirl7 := Twirler():new( 4, 15, "Seven: ", ".oOo", 0.2 )
    CLS
    SET CURSOR OFF
    @ 1,1 SAY "Twirler test program - press any key to stop"
    twirl1:show()
    twirl2:show()
    twirl3:show()
    twirl4:show()
    twirl5:show()
    twirl6:show()
    twirl7:show()
    start_time := SECONDS()
    WHILE INKEY() == 0
        counter++
        twirl1:twirl()
        twirl2:twirl()
        twirl3:twirl()
        twirl4:twirl()
        twirl6:twirl()
        IF counter % 5 == 0
           twirl5:twirl()
           if counter % 10 == 0
              twirl7:twirl()
           end if
        END IF
    END WHILE
    stop_time := SECONDS()
    twirl7:hide()
    twirl6:hide()
    twirl5:hide()
    twirl4:hide()
    twirl3:hide()
    twirl2:hide()
    twirl1:hide()
    @ 2,5 SAY counter
    @ 3,5 SAY stop_time - start_time
    SET CURSOR ON
QUIT