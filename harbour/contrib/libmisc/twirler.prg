#include <hbclass.ch>

class Twirler
   var n_Row
   var n_Col
   var n_Index
   var n_Seconds
   var n_Smooth
   var c_Chars
   var c_Title
   method new( nRow, nCol, cTitle, cChars )
   method twirl()
   method show()
   method hide()
end class

method new( nRow, nCol, cTitle, cChars, nSmooth ) class Twirler
   ::n_Row := nRow
   ::n_Col := nCol
   ::n_Smooth := nSmooth
   ::c_Chars := IF( EMPTY( cChars ), "|/-\", cChars )
   ::c_Title := cTitle
   IF EMPTY( ::c_Title )
      ::c_Title := ""
   END IF
   ::n_Col += LEN( ::c_Title )
return Self

method twirl() class Twirler
local nSeconds := SECONDS()
   IF EMPTY( ::n_Seconds ) .OR. nSeconds - ::n_Seconds >= ::n_Smooth .OR. nSeconds < ::n_Seconds
      @ ::n_Row, ::n_Col SAY SUBSTR( ::c_Chars, ::n_Index, 1 )
      ::n_Index++
      if ::n_Index > LEN( ::c_Chars )
         ::n_Index := 1
      end if
      IF !EMPTY( ::n_Seconds )
         ::n_Seconds := nSeconds
      END IF
   END IF
return Self

method show() class Twirler
   ::n_Index := 1
   IF ! EMPTY( ::n_Smooth )
      ::n_Seconds := -::n_Smooth
   END IF
   @ ::n_Row, ::n_Col - LEN( ::c_Title ) SAY ::c_Title
return Self

method hide() class Twirler
   @ ::n_Row, ::n_Col - LEN( ::c_Title ) SAY SPACE( LEN( ::c_Title ) + 1 )
return Self
