// ; Author: "Michael Mozina" <et@snowcrest.net>
// ; Donated to the Public Domain.

// ; Decimals not supported

FUNCTION NumToTxtEN( nValue )

   LOCAL cRetVal := ""

   IF nValue == 0
      RETURN "zero"
   ENDIF

   IF nValue < 0
      nValue := -nValue
      cRetVal += "minus "
   ENDIF

   IF nValue >= 1000000
      IF nValue >= 100000000
         cRetVal += int_to_string( Int( nValue / 100000000 ) ) + " hundred "
         nValue -= 100000000 * Int( nValue / 100000000 )
      ENDIF
      IF nValue >= 1000000
         cRetVal += int_to_string( Int( nValue / 1000000 ) ) + " "
         nValue -= 1000000 * Int( nValue / 1000000 )
      ENDIF
      cRetVal += "million "
   ENDIF
   IF nValue >= 1000
      IF nValue >= 100000
         cRetVal += int_to_string( Int( nValue / 100000 ) ) + " hundred "
         nValue -= 100000 * Int( nValue / 100000 )
      ENDIF
      IF nValue >= 1000
         cRetVal += int_to_string( Int( nValue / 1000 ) ) + " "
         nValue -= 1000 * Int( nValue / 1000 )
      ENDIF
      cRetVal += "thousand "
   ENDIF
   IF nValue >= 100
      cRetVal += int_to_string( Int( nValue / 100 ) ) + " hundred "
      nValue -= 100 * Int( nValue / 100 )
   ENDIF
   IF nValue >= 1
      cRetVal += int_to_string( Int( nValue ) )
      nValue -= Int( nValue )
   ENDIF

   RETURN RTrim( cRetVal )

STATIC FUNCTION int_to_string( nValue )

   LOCAL cRetVal
   LOCAL aArray1 := { ;
      "one", ;
      "two", ;
      "three", ;
      "four", ;
      "five", ;
      "six", ;
      "seven", ;
      "eight", ;
      "nine", ;
      "ten", ;
      "eleven", ;
      "twelve", ;
      "thirteen", ;
      "fourteen", ;
      "fifteen", ;
      "sixteen", ;
      "seventeen", ;
      "eighteen", ;
      "nineteen" }

   LOCAL aArray2 := { ;
      "ten", ;
      "twenty", ;
      "thirty", ;
      "forty", ;
      "fifty", ;
      "sixty", ;
      "seventy", ;
      "eighty", ;
      "ninety" }

   IF nValue < 20
      cRetVal := aArray1[ nValue ]
   ELSE
      cRetVal := aArray2[ Int( nValue / 10 ) ]
      nValue -= 10 * Int( nValue / 10 )
      IF Int( nValue ) >= 1
         cRetVal += " " + aArray1[ Int( nValue ) ]
      ENDIF
   ENDIF

   RETURN cRetVal
