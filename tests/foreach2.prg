/*
 * Harbour Project source code:
 *    demonstration code for FOR EACH overloading
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://harbour-project.org
 *
 */


#include "hbclass.ch"

proc main()
local e, o

   ? "FOR EACH e IN myclass1()"
   o := myclass1()
   for each e in o
      ? e:__enumIndex(), "=>", e
   next
   ? "FOR EACH e IN myclass1() DESCEND"
   for each e in o descend
      ? e:__enumIndex(), "=>", e
   next

   ?
   ? "FOR EACH e IN myclass2()"
   o:=myclass2()
   for each e in o
      ? e:__enumIndex(), "=>", e
   next
   ? "FOR EACH e IN myclass2() DESCEND"
   for each e in o descend
      ? e:__enumIndex(), "=>", e
   next

return


/* The first form of FOR EACH overloading
 * Can be use when all values can be calculated when FOR EACH starts
 * It overloads __enumStart() message which set base enumerator values.
 * for ascending iteration it sets: {"A","B","C"}
 * and for descending: {1, 2, 3}
 */
CREATE CLASS myclass1
   METHOD __enumStart( enum, lDescend )
END CLASS

METHOD __enumStart( enum, lDescend ) CLASS myclass1
   /* set base value for enumerator */
   /* important: the messages have to be send to the enumerator
    * reference (@enum) not directly to the enumerator enum
    */
   (@enum):__enumBase( iif( lDescend, {1, 2, 3}, {"A","B","C"} ) )
   return .T. /* .F. means stop iteration */


/* The second form of FOR EACH overloading
 * We are overloading the folowing messages:
 *    __enumStart()     // executed when FOR EACH starts
 *    __enumSkip()      // executed on each LOOP
 *    __enumStop()      // executed when FOR EACH stops
 * Such method can be used when we cannot calculate all values
 * when FOR EACH starts or such calculation is expensive so we
 * want to make them dynamically
 */

CREATE CLASS myclass2
   VAR    value
   METHOD __enumStart( enum, lDescend )
   METHOD __enumSkip( enum, lDescend )
   METHOD __enumStop()
END CLASS

METHOD __enumStart( enum, lDescend ) CLASS myclass2
   /* Here we can have initialization code */
   ::value := 0.00
   /* set enumerator value */
   (@enum):__enumValue( ::value )
   /* set enumerator index, to make it more funny for descend
    * iterations we will use negative indexes ;-)
    */
   (@enum):__enumIndex( iif( lDescend, -1, 1 ) )
   return .T. /* continue iteration */

METHOD __enumSkip( enum, lDescend ) CLASS myclass2
   if lDescend
      ::value -= 3
   else
      ::value += 3
   endif
   /* stop iteration when abs( ::value ) > 20 */
   if abs( ::value ) > 20
      return .F.
   endif
   /* set enumerator value */
   (@enum):__enumValue( ::value / 10.0 )
   /* the index is updated automatically but if we want some noncontinuous
    * indexes then here we can set it using (@enum):__enumIndex( nNeIndex )
    * message
    */
   return .T. /* continue iteration */

METHOD PROCEDURE __enumStop() CLASS myclass2
   /* Here we can have cleanup code */
   return
