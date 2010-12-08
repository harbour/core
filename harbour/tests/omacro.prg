/*
 * $Id$
 */

/*
 * This file tests support for passing object methods and vars
 * using macro syntax
 */

PROCEDURE MAIN()
   LOCAL obj:=ErrorNew()
   MEMVAR send1, send2

   PRIVATE send1:="_description"
   PRIVATE send2:="_tries"

   obj:tries := 1
   obj:&send1 := 'test'

   obj:tries += 1
   obj:tries++
   ++obj:tries

   WITH OBJECT obj
      :tries += 1
      :tries++
      ++:tries

/*
   Notice that for post/pre increment decrement operators and
   for assigments (:=,+=,-=,*=,/=) the macro have to
   start from the underscore symbol '_'

   To access the object variable using macro the '_' should be omitted
*/
      :&send2 +=1
      :&send2++
      ++:&send2
      ++:&(send2)

      :&( send2 ) := :&( SUBSTR(send2,2) ) +1

      :&send1 +=' description'
      :&(send1) += ' of '
   ENDWITH

   obj:&( "_"+ SUBSTR(send1,2) ) += "Error object"
   ? send1, "=", obj:&( SUBSTR(send1,2) )
   ? send2, "=", obj:tries

   RETURN
