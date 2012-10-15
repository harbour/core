/*
 * $Id$
 */

/*
 * This file tests support for passing object methods and vars
 * using macro syntax
 */

PROCEDURE Main()

   LOCAL obj := ErrorNew()

   MEMVAR m_send1
   MEMVAR m_send2

   PRIVATE m_send1 := "_description"
   PRIVATE m_send2 := "_tries"

   obj:tries := 1
   obj:&m_send1 := "test"

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
      :&m_send2 += 1
      :&m_send2++
      ++:&m_send2
      ++:&( m_send2 )

      :&( m_send2 ) := :&( SubStr( m_send2, 2 ) ) + 1

      :&m_send1 += " description"
      :&( m_send1 ) += " of "
   ENDWITH

   obj:&( "_" + SubStr( m_send1, 2 ) ) += "Error object"
   ? m_send1, "=", obj:&( SubStr( m_send1, 2 ) )
   ? m_send2, "=", obj:tries

   RETURN
