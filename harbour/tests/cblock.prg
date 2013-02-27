/*
 * $Id$
 */

PROCEDURE Main()

   LOCAL i, bBlock[ 3 ]

   MEMVAR  m_Var1, m_Var2, m_Var3, m_Macro
   PRIVATE m_Var1, m_Var2, m_Var3, m_Macro

   m_Var1 := "m_Var1"
   m_Var2 := "m_Var2"
   m_Var3 := "m_Var3"

   FOR i := 1 TO 3
      m_Macro := "m_Var" + Str( i, 1 )
      bBlock[ i ] := {|| &m_Macro }
   NEXT

   ? "Early (Simple):"

   FOR i := 1 TO 3
      ? Eval( bBlock[ i ] )
   NEXT

   FOR i := 1 TO 3
      m_Macro := "m_Var" + Str( i, 1 )
      bBlock[ i ] := {|| &m_Macro + "!" }
   NEXT

   ?
   ? "Early (Complex):"

   FOR i := 1 TO 3
      ? Eval( bBlock[ i ] )
   NEXT

   FOR i := 1 TO 3
      m_Macro := "m_Var" + Str( i, 1 )
      bBlock[ i ] := {|| &( m_Macro ) }
   NEXT

   ?
   ? "Late:"

   FOR i := 1 TO 3
      ? Eval( bBlock[ i ] )
   NEXT

   RETURN
