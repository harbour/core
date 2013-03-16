
PROCEDURE Main()

   LOCAL   GetList := {}, cVar := "Hello"

   MEMVAR  m_aVar, m_nIndex, m_cMacro, m_cEarly, m_cEarly2, m_cLate
   PRIVATE m_aVar := { "World", "Again" }, m_nIndex := 1, m_cMacro := "m_cEarly", m_cEarly := { "Early" }, m_cLate := "Late!", m_cEarly2 := { "Early2" }

   CLS

   ? "2nd GET should say 'Early'."

   @ 10, 10 SAY "cVar            :" GET cVar PICTURE "@K!"
   @ 12, 10 SAY "m_cMacro[1]     :" GET &m_cMacro[ 1 ]
   @ 14, 10 SAY "m_cMacro.2[1]   :" GET &m_cMacro.2[ 1 ]
   @ 16, 10 SAY "m_cEarly[1]     :" GET m_cEarly[ 1 ]
// @ 14, 10 SAY "m_cMacro        :" GET &( m_cMacro )[ 1 ]
   m_nIndex := 2
   @ 18, 10 SAY "m_aVar          :" GET m_aVar[ m_nIndex ]
   @ 20, 10 SAY "Picture of GET-1:" GET GetList[ 1 ]:Picture
   m_nIndex := 3
   m_cMacro := "m_cLate"
   READ

   CLS

/* Clipper Error "Get contains complex macro"
   ? "This GET should say 'Late!'."
   m_cMacro := "m_cEarly"
   @ 10, 10 SAY "m_cMacro          :" GET &( m_cMacro )
   m_cMacro := "m_cLate"
   READ
*/

   RETURN
