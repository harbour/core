// Donated to the public domain by Viktor Szakats (vszakats.net/harbour)

#ifndef __HARBOUR__
#include "clipper.ch"
#endif

MEMVAR m_cLongerNameThen10Chars
#ifdef __HARBOUR__
MEMVAR m_cLongerN
#endif
MEMVAR m_cString
MEMVAR m_nDouble
MEMVAR m_nDoubleH
MEMVAR m_nInt
MEMVAR m_nLong
MEMVAR m_dDate
MEMVAR m_lLogicT
MEMVAR m_lLogicF
MEMVAR m_xStayHer

PROCEDURE Main()

   PRIVATE m_cLongerNameThen10Chars := "Long String Name!"
   PRIVATE m_cString   := "This is a" + Chr( 0 ) + "string to save."
   PRIVATE m_nDouble   := 100.0000
   PRIVATE m_nDoubleH  := 5000000000
   PRIVATE m_nInt      := 35
   PRIVATE m_nLong     := 3000000
   PRIVATE m_dDate     := Date()
   PRIVATE m_lLogicT := .F.
   PRIVATE m_lLogicF := .T.

   SAVE TO memfile ALL
   SAVE TO memsome ALL LIKE "m_nDouble*"

   CLEAR MEMORY

   SAVE TO memempt ALL

   PRIVATE m_xStayHer := "CONST"

   RESTORE FROM memfile ADDITIVE

   ? m_xStayHer
   ? m_cLongerN /* Can't restore the part of the name beyond the tenth char */
   ? m_cString
   ? m_nDouble
   ? m_nDoubleH
   ? m_nInt
   ? m_nLong
   ? m_dDate
   ? m_lLogicF
   ? m_lLogicT

   RESTORE FROM memfile

// ? m_xStayHer
   ? m_cLongerN /* Can't restore the part of the name beyond the tenth char */
   ? m_cString
   ? m_nDouble
   ? m_nDoubleH
   ? m_nInt
   ? m_nLong
   ? m_dDate
   ? m_lLogicF
   ? m_lLogicT

   ? __MRestore( "memfile", .F., "m_nDouble*", .T. )
   ? m_nDouble
   ? m_nDoubleH
   ? __mvScope( "m_nInt" )

   FErase( "memempt.mem" )
   FErase( "memfile.mem" )
   FErase( "memsome.mem" )

   RETURN
