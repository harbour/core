/*
 * $Id$
 */

/* ; Donated to the public domain by Victor Szel <info@szelvesz.hu> */

FUNCTION Main()
   PRIVATE mcString   := "This is a" + Chr(0) + "string to save."
   PRIVATE mnDouble   := 100.0000
   PRIVATE mnDoubleH  := 5000000000
   PRIVATE mnInt      := 35
   PRIVATE mnLong     := 3000000
   PRIVATE mdDate     := Date()
   PRIVATE mlLogicalT := .F.
   PRIVATE mlLogicalF := .T.

   SAVE TO memfile ALL
   SAVE TO memsome ALL LIKE "mnDouble*"

   CLEAR MEMORY

   SAVE TO memempt ALL

   PRIVATE mxStayHere := "CONST"

   RESTORE FROM memfile ADDITIVE

   ? mxStayHere
   ? mcString
   ? mnDouble
   ? mnDoubleH
   ? mnInt
   ? mnLong
   ? mdDate
   ? mlLogicalF
   ? mlLogicalT

   RESTORE FROM memfile

// ? mxStayHere
   ? mcString
   ? mnDouble
   ? mnDoubleH
   ? mnInt
   ? mnLong
   ? mdDate
   ? mlLogicalF
   ? mlLogicalT

   ? __MRestore( "memfile", .F., "mndouble*", .T. )
   ? mnDouble
   ? mnDoubleH
   ? mnInt

   // ; Error cases

   ? __MRestore( "mamfula", .F. )
   ? mxStayHere
// SAVE TO memempt* ALL

   RETURN NIL
