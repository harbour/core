PROCEDURE Main()

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   USE test.dbf READONLY

   REPORT FORM test.frm

   LABEL FORM test.lbl

   RETURN
