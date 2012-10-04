/*
 * $Id$
 */

REQUEST FCOMMA

PROCEDURE Main()

   USE test.csv VIA "FCOMMA"
   ? "ALIAS", Alias(), "RECNO", RecNo(), ;
      "BOF", BOF(), "EOF", EOF(), "LASTREC", LastRec()
   ? RecNo(), '"' + FIELD->LINE + '"'
   dbGoBottom()
   ? RecNo(), '"' + FIELD->LINE + '"'
   dbGoTop()
   ? RecNo(), '"' + FIELD->LINE + '"'
   WAIT
   DO WHILE !EOF()
      ? RecNo(), '"' + FIELD->LINE + '"'
      IF RecNo() == 20
         Inkey( 0 )
      ENDIF
      dbSkip()
   ENDDO
   ? "ALIAS", Alias(), "RECNO", RecNo(), ;
      "BOF", BOF(), "EOF", EOF(), "LASTREC", LastRec()
   WAIT
   dbGoBottom()
   ? "ALIAS", Alias(), "RECNO", RecNo(), ;
      "BOF", BOF(), "EOF", EOF(), "LASTREC", LastRec()
   WAIT
   DO WHILE !BOF()
      ? RecNo(), '[' + FIELD->LINE + ']'
      IF RecNo() == LastRec() - 20
         Inkey( 0 )
      ENDIF
      dbSkip( -1 )
   ENDDO
   ? "ALIAS", Alias(), "RECNO", RecNo(), ;
      "BOF", BOF(), "EOF", EOF(), "LASTREC", LastRec()
   WAIT
   Browse()

   RETURN
