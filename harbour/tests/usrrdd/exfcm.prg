/*
 * $Id$
 */

REQUEST FCOMMA

PROCEDURE Main()

   USE test.csv VIA "FCOMMA"
   ? "ALIAS", Alias(), "RECNO", RecNo(), ;
      "BOF", Bof(), "EOF", Eof(), "LASTREC", LastRec()
   ? RecNo(), '"' + FIELD->LINE + '"'
   dbGoBottom()
   ? RecNo(), '"' + FIELD->LINE + '"'
   dbGoTop()
   ? RecNo(), '"' + FIELD->LINE + '"'
   WAIT
   DO WHILE ! Eof()
      ? RecNo(), '"' + FIELD->LINE + '"'
      IF RecNo() == 20
         Inkey( 0 )
      ENDIF
      dbSkip()
   ENDDO
   ? "ALIAS", Alias(), "RECNO", RecNo(), ;
      "BOF", Bof(), "EOF", Eof(), "LASTREC", LastRec()
   WAIT
   dbGoBottom()
   ? "ALIAS", Alias(), "RECNO", RecNo(), ;
      "BOF", Bof(), "EOF", Eof(), "LASTREC", LastRec()
   WAIT
   DO WHILE ! Bof()
      ? RecNo(), '[' + FIELD->LINE + ']'
      IF RecNo() == LastRec() - 20
         Inkey( 0 )
      ENDIF
      dbSkip( -1 )
   ENDDO
   ? "ALIAS", Alias(), "RECNO", RecNo(), ;
      "BOF", Bof(), "EOF", Eof(), "LASTREC", LastRec()
   WAIT
   Browse()

   RETURN
