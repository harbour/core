PROCEDURE Main()

   Set( _SET_DATEFORMAT, "yyyy-mm-dd" )

   ? "Should be 2005-11-12:", 0d20051112
   ? "Should be 1834-11-12:", 0d18341112
   ? "Should be 2004-03-01:", 0d20040229 + 1
   ? "Should be 2004-02-28:", 0d20040229 - 1
   ? "Should be 4:", 0d20040229 - 0d20040225
   ? "Should be 0:", 0d20040229 - 0d20040229
   ? "Should be 2000-02-29:", 0d20000229
   ? "Should be 2004-03-01:", &( "0d20040229 + 1" )
   ? "Number of days since 2005-11-12:", &( "Date() - 0d20051112" )

   RETURN
