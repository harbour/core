#require "hbnf"
#require "hbtest"

PROCEDURE Main()

   HBTEST ft_AcctMonth( 0d19900915 )[ 1 ] IS "199009"
   HBTEST ft_AcctMonth( 0d19900915 )[ 2 ] IS 0d19900902
   HBTEST ft_AcctMonth( 0d19900915 )[ 3 ] IS 0d19900929

   HBTEST ft_AcctMonth( 0d19900915, 5 )[ 1 ] IS "199005"
   HBTEST ft_AcctMonth( 0d19900915, 5 )[ 2 ] IS 0d19900429
   HBTEST ft_AcctMonth( 0d19900915, 5 )[ 3 ] IS 0d19900602

   HBTEST ft_Easter( 1990 ) IS 0d19900415

   HBTEST ft_ElapMin( "1718", "2040" ) IS  202
   HBTEST ft_ElapMin( "2040", "1718" ) IS -202

   HBTEST ft_ElTime( "22:40:12", "23:55:17" ) IS "01:15:05"
   HBTEST ft_ElTime( "23:55:17", "22:40:12" ) IS "01:15:05"

   HBTEST ft_EscCode( "\015" ) IS hb_BChar( 15 )
   HBTEST ft_EscCode( "\\" )   IS "\"

   RETURN
