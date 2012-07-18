/*
 * $Id$
 */

#require "hbgt"

PROCEDURE Main()

   QOut( 'gt_ascpos( "Harbour", 1 ) => ' + hb_ntos( gt_ascpos( "Harbour", 1 ) ) )
   QOut( 'gt_atdiff( "This Is Harbour", "This Is Clipper" ) => ' + hb_ntos( gt_atdiff( "This Is Harbour", "This Is Clipper" ) ) )
   QOut( 'gt_chareven( "The_Power_Of_Harbour" ) => ' + gt_chareven( "The_Power_Of_Harbour" ) )
   QOut( 'gt_charodd( "The_Power_Of_Harbour" ) => ' + gt_charodd( "The_Power_Of_Harbour" ) )
   QOut( 'gt_chrcount( "s", "she sells shells by the sea shore" ) => ' + hb_ntos( gt_chrcount( "s", "she sells shells by the sea shore" ) ) )
   QOut( 'gt_chrtotal( "sl", "she sells shells by the sea shore" ) => ' + hb_ntos( gt_chrtotal( "sl", "she sells shells by the sea shore" ) ) )
   QOut( 'gt_charmix( "CLIPPER", "harbour" ) => ' + gt_charmix( "CLIPPER", "harbour" ) )
   QOut( 'gt_asciisum( "harbour" ) => ' + hb_ntos( gt_asciisum( "harbour" ) ) )
   QOut( 'gt_chrfirst( "Ho",  "the power of Harbour" ) => ' + hb_ntos( gt_chrfirst( "Ho", "the power of Harbour" ) ) )
   QOut( 'gt_strcount( "the", "the cat sat on the mat" ) => ' + hb_ntos( gt_strcount( "the", "the cat sat on the mat" ) ) )
   QOut( 'gt_strcspn( "this is a test", "as " ) => ' + hb_ntos( gt_strcspn( "this is a test", "as " ) ) )
   QOut( 'gt_strcspn( "this is a test", "elnjpq" ) => ' + hb_ntos( gt_strcspn( "this is a test", "elnjpq" ) ) )
   QOut( 'gt_strDiff( "the cat", "the rat" ) => ' + gt_strDiff( "the cat", "the rat" ) )
   QOut( 'gt_strexpand( "HARBOUR", 2, "-" ) => ' + gt_strexpand( "HARBOUR", 2, "-" ) )
   QOut( 'gt_strleft( "this is a test", "hsit " ) => ' + hb_ntos( gt_strleft( "this is a test", "hsit " ) ) )
   QOut( 'gt_strpbrk( "this is a test", "sa " )   => ' + gt_strpbrk( "this is a test", "sa " )  )
   QOut( 'gt_strright( "this is a test", "teas " ) => ' + hb_ntos( gt_strright( "this is a test", "teas " ) ) )

   RETURN
