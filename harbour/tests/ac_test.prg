/*
 * $Id$
 */

//+====================================================================
//+
//+    Source Module => ac_test.prg
//+
//+    Released to Public Domain.
//+
//+    Functions: Procedure test()
//+               Function cUserFunction()
//+
//+       Tables: use vendor exclusive
//+
//+    Reformatted by Click! 1.10 on Aug-13-1997 at 11:39 pm
//+
//+====================================================================

#include "achoice.ch"
#include "inkey.ch"

#ifndef __HARBOUR__
#define hb_ntos( n ) LTrim( Str( n ) )
#endif

//+--------------------------------------------------------------------
//+
//+    Procedure test()
//+
//+--------------------------------------------------------------------

PROCEDURE Main()

   LOCAL aPrompts := { ;
      "AGRI-PLANTS"                   , ;
      "ALAN R. SMITH GREENHOUSES"     , ;
      "ALLAN MURRAY NURSERY, INC."    , ;
      "APOPKA FOREST"                 , ;
      "LIGHT HOUSE NURSERIES"         , ;
      "BAUCOM'S"                      , ;
      "BAY HILL NURSERY, INC."        , ;
      "BAYWOOD NURSERIES"             , ;
      "BIG OAK NURSERY"               , ;
      "C & N NURSERY"                 , ;
      "CHARLES QUALITY PLANTS"        , ;
      "CONNELL FARMS"                 , ;
      "DEWAR NURSERIES, INC."         , ;
      "DIAMOND T NURSERY"             , ;
      "DISTINCTIVE PALMS NURSERIES"   , ;
      "DONKAY NURSERY"                , ;
      "DOUG INGRAM & SONS NURSERY"    , ;
      "DRIFTWOOD GARDENS, INC."       , ;
      "ELVA PLANT NURSERY, INC."      , ;
      "ERINON"                        , ;
      "EVANS NURSERY"                 , ;
      "FANCY PLANTS"                  , ;
      "FL.PLANT GROWERSCOOP"          , ;
      "FLORIDA CACTUS INC."           , ;
      "FLOWERING TREE GROWERS, INC."  , ;
      "FLOWERWOOD NURSERY"            , ;
      "FOLIAGE FACTORY TOO"           , ;
      "GATOR GROWERS NURSERY, INC."   , ;
      "GAZEBO LANDSCAPE DESIGN, INC." , ;
      "GEM ORNAMENTALS"               , ;
      "GRANNY'S GARDEN"               , ;
      "GRAY'S ORNAMENTALS"            , ;
      "GREEN MASTERS INC."            , ;
      "GREEN MEADOW NURSERY"          , ;
      "PIXLEY'S PLANT PLACE"          , ;
      "HARRISON'S NURSERY, INC."      , ;
      "G & G FOLIAGE"                 , ;
      "IVEY'S NURSERY, INC."          , ;
      "JB NURSERIES, INC."            , ;
      "JON'S NURSERY"                 , ;
      "JONES & JONES NURSERY, INC."   , ;
      "KAGER'S NURSERY"               , ;
      "KIRKLAND'S NURSERY"            , ;
      "LAND OF BROMELIADS"            , ;
      "LANDSCAPE NURSERY, INC."       , ;
      "LIEWALD'S NURSERY INC."        , ;
      "LLOYD & RINGS NURSERY"         , ;
      "LONG VAN DOUNG"                , ;
      "MAPEL'S LANDSCAPE NURSERY"     , ;
      "MILESTONE AGRICULTURE, INC."   , ;
      "MOJICA NURSERY & FRUITS"       , ;
      "NELSON'S ROSES"                , ;
      "PARK GARDENS"                  , ;
      "PAUL LUKAS INC."               , ;
      "PECKETT'S INC."                , ;
      "PENANG NURSERY, INC."          , ;
      "PINES III NURERIES"            , ;
      "PINEVIEW NURSERY"              , ;
      "POUL JENSEN NURSERY"           , ;
      "R.P. WELKER"                   , ;
      "RICHARD ROGERS NURSERY, INC."  , ;
      "SPRING HILL NURSERY"           , ;
      "T.O. MAHAFFEY, JR."            , ;
      "TUCKER NURSERY"                , ;
      "TURTLE POND NURSERY"           , ;
      "TUTTLE'S NURSERY INC."         , ;
      "VALLEY CACTUS"                 , ;
      "WHISPER WINDS, INC."           , ;
      "WHITE ROSE NURSERIES INC."     , ;
      "WOODWAY"                       , ;
      "FLORI-DESIGN"                  , ;
      "GREEN ACRES FOLIAGE, INC"      , ;
      "FLORAL EXPO"                   , ;
      "TORRES NURSERY"                , ;
      "DARRYL KOON"                   , ;
      "TRISTAR NURSERY"               , ;
      "KAY WEST NURSERY"              , ;
      "JAYMAR NURSERY"                , ;
      "J D F LANDSCAPE NURSERY"       , ;
      "DEROOSE PLANTS, INC."          , ;
      "THE TREEHOUSE"                 , ;
      "COSTELLO'S ARECAS, INC."       , ;
      "FLORICO FOLIAGE"               , ;
      "THE NATIVES"                   , ;
      "GREENS NURSERY"                , ;
      "STEWART NURSERIES"             , ;
      "G & T FOLIAGE, INC."           , ;
      "GOOD TIMES NURSERY"            , ;
      "CONCEPTS IN GREENERY, INC."    , ;
      "DUNN BROTHERS CITRUS, INC."    , ;
      "JOHN PLANK GREENHOUSES"        , ;
      "GREENER PASTURES NURSERY"      , ;
      "MULVEHILL NURSERY"             , ;
      "A NU LEAF"                     , ;
      "IVY DESIGNS, INC."             , ;
      "B & C TROPICALS"               , ;
      "SPANISH RIVER NURSERY, INC."   , ;
      "JACK CHRISTMAS & ASSOCIATES"   , ;
      "SPECIALIST GROWERS"            , ;
      "HOMRICH NURSERY, INC."         , ;
      "COUNTRYSIDE FOLIAGE, INC."     , ;
      "RFJ COMPANY"                   , ;
      "LAKE BRANTLEY PLANT CORP."     , ;
      "MARISTYME"                     , ;
      "MERISTEM NURSERY, INC."        , ;
      "TROPIC DECOR - EARL WILSON"    , ;
      "URQUHART'S NURSERY"            , ;
      "ACE PLANT NURSERY"             , ;
      "HATTAWAYS GREENHOUSE, INC."    , ;
      "Florida Plant Growers"         , ;
      "Junior Nursery"                , ;
      "Fox's Nurseries, Inc."         , ;
      "Vaughan Nursery"               , ;
      "MERRYGRO FARMS"                , ;
      "ALL SEASONS NURSERY"           , ;
      "BENCHMARK FOLIAGE"             , ;
      "SAMMY'S NURSERY"               , ;
      "SUNSHINE GROWERS"              , ;
      "Blooming-Fields Nursery"       }

   LOCAL aPermits := {}
   LOCAL x
   LOCAL nChoice
   LOCAL ncntr

   CLS

   SetColor( "GB+/B,GR+/R,,,W/N" )

   SET CURSOR ON

   ASize( aPermits, Len( aPrompts ) )

   FOR x := 1 TO Len( aPrompts )
      aPermits[ x ] := !( "V" $ aPrompts[ x ] )
   NEXT

#define TEST1
#ifdef TEST1
   FOR ncntr := 1 TO 17
      aPermits[ nCntr ]                       := .F.
      aPermits[ Len( aPrompts ) - nCntr + 1 ] := .F.
   NEXT
   aPermits[ 32 ] := .F.
   aPermits[ 33 ] := .F.
   aPermits[ 34 ] := .F.
#endif

   nChoice := AChoice( 5, 10, 20, 70, aPrompts, aPermits, "cUserFunction" )

   SetPos( 0, 0 )
// CLS
   ? nChoice

   IF nChoice > 0
      ? aPrompts[ nChoice ]
   ENDIF
   SetPos( MaxRow() - 2, 0 )

   RETURN

//+--------------------------------------------------------------------
//+
//+    Function cUserFunction()
//+
//+--------------------------------------------------------------------

FUNCTION cUserFunction( nMode, nCurElement, nRowPos )

   LOCAL nRetVal := AC_CONT                // Default, Continue
   LOCAL nKey    := LastKey()

   LOCAL nRow := Row()
   LOCAL nCol := Col()

   HB_SYMBOL_UNUSED( nRowPos )

   @ 0, 20 SAY Str( nRow, 3 ) + " " + Str( nCol, 3 )

   DO CASE
      // After all pending keys are processed, display message
   CASE nMode == AC_IDLE
      @  0,  0 SAY PadR( hb_ntos( nCurElement ), 10 )
      nRetVal := AC_CONT                   // Continue ACHOICE()
   CASE nMode == AC_HITTOP                 // Attempt to go past Top
      @  0,  0 SAY "Hit Top   "
      // Tone( 100, 3 )
   CASE nMode == AC_HITBOTTOM              // Attempt to go past Bottom
      @  0,  0 SAY "Hit Bottom"
      // Tone( 100, 3 )
   CASE nMode == AC_EXCEPT                 // Key Exception
      @  0,  0 SAY "Exception "
      DO CASE
      CASE nKey == K_RETURN                // If RETURN key, select
         nRetVal := AC_SELECT
      CASE nKey == K_ESC                   // If ESCAPE key, abort
         nRetVal := AC_ABORT
      OTHERWISE
         nRetVal := AC_GOTO                // Otherwise, go to item
      ENDCASE
   ENDCASE

   RETURN nRetVal
