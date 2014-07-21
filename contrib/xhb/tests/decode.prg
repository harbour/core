/* Copyright 2006 Francesco Saverio Giudice <info / at / fsgiudice / dot / com> */

#require "hbtest"
#require "xhb"

PROCEDURE Main()

   LOCAL aArray := { 1 }
   LOCAL bBlk := {|| 2 }

   HBTEST hb_Decode( 10 )                                          IS 0     // Single, return empty value
   HBTEST hb_Decode( 3, 1, "A", 2, "B", 3, "C" )                   IS "C"   // A list
   HBTEST hb_Decode( 4, 1, "A", 2, "B", 3, "C", "X" )              IS "X"   // A list with default
   HBTEST hb_Decode( 2, { 1, "A", 2, "B", 3, "C" } )               IS "B"   // Using an array as list of values to check
   HBTEST hb_Decode( 2, { 1, "A", 2, "B", 3, "C", "X" } )          IS "B"   // Using an array with default as list of values to check
   HBTEST hb_Decode( 2, { 1 => "A", 2 => "B", 3 => "C" } )         IS "B"   // Using an hash as list
   HBTEST hb_Decode( 2, 1, {|| 1 }, 2, bBlk, 3, {|| 3 } )          IS bBlk  // Returning a codeblock
   HBTEST hb_Decode( aArray, aArray, "A", { 2 }, "B", { 3 }, "C" ) IS "A"   // Checking an array

   RETURN
