/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Regression tests for the runtime library (file)
 *
 * Copyright 1999 Victor Szel <info@szelvesz.hu>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

#include "rt_main.ch"

#include "fileio.ch"

STATIC scString
STATIC scStringM
STATIC scStringE
STATIC scStringZ
STATIC scStringW
STATIC snIntZ
STATIC snDoubleZ
STATIC snIntP
STATIC snIntP1
STATIC snLongP
STATIC snDoubleP
STATIC snIntN
STATIC snLongN
STATIC snDoubleN
STATIC snDoubleI
STATIC sdDate
STATIC sdDateE
STATIC slFalse
STATIC slTrue
STATIC soObject
STATIC suNIL
STATIC sbBlock
STATIC sbBlockC
STATIC saArray
STATIC saAllTypes

MEMVAR mxNotHere /* Please don't declare this variable, since it's used to test undeclared MEMVAR situations. */
MEMVAR mcLongerNameThen10Chars
MEMVAR mcString
MEMVAR mcStringE
MEMVAR mcStringZ
MEMVAR mcStringW
MEMVAR mnIntZ
MEMVAR mnDoubleZ
MEMVAR mnIntP
MEMVAR mnLongP
MEMVAR mnDoubleP
MEMVAR mnDoubleI
MEMVAR mnIntN
MEMVAR mnLongN
MEMVAR mnDoubleN
MEMVAR mdDate
MEMVAR mdDateE
MEMVAR mlFalse
MEMVAR mlTrue
MEMVAR moObject
MEMVAR muNIL
MEMVAR mbBlock
MEMVAR mbBlockC
MEMVAR maArray

INIT PROCEDURE RT_InitStatics()

   /* NOTE: Some basic values we may need for some tests.
            ( passing by reference, avoid preprocessor bugs, etc. ) */

   scString  := "HELLO"
   scStringM := "Hello"
   scStringE := ""
   scStringZ := "A" + Chr( 0 ) + "B"
   scStringW := Chr(13)+Chr(10)+Chr(141)+Chr(10)+Chr(9)
   snIntZ    := 0
   snDoubleZ := 0.0
   snIntP    := 10
   snIntP1   := 65
   snLongP   := 100000
   snDoubleP := 10.567 /* Use different number of decimals than the default */
   snIntN    := -10
   snLongN   := -100000
   snDoubleN := -10.567 /* Use different number of decimals than the default */
   snDoubleI := 0   //Log( 0 )
   sdDate    := SToD( "19800101" )
   sdDateE   := SToD( "" )
   slFalse   := .F.
   slTrue    := .T.
   soObject  := ErrorNew()
   suNIL     := NIL
   sbBlock   := {|| NIL }
   sbBlockC  := {|| "(string)" }
   saArray   := { 9898 }

   saAllTypes := {;
      scString  ,;
      scStringE ,;
      scStringZ ,;
      snIntZ    ,;
      snDoubleZ ,;
      snIntP    ,;
      snLongP   ,;
      snDoubleP ,;
      snIntN    ,;
      snLongN   ,;
      snDoubleN ,;
      snDoubleI ,;
      sdDateE   ,;
      slFalse   ,;
      slTrue    ,;
      soObject  ,;
      suNIL     ,;
      sbBlock   ,;
      sbBlockC  ,;
      saArray   }

   RETURN

/* NOTE: The order of the tests is relevant here, so don't
         rearrange them. */

FUNCTION Main_FILE()
   LOCAL cFileName := "$$FILEIO.TMP"
   LOCAL nFlags

   LOCAL cBuff4   := Space( 4 )
   LOCAL cBuff100 := Space( 100 )

   LOCAL fhnd

   nFlags := FC_NORMAL
   fhnd := FCreate( cFileName, nFlags )

   TEST_LINE( FError()                                                 , 0 )
   TEST_LINE( TESTFIER( FWrite( fhnd, ">1234567890<" ) )               , "E: 0      R: 12"     )
   TEST_LINE( TESTFIER( FWrite( fhnd, "(123" + Chr(0) + "4567890)" ) ) , "E: 0      R: 13"     )
   TEST_LINE( TESTFIER( FSeek( fhnd ) )                                , "E: 0      R: 0"      )
   TEST_LINE( TESTFIER( FSeek( fhnd, 5 ) )                             , "E: 0      R: 5"      )
   TEST_LINE( TESTFIER( FSeek( fhnd, -1, FS_SET ) )                    , "E: 25     R: 5"      )
   TEST_LINE( TESTFIER( FSeek( fhnd, -10, FS_SET ) )                   , "E: 25     R: 5"      )
   TEST_LINE( TESTFIER( FSeek( fhnd, -100, FS_SET ) )                  , "E: 25     R: 5"      )
   TEST_LINE( TESTFIER( FWrite( fhnd, "!" ) )                          , "E: 0      R: 1"      )
   TEST_LINE( TESTFIER( FSeek( fhnd, 1 ) )                             , "E: 0      R: 1"      )
   TEST_LINE( TESTFIER( FWrite( fhnd, "A" ) )                          , "E: 0      R: 1"      )
   TEST_LINE( TESTFIER( FSeek( fhnd, 2, FS_SET ) )                     , "E: 0      R: 2"      )
   TEST_LINE( TESTFIER( FWrite( fhnd, "B" ) )                          , "E: 0      R: 1"      )
   TEST_LINE( TESTFIER( FSeek( fhnd, 3, FS_RELATIVE ) )                , "E: 0      R: 6"      )
   TEST_LINE( TESTFIER( FWrite( fhnd, "C" ) )                          , "E: 0      R: 1"      )
   TEST_LINE( TESTFIER( FSeek( fhnd, -1, FS_RELATIVE ) )               , "E: 0      R: 6"      )
   TEST_LINE( TESTFIER( FWrite( fhnd, "D" ) )                          , "E: 0      R: 1"      )
   TEST_LINE( TESTFIER( FSeek( fhnd, 3, FS_END ) )                     , "E: 0      R: 28"     )
   TEST_LINE( TESTFIER( FWrite( fhnd, "E" ) )                          , "E: 0      R: 1"      )
   TEST_LINE( TESTFIER( FSeek( fhnd, -1, FS_END ) )                    , "E: 0      R: 28"     )
   TEST_LINE( TESTFIER( FWrite( fhnd, "F" ) )                          , "E: 0      R: 1"      )
   TEST_LINE( TESTFIER( FSeek( fhnd, 0 ) )                             , "E: 0      R: 0"      )
   TEST_LINE( TESTFIER( FRead( fhnd, mnLongP ) )                       , "E: 0      R: 0"      )
   TEST_LINE( TESTFIER( FRead( fhnd, @mnLongP, 2 ) )                   , "E: 0      R: 0"      )
   TEST_LINE( TESTFIER( FRead( fhnd, cBuff4 ) )                        , "E: 0      R: 0"      )
   TEST_LINE( TESTFIER( FRead( fhnd, cBuff4, 2 ) )                     , "E: 0      R: 0"      )
#ifdef __CLIPPER__
// TEST_LINE( TESTFIER( FRead( fhnd, @cBuff4, Len( cBuff4 ) + 1 ) )    , "E: 0      R: 0"      )
#endif
   TEST_LINE( TESTFIER( FRead( fhnd, @cBuff4, 1000 ) )                 , 'E: 0      R: 0'                    )
   TEST_LINE( TESTFIER( FRead( fhnd, @cBuff4, 3 ) )                    , 'E: 0      R: 3'                    )
   TEST_LINE( TESTFIER( FRead( fhnd, @cBuff100, 100 ) )                , 'E: 0      R: 26'                   )
   TEST_LINE( TESTFIER( FSeek( fhnd, 0 ) )                             , 'E: 0      R: 0'                    )
   TEST_LINE( TESTFIER( FReadStr( fhnd, 4 ) )                          , 'E: 0      R: ">AB3"'               )
   TEST_LINE( TESTFIER( FSeek( fhnd, 0 ) )                             , 'E: 0      R: 0'                    )
   TEST_LINE( TESTFIER( FReadStr( fhnd, 100 ) )                        , 'E: 0      R: ">AB34!D7890<(123"'   )
   TEST_LINE( TESTFIER( FSeek( fhnd, 1, FS_RELATIVE ) )                , 'E: 0      R: 30'                   )
   TEST_LINE( TESTFIER( FReadStr( fhnd, 2 ) )                          , 'E: 0      R: ""'                   )
   TEST_LINE( TESTFIER( FSeek( fhnd, -4, FS_END ) )                    , 'E: 0      R: 25'                   )
   TEST_LINE( TESTFIER( FReadStr( fhnd, 1 ) )                          , 'E: 0      R: ""'                   )
   TEST_LINE( TESTFIER( FReadStr( fhnd, 20 ) )                         , 'E: 0      R: ""'                   )
   TEST_LINE( TESTFIER( FSeek( fhnd, 0, FS_END ) )                     , 'E: 0      R: 29'                   )
   TEST_LINE( TESTFIER( FWrite( fhnd, "_-_-_-_-_-_-_" ) )              , 'E: 0      R: 13'                   )
   TEST_LINE( TESTFIER( FSeek( fhnd, -4, FS_END ) )                    , 'E: 0      R: 38'                   )
   TEST_LINE( TESTFIER( FReadStr( fhnd, 1 ) )                          , 'E: 0      R: "-"'                  )
   TEST_LINE( TESTFIER( FReadStr( fhnd, 20 ) )                         , 'E: 0      R: "_-_"'                )
   TEST_LINE( TESTFIER( FSeek( fhnd, 3, FS_END ) )                     , 'E: 0      R: 45'                   )
   TEST_LINE( TESTFIER( FWrite( fhnd, "V" ) )                          , 'E: 0      R: 1'                    )
   TEST_LINE( TESTFIER( FSeek( fhnd, -3, FS_END ) )                    , 'E: 0      R: 43'                   )
   TEST_LINE( TESTFIER( FWrite( fhnd, "W" ) )                          , 'E: 0      R: 1'                    )
   TEST_LINE( TESTFIER( FClose() )                                     , 'E: 0      R: .F.'                  )
   TEST_LINE( TESTFIER( FClose( fhnd ) )                               , 'E: 0      R: .T.'                  )
   TEST_LINE( TESTFIER( FClose( fhnd ) )                               , 'E: 6      R: .F.'                  )
   TEST_LINE( TESTFIER( FErase( "NOT_HERE.$$$" ) )                     , 'E: 2      R: -1'                   )
   TEST_LINE( TESTFIER( FErase( 1 ) )                                  , 'E: 3      R: -1'                   )
   TEST_LINE( TESTFIER( FErase( "NOT_HERE.$$$" ) )                     , 'E: 2      R: -1'                   )
   TEST_LINE( TESTFIER( FRename( "NOT_HERE.$$$", 'A' ) )               , 'E: 2      R: -1'                   )

   nFlags := FO_READWRITE
   fhnd := FOpen( cFileName, nFlags )

   TEST_LINE( FError()                                                 , 0 )
   TEST_LINE( TESTFIER( FWrite( fhnd, ">1234567890<" ) )               , "E: 0      R: 12"     )
   TEST_LINE( TESTFIER( FWrite( fhnd, "(123" + Chr(0) + "4567890)" ) ) , "E: 0      R: 13"     )
   TEST_LINE( TESTFIER( FSeek( fhnd ) )                                , "E: 0      R: 0"      )
   TEST_LINE( TESTFIER( FSeek( fhnd, 5 ) )                             , "E: 0      R: 5"      )
   TEST_LINE( TESTFIER( FSeek( fhnd, -1, FS_SET ) )                    , "E: 25     R: 5"      )
   TEST_LINE( TESTFIER( FSeek( fhnd, -10, FS_SET ) )                   , "E: 25     R: 5"      )
   TEST_LINE( TESTFIER( FSeek( fhnd, -100, FS_SET ) )                  , "E: 25     R: 5"      )
   TEST_LINE( TESTFIER( FWrite( fhnd, "!" ) )                          , "E: 0      R: 1"      )
   TEST_LINE( TESTFIER( FSeek( fhnd, 1 ) )                             , "E: 0      R: 1"      )
   TEST_LINE( TESTFIER( FWrite( fhnd, "A" ) )                          , "E: 0      R: 1"      )
   TEST_LINE( TESTFIER( FSeek( fhnd, 2, FS_SET ) )                     , "E: 0      R: 2"      )
   TEST_LINE( TESTFIER( FWrite( fhnd, "B" ) )                          , "E: 0      R: 1"      )
   TEST_LINE( TESTFIER( FSeek( fhnd, 3, FS_RELATIVE ) )                , "E: 0      R: 6"      )
   TEST_LINE( TESTFIER( FWrite( fhnd, "C" ) )                          , "E: 0      R: 1"      )
   TEST_LINE( TESTFIER( FSeek( fhnd, -1, FS_RELATIVE ) )               , "E: 0      R: 6"      )
   TEST_LINE( TESTFIER( FWrite( fhnd, "D" ) )                          , "E: 0      R: 1"      )
   TEST_LINE( TESTFIER( FSeek( fhnd, 3, FS_END ) )                     , "E: 0      R: 49"     )
   TEST_LINE( TESTFIER( FWrite( fhnd, "E" ) )                          , "E: 0      R: 1"      )
   TEST_LINE( TESTFIER( FSeek( fhnd, -1, FS_END ) )                    , "E: 0      R: 49"     )
   TEST_LINE( TESTFIER( FWrite( fhnd, "F" ) )                          , "E: 0      R: 1"      )
   TEST_LINE( TESTFIER( FSeek( fhnd, 0 ) )                             , "E: 0      R: 0"      )
   TEST_LINE( TESTFIER( FRead( fhnd, mnLongP ) )                       , "E: 0      R: 0"      )
   TEST_LINE( TESTFIER( FRead( fhnd, @mnLongP, 2 ) )                   , "E: 0      R: 0"      )
   TEST_LINE( TESTFIER( FRead( fhnd, cBuff4 ) )                        , "E: 0      R: 0"      )
   TEST_LINE( TESTFIER( FRead( fhnd, cBuff4, 2 ) )                     , "E: 0      R: 0"      )
#ifdef __CLIPPER__
// TEST_LINE( TESTFIER( FRead( fhnd, @cBuff4, Len( cBuff4 ) + 1 ) )    , "E: 0      R: 0"      )
#endif
   TEST_LINE( TESTFIER( FRead( fhnd, @cBuff4, 1000 ) )                 , 'E: 0      R: 0'                  )
   TEST_LINE( TESTFIER( FRead( fhnd, @cBuff4, 3 ) )                    , 'E: 0      R: 3'                  )
   TEST_LINE( TESTFIER( FRead( fhnd, @cBuff100, 100 ) )                , 'E: 0      R: 47'                 )
   TEST_LINE( TESTFIER( FSeek( fhnd, 0 ) )                             , 'E: 0      R: 0'                  )
   TEST_LINE( TESTFIER( FReadStr( fhnd, 4 ) )                          , 'E: 0      R: ">AB3"'             )
   TEST_LINE( TESTFIER( FSeek( fhnd, 0 ) )                             , 'E: 0      R: 0'                  )
   TEST_LINE( TESTFIER( FReadStr( fhnd, 100 ) )                        , 'E: 0      R: ">AB34!D7890<(123"' )
   TEST_LINE( TESTFIER( FSeek( fhnd, 1, FS_RELATIVE ) )                , 'E: 0      R: 51'                 )
   TEST_LINE( TESTFIER( FReadStr( fhnd, 2 ) )                          , 'E: 0      R: ""'                 )
   TEST_LINE( TESTFIER( FSeek( fhnd, -4, FS_END ) )                    , 'E: 0      R: 46'                 )
   TEST_LINE( TESTFIER( FReadStr( fhnd, 1 ) )                          , 'E: 0      R: ""'                 )
   TEST_LINE( TESTFIER( FReadStr( fhnd, 20 ) )                         , 'E: 0      R: ""'                 )
   TEST_LINE( TESTFIER( FSeek( fhnd, 0, FS_END ) )                     , 'E: 0      R: 50'                 )
   TEST_LINE( TESTFIER( FWrite( fhnd, "_-_-_-_-_-_-_" ) )              , 'E: 0      R: 13'                 )
   TEST_LINE( TESTFIER( FSeek( fhnd, -4, FS_END ) )                    , 'E: 0      R: 59'                 )
   TEST_LINE( TESTFIER( FReadStr( fhnd, 1 ) )                          , 'E: 0      R: "-"'                )
   TEST_LINE( TESTFIER( FReadStr( fhnd, 20 ) )                         , 'E: 0      R: "_-_"'              )
   TEST_LINE( TESTFIER( FSeek( fhnd, 3, FS_END ) )                     , 'E: 0      R: 66'                 )
   TEST_LINE( TESTFIER( FWrite( fhnd, "V" ) )                          , 'E: 0      R: 1'                  )
   TEST_LINE( TESTFIER( FSeek( fhnd, -3, FS_END ) )                    , 'E: 0      R: 64'                 )
   TEST_LINE( TESTFIER( FWrite( fhnd, "W" ) )                          , 'E: 0      R: 1'                  )
   TEST_LINE( TESTFIER( FWrite( fhnd, "" ) )                           , 'E: 0      R: 0'                  )
   TEST_LINE( TESTFIER( FSeek( fhnd, 0, FS_END ) )                     , 'E: 0      R: 65'                 )
   TEST_LINE( TESTFIER( FClose() )                                     , 'E: 0      R: .F.'                )
   TEST_LINE( TESTFIER( FClose( fhnd ) )                               , 'E: 0      R: .T.'                )
   TEST_LINE( TESTFIER( FClose( fhnd ) )                               , 'E: 6      R: .F.'                )
   TEST_LINE( TESTFIER( FErase( "NOT_HERE.$$$" ) )                     , 'E: 2      R: -1'                 )
   TEST_LINE( TESTFIER( FErase( 1 ) )                                  , 'E: 3      R: -1'                 )
   TEST_LINE( TESTFIER( FErase( "NOT_HERE.$$$" ) )                     , 'E: 2      R: -1'                 )
   TEST_LINE( TESTFIER( FRename( "NOT_HERE.$$$", 'A' ) )               , 'E: 2      R: -1'                 )

   TEST_LINE( TESTFIER( File( cFileName ) )                            , "E: 2      R: .T."    )
   TEST_LINE( TESTFIER( File( "NOT_HERE.$$$" ) )                       , "E: 2      R: .F."    )

   FErase("$$FILEIO.TMP")

   RETURN NIL

STATIC FUNCTION TESTFIER( xRetVal )
   RETURN PadR( "E: " + LTrim( Str( FError() ) ), 9 ) + " R: " + XToStr( xRetVal )
