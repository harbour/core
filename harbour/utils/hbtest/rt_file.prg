/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Regression tests for the runtime library (file)
 *
 * Copyright 1999-2001 Viktor Szakats (harbour syenar.net)
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#include "rt_main.ch"

#include "fileio.ch"

/* Don't change the position of this #include. */
#include "rt_vars.ch"

/* NOTE: The order of the tests is relevant here, so don't
         rearrange them. */

PROCEDURE Main_FILE()
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
#ifndef __XPP__
   TEST_LINE( TESTFIER( FSeek( fhnd ) )                                , "E: 0      R: 0"      )
#endif
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
#ifndef __XPP__
   TEST_LINE( TESTFIER( FRead( fhnd, mnLongP ) )                       , "E: 0      R: 0"      )
#endif
   TEST_LINE( TESTFIER( FRead( fhnd, @mnLongP, 2 ) )                   , "E: 0      R: 0"      )
#ifndef __XPP__
   TEST_LINE( TESTFIER( FRead( fhnd, cBuff4 ) )                        , "E: 0      R: 0"      )
#endif
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
#ifndef __XPP__
   TEST_LINE( TESTFIER( FClose() )                                     , 'E: 0      R: .F.'                  )
#endif
   TEST_LINE( TESTFIER( FClose( fhnd ) )                               , 'E: 0      R: .T.'                  )
   TEST_LINE( TESTFIER( FClose( fhnd ) )                               , 'E: 6      R: .F.'                  )
   TEST_LINE( TESTFIER( FErase( "NOT_HERE.$$$" ) )                     , 'E: 2      R: -1'                   )
   TEST_LINE( TESTFIER( FErase( 1 ) )                                  , 'E: 3      R: -1'                   )
   TEST_LINE( TESTFIER( FErase( "NOT_HERE.$$$" ) )                     , 'E: 2      R: -1'                   )
   TEST_LINE( TESTFIER( FRename( "NOT_HERE.$$$", 'A' ) )               , 'E: 2      R: -1'                   )
   TEST_LINE( TESTFIER( FOpen( "NOT_HERE.$$$" ) )                      , 'E: 2      R: -1'                   )

   nFlags := FO_READWRITE
   fhnd := FOpen( cFileName, nFlags )

   TEST_LINE( FError()                                                 , 0 )
   TEST_LINE( TESTFIER( FWrite( fhnd, ">1234567890<" ) )               , "E: 0      R: 12"     )
   TEST_LINE( TESTFIER( FWrite( fhnd, "(123" + Chr(0) + "4567890)" ) ) , "E: 0      R: 13"     )
#ifndef __XPP__
   TEST_LINE( TESTFIER( FSeek( fhnd ) )                                , "E: 0      R: 0"      )
#endif
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
#ifndef __XPP__
   TEST_LINE( TESTFIER( FRead( fhnd, mnLongP ) )                       , "E: 0      R: 0"      )
#endif
   TEST_LINE( TESTFIER( FRead( fhnd, @mnLongP, 2 ) )                   , "E: 0      R: 0"      )
#ifndef __XPP__
   TEST_LINE( TESTFIER( FRead( fhnd, cBuff4 ) )                        , "E: 0      R: 0"      )
#endif
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
#ifndef __XPP__
   TEST_LINE( TESTFIER( FClose() )                                     , 'E: 0      R: .F.'                )
#endif
   TEST_LINE( TESTFIER( FClose( fhnd ) )                               , 'E: 0      R: .T.'                )
   TEST_LINE( TESTFIER( FClose( fhnd ) )                               , 'E: 6      R: .F.'                )
   TEST_LINE( TESTFIER( FErase( "NOT_HERE.$$$" ) )                     , 'E: 2      R: -1'                 )
   TEST_LINE( TESTFIER( FErase( 1 ) )                                  , 'E: 3      R: -1'                 )
   TEST_LINE( TESTFIER( FErase( "NOT_HERE.$$$" ) )                     , 'E: 2      R: -1'                 )
   TEST_LINE( TESTFIER( FRename( "NOT_HERE.$$$", 'A' ) )               , 'E: 2      R: -1'                 )

   TEST_LINE( TESTFIER( File( cFileName ) )                            , "E: 2      R: .T."    )
   TEST_LINE( TESTFIER( File( "NOT_HERE.$$$" ) )                       , "E: 2      R: .F."    )

   FErase("$$FILEIO.TMP")

   RETURN

STATIC FUNCTION TESTFIER( xRetVal )
   RETURN PadR( "E: " + LTrim( Str( FError() ) ), 9 ) + " R: " + XToStr( xRetVal )

/* Don't change the position of this #include. */
#include "rt_init.ch"
