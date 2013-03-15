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
 * along with this software; see the file COPYING.txt.  If not, write to
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

   HBTEST FError()                                                   IS 0
   HBTEST TESTFIER( FWrite( fhnd, ">1234567890<" ) )                 IS "E: 0      R: 12"
   HBTEST TESTFIER( FWrite( fhnd, "(123" + Chr( 0 ) + "4567890)" ) ) IS "E: 0      R: 13"
#ifndef __XPP__
   HBTEST TESTFIER( FSeek( fhnd ) )                                  IS "E: 0      R: 0"
#endif
   HBTEST TESTFIER( FSeek( fhnd, 5 ) )                               IS "E: 0      R: 5"
   HBTEST TESTFIER( FSeek( fhnd, -1, FS_SET ) )                      IS "E: 25     R: 5"
   HBTEST TESTFIER( FSeek( fhnd, -10, FS_SET ) )                     IS "E: 25     R: 5"
   HBTEST TESTFIER( FSeek( fhnd, -100, FS_SET ) )                    IS "E: 25     R: 5"
   HBTEST TESTFIER( FWrite( fhnd, "!" ) )                            IS "E: 0      R: 1"
   HBTEST TESTFIER( FSeek( fhnd, 1 ) )                               IS "E: 0      R: 1"
   HBTEST TESTFIER( FWrite( fhnd, "A" ) )                            IS "E: 0      R: 1"
   HBTEST TESTFIER( FSeek( fhnd, 2, FS_SET ) )                       IS "E: 0      R: 2"
   HBTEST TESTFIER( FWrite( fhnd, "B" ) )                            IS "E: 0      R: 1"
   HBTEST TESTFIER( FSeek( fhnd, 3, FS_RELATIVE ) )                  IS "E: 0      R: 6"
   HBTEST TESTFIER( FWrite( fhnd, "C" ) )                            IS "E: 0      R: 1"
   HBTEST TESTFIER( FSeek( fhnd, -1, FS_RELATIVE ) )                 IS "E: 0      R: 6"
   HBTEST TESTFIER( FWrite( fhnd, "D" ) )                            IS "E: 0      R: 1"
   HBTEST TESTFIER( FSeek( fhnd, 3, FS_END ) )                       IS "E: 0      R: 28"
   HBTEST TESTFIER( FWrite( fhnd, "E" ) )                            IS "E: 0      R: 1"
   HBTEST TESTFIER( FSeek( fhnd, -1, FS_END ) )                      IS "E: 0      R: 28"
   HBTEST TESTFIER( FWrite( fhnd, "F" ) )                            IS "E: 0      R: 1"
   HBTEST TESTFIER( FSeek( fhnd, 0 ) )                               IS "E: 0      R: 0"
#ifndef __XPP__
   HBTEST TESTFIER( FRead( fhnd, mnLongP ) )                         IS "E: 0      R: 0"
#endif
   HBTEST TESTFIER( FRead( fhnd, @mnLongP, 2 ) )                     IS "E: 0      R: 0"
#ifndef __XPP__
   HBTEST TESTFIER( FRead( fhnd, cBuff4 ) )                          IS "E: 0      R: 0"
#endif
   HBTEST TESTFIER( FRead( fhnd, cBuff4, 2 ) )                       IS "E: 0      R: 0"
#ifdef __CLIPPER__
// HBTEST TESTFIER( FRead( fhnd, @cBuff4, Len( cBuff4 ) + 1 ) )      IS "E: 0      R: 0"
#endif
   HBTEST TESTFIER( FRead( fhnd, @cBuff4, 1000 ) )                   IS 'E: 0      R: 0'
   HBTEST TESTFIER( FRead( fhnd, @cBuff4, 3 ) )                      IS 'E: 0      R: 3'
   HBTEST TESTFIER( FRead( fhnd, @cBuff100, 100 ) )                  IS 'E: 0      R: 26'
   HBTEST TESTFIER( FSeek( fhnd, 0 ) )                               IS 'E: 0      R: 0'
   HBTEST TESTFIER( FReadStr( fhnd, 4 ) )                            IS 'E: 0      R: ">AB3"'
   HBTEST TESTFIER( FSeek( fhnd, 0 ) )                               IS 'E: 0      R: 0'
   HBTEST TESTFIER( FReadStr( fhnd, 100 ) )                          IS 'E: 0      R: ">AB34!D7890<(123"'
   HBTEST TESTFIER( FSeek( fhnd, 1, FS_RELATIVE ) )                  IS 'E: 0      R: 30'
   HBTEST TESTFIER( FReadStr( fhnd, 2 ) )                            IS 'E: 0      R: ""'
   HBTEST TESTFIER( FSeek( fhnd, -4, FS_END ) )                      IS 'E: 0      R: 25'
   HBTEST TESTFIER( FReadStr( fhnd, 1 ) )                            IS 'E: 0      R: ""'
   HBTEST TESTFIER( FReadStr( fhnd, 20 ) )                           IS 'E: 0      R: ""'
   HBTEST TESTFIER( FSeek( fhnd, 0, FS_END ) )                       IS 'E: 0      R: 29'
   HBTEST TESTFIER( FWrite( fhnd, "_-_-_-_-_-_-_" ) )                IS 'E: 0      R: 13'
   HBTEST TESTFIER( FSeek( fhnd, -4, FS_END ) )                      IS 'E: 0      R: 38'
   HBTEST TESTFIER( FReadStr( fhnd, 1 ) )                            IS 'E: 0      R: "-"'
   HBTEST TESTFIER( FReadStr( fhnd, 20 ) )                           IS 'E: 0      R: "_-_"'
   HBTEST TESTFIER( FSeek( fhnd, 3, FS_END ) )                       IS 'E: 0      R: 45'
   HBTEST TESTFIER( FWrite( fhnd, "V" ) )                            IS 'E: 0      R: 1'
   HBTEST TESTFIER( FSeek( fhnd, -3, FS_END ) )                      IS 'E: 0      R: 43'
   HBTEST TESTFIER( FWrite( fhnd, "W" ) )                            IS 'E: 0      R: 1'
#ifndef __XPP__
   HBTEST TESTFIER( FClose() )                                       IS 'E: 0      R: .F.'
#endif
   HBTEST TESTFIER( FClose( fhnd ) )                                 IS 'E: 0      R: .T.'
   HBTEST TESTFIER( FClose( fhnd ) )                                 IS 'E: 6      R: .F.'
   HBTEST TESTFIER( FErase( "NOT_HERE.$$$" ) )                       IS 'E: 2      R: -1'
   HBTEST TESTFIER( FErase( 1 ) )                                    IS 'E: 3      R: -1'
   HBTEST TESTFIER( FErase( "NOT_HERE.$$$" ) )                       IS 'E: 2      R: -1'
   HBTEST TESTFIER( FRename( "NOT_HERE.$$$", 'A' ) )                 IS 'E: 2      R: -1'
   HBTEST TESTFIER( FOpen( "NOT_HERE.$$$" ) )                        IS 'E: 2      R: -1'

   nFlags := FO_READWRITE
   fhnd := FOpen( cFileName, nFlags )

   HBTEST FError()                                                   IS 0
   HBTEST TESTFIER( FWrite( fhnd, ">1234567890<" ) )                 IS "E: 0      R: 12"
   HBTEST TESTFIER( FWrite( fhnd, "(123" + Chr( 0 ) + "4567890)" ) ) IS "E: 0      R: 13"
#ifndef __XPP__
   HBTEST TESTFIER( FSeek( fhnd ) )                                  IS "E: 0      R: 0"
#endif
   HBTEST TESTFIER( FSeek( fhnd, 5 ) )                               IS "E: 0      R: 5"
   HBTEST TESTFIER( FSeek( fhnd, -1, FS_SET ) )                      IS "E: 25     R: 5"
   HBTEST TESTFIER( FSeek( fhnd, -10, FS_SET ) )                     IS "E: 25     R: 5"
   HBTEST TESTFIER( FSeek( fhnd, -100, FS_SET ) )                    IS "E: 25     R: 5"
   HBTEST TESTFIER( FWrite( fhnd, "!" ) )                            IS "E: 0      R: 1"
   HBTEST TESTFIER( FSeek( fhnd, 1 ) )                               IS "E: 0      R: 1"
   HBTEST TESTFIER( FWrite( fhnd, "A" ) )                            IS "E: 0      R: 1"
   HBTEST TESTFIER( FSeek( fhnd, 2, FS_SET ) )                       IS "E: 0      R: 2"
   HBTEST TESTFIER( FWrite( fhnd, "B" ) )                            IS "E: 0      R: 1"
   HBTEST TESTFIER( FSeek( fhnd, 3, FS_RELATIVE ) )                  IS "E: 0      R: 6"
   HBTEST TESTFIER( FWrite( fhnd, "C" ) )                            IS "E: 0      R: 1"
   HBTEST TESTFIER( FSeek( fhnd, -1, FS_RELATIVE ) )                 IS "E: 0      R: 6"
   HBTEST TESTFIER( FWrite( fhnd, "D" ) )                            IS "E: 0      R: 1"
   HBTEST TESTFIER( FSeek( fhnd, 3, FS_END ) )                       IS "E: 0      R: 49"
   HBTEST TESTFIER( FWrite( fhnd, "E" ) )                            IS "E: 0      R: 1"
   HBTEST TESTFIER( FSeek( fhnd, -1, FS_END ) )                      IS "E: 0      R: 49"
   HBTEST TESTFIER( FWrite( fhnd, "F" ) )                            IS "E: 0      R: 1"
   HBTEST TESTFIER( FSeek( fhnd, 0 ) )                               IS "E: 0      R: 0"
#ifndef __XPP__
   HBTEST TESTFIER( FRead( fhnd, mnLongP ) )                         IS "E: 0      R: 0"
#endif
   HBTEST TESTFIER( FRead( fhnd, @mnLongP, 2 ) )                     IS "E: 0      R: 0"
#ifndef __XPP__
   HBTEST TESTFIER( FRead( fhnd, cBuff4 ) )                          IS "E: 0      R: 0"
#endif
   HBTEST TESTFIER( FRead( fhnd, cBuff4, 2 ) )                       IS "E: 0      R: 0"
#ifdef __CLIPPER__
// HBTEST TESTFIER( FRead( fhnd, @cBuff4, Len( cBuff4 ) + 1 ) )      IS "E: 0      R: 0"
#endif
   HBTEST TESTFIER( FRead( fhnd, @cBuff4, 1000 ) )                   IS 'E: 0      R: 0'
   HBTEST TESTFIER( FRead( fhnd, @cBuff4, 3 ) )                      IS 'E: 0      R: 3'
   HBTEST TESTFIER( FRead( fhnd, @cBuff100, 100 ) )                  IS 'E: 0      R: 47'
   HBTEST TESTFIER( FSeek( fhnd, 0 ) )                               IS 'E: 0      R: 0'
   HBTEST TESTFIER( FReadStr( fhnd, 4 ) )                            IS 'E: 0      R: ">AB3"'
   HBTEST TESTFIER( FSeek( fhnd, 0 ) )                               IS 'E: 0      R: 0'
   HBTEST TESTFIER( FReadStr( fhnd, 100 ) )                          IS 'E: 0      R: ">AB34!D7890<(123"'
   HBTEST TESTFIER( FSeek( fhnd, 1, FS_RELATIVE ) )                  IS 'E: 0      R: 51'
   HBTEST TESTFIER( FReadStr( fhnd, 2 ) )                            IS 'E: 0      R: ""'
   HBTEST TESTFIER( FSeek( fhnd, -4, FS_END ) )                      IS 'E: 0      R: 46'
   HBTEST TESTFIER( FReadStr( fhnd, 1 ) )                            IS 'E: 0      R: ""'
   HBTEST TESTFIER( FReadStr( fhnd, 20 ) )                           IS 'E: 0      R: ""'
   HBTEST TESTFIER( FSeek( fhnd, 0, FS_END ) )                       IS 'E: 0      R: 50'
   HBTEST TESTFIER( FWrite( fhnd, "_-_-_-_-_-_-_" ) )                IS 'E: 0      R: 13'
   HBTEST TESTFIER( FSeek( fhnd, -4, FS_END ) )                      IS 'E: 0      R: 59'
   HBTEST TESTFIER( FReadStr( fhnd, 1 ) )                            IS 'E: 0      R: "-"'
   HBTEST TESTFIER( FReadStr( fhnd, 20 ) )                           IS 'E: 0      R: "_-_"'
   HBTEST TESTFIER( FSeek( fhnd, 3, FS_END ) )                       IS 'E: 0      R: 66'
   HBTEST TESTFIER( FWrite( fhnd, "V" ) )                            IS 'E: 0      R: 1'
   HBTEST TESTFIER( FSeek( fhnd, -3, FS_END ) )                      IS 'E: 0      R: 64'
   HBTEST TESTFIER( FWrite( fhnd, "W" ) )                            IS 'E: 0      R: 1'
   HBTEST TESTFIER( FWrite( fhnd, "" ) )                             IS 'E: 0      R: 0'
   HBTEST TESTFIER( FSeek( fhnd, 0, FS_END ) )                       IS 'E: 0      R: 65'
#ifndef __XPP__
   HBTEST TESTFIER( FClose() )                                       IS 'E: 0      R: .F.'
#endif
   HBTEST TESTFIER( FClose( fhnd ) )                                 IS 'E: 0      R: .T.'
   HBTEST TESTFIER( FClose( fhnd ) )                                 IS 'E: 6      R: .F.'
   HBTEST TESTFIER( FErase( "NOT_HERE.$$$" ) )                       IS 'E: 2      R: -1'
   HBTEST TESTFIER( FErase( 1 ) )                                    IS 'E: 3      R: -1'
   HBTEST TESTFIER( FErase( "NOT_HERE.$$$" ) )                       IS 'E: 2      R: -1'
   HBTEST TESTFIER( FRename( "NOT_HERE.$$$", 'A' ) )                 IS 'E: 2      R: -1'

   HBTEST TESTFIER( File( cFileName ) )                              IS "E: 2      R: .T."
   HBTEST TESTFIER( File( "NOT_HERE.$$$" ) )                         IS "E: 2      R: .F."

   FErase( "$$FILEIO.TMP" )

   RETURN

STATIC FUNCTION TESTFIER( xRetVal )
   RETURN PadR( "E: " + LTrim( Str( FError() ) ), 9 ) + " R: " + XToStr( xRetVal )

/* Don't change the position of this #include. */
#include "rt_init.ch"
