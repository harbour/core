/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * profiler
 *
 * Copyright 2001 Antonio Linares <alinares@fivetech.com>
 * www - http://www.harbour-project.org
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

/*
 * Copyright 2001 Patrick Mast <email@patrickmast.com>
 *
 * 2001-07-15 16:23 GMT+1
 *    Added: Added the <lOnlyUsed> parameter. If profiler is used like
 *           this Profiler(.t.), the profiler.txt will only be filled
 *           with used classes and/or functions.
 *
 * 2001-07-16 13:00 GMT+1
 *    - Removed <lOnlyUsed> parameter
 *    + Added <cFile> parameter.
 *    + Added <lAll> parameter
 *    + Added Cunsumed time in seconds
 *    * Replaced MemoWrit() function with more controllable f* functions
 *    + profiler() returns a array with profiler info
 *
 *      Profiler()
 *       => Writes NO info to file, returns Array of profiler info.
 *          Array only contains USED functions/classes.
 *
 *      Profiler(,.t.)
 *       => Writes NO info to file, returns Array of profiler info.
 *          Array only contains ALL functions/classes.
 *
 *      Profiler("profiler.txt")
 *       => Writes profiler info to <profiler.txt> and returns Array of
 *          profiler info. Array only contains USED functions/classes.
 *
 *      Profiler("profiler.txt", .t.)
 *       => Writes ALL profiler info to <profiler.txt> and returns Array of
 *          profiler info. Array contains ALL functions/classes.
 *
 * 2001-07-16 15:19 GMT+1
 *    * Renamed function from Profiler() to HB_Profiler()
 */

#define CRLF HB_OsNewLine()

Function HB_Profiler(cFile, lAll)
LOCAL n, m, cClass, aFunProcInfo, aInfo, aMethodInfo
LOCAL hFile, aProf:={}, cText:=""

   if Upper(ValType(lAll))#"L" // Put ALL classes/functions in profiler report?
      lAll:=.f.
   endif

   for n = __DynSCount() to 1 step - 1 // Number of dynamic symbols on the global
                                       // symbol table. Their names are ordered
                                       // in reverse order.
      if __DynSIsFun( n )              // Is this symbol a function or a procedure ?

         aFunProcInfo = __DynSGetPrf( n ) // We get its profiler info

         if lAll .or. aFunProcInfo[ 1 ]>0

            if !Empty(cFile)
               cText += "      " +;
                        PadR( __DynSGetName( n ), 20 ) + ;
                        Str( aFunProcInfo[ 1 ], 7 )    + ;
                        Str( aFunProcInfo[ 2 ], 14 )   + ;
                        Str( aFunProcInfo[ 2 ]/1000, 11,2 ) + CRLF
            endif

            Aadd(aProf, {"F"                , ;
                         __DynSGetName( n ) , ;
                         aFunProcInfo[ 1 ]  , ;
                         aFunProcInfo[ 2 ]  , ;
                         aFunProcInfo[ 2 ]/1000 } )

         endif
      endif
   next

   if !Empty( cFile )
      cText += CRLF + CRLF + ;
               "                                     --- CONSUMED TIME ---" + CRLF + ;
               "   CLASSES                   CALLS   CLOCK TICKS   SECONDS" + CRLF + ;
               "==========================================================" + CRLF
   endif

   n = 1
   while ! Empty( cClass := __ClassName( n ) )

      cText += CRLF + "   CLASS " + cClass + CRLF
      aInfo = ASort( __ClassSel( n ) ) // Retrieves all Class datas and methods names

      for m = 1 to Len( aInfo )

         if !Empty( aInfo[ m ] )  // why __ClassSel() returns empty strings ?

            aMethodInfo = __GetMsgPrf( n, aInfo[ m ] ) // We get its profiler info

            if lAll .or. aMethodInfo[ 1 ]>0

               if !Empty(cFile)
                  cText += "      " +;
                           PadR( aInfo[ m ], 20 )      + ;
                           Str( aMethodInfo[ 1 ], 7 )  + ;
                           Str( aMethodInfo[ 2 ], 14 ) + ;
                           Str( aMethodInfo[ 2 ]/1000, 11,2 ) + CRLF
               endif

               Aadd(aProf, {"C"              , ;
                            aInfo[ m ]       , ;
                            aMethodInfo[ 1 ] , ;
                            aMethodInfo[ 2 ] , ;
                            aMethodInfo[ 2 ]/1000 } )

            endif
         endif
      next
      n++
   end


   if !Empty(cFile)

      cText := "              *** Harbour profiler report ***"              + CRLF + CRLF + ;
               "                                     --- CONSUMED TIME ---" + CRLF + ;
               "   FUNCTIONS/PROCEDURES      CALLS   CLOCK TICKS   SECONDS" + CRLF + ;
               "==========================================================" + CRLF + cText

      hFile := FCreate( cFile )
      if hFile == -1
         Alert( "ERROR! creating '"+ cFile +"' , O/S Error: " + Str( FError(), 2 ) )
      endif

      if FWrite(hFile, cText) == len(cText)
         Fclose(hFile)
      else
         Alert("ERROR! writing '"+ cFile +"'")
         Fclose(hFile)
      endif

   endif


RETURN aProf