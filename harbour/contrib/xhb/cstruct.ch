/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 *
 * Copyright 2000 Ron Pinkas <ronpinkas@profit-master.com>
 * www - http://www.xharbour.org
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

#include "hbctypes.ch"

#ifndef __HBCSTRUCT_CH_
   #define __HBCSTRUCT_CH_

   // Exclude from C compilation
   #ifdef _SET_CH
      #xtranslate __ClsSetModule( <x> ) => Empty( <x> )
      #xcommand C STRUCTURE <!stru!> [ALIGN <align> ] => ;
               INIT PROCEDURE __INIT_<stru>; ;
                  __ActiveStructure( #<stru>, <align> ) ; ;
               #xtranslate IS <stru> \[ \<x: :=, INIT> { \<initlist,...> } ] => := hb_CStructure( #<stru> ):Init( {\<initlist>} ); ;
               #xtranslate IS <stru> FROM \<pointer> => := hb_CStructure( #<stru> ):Buffer( \<pointer> )

      // <elem> instead of <!elem!> to allow ElemName[n] syntax.
      #xcommand MEMBER <elem> IS <type> => hb_Member( #<elem>, <type> )

      /*
         Will match:
            MEMBER <elem> IS <!stru!>
         due to expansion of:
            #xtranslate IS <stru> [...] => := hb_CStructure( #<stru> ):Init( {} )
         as established by C STRUCTURE <!stru!> #xcommand for the given structure.
      */
      #xcommand MEMBER <elem> := hb_CStructure( <literalstru> ):Init( {} ) => ;
               hb_Member( #<elem>, hb_CStructureId( <literalstru>, .T. ) )

      #xcommand MEMBER <!elem!> IS <type> ( <nlen> ) => hb_Member( #<elem>, hb_CTypeArrayId( <type>, <nlen> ) )

      #xcommand MEMBER <!elem!> AS <!stru!> => ;
               hb_Member( #<elem>, hb_CStructureId( #<stru>, .F. ) )

      #xcommand END C STRUCTURE [<!stru!>] => ; ;
                  __ClsSetModule( __ActiveStructure() ); ;
               RETURN

      #xcommand IMPORT C STRUCTURE <!stru!> => ;
               #xtranslate IS <stru> \[ \<x: :=, INIT> { \<initlist,...> } ] => := hb_CStructure( #<stru> ):Init( {\<initlist>} ); ;
               #xtranslate IS <stru> FROM \<pointer> => := hb_CStructure( #<stru> ):Buffer( \<pointer> )

      //----------------------------- C Syntax support ---------------------------------//
      /* NOTES:

        1. #pragma pack(<x>) needs to be translated to pragma pack(<X>) without the <#>.

        2. First line must end with <;> so the whole definition is a single PRG line!
      */

      #define __PACK 8

      #xcommand typedef struct [<!tag!>] { [[struct] <elem>] } <!stru!> [, <*synon*>] => ;
                INIT PROCEDURE __INIT_<stru>; ;
                   hb_CStructureCSyntax( #<stru>, {[#<elem>,]}, <(tag)>, <"synon">, __PACK ); ;
                   __ClsSetModule( __ActiveStructure() ); ;
                RETURN; ;
                #xtranslate IS <stru> \[ \<x: :=, INIT> { \<initlist,...> } ] => := hb_CStructure( #<stru> ):Init( {\<initlist>} ); ;
                #xtranslate IS <stru> FROM \<pointer> => := hb_CStructure( #<stru> ):Buffer( \<pointer> )

      #xcommand pragma pack( <pack> ) => #undef __PACK; #define __PACK <pack>
      #xcommand pragma pack() => #undef __PACK; #define __PACK 8

      #xtranslate ( struct <stru> ) [<buffer>] => hb_CStructure( #<stru> ):Buffer( <buffer> )
      #xtranslate ( struct <stru>* ) <pointer> => hb_CStructure( #<stru> ):Pointer( <pointer> )
   #endif

#endif /* __HBCSTRUCT_CH_ */
