/*
 * $Id$
 */

/*
 * xHarbour Project source code:
 *
 * ClassEx.ch Class Extension
 *
 * Copyright 2002 Francesco Saverio Giudice [info@fsgiudice.com]
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

/* Class Extension

   Property is a better way to write DATA ACCESS in classes
   The command is:

   PROPERTY varname [AS type] [INIT value] [INDEX pos]
            [READ mtread] [WRITE mtwrite]

   where
      - varname = the visible DATA name. *** INTERNAL DATA is stored with F prefix ***
                  so when you have to access this data you have to refer with F prefix
                  example:
                  ....
                  PROPERTY Title READ GetTitle
                  ....
                  METHOD GetTitle INLINE ::FTitle
                  ....
                  internal data is stored with HIDDEN scope so you cannot access it
                  outside of declaration file

      - type    = the type of data - see DATA help for explanation

      - value   = initial value of var

      - pos     = index is a powerfull access method. You can use same method to get/set
                  data, but trough index you can access to correct data.
                  example:
                  ....
                  PROPERTY Top     AS NUMERIC INIT 10 INDEX 1 READ GetCoord WRITE SetCoord
                  PROPERTY Left    AS NUMERIC INIT  0 INDEX 2 READ GetCoord WRITE SetCoord
                  ....
                  METHOD SetCoord( i, x )    // Look at index parameter
                    DO CASE
                       CASE i == 1 // Top
                         ::FTop  := x
                       CASE i == 2 // Left
                         ::FLeft := x
                       ...
                    ENDCASE
                    ::Changed := .T.
                  RETURN Self

                  METHOD GetCoord( i )
                    LOCAL nRet
                    DO CASE
                       CASE i == 1 // Top
                         nRet := ::FTop
                       CASE i == 2 // Left
                         nRet := ::FLeft
                       ...
                    ENDCASE
                  RETURN nRet

      - mtread  = the method to read DATA. It can be a method name or directly the
                  internal data name.
                  example:
                  ....
                  PROPERTY Title READ FTitle WRITE SetTitle
                  ....
                  METHOD SetTitle(x) INLINE ::FTitle := x, ::Changed := .T.
                  ....

      - mtwrite = the method to write DATA. It can be a method name or directly the
                  internal data name.
                  example above.

 */

#xcommand PROPERTY <x>  [TYPE <type>] [AS <astype>] [INDEX <i>] READ <rf> WRITE <wf> [DEFAULT <d>];
                        [<scope: EXPORTED, EXPORT, VISIBLE, PUBLIC, PROTECTED, HIDDEN, PRIVATE, READONLY, RO, PUBLISHED >] ;
           =>  ;
           DATA T_<x>    AS STRING INIT <(type)> PROTECTED  ;;
           #xtranslate __FNAME(<x>) => F<x> ;;
           DATA __FNAME(<x>)     [AS <astype>] PROTECTED [INIT <d>] ;;
           ACCESS <x>    INLINE ::<rf>( [ <i> ] )           ;;
           ASSIGN <x>(v) INLINE ::<wf>( [ <i>, ] v)

#xcommand PROPERTY <x>  [AS <astype>] [INDEX <i>] READ <rf> WRITE <wf> [DEFAULT <d>];
                        [<scope: EXPORTED, EXPORT, VISIBLE, PUBLIC, PROTECTED, HIDDEN, PRIVATE, READONLY, RO, PUBLISHED >] ;
           =>  ;
           #xtranslate __FNAME(<x>) => F<x> ;;
           DATA __FNAME(<x>)     [AS <astype>] PROTECTED [INIT <d>] ;;
           ACCESS <x>    INLINE ::<rf>( [ <i> ] )          ;;
           ASSIGN <x>(v) INLINE ::<wf>( [ <i>, ] v)


#xcommand PROPERTY <x>  [TYPE <type>] [AS <astype>] [INDEX <i>] WRITE <wf> [DEFAULT <d>];
                        [<scope: EXPORTED, EXPORT, VISIBLE, PUBLIC, PROTECTED, HIDDEN, PRIVATE, READONLY, RO, PUBLISHED >] ;
           =>  ;
           DATA T_<x>    AS STRING INIT <(type)> PROTECTED   ;;
           #xtranslate __FNAME(<x>) => F<x> ;;
           DATA __FNAME(<x>)     [AS <astype>] PROTECTED [INIT <d>] ;;
           ASSIGN <x>(v) INLINE ::<wf>( [ <i>, ] v)

#xcommand PROPERTY <x>  [AS <astype>] [INDEX <i>] WRITE <wf> [DEFAULT <d>];
                        [<scope: EXPORTED, EXPORT, VISIBLE, PUBLIC, PROTECTED, HIDDEN, PRIVATE, READONLY, RO, PUBLISHED >] ;
           =>  ;
           #xtranslate __FNAME(<x>) => F<x> ;;
           DATA __FNAME(<x>)     [AS <astype>] PROTECTED [INIT <d>] ;;
           ASSIGN <x>(v) INLINE ::<wf>( [ <i>, ] v)

#xcommand PROPERTY <x>  [TYPE <type>] [AS <astype>] [INDEX <i>] READ <rf> [DEFAULT <d>];
                        [<scope: EXPORTED, EXPORT, VISIBLE, PUBLIC, PROTECTED, HIDDEN, PRIVATE, READONLY, RO, PUBLISHED >] ;
           =>  ;
           DATA T_<x>    AS STRING INIT <(type)> PROTECTED   ;;
           #xtranslate __FNAME(<x>) => F<x> ;;
           DATA __FNAME(<x>)     [AS <astype>] PROTECTED [INIT <d>] ;;
           ACCESS <x>    INLINE ::<rf>( [ <i> ] )

#xcommand PROPERTY <x>  [AS <astype>] [INDEX <i>] READ <rf> [DEFAULT <d>];
                        [<scope: EXPORTED, EXPORT, VISIBLE, PUBLIC, PROTECTED, HIDDEN, PRIVATE, READONLY, RO, PUBLISHED >] ;
           =>  ;
           #xtranslate __FNAME(<x>) => F<x> ;;
           DATA __FNAME(<x>)     [AS <astype>] PROTECTED [INIT <d>] ;;
           ACCESS <x>    INLINE ::<rf>( [ <i> ] )


#xcommand PROPERTY <x>  [TYPE <type>] [AS <astype>] [DEFAULT <d>];
                        [<scope: EXPORTED, EXPORT, VISIBLE, PUBLIC, PROTECTED, HIDDEN, PRIVATE, READONLY, RO, PUBLISHED >] ;
           =>  ;
           DATA T_<x>    AS STRING INIT <(type)> PROTECTED   ;;
           #xtranslate __FNAME(<x>) => F<x> ;;
           DATA __FNAME(<x>)     [AS <astype>] EXPORTED  [INIT <d>]

#xcommand PROPERTY <x>  [AS <astype>] [DEFAULT <d>];
                        [<scope: EXPORTED, EXPORT, VISIBLE, PUBLIC, PROTECTED, HIDDEN, PRIVATE, READONLY, RO, PUBLISHED >] ;
           =>  ;
           #xtranslate __FNAME(<x>) => F<x> ;;
           DATA __FNAME(<x>)     [AS <astype>] EXPORTED  [INIT <d>]

#xtranslate __FNAME(<x>) => F<x>
