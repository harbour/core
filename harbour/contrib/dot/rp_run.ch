/*
 * xBaseScript Project source code:
 * Pre-Processor / Dot prompt environment / Script Interpreter
 *
 * Copyright 2000-2001 Ron Pinkas <ronpinkas@profit-master.com>
 * www - http://www.xBaseScript.com
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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
 */

#ifdef __HARBOUR__
   #ifdef WIN
      #COMMAND Alert( <x> ) => MessageBox( 0, xToStr( <x> ), "xBaseScript for Windows", 0 )
   #endif
#else
   //#define __CLIPPER__
#endif

#TRANSLATE AS <type: ANYTYPE, ARRAY, CHARACTER, CODEBLOCK, DATE, LOGICAL, NUMERIC, OBJECT, STRING, USUAL> =>
#TRANSLATE AS ARRAY OF <x> =>
#TRANSLATE AS CLASS <!x!> =>
#TRANSLATE AS CLASS <!x!> := => :=
#COMMAND _HB_CLASS <*x*> =>
#COMMAND _HB_MEMBER <*x*> =>

#XTRANSLATE QSelf() => PP_Qself()
#XTRANSLATE AddMethod( <MethodName>, @<!FunName!>(), <n>, <l> ) => AddInLine( <MethodName>, {|Self,p1,p2,p3,p4,p5,p6,p7,p8,p9| PP_QSelf(Self), PP_ExecMethod( <"FunName">, p1,p2,p3,p4,p5,p6,p7,p8,p9 ) }, <n>, <l> )
#TRANSLATE :: => Self:

#COMMAND MEMVAR <*x*> =>

//#COMMAND BROWSE => Browse( 1, 0, MaxRow() - 1, MaxCol() )

#TRANSLATE _GET_( <var>, <varname>, [<pic>], [<valid>], [<when>] ) => __GET( MEMVARBLOCK(<varname>), <varname>, <pic>, <valid>, <when> )
#TRANSLATE __GET( <parlist,...>):Display() => __GET(<parlist>)

//#COMMAND EXTERNAL <!file1!> [, <!fileN!> ] => PP_ProcessFile( <file1> ) [; PP_ProcessFile( <fileN> ) ]
#COMMAND EXTERNAL <!file1!> [, <fileN> ] =>

#COMMAND DECLARE <!class!> <declaraion> <*x*> =>

// Must precede rule for DO CASE.
#COMMAND DO <!proc!> => <proc>()
#COMMAND DO <!proc!> WITH <arg1> [, <argN>] => <proc>( <arg1>[, <argN>] )

#COMMAND IF <ifExp>         => PP__IF <ifExp>
#COMMAND ELSEIF <elseifExp> => PP__ELSEIF <elseifExp>
#COMMAND ELSE               => PP__ELSE
#COMMAND ENDIF [<*x*>]      => PP__ENDIF
#COMMAND END [<*x*>]        => PP__END

#COMMAND DO CASE            => PP__DOCASE
#COMMAND CASE <caseExp>     => PP__CASE <caseExp>
#COMMAND OTHERWISE          => PP__OTHERWISE
#COMMAND ENDCASE [<*x*>]    => PP__ENDCASE

#COMMAND FOR <counter> := <start> TO <end> [STEP <step>] => PP__FOR <counter>:=<start>~TO~<end>~STEP~<step>
#COMMAND FOR <counter> =  <start> TO <end> [STEP <step>] => PP__FOR <counter>:=<start>~TO~<end>~STEP~<step>
#COMMAND LOOP [<*x*>]                                    => PP__LOOP
#COMMAND EXIT [<*x*>]                                    => PP__EXIT
#COMMAND NEXT [<*x*>]                                    => PP__NEXT

#COMMAND DO WHILE <cond>                                 => PP__WHILE <cond>
#COMMAND WHILE <cond>                                    => PP__WHILE <cond>
#COMMAND ENDDO [<*x*>]                                   => PP__ENDDO

#COMMAND DO <(file)>.prg => PP_Run( #<file> + ".prg" )

#COMMAND INIT PROCEDURE <!name!>[()]            => PP_PROC_INIT <name>
#COMMAND EXIT PROCEDURE <!name!>[()]            => PP_PROC_EXIT <name>

#COMMAND STATIC PROCEDURE <!name!>( <par,...> ) => PP_PROC_PRG <name> ; PP_LocalParams( { <"par"> } )
#COMMAND STATIC PROCEDURE <!name!>[()]          => PP_PROC_PRG <name>
#COMMAND STATIC FUNCTION <!name!>( <par,...> )  => PP_PROC_PRG <name> ; PP_LocalParams( { <"par"> } )
#COMMAND STATIC FUNCTION <!name!>[()]           => PP_PROC_PRG <name>

#COMMAND PROCEDURE <!name!>( <par,...> ) => PP_PROC <name> ; PP_LocalParams( { <"par"> } )
#COMMAND PROCEDURE <!name!>[()]          => PP_PROC <name>
#COMMAND FUNCTION <!name!>( <par,...> )  => PP_PROC <name> ; PP_LocalParams( { <"par"> } )
#COMMAND FUNCTION <!name!>[()]           => PP_PROC <name>

#COMMAND RETURN [<retExp>]             => PP_SetReturn( <retExp> )

#COMMAND PARAMETERS <par,...> => PP_Params( { <"par"> } )
#COMMAND PRIVATE <var,...>    => PP_Privates( { <"var"> } )
#COMMAND DECLARE <var,...>    => PP_Privates( { <"var"> } )
#COMMAND PUBLIC <var,...>     => PP_Publics( { <"var"> } )
#COMMAND LOCAL <var,...>      => PP_Locals( { <"var"> } )
#COMMAND STATIC <var,...>     => PP_Statics( { <"var"> } )

#TRANSLATE ProcName( [<n>] ) => PP_ProcName( <n> )
#TRANSLATE ProcLine( [<n>] ) => PP_ProcLine( <n> )
