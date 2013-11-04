/*
 * Harbour Project source code:
 * Visual FoxPro compatibility header
 *
 * Copyright 2010 Viktor Szakats (vszakats.net/harbour)
 * Copyright 2010 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

#ifndef HBFOXPRO_CH_
#define HBFOXPRO_CH_


/* messages in FP */
#xtranslate .<!msg!> => :<msg>


/* array declarations */
#xtranslate __FP_DIM( <exp> ) => <exp>
#xtranslate __FP_DIM( <!name!>( <dim,...> ) ) => <name>\[ <dim> \]

#command PUBLIC <var1> [, <varN> ] => ;
         <@> PUBLIC __FP_DIM( <var1> ) [, __FP_DIM( <varN> ) ]
#command PRIVATE <var1> [, <varN> ] => ;
         <@> PRIVATE __FP_DIM( <var1> ) [, __FP_DIM( <varN> ) ]
#command DIMENSIONS <!name1!>( <dim1,...> ) [, <!nameN!>( <dimN,...> ) ] => ;
         PRIVATE <name1>\[ <dim1> \] [, <nameN>\[ <dimN> \] ]


/* workaround for problem with command using FIELDS keyword which can
   wrongly translate FIELD->fieldname.
 */
#translate FIELD-><!name!> => _FIELD-><name>


/* commands using FIELDS clause which is not accepted by Clipper */
#command DISPLAY [FIELDS <v,...>] [<off:OFF>] ;
                 [<prn:TO PRINTER>] [TO FILE <(f)>] ;
                 [FOR <for>] [WHILE <while>] [NEXT <next>] ;
                 [RECORD <rec>] [<rest:REST>] [<all:ALL>] => ;
         __dbList( <.off.>, { <{v}> }, <.all.>, ;
                   <{for}>, <{while}>, <next>, ;
                   <rec>, <.rest.>, <.prn.>, <(f)> )


/* commands and standard functions with alias */
#command SEEK <exp> [<soft: SOFTSEEK>] [<last: LAST>] ;
              [TAG <tag>] [IN <wa>] => ;
         __fox_Seek( <exp>, iif( <.soft.>, .T., NIL ), ;
                            iif( <.last.>, .T., NIL ), ;
                     <(wa)>, <(tag)> )
#command SET FILTER TO <exp> IN <wa> [NOOPTIMIZE] => ;
                                 <wa>->( DbSetFilter( <{exp}>, <"exp"> ) )
#command SKIP [<n>] IN <wa>   => <wa>->( DbSkip( <n> ) )
#command UNLOCK IN <wa>       => <wa>->( DbUnlock() )
#command GO TOP IN <wa>       => <wa>->( DbGoTop() )
#command GO BOTTOM IN <wa>    => <wa>->( DbGoBottom() )
#command GOTO <nRec> IN <wa>  => <wa>->( DbGoTo( <nRec> ) )

#xtranslate SEEK( <x>, <wa> ) => (<wa>)->( DbSeek( <x> ) )
#xtranslate RECCOUNT( <wa> )  => (<wa>)->( RecCount() )
#xtranslate RECSIZE( <wa> )   => (<wa>)->( RecSize() )
#xtranslate FCOUNT( <wa> )    => (<wa>)->( FCount() )
#xtranslate RECNO( <wa> )     => (<wa>)->( RecNo() )
#xtranslate RLOCK( <wa> )     => (<wa>)->( Rlock() )

#xtranslate USED( <wa> )    => __fox_Used( <wa> )


/* other commands */
#command SCAN [FOR <for>] [WHILE <while>] [NEXT <next>] ;
              [RECORD <rec>] [<rest:REST>] [ALL] [NOOPTIMIZE] => ;
         __dbLocate( <{for}>, <{while}>, <next>, <rec>, <.rest.> ) ;;
         WHILE Found()
#command ENDSCAN => __dbContinue(); ENDDO

#command EJECT PAGE => __Eject()
#command FLUSH      => DbCommitAll()
#command REGIONAL [<defs,...>] => LOCAL <defs>


#endif /* HBFOXPRO_CH_ */
