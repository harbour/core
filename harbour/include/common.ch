/*
 * $Id$
 */

#ifndef _COMMON_CH
#define _COMMON_CH

/* Friendly logical aliases */
#define TRUE                    .T.
#define FALSE                   .F.
#define YES                     .T.
#define NO                      .F.

/* Type checking macros */
#translate ISNIL( <xValue> )    => ( <xValue> == NIL )
#translate ISARRAY( <xValue> )  => ( ValType( <xValue> ) == "A" )
#translate ISBLOCK( <xValue> )  => ( ValType( <xValue> ) == "B" )
#translate ISCHAR( <xValue> )   => ( ValType( <xValue> ) == "C" )
#translate ISDATE( <xValue> )   => ( ValType( <xValue> ) == "D" )
#translate ISLOG( <xValue> )    => ( ValType( <xValue> ) == "L" )
#translate ISMEMO( <xValue> )   => ( ValType( <xValue> ) == "M" )
#translate ISNUM( <xValue> )    => ( ValType( <xValue> ) == "N" )
#translate ISOBJECT( <xValue> ) => ( ValType( <xValue> ) == "O" )

/* DEFAULT and UPDATE commands */
#xcommand DEFAULT <v1> TO <x1> [, <vn> TO <xn> ] => ;
                                IF <v1> == NIL ; <v1> := <x1> ; END ;
                                [; IF <vn> == NIL ; <vn> := <xn> ; END ]

#command UPDATE <v1> IF <exp> TO <v2> => ;
                                IF <exp> ; <v1> := <v2> ; END

#endif /* _COMMON_CH */
