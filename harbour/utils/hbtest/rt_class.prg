/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour class/OOP test
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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

/* Don't change the position of this #include. */
#include "rt_vars.ch"


#include "hbclass.ch"

MEMVAR objHolder, cDtorResult

PROCEDURE Main_CLASS()
   LOCAL oValue, aRef
   PRIVATE objHolder, cDtorResult

#ifdef __HARBOUR__

   /* Test destructors */

   TEST_LINE( cDtorResult := ""               , ""                                     )
   TEST_LINE( objHolder := NIL                , NIL                                    )
   oValue := DTORCLASS():NEW(0)
   TEST_LINE( oValue:type                     , 0                                      )
   TEST_LINE( oValue := NIL                   , NIL                                    )
   TEST_LINE( objHolder                       , NIL                                    )
   TEST_LINE( cDtorResult                     , "No references to self."               )

   TEST_LINE( cDtorResult := ""               , ""                                     )
   TEST_LINE( objHolder := NIL                , NIL                                    )
   oValue := DTORCLASS():NEW(1)
   TEST_LINE( oValue:type                     , 1                                      )
   TEST_LINE( oValue := NIL                   , NIL                                    )
   TEST_LINE( objHolder                       , NIL                                    )
   TEST_LINE( cDtorResult                     , "Reference to self in instance variable." )

   TEST_LINE( cDtorResult := ""               , ""                                     )
   TEST_LINE( objHolder := NIL                , NIL                                    )
   oValue := DTORCLASS():NEW(2)
   TEST_LINE( oValue:type                     , 2                                      )
   TEST_LINE( oValue := NIL                   , "E 45 BASE 1301 Object destructor failure (Reference to freed block) OS:0 #:0 " )
   TEST_LINE( objHolder                       , NIL                                    )
   TEST_LINE( cDtorResult                     , "Reference to self in class variable." )

   TEST_LINE( cDtorResult := ""               , ""                                     )
   TEST_LINE( objHolder := NIL                , NIL                                    )
   oValue := DTORCLASS():NEW(3)
   TEST_LINE( oValue:type                     , 3                                      )
   TEST_LINE( oValue := NIL                   , "E 45 BASE 1301 Object destructor failure (Reference to freed block) OS:0 #:0 " )
   TEST_LINE( valtype(objHolder)              , "A"                                    )
   TEST_LINE( len(objHolder)                  , 0                                      )
   TEST_LINE( cDtorResult                     , "Reference to self in private memvar." )


   /* Tests with cross references and releasing by Garbage Collector */

   TEST_LINE( cDtorResult := ""               , ""                                     )
   TEST_LINE( objHolder := NIL                , NIL                                    )
   oValue := DTORCLASS():NEW(0)
   TEST_LINE( oValue:type                     , 0                                      )
   /* create cross reference */
   aRef := { oValue, NIL }; aRef[2] := aRef; aRef := NIL
   TEST_LINE( oValue := NIL                   , NIL                                    )
   TEST_LINE( objHolder                       , NIL                                    )
   TEST_LINE( cDtorResult                     , ""                                     )
   TEST_LINE( hb_gcAll()                      , NIL                                    )
   TEST_LINE( objHolder                       , NIL                                    )
   TEST_LINE( cDtorResult                     , "No references to self."               )

   TEST_LINE( cDtorResult := ""               , ""                                     )
   TEST_LINE( objHolder := NIL                , NIL                                    )
   oValue := DTORCLASS():NEW(1)
   TEST_LINE( oValue:type                     , 1                                      )
   /* create cross reference */
   aRef := { oValue, NIL }; aRef[2] := aRef; aRef := NIL
   TEST_LINE( oValue := NIL                   , NIL                                    )
   TEST_LINE( objHolder                       , NIL                                    )
   TEST_LINE( cDtorResult                     , ""                                     )
   TEST_LINE( hb_gcAll()                      , NIL                                    )
   TEST_LINE( objHolder                       , NIL                                    )
   TEST_LINE( cDtorResult                     , "Reference to self in instance variable." )

   TEST_LINE( cDtorResult := ""               , ""                                     )
   TEST_LINE( objHolder := NIL                , NIL                                    )
   oValue := DTORCLASS():NEW(2)
   TEST_LINE( oValue:type                     , 2                                      )
   /* create cross reference */
   aRef := { oValue, NIL }; aRef[2] := aRef; aRef := NIL
   TEST_LINE( oValue := NIL                   , NIL                                    )
   TEST_LINE( objHolder                       , NIL                                    )
   TEST_LINE( cDtorResult                     , ""                                     )
   TEST_LINE( hb_gcAll()                      , "E 45 BASE 1301 Object destructor failure (Reference to freed block) OS:0 #:0 " )
   TEST_LINE( objHolder                       , NIL                                    )
   TEST_LINE( cDtorResult                     , "Reference to self in class variable." )

   TEST_LINE( cDtorResult := ""               , ""                                     )
   TEST_LINE( objHolder := NIL                , NIL                                    )
   oValue := DTORCLASS():NEW(3)
   TEST_LINE( oValue:type                     , 3                                      )
   /* create cross reference */
   aRef := { oValue, NIL }; aRef[2] := aRef; aRef := NIL
   TEST_LINE( oValue := NIL                   , NIL                                    )
   TEST_LINE( objHolder                       , NIL                                    )
   TEST_LINE( cDtorResult                     , ""                                     )
   TEST_LINE( hb_gcAll()                      , "E 45 BASE 1301 Object destructor failure (Reference to freed block) OS:0 #:0 " )
   TEST_LINE( valtype(objHolder)              , "A"                                    )
   TEST_LINE( len(objHolder)                  , 0                                      )
   TEST_LINE( cDtorResult                     , "Reference to self in private memvar." )



   /* Test instance area allocating and casting */

   oValue := IVARSCLASS4():new()

   TEST_LINE( oValue:x1                       , "(x1)"                                 )
   TEST_LINE( oValue:y1                       , "(y1)"                                 )
   TEST_LINE( oValue:z1                       , "(z1)"                                 )
   TEST_LINE( oValue:x2                       , "(x2)"                                 )
   TEST_LINE( oValue:y2                       , "(y2)"                                 )
   TEST_LINE( oValue:z2                       , "(z2)"                                 )
   TEST_LINE( oValue:x3                       , "(x3)"                                 )
   TEST_LINE( oValue:y3                       , "(y3)"                                 )
   TEST_LINE( oValue:z3                       , "(z3)"                                 )
   TEST_LINE( oValue:x4                       , "(x4)"                                 )
   TEST_LINE( oValue:y4                       , "(y4)"                                 )
   TEST_LINE( oValue:z4                       , "(z4)"                                 )
   TEST_LINE( INSTANCE_DATA( oValue )         , "[12]: (x1) (y1) (z1) (x2) (y2) (z2) (x3) (y3) (z3) (x4) (y4) (z4)" )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS1:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS2:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS3:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS4:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:classH )             , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS1:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS2:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS3:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS4:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:classH )             , 0                        )

   /* simple assignment... */
   TEST_LINE( oValue:x1 := " X1 "             , " X1 "                                 )
   TEST_LINE( oValue:y1 := " Y1 "             , " Y1 "                                 )
   TEST_LINE( oValue:z1 := " Z1 "             , " Z1 "                                 )
   TEST_LINE( oValue:x2 := " X2 "             , " X2 "                                 )
   TEST_LINE( oValue:y2 := " Y2 "             , " Y2 "                                 )
   TEST_LINE( oValue:z2 := " Z2 "             , " Z2 "                                 )
   TEST_LINE( oValue:x3 := " X3 "             , " X3 "                                 )
   TEST_LINE( oValue:y3 := " Y3 "             , " Y3 "                                 )
   TEST_LINE( oValue:z3 := " Z3 "             , " Z3 "                                 )
   TEST_LINE( oValue:x4 := " X4 "             , " X4 "                                 )
   TEST_LINE( oValue:y4 := " Y4 "             , " Y4 "                                 )
   TEST_LINE( oValue:z4 := " Z4 "             , " Z4 "                                 )

   TEST_LINE( oValue:x1                       , " X1 "                                 )
   TEST_LINE( oValue:y1                       , " Y1 "                                 )
   TEST_LINE( oValue:z1                       , " Z1 "                                 )
   TEST_LINE( oValue:x2                       , " X2 "                                 )
   TEST_LINE( oValue:y2                       , " Y2 "                                 )
   TEST_LINE( oValue:z2                       , " Z2 "                                 )
   TEST_LINE( oValue:x3                       , " X3 "                                 )
   TEST_LINE( oValue:y3                       , " Y3 "                                 )
   TEST_LINE( oValue:z3                       , " Z3 "                                 )
   TEST_LINE( oValue:x4                       , " X4 "                                 )
   TEST_LINE( oValue:y4                       , " Y4 "                                 )
   TEST_LINE( oValue:z4                       , " Z4 "                                 )
   TEST_LINE( INSTANCE_DATA( oValue )         , "[12]:  X1   Y1   Z1   X2   Y2   Z2   X3   Y3   Z3   X4   Y4   Z4 " )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS1:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS2:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS3:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS4:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:classH )             , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS1:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS2:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS3:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS4:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:classH )             , 0                        )

   /* Setting IVARSCLASS1 instance variables... */
   TEST_LINE( oValue:IVARSCLASS1:x1 := "[X1]"    , "[X1]"                              )
   TEST_LINE( oValue:IVARSCLASS1:y1 := "[Y1]"    , "[Y1]"                              )
   TEST_LINE( oValue:IVARSCLASS1:z1 := "[Z1]"    , "[Z1]"                              )

   TEST_LINE( oValue:x1                       , "[X1]"                                 )
   TEST_LINE( oValue:y1                       , "[Y1]"                                 )
   TEST_LINE( oValue:z1                       , "[Z1]"                                 )
   TEST_LINE( oValue:x2                       , " X2 "                                 )
   TEST_LINE( oValue:y2                       , " Y2 "                                 )
   TEST_LINE( oValue:z2                       , " Z2 "                                 )
   TEST_LINE( oValue:x3                       , " X3 "                                 )
   TEST_LINE( oValue:y3                       , " Y3 "                                 )
   TEST_LINE( oValue:z3                       , " Z3 "                                 )
   TEST_LINE( oValue:x4                       , " X4 "                                 )
   TEST_LINE( oValue:y4                       , " Y4 "                                 )
   TEST_LINE( oValue:z4                       , " Z4 "                                 )
   TEST_LINE( INSTANCE_DATA( oValue )         , "[12]: [X1] [Y1] [Z1]  X2   Y2   Z2   X3   Y3   Z3   X4   Y4   Z4 " )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS1:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS2:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS3:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS4:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:classH )             , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS1:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS2:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS3:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS4:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:classH )             , 0                        )

   /* Setting IVARSCLASS2 instance variables... */
   TEST_LINE( oValue:IVARSCLASS2:x2 := "[X2]"    , "[X2]"                              )
   TEST_LINE( oValue:IVARSCLASS2:y2 := "[Y2]"    , "[Y2]"                              )
   TEST_LINE( oValue:IVARSCLASS2:z2 := "[Z2]"    , "[Z2]"                              )

   TEST_LINE( oValue:x1                       , "[X1]"                                 )
   TEST_LINE( oValue:y1                       , "[Y1]"                                 )
   TEST_LINE( oValue:z1                       , "[Z1]"                                 )
   TEST_LINE( oValue:x2                       , "[X2]"                                 )
   TEST_LINE( oValue:y2                       , "[Y2]"                                 )
   TEST_LINE( oValue:z2                       , "[Z2]"                                 )
   TEST_LINE( oValue:x3                       , " X3 "                                 )
   TEST_LINE( oValue:y3                       , " Y3 "                                 )
   TEST_LINE( oValue:z3                       , " Z3 "                                 )
   TEST_LINE( oValue:x4                       , " X4 "                                 )
   TEST_LINE( oValue:y4                       , " Y4 "                                 )
   TEST_LINE( oValue:z4                       , " Z4 "                                 )
   TEST_LINE( INSTANCE_DATA( oValue )         , "[12]: [X1] [Y1] [Z1] [X2] [Y2] [Z2]  X3   Y3   Z3   X4   Y4   Z4 " )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS1:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS2:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS3:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS4:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:classH )             , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS1:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS2:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS3:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS4:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:classH )             , 0                        )

   /* Setting IVARSCLASS3 instance variables... */
   TEST_LINE( oValue:IVARSCLASS3:x3 := "[X3]"    , "[X3]"                              )
   TEST_LINE( oValue:IVARSCLASS3:y3 := "[Y3]"    , "[Y3]"                              )
   TEST_LINE( oValue:IVARSCLASS3:z3 := "[Z3]"    , "[Z3]"                              )

   TEST_LINE( oValue:x1                       , "[X1]"                                 )
   TEST_LINE( oValue:y1                       , "[Y1]"                                 )
   TEST_LINE( oValue:z1                       , "[Z1]"                                 )
   TEST_LINE( oValue:x2                       , "[X2]"                                 )
   TEST_LINE( oValue:y2                       , "[Y2]"                                 )
   TEST_LINE( oValue:z2                       , "[Z2]"                                 )
   TEST_LINE( oValue:x3                       , "[X3]"                                 )
   TEST_LINE( oValue:y3                       , "[Y3]"                                 )
   TEST_LINE( oValue:z3                       , "[Z3]"                                 )
   TEST_LINE( oValue:x4                       , " X4 "                                 )
   TEST_LINE( oValue:y4                       , " Y4 "                                 )
   TEST_LINE( oValue:z4                       , " Z4 "                                 )
   TEST_LINE( INSTANCE_DATA( oValue )         , "[12]: [X1] [Y1] [Z1] [X2] [Y2] [Z2] [X3] [Y3] [Z3]  X4   Y4   Z4 " )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS1:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS2:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS3:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS4:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:classH )             , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS1:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS2:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS3:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS4:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:classH )             , 0                        )

   /* Setting IVARSCLASS4 instance variables... */
   TEST_LINE( oValue:IVARSCLASS4:x4 := "[X4]"    , "[X4]"                              )
   TEST_LINE( oValue:IVARSCLASS4:y4 := "[Y4]"    , "[Y4]"                              )
   TEST_LINE( oValue:IVARSCLASS4:z4 := "[Z4]"    , "[Z4]"                              )

   TEST_LINE( oValue:x1                       , "[X1]"                                 )
   TEST_LINE( oValue:y1                       , "[Y1]"                                 )
   TEST_LINE( oValue:z1                       , "[Z1]"                                 )
   TEST_LINE( oValue:x2                       , "[X2]"                                 )
   TEST_LINE( oValue:y2                       , "[Y2]"                                 )
   TEST_LINE( oValue:z2                       , "[Z2]"                                 )
   TEST_LINE( oValue:x3                       , "[X3]"                                 )
   TEST_LINE( oValue:y3                       , "[Y3]"                                 )
   TEST_LINE( oValue:z3                       , "[Z3]"                                 )
   TEST_LINE( oValue:x4                       , "[X4]"                                 )
   TEST_LINE( oValue:y4                       , "[Y4]"                                 )
   TEST_LINE( oValue:z4                       , "[Z4]"                                 )
   TEST_LINE( INSTANCE_DATA( oValue )         , "[12]: [X1] [Y1] [Z1] [X2] [Y2] [Z2] [X3] [Y3] [Z3] [X4] [Y4] [Z4]" )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS1:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS2:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS3:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS4:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:classH )             , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS1:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS2:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS3:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS4:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:classH )             , 0                        )

   /* Setting IVARSCLASS3:IVARSCLASS1 instance variables... */
   TEST_LINE( oValue:IVARSCLASS3:IVARSCLASS1:x1 := "<X1>"    , "<X1>"                  )
   TEST_LINE( oValue:IVARSCLASS3:IVARSCLASS1:y1 := "<Y1>"    , "<Y1>"                  )
   TEST_LINE( oValue:IVARSCLASS3:IVARSCLASS1:z1 := "<Z1>"    , "<Z1>"                  )

   TEST_LINE( oValue:x1                       , "<X1>"                                 )
   TEST_LINE( oValue:y1                       , "<Y1>"                                 )
   TEST_LINE( oValue:z1                       , "<Z1>"                                 )
   TEST_LINE( oValue:x2                       , "[X2]"                                 )
   TEST_LINE( oValue:y2                       , "[Y2]"                                 )
   TEST_LINE( oValue:z2                       , "[Z2]"                                 )
   TEST_LINE( oValue:x3                       , "[X3]"                                 )
   TEST_LINE( oValue:y3                       , "[Y3]"                                 )
   TEST_LINE( oValue:z3                       , "[Z3]"                                 )
   TEST_LINE( oValue:x4                       , "[X4]"                                 )
   TEST_LINE( oValue:y4                       , "[Y4]"                                 )
   TEST_LINE( oValue:z4                       , "[Z4]"                                 )
   TEST_LINE( INSTANCE_DATA( oValue )         , "[12]: <X1> <Y1> <Z1> [X2] [Y2] [Z2] [X3] [Y3] [Z3] [X4] [Y4] [Z4]" )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS1:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS2:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS3:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS4:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:classH )             , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS1:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS2:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS3:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS4:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:classH )             , 0                        )

   /* Setting IVARSCLASS3:IVARSCLASS2 instance variables... */
   TEST_LINE( oValue:IVARSCLASS3:IVARSCLASS2:x2 := "<X2>"    , "<X2>"                  )
   TEST_LINE( oValue:IVARSCLASS3:IVARSCLASS2:y2 := "<Y2>"    , "<Y2>"                  )
   TEST_LINE( oValue:IVARSCLASS3:IVARSCLASS2:z2 := "<Z2>"    , "<Z2>"                  )

   TEST_LINE( oValue:x1                       , "<X1>"                                 )
   TEST_LINE( oValue:y1                       , "<Y1>"                                 )
   TEST_LINE( oValue:z1                       , "<Z1>"                                 )
   TEST_LINE( oValue:x2                       , "<X2>"                                 )
   TEST_LINE( oValue:y2                       , "<Y2>"                                 )
   TEST_LINE( oValue:z2                       , "<Z2>"                                 )
   TEST_LINE( oValue:x3                       , "[X3]"                                 )
   TEST_LINE( oValue:y3                       , "[Y3]"                                 )
   TEST_LINE( oValue:z3                       , "[Z3]"                                 )
   TEST_LINE( oValue:x4                       , "[X4]"                                 )
   TEST_LINE( oValue:y4                       , "[Y4]"                                 )
   TEST_LINE( oValue:z4                       , "[Z4]"                                 )
   TEST_LINE( INSTANCE_DATA( oValue )         , "[12]: <X1> <Y1> <Z1> <X2> <Y2> <Z2> [X3] [Y3] [Z3] [X4] [Y4] [Z4]" )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS1:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS2:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS3:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS4:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:classH )             , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS1:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS2:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS3:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS4:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:classH )             , 0                        )

   /* Setting SUPER instance variables... */
   TEST_LINE( oValue:super:x1 := "{X1}"       , "{X1}"                                 )
   TEST_LINE( oValue:super:y1 := "{Y1}"       , "{Y1}"                                 )
   TEST_LINE( oValue:super:z1 := "{Z1}"       , "{Z1}"                                 )
   TEST_LINE( oValue:super:x2 := "{X2}"       , "{X2}"                                 )
   TEST_LINE( oValue:super:y2 := "{Y2}"       , "{Y2}"                                 )
   TEST_LINE( oValue:super:z2 := "{Z2}"       , "{Z2}"                                 )
   TEST_LINE( oValue:super:x3 := "{X3}"       , "{X3}"                                 )
   TEST_LINE( oValue:super:y3 := "{Y3}"       , "{Y3}"                                 )
   TEST_LINE( oValue:super:z3 := "{Z3}"       , "{Z3}"                                 )

   TEST_LINE( oValue:x1                       , "{X1}"                                 )
   TEST_LINE( oValue:y1                       , "{Y1}"                                 )
   TEST_LINE( oValue:z1                       , "{Z1}"                                 )
   TEST_LINE( oValue:x2                       , "{X2}"                                 )
   TEST_LINE( oValue:y2                       , "{Y2}"                                 )
   TEST_LINE( oValue:z2                       , "{Z2}"                                 )
   TEST_LINE( oValue:x3                       , "{X3}"                                 )
   TEST_LINE( oValue:y3                       , "{Y3}"                                 )
   TEST_LINE( oValue:z3                       , "{Z3}"                                 )
   TEST_LINE( oValue:x4                       , "[X4]"                                 )
   TEST_LINE( oValue:y4                       , "[Y4]"                                 )
   TEST_LINE( oValue:z4                       , "[Z4]"                                 )
   TEST_LINE( INSTANCE_DATA( oValue )         , "[12]: {X1} {Y1} {Z1} {X2} {Y2} {Z2} {X3} {Y3} {Z3} [X4] [Y4] [Z4]" )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS1:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS2:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS3:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:IVARSCLASS4:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:classH )             , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS1:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS2:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS3:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:IVARSCLASS4:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:classH )             , 0                        )


   /* Test class variables allocating and casting */

   oValue := CVARSCLASS4():new()

   TEST_LINE( oValue:x1                       , "(x1)"                                 )
   TEST_LINE( oValue:y1                       , "(y1)"                                 )
   TEST_LINE( oValue:z1                       , "(z1)"                                 )
   TEST_LINE( oValue:x2                       , "(x2)"                                 )
   TEST_LINE( oValue:y2                       , "(y2)"                                 )
   TEST_LINE( oValue:z2                       , "(z2)"                                 )
   TEST_LINE( oValue:x3                       , "(x3)"                                 )
   TEST_LINE( oValue:y3                       , "(y3)"                                 )
   TEST_LINE( oValue:z3                       , "(z3)"                                 )
   TEST_LINE( oValue:x4                       , "(x4)"                                 )
   TEST_LINE( oValue:y4                       , "(y4)"                                 )
   TEST_LINE( oValue:z4                       , "(z4)"                                 )
   TEST_LINE( oValue:CVARSCLASS1:x1           , "(x1)"                                 )
   TEST_LINE( oValue:CVARSCLASS1:y1           , "(y1)"                                 )
   TEST_LINE( oValue:CVARSCLASS1:z1           , "(z1)"                                 )
   TEST_LINE( oValue:CVARSCLASS2:x1           , "(x1)"                                 )
   TEST_LINE( oValue:CVARSCLASS2:y1           , "(y1)"                                 )
   TEST_LINE( oValue:CVARSCLASS2:z1           , "(z1)"                                 )
   TEST_LINE( oValue:CVARSCLASS2:x2           , "(x2)"                                 )
   TEST_LINE( oValue:CVARSCLASS2:y2           , "(y2)"                                 )
   TEST_LINE( oValue:CVARSCLASS2:z2           , "(z2)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:x1           , "(x1)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:y1           , "(y1)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:z1           , "(z1)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:x2           , "(x2)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:y2           , "(y2)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:z2           , "(z2)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:x3           , "(x3)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:y3           , "(y3)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:z3           , "(z3)"                                 )
   TEST_LINE( oValue:CVARSCLASS4:x1           , "(x1)"                                 )
   TEST_LINE( oValue:CVARSCLASS4:y1           , "(y1)"                                 )
   TEST_LINE( oValue:CVARSCLASS4:z1           , "(z1)"                                 )
   TEST_LINE( oValue:CVARSCLASS4:x2           , "(x2)"                                 )
   TEST_LINE( oValue:CVARSCLASS4:y2           , "(y2)"                                 )
   TEST_LINE( oValue:CVARSCLASS4:z2           , "(z2)"                                 )
   TEST_LINE( oValue:CVARSCLASS4:x3           , "(x3)"                                 )
   TEST_LINE( oValue:CVARSCLASS4:y3           , "(y3)"                                 )
   TEST_LINE( oValue:CVARSCLASS4:z3           , "(z3)"                                 )
   TEST_LINE( oValue:CVARSCLASS4:x4           , "(x4)"                                 )
   TEST_LINE( oValue:CVARSCLASS4:y4           , "(y4)"                                 )
   TEST_LINE( oValue:CVARSCLASS4:z4           , "(z4)"                                 )
   TEST_LINE( INSTANCE_DATA( oValue )         , "[0]:"                                 )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:CVARSCLASS1:classH ) , 3                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:CVARSCLASS2:classH ) , 6                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:CVARSCLASS3:classH ) , 9                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:CVARSCLASS4:classH ) , 12                       )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:classH )             , 12                       )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:CVARSCLASS1:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:CVARSCLASS2:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:CVARSCLASS3:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:CVARSCLASS4:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:classH )             , 0                        )

   /* simple assignment... */
   TEST_LINE( oValue:x1 := " X1 "             , " X1 "                                 )
   TEST_LINE( oValue:y1 := " Y1 "             , " Y1 "                                 )
   TEST_LINE( oValue:z1 := " Z1 "             , " Z1 "                                 )
   TEST_LINE( oValue:x2 := " X2 "             , " X2 "                                 )
   TEST_LINE( oValue:y2 := " Y2 "             , " Y2 "                                 )
   TEST_LINE( oValue:z2 := " Z2 "             , " Z2 "                                 )
   TEST_LINE( oValue:x3 := " X3 "             , " X3 "                                 )
   TEST_LINE( oValue:y3 := " Y3 "             , " Y3 "                                 )
   TEST_LINE( oValue:z3 := " Z3 "             , " Z3 "                                 )
   TEST_LINE( oValue:x4 := " X4 "             , " X4 "                                 )
   TEST_LINE( oValue:y4 := " Y4 "             , " Y4 "                                 )
   TEST_LINE( oValue:z4 := " Z4 "             , " Z4 "                                 )

   TEST_LINE( oValue:x1                       , " X1 "                                 )
   TEST_LINE( oValue:y1                       , " Y1 "                                 )
   TEST_LINE( oValue:z1                       , " Z1 "                                 )
   TEST_LINE( oValue:x2                       , " X2 "                                 )
   TEST_LINE( oValue:y2                       , " Y2 "                                 )
   TEST_LINE( oValue:z2                       , " Z2 "                                 )
   TEST_LINE( oValue:x3                       , " X3 "                                 )
   TEST_LINE( oValue:y3                       , " Y3 "                                 )
   TEST_LINE( oValue:z3                       , " Z3 "                                 )
   TEST_LINE( oValue:x4                       , " X4 "                                 )
   TEST_LINE( oValue:y4                       , " Y4 "                                 )
   TEST_LINE( oValue:z4                       , " Z4 "                                 )
   TEST_LINE( oValue:CVARSCLASS1:x1           , "(x1)"                                 )
   TEST_LINE( oValue:CVARSCLASS1:y1           , "(y1)"                                 )
   TEST_LINE( oValue:CVARSCLASS1:z1           , "(z1)"                                 )
   TEST_LINE( oValue:CVARSCLASS2:x1           , "(x1)"                                 )
   TEST_LINE( oValue:CVARSCLASS2:y1           , "(y1)"                                 )
   TEST_LINE( oValue:CVARSCLASS2:z1           , "(z1)"                                 )
   TEST_LINE( oValue:CVARSCLASS2:x2           , "(x2)"                                 )
   TEST_LINE( oValue:CVARSCLASS2:y2           , "(y2)"                                 )
   TEST_LINE( oValue:CVARSCLASS2:z2           , "(z2)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:x1           , "(x1)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:y1           , "(y1)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:z1           , "(z1)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:x2           , "(x2)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:y2           , "(y2)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:z2           , "(z2)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:x3           , "(x3)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:y3           , "(y3)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:z3           , "(z3)"                                 )
   TEST_LINE( oValue:CVARSCLASS4:x1           , " X1 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:y1           , " Y1 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:z1           , " Z1 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:x2           , " X2 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:y2           , " Y2 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:z2           , " Z2 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:x3           , " X3 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:y3           , " Y3 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:z3           , " Z3 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:x4           , " X4 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:y4           , " Y4 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:z4           , " Z4 "                                 )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:CVARSCLASS1:classH ) , 3                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:CVARSCLASS2:classH ) , 6                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:CVARSCLASS3:classH ) , 9                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:CVARSCLASS4:classH ) , 12                       )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:classH )             , 12                       )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:CVARSCLASS1:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:CVARSCLASS2:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:CVARSCLASS3:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:CVARSCLASS4:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:classH )             , 0                        )

   /* Setting CVARSCLASS1 class variables... */
   TEST_LINE( oValue:CVARSCLASS1:x1 := "[X1]"    , "[X1]"                              )
   TEST_LINE( oValue:CVARSCLASS1:y1 := "[Y1]"    , "[Y1]"                              )
   TEST_LINE( oValue:CVARSCLASS1:z1 := "[Z1]"    , "[Z1]"                              )

   TEST_LINE( oValue:x1                       , " X1 "                                 )
   TEST_LINE( oValue:y1                       , " Y1 "                                 )
   TEST_LINE( oValue:z1                       , " Z1 "                                 )
   TEST_LINE( oValue:x2                       , " X2 "                                 )
   TEST_LINE( oValue:y2                       , " Y2 "                                 )
   TEST_LINE( oValue:z2                       , " Z2 "                                 )
   TEST_LINE( oValue:x3                       , " X3 "                                 )
   TEST_LINE( oValue:y3                       , " Y3 "                                 )
   TEST_LINE( oValue:z3                       , " Z3 "                                 )
   TEST_LINE( oValue:x4                       , " X4 "                                 )
   TEST_LINE( oValue:y4                       , " Y4 "                                 )
   TEST_LINE( oValue:z4                       , " Z4 "                                 )
   TEST_LINE( oValue:CVARSCLASS1:x1           , "[X1]"                                 )
   TEST_LINE( oValue:CVARSCLASS1:y1           , "[Y1]"                                 )
   TEST_LINE( oValue:CVARSCLASS1:z1           , "[Z1]"                                 )
   TEST_LINE( oValue:CVARSCLASS2:x1           , "(x1)"                                 )
   TEST_LINE( oValue:CVARSCLASS2:y1           , "(y1)"                                 )
   TEST_LINE( oValue:CVARSCLASS2:z1           , "(z1)"                                 )
   TEST_LINE( oValue:CVARSCLASS2:x2           , "(x2)"                                 )
   TEST_LINE( oValue:CVARSCLASS2:y2           , "(y2)"                                 )
   TEST_LINE( oValue:CVARSCLASS2:z2           , "(z2)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:x1           , "(x1)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:y1           , "(y1)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:z1           , "(z1)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:x2           , "(x2)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:y2           , "(y2)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:z2           , "(z2)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:x3           , "(x3)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:y3           , "(y3)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:z3           , "(z3)"                                 )
   TEST_LINE( oValue:CVARSCLASS4:x1           , " X1 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:y1           , " Y1 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:z1           , " Z1 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:x2           , " X2 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:y2           , " Y2 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:z2           , " Z2 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:x3           , " X3 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:y3           , " Y3 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:z3           , " Z3 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:x4           , " X4 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:y4           , " Y4 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:z4           , " Z4 "                                 )
   TEST_LINE( INSTANCE_DATA( oValue )         , "[0]:"                                 )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:CVARSCLASS1:classH ) , 3                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:CVARSCLASS2:classH ) , 6                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:CVARSCLASS3:classH ) , 9                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:CVARSCLASS4:classH ) , 12                       )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:classH )             , 12                       )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:CVARSCLASS1:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:CVARSCLASS2:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:CVARSCLASS3:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:CVARSCLASS4:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:classH )             , 0                        )

   /* Setting CVARSCLASS2 class variables... */
   TEST_LINE( oValue:CVARSCLASS2:x1 := "{X1}"    , "{X1}"                              )
   TEST_LINE( oValue:CVARSCLASS2:y1 := "{Y1}"    , "{Y1}"                              )
   TEST_LINE( oValue:CVARSCLASS2:z1 := "{Z1}"    , "{Z1}"                              )
   TEST_LINE( oValue:CVARSCLASS2:x2 := "{X2}"    , "{X2}"                              )
   TEST_LINE( oValue:CVARSCLASS2:y2 := "{Y2}"    , "{Y2}"                              )
   TEST_LINE( oValue:CVARSCLASS2:z2 := "{Z2}"    , "{Z2}"                              )

   TEST_LINE( oValue:x1                       , " X1 "                                 )
   TEST_LINE( oValue:y1                       , " Y1 "                                 )
   TEST_LINE( oValue:z1                       , " Z1 "                                 )
   TEST_LINE( oValue:x2                       , " X2 "                                 )
   TEST_LINE( oValue:y2                       , " Y2 "                                 )
   TEST_LINE( oValue:z2                       , " Z2 "                                 )
   TEST_LINE( oValue:x3                       , " X3 "                                 )
   TEST_LINE( oValue:y3                       , " Y3 "                                 )
   TEST_LINE( oValue:z3                       , " Z3 "                                 )
   TEST_LINE( oValue:x4                       , " X4 "                                 )
   TEST_LINE( oValue:y4                       , " Y4 "                                 )
   TEST_LINE( oValue:z4                       , " Z4 "                                 )
   TEST_LINE( oValue:CVARSCLASS1:x1           , "[X1]"                                 )
   TEST_LINE( oValue:CVARSCLASS1:y1           , "[Y1]"                                 )
   TEST_LINE( oValue:CVARSCLASS1:z1           , "[Z1]"                                 )
   TEST_LINE( oValue:CVARSCLASS2:x1           , "{X1}"                                 )
   TEST_LINE( oValue:CVARSCLASS2:y1           , "{Y1}"                                 )
   TEST_LINE( oValue:CVARSCLASS2:z1           , "{Z1}"                                 )
   TEST_LINE( oValue:CVARSCLASS2:x2           , "{X2}"                                 )
   TEST_LINE( oValue:CVARSCLASS2:y2           , "{Y2}"                                 )
   TEST_LINE( oValue:CVARSCLASS2:z2           , "{Z2}"                                 )
   TEST_LINE( oValue:CVARSCLASS3:x1           , "(x1)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:y1           , "(y1)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:z1           , "(z1)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:x2           , "(x2)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:y2           , "(y2)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:z2           , "(z2)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:x3           , "(x3)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:y3           , "(y3)"                                 )
   TEST_LINE( oValue:CVARSCLASS3:z3           , "(z3)"                                 )
   TEST_LINE( oValue:CVARSCLASS4:x1           , " X1 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:y1           , " Y1 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:z1           , " Z1 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:x2           , " X2 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:y2           , " Y2 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:z2           , " Z2 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:x3           , " X3 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:y3           , " Y3 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:z3           , " Z3 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:x4           , " X4 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:y4           , " Y4 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:z4           , " Z4 "                                 )
   TEST_LINE( INSTANCE_DATA( oValue )         , "[0]:"                                 )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:CVARSCLASS1:classH ) , 3                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:CVARSCLASS2:classH ) , 6                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:CVARSCLASS3:classH ) , 9                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:CVARSCLASS4:classH ) , 12                       )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:classH )             , 12                       )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:CVARSCLASS1:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:CVARSCLASS2:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:CVARSCLASS3:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:CVARSCLASS4:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:classH )             , 0                        )

   /* Setting CVARSCLASS3 class variables... */
   TEST_LINE( oValue:CVARSCLASS3:x1 := "<X1>"    , "<X1>"                              )
   TEST_LINE( oValue:CVARSCLASS3:y1 := "<Y1>"    , "<Y1>"                              )
   TEST_LINE( oValue:CVARSCLASS3:z1 := "<Z1>"    , "<Z1>"                              )
   TEST_LINE( oValue:CVARSCLASS3:x2 := "<X2>"    , "<X2>"                              )
   TEST_LINE( oValue:CVARSCLASS3:y2 := "<Y2>"    , "<Y2>"                              )
   TEST_LINE( oValue:CVARSCLASS3:z2 := "<Z2>"    , "<Z2>"                              )
   TEST_LINE( oValue:CVARSCLASS3:x3 := "<X3>"    , "<X3>"                              )
   TEST_LINE( oValue:CVARSCLASS3:y3 := "<Y3>"    , "<Y3>"                              )
   TEST_LINE( oValue:CVARSCLASS3:z3 := "<Z3>"    , "<Z3>"                              )

   TEST_LINE( oValue:x1                       , " X1 "                                 )
   TEST_LINE( oValue:y1                       , " Y1 "                                 )
   TEST_LINE( oValue:z1                       , " Z1 "                                 )
   TEST_LINE( oValue:x2                       , " X2 "                                 )
   TEST_LINE( oValue:y2                       , " Y2 "                                 )
   TEST_LINE( oValue:z2                       , " Z2 "                                 )
   TEST_LINE( oValue:x3                       , " X3 "                                 )
   TEST_LINE( oValue:y3                       , " Y3 "                                 )
   TEST_LINE( oValue:z3                       , " Z3 "                                 )
   TEST_LINE( oValue:x4                       , " X4 "                                 )
   TEST_LINE( oValue:y4                       , " Y4 "                                 )
   TEST_LINE( oValue:z4                       , " Z4 "                                 )
   TEST_LINE( oValue:CVARSCLASS1:x1           , "[X1]"                                 )
   TEST_LINE( oValue:CVARSCLASS1:y1           , "[Y1]"                                 )
   TEST_LINE( oValue:CVARSCLASS1:z1           , "[Z1]"                                 )
   TEST_LINE( oValue:CVARSCLASS2:x1           , "{X1}"                                 )
   TEST_LINE( oValue:CVARSCLASS2:y1           , "{Y1}"                                 )
   TEST_LINE( oValue:CVARSCLASS2:z1           , "{Z1}"                                 )
   TEST_LINE( oValue:CVARSCLASS2:x2           , "{X2}"                                 )
   TEST_LINE( oValue:CVARSCLASS2:y2           , "{Y2}"                                 )
   TEST_LINE( oValue:CVARSCLASS2:z2           , "{Z2}"                                 )
   TEST_LINE( oValue:CVARSCLASS3:x1           , "<X1>"                                 )
   TEST_LINE( oValue:CVARSCLASS3:y1           , "<Y1>"                                 )
   TEST_LINE( oValue:CVARSCLASS3:z1           , "<Z1>"                                 )
   TEST_LINE( oValue:CVARSCLASS3:x2           , "<X2>"                                 )
   TEST_LINE( oValue:CVARSCLASS3:y2           , "<Y2>"                                 )
   TEST_LINE( oValue:CVARSCLASS3:z2           , "<Z2>"                                 )
   TEST_LINE( oValue:CVARSCLASS3:x3           , "<X3>"                                 )
   TEST_LINE( oValue:CVARSCLASS3:y3           , "<Y3>"                                 )
   TEST_LINE( oValue:CVARSCLASS3:z3           , "<Z3>"                                 )
   TEST_LINE( oValue:CVARSCLASS4:x1           , " X1 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:y1           , " Y1 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:z1           , " Z1 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:x2           , " X2 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:y2           , " Y2 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:z2           , " Z2 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:x3           , " X3 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:y3           , " Y3 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:z3           , " Z3 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:x4           , " X4 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:y4           , " Y4 "                                 )
   TEST_LINE( oValue:CVARSCLASS4:z4           , " Z4 "                                 )
   TEST_LINE( INSTANCE_DATA( oValue )         , "[0]:"                                 )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:CVARSCLASS1:classH ) , 3                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:CVARSCLASS2:classH ) , 6                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:CVARSCLASS3:classH ) , 9                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:CVARSCLASS4:classH ) , 12                       )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:classH )             , 12                       )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:CVARSCLASS1:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:CVARSCLASS2:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:CVARSCLASS3:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:CVARSCLASS4:classH ) , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:classH )             , 0                        )


   /* Test shared class variables allocating and casting */

   oValue := SVARSCLASS4():new()

   TEST_LINE( oValue:x1                       , "(x1)"                                 )
   TEST_LINE( oValue:y1                       , "(y1)"                                 )
   TEST_LINE( oValue:z1                       , "(z1)"                                 )
   TEST_LINE( oValue:x2                       , "(x2)"                                 )
   TEST_LINE( oValue:y2                       , "(y2)"                                 )
   TEST_LINE( oValue:z2                       , "(z2)"                                 )
   TEST_LINE( oValue:x3                       , "(x3)"                                 )
   TEST_LINE( oValue:y3                       , "(y3)"                                 )
   TEST_LINE( oValue:z3                       , "(z3)"                                 )
   TEST_LINE( oValue:x4                       , "(x4)"                                 )
   TEST_LINE( oValue:y4                       , "(y4)"                                 )
   TEST_LINE( oValue:z4                       , "(z4)"                                 )
   TEST_LINE( oValue:SVARSCLASS1:x1           , "(x1)"                                 )
   TEST_LINE( oValue:SVARSCLASS1:y1           , "(y1)"                                 )
   TEST_LINE( oValue:SVARSCLASS1:z1           , "(z1)"                                 )
   TEST_LINE( oValue:SVARSCLASS2:x1           , "(x1)"                                 )
   TEST_LINE( oValue:SVARSCLASS2:y1           , "(y1)"                                 )
   TEST_LINE( oValue:SVARSCLASS2:z1           , "(z1)"                                 )
   TEST_LINE( oValue:SVARSCLASS2:x2           , "(x2)"                                 )
   TEST_LINE( oValue:SVARSCLASS2:y2           , "(y2)"                                 )
   TEST_LINE( oValue:SVARSCLASS2:z2           , "(z2)"                                 )
   TEST_LINE( oValue:SVARSCLASS3:x1           , "(x1)"                                 )
   TEST_LINE( oValue:SVARSCLASS3:y1           , "(y1)"                                 )
   TEST_LINE( oValue:SVARSCLASS3:z1           , "(z1)"                                 )
   TEST_LINE( oValue:SVARSCLASS3:x2           , "(x2)"                                 )
   TEST_LINE( oValue:SVARSCLASS3:y2           , "(y2)"                                 )
   TEST_LINE( oValue:SVARSCLASS3:z2           , "(z2)"                                 )
   TEST_LINE( oValue:SVARSCLASS3:x3           , "(x3)"                                 )
   TEST_LINE( oValue:SVARSCLASS3:y3           , "(y3)"                                 )
   TEST_LINE( oValue:SVARSCLASS3:z3           , "(z3)"                                 )
   TEST_LINE( oValue:SVARSCLASS4:x1           , "(x1)"                                 )
   TEST_LINE( oValue:SVARSCLASS4:y1           , "(y1)"                                 )
   TEST_LINE( oValue:SVARSCLASS4:z1           , "(z1)"                                 )
   TEST_LINE( oValue:SVARSCLASS4:x2           , "(x2)"                                 )
   TEST_LINE( oValue:SVARSCLASS4:y2           , "(y2)"                                 )
   TEST_LINE( oValue:SVARSCLASS4:z2           , "(z2)"                                 )
   TEST_LINE( oValue:SVARSCLASS4:x3           , "(x3)"                                 )
   TEST_LINE( oValue:SVARSCLASS4:y3           , "(y3)"                                 )
   TEST_LINE( oValue:SVARSCLASS4:z3           , "(z3)"                                 )
   TEST_LINE( oValue:SVARSCLASS4:x4           , "(x4)"                                 )
   TEST_LINE( oValue:SVARSCLASS4:y4           , "(y4)"                                 )
   TEST_LINE( oValue:SVARSCLASS4:z4           , "(z4)"                                 )
   TEST_LINE( INSTANCE_DATA( oValue )         , "[0]:"                                 )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:SVARSCLASS1:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:SVARSCLASS2:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:SVARSCLASS3:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:SVARSCLASS4:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:classH )             , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:SVARSCLASS1:classH ) , 3                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:SVARSCLASS2:classH ) , 3                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:SVARSCLASS3:classH ) , 3                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:SVARSCLASS4:classH ) , 3                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:classH )             , 3                        )

   /* simple assignment... */
   TEST_LINE( oValue:x1 := " X1 "             , " X1 "                                 )
   TEST_LINE( oValue:y1 := " Y1 "             , " Y1 "                                 )
   TEST_LINE( oValue:z1 := " Z1 "             , " Z1 "                                 )
   TEST_LINE( oValue:x2 := " X2 "             , " X2 "                                 )
   TEST_LINE( oValue:y2 := " Y2 "             , " Y2 "                                 )
   TEST_LINE( oValue:z2 := " Z2 "             , " Z2 "                                 )
   TEST_LINE( oValue:x3 := " X3 "             , " X3 "                                 )
   TEST_LINE( oValue:y3 := " Y3 "             , " Y3 "                                 )
   TEST_LINE( oValue:z3 := " Z3 "             , " Z3 "                                 )
   TEST_LINE( oValue:x4 := " X4 "             , " X4 "                                 )
   TEST_LINE( oValue:y4 := " Y4 "             , " Y4 "                                 )
   TEST_LINE( oValue:z4 := " Z4 "             , " Z4 "                                 )

   TEST_LINE( oValue:x1                       , " X1 "                                 )
   TEST_LINE( oValue:y1                       , " Y1 "                                 )
   TEST_LINE( oValue:z1                       , " Z1 "                                 )
   TEST_LINE( oValue:x2                       , " X2 "                                 )
   TEST_LINE( oValue:y2                       , " Y2 "                                 )
   TEST_LINE( oValue:z2                       , " Z2 "                                 )
   TEST_LINE( oValue:x3                       , " X3 "                                 )
   TEST_LINE( oValue:y3                       , " Y3 "                                 )
   TEST_LINE( oValue:z3                       , " Z3 "                                 )
   TEST_LINE( oValue:x4                       , " X4 "                                 )
   TEST_LINE( oValue:y4                       , " Y4 "                                 )
   TEST_LINE( oValue:z4                       , " Z4 "                                 )
   TEST_LINE( oValue:SVARSCLASS1:x1           , " X1 "                                 )
   TEST_LINE( oValue:SVARSCLASS1:y1           , " Y1 "                                 )
   TEST_LINE( oValue:SVARSCLASS1:z1           , " Z1 "                                 )
   TEST_LINE( oValue:SVARSCLASS2:x1           , " X1 "                                 )
   TEST_LINE( oValue:SVARSCLASS2:y1           , " Y1 "                                 )
   TEST_LINE( oValue:SVARSCLASS2:z1           , " Z1 "                                 )
   TEST_LINE( oValue:SVARSCLASS2:x2           , " X2 "                                 )
   TEST_LINE( oValue:SVARSCLASS2:y2           , " Y2 "                                 )
   TEST_LINE( oValue:SVARSCLASS2:z2           , " Z2 "                                 )
   TEST_LINE( oValue:SVARSCLASS3:x1           , " X1 "                                 )
   TEST_LINE( oValue:SVARSCLASS3:y1           , " Y1 "                                 )
   TEST_LINE( oValue:SVARSCLASS3:z1           , " Z1 "                                 )
   TEST_LINE( oValue:SVARSCLASS3:x2           , " X2 "                                 )
   TEST_LINE( oValue:SVARSCLASS3:y2           , " Y2 "                                 )
   TEST_LINE( oValue:SVARSCLASS3:z2           , " Z2 "                                 )
   TEST_LINE( oValue:SVARSCLASS3:x3           , " X3 "                                 )
   TEST_LINE( oValue:SVARSCLASS3:y3           , " Y3 "                                 )
   TEST_LINE( oValue:SVARSCLASS3:z3           , " Z3 "                                 )
   TEST_LINE( oValue:SVARSCLASS4:x1           , " X1 "                                 )
   TEST_LINE( oValue:SVARSCLASS4:y1           , " Y1 "                                 )
   TEST_LINE( oValue:SVARSCLASS4:z1           , " Z1 "                                 )
   TEST_LINE( oValue:SVARSCLASS4:x2           , " X2 "                                 )
   TEST_LINE( oValue:SVARSCLASS4:y2           , " Y2 "                                 )
   TEST_LINE( oValue:SVARSCLASS4:z2           , " Z2 "                                 )
   TEST_LINE( oValue:SVARSCLASS4:x3           , " X3 "                                 )
   TEST_LINE( oValue:SVARSCLASS4:y3           , " Y3 "                                 )
   TEST_LINE( oValue:SVARSCLASS4:z3           , " Z3 "                                 )
   TEST_LINE( oValue:SVARSCLASS4:x4           , " X4 "                                 )
   TEST_LINE( oValue:SVARSCLASS4:y4           , " Y4 "                                 )
   TEST_LINE( oValue:SVARSCLASS4:z4           , " Z4 "                                 )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:SVARSCLASS1:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:SVARSCLASS2:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:SVARSCLASS3:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:SVARSCLASS4:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:classH )             , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:SVARSCLASS1:classH ) , 3                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:SVARSCLASS2:classH ) , 3                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:SVARSCLASS3:classH ) , 3                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:SVARSCLASS4:classH ) , 3                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:classH )             , 3                        )
   TEST_LINE( INSTANCE_DATA( oValue )         , "[0]:"                                 )

   /* Setting SVARSCLASS1 class variables... */
   TEST_LINE( oValue:SVARSCLASS1:x1 := "[X1]"    , "[X1]"                              )
   TEST_LINE( oValue:SVARSCLASS1:y1 := "[Y1]"    , "[Y1]"                              )
   TEST_LINE( oValue:SVARSCLASS1:z1 := "[Z1]"    , "[Z1]"                              )

   TEST_LINE( oValue:x1                       , "[X1]"                                 )
   TEST_LINE( oValue:y1                       , "[Y1]"                                 )
   TEST_LINE( oValue:z1                       , "[Z1]"                                 )
   TEST_LINE( oValue:x2                       , " X2 "                                 )
   TEST_LINE( oValue:y2                       , " Y2 "                                 )
   TEST_LINE( oValue:z2                       , " Z2 "                                 )
   TEST_LINE( oValue:x3                       , " X3 "                                 )
   TEST_LINE( oValue:y3                       , " Y3 "                                 )
   TEST_LINE( oValue:z3                       , " Z3 "                                 )
   TEST_LINE( oValue:x4                       , " X4 "                                 )
   TEST_LINE( oValue:y4                       , " Y4 "                                 )
   TEST_LINE( oValue:z4                       , " Z4 "                                 )
   TEST_LINE( oValue:SVARSCLASS1:x1           , "[X1]"                                 )
   TEST_LINE( oValue:SVARSCLASS1:y1           , "[Y1]"                                 )
   TEST_LINE( oValue:SVARSCLASS1:z1           , "[Z1]"                                 )
   TEST_LINE( oValue:SVARSCLASS2:x1           , "[X1]"                                 )
   TEST_LINE( oValue:SVARSCLASS2:y1           , "[Y1]"                                 )
   TEST_LINE( oValue:SVARSCLASS2:z1           , "[Z1]"                                 )
   TEST_LINE( oValue:SVARSCLASS2:x2           , " X2 "                                 )
   TEST_LINE( oValue:SVARSCLASS2:y2           , " Y2 "                                 )
   TEST_LINE( oValue:SVARSCLASS2:z2           , " Z2 "                                 )
   TEST_LINE( oValue:SVARSCLASS3:x1           , "[X1]"                                 )
   TEST_LINE( oValue:SVARSCLASS3:y1           , "[Y1]"                                 )
   TEST_LINE( oValue:SVARSCLASS3:z1           , "[Z1]"                                 )
   TEST_LINE( oValue:SVARSCLASS3:x2           , " X2 "                                 )
   TEST_LINE( oValue:SVARSCLASS3:y2           , " Y2 "                                 )
   TEST_LINE( oValue:SVARSCLASS3:z2           , " Z2 "                                 )
   TEST_LINE( oValue:SVARSCLASS3:x3           , " X3 "                                 )
   TEST_LINE( oValue:SVARSCLASS3:y3           , " Y3 "                                 )
   TEST_LINE( oValue:SVARSCLASS3:z3           , " Z3 "                                 )
   TEST_LINE( oValue:SVARSCLASS4:x1           , "[X1]"                                 )
   TEST_LINE( oValue:SVARSCLASS4:y1           , "[Y1]"                                 )
   TEST_LINE( oValue:SVARSCLASS4:z1           , "[Z1]"                                 )
   TEST_LINE( oValue:SVARSCLASS4:x2           , " X2 "                                 )
   TEST_LINE( oValue:SVARSCLASS4:y2           , " Y2 "                                 )
   TEST_LINE( oValue:SVARSCLASS4:z2           , " Z2 "                                 )
   TEST_LINE( oValue:SVARSCLASS4:x3           , " X3 "                                 )
   TEST_LINE( oValue:SVARSCLASS4:y3           , " Y3 "                                 )
   TEST_LINE( oValue:SVARSCLASS4:z3           , " Z3 "                                 )
   TEST_LINE( oValue:SVARSCLASS4:x4           , " X4 "                                 )
   TEST_LINE( oValue:SVARSCLASS4:y4           , " Y4 "                                 )
   TEST_LINE( oValue:SVARSCLASS4:z4           , " Z4 "                                 )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:SVARSCLASS1:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:SVARSCLASS2:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:SVARSCLASS3:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:SVARSCLASS4:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:classH )             , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:SVARSCLASS1:classH ) , 3                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:SVARSCLASS2:classH ) , 3                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:SVARSCLASS3:classH ) , 3                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:SVARSCLASS4:classH ) , 3                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:classH )             , 3                        )
   TEST_LINE( INSTANCE_DATA( oValue )         , "[0]:"                                 )

   /* Setting SVARSCLASS2 class variables... */
   TEST_LINE( oValue:SVARSCLASS2:x1 := "{X1}"    , "{X1}"                              )
   TEST_LINE( oValue:SVARSCLASS2:y1 := "{Y1}"    , "{Y1}"                              )
   TEST_LINE( oValue:SVARSCLASS2:z1 := "{Z1}"    , "{Z1}"                              )
   TEST_LINE( oValue:SVARSCLASS2:x2 := "{X2}"    , "{X2}"                              )
   TEST_LINE( oValue:SVARSCLASS2:y2 := "{Y2}"    , "{Y2}"                              )
   TEST_LINE( oValue:SVARSCLASS2:z2 := "{Z2}"    , "{Z2}"                              )

   TEST_LINE( oValue:x1                       , "{X1}"                                 )
   TEST_LINE( oValue:y1                       , "{Y1}"                                 )
   TEST_LINE( oValue:z1                       , "{Z1}"                                 )
   TEST_LINE( oValue:x2                       , "{X2}"                                 )
   TEST_LINE( oValue:y2                       , "{Y2}"                                 )
   TEST_LINE( oValue:z2                       , "{Z2}"                                 )
   TEST_LINE( oValue:x3                       , " X3 "                                 )
   TEST_LINE( oValue:y3                       , " Y3 "                                 )
   TEST_LINE( oValue:z3                       , " Z3 "                                 )
   TEST_LINE( oValue:x4                       , " X4 "                                 )
   TEST_LINE( oValue:y4                       , " Y4 "                                 )
   TEST_LINE( oValue:z4                       , " Z4 "                                 )
   TEST_LINE( oValue:SVARSCLASS1:x1           , "{X1}"                                 )
   TEST_LINE( oValue:SVARSCLASS1:y1           , "{Y1}"                                 )
   TEST_LINE( oValue:SVARSCLASS1:z1           , "{Z1}"                                 )
   TEST_LINE( oValue:SVARSCLASS2:x1           , "{X1}"                                 )
   TEST_LINE( oValue:SVARSCLASS2:y1           , "{Y1}"                                 )
   TEST_LINE( oValue:SVARSCLASS2:z1           , "{Z1}"                                 )
   TEST_LINE( oValue:SVARSCLASS2:x2           , "{X2}"                                 )
   TEST_LINE( oValue:SVARSCLASS2:y2           , "{Y2}"                                 )
   TEST_LINE( oValue:SVARSCLASS2:z2           , "{Z2}"                                 )
   TEST_LINE( oValue:SVARSCLASS3:x1           , "{X1}"                                 )
   TEST_LINE( oValue:SVARSCLASS3:y1           , "{Y1}"                                 )
   TEST_LINE( oValue:SVARSCLASS3:z1           , "{Z1}"                                 )
   TEST_LINE( oValue:SVARSCLASS3:x2           , "{X2}"                                 )
   TEST_LINE( oValue:SVARSCLASS3:y2           , "{Y2}"                                 )
   TEST_LINE( oValue:SVARSCLASS3:z2           , "{Z2}"                                 )
   TEST_LINE( oValue:SVARSCLASS3:x3           , " X3 "                                 )
   TEST_LINE( oValue:SVARSCLASS3:y3           , " Y3 "                                 )
   TEST_LINE( oValue:SVARSCLASS3:z3           , " Z3 "                                 )
   TEST_LINE( oValue:SVARSCLASS4:x1           , "{X1}"                                 )
   TEST_LINE( oValue:SVARSCLASS4:y1           , "{Y1}"                                 )
   TEST_LINE( oValue:SVARSCLASS4:z1           , "{Z1}"                                 )
   TEST_LINE( oValue:SVARSCLASS4:x2           , "{X2}"                                 )
   TEST_LINE( oValue:SVARSCLASS4:y2           , "{Y2}"                                 )
   TEST_LINE( oValue:SVARSCLASS4:z2           , "{Z2}"                                 )
   TEST_LINE( oValue:SVARSCLASS4:x3           , " X3 "                                 )
   TEST_LINE( oValue:SVARSCLASS4:y3           , " Y3 "                                 )
   TEST_LINE( oValue:SVARSCLASS4:z3           , " Z3 "                                 )
   TEST_LINE( oValue:SVARSCLASS4:x4           , " X4 "                                 )
   TEST_LINE( oValue:SVARSCLASS4:y4           , " Y4 "                                 )
   TEST_LINE( oValue:SVARSCLASS4:z4           , " Z4 "                                 )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:SVARSCLASS1:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:SVARSCLASS2:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:SVARSCLASS3:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:SVARSCLASS4:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:classH )             , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:SVARSCLASS1:classH ) , 3                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:SVARSCLASS2:classH ) , 3                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:SVARSCLASS3:classH ) , 3                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:SVARSCLASS4:classH ) , 3                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:classH )             , 3                        )
   TEST_LINE( INSTANCE_DATA( oValue )         , "[0]:"                                 )

   /* Setting SVARSCLASS3 class variables... */
   TEST_LINE( oValue:SVARSCLASS3:x1 := "<X1>"    , "<X1>"                              )
   TEST_LINE( oValue:SVARSCLASS3:y1 := "<Y1>"    , "<Y1>"                              )
   TEST_LINE( oValue:SVARSCLASS3:z1 := "<Z1>"    , "<Z1>"                              )
   TEST_LINE( oValue:SVARSCLASS3:x2 := "<X2>"    , "<X2>"                              )
   TEST_LINE( oValue:SVARSCLASS3:y2 := "<Y2>"    , "<Y2>"                              )
   TEST_LINE( oValue:SVARSCLASS3:z2 := "<Z2>"    , "<Z2>"                              )
   TEST_LINE( oValue:SVARSCLASS3:x3 := "<X3>"    , "<X3>"                              )
   TEST_LINE( oValue:SVARSCLASS3:y3 := "<Y3>"    , "<Y3>"                              )
   TEST_LINE( oValue:SVARSCLASS3:z3 := "<Z3>"    , "<Z3>"                              )

   TEST_LINE( oValue:x1                       , "<X1>"                                 )
   TEST_LINE( oValue:y1                       , "<Y1>"                                 )
   TEST_LINE( oValue:z1                       , "<Z1>"                                 )
   TEST_LINE( oValue:x2                       , "<X2>"                                 )
   TEST_LINE( oValue:y2                       , "<Y2>"                                 )
   TEST_LINE( oValue:z2                       , "<Z2>"                                 )
   TEST_LINE( oValue:x3                       , "<X3>"                                 )
   TEST_LINE( oValue:y3                       , "<Y3>"                                 )
   TEST_LINE( oValue:z3                       , "<Z3>"                                 )
   TEST_LINE( oValue:x4                       , " X4 "                                 )
   TEST_LINE( oValue:y4                       , " Y4 "                                 )
   TEST_LINE( oValue:z4                       , " Z4 "                                 )
   TEST_LINE( oValue:SVARSCLASS1:x1           , "<X1>"                                 )
   TEST_LINE( oValue:SVARSCLASS1:y1           , "<Y1>"                                 )
   TEST_LINE( oValue:SVARSCLASS1:z1           , "<Z1>"                                 )
   TEST_LINE( oValue:SVARSCLASS2:x1           , "<X1>"                                 )
   TEST_LINE( oValue:SVARSCLASS2:y1           , "<Y1>"                                 )
   TEST_LINE( oValue:SVARSCLASS2:z1           , "<Z1>"                                 )
   TEST_LINE( oValue:SVARSCLASS2:x2           , "<X2>"                                 )
   TEST_LINE( oValue:SVARSCLASS2:y2           , "<Y2>"                                 )
   TEST_LINE( oValue:SVARSCLASS2:z2           , "<Z2>"                                 )
   TEST_LINE( oValue:SVARSCLASS3:x1           , "<X1>"                                 )
   TEST_LINE( oValue:SVARSCLASS3:y1           , "<Y1>"                                 )
   TEST_LINE( oValue:SVARSCLASS3:z1           , "<Z1>"                                 )
   TEST_LINE( oValue:SVARSCLASS3:x2           , "<X2>"                                 )
   TEST_LINE( oValue:SVARSCLASS3:y2           , "<Y2>"                                 )
   TEST_LINE( oValue:SVARSCLASS3:z2           , "<Z2>"                                 )
   TEST_LINE( oValue:SVARSCLASS3:x3           , "<X3>"                                 )
   TEST_LINE( oValue:SVARSCLASS3:y3           , "<Y3>"                                 )
   TEST_LINE( oValue:SVARSCLASS3:z3           , "<Z3>"                                 )
   TEST_LINE( oValue:SVARSCLASS4:x1           , "<X1>"                                 )
   TEST_LINE( oValue:SVARSCLASS4:y1           , "<Y1>"                                 )
   TEST_LINE( oValue:SVARSCLASS4:z1           , "<Z1>"                                 )
   TEST_LINE( oValue:SVARSCLASS4:x2           , "<X2>"                                 )
   TEST_LINE( oValue:SVARSCLASS4:y2           , "<Y2>"                                 )
   TEST_LINE( oValue:SVARSCLASS4:z2           , "<Z2>"                                 )
   TEST_LINE( oValue:SVARSCLASS4:x3           , "<X3>"                                 )
   TEST_LINE( oValue:SVARSCLASS4:y3           , "<Y3>"                                 )
   TEST_LINE( oValue:SVARSCLASS4:z3           , "<Z3>"                                 )
   TEST_LINE( oValue:SVARSCLASS4:x4           , " X4 "                                 )
   TEST_LINE( oValue:SVARSCLASS4:y4           , " Y4 "                                 )
   TEST_LINE( oValue:SVARSCLASS4:z4           , " Z4 "                                 )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:SVARSCLASS1:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:SVARSCLASS2:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:SVARSCLASS3:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:SVARSCLASS4:classH ) , 0                        )
   TEST_LINE( __CLS_CNTCLSDATA( oValue:classH )             , 0                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:SVARSCLASS1:classH ) , 3                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:SVARSCLASS2:classH ) , 3                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:SVARSCLASS3:classH ) , 3                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:SVARSCLASS4:classH ) , 3                        )
   TEST_LINE( __CLS_CNTSHRDATA( oValue:classH )             , 3                        )
   TEST_LINE( INSTANCE_DATA( oValue )         , "[0]:"                                 )



#endif

   RETURN


#ifdef __HARBOUR__

STATIC FUNCTION INSTANCE_DATA( oValue )
   LOCAL cData, i

   cData := "[" + LTRIM( STR( LEN( oValue ) ) ) + "]:"
   FOR i := 1 TO LEN( oValue )
      IF VALTYPE( oValue[ i ] ) == "C"
         cData += " " + oValue[ i ]
      ELSEIF oValue[ i ] == NIL
         cData += " NIL"
      ELSE
         cData += " ..."
      ENDIF
   NEXT

   RETURN cData



CREATE CLASS DTORCLASS
EXPORTED:
   VAR         type
   VAR         var1
   CLASS VAR   var2
   METHOD      init
   DESTRUCTOR  dtor
END CLASS

METHOD INIT( type ) CLASS DTORCLASS
   ::type := type
RETURN self

METHOD PROCEDURE DTOR CLASS DTORCLASS
   IF ::type == 1
      cDtorResult += "Reference to self in instance variable."
      ::var1 := self
   ELSEIF ::Type == 2
      cDtorResult += "Reference to self in class variable."
      ::var2 := self
   ELSEIF ::Type == 3
      cDtorResult += "Reference to self in private memvar."
      objHolder := self
   ELSE
      cDtorResult += "No references to self."
   ENDIF

   RETURN



CREATE CLASS IVARSCLASS1
EXPORTED:
    VAR x1 INIT "(x1)"
    VAR y1 INIT "(y1)"
    VAR z1 INIT "(z1)"
END CLASS

CREATE CLASS IVARSCLASS2 FROM IVARSCLASS1
EXPORTED:
    VAR x2 INIT "(x2)"
    VAR y2 INIT "(y2)"
    VAR z2 INIT "(z2)"
END CLASS

CREATE CLASS IVARSCLASS3 FROM IVARSCLASS1, IVARSCLASS2
EXPORTED:
    VAR x3 INIT "(x3)"
    VAR y3 INIT "(y3)"
    VAR z3 INIT "(z3)"
END CLASS

CREATE CLASS IVARSCLASS4 FROM IVARSCLASS3, IVARSCLASS2
EXPORTED:
    VAR x4 INIT "(x4)"
    VAR y4 INIT "(y4)"
    VAR z4 INIT "(z4)"
END CLASS



CREATE CLASS CVARSCLASS1
EXPORTED:
   CLASS VAR x1 INIT "(x1)"
   CLASS VAR y1 INIT "(y1)"
   CLASS VAR z1 INIT "(z1)"
END CLASS

CREATE CLASS CVARSCLASS2 FROM CVARSCLASS1
EXPORTED:
   CLASS VAR x2 INIT "(x2)"
   CLASS VAR y2 INIT "(y2)"
   CLASS VAR z2 INIT "(z2)"
END CLASS

CREATE CLASS CVARSCLASS3 FROM CVARSCLASS1, CVARSCLASS2
EXPORTED:
   CLASS VAR x3 INIT "(x3)"
   CLASS VAR y3 INIT "(y3)"
   CLASS VAR z3 INIT "(z3)"
END CLASS

CREATE CLASS CVARSCLASS4 FROM CVARSCLASS3, CVARSCLASS2
EXPORTED:
   CLASS VAR x4 INIT "(x4)"
   CLASS VAR y4 INIT "(y4)"
   CLASS VAR z4 INIT "(z4)"
END CLASS



CREATE CLASS SVARSCLASS1
EXPORTED:
   CLASS VAR x1 INIT "(x1)" SHARED
   CLASS VAR y1 INIT "(y1)" SHARED
   CLASS VAR z1 INIT "(z1)" SHARED
endclass

CREATE CLASS SVARSCLASS2 FROM SVARSCLASS1
EXPORTED:
   CLASS VAR x2 INIT "(x2)" SHARED
   CLASS VAR y2 INIT "(y2)" SHARED
   CLASS VAR z2 INIT "(z2)" SHARED
endclass

CREATE CLASS SVARSCLASS3 FROM SVARSCLASS1, SVARSCLASS2
EXPORTED:
   CLASS VAR x3 INIT "(x3)" SHARED
   CLASS VAR y3 INIT "(y3)" SHARED
   CLASS VAR z3 INIT "(z3)" SHARED
endclass

CREATE CLASS SVARSCLASS4 FROM SVARSCLASS3, SVARSCLASS2
EXPORTED:
   CLASS VAR x4 INIT "(x4)" SHARED
   CLASS VAR y4 INIT "(y4)" SHARED
   CLASS VAR z4 INIT "(z4)" SHARED
endclass
#endif


/* Don't change the position of this #include. */
#include "rt_init.ch"
