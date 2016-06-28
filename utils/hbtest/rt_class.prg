/*
 * Harbour class/OOP test
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

   HBTEST cDtorResult := ""               IS ""
   HBTEST objHolder := NIL                IS NIL
   oValue := DTORCLASS():NEW( 0 )
   HBTEST oValue:type                     IS 0
   HBTEST oValue := NIL                   IS NIL
   HBTEST objHolder                       IS NIL
   HBTEST cDtorResult                     IS "No references to self."

   HBTEST cDtorResult := ""               IS ""
   HBTEST objHolder := NIL                IS NIL
   oValue := DTORCLASS():NEW( 1 )
   HBTEST oValue:type                     IS 1
   HBTEST oValue := NIL                   IS NIL
   HBTEST objHolder                       IS NIL
   HBTEST cDtorResult                     IS "Reference to self in instance variable."

   HBTEST cDtorResult := ""               IS ""
   HBTEST objHolder := NIL                IS NIL
   oValue := DTORCLASS():NEW( 2 )
   HBTEST oValue:type                     IS 2
   HBTEST oValue := NIL                   IS "E 45 BASE 1301 Object destructor failure (Reference to freed block) OS:0 #:0 "
   HBTEST objHolder                       IS NIL
   HBTEST cDtorResult                     IS "Reference to self in class variable."

   HBTEST cDtorResult := ""               IS ""
   HBTEST objHolder := NIL                IS NIL
   oValue := DTORCLASS():NEW( 3 )
   HBTEST oValue:type                     IS 3
   HBTEST oValue := NIL                   IS "E 45 BASE 1301 Object destructor failure (Reference to freed block) OS:0 #:0 "
   HBTEST ValType( objHolder )            IS "A"
   HBTEST Len( objHolder )                IS 0
   HBTEST cDtorResult                     IS "Reference to self in private memvar."


   /* Tests with cross references and releasing by Garbage Collector */

   HBTEST cDtorResult := ""               IS ""
   HBTEST objHolder := NIL                IS NIL
   oValue := DTORCLASS():NEW( 0 )
   HBTEST oValue:type                     IS 0
   /* create cross reference */
   aRef := { oValue, NIL }; aRef[ 2 ] := aRef; aRef := NIL
   HBTEST oValue := NIL                   IS NIL
   HBTEST objHolder                       IS NIL
   HBTEST cDtorResult                     IS ""
   HBTEST hb_gcAll()                      IS NIL
   HBTEST objHolder                       IS NIL
   HBTEST cDtorResult                     IS "No references to self."

   HBTEST cDtorResult := ""               IS ""
   HBTEST objHolder := NIL                IS NIL
   oValue := DTORCLASS():NEW( 1 )
   HBTEST oValue:type                     IS 1
   /* create cross reference */
   aRef := { oValue, NIL }; aRef[ 2 ] := aRef; aRef := NIL
   HBTEST oValue := NIL                   IS NIL
   HBTEST objHolder                       IS NIL
   HBTEST cDtorResult                     IS ""
   HBTEST hb_gcAll()                      IS NIL
   HBTEST objHolder                       IS NIL
   HBTEST cDtorResult                     IS "Reference to self in instance variable."

   HBTEST cDtorResult := ""               IS ""
   HBTEST objHolder := NIL                IS NIL
   oValue := DTORCLASS():NEW( 2 )
   HBTEST oValue:type                     IS 2
   /* create cross reference */
   aRef := { oValue, NIL }; aRef[ 2 ] := aRef; aRef := NIL
   HBTEST oValue := NIL                   IS NIL
   HBTEST objHolder                       IS NIL
   HBTEST cDtorResult                     IS ""
   HBTEST hb_gcAll()                      IS "E 45 BASE 1302 Object destructor failure (Reference to freed block) OS:0 #:0 "
   HBTEST objHolder                       IS NIL
   HBTEST cDtorResult                     IS "Reference to self in class variable."

   HBTEST cDtorResult := ""               IS ""
   HBTEST objHolder := NIL                IS NIL
   oValue := DTORCLASS():NEW( 3 )
   HBTEST oValue:type                     IS 3
   /* create cross reference */
   aRef := { oValue, NIL }; aRef[ 2 ] := aRef; aRef := NIL
   HBTEST oValue := NIL                   IS NIL
   HBTEST objHolder                       IS NIL
   HBTEST cDtorResult                     IS ""
   HBTEST hb_gcAll()                      IS "E 45 BASE 1302 Object destructor failure (Reference to freed block) OS:0 #:0 "
   HBTEST ValType( objHolder )            IS "A"
   HBTEST Len( objHolder )                IS 0
   HBTEST cDtorResult                     IS "Reference to self in private memvar."



   /* Test instance area allocating and casting */

   oValue := IVARSCLASS4():new()

   HBTEST oValue:x1                       IS "(x1)"
   HBTEST oValue:y1                       IS "(y1)"
   HBTEST oValue:z1                       IS "(z1)"
   HBTEST oValue:x2                       IS "(x2)"
   HBTEST oValue:y2                       IS "(y2)"
   HBTEST oValue:z2                       IS "(z2)"
   HBTEST oValue:x3                       IS "(x3)"
   HBTEST oValue:y3                       IS "(y3)"
   HBTEST oValue:z3                       IS "(z3)"
   HBTEST oValue:x4                       IS "(x4)"
   HBTEST oValue:y4                       IS "(y4)"
   HBTEST oValue:z4                       IS "(z4)"
   HBTEST INSTANCE_DATA( oValue )         IS "[12]: (x1) (y1) (z1) (x2) (y2) (z2) (x3) (y3) (z3) (x4) (y4) (z4)"
   HBTEST __cls_CntClsData( oValue:IVARSCLASS1:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:IVARSCLASS2:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:IVARSCLASS3:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:IVARSCLASS4:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:classH )             IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS1:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS2:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS3:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS4:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:classH )             IS 0

   /* simple assignment... */
   HBTEST oValue:x1 := " X1 "             IS " X1 "
   HBTEST oValue:y1 := " Y1 "             IS " Y1 "
   HBTEST oValue:z1 := " Z1 "             IS " Z1 "
   HBTEST oValue:x2 := " X2 "             IS " X2 "
   HBTEST oValue:y2 := " Y2 "             IS " Y2 "
   HBTEST oValue:z2 := " Z2 "             IS " Z2 "
   HBTEST oValue:x3 := " X3 "             IS " X3 "
   HBTEST oValue:y3 := " Y3 "             IS " Y3 "
   HBTEST oValue:z3 := " Z3 "             IS " Z3 "
   HBTEST oValue:x4 := " X4 "             IS " X4 "
   HBTEST oValue:y4 := " Y4 "             IS " Y4 "
   HBTEST oValue:z4 := " Z4 "             IS " Z4 "

   HBTEST oValue:x1                       IS " X1 "
   HBTEST oValue:y1                       IS " Y1 "
   HBTEST oValue:z1                       IS " Z1 "
   HBTEST oValue:x2                       IS " X2 "
   HBTEST oValue:y2                       IS " Y2 "
   HBTEST oValue:z2                       IS " Z2 "
   HBTEST oValue:x3                       IS " X3 "
   HBTEST oValue:y3                       IS " Y3 "
   HBTEST oValue:z3                       IS " Z3 "
   HBTEST oValue:x4                       IS " X4 "
   HBTEST oValue:y4                       IS " Y4 "
   HBTEST oValue:z4                       IS " Z4 "
   HBTEST INSTANCE_DATA( oValue )         IS "[12]:  X1   Y1   Z1   X2   Y2   Z2   X3   Y3   Z3   X4   Y4   Z4 "
   HBTEST __cls_CntClsData( oValue:IVARSCLASS1:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:IVARSCLASS2:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:IVARSCLASS3:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:IVARSCLASS4:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:classH )             IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS1:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS2:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS3:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS4:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:classH )             IS 0

   /* Setting IVARSCLASS1 instance variables... */
   HBTEST oValue:IVARSCLASS1:x1 := "[X1]"    IS "[X1]"
   HBTEST oValue:IVARSCLASS1:y1 := "[Y1]"    IS "[Y1]"
   HBTEST oValue:IVARSCLASS1:z1 := "[Z1]"    IS "[Z1]"

   HBTEST oValue:x1                       IS "[X1]"
   HBTEST oValue:y1                       IS "[Y1]"
   HBTEST oValue:z1                       IS "[Z1]"
   HBTEST oValue:x2                       IS " X2 "
   HBTEST oValue:y2                       IS " Y2 "
   HBTEST oValue:z2                       IS " Z2 "
   HBTEST oValue:x3                       IS " X3 "
   HBTEST oValue:y3                       IS " Y3 "
   HBTEST oValue:z3                       IS " Z3 "
   HBTEST oValue:x4                       IS " X4 "
   HBTEST oValue:y4                       IS " Y4 "
   HBTEST oValue:z4                       IS " Z4 "
   HBTEST INSTANCE_DATA( oValue )         IS "[12]: [X1] [Y1] [Z1]  X2   Y2   Z2   X3   Y3   Z3   X4   Y4   Z4 "
   HBTEST __cls_CntClsData( oValue:IVARSCLASS1:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:IVARSCLASS2:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:IVARSCLASS3:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:IVARSCLASS4:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:classH )             IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS1:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS2:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS3:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS4:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:classH )             IS 0

   /* Setting IVARSCLASS2 instance variables... */
   HBTEST oValue:IVARSCLASS2:x2 := "[X2]"    IS "[X2]"
   HBTEST oValue:IVARSCLASS2:y2 := "[Y2]"    IS "[Y2]"
   HBTEST oValue:IVARSCLASS2:z2 := "[Z2]"    IS "[Z2]"

   HBTEST oValue:x1                       IS "[X1]"
   HBTEST oValue:y1                       IS "[Y1]"
   HBTEST oValue:z1                       IS "[Z1]"
   HBTEST oValue:x2                       IS "[X2]"
   HBTEST oValue:y2                       IS "[Y2]"
   HBTEST oValue:z2                       IS "[Z2]"
   HBTEST oValue:x3                       IS " X3 "
   HBTEST oValue:y3                       IS " Y3 "
   HBTEST oValue:z3                       IS " Z3 "
   HBTEST oValue:x4                       IS " X4 "
   HBTEST oValue:y4                       IS " Y4 "
   HBTEST oValue:z4                       IS " Z4 "
   HBTEST INSTANCE_DATA( oValue )         IS "[12]: [X1] [Y1] [Z1] [X2] [Y2] [Z2]  X3   Y3   Z3   X4   Y4   Z4 "
   HBTEST __cls_CntClsData( oValue:IVARSCLASS1:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:IVARSCLASS2:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:IVARSCLASS3:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:IVARSCLASS4:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:classH )             IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS1:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS2:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS3:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS4:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:classH )             IS 0

   /* Setting IVARSCLASS3 instance variables... */
   HBTEST oValue:IVARSCLASS3:x3 := "[X3]"    IS "[X3]"
   HBTEST oValue:IVARSCLASS3:y3 := "[Y3]"    IS "[Y3]"
   HBTEST oValue:IVARSCLASS3:z3 := "[Z3]"    IS "[Z3]"

   HBTEST oValue:x1                       IS "[X1]"
   HBTEST oValue:y1                       IS "[Y1]"
   HBTEST oValue:z1                       IS "[Z1]"
   HBTEST oValue:x2                       IS "[X2]"
   HBTEST oValue:y2                       IS "[Y2]"
   HBTEST oValue:z2                       IS "[Z2]"
   HBTEST oValue:x3                       IS "[X3]"
   HBTEST oValue:y3                       IS "[Y3]"
   HBTEST oValue:z3                       IS "[Z3]"
   HBTEST oValue:x4                       IS " X4 "
   HBTEST oValue:y4                       IS " Y4 "
   HBTEST oValue:z4                       IS " Z4 "
   HBTEST INSTANCE_DATA( oValue )         IS "[12]: [X1] [Y1] [Z1] [X2] [Y2] [Z2] [X3] [Y3] [Z3]  X4   Y4   Z4 "
   HBTEST __cls_CntClsData( oValue:IVARSCLASS1:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:IVARSCLASS2:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:IVARSCLASS3:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:IVARSCLASS4:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:classH )             IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS1:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS2:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS3:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS4:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:classH )             IS 0

   /* Setting IVARSCLASS4 instance variables... */
   HBTEST oValue:IVARSCLASS4:x4 := "[X4]"    IS "[X4]"
   HBTEST oValue:IVARSCLASS4:y4 := "[Y4]"    IS "[Y4]"
   HBTEST oValue:IVARSCLASS4:z4 := "[Z4]"    IS "[Z4]"

   HBTEST oValue:x1                       IS "[X1]"
   HBTEST oValue:y1                       IS "[Y1]"
   HBTEST oValue:z1                       IS "[Z1]"
   HBTEST oValue:x2                       IS "[X2]"
   HBTEST oValue:y2                       IS "[Y2]"
   HBTEST oValue:z2                       IS "[Z2]"
   HBTEST oValue:x3                       IS "[X3]"
   HBTEST oValue:y3                       IS "[Y3]"
   HBTEST oValue:z3                       IS "[Z3]"
   HBTEST oValue:x4                       IS "[X4]"
   HBTEST oValue:y4                       IS "[Y4]"
   HBTEST oValue:z4                       IS "[Z4]"
   HBTEST INSTANCE_DATA( oValue )         IS "[12]: [X1] [Y1] [Z1] [X2] [Y2] [Z2] [X3] [Y3] [Z3] [X4] [Y4] [Z4]"
   HBTEST __cls_CntClsData( oValue:IVARSCLASS1:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:IVARSCLASS2:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:IVARSCLASS3:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:IVARSCLASS4:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:classH )             IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS1:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS2:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS3:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS4:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:classH )             IS 0

   /* Setting IVARSCLASS3:IVARSCLASS1 instance variables... */
   HBTEST oValue:IVARSCLASS3:IVARSCLASS1:x1 := "<X1>"    IS "<X1>"
   HBTEST oValue:IVARSCLASS3:IVARSCLASS1:y1 := "<Y1>"    IS "<Y1>"
   HBTEST oValue:IVARSCLASS3:IVARSCLASS1:z1 := "<Z1>"    IS "<Z1>"

   HBTEST oValue:x1                       IS "<X1>"
   HBTEST oValue:y1                       IS "<Y1>"
   HBTEST oValue:z1                       IS "<Z1>"
   HBTEST oValue:x2                       IS "[X2]"
   HBTEST oValue:y2                       IS "[Y2]"
   HBTEST oValue:z2                       IS "[Z2]"
   HBTEST oValue:x3                       IS "[X3]"
   HBTEST oValue:y3                       IS "[Y3]"
   HBTEST oValue:z3                       IS "[Z3]"
   HBTEST oValue:x4                       IS "[X4]"
   HBTEST oValue:y4                       IS "[Y4]"
   HBTEST oValue:z4                       IS "[Z4]"
   HBTEST INSTANCE_DATA( oValue )         IS "[12]: <X1> <Y1> <Z1> [X2] [Y2] [Z2] [X3] [Y3] [Z3] [X4] [Y4] [Z4]"
   HBTEST __cls_CntClsData( oValue:IVARSCLASS1:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:IVARSCLASS2:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:IVARSCLASS3:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:IVARSCLASS4:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:classH )             IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS1:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS2:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS3:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS4:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:classH )             IS 0

   /* Setting IVARSCLASS3:IVARSCLASS2 instance variables... */
   HBTEST oValue:IVARSCLASS3:IVARSCLASS2:x2 := "<X2>"    IS "<X2>"
   HBTEST oValue:IVARSCLASS3:IVARSCLASS2:y2 := "<Y2>"    IS "<Y2>"
   HBTEST oValue:IVARSCLASS3:IVARSCLASS2:z2 := "<Z2>"    IS "<Z2>"

   HBTEST oValue:x1                       IS "<X1>"
   HBTEST oValue:y1                       IS "<Y1>"
   HBTEST oValue:z1                       IS "<Z1>"
   HBTEST oValue:x2                       IS "<X2>"
   HBTEST oValue:y2                       IS "<Y2>"
   HBTEST oValue:z2                       IS "<Z2>"
   HBTEST oValue:x3                       IS "[X3]"
   HBTEST oValue:y3                       IS "[Y3]"
   HBTEST oValue:z3                       IS "[Z3]"
   HBTEST oValue:x4                       IS "[X4]"
   HBTEST oValue:y4                       IS "[Y4]"
   HBTEST oValue:z4                       IS "[Z4]"
   HBTEST INSTANCE_DATA( oValue )         IS "[12]: <X1> <Y1> <Z1> <X2> <Y2> <Z2> [X3] [Y3] [Z3] [X4] [Y4] [Z4]"
   HBTEST __cls_CntClsData( oValue:IVARSCLASS1:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:IVARSCLASS2:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:IVARSCLASS3:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:IVARSCLASS4:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:classH )             IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS1:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS2:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS3:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS4:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:classH )             IS 0

   /* Setting SUPER instance variables... */
   HBTEST oValue:super:x1 := "{X1}"       IS "{X1}"
   HBTEST oValue:super:y1 := "{Y1}"       IS "{Y1}"
   HBTEST oValue:super:z1 := "{Z1}"       IS "{Z1}"
   HBTEST oValue:super:x2 := "{X2}"       IS "{X2}"
   HBTEST oValue:super:y2 := "{Y2}"       IS "{Y2}"
   HBTEST oValue:super:z2 := "{Z2}"       IS "{Z2}"
   HBTEST oValue:super:x3 := "{X3}"       IS "{X3}"
   HBTEST oValue:super:y3 := "{Y3}"       IS "{Y3}"
   HBTEST oValue:super:z3 := "{Z3}"       IS "{Z3}"

   HBTEST oValue:x1                       IS "{X1}"
   HBTEST oValue:y1                       IS "{Y1}"
   HBTEST oValue:z1                       IS "{Z1}"
   HBTEST oValue:x2                       IS "{X2}"
   HBTEST oValue:y2                       IS "{Y2}"
   HBTEST oValue:z2                       IS "{Z2}"
   HBTEST oValue:x3                       IS "{X3}"
   HBTEST oValue:y3                       IS "{Y3}"
   HBTEST oValue:z3                       IS "{Z3}"
   HBTEST oValue:x4                       IS "[X4]"
   HBTEST oValue:y4                       IS "[Y4]"
   HBTEST oValue:z4                       IS "[Z4]"
   HBTEST INSTANCE_DATA( oValue )         IS "[12]: {X1} {Y1} {Z1} {X2} {Y2} {Z2} {X3} {Y3} {Z3} [X4] [Y4] [Z4]"
   HBTEST __cls_CntClsData( oValue:IVARSCLASS1:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:IVARSCLASS2:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:IVARSCLASS3:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:IVARSCLASS4:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:classH )             IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS1:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS2:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS3:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:IVARSCLASS4:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:classH )             IS 0


   /* Test class variables allocating and casting */

   oValue := CVARSCLASS4():new()

   HBTEST oValue:x1                       IS "(x1)"
   HBTEST oValue:y1                       IS "(y1)"
   HBTEST oValue:z1                       IS "(z1)"
   HBTEST oValue:x2                       IS "(x2)"
   HBTEST oValue:y2                       IS "(y2)"
   HBTEST oValue:z2                       IS "(z2)"
   HBTEST oValue:x3                       IS "(x3)"
   HBTEST oValue:y3                       IS "(y3)"
   HBTEST oValue:z3                       IS "(z3)"
   HBTEST oValue:x4                       IS "(x4)"
   HBTEST oValue:y4                       IS "(y4)"
   HBTEST oValue:z4                       IS "(z4)"
   HBTEST oValue:CVARSCLASS1:x1           IS "(x1)"
   HBTEST oValue:CVARSCLASS1:y1           IS "(y1)"
   HBTEST oValue:CVARSCLASS1:z1           IS "(z1)"
   HBTEST oValue:CVARSCLASS2:x1           IS "(x1)"
   HBTEST oValue:CVARSCLASS2:y1           IS "(y1)"
   HBTEST oValue:CVARSCLASS2:z1           IS "(z1)"
   HBTEST oValue:CVARSCLASS2:x2           IS "(x2)"
   HBTEST oValue:CVARSCLASS2:y2           IS "(y2)"
   HBTEST oValue:CVARSCLASS2:z2           IS "(z2)"
   HBTEST oValue:CVARSCLASS3:x1           IS "(x1)"
   HBTEST oValue:CVARSCLASS3:y1           IS "(y1)"
   HBTEST oValue:CVARSCLASS3:z1           IS "(z1)"
   HBTEST oValue:CVARSCLASS3:x2           IS "(x2)"
   HBTEST oValue:CVARSCLASS3:y2           IS "(y2)"
   HBTEST oValue:CVARSCLASS3:z2           IS "(z2)"
   HBTEST oValue:CVARSCLASS3:x3           IS "(x3)"
   HBTEST oValue:CVARSCLASS3:y3           IS "(y3)"
   HBTEST oValue:CVARSCLASS3:z3           IS "(z3)"
   HBTEST oValue:CVARSCLASS4:x1           IS "(x1)"
   HBTEST oValue:CVARSCLASS4:y1           IS "(y1)"
   HBTEST oValue:CVARSCLASS4:z1           IS "(z1)"
   HBTEST oValue:CVARSCLASS4:x2           IS "(x2)"
   HBTEST oValue:CVARSCLASS4:y2           IS "(y2)"
   HBTEST oValue:CVARSCLASS4:z2           IS "(z2)"
   HBTEST oValue:CVARSCLASS4:x3           IS "(x3)"
   HBTEST oValue:CVARSCLASS4:y3           IS "(y3)"
   HBTEST oValue:CVARSCLASS4:z3           IS "(z3)"
   HBTEST oValue:CVARSCLASS4:x4           IS "(x4)"
   HBTEST oValue:CVARSCLASS4:y4           IS "(y4)"
   HBTEST oValue:CVARSCLASS4:z4           IS "(z4)"
   HBTEST INSTANCE_DATA( oValue )         IS "[0]:"
   HBTEST __cls_CntClsData( oValue:CVARSCLASS1:classH ) IS 3
   HBTEST __cls_CntClsData( oValue:CVARSCLASS2:classH ) IS 6
   HBTEST __cls_CntClsData( oValue:CVARSCLASS3:classH ) IS 9
   HBTEST __cls_CntClsData( oValue:CVARSCLASS4:classH ) IS 12
   HBTEST __cls_CntClsData( oValue:classH )             IS 12
   HBTEST __cls_CntShrData( oValue:CVARSCLASS1:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:CVARSCLASS2:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:CVARSCLASS3:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:CVARSCLASS4:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:classH )             IS 0

   /* simple assignment... */
   HBTEST oValue:x1 := " X1 "             IS " X1 "
   HBTEST oValue:y1 := " Y1 "             IS " Y1 "
   HBTEST oValue:z1 := " Z1 "             IS " Z1 "
   HBTEST oValue:x2 := " X2 "             IS " X2 "
   HBTEST oValue:y2 := " Y2 "             IS " Y2 "
   HBTEST oValue:z2 := " Z2 "             IS " Z2 "
   HBTEST oValue:x3 := " X3 "             IS " X3 "
   HBTEST oValue:y3 := " Y3 "             IS " Y3 "
   HBTEST oValue:z3 := " Z3 "             IS " Z3 "
   HBTEST oValue:x4 := " X4 "             IS " X4 "
   HBTEST oValue:y4 := " Y4 "             IS " Y4 "
   HBTEST oValue:z4 := " Z4 "             IS " Z4 "

   HBTEST oValue:x1                       IS " X1 "
   HBTEST oValue:y1                       IS " Y1 "
   HBTEST oValue:z1                       IS " Z1 "
   HBTEST oValue:x2                       IS " X2 "
   HBTEST oValue:y2                       IS " Y2 "
   HBTEST oValue:z2                       IS " Z2 "
   HBTEST oValue:x3                       IS " X3 "
   HBTEST oValue:y3                       IS " Y3 "
   HBTEST oValue:z3                       IS " Z3 "
   HBTEST oValue:x4                       IS " X4 "
   HBTEST oValue:y4                       IS " Y4 "
   HBTEST oValue:z4                       IS " Z4 "
   HBTEST oValue:CVARSCLASS1:x1           IS "(x1)"
   HBTEST oValue:CVARSCLASS1:y1           IS "(y1)"
   HBTEST oValue:CVARSCLASS1:z1           IS "(z1)"
   HBTEST oValue:CVARSCLASS2:x1           IS "(x1)"
   HBTEST oValue:CVARSCLASS2:y1           IS "(y1)"
   HBTEST oValue:CVARSCLASS2:z1           IS "(z1)"
   HBTEST oValue:CVARSCLASS2:x2           IS "(x2)"
   HBTEST oValue:CVARSCLASS2:y2           IS "(y2)"
   HBTEST oValue:CVARSCLASS2:z2           IS "(z2)"
   HBTEST oValue:CVARSCLASS3:x1           IS "(x1)"
   HBTEST oValue:CVARSCLASS3:y1           IS "(y1)"
   HBTEST oValue:CVARSCLASS3:z1           IS "(z1)"
   HBTEST oValue:CVARSCLASS3:x2           IS "(x2)"
   HBTEST oValue:CVARSCLASS3:y2           IS "(y2)"
   HBTEST oValue:CVARSCLASS3:z2           IS "(z2)"
   HBTEST oValue:CVARSCLASS3:x3           IS "(x3)"
   HBTEST oValue:CVARSCLASS3:y3           IS "(y3)"
   HBTEST oValue:CVARSCLASS3:z3           IS "(z3)"
   HBTEST oValue:CVARSCLASS4:x1           IS " X1 "
   HBTEST oValue:CVARSCLASS4:y1           IS " Y1 "
   HBTEST oValue:CVARSCLASS4:z1           IS " Z1 "
   HBTEST oValue:CVARSCLASS4:x2           IS " X2 "
   HBTEST oValue:CVARSCLASS4:y2           IS " Y2 "
   HBTEST oValue:CVARSCLASS4:z2           IS " Z2 "
   HBTEST oValue:CVARSCLASS4:x3           IS " X3 "
   HBTEST oValue:CVARSCLASS4:y3           IS " Y3 "
   HBTEST oValue:CVARSCLASS4:z3           IS " Z3 "
   HBTEST oValue:CVARSCLASS4:x4           IS " X4 "
   HBTEST oValue:CVARSCLASS4:y4           IS " Y4 "
   HBTEST oValue:CVARSCLASS4:z4           IS " Z4 "
   HBTEST __cls_CntClsData( oValue:CVARSCLASS1:classH ) IS 3
   HBTEST __cls_CntClsData( oValue:CVARSCLASS2:classH ) IS 6
   HBTEST __cls_CntClsData( oValue:CVARSCLASS3:classH ) IS 9
   HBTEST __cls_CntClsData( oValue:CVARSCLASS4:classH ) IS 12
   HBTEST __cls_CntClsData( oValue:classH )             IS 12
   HBTEST __cls_CntShrData( oValue:CVARSCLASS1:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:CVARSCLASS2:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:CVARSCLASS3:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:CVARSCLASS4:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:classH )             IS 0

   /* Setting CVARSCLASS1 class variables... */
   HBTEST oValue:CVARSCLASS1:x1 := "[X1]"    IS "[X1]"
   HBTEST oValue:CVARSCLASS1:y1 := "[Y1]"    IS "[Y1]"
   HBTEST oValue:CVARSCLASS1:z1 := "[Z1]"    IS "[Z1]"

   HBTEST oValue:x1                       IS " X1 "
   HBTEST oValue:y1                       IS " Y1 "
   HBTEST oValue:z1                       IS " Z1 "
   HBTEST oValue:x2                       IS " X2 "
   HBTEST oValue:y2                       IS " Y2 "
   HBTEST oValue:z2                       IS " Z2 "
   HBTEST oValue:x3                       IS " X3 "
   HBTEST oValue:y3                       IS " Y3 "
   HBTEST oValue:z3                       IS " Z3 "
   HBTEST oValue:x4                       IS " X4 "
   HBTEST oValue:y4                       IS " Y4 "
   HBTEST oValue:z4                       IS " Z4 "
   HBTEST oValue:CVARSCLASS1:x1           IS "[X1]"
   HBTEST oValue:CVARSCLASS1:y1           IS "[Y1]"
   HBTEST oValue:CVARSCLASS1:z1           IS "[Z1]"
   HBTEST oValue:CVARSCLASS2:x1           IS "(x1)"
   HBTEST oValue:CVARSCLASS2:y1           IS "(y1)"
   HBTEST oValue:CVARSCLASS2:z1           IS "(z1)"
   HBTEST oValue:CVARSCLASS2:x2           IS "(x2)"
   HBTEST oValue:CVARSCLASS2:y2           IS "(y2)"
   HBTEST oValue:CVARSCLASS2:z2           IS "(z2)"
   HBTEST oValue:CVARSCLASS3:x1           IS "(x1)"
   HBTEST oValue:CVARSCLASS3:y1           IS "(y1)"
   HBTEST oValue:CVARSCLASS3:z1           IS "(z1)"
   HBTEST oValue:CVARSCLASS3:x2           IS "(x2)"
   HBTEST oValue:CVARSCLASS3:y2           IS "(y2)"
   HBTEST oValue:CVARSCLASS3:z2           IS "(z2)"
   HBTEST oValue:CVARSCLASS3:x3           IS "(x3)"
   HBTEST oValue:CVARSCLASS3:y3           IS "(y3)"
   HBTEST oValue:CVARSCLASS3:z3           IS "(z3)"
   HBTEST oValue:CVARSCLASS4:x1           IS " X1 "
   HBTEST oValue:CVARSCLASS4:y1           IS " Y1 "
   HBTEST oValue:CVARSCLASS4:z1           IS " Z1 "
   HBTEST oValue:CVARSCLASS4:x2           IS " X2 "
   HBTEST oValue:CVARSCLASS4:y2           IS " Y2 "
   HBTEST oValue:CVARSCLASS4:z2           IS " Z2 "
   HBTEST oValue:CVARSCLASS4:x3           IS " X3 "
   HBTEST oValue:CVARSCLASS4:y3           IS " Y3 "
   HBTEST oValue:CVARSCLASS4:z3           IS " Z3 "
   HBTEST oValue:CVARSCLASS4:x4           IS " X4 "
   HBTEST oValue:CVARSCLASS4:y4           IS " Y4 "
   HBTEST oValue:CVARSCLASS4:z4           IS " Z4 "
   HBTEST INSTANCE_DATA( oValue )         IS "[0]:"
   HBTEST __cls_CntClsData( oValue:CVARSCLASS1:classH ) IS 3
   HBTEST __cls_CntClsData( oValue:CVARSCLASS2:classH ) IS 6
   HBTEST __cls_CntClsData( oValue:CVARSCLASS3:classH ) IS 9
   HBTEST __cls_CntClsData( oValue:CVARSCLASS4:classH ) IS 12
   HBTEST __cls_CntClsData( oValue:classH )             IS 12
   HBTEST __cls_CntShrData( oValue:CVARSCLASS1:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:CVARSCLASS2:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:CVARSCLASS3:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:CVARSCLASS4:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:classH )             IS 0

   /* Setting CVARSCLASS2 class variables... */
   HBTEST oValue:CVARSCLASS2:x1 := "{X1}"    IS "{X1}"
   HBTEST oValue:CVARSCLASS2:y1 := "{Y1}"    IS "{Y1}"
   HBTEST oValue:CVARSCLASS2:z1 := "{Z1}"    IS "{Z1}"
   HBTEST oValue:CVARSCLASS2:x2 := "{X2}"    IS "{X2}"
   HBTEST oValue:CVARSCLASS2:y2 := "{Y2}"    IS "{Y2}"
   HBTEST oValue:CVARSCLASS2:z2 := "{Z2}"    IS "{Z2}"

   HBTEST oValue:x1                       IS " X1 "
   HBTEST oValue:y1                       IS " Y1 "
   HBTEST oValue:z1                       IS " Z1 "
   HBTEST oValue:x2                       IS " X2 "
   HBTEST oValue:y2                       IS " Y2 "
   HBTEST oValue:z2                       IS " Z2 "
   HBTEST oValue:x3                       IS " X3 "
   HBTEST oValue:y3                       IS " Y3 "
   HBTEST oValue:z3                       IS " Z3 "
   HBTEST oValue:x4                       IS " X4 "
   HBTEST oValue:y4                       IS " Y4 "
   HBTEST oValue:z4                       IS " Z4 "
   HBTEST oValue:CVARSCLASS1:x1           IS "[X1]"
   HBTEST oValue:CVARSCLASS1:y1           IS "[Y1]"
   HBTEST oValue:CVARSCLASS1:z1           IS "[Z1]"
   HBTEST oValue:CVARSCLASS2:x1           IS "{X1}"
   HBTEST oValue:CVARSCLASS2:y1           IS "{Y1}"
   HBTEST oValue:CVARSCLASS2:z1           IS "{Z1}"
   HBTEST oValue:CVARSCLASS2:x2           IS "{X2}"
   HBTEST oValue:CVARSCLASS2:y2           IS "{Y2}"
   HBTEST oValue:CVARSCLASS2:z2           IS "{Z2}"
   HBTEST oValue:CVARSCLASS3:x1           IS "(x1)"
   HBTEST oValue:CVARSCLASS3:y1           IS "(y1)"
   HBTEST oValue:CVARSCLASS3:z1           IS "(z1)"
   HBTEST oValue:CVARSCLASS3:x2           IS "(x2)"
   HBTEST oValue:CVARSCLASS3:y2           IS "(y2)"
   HBTEST oValue:CVARSCLASS3:z2           IS "(z2)"
   HBTEST oValue:CVARSCLASS3:x3           IS "(x3)"
   HBTEST oValue:CVARSCLASS3:y3           IS "(y3)"
   HBTEST oValue:CVARSCLASS3:z3           IS "(z3)"
   HBTEST oValue:CVARSCLASS4:x1           IS " X1 "
   HBTEST oValue:CVARSCLASS4:y1           IS " Y1 "
   HBTEST oValue:CVARSCLASS4:z1           IS " Z1 "
   HBTEST oValue:CVARSCLASS4:x2           IS " X2 "
   HBTEST oValue:CVARSCLASS4:y2           IS " Y2 "
   HBTEST oValue:CVARSCLASS4:z2           IS " Z2 "
   HBTEST oValue:CVARSCLASS4:x3           IS " X3 "
   HBTEST oValue:CVARSCLASS4:y3           IS " Y3 "
   HBTEST oValue:CVARSCLASS4:z3           IS " Z3 "
   HBTEST oValue:CVARSCLASS4:x4           IS " X4 "
   HBTEST oValue:CVARSCLASS4:y4           IS " Y4 "
   HBTEST oValue:CVARSCLASS4:z4           IS " Z4 "
   HBTEST INSTANCE_DATA( oValue )         IS "[0]:"
   HBTEST __cls_CntClsData( oValue:CVARSCLASS1:classH ) IS 3
   HBTEST __cls_CntClsData( oValue:CVARSCLASS2:classH ) IS 6
   HBTEST __cls_CntClsData( oValue:CVARSCLASS3:classH ) IS 9
   HBTEST __cls_CntClsData( oValue:CVARSCLASS4:classH ) IS 12
   HBTEST __cls_CntClsData( oValue:classH )             IS 12
   HBTEST __cls_CntShrData( oValue:CVARSCLASS1:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:CVARSCLASS2:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:CVARSCLASS3:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:CVARSCLASS4:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:classH )             IS 0

   /* Setting CVARSCLASS3 class variables... */
   HBTEST oValue:CVARSCLASS3:x1 := "<X1>"    IS "<X1>"
   HBTEST oValue:CVARSCLASS3:y1 := "<Y1>"    IS "<Y1>"
   HBTEST oValue:CVARSCLASS3:z1 := "<Z1>"    IS "<Z1>"
   HBTEST oValue:CVARSCLASS3:x2 := "<X2>"    IS "<X2>"
   HBTEST oValue:CVARSCLASS3:y2 := "<Y2>"    IS "<Y2>"
   HBTEST oValue:CVARSCLASS3:z2 := "<Z2>"    IS "<Z2>"
   HBTEST oValue:CVARSCLASS3:x3 := "<X3>"    IS "<X3>"
   HBTEST oValue:CVARSCLASS3:y3 := "<Y3>"    IS "<Y3>"
   HBTEST oValue:CVARSCLASS3:z3 := "<Z3>"    IS "<Z3>"

   HBTEST oValue:x1                       IS " X1 "
   HBTEST oValue:y1                       IS " Y1 "
   HBTEST oValue:z1                       IS " Z1 "
   HBTEST oValue:x2                       IS " X2 "
   HBTEST oValue:y2                       IS " Y2 "
   HBTEST oValue:z2                       IS " Z2 "
   HBTEST oValue:x3                       IS " X3 "
   HBTEST oValue:y3                       IS " Y3 "
   HBTEST oValue:z3                       IS " Z3 "
   HBTEST oValue:x4                       IS " X4 "
   HBTEST oValue:y4                       IS " Y4 "
   HBTEST oValue:z4                       IS " Z4 "
   HBTEST oValue:CVARSCLASS1:x1           IS "[X1]"
   HBTEST oValue:CVARSCLASS1:y1           IS "[Y1]"
   HBTEST oValue:CVARSCLASS1:z1           IS "[Z1]"
   HBTEST oValue:CVARSCLASS2:x1           IS "{X1}"
   HBTEST oValue:CVARSCLASS2:y1           IS "{Y1}"
   HBTEST oValue:CVARSCLASS2:z1           IS "{Z1}"
   HBTEST oValue:CVARSCLASS2:x2           IS "{X2}"
   HBTEST oValue:CVARSCLASS2:y2           IS "{Y2}"
   HBTEST oValue:CVARSCLASS2:z2           IS "{Z2}"
   HBTEST oValue:CVARSCLASS3:x1           IS "<X1>"
   HBTEST oValue:CVARSCLASS3:y1           IS "<Y1>"
   HBTEST oValue:CVARSCLASS3:z1           IS "<Z1>"
   HBTEST oValue:CVARSCLASS3:x2           IS "<X2>"
   HBTEST oValue:CVARSCLASS3:y2           IS "<Y2>"
   HBTEST oValue:CVARSCLASS3:z2           IS "<Z2>"
   HBTEST oValue:CVARSCLASS3:x3           IS "<X3>"
   HBTEST oValue:CVARSCLASS3:y3           IS "<Y3>"
   HBTEST oValue:CVARSCLASS3:z3           IS "<Z3>"
   HBTEST oValue:CVARSCLASS4:x1           IS " X1 "
   HBTEST oValue:CVARSCLASS4:y1           IS " Y1 "
   HBTEST oValue:CVARSCLASS4:z1           IS " Z1 "
   HBTEST oValue:CVARSCLASS4:x2           IS " X2 "
   HBTEST oValue:CVARSCLASS4:y2           IS " Y2 "
   HBTEST oValue:CVARSCLASS4:z2           IS " Z2 "
   HBTEST oValue:CVARSCLASS4:x3           IS " X3 "
   HBTEST oValue:CVARSCLASS4:y3           IS " Y3 "
   HBTEST oValue:CVARSCLASS4:z3           IS " Z3 "
   HBTEST oValue:CVARSCLASS4:x4           IS " X4 "
   HBTEST oValue:CVARSCLASS4:y4           IS " Y4 "
   HBTEST oValue:CVARSCLASS4:z4           IS " Z4 "
   HBTEST INSTANCE_DATA( oValue )         IS "[0]:"
   HBTEST __cls_CntClsData( oValue:CVARSCLASS1:classH ) IS 3
   HBTEST __cls_CntClsData( oValue:CVARSCLASS2:classH ) IS 6
   HBTEST __cls_CntClsData( oValue:CVARSCLASS3:classH ) IS 9
   HBTEST __cls_CntClsData( oValue:CVARSCLASS4:classH ) IS 12
   HBTEST __cls_CntClsData( oValue:classH )             IS 12
   HBTEST __cls_CntShrData( oValue:CVARSCLASS1:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:CVARSCLASS2:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:CVARSCLASS3:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:CVARSCLASS4:classH ) IS 0
   HBTEST __cls_CntShrData( oValue:classH )             IS 0


   /* Test shared class variables allocating and casting */

   oValue := SVARSCLASS4():new()

   HBTEST oValue:x1                       IS "(x1)"
   HBTEST oValue:y1                       IS "(y1)"
   HBTEST oValue:z1                       IS "(z1)"
   HBTEST oValue:x2                       IS "(x2)"
   HBTEST oValue:y2                       IS "(y2)"
   HBTEST oValue:z2                       IS "(z2)"
   HBTEST oValue:x3                       IS "(x3)"
   HBTEST oValue:y3                       IS "(y3)"
   HBTEST oValue:z3                       IS "(z3)"
   HBTEST oValue:x4                       IS "(x4)"
   HBTEST oValue:y4                       IS "(y4)"
   HBTEST oValue:z4                       IS "(z4)"
   HBTEST oValue:SVARSCLASS1:x1           IS "(x1)"
   HBTEST oValue:SVARSCLASS1:y1           IS "(y1)"
   HBTEST oValue:SVARSCLASS1:z1           IS "(z1)"
   HBTEST oValue:SVARSCLASS2:x1           IS "(x1)"
   HBTEST oValue:SVARSCLASS2:y1           IS "(y1)"
   HBTEST oValue:SVARSCLASS2:z1           IS "(z1)"
   HBTEST oValue:SVARSCLASS2:x2           IS "(x2)"
   HBTEST oValue:SVARSCLASS2:y2           IS "(y2)"
   HBTEST oValue:SVARSCLASS2:z2           IS "(z2)"
   HBTEST oValue:SVARSCLASS3:x1           IS "(x1)"
   HBTEST oValue:SVARSCLASS3:y1           IS "(y1)"
   HBTEST oValue:SVARSCLASS3:z1           IS "(z1)"
   HBTEST oValue:SVARSCLASS3:x2           IS "(x2)"
   HBTEST oValue:SVARSCLASS3:y2           IS "(y2)"
   HBTEST oValue:SVARSCLASS3:z2           IS "(z2)"
   HBTEST oValue:SVARSCLASS3:x3           IS "(x3)"
   HBTEST oValue:SVARSCLASS3:y3           IS "(y3)"
   HBTEST oValue:SVARSCLASS3:z3           IS "(z3)"
   HBTEST oValue:SVARSCLASS4:x1           IS "(x1)"
   HBTEST oValue:SVARSCLASS4:y1           IS "(y1)"
   HBTEST oValue:SVARSCLASS4:z1           IS "(z1)"
   HBTEST oValue:SVARSCLASS4:x2           IS "(x2)"
   HBTEST oValue:SVARSCLASS4:y2           IS "(y2)"
   HBTEST oValue:SVARSCLASS4:z2           IS "(z2)"
   HBTEST oValue:SVARSCLASS4:x3           IS "(x3)"
   HBTEST oValue:SVARSCLASS4:y3           IS "(y3)"
   HBTEST oValue:SVARSCLASS4:z3           IS "(z3)"
   HBTEST oValue:SVARSCLASS4:x4           IS "(x4)"
   HBTEST oValue:SVARSCLASS4:y4           IS "(y4)"
   HBTEST oValue:SVARSCLASS4:z4           IS "(z4)"
   HBTEST INSTANCE_DATA( oValue )         IS "[0]:"
   HBTEST __cls_CntClsData( oValue:SVARSCLASS1:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:SVARSCLASS2:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:SVARSCLASS3:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:SVARSCLASS4:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:classH )             IS 0
   HBTEST __cls_CntShrData( oValue:SVARSCLASS1:classH ) IS 3
   HBTEST __cls_CntShrData( oValue:SVARSCLASS2:classH ) IS 3
   HBTEST __cls_CntShrData( oValue:SVARSCLASS3:classH ) IS 3
   HBTEST __cls_CntShrData( oValue:SVARSCLASS4:classH ) IS 3
   HBTEST __cls_CntShrData( oValue:classH )             IS 3

   /* simple assignment... */
   HBTEST oValue:x1 := " X1 "             IS " X1 "
   HBTEST oValue:y1 := " Y1 "             IS " Y1 "
   HBTEST oValue:z1 := " Z1 "             IS " Z1 "
   HBTEST oValue:x2 := " X2 "             IS " X2 "
   HBTEST oValue:y2 := " Y2 "             IS " Y2 "
   HBTEST oValue:z2 := " Z2 "             IS " Z2 "
   HBTEST oValue:x3 := " X3 "             IS " X3 "
   HBTEST oValue:y3 := " Y3 "             IS " Y3 "
   HBTEST oValue:z3 := " Z3 "             IS " Z3 "
   HBTEST oValue:x4 := " X4 "             IS " X4 "
   HBTEST oValue:y4 := " Y4 "             IS " Y4 "
   HBTEST oValue:z4 := " Z4 "             IS " Z4 "

   HBTEST oValue:x1                       IS " X1 "
   HBTEST oValue:y1                       IS " Y1 "
   HBTEST oValue:z1                       IS " Z1 "
   HBTEST oValue:x2                       IS " X2 "
   HBTEST oValue:y2                       IS " Y2 "
   HBTEST oValue:z2                       IS " Z2 "
   HBTEST oValue:x3                       IS " X3 "
   HBTEST oValue:y3                       IS " Y3 "
   HBTEST oValue:z3                       IS " Z3 "
   HBTEST oValue:x4                       IS " X4 "
   HBTEST oValue:y4                       IS " Y4 "
   HBTEST oValue:z4                       IS " Z4 "
   HBTEST oValue:SVARSCLASS1:x1           IS " X1 "
   HBTEST oValue:SVARSCLASS1:y1           IS " Y1 "
   HBTEST oValue:SVARSCLASS1:z1           IS " Z1 "
   HBTEST oValue:SVARSCLASS2:x1           IS " X1 "
   HBTEST oValue:SVARSCLASS2:y1           IS " Y1 "
   HBTEST oValue:SVARSCLASS2:z1           IS " Z1 "
   HBTEST oValue:SVARSCLASS2:x2           IS " X2 "
   HBTEST oValue:SVARSCLASS2:y2           IS " Y2 "
   HBTEST oValue:SVARSCLASS2:z2           IS " Z2 "
   HBTEST oValue:SVARSCLASS3:x1           IS " X1 "
   HBTEST oValue:SVARSCLASS3:y1           IS " Y1 "
   HBTEST oValue:SVARSCLASS3:z1           IS " Z1 "
   HBTEST oValue:SVARSCLASS3:x2           IS " X2 "
   HBTEST oValue:SVARSCLASS3:y2           IS " Y2 "
   HBTEST oValue:SVARSCLASS3:z2           IS " Z2 "
   HBTEST oValue:SVARSCLASS3:x3           IS " X3 "
   HBTEST oValue:SVARSCLASS3:y3           IS " Y3 "
   HBTEST oValue:SVARSCLASS3:z3           IS " Z3 "
   HBTEST oValue:SVARSCLASS4:x1           IS " X1 "
   HBTEST oValue:SVARSCLASS4:y1           IS " Y1 "
   HBTEST oValue:SVARSCLASS4:z1           IS " Z1 "
   HBTEST oValue:SVARSCLASS4:x2           IS " X2 "
   HBTEST oValue:SVARSCLASS4:y2           IS " Y2 "
   HBTEST oValue:SVARSCLASS4:z2           IS " Z2 "
   HBTEST oValue:SVARSCLASS4:x3           IS " X3 "
   HBTEST oValue:SVARSCLASS4:y3           IS " Y3 "
   HBTEST oValue:SVARSCLASS4:z3           IS " Z3 "
   HBTEST oValue:SVARSCLASS4:x4           IS " X4 "
   HBTEST oValue:SVARSCLASS4:y4           IS " Y4 "
   HBTEST oValue:SVARSCLASS4:z4           IS " Z4 "
   HBTEST __cls_CntClsData( oValue:SVARSCLASS1:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:SVARSCLASS2:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:SVARSCLASS3:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:SVARSCLASS4:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:classH )             IS 0
   HBTEST __cls_CntShrData( oValue:SVARSCLASS1:classH ) IS 3
   HBTEST __cls_CntShrData( oValue:SVARSCLASS2:classH ) IS 3
   HBTEST __cls_CntShrData( oValue:SVARSCLASS3:classH ) IS 3
   HBTEST __cls_CntShrData( oValue:SVARSCLASS4:classH ) IS 3
   HBTEST __cls_CntShrData( oValue:classH )             IS 3
   HBTEST INSTANCE_DATA( oValue )         IS "[0]:"

   /* Setting SVARSCLASS1 class variables... */
   HBTEST oValue:SVARSCLASS1:x1 := "[X1]"    IS "[X1]"
   HBTEST oValue:SVARSCLASS1:y1 := "[Y1]"    IS "[Y1]"
   HBTEST oValue:SVARSCLASS1:z1 := "[Z1]"    IS "[Z1]"

   HBTEST oValue:x1                       IS "[X1]"
   HBTEST oValue:y1                       IS "[Y1]"
   HBTEST oValue:z1                       IS "[Z1]"
   HBTEST oValue:x2                       IS " X2 "
   HBTEST oValue:y2                       IS " Y2 "
   HBTEST oValue:z2                       IS " Z2 "
   HBTEST oValue:x3                       IS " X3 "
   HBTEST oValue:y3                       IS " Y3 "
   HBTEST oValue:z3                       IS " Z3 "
   HBTEST oValue:x4                       IS " X4 "
   HBTEST oValue:y4                       IS " Y4 "
   HBTEST oValue:z4                       IS " Z4 "
   HBTEST oValue:SVARSCLASS1:x1           IS "[X1]"
   HBTEST oValue:SVARSCLASS1:y1           IS "[Y1]"
   HBTEST oValue:SVARSCLASS1:z1           IS "[Z1]"
   HBTEST oValue:SVARSCLASS2:x1           IS "[X1]"
   HBTEST oValue:SVARSCLASS2:y1           IS "[Y1]"
   HBTEST oValue:SVARSCLASS2:z1           IS "[Z1]"
   HBTEST oValue:SVARSCLASS2:x2           IS " X2 "
   HBTEST oValue:SVARSCLASS2:y2           IS " Y2 "
   HBTEST oValue:SVARSCLASS2:z2           IS " Z2 "
   HBTEST oValue:SVARSCLASS3:x1           IS "[X1]"
   HBTEST oValue:SVARSCLASS3:y1           IS "[Y1]"
   HBTEST oValue:SVARSCLASS3:z1           IS "[Z1]"
   HBTEST oValue:SVARSCLASS3:x2           IS " X2 "
   HBTEST oValue:SVARSCLASS3:y2           IS " Y2 "
   HBTEST oValue:SVARSCLASS3:z2           IS " Z2 "
   HBTEST oValue:SVARSCLASS3:x3           IS " X3 "
   HBTEST oValue:SVARSCLASS3:y3           IS " Y3 "
   HBTEST oValue:SVARSCLASS3:z3           IS " Z3 "
   HBTEST oValue:SVARSCLASS4:x1           IS "[X1]"
   HBTEST oValue:SVARSCLASS4:y1           IS "[Y1]"
   HBTEST oValue:SVARSCLASS4:z1           IS "[Z1]"
   HBTEST oValue:SVARSCLASS4:x2           IS " X2 "
   HBTEST oValue:SVARSCLASS4:y2           IS " Y2 "
   HBTEST oValue:SVARSCLASS4:z2           IS " Z2 "
   HBTEST oValue:SVARSCLASS4:x3           IS " X3 "
   HBTEST oValue:SVARSCLASS4:y3           IS " Y3 "
   HBTEST oValue:SVARSCLASS4:z3           IS " Z3 "
   HBTEST oValue:SVARSCLASS4:x4           IS " X4 "
   HBTEST oValue:SVARSCLASS4:y4           IS " Y4 "
   HBTEST oValue:SVARSCLASS4:z4           IS " Z4 "
   HBTEST __cls_CntClsData( oValue:SVARSCLASS1:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:SVARSCLASS2:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:SVARSCLASS3:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:SVARSCLASS4:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:classH )             IS 0
   HBTEST __cls_CntShrData( oValue:SVARSCLASS1:classH ) IS 3
   HBTEST __cls_CntShrData( oValue:SVARSCLASS2:classH ) IS 3
   HBTEST __cls_CntShrData( oValue:SVARSCLASS3:classH ) IS 3
   HBTEST __cls_CntShrData( oValue:SVARSCLASS4:classH ) IS 3
   HBTEST __cls_CntShrData( oValue:classH )             IS 3
   HBTEST INSTANCE_DATA( oValue )         IS "[0]:"

   /* Setting SVARSCLASS2 class variables... */
   HBTEST oValue:SVARSCLASS2:x1 := "{X1}"    IS "{X1}"
   HBTEST oValue:SVARSCLASS2:y1 := "{Y1}"    IS "{Y1}"
   HBTEST oValue:SVARSCLASS2:z1 := "{Z1}"    IS "{Z1}"
   HBTEST oValue:SVARSCLASS2:x2 := "{X2}"    IS "{X2}"
   HBTEST oValue:SVARSCLASS2:y2 := "{Y2}"    IS "{Y2}"
   HBTEST oValue:SVARSCLASS2:z2 := "{Z2}"    IS "{Z2}"

   HBTEST oValue:x1                       IS "{X1}"
   HBTEST oValue:y1                       IS "{Y1}"
   HBTEST oValue:z1                       IS "{Z1}"
   HBTEST oValue:x2                       IS "{X2}"
   HBTEST oValue:y2                       IS "{Y2}"
   HBTEST oValue:z2                       IS "{Z2}"
   HBTEST oValue:x3                       IS " X3 "
   HBTEST oValue:y3                       IS " Y3 "
   HBTEST oValue:z3                       IS " Z3 "
   HBTEST oValue:x4                       IS " X4 "
   HBTEST oValue:y4                       IS " Y4 "
   HBTEST oValue:z4                       IS " Z4 "
   HBTEST oValue:SVARSCLASS1:x1           IS "{X1}"
   HBTEST oValue:SVARSCLASS1:y1           IS "{Y1}"
   HBTEST oValue:SVARSCLASS1:z1           IS "{Z1}"
   HBTEST oValue:SVARSCLASS2:x1           IS "{X1}"
   HBTEST oValue:SVARSCLASS2:y1           IS "{Y1}"
   HBTEST oValue:SVARSCLASS2:z1           IS "{Z1}"
   HBTEST oValue:SVARSCLASS2:x2           IS "{X2}"
   HBTEST oValue:SVARSCLASS2:y2           IS "{Y2}"
   HBTEST oValue:SVARSCLASS2:z2           IS "{Z2}"
   HBTEST oValue:SVARSCLASS3:x1           IS "{X1}"
   HBTEST oValue:SVARSCLASS3:y1           IS "{Y1}"
   HBTEST oValue:SVARSCLASS3:z1           IS "{Z1}"
   HBTEST oValue:SVARSCLASS3:x2           IS "{X2}"
   HBTEST oValue:SVARSCLASS3:y2           IS "{Y2}"
   HBTEST oValue:SVARSCLASS3:z2           IS "{Z2}"
   HBTEST oValue:SVARSCLASS3:x3           IS " X3 "
   HBTEST oValue:SVARSCLASS3:y3           IS " Y3 "
   HBTEST oValue:SVARSCLASS3:z3           IS " Z3 "
   HBTEST oValue:SVARSCLASS4:x1           IS "{X1}"
   HBTEST oValue:SVARSCLASS4:y1           IS "{Y1}"
   HBTEST oValue:SVARSCLASS4:z1           IS "{Z1}"
   HBTEST oValue:SVARSCLASS4:x2           IS "{X2}"
   HBTEST oValue:SVARSCLASS4:y2           IS "{Y2}"
   HBTEST oValue:SVARSCLASS4:z2           IS "{Z2}"
   HBTEST oValue:SVARSCLASS4:x3           IS " X3 "
   HBTEST oValue:SVARSCLASS4:y3           IS " Y3 "
   HBTEST oValue:SVARSCLASS4:z3           IS " Z3 "
   HBTEST oValue:SVARSCLASS4:x4           IS " X4 "
   HBTEST oValue:SVARSCLASS4:y4           IS " Y4 "
   HBTEST oValue:SVARSCLASS4:z4           IS " Z4 "
   HBTEST __cls_CntClsData( oValue:SVARSCLASS1:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:SVARSCLASS2:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:SVARSCLASS3:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:SVARSCLASS4:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:classH )             IS 0
   HBTEST __cls_CntShrData( oValue:SVARSCLASS1:classH ) IS 3
   HBTEST __cls_CntShrData( oValue:SVARSCLASS2:classH ) IS 3
   HBTEST __cls_CntShrData( oValue:SVARSCLASS3:classH ) IS 3
   HBTEST __cls_CntShrData( oValue:SVARSCLASS4:classH ) IS 3
   HBTEST __cls_CntShrData( oValue:classH )             IS 3
   HBTEST INSTANCE_DATA( oValue )         IS "[0]:"

   /* Setting SVARSCLASS3 class variables... */
   HBTEST oValue:SVARSCLASS3:x1 := "<X1>"    IS "<X1>"
   HBTEST oValue:SVARSCLASS3:y1 := "<Y1>"    IS "<Y1>"
   HBTEST oValue:SVARSCLASS3:z1 := "<Z1>"    IS "<Z1>"
   HBTEST oValue:SVARSCLASS3:x2 := "<X2>"    IS "<X2>"
   HBTEST oValue:SVARSCLASS3:y2 := "<Y2>"    IS "<Y2>"
   HBTEST oValue:SVARSCLASS3:z2 := "<Z2>"    IS "<Z2>"
   HBTEST oValue:SVARSCLASS3:x3 := "<X3>"    IS "<X3>"
   HBTEST oValue:SVARSCLASS3:y3 := "<Y3>"    IS "<Y3>"
   HBTEST oValue:SVARSCLASS3:z3 := "<Z3>"    IS "<Z3>"

   HBTEST oValue:x1                       IS "<X1>"
   HBTEST oValue:y1                       IS "<Y1>"
   HBTEST oValue:z1                       IS "<Z1>"
   HBTEST oValue:x2                       IS "<X2>"
   HBTEST oValue:y2                       IS "<Y2>"
   HBTEST oValue:z2                       IS "<Z2>"
   HBTEST oValue:x3                       IS "<X3>"
   HBTEST oValue:y3                       IS "<Y3>"
   HBTEST oValue:z3                       IS "<Z3>"
   HBTEST oValue:x4                       IS " X4 "
   HBTEST oValue:y4                       IS " Y4 "
   HBTEST oValue:z4                       IS " Z4 "
   HBTEST oValue:SVARSCLASS1:x1           IS "<X1>"
   HBTEST oValue:SVARSCLASS1:y1           IS "<Y1>"
   HBTEST oValue:SVARSCLASS1:z1           IS "<Z1>"
   HBTEST oValue:SVARSCLASS2:x1           IS "<X1>"
   HBTEST oValue:SVARSCLASS2:y1           IS "<Y1>"
   HBTEST oValue:SVARSCLASS2:z1           IS "<Z1>"
   HBTEST oValue:SVARSCLASS2:x2           IS "<X2>"
   HBTEST oValue:SVARSCLASS2:y2           IS "<Y2>"
   HBTEST oValue:SVARSCLASS2:z2           IS "<Z2>"
   HBTEST oValue:SVARSCLASS3:x1           IS "<X1>"
   HBTEST oValue:SVARSCLASS3:y1           IS "<Y1>"
   HBTEST oValue:SVARSCLASS3:z1           IS "<Z1>"
   HBTEST oValue:SVARSCLASS3:x2           IS "<X2>"
   HBTEST oValue:SVARSCLASS3:y2           IS "<Y2>"
   HBTEST oValue:SVARSCLASS3:z2           IS "<Z2>"
   HBTEST oValue:SVARSCLASS3:x3           IS "<X3>"
   HBTEST oValue:SVARSCLASS3:y3           IS "<Y3>"
   HBTEST oValue:SVARSCLASS3:z3           IS "<Z3>"
   HBTEST oValue:SVARSCLASS4:x1           IS "<X1>"
   HBTEST oValue:SVARSCLASS4:y1           IS "<Y1>"
   HBTEST oValue:SVARSCLASS4:z1           IS "<Z1>"
   HBTEST oValue:SVARSCLASS4:x2           IS "<X2>"
   HBTEST oValue:SVARSCLASS4:y2           IS "<Y2>"
   HBTEST oValue:SVARSCLASS4:z2           IS "<Z2>"
   HBTEST oValue:SVARSCLASS4:x3           IS "<X3>"
   HBTEST oValue:SVARSCLASS4:y3           IS "<Y3>"
   HBTEST oValue:SVARSCLASS4:z3           IS "<Z3>"
   HBTEST oValue:SVARSCLASS4:x4           IS " X4 "
   HBTEST oValue:SVARSCLASS4:y4           IS " Y4 "
   HBTEST oValue:SVARSCLASS4:z4           IS " Z4 "
   HBTEST __cls_CntClsData( oValue:SVARSCLASS1:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:SVARSCLASS2:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:SVARSCLASS3:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:SVARSCLASS4:classH ) IS 0
   HBTEST __cls_CntClsData( oValue:classH )             IS 0
   HBTEST __cls_CntShrData( oValue:SVARSCLASS1:classH ) IS 3
   HBTEST __cls_CntShrData( oValue:SVARSCLASS2:classH ) IS 3
   HBTEST __cls_CntShrData( oValue:SVARSCLASS3:classH ) IS 3
   HBTEST __cls_CntShrData( oValue:SVARSCLASS4:classH ) IS 3
   HBTEST __cls_CntShrData( oValue:classH )             IS 3
   HBTEST INSTANCE_DATA( oValue )         IS "[0]:"

   /* Test non virtual hidden messages */

   oValue := NVCLASS4():new()

   HBTEST oValue:m1() IS "NVCLASS1:M1 (a1) (b1) (c3) (d3) (e3) (f3) (ex1)|" + ;
                         "NVCLASS1:X  (a1) (b1) (c3) (d3) (e3) (f3) (ex1)|" + ;
                         "NVCLASS3:Y  (a3) (b3) (c3) (d3) (e3) (f3) (HI3)|" + ;
                         "NVCLASS3:Z  (a3) (b3) (c3) (d3) (e3) (f3) (HI3)"
   HBTEST oValue:m2() IS "NVCLASS2:M2 (a2) (b2) (c3) (d3) (e3) (f3) (ex1)|" + ;
                         "NVCLASS2:X  (a2) (b2) (c3) (d3) (e3) (f3) (ex1)|" + ;
                         "NVCLASS3:Y  (a3) (b3) (c3) (d3) (e3) (f3) (HI3)|" + ;
                         "NVCLASS3:Z  (a3) (b3) (c3) (d3) (e3) (f3) (HI3)"
   HBTEST oValue:m3() IS "NVCLASS3:M3 (a3) (b3) (c3) (d3) (e3) (f3) (HI3)|" + ;
                         "NVCLASS3:X  (a3) (b3) (c3) (d3) (e3) (f3) (HI3)|" + ;
                         "NVCLASS3:Y  (a3) (b3) (c3) (d3) (e3) (f3) (HI3)|" + ;
                         "NVCLASS3:Z  (a3) (b3) (c3) (d3) (e3) (f3) (HI3)"

   /* Test super casting */

   HBTEST oValue:classname()                                IS "NVCLASS4"
   HBTEST oValue:super:classname()                          IS "NVCLASS3"
   HBTEST oValue:super:super:classname()                    IS "NVCLASS1"
   HBTEST oValue:super:super:super:classname()              IS "HBOBJECT"
   HBTEST oValue:super:super:super:super:classname()        IS "E 13 BASE 1004 Message not found (NVCLASS4:SUPER) OS:0 #:0 A:1:O:NVCLASS4 Object F:S"

#endif

   RETURN


#ifdef __HARBOUR__

STATIC FUNCTION INSTANCE_DATA( oValue )

   LOCAL cData, i

   cData := "[" + hb_ntos( Len( oValue ) ) + "]:"
   FOR i := 1 TO Len( oValue )
      DO CASE
      CASE HB_ISSTRING( oValue[ i ] )
         cData += " " + oValue[ i ]
      CASE oValue[ i ] == NIL
         cData += " NIL"
      OTHERWISE
         cData += " ..."
      ENDCASE
   NEXT

   RETURN cData



CREATE CLASS DTORCLASS
EXPORTED:
   VAR         type
   VAR         var1
   CLASS VAR   var2
   METHOD      init
   DESTRUCTOR  dtor
ENDCLASS

METHOD INIT( type ) CLASS DTORCLASS
   ::type := type
RETURN self

METHOD PROCEDURE DTOR CLASS DTORCLASS

   DO CASE
   CASE ::Type == 1
      cDtorResult += "Reference to self in instance variable."
      ::var1 := self
   CASE ::Type == 2
      cDtorResult += "Reference to self in class variable."
      ::var2 := self
   CASE ::Type == 3
      cDtorResult += "Reference to self in private memvar."
      objHolder := self
   OTHERWISE
      cDtorResult += "No references to self."
   ENDCASE

   RETURN



CREATE CLASS IVARSCLASS1
EXPORTED:
    VAR x1 INIT "(x1)"
    VAR y1 INIT "(y1)"
    VAR z1 INIT "(z1)"
ENDCLASS

CREATE CLASS IVARSCLASS2 INHERIT IVARSCLASS1
EXPORTED:
    VAR x2 INIT "(x2)"
    VAR y2 INIT "(y2)"
    VAR z2 INIT "(z2)"
ENDCLASS

CREATE CLASS IVARSCLASS3 INHERIT IVARSCLASS1, IVARSCLASS2
EXPORTED:
    VAR x3 INIT "(x3)"
    VAR y3 INIT "(y3)"
    VAR z3 INIT "(z3)"
ENDCLASS

CREATE CLASS IVARSCLASS4 INHERIT IVARSCLASS3, IVARSCLASS2
EXPORTED:
    VAR x4 INIT "(x4)"
    VAR y4 INIT "(y4)"
    VAR z4 INIT "(z4)"
ENDCLASS



CREATE CLASS CVARSCLASS1
EXPORTED:
   CLASS VAR x1 INIT "(x1)"
   CLASS VAR y1 INIT "(y1)"
   CLASS VAR z1 INIT "(z1)"
ENDCLASS

CREATE CLASS CVARSCLASS2 INHERIT CVARSCLASS1
EXPORTED:
   CLASS VAR x2 INIT "(x2)"
   CLASS VAR y2 INIT "(y2)"
   CLASS VAR z2 INIT "(z2)"
ENDCLASS

CREATE CLASS CVARSCLASS3 INHERIT CVARSCLASS1, CVARSCLASS2
EXPORTED:
   CLASS VAR x3 INIT "(x3)"
   CLASS VAR y3 INIT "(y3)"
   CLASS VAR z3 INIT "(z3)"
ENDCLASS

CREATE CLASS CVARSCLASS4 INHERIT CVARSCLASS3, CVARSCLASS2
EXPORTED:
   CLASS VAR x4 INIT "(x4)"
   CLASS VAR y4 INIT "(y4)"
   CLASS VAR z4 INIT "(z4)"
ENDCLASS



CREATE CLASS SVARSCLASS1
EXPORTED:
   CLASS VAR x1 INIT "(x1)" SHARED
   CLASS VAR y1 INIT "(y1)" SHARED
   CLASS VAR z1 INIT "(z1)" SHARED
ENDCLASS

CREATE CLASS SVARSCLASS2 INHERIT SVARSCLASS1
EXPORTED:
   CLASS VAR x2 INIT "(x2)" SHARED
   CLASS VAR y2 INIT "(y2)" SHARED
   CLASS VAR z2 INIT "(z2)" SHARED
ENDCLASS

CREATE CLASS SVARSCLASS3 INHERIT SVARSCLASS1, SVARSCLASS2
EXPORTED:
   CLASS VAR x3 INIT "(x3)" SHARED
   CLASS VAR y3 INIT "(y3)" SHARED
   CLASS VAR z3 INIT "(z3)" SHARED
ENDCLASS

CREATE CLASS SVARSCLASS4 INHERIT SVARSCLASS3, SVARSCLASS2
EXPORTED:
   CLASS VAR x4 INIT "(x4)" SHARED
   CLASS VAR y4 INIT "(y4)" SHARED
   CLASS VAR z4 INIT "(z4)" SHARED
ENDCLASS


CREATE CLASS NVCLASS1
HIDDEN:
   VAR a init "(a1)"
   CLASS VAR b init "(b1)"
   METHOD x

PROTECTED:
   VAR c init "(c1)"
   CLASS VAR d init "(d1)"
   METHOD y

EXPORTED:
   VAR e init "(e1)"
   CLASS VAR f init "(f1)"
   VAR v init "(ex1)"
   METHOD z
   METHOD m1
ENDCLASS

METHOD m1 CLASS NVCLASS1

   RETURN "NVCLASS1:M1 " + ;
          hb_CStr( ::a ) + " " + ;
          hb_CStr( ::b ) + " " + ;
          hb_CStr( ::c ) + " " + ;
          hb_CStr( ::d ) + " " + ;
          hb_CStr( ::e ) + " " + ;
          hb_CStr( ::f ) + " " + ;
          hb_CStr( ::v ) + "|" + ;
         ::x() + "|" + ;
         ::y() + "|" + ;
         ::z()

METHOD x CLASS NVCLASS1

   RETURN "NVCLASS1:X  " + ;
          hb_CStr( ::a ) + " " + ;
          hb_CStr( ::b ) + " " + ;
          hb_CStr( ::c ) + " " + ;
          hb_CStr( ::d ) + " " + ;
          hb_CStr( ::e ) + " " + ;
          hb_CStr( ::f ) + " " + ;
          hb_CStr( ::v )

METHOD y CLASS NVCLASS1

   RETURN "NVCLASS1:Y  " + ;
          hb_CStr( ::a ) + " " + ;
          hb_CStr( ::b ) + " " + ;
          hb_CStr( ::c ) + " " + ;
          hb_CStr( ::d ) + " " + ;
          hb_CStr( ::e ) + " " + ;
          hb_CStr( ::f ) + " " + ;
          hb_CStr( ::v )

METHOD z CLASS NVCLASS1

   RETURN "NVCLASS1:Z  " + ;
          hb_CStr( ::a ) + " " + ;
          hb_CStr( ::b ) + " " + ;
          hb_CStr( ::c ) + " " + ;
          hb_CStr( ::d ) + " " + ;
          hb_CStr( ::e ) + " " + ;
          hb_CStr( ::f ) + " " + ;
          hb_CStr( ::v )


CREATE CLASS NVCLASS2
HIDDEN:
   VAR a init "(a2)"
   CLASS VAR b init "(b2)"
   METHOD x

PROTECTED:
   VAR c init "(c2)"
   CLASS VAR d init "(d2)"
   METHOD y

EXPORTED:
   VAR e init "(e2)"
   CLASS VAR f init "(f2)"
   METHOD z
   METHOD m2

ENDCLASS

METHOD m2 CLASS NVCLASS2

   RETURN "NVCLASS2:M2 " + ;
          hb_CStr( ::a ) + " " + ;
          hb_CStr( ::b ) + " " + ;
          hb_CStr( ::c ) + " " + ;
          hb_CStr( ::d ) + " " + ;
          hb_CStr( ::e ) + " " + ;
          hb_CStr( ::f ) + " " + ;
          hb_CStr( ::v ) + "|" + ;
         ::x() + "|" + ;
         ::y() + "|" + ;
         ::z()

METHOD x CLASS NVCLASS2

   RETURN "NVCLASS2:X  " + ;
          hb_CStr( ::a ) + " " + ;
          hb_CStr( ::b ) + " " + ;
          hb_CStr( ::c ) + " " + ;
          hb_CStr( ::d ) + " " + ;
          hb_CStr( ::e ) + " " + ;
          hb_CStr( ::f ) + " " + ;
          hb_CStr( ::v )

METHOD y CLASS NVCLASS2

   RETURN "NVCLASS2:Y  " + ;
          hb_CStr( ::a ) + " " + ;
          hb_CStr( ::b ) + " " + ;
          hb_CStr( ::c ) + " " + ;
          hb_CStr( ::d ) + " " + ;
          hb_CStr( ::e ) + " " + ;
          hb_CStr( ::f ) + " " + ;
          hb_CStr( ::v )

METHOD z CLASS NVCLASS2

   RETURN "NVCLASS2:Z  " + ;
          hb_CStr( ::a ) + " " + ;
          hb_CStr( ::b ) + " " + ;
          hb_CStr( ::c ) + " " + ;
          hb_CStr( ::d ) + " " + ;
          hb_CStr( ::e ) + " " + ;
          hb_CStr( ::f ) + " " + ;
          hb_CStr( ::v )


CREATE CLASS NVCLASS3 INHERIT NVCLASS1, NVCLASS2
HIDDEN:
   VAR a init "(a3)"
   CLASS VAR b init "(b3)"
   METHOD x
   VAR v init "(HI3)"

PROTECTED:
   VAR c init "(c3)"
   CLASS VAR d init "(d3)"
   METHOD y

EXPORTED:
   VAR e init "(e3)"
   CLASS VAR f init "(f3)"
   METHOD z
   METHOD m3

ENDCLASS

METHOD m3 CLASS NVCLASS3

   RETURN "NVCLASS3:M3 " + ;
          hb_CStr( ::a ) + " " + ;
          hb_CStr( ::b ) + " " + ;
          hb_CStr( ::c ) + " " + ;
          hb_CStr( ::d ) + " " + ;
          hb_CStr( ::e ) + " " + ;
          hb_CStr( ::f ) + " " + ;
          hb_CStr( ::v ) + "|" + ;
         ::x() + "|" + ;
         ::y() + "|" + ;
         ::z()

METHOD x CLASS NVCLASS3

   RETURN "NVCLASS3:X  " + ;
          hb_CStr( ::a ) + " " + ;
          hb_CStr( ::b ) + " " + ;
          hb_CStr( ::c ) + " " + ;
          hb_CStr( ::d ) + " " + ;
          hb_CStr( ::e ) + " " + ;
          hb_CStr( ::f ) + " " + ;
          hb_CStr( ::v )

METHOD y CLASS NVCLASS3

   RETURN "NVCLASS3:Y  " + ;
          hb_CStr( ::a ) + " " + ;
          hb_CStr( ::b ) + " " + ;
          hb_CStr( ::c ) + " " + ;
          hb_CStr( ::d ) + " " + ;
          hb_CStr( ::e ) + " " + ;
          hb_CStr( ::f ) + " " + ;
          hb_CStr( ::v )

METHOD z CLASS NVCLASS3

   RETURN "NVCLASS3:Z  " + ;
          hb_CStr( ::a ) + " " + ;
          hb_CStr( ::b ) + " " + ;
          hb_CStr( ::c ) + " " + ;
          hb_CStr( ::d ) + " " + ;
          hb_CStr( ::e ) + " " + ;
          hb_CStr( ::f ) + " " + ;
          hb_CStr( ::v )


CREATE CLASS NVCLASS4 INHERIT NVCLASS3
   METHOD m4
ENDCLASS

METHOD m4 CLASS NVCLASS4

   RETURN "NVCLASS4:M4 " + ;
          hb_CStr( ::a ) + " " + ;
          hb_CStr( ::b ) + " " + ;
          hb_CStr( ::c ) + " " + ;
          hb_CStr( ::d ) + " " + ;
          hb_CStr( ::e ) + " " + ;
          hb_CStr( ::f ) + " " + ;
          hb_CStr( ::v ) + "|" + ;
         ::x() + "|" + ;
         ::y() + "|" + ;
         ::z()

#endif


/* Don't change the position of this #include. */
#include "rt_init.ch"
