/*
 * $Id$
 */

/*
 * File......: any2any.prg
 * Author....: David Husnian
 * CIS ID....: ?
 *
 * This is an original work by David Husnian and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   15 Aug 1991 23:02:46   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.1   14 Jun 1991 19:50:54   GLENN
 * Minor edit to file header
 *
 *    Rev 1.0   01 Apr 1991 01:00:34   GLENN
 * Nanforum Toolkit
 *
 */

#define BLOCKIFY(x)                  { || x }
#define IS_CHAR(x)                   (VALTYPE(x) == "C")
#define IS_DATE(x)                   (VALTYPE(x) == "D")
#define IS_LOGICAL(x)                (VALTYPE(x) == "L")
#define IS_NUMERIC(x)                (VALTYPE(x) == "N")
#define CASE_AT(x,y,z)               z[AT(x,y)+1]
#define TRIM_NUMBER(x)               hb_ntos(x)
#define NULL                         ""
#define IS_NOT_CHAR(x)               (VALTYPE(x) != "C")
#define IS_NOT_DATE(x)               (VALTYPE(x) != "D")
#define EARLIEST_DATE                STOD("01000101")
#define BLANK_DATE                   STOD()
#define IS_NOT_ARRAY(x)              (VALTYPE(x) != "A")
#define IS_NOT_LOGICAL(x)            (VALTYPE(x) != "L")
#define IS_NOT_NUMERIC(x)            (VALTYPE(x) != "N")
#define IS_NOT_CODE_BLOCK(x)         (VALTYPE(x) != "B")
#define TRUE                         (.t.)
#define FALSE                        (.f.)

#Define XTOC(x)           CASE_AT(VALTYPE(x), "CNDLM", ;
                             { NULL, ;
                               x, ;
                               iif(IS_NUMERIC(x),;
                                  TRIM_NUMBER(x), ;
                                  NULL), ;
                               iif(IS_DATE(x),DTOC(x),NULL),;
                               iif(IS_LOGICAL(x),;
                                  iif(x,".T.",".F."), ;
                                  NULL), ;
                               x })

#command    DEFAULT <Param1> TO <Def1> [, <ParamN> TO <DefN> ] ;
            => ;
            <Param1> := iif(<Param1> == NIL,<Def1>,<Param1>) ;
         [; <ParamN> := iif(<ParamN> == NIL,<DefN>,<ParamN>)]

FUNCTION FT_XTOY(xValueToConvert, cTypeToConvertTo, lWantYesNo)

   DEFAULT lWantYesNo TO FALSE

   DO CASE

      CASE cTypeToConvertTo == "C" .AND.; // They Want a Character String
           IS_NOT_CHAR(xValueToConvert)

         xValueToConvert := XTOC(xValueToConvert)

      CASE cTypeToConvertTo == "D" .AND.; // They Want a Date
           IS_NOT_DATE(xValueToConvert)

         xValueToConvert := iif(IS_CHAR(xValueToConvert), ;
                                      ; // Convert from a Character
                               CTOD(xValueToConvert), ;
                               iif(IS_NUMERIC(xValueToConvert), ;
                                      ; // Convert from a Number
                                  xValueToConvert + EARLIEST_DATE, ;
                                  iif(IS_LOGICAL(xValueToConvert), ;
                                      ; // Convert from a Logical
                                     iif(xValueToConvert, DATE(), BLANK_DATE), ;
                                      ; // Unsupported Type
                                     BLANK_DATE)))

      CASE cTypeToConvertTo == "N" .AND.; // They Want a Number
           IS_NOT_NUMERIC(xValueToConvert)

         xValueToConvert := iif(IS_CHAR(xValueToConvert), ;
                                      ; // Convert from a Character
                               VAL(xValueToConvert), ;
                               iif(IS_DATE(xValueToConvert), ;
                                      ; // Convert from a Date
                                  xValueToConvert - EARLIEST_DATE, ;
                                  iif(IS_LOGICAL(xValueToConvert), ;
                                      ; // Convert from a Logical
                                     iif(xValueToConvert, 1, 0), ;
                                      ; // Unsupported Type
                                     0)))

      CASE cTypeToConvertTo == "L" .AND.; // They Want a Logical
           IS_NOT_LOGICAL(xValueToConvert)

         xValueToConvert := iif(IS_CHAR(xValueToConvert), ;
                                      ; // Convert from a Character
                               UPPER(xValueToConvert) == iif(lWantYesNo,"Y",".T."), ;
                               iif(IS_DATE(xValueToConvert), ;
                                      ; // Convert from a Date
                                  ! EMPTY(xValueToConvert), ;
                                  iif(IS_NUMERIC(xValueToConvert), ;
                                      ; // Convert from a Number
                                     xValueToConvert != 0, ;
                                      ; // Unsupported Type
                                     FALSE)))

      CASE cTypeToConvertTo == "A" .AND.; // They Want an Array
           IS_NOT_ARRAY(xValueToConvert)

         xValueToConvert := { xValueToConvert }

      CASE cTypeToConvertTo == "B" .AND.; // They Want a Code Block
           IS_NOT_CODE_BLOCK(xValueToConvert)

         xValueToConvert := BLOCKIFY(xValueToConvert)

   ENDCASE

   RETURN xValueToConvert             // XToY
