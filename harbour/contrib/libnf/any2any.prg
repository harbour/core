/*
 * File......: Any2Any.Prg
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


/*  $DOC$
 *  $FUNCNAME$
 *     FT_XTOY()
 *  $CATEGORY$
 *     Conversion
 *  $ONELINER$
 *     Convert from any data type to any other data type
 *  $SYNTAX$
 *     FT_XTOY( <xValueToConvert>, <cTypeToConvertTo> ;
 *              [, <lWantYesNo> ] ) -> xResult
 *  $ARGUMENTS$
 *     <xValueToConvert> is the value to convert.
 *
 *     <cTypeToConvertTo> is the type of value to convert to
 *     ("C","D","L","N","A" or "B").
 *
 *     <lWantYesNo> is a logical to signal if 'Y' or 'N' is to be returned
 *     if Converting a logical, otherwise '.T.' or '.F.' will be returned
 *     for logicals.
 *  $RETURNS$
 *     The original value converted to the new type.
 *  $DESCRIPTION$
 *     This function converts a value of character, date, numeric, logical,
 *     array or code block type to any of the other type.  While it is
 *     guaranteed to return a value of the correct type, that value may not
 *     be meaningful (i.e., converting from a code block returns an EMPTY()
 *     value of the desired type).
 *  $EXAMPLES$
 *     nNumericValue := FT_XTOY(cInputValue, "N")
 *     IF (FT_XTOY(nInputValue, "L"))
 *  $END$
 */


#define BLOCKIFY(x)                  { || x }
#define IS_CHAR(x)                   (VALTYPE(x) == "C")
#define IS_DATE(x)                   (VALTYPE(x) == "D")
#define IS_LOGICAL(x)                (VALTYPE(x) == "L")
#define IS_NUMERIC(x)                (VALTYPE(x) == "N")
#define CASE_AT(x,y,z)               z[AT(x,y)+1]
#define TRIM_NUMBER(x)               LTRIM(STR(x))
#define NULL                         ""
#define IS_NOT_CHAR(x)               (VALTYPE(x) != "C")
#define IS_NOT_DATE(x)               (VALTYPE(x) != "D")
#define EARLIEST_DATE                CTOD("01/01/0100")
#define BLANK_DATE                   CTOD(NULL)
#define IS_NOT_ARRAY(x)              (VALTYPE(x) != "A")
#define IS_NOT_LOGICAL(x)            (VALTYPE(x) != "L")
#define IS_NOT_NUMERIC(x)            (VALTYPE(x) != "N")
#define IS_NOT_CODE_BLOCK(x)         (VALTYPE(x) != "B")
#define TRUE                         (.t.)
#define FALSE                        (.f.)

#Define XTOC(x)           CASE_AT(VALTYPE(x), "CNDLM", ;
                             { NULL, ;
                               x, ;
                               IF(IS_NUMERIC(x),;
                                  TRIM_NUMBER(x), ;
                                  NULL), ;
                               IF(IS_DATE(x),DTOC(x),NULL),;
                               IF(IS_LOGICAL(x),;
                                  IF(x,".T.",".F."), ;
                                  NULL), ;
                               x })

#command    DEFAULT <Param1> TO <Def1> [, <ParamN> TO <DefN> ] ;
            => ;
            <Param1> := IF(<Param1> == NIL,<Def1>,<Param1>) ;
         [; <ParamN> := IF(<ParamN> == NIL,<DefN>,<ParamN>)]


FUNCTION FT_XTOY(xValueToConvert, cTypeToConvertTo, lWantYesNo)

   DEFAULT lWantYesNo TO FALSE

   DO CASE

      CASE cTypeToConvertTo == "C" .AND.; // They Want a Character String
           IS_NOT_CHAR(xValueToConvert)

         xValueToConvert := XTOC(xValueToConvert)

      CASE cTypeToConvertTo == "D" .AND.; // They Want a Date
           IS_NOT_DATE(xValueToConvert)


         xValueToConvert := IF(IS_CHAR(xValueToConvert), ;
                                      ; // Convert from a Character
                               CTOD(xValueToConvert), ;
                               IF(IS_NUMERIC(xValueToConvert), ;
                                      ; // Convert from a Number
                                  xValueToConvert + EARLIEST_DATE, ;
                                  IF(IS_LOGICAL(xValueToConvert), ;
                                      ; // Convert from a Logical
                                     IF(xValueToConvert, DATE(), BLANK_DATE), ;
                                      ; // Unsupported Type
                                     BLANK_DATE)))

      CASE cTypeToConvertTo == "N" .AND.; // They Want a Number
           IS_NOT_NUMERIC(xValueToConvert)


         xValueToConvert := IF(IS_CHAR(xValueToConvert), ;
                                      ; // Convert from a Character
                               VAL(xValueToConvert), ;
                               IF(IS_DATE(xValueToConvert), ;
                                      ; // Convert from a Date
                                  xValueToConvert - EARLIEST_DATE, ;
                                  IF(IS_LOGICAL(xValueToConvert), ;
                                      ; // Convert from a Logical
                                     IF(xValueToConvert, 1, 0), ;
                                      ; // Unsupported Type
                                     0)))

      CASE cTypeToConvertTo == "L" .AND.; // They Want a Logical
           IS_NOT_LOGICAL(xValueToConvert)


         xValueToConvert := IF(IS_CHAR(xValueToConvert), ;
                                      ; // Convert from a Character
                               UPPER(xValueToConvert) == IF(lWantYesNo,"Y",".T."), ;
                               IF(IS_DATE(xValueToConvert), ;
                                      ; // Convert from a Date
                                  ! EMPTY(xValueToConvert), ;
                                  IF(IS_NUMERIC(xValueToConvert), ;
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

   RETURN (xValueToConvert)             // XToY                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
