/*
 * $Id$
 */

--------------------------------------
Short description of Harbour functions
--------------------------------------

   cAMF := AMF3_ENCODE( xVal, symConvOut, lBinaryStrings )
   xVal           - any supported datatype:
                    Character (String/MEMO), Numeric (Integer/Double), NIL, Logical, Date (encoded as DateTime),
                    DateTime, Array, Hash (String and Integer keys only),
                    Object (anonymous hash-like, externalizable, class-mapped)

                    AMF supports references, so this example a1 value will be serialized correctly.
                      a1 := { NIL }
                      a2 := { a1 }
                      a1[1] := a2

   symConvOut     - function symbol for outbound conversion
                    Acts recursively, so if xVal is array,
                    all of it's values will be passed to this
                    function.

   lBinaryStrings - treat strings as binary, resulting AMF
                    datatype will be ByteArray. Normally
                    a string is encoded to UTF-8.

   xVal := AMF3_DECODE( cAMF, symConvIn )
   cAMF           - AMF3 serialized binary string
   symConvIn      - function symbol for inbound conversion

   cAMF := AMF3_FROMWA( [ <bWhile> ], [ <bFor> ], [ <aFields> ], [ <nCount> ], [ <lStrTrim> ], [ <nPackage> ], [ pContext ] )

   Function to convert current workarea to AMF3 Array.

   bWhile   - COPY TO like WHILE codeblock
   bFor     - COPY TO like FOR codeblock
   aFields  - array of fieldnames (codeblocks are going to be supported here too)
   nCount   - NEXT like, process only specified count of records
   lStrTrim - RTrim() strings, default is .T.

   nPackage - determine the exact output AMF format
              0 - Array of records contains Arrays of fields (default)
              1 - Array of records contains Anonymous objects
              2 - ArrayCollection object
              Lower number means lesser packet size on the network.

   pContext - when this function is used inside AMF3_ENCODE specified
              outbound conversion function, you must pass pointer to
              encoding context, otherwise AMF3 references will encoded
              incorrectly. Example code of such case:
              STATIC FUNCTION ConvOut( xVal, pOuterContext )
                 LOCAL lClose

                 IF ValType( xVal ) = "O"
                     IF xVal:className == "WORKAREAEXPORT"
                       lClose := xVal:lCloseWA
                       SELECT ( xVal:nWorkArea )

                       xVal := RawAMF():New( AMF3_FROMWA( xVal:bWhile, xVal:bFor, xVal:aFields, xVal:nCount, xVal:lStrTrim, 1, pOuterContext ) )
                       IF lClose
                          CLOSE
                       ENDIF
                    ENDIF
                 ENDIF

              RETURN xVal
