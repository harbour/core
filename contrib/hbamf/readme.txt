Short description of Harbour functions:

   cAMF := amf3_Encode( xVal, symConvOut, lBinaryStrings )
   xVal           - any supported datatype:
                    Character (String/MEMO), Numeric (Integer/Double), NIL, Logical, Date (encoded as DateTime),
                    DateTime, Array, Hash (String and Integer keys only),
                    Object (anonymous hash-like, externalizable, class-mapped)

                    AMF supports references, so this example a1 value will be serialized correctly.
                      a1 := { NIL }
                      a2 := { a1 }
                      a1[ 1 ] := a2

   symConvOut     - function symbol for outbound conversion
                    Acts recursively, so if xVal is array,
                    all of it's values will be passed to this
                    function.

   lBinaryStrings - treat strings as binary, resulting AMF
                    datatype will be ByteArray. Normally
                    a string is encoded to UTF-8.

   xVal := amf3_Decode( cAMF, symConvIn )
   cAMF           - AMF3 serialized binary string
   symConvIn      - function symbol for inbound conversion

   cAMF := amf3_FromWA( [ <bWhile> ], [ <bFor> ], [ <aFields> ], [ <nCount> ], [ <lStrTrim> ], [ <nPackage> ], [ pContext ] )

   Function to convert current workarea to AMF3 Array.

   bWhile   - COPY TO like WHILE codeblock
   bFor     - COPY TO like FOR codeblock
   aFields  - array of field names (codeblocks are going to be supported here too)
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

                 IF HB_ISOBJECT( xVal ) .AND. xVal:className() == "WORKAREAEXPORT"
                    lClose := xVal:lCloseWA
                    dbSelectArea( xVal:nWorkArea )

                    xVal := amf_Raw():New( amf3_FromWA( xVal:bWhile, xVal:bFor, xVal:aFields, xVal:nCount, xVal:lStrTrim, 1, pOuterContext ) )
                    IF lClose
                       dbCloseArea()
                    ENDIF
                 ENDIF

                 RETURN xVal

Issues
======

  ; amf3_Encode(), amf3_Decode()
    - there is no real serialization class-mapping included at the moment,
      due to lack of time to make a concept of it complete.
      Only anonymous (emulated on Harbour side using ObjAMF class)
      and externalizable objects are supported.

    - context->positon-- decrements should be removed from the .c code,
      before we make the functions able to work on real streams without
      buffering. There is no rewind in such situations.

  ; amf3_FromWA() - generates AMF3 array from current workarea,
    but the function doesn't have an idea of SET DELETED switch!
    It should have, because it tries to predict the number of records.
    As a workaround <bFor> parameter could be used, because it switches
    off the prediction. Another option could be creating temporary INDEX FOR,
    it should have correct ordKeyCount().

  ; amf3_Decode() - really doesn't need a hash for references, because
    reference id in AMF increase sequentially. It could be okay and faster
    to use some array with decent resize schema. Other than that benchmarks (in
    ST mode) showed that decoding speed is a little bit faster in this
    implementation than with Flash's built-in.

  ; .c function amf3_encode_string() does string hb_strRTrimLen()
    on a UTF-8 values. so far i haven't found a string that was broken
    by this, but i have a feeling that it is possible...

Note your issues too!
