/* Demonstrating operator overloading for creating an HTML document */

#include "dbstruct.ch"

#require "hbtip"
#require "hbtest"

PROCEDURE Main()

    LOCAL aDBStruct AS ARRAY

    LOCAL cAlias    AS CHARACTER
    LOCAL ctip_HtmlToStr AS CHARACTER

    LOCAL nField    AS NUMERIC
    LOCAL nFields   AS NUMERIC
    LOCAL nOption   AS NUMERIC
    LOCAL nProgress AS NUMERIC

    LOCAL oRow      AS OBJECT
    LOCAL oDoc      AS OBJECT
    LOCAL oLang     AS OBJECT
    LOCAL oNode     AS OBJECT
    LOCAL oCell     AS OBJECT
    LOCAL oLink     AS OBJECT
    LOCAL oMeta     AS OBJECT
    LOCAL oTable    AS OBJECT
    LOCAL oTitle    AS OBJECT
    LOCAL oScript   AS OBJECT
    LOCAL oComment  AS OBJECT

    LOCAL uValue

    IF !hbtest_Table()
        ? "Error: Test database couldn't be created"
        RETURN
    ENDIF

    cAlias:=Alias()

    oDoc:=THtmlDocument():New()

    with object oDoc

        oLang:=oDoc:root:html
        oLang:attr:={"lang"=>"en"}

        /* Operator "+" creates a new node */
        oMeta:=:head+"meta"
        with object oMeta
            :attr:={"http-equiv"=>"content-type","content"=>"text/html; charset=UTF-8"}
        end with

        /* Operator "+" creates a new node */
        oComment:=:head+"!--"
        with object oComment
            oComment:text:="Required meta tags"
        end with
        :head:AddNode(THtmlNode():New(:head,"--"))

        /* Operator "+" creates a new node */
        oMeta:=:head+"meta"
        with object oMeta
            :attr:={"charset"=>"UTF-8"}
        end with

        /* Operator "+" creates a new node */
        oMeta:=:head+"meta"
        with object oMeta
            :attr:={;
                "name"=>"viewport",;
                "content"=>"width=device-width, initial-scale=1, shrink-to-fit=no";
            }
        end with

        /* Operator "+" creates a new node */
        oMeta:=:head+"meta"
        with object oMeta
            :name:="Generator"
            :content:="Harbour/THtmlDocument"
        end with

        /* Operator "+" creates a new node */
        oComment:=:head+"!--"
        with object oComment
            oComment:text:="Bootstrap CSS"
        end with
        :head:AddNode(THtmlNode():New(:head,"--"))

        /* Operator "+" creates a new node */
        oLink:=:head+"link"
        with object oLink
            :attr:={;
                "rel"=>"stylesheet",;
                "href"=>"https://cdn.jsdelivr.net/npm/bootstrap@4.0.0/dist/css/bootstrap.min.css",;
                "integrity"=>"sha384-Gn5384xqQ1aoWXA+058RXPxPg6fy4IWvTNh0E263XmFcJlSAwiGgFAW/dAiS6JXm",;
                "crossorigin"=>"anonymous";
            }
        end with

        /* Operator "+" creates a new node */
        oComment:=:head+"!--"
        with object oComment
            oComment:text:="Optional JavaScript :: jQuery first, then Popper.js, then Bootstrap JS"
        end with
        :head:AddNode(THtmlNode():New(:head,"--"))

        /* Operator "+" creates a new node */
        oScript:=:head+"script"
        with object oScript
            :attr:={;
                "src"=>"https://code.jquery.com/jquery-3.2.1.slim.min.js",;
                "integrity"=>"sha384-KJ3o2DKtIkvYIK3UENzmM7KCkRr/rE9/Qpg6aAZGJwFDMVNA/GpGFF93hXpG5KkN",;
                "crossorigin"=>"anonymous";
            }
        end with

        /* Operator "+" creates a new node */
        oScript:=:head+"script"
        with object oScript
            :attr:={;
                "src"=>"https://cdn.jsdelivr.net/npm/popper.js@1.12.9/dist/umd/popper.min.js",;
                "integrity"=>"sha384-ApNbgh9B+Y1QKtv3Rn7W3mgPxhU9K/ScQsAP7hUibX39j7fakFPskvXusvfa0b4Q",;
                "crossorigin"=>"anonymous";
            }
        end with

        /* Operator "+" creates a new node */
        oScript:=:head+"script"
        with object oScript
            :attr:={;
                "src"=>"https://cdn.jsdelivr.net/npm/bootstrap@4.0.0/dist/js/bootstrap.min.js",;
                "integrity"=>"sha384-JZR6Spejh4U02d8jOt6vLEHfe/JQGiRRSQQxSfFWpi1MquVdAyjUar5+76PVCmYl",;
                "crossorigin"=>"anonymous";
            }
        end with

        /* Operator "+" creates a new node */
        oComment:=:head+"!--"
        with object oComment
            oComment:text:="https://materializecss.com :: Compiled and minified CSS"
        end with
        :head:AddNode(THtmlNode():New(:head,"--"))

        /* Operator "+" creates a new node */
        oLink:=:head+"link"
        with object oLink
            :attr:={;
                "rel"=>"stylesheet",;
                "href"=>"https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/css/materialize.min.css",;
                "crossorigin"=>"anonymous";
            }
        end with

        /* Operator "+" creates a new node */
        oComment:=:head+"!--"
        with object oComment
            oComment:text:="Compiled and minified JavaScript"
        end with
        :head:AddNode(THtmlNode():New(:head,"--"))

        /* Operator "+" creates a new node */
        oScript:=:head+"script"
        with object oScript
            :attr:={;
                "src"=>"https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/js/materialize.min.js",;
                "crossorigin"=>"anonymous";
            }
        end with

        /* Operator "+" creates a new node */
        oComment:=:head+"!--"
        with object oComment
            oComment:text:="https://materializecss.com/icons.html"
        end with
        :head:AddNode(THtmlNode():New(:head,"--"))

        /* Operator "+" creates a new node */
        oLink:=:head+"link"
        with object oLink
            :attr:={;
                "rel"=>"stylesheet",;
                "href"=>"https://fonts.googleapis.com/icon?family=Material+Icons";
            }
        end with

        oTitle:=:head:title
        oTitle:text="Harbour/dbtohtml.prg"

    end with

    with object oDoc:body

        /* Operator ":" returns first "h1" from body (creates if not existent) */
        oNode:=:h1
        with object oNode
            :text:="My address book"
        end with

        /* Operator "+" creates a new <p> node */
        oNode:=oDoc:body+"p"

            /* Operator "+=" creates a new <font> node with attribute */
            oNode+='font size="5"'
            oNode:text:="This is a "

                /* Operator "+" creates a new <b> node */
                oNode+="b"

                    /* Operator "+" creates a new <font> node with attribute */
                    oNode:=oNode+'font color="blue"'
                    oNode:text:="sample "

                    /* Operator "-=" closes 2nd <font>, result is <b> node */
                    oNode-="font"

                /* Operator "-=" closes <b> node, result is 1st <font> node */
                oNode-="b"

                oNode:text:="database!"

            /* Operator "-" closes 1st <font> node, result is <p> node */
            oNode-="font"
            HB_SYMBOL_UNUSED( oNode )

        oNode:=oDoc:body:AddNode(THtmlNode():New(oDoc:body,"/p"))
        HB_SYMBOL_UNUSED( oNode )

        oNode+="hr"
        HB_SYMBOL_UNUSED(oNode)

        /* Operator ":" returns first "table" from body (creates if not existent) */
        oTable:=:table
        with object oTable

            :attr:='class="table"'

            aDBStruct:=(cAlias)->(dbStruct())
            nFields:=Len(aDBStruct)
            nProgress:=0

            oRow:=oTable +'tr bgcolor="lightcyan"'
            FOR nField:=1 TO nFields
                Progress(@nProgress,Row(),Col()+3)
                oCell:=oRow+"th"
                oCell:text:=aDBStruct[nField][DBS_NAME]
                oCell-="th"
                HB_SYMBOL_UNUSED(oCell)
            NEXT nField

            oRow-="tr"
            HB_SYMBOL_UNUSED(oRow)

            (cAlias)->(dbGoTop())
            WHILE ((cAlias)->(!Eof()))

                Progress(@nProgress,Row(),Col()+3)

                oRow:=oTable+"tr"
                oRow:bgColor:=iif( RecNo() % 2 == 0, "lightgrey", "white" )

                FOR nField:=1 TO nFields
                    Progress(@nProgress,Row(),Col()+3)
                    oCell:=oRow+"td"
                    uValue:=(cAlias)->(FieldGet(nField))
                    if (aDbStruct[nField][DBS_TYPE]=="C")
                        uValue:=allTrim(uValue)
                    endif
                    oCell:text:=uValue
                    oCell-="td"
                    HB_SYMBOL_UNUSED(oCell)
                NEXT nField

                oRow-="tr"
                HB_SYMBOL_UNUSED(oRow)

                (cAlias)->(dbSkip())

            END WHILE

            oNode:=oDoc:body+"hr"
            HB_SYMBOL_UNUSED( oNode )
            oNode:=oDoc:body+"p"
            oNode:text:=hb_NToS((cAlias)->(RecCount()))+" records from database "+cAlias
            oNode:=oDoc:body:AddNode(THtmlNode():New(oDoc:body,"/p"))
            HB_SYMBOL_UNUSED( oNode )

        end with

    end with

    (cAlias)->(dbCloseArea())

    IF oDoc:writeFile( "address.html" , -9 , 4 )
        ? "File created:", "address.html"
    ELSE
        ? "Error:", FError()
    ENDIF

    ctip_HtmlToStr:=tip_HtmlToStr( oDoc:body:getText() )

    WAIT
    ? ctip_HtmlToStr
    hb_Memowrit("address.HtmlToStr.txt",ctip_HtmlToStr)

    hb_run( "address.html" )

    while .T.
        nOption := Alert( "Do you want to delete the address.html file?", { "YES" , "NO" } )
        switch nOption
        case 0
        case 2
            exit
        case 1
            hb_vfErase( "address.html" )
            exit
        end switch
        if (nOption>=1)
            exit
        endif
    end while

    RETURN

PROCEDURE Progress( nProgress, nDrow, nDcol )

   LOCAL nRow := Row(), nCol := Col()

   @ nDrow, nDcol SAY "[ ]"

   DO CASE
   CASE nProgress == 0
      @ nDrow, nDcol + 1 SAY "-"
   CASE nProgress == 1
      @ nDrow, nDcol + 1 SAY "\"
   CASE nProgress == 2
      @ nDrow, nDcol + 1 SAY "|"
   CASE nProgress == 3
      @ nDrow, nDcol + 1 SAY "/"
   ENDCASE

   nProgress++

   IF nProgress == 4
      nProgress := 0
   ENDIF

   @ nRow, nCol

   RETURN
