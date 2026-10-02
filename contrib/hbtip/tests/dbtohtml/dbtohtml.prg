/*
    Demonstrating operator overloading for creating an HTML document
    Document checking completed. No errors or warnings to show.
    https://validator.w3.org/nu/#file
*/

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

    with object oDoc:body:main

        /* Operator ":" returns first "h1" from body (creates if not existent) */
        oNode:=:h1
        with object oNode
            :text:="My address book"
        end with

        /* Operator "+" creates a new <p> node */
        oNode:=oDoc:body:main+"p"
        oNode:attr:={"style"=>"font-size: 30px"}
        oNode:text:="Harbour THtmlDocument "

        /* Operator "+" creates a new <b> node */
        oNode+="b"
            oNode:attr:={"style"=>"color:blue"}
            oNode:text:="sample "
        /* Operator "-=" closes <b> node*/
        oNode-="b"

        oNode:text:="database!"

        /* closes <p> node*/
        oNode:=oDoc:body:main:AddNode(THtmlNode():New(oDoc:body:main,"/p"))
        HB_SYMBOL_UNUSED( oNode )

        oNode+="hr"
        HB_SYMBOL_UNUSED(oNode)

        /* Operator ":" returns first "table" from body (creates if not existent) */
        oTable:=:table
        with object oTable

            :attr:='class="table table-striped table-hover"'

            aDBStruct:=(cAlias)->(dbStruct())
            nFields:=Len(aDBStruct)
            nProgress:=0

            oRow:=oTable +'tr'

            oCell:=oRow:AddNode(THtmlNode():New(oRow,"th"))
            oCell:scope:="row"
            oCell:text:="#"
            oCell:=oCell-"th"
            HB_SYMBOL_UNUSED(oCell)

            FOR nField:=1 TO nFields
                Progress(@nProgress,Row(),Col()+3)
                oCell:=oRow+"th"
                oCell:scope:="col"
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

                oCell:=oRow:AddNode(THtmlNode():New(oRow,"th"))
                oCell:scope:="row"
                oCell:text:=(cAlias)->(RecNo())
                oCell:=oCell-"th"
                HB_SYMBOL_UNUSED(oCell)

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

            oNode:=oDoc:body:main+"hr"
            HB_SYMBOL_UNUSED( oNode )
            oNode:=oDoc:body:main+"p"
            oNode:text:=hb_NToS((cAlias)->(RecCount()))+" records from database "+cAlias
            oNode:=oDoc:body:main:AddNode(THtmlNode():New(oDoc:body:main,"/p"))
            HB_SYMBOL_UNUSED( oNode )

        end with

    end with

    (cAlias)->(dbCloseArea())

    IF oDoc:writeFile( "address.html" , -9 , 4 )
        ? "File created:", "address.html"
    ELSE
        ? "Error:", FError()
    ENDIF

    ctip_HtmlToStr:=tip_HtmlToStr( oDoc:body:main:getText() )

    IF hb_Memowrit("address.HtmlToStr.txt",ctip_HtmlToStr)
        ? "File created:", "address.HtmlToStr.txt"
    ENDIF

    hb_run( "address.html" )

    while .T.
        nOption := Alert( "Do you want to delete the address.html and address.HtmlToStr.txt files?", { "YES" , "NO" } )
        switch nOption
        case 0
        case 2
            exit
        case 1
            hb_vfErase( "address.html" )
            hb_vfErase( "address.HtmlToStr.txt" )
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
