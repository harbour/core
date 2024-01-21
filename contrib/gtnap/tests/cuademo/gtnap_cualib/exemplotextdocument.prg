/* encoding: cp850 */
#INCLUDE "cua.ch"
#INCLUDE "gtnap.ch"

****************************
PROC EXEMPLO_TEXT_DOCUMENT
****************************
LOCAL V_Janela

CUA20 @ 15,20,30,70 JANELA V_Janela ;
     TITULO "Escolha o tipo de operação" SUBTITULO "%T";
     AJUDA "T?????"

ESPECIALIZE V_Janela MENU
ADDOPCAO V_Janela TEXTO "Create R00688-PPA.pdf" ;
    ACAO TST_CREATE_DOCUMENT1() AJUDA "P06685"
 ADDOPCAO V_Janela TEXTO "Create R00236.pdf" ;
   ACAO TST_CREATE_DOCUMENT2() AJUDA "P06685"

ATIVE(V_Janela)

*************************************
STAT FUNCTION OFFICE_ERROR( C_Text )
*************************************
LOCAL N_Err := NAP_OFFICE_LAST_ERROR()
LOCAL C_Err := NAP_OFFICE_ERROR_STR(N_Err)
LOCAL L_Err := .F.

IF N_Err != SDKRES_OK
    MOSTRAR("M15566", C_Text + ": " + C_Err)
    L_Err = .T.
ENDIF

RETURN L_Err

******************************
STAT FUNC PAGINA_TEXT(N_Page)
******************************
    IF N_Page < 10
        RETURN "Página : 00" + hb_ntos(N_Page)
    ELSEIF N_Page < 100
        RETURN "Página : 0" + hb_ntos(N_Page)
    ENDIF

    RETURN "Página : " + hb_ntos(N_Page)

********************************
STAT FUNC SEPARATOR_LINE(N_Size)
********************************
    LOCAL C_STR := ""
    LOCAL N_Cont := 1

    FOR N_Cont:= 1 TO N_Size
        C_STR += "-"
    NEXT

    RETURN C_STR

********************************
STAT FUNC INDETERMINATE(N_Size)
********************************
    LOCAL C_STR := ""
    LOCAL N_Cont := 1

    FOR N_Cont:= 1 TO N_Size
        C_STR += "#"
    NEXT

    RETURN C_STR

********************************
STAT FUNC UNKNOWN(N_Size)
********************************
    LOCAL C_STR := ""
    LOCAL N_Cont := 1

    FOR N_Cont:= 1 TO N_Size
        C_STR += "?"
    NEXT

    RETURN C_STR

********************************
STAT FUNC COMPOSE_LINE(V_Line)
********************************
    LOCAL C_STR := ""
    LOCAL N_Cont := 1

    FOR N_Cont := 1 TO LEN(V_Line)
        IF VALTYPE(V_Line[N_Cont]) == "C"
            C_STR += V_Line[N_Cont]
        ELSEIF VALTYPE(V_Line[N_Cont]) == "N"
            C_STR += SPACE(V_Line[N_Cont])
        ENDIF
    NEXT

    RETURN C_STR

********************************
STAT FUNC COMPOSE_TABLINE(V_Tabs, V_Values)
********************************
    LOCAL C_STR := ""
    LOCAL C_Field := ""
    LOCAL N_Field_width := 0
    LOCAL N_Cont := 1
    LOCAL N_Width, N_Align

    FOR N_Cont := 1 TO LEN(V_Values)
        N_Width := V_Tabs[(N_Cont * 2) - 1]
        N_Align := V_Tabs[N_Cont * 2]
        IF VALTYPE(V_Values[N_Cont]) == "C"
            C_Field := V_Values[N_Cont]
        ELSEIF VALTYPE(V_Values[N_Cont]) == "N"
            C_Field := Transform(V_Values[N_Cont], "9.999.999,99")
        ENDIF

        N_Field_width := LEN(C_Field)

        // Write the data in its column
        IF N_Field_width <= N_Width
            IF N_Align == SDK_HALIGN_LEFT
                C_STR += C_Field
                C_STR += SPACE(N_Width - N_Field_width)
            ELSEIF N_Align == SDK_HALIGN_RIGHT
                C_STR += SPACE(N_Width - N_Field_width)
                C_STR += C_Field
            ELSE
                C_STR += UNKNOWN(N_Width)
            ENDIF

        // Data is wider than column
        ELSE
            C_STR += INDETERMINATE(N_Width)
        ENDIF
    NEXT

    RETURN C_STR

***********************************************************
STAT PROC DOCUMENT1_HEADER(O_DOC, N_Page, N_Width, V_Tabs)
***********************************************************
    LOCAL C_Line1 := COMPOSE_LINE({"Ceará", 52, "PPA 2022-2025"})
    LOCAL C_Line2 := COMPOSE_LINE({"Governo Municipal de Piquet Carneiro", 12, "DESPESAS POR FUNÇÃO E SUBFUNÇÃO", 30, PAGINA_TEXT(N_Page)})
    LOCAL C_Line3 := SEPARATOR_LINE(N_Width)
    LOCAL C_Line4 := COMPOSE_TABLINE(V_Tabs, {"FUNÇÃO/SUBFUNÇÃO", "Valor 2022", "Valor 2023", "Valor 2024", "Valor 2025", "Total"})
    LOCAL C_Line5 := SEPARATOR_LINE(N_Width)
    NAP_DOC_INSERT_TEXT(O_DOC, C_Line1)
    NAP_DOC_NEW_LINE(O_DOC)
    NAP_DOC_INSERT_TEXT(O_DOC, C_Line2)
    NAP_DOC_NEW_LINE(O_DOC)
    NAP_DOC_INSERT_TEXT(O_DOC, C_Line3)
    NAP_DOC_NEW_LINE(O_DOC)
    NAP_DOC_INSERT_TEXT(O_DOC, C_Line4)
    NAP_DOC_NEW_LINE(O_DOC)
    NAP_DOC_INSERT_TEXT(O_DOC, C_Line5)
    NAP_DOC_NEW_LINE(O_DOC)

***********************************
STAT PROC TST_CREATE_DOCUMENT1
***********************************
// R00688-PPA.pdf

LOCAL O_DOC := NAP_DOC_CREATE()

// Total chars by line
LOCAL N_Width := 130

// Field tabulations
LOCAL V_Tabs := {40, SDK_HALIGN_LEFT, 18, SDK_HALIGN_RIGHT, 18, SDK_HALIGN_RIGHT, 18, SDK_HALIGN_RIGHT, 18, SDK_HALIGN_RIGHT, 18, SDK_HALIGN_RIGHT}

IF OFFICE_ERROR("Erro ao criar documento de texto")
    RETURN
ENDIF

NAP_DOC_FONT_FAMILY(O_DOC, "Courier New")
NAP_DOC_FONT_SIZE(O_DOC, 6.0)

DOCUMENT1_HEADER(O_DOC, 1, N_Width, V_Tabs)

// Save the document
NAP_DOC_SAVE(O_DOC, {|| NAP_WORK_PATH() + "/../office/ods_gen/R00688-PPA.odt" })
OFFICE_ERROR("Erro ao salvar documento de texto")

// Export to PDF
NAP_DOC_PDF(O_DOC, {|| NAP_WORK_PATH() + "/../office/ods_gen/R00688-PPA.pdf" })
OFFICE_ERROR("Exportando para PDF")

// Close the document (mandatory)
NAP_DOC_CLOSE(O_DOC)
OFFICE_ERROR("Erro ao fechar o documento de texto")

MOSTRAR("M15566", "O documento de texto foi criado com sucesso.")

// Open the result into a LibreOffice window
NAP_OFFICE_BROWSE_DOC(NAP_WORK_PATH() + "/../office/ods_gen/R00688-PPA.odt")


***********************************
STAT PROC TST_CREATE_DOCUMENT2
***********************************
// R00236.pdf

LOCAL O_DOC := NAP_DOC_CREATE()

IF OFFICE_ERROR("Erro ao criar documento de texto")
    RETURN
ENDIF

NAP_DOC_FONT_FAMILY(O_DOC, "LucidaConsole")
NAP_DOC_FONT_SIZE(O_DOC, 12.0)
NAP_DOC_HALIGN(O_DOC, SDK_HALIGN_CENTER)
NAP_DOC_INSERT_TEXT(O_DOC, "PREFEITURA MUNICIPAL DE MACAPÁ")

// Save the document
NAP_DOC_SAVE(O_DOC, {|| NAP_WORK_PATH() + "/../office/ods_gen/Exemple_R00236.odt" })
OFFICE_ERROR("Erro ao salvar documento de texto")

// Export to PDF
NAP_DOC_PDF(O_DOC, {|| NAP_WORK_PATH() + "/../office/ods_gen/Exemple_R00236.pdf" })
OFFICE_ERROR("Exportando para PDF")

// Close the document (mandatory)
NAP_DOC_CLOSE(O_DOC)
OFFICE_ERROR("Erro ao fechar o documento de texto")

MOSTRAR("M15566", "O documento de texto foi criado com sucesso.")

// Open the result into a LibreOffice window
NAP_OFFICE_BROWSE_DOC(NAP_WORK_PATH() + "/../office/ods_gen/Exemple_R00236.odt")
