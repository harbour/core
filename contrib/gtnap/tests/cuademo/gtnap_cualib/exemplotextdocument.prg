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
STAT PROC SEPARATOR_LINE(O_DOC, N_Size)
********************************
    LOCAL N_Cont := 1

    FOR N_Cont:= 1 TO N_Size
        NAP_DOC_INSERT_DASH(O_DOC)
    NEXT

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
            C_Field := Transform(V_Values[N_Cont], "99,999,999.99")
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
    LOCAL C_Line3 := COMPOSE_TABLINE(V_Tabs, {"FUNÇÃO/SUBFUNÇÃO", "Valor 2022", "Valor 2023", "Valor 2024", "Valor 2025", "Total"})
    NAP_DOC_INSERT_TEXT(O_DOC, C_Line1)
    NAP_DOC_NEW_LINE(O_DOC)
    NAP_DOC_INSERT_TEXT(O_DOC, C_Line2)
    NAP_DOC_NEW_LINE(O_DOC)
    SEPARATOR_LINE(O_DOC, N_Width)
    NAP_DOC_NEW_LINE(O_DOC)
    NAP_DOC_INSERT_TEXT(O_DOC, C_Line3)
    NAP_DOC_NEW_LINE(O_DOC)
    SEPARATOR_LINE(O_DOC, N_Width)
    NAP_DOC_NEW_LINE(O_DOC)

********************************************
STAT PROC DOCUMENT1_SEPARATOR(O_DOC, N_Width)
********************************************
    SEPARATOR_LINE(O_DOC, N_Width)
    NAP_DOC_NEW_LINE(O_DOC)

***********************************************************
STAT PROC DOCUMENT1_TABLINE(O_DOC, V_Values, V_Tabs)
***********************************************************
    LOCAL C_Line := COMPOSE_TABLINE(V_Tabs, V_Values)
    NAP_DOC_INSERT_TEXT(O_DOC, C_Line)
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
NAP_DOC_LSPACING(O_DOC, 330)

// Page 1
DOCUMENT1_HEADER(O_DOC, 1, N_Width, V_Tabs)
DOCUMENT1_TABLINE(O_DOC, {"01-Legislativa", "", "", "", "", ""}, V_Tabs)
DOCUMENT1_TABLINE(O_DOC, {"   031-Ação Legislativa", 2015410.00, 2196797.00, 2394509.00, 2610014.00, 9216730.00}, V_Tabs)
DOCUMENT1_SEPARATOR(O_DOC, N_Width)
DOCUMENT1_TABLINE(O_DOC, {"   SUBTOTAL", 2015410.00, 2196797.00, 2394509.00, 2610014.00, 9216730.00}, V_Tabs)
DOCUMENT1_SEPARATOR(O_DOC, N_Width)
DOCUMENT1_TABLINE(O_DOC, {"04-Administração", "", "", "", "", ""}, V_Tabs)
DOCUMENT1_TABLINE(O_DOC, {"   122-Administração Geral", 4299393.00, 4717653.00, 5130657.00, 5604100.00, 19751803.00}, V_Tabs)
DOCUMENT1_TABLINE(O_DOC, {"   123-Administração Financeira", 70300.00, 83100.00, 90600.00, 98800.00, 342800.00}, V_Tabs)
DOCUMENT1_TABLINE(O_DOC, {"   124-Controle Interno", 130800.00, 142570.00, 155400.00, 169390.00, 598160.00}, V_Tabs)
DOCUMENT1_TABLINE(O_DOC, {"   128-Formação de Recursos Humanos", 25000.00, 25000.00, 25000.00, 25000.00, 100000.00}, V_Tabs)
DOCUMENT1_TABLINE(O_DOC, {"   131-Comunicação Social", 163500.00, 178200.00, 194200.00, 211700.00, 747600.00}, V_Tabs)
DOCUMENT1_TABLINE(O_DOC, {"   SUBTOTAL", 4688993.00, 5146523.00,  5595857.00,  6108990.00, 21540363.00}, V_Tabs)
DOCUMENT1_SEPARATOR(O_DOC, N_Width)
DOCUMENT1_TABLINE(O_DOC, {"06-Segurança Pública", "", "", "", "", ""}, V_Tabs)
DOCUMENT1_TABLINE(O_DOC, {"   122-Administração Geral", 30000.00, 35000.00, 40000.00, 45000.00, 150000.00}, V_Tabs)
DOCUMENT1_TABLINE(O_DOC, {"   181-Policiamento", 52000.00, 65000.00, 78000.00, 90000.00, 285000.00}, V_Tabs)
DOCUMENT1_SEPARATOR(O_DOC, N_Width)
DOCUMENT1_TABLINE(O_DOC, {"   SUBTOTAL", 82000.00, 100000.00, 118000.00, 135000.00, 435000.00}, V_Tabs)
DOCUMENT1_SEPARATOR(O_DOC, N_Width)
DOCUMENT1_TABLINE(O_DOC, {"08-Assistência Social", "", "", "", "", ""}, V_Tabs)
DOCUMENT1_TABLINE(O_DOC, {"   122-Administração Geral", 1408000.00, 1525720.00, 1654035.00, 1793898.00, 6381653.00}, V_Tabs)
DOCUMENT1_TABLINE(O_DOC, {"   125-Normalização e Fiscalização", 35000.00, 45000.00, 55000.00, 65000.00, 200000.00}, V_Tabs)
DOCUMENT1_TABLINE(O_DOC, {"   241-Assistência ao Idoso", 115000.00, 115000.00, 115000.00, 115000.00, 460000.00}, V_Tabs)
DOCUMENT1_TABLINE(O_DOC, {"   242-Assistência ao Portador de Defici", 40000.00, 40000.00, 40000.00, 40000.00, 160000.00}, V_Tabs)
DOCUMENT1_TABLINE(O_DOC, {"   243-Assistência à Criança e ao Adoles", 448700.00, 492845.00, 541159.00, 666037.00, 2148741.00}, V_Tabs)
DOCUMENT1_TABLINE(O_DOC, {"   244-Assistência Comunitária", 835760.00, 905253.00, 981822.00, 1066189.00, 3789024.00}, V_Tabs)
DOCUMENT1_SEPARATOR(O_DOC, N_Width)
DOCUMENT1_TABLINE(O_DOC, {"   SUBTOTAL", 2882460.00, 3123818.00, 3387016.00, 3746124.00, 13139418.00}, V_Tabs)
DOCUMENT1_SEPARATOR(O_DOC, N_Width)
DOCUMENT1_TABLINE(O_DOC, {"09-Previdência Social", "", "", "", "", ""}, V_Tabs)
DOCUMENT1_TABLINE(O_DOC, {"   272-Previdência do Regime Estatutário", 348800.00, 380100.00, 414400.00, 451700.00, 1595000.00}, V_Tabs)
DOCUMENT1_SEPARATOR(O_DOC, N_Width)
DOCUMENT1_TABLINE(O_DOC, {"   SUBTOTAL", 348800.00, 380100.00, 414400.00, 451700.00, 1595000.00}, V_Tabs)
DOCUMENT1_SEPARATOR(O_DOC, N_Width)
DOCUMENT1_TABLINE(O_DOC, {"10-Saúde", "", "", "", "", ""}, V_Tabs)
DOCUMENT1_TABLINE(O_DOC, {"   122-Administração Geral", 4533985.00, 5109009.00, 5754513.00, 6478763.00, 21876270.00}, V_Tabs)
DOCUMENT1_TABLINE(O_DOC, {"   125-Normalização e Fiscalização", 15000.00, 20000.00, 25000.00, 30000.00, 90000.00}, V_Tabs)
DOCUMENT1_TABLINE(O_DOC, {"   301-Atenção Básica", 6265700.00, 6827113.00, 7440517.00, 8108361.00, 28641691.00}, V_Tabs)
DOCUMENT1_TABLINE(O_DOC, {"   302-Assistência Hospitalar e Ambulato", 3923300.00, 4253897.00, 4614249.00, 5007030.00, 17798476.00}, V_Tabs)
DOCUMENT1_TABLINE(O_DOC, {"   303-Suporte Profilático e Terapêutico", 51800.00, 53762.00, 55901.00, 58232.00, 219695.00}, V_Tabs)
DOCUMENT1_TABLINE(O_DOC, {"   305-Vigilância Epidemiológica", 1090000.00, 1188080.00, 1295028.00, 1411582.00, 4984690.00}, V_Tabs)
DOCUMENT1_TABLINE(O_DOC, {"   306-Alimentação e Nutrição", 20000.00, 20000.00, 20000.00, 20000.00, 80000.00}, V_Tabs)
DOCUMENT1_TABLINE(O_DOC, {"   481-Habitação Rural", 81676.00, 211479.00, 362647.00, 540795.00, 1196597.00}, V_Tabs)
DOCUMENT1_SEPARATOR(O_DOC, N_Width)
DOCUMENT1_TABLINE(O_DOC, {"   SUBTOTAL", 15981461.00, 17683340.00, 19567855.00, 21654763.00, 74887419.00}, V_Tabs)
DOCUMENT1_SEPARATOR(O_DOC, N_Width)
DOCUMENT1_TABLINE(O_DOC, {"12-Educação", "", "", "", "", ""}, V_Tabs)
DOCUMENT1_TABLINE(O_DOC, {"   122-Administração Geral", 1815115.00, 1969475.00, 2137728.00, 2321124.00, 8243442.00}, V_Tabs)
DOCUMENT1_TABLINE(O_DOC, {"   125-Normalização e Fiscalização", 15000.00, 20000.00, 25000.00, 30000.00, 90000.00}, V_Tabs)
DOCUMENT1_TABLINE(O_DOC, {"   306-Alimentação e Nutrição", 514044.00, 560308.00, 610735.00, 665702.00, 2350789.00}, V_Tabs)
NAP_DOC_PAGE_BREAK(O_DOC)

// Page 2 (TODO)
DOCUMENT1_HEADER(O_DOC, 2, N_Width, V_Tabs)
DOCUMENT1_TABLINE(O_DOC, {"01-Legislativa", "", "", "", "", ""}, V_Tabs)
DOCUMENT1_TABLINE(O_DOC, {"   031-Ação Legislativa", 2015410.00, 2196797.00, 2394509.00, 2610014.00, 9216730.00}, V_Tabs)
DOCUMENT1_SEPARATOR(O_DOC, N_Width)

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
