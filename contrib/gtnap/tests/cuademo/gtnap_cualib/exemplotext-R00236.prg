/* encoding: cp850 */
#INCLUDE "cua.ch"
#INCLUDE "gtnap.ch"
#INCLUDE "hboffice.ch"

***********************************
PROC DOC_ACCOUNT_DATA(O_DOC, C_Line)
    NAP_DOC_FONT_SIZE(O_DOC, 11.0)
    NAP_DOC_INSERT_TEXT(O_DOC, C_Line)
    NAP_DOC_INSERT_NEW_LINE(O_DOC)

***********************************
PROC DOC_ACCOUNT(O_DOC, V_Values, V_Tabs)
    NAP_DOC_FONT_SIZE(O_DOC, 6.7)
    DOCUMENT1_TABLINE(O_DOC, V_Values, V_Tabs)

***********************************
PROC TST_R00236
***********************************
LOCAL O_DOC := NAP_DOC_CREATE()
// Total chars by line
LOCAL N_Width := 130
LOCAL N_Cont := 1

// Field tabulations
LOCAL V_Tabs := {22, HBOFFICE_HALIGN_RIGHT, 18, HBOFFICE_HALIGN_RIGHT, 18, HBOFFICE_HALIGN_RIGHT, 18, HBOFFICE_HALIGN_RIGHT, 18, HBOFFICE_HALIGN_RIGHT, 18, HBOFFICE_HALIGN_RIGHT, 18, HBOFFICE_HALIGN_RIGHT}

IF OFFICE_ERROR("Erro ao criar documento de texto")
    RETURN
ENDIF

// Header, footer and page margins config
NAP_DOC_PAGE_HEADER_SHOW(O_DOC, .T.)
NAP_DOC_PAGE_HEADER_MARGINS(O_DOC, 0, 0, 508, 102, .T., .T.)
NAP_DOC_PAGE_FOOTER_SHOW(O_DOC, .T.)
NAP_DOC_PAGE_FOOTER_MARGINS(O_DOC, 354, 380, 608, 202, .T., .T.)
NAP_DOC_PAGE_MARGINS(O_DOC, 1270, 178, 1270, 533, 0)

// Create the header with image, document data and table headers
NAP_DOC_TEXT_SPACE(O_DOC, HBOFFICE_TEXT_SPACE_HEADER)
NAP_DOC_FONT_FAMILY(O_DOC, "Courier New")
NAP_DOC_FONT_SIZE(O_DOC, 10.0)
NAP_DOC_PARAGRAPH_HALIGN(O_DOC, HBOFFICE_HALIGN_CENTER)
DOC_LINE(O_DOC, "PREFEITURA MUNICIPAL DE MACAPÁ")
NAP_DOC_INSERT_IMAGE(O_DOC, HBOFFICE_ANCHOR_AT_PARAGRAPH, 2000, 2000, HBOFFICE_HALIGN_LEFT, HBOFFICE_VALIGN_CENTER, {|| NAP_WORK_PATH() + "/../office/ods/macapa.png"} )
NAP_DOC_INSERT_NEW_LINE(O_DOC)
NAP_DOC_INSERT_NEW_LINE(O_DOC)
NAP_DOC_INSERT_NEW_LINE(O_DOC)
NAP_DOC_INSERT_NEW_LINE(O_DOC)
NAP_DOC_INSERT_NEW_LINE(O_DOC)

NAP_DOC_INSERT_PARAGRAPH(O_DOC)
NAP_DOC_PARAGRAPH_HALIGN(O_DOC, HBOFFICE_HALIGN_LEFT)
NAP_DOC_FONT_SIZE(O_DOC, 6.7)
NAP_DOC_INSERT_TEXT(O_DOC, "Amapá")
NAP_DOC_INSERT_TEXT(O_DOC, SPACE(52))
NAP_DOC_INSERT_TEXT(O_DOC, "RESUMO  FINANCEIRO")
NAP_DOC_INSERT_NEW_LINE(O_DOC)
NAP_DOC_INSERT_TEXT(O_DOC, "Governo Municipal de Macapá")
NAP_DOC_INSERT_TEXT(O_DOC, SPACE(23))
NAP_DOC_INSERT_TEXT(O_DOC, "01/01/2023 a 31/01/2023 - em R$")
NAP_DOC_INSERT_TEXT(O_DOC, SPACE(39))
NAP_DOC_INSERT_TEXT(O_DOC, "Página: ")
NAP_DOC_INSERT_PAGE_NUMBER(O_DOC)
NAP_DOC_INSERT_NEW_LINE(O_DOC)
NAP_DOC_INSERT_TEXT(O_DOC, "Prefeitura Municipal de Macapá")
NAP_DOC_INSERT_NEW_LINE(O_DOC)
NAP_DOC_INSERT_DASH(O_DOC, N_Width)
NAP_DOC_INSERT_NEW_LINE(O_DOC)
DOCUMENT1_TABLINE(O_DOC, {"SALDO ANTERIOR", "RECEITAS", "DEPÓSITOS", "TRANSFERÊNCIAS", "SAQUES", "DESPESAS", "SALDO ATUAL"}, V_Tabs)
NAP_DOC_INSERT_DASH(O_DOC, N_Width)
NAP_DOC_INSERT_NEW_LINE(O_DOC)
// END OF HEADER

// All account data
NAP_DOC_TEXT_SPACE(O_DOC, HBOFFICE_TEXT_SPACE_PAGE)
NAP_DOC_PARAGRAPH_LSPACING(O_DOC, 330)
NAP_DOC_FONT_FAMILY(O_DOC, "Courier New")


// Seven pages
FOR N_Cont:= 1 TO 7

DOC_ACCOUNT_DATA(O_DOC, "BB..................5.159-4 (OB URB CANAL JANDIA)")
DOC_ACCOUNT(O_DOC, {"", 34524.89, "", "", "", "", ""}, V_Tabs)
DOC_ACCOUNT(O_DOC, {"", "------------------", "", "", "", "", ""}, V_Tabs)
DOC_ACCOUNT(O_DOC, {3830991.42, 34524.89, 0.00, 0.00, 0.00, 0.00, 3865516.31}, V_Tabs)
NAP_DOC_INSERT_DASH(O_DOC, N_Width)
NAP_DOC_INSERT_NEW_LINE(O_DOC)

DOC_ACCOUNT_DATA(O_DOC, "BB..................5.292-2 (CIDE-CONTRIB.I)")
DOC_ACCOUNT(O_DOC, {"", 8140.40, "", "", "", -23.86, ""}, V_Tabs)
DOC_ACCOUNT(O_DOC, {"", "------------------", "", "", "", "------------------", ""}, V_Tabs)
DOC_ACCOUNT(O_DOC, {604620.89, 8140.40, 0.00, 0.00, 0.00, -23.86, 612737.43}, V_Tabs)
NAP_DOC_INSERT_DASH(O_DOC, N_Width)
NAP_DOC_INSERT_NEW_LINE(O_DOC)

DOC_ACCOUNT_DATA(O_DOC, "BB..................5.881-5 (PMM PAVI URBAN)")
DOC_ACCOUNT(O_DOC, {"", 44.11, "", "", "", "", ""}, V_Tabs)
DOC_ACCOUNT(O_DOC, {"", "------------------", "", "", "", "", ""}, V_Tabs)
DOC_ACCOUNT(O_DOC, {4893.64, 44.11, 0.00, 0.00, 0.00, 0.00, 4937.75}, V_Tabs)
NAP_DOC_INSERT_DASH(O_DOC, N_Width)
NAP_DOC_INSERT_NEW_LINE(O_DOC)

DOC_ACCOUNT_DATA(O_DOC, "BB..................6.030-5 (SNA-SIMPLES NA) ")
DOC_ACCOUNT(O_DOC, {"", 1475972.94 , "", -1505000.00, "", "", ""}, V_Tabs)
DOC_ACCOUNT(O_DOC, {"", "------------------", "", "------------------", "", "", ""}, V_Tabs)
DOC_ACCOUNT(O_DOC, {52091.66, 1475972.94, 0.00, -1505000.00, 0.00, 0.00, 23064.60}, V_Tabs)
NAP_DOC_INSERT_DASH(O_DOC, N_Width)
NAP_DOC_INSERT_NEW_LINE(O_DOC)

DOC_ACCOUNT_DATA(O_DOC, "BB..................6.336-3 (ICMS)")
DOC_ACCOUNT(O_DOC, {"", 11740815.37, "", -10964000.00, "", "", ""}, V_Tabs)
DOC_ACCOUNT(O_DOC, {"", "------------------", "", "------------------", "", "", ""}, V_Tabs)
DOC_ACCOUNT(O_DOC, {1304.41, 11740815.37, 0.00, -10964000.00, 0.00, 0.00, 778119.78}, V_Tabs)
NAP_DOC_INSERT_DASH(O_DOC, N_Width)
NAP_DOC_INSERT_NEW_LINE(O_DOC)

DOC_ACCOUNT_DATA(O_DOC, "BB..................6.337-1 (IPVA)")
DOC_ACCOUNT(O_DOC, {"", 3129524.77, "", -3130000.00, "", "", ""}, V_Tabs)
DOC_ACCOUNT(O_DOC, {"", "------------------", "", "------------------", "", "", ""}, V_Tabs)
DOC_ACCOUNT(O_DOC, {2514.12, 3129524.77, 0.00, -3130000.00, 0.00, 0.00, 2038.89 }, V_Tabs)
NAP_DOC_INSERT_DASH(O_DOC, N_Width)
NAP_DOC_INSERT_NEW_LINE(O_DOC)

DOC_ACCOUNT_DATA(O_DOC, "BB..................6.442-4 (ABRIGO ÔNIBUS)")
DOC_ACCOUNT(O_DOC, {"", 72.31, "","", "", "", ""}, V_Tabs)
DOC_ACCOUNT(O_DOC, {"", "------------------", "", "", "", "", ""}, V_Tabs)
DOC_ACCOUNT(O_DOC, {8023.78, 72.31, 0.00, 0.00, 0.00, 0.00, 8096.09}, V_Tabs)
NAP_DOC_INSERT_DASH(O_DOC, N_Width)
NAP_DOC_INSERT_NEW_LINE(O_DOC)

DOC_ACCOUNT_DATA(O_DOC, "BB..................7.221-4 (ISS SUB STN TES.NAC)")
DOC_ACCOUNT(O_DOC, {"", 317334.71, "", -447512.48, "", "", ""}, V_Tabs)
DOC_ACCOUNT(O_DOC, {"", "------------------", "", "------------------", "", "", ""}, V_Tabs)
DOC_ACCOUNT(O_DOC, {180036.10, 317334.71, 0.00, -447512.48, 0.00, 0.00, 49858.33}, V_Tabs)
NAP_DOC_INSERT_DASH(O_DOC, N_Width)
NAP_DOC_INSERT_NEW_LINE(O_DOC)

DOC_ACCOUNT_DATA(O_DOC, "BB..................7.493-4 (CONV 003/14 GEA SETR) ")
DOC_ACCOUNT(O_DOC, {"", 733.15, "","", "", "", ""}, V_Tabs)
DOC_ACCOUNT(O_DOC, {"", "------------------", "", "", "", "", ""}, V_Tabs)
DOC_ACCOUNT(O_DOC, {81352.01, 733.15, 0.00, 0.00, 0.00, 0.00, 82085.16}, V_Tabs)
NAP_DOC_INSERT_DASH(O_DOC, N_Width)
NAP_DOC_INSERT_NEW_LINE(O_DOC)

DOC_ACCOUNT_DATA(O_DOC, "BB..................7.728-3 (PREC TRIBUNAL JUSTI)")
DOC_ACCOUNT(O_DOC, {"", 21121.69, "", -3077.70, "", "", ""}, V_Tabs)
DOC_ACCOUNT(O_DOC, {"", "", "", -530982.90, "", "", ""}, V_Tabs)
DOC_ACCOUNT(O_DOC, {"", "------------------", "", "------------------", "", "", ""}, V_Tabs)
DOC_ACCOUNT(O_DOC, {2225323.20, 21121.69, 0.00, -527905.20, 0.00, 0.00, 1718539.69}, V_Tabs)
NAP_DOC_INSERT_DASH(O_DOC, N_Width)
NAP_DOC_INSERT_NEW_LINE(O_DOC)

DOC_ACCOUNT_DATA(O_DOC, "BB..................5.881-5 (PMM PAVI URBAN)")
DOC_ACCOUNT(O_DOC, {"", 44.11, "", "", "", "", ""}, V_Tabs)
DOC_ACCOUNT(O_DOC, {"", "------------------", "", "", "", "", ""}, V_Tabs)
DOC_ACCOUNT(O_DOC, {4893.64, 44.11, 0.00, 0.00, 0.00, 0.00, 4937.75}, V_Tabs)
NAP_DOC_INSERT_DASH(O_DOC, N_Width)
NAP_DOC_INSERT_NEW_LINE(O_DOC)

NAP_DOC_INSERT_PAGE_BREAK(O_DOC)
NEXT

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
