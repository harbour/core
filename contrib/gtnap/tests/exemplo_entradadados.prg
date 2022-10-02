#require "gtnap"
#include "gtnap.ch"

/*---------------------------------------------------------------------------*/

PROC EXEMPLO_ENTRADA_DADOS()
    LOCAL V_Janela, V_Panel, V_Layout1
    LOCAL V_MenuVert
    LOCAL V_Label1

    V_Janela := NAP_WINDOW_CREATE(ekNAP_WINDOW_STD + ekNAP_WINDOW_ESC)
    V_Panel := NAP_PANEL_CREATE()
    V_Layout1 := NAP_LAYOUT_CREATE(1, 2)
    V_MenuVert := NAP_MENUVERT_CREATE()
    V_Label1 := NAP_LABEL_WITH_TEXT("Escolha o tipo de janela entrada de dados")
    NAP_MENUVERT_ADD(V_MenuVert, "Entrada de dados (read-only)", {|| TST_ENTRADA_DADOS_TODA_READ_ONLY() })
    NAP_MENUVERT_ADD(V_MenuVert, "Entrada de dados com campo read-only", {|| TST_ENTRADA_DADOS_COM_CAMPO_READ_ONLY() })
    NAP_MENUVERT_ADD(V_MenuVert, "Entrada de dados com confirmações", {|| TST_ENTRADA_DADOS_COM_CONFIRMACOES() })
    NAP_MENUVERT_ADD(V_MenuVert, "Entrada de dados com data inválida", {|| TST_ENTRADA_DADOS_DATA_INVALIDA() })
    NAP_MENUVERT_ADD(V_MenuVert, "Entrada de dados com filtro de tecla", {|| TST_ENTRADA_DADOS_FILTRO_TECLAS() })
    NAP_MENUVERT_ADD(V_MenuVert, "Entrada de dados com rolamento vertical", {|| TST_ENTRADA_DADOS_COM_ROLAMENTO() })
    NAP_MENUVERT_ADD(V_MenuVert, "Entrada de dados com cores e mensagens", {|| TST_ENTRADA_DADOS_CORES_MENSAGENS() })
    NAP_MENUVERT_ADD(V_MenuVert, "Entrada de dados com valids e campo memo", {|| TST_ENTRADA_DADOS_VALID() })
    NAP_MENUVERT_ADD(V_MenuVert, "Entrada de dados com 'lista' e 'auto'", {|| TST_ENTRADA_DADOS_LISTA_AUTO() })
    NAP_LAYOUT_LABEL(V_Layout1, V_Label1, 0, 0)
    NAP_LAYOUT_PANEL(V_Layout1, V_MenuVert, 0, 1)
    NAP_LAYOUT_VMARGIN(V_Layout1, 0, 20)
    NAP_LAYOUT_MARGIN4(V_Layout1, 0, 100, 20, 100)
    NAP_PANEL_LAYOUT(V_Panel, V_Layout1)
    NAP_WINDOW_PANEL(V_Janela, V_Panel)
    NAP_WINDOW_TITLE(V_Janela, "Escolha o tipo de janela")
    NAP_WINDOW_MODAL(V_Janela)
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_ENTRADA_DADOS_TODA_READ_ONLY()
    LOCAL V_Janela, V_Panel, V_Layout1, V_Layout2, V_Layout3
    LOCAL V_Label1
    LOCAL V_ELabel1, V_ELabel2
    LOCAL V_Edit1, V_Edit2
    LOCAL C_String

    C_String := "Teste de entrada de dados" + hb_eol() + "toda read-only"
    V_Janela := NAP_WINDOW_CREATE(ekNAP_WINDOW_STD + ekNAP_WINDOW_ESC)
    V_Panel := NAP_PANEL_CREATE()
    V_Layout1 := NAP_LAYOUT_CREATE(1, 2)
    V_Layout2 := NAP_LAYOUT_CREATE(2, 2)
    V_Label1 := NAP_LABEL_MULTILINE()
    V_ELabel1 := NAP_LABEL_WITH_TEXT("Campo 1")
    V_ELabel2 := NAP_LABEL_WITH_TEXT("Campo 2")
    V_Edit1 := NAP_EDIT_CREATE()
    V_Edit2 := NAP_EDIT_CREATE()

    // Widget configuration
    NAP_LABEL_TEXT(V_Label1, C_String)
    NAP_LABEL_ALIGN(V_Label1, ekNAP_ALIGN_CENTER)
    NAP_EDIT_TEXT(V_Edit1, "Este campo é read-only")
    NAP_EDIT_TEXT(V_Edit2, "Este campo é read-only")
    NAP_EDIT_EDITABLE(V_Edit1, .F.)
    NAP_EDIT_EDITABLE(V_Edit2, .F.)

    // Widget disposition in layouts
    NAP_LAYOUT_LABEL(V_Layout1, V_Label1, 0, 0)
    NAP_LAYOUT_LABEL(V_Layout2, V_ELabel1, 0, 0)
    NAP_LAYOUT_LABEL(V_Layout2, V_ELabel2, 0, 1)
    NAP_LAYOUT_EDIT(V_Layout2, V_Edit1, 1, 0)
    NAP_LAYOUT_EDIT(V_Layout2, V_Edit2, 1, 1)
    NAP_LAYOUT_LAYOUT(V_Layout1, V_Layout2, 0, 1)

    // Margins and alignment
    // Main label is horizontally centered with image-menu
    NAP_LAYOUT_HALIGN(V_Layout1, 0, 0, ekNAP_ALIGN_CENTER)
    // Force the EditBox column to 400 px
    NAP_LAYOUT_HSIZE(V_Layout2, 1, 400)
    // Horizontal space between labels and edits
    NAP_LAYOUT_HMARGIN(V_Layout2, 0, 40)
    // Vertical space between edits
    NAP_LAYOUT_VMARGIN(V_Layout2, 0, 10)
    // Global border margin
    NAP_LAYOUT_MARGIN4(V_Layout1, 0, 120, 250, 50)

    // Adding toolbar
    V_Layout3 := LAYOUT_WITH_TOOLBAR(V_Layout1, {"copia.bmp", "calcula.bmp", "ajuda.bmp", "saida.bmp"}, {"Copiar", "Calculadora", "Ajuda", "Saida"})

    // Configure window and launch
    NAP_PANEL_LAYOUT(V_Panel, V_Layout3)
    NAP_WINDOW_PANEL(V_Janela, V_Panel)
    NAP_WINDOW_TITLE(V_Janela, "Teste de entrada de dados")
    NAP_WINDOW_MODAL(V_Janela)
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_ENTRADA_DADOS_COM_CAMPO_READ_ONLY()
    LOCAL V_Janela, V_Panel, V_Layout1, V_Layout2, V_Layout3
    LOCAL V_Label1
    LOCAL V_ELabel1, V_ELabel2
    LOCAL V_Edit1, V_Edit2
    LOCAL C_String

    C_String := "Teste de entrada de dados" + hb_eol() + "com campo read-only"
    V_Janela := NAP_WINDOW_CREATE(ekNAP_WINDOW_STD + ekNAP_WINDOW_ESC)
    V_Panel := NAP_PANEL_CREATE()
    V_Layout1 := NAP_LAYOUT_CREATE(1, 2)
    V_Layout2 := NAP_LAYOUT_CREATE(2, 2)
    V_Label1 := NAP_LABEL_MULTILINE()
    V_ELabel1 := NAP_LABEL_WITH_TEXT("Campo 1")
    V_ELabel2 := NAP_LABEL_WITH_TEXT("Campo 2")
    V_Edit1 := NAP_EDIT_CREATE()
    V_Edit2 := NAP_EDIT_CREATE()

    // Widget configuration
    NAP_LABEL_TEXT(V_Label1, C_String)
    NAP_LABEL_ALIGN(V_Label1, ekNAP_ALIGN_CENTER)
    NAP_EDIT_TEXT(V_Edit1, "O campo 2 é read-write")
    NAP_EDIT_TEXT(V_Edit2, "só se o campo 1 estiver vazio")
    NAP_EDIT_EDITABLE(V_Edit1, .T.)
    NAP_EDIT_EDITABLE(V_Edit2, .F.)

    // Widget disposition in layouts
    NAP_LAYOUT_LABEL(V_Layout1, V_Label1, 0, 0)
    NAP_LAYOUT_LABEL(V_Layout2, V_ELabel1, 0, 0)
    NAP_LAYOUT_LABEL(V_Layout2, V_ELabel2, 0, 1)
    NAP_LAYOUT_EDIT(V_Layout2, V_Edit1, 1, 0)
    NAP_LAYOUT_EDIT(V_Layout2, V_Edit2, 1, 1)
    NAP_LAYOUT_LAYOUT(V_Layout1, V_Layout2, 0, 1)

    // Margins and alignment
    // Main label is horizontally centered with image-menu
    NAP_LAYOUT_HALIGN(V_Layout1, 0, 0, ekNAP_ALIGN_CENTER)
    // Force the EditBox column to 400 px
    NAP_LAYOUT_HSIZE(V_Layout2, 1, 400)
    // Horizontal space between labels and edits
    NAP_LAYOUT_HMARGIN(V_Layout2, 0, 40)
    // Vertical space between edits
    NAP_LAYOUT_VMARGIN(V_Layout2, 0, 10)
    // Global border margin
    NAP_LAYOUT_MARGIN4(V_Layout1, 0, 120, 250, 50)

    // Adding toolbar
    V_Layout3 := LAYOUT_WITH_TOOLBAR(V_Layout1, {"recortar.bmp", "copia.bmp", "colar.bmp", "desfazer.bmp", "calcula.bmp", "ajuda.bmp", "saida.bmp"}, {"Recortar", "Copiar", "Colar", "Desfazer", "Calculadora", "Ajuda", "Saida"})

    // Configure window and launch
    NAP_PANEL_LAYOUT(V_Panel, V_Layout3)
    NAP_WINDOW_PANEL(V_Janela, V_Panel)
    NAP_WINDOW_TITLE(V_Janela, "Teste de entrada de dados")
    NAP_LAYOUT_FOCUS(V_Layout2, 1, 0)
    NAP_WINDOW_MODAL(V_Janela)
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_ENTRADA_DADOS_COM_CONFIRMACOES()
    LOCAL V_Janela, V_Panel, V_Layout1, V_Layout2, V_Layout3
    LOCAL V_Label1
    LOCAL V_ELabel1, V_ELabel2
    LOCAL V_Edit1, V_Edit2
    LOCAL C_String

    C_String := "Teste de entrada de dados" + hb_eol() + "com confirmações"
    V_Janela := NAP_WINDOW_CREATE(ekNAP_WINDOW_STD + ekNAP_WINDOW_ESC)
    V_Panel := NAP_PANEL_CREATE()
    V_Layout1 := NAP_LAYOUT_CREATE(1, 2)
    V_Layout2 := NAP_LAYOUT_CREATE(2, 2)
    V_Label1 := NAP_LABEL_MULTILINE()
    V_ELabel1 := NAP_LABEL_WITH_TEXT("Campo 1")
    V_ELabel2 := NAP_LABEL_WITH_TEXT("Campo 2")
    V_Edit1 := NAP_EDIT_CREATE()
    V_Edit2 := NAP_EDIT_CREATE()

    // Widget configuration
    NAP_LABEL_TEXT(V_Label1, C_String)
    NAP_LABEL_ALIGN(V_Label1, ekNAP_ALIGN_CENTER)
    NAP_EDIT_TEXT(V_Edit1, "O campo 2 é read-write")
    NAP_EDIT_TEXT(V_Edit2, "só se o campo 1 estiver vazio")
    NAP_EDIT_ONCHANGE(V_Edit2, {|| CONCLUIR_ENTRADA(V_Layout2) })

    // Widget disposition in layouts
    NAP_LAYOUT_LABEL(V_Layout1, V_Label1, 0, 0)
    NAP_LAYOUT_LABEL(V_Layout2, V_ELabel1, 0, 0)
    NAP_LAYOUT_LABEL(V_Layout2, V_ELabel2, 0, 1)
    NAP_LAYOUT_EDIT(V_Layout2, V_Edit1, 1, 0)
    NAP_LAYOUT_EDIT(V_Layout2, V_Edit2, 1, 1)
    NAP_LAYOUT_LAYOUT(V_Layout1, V_Layout2, 0, 1)

    // Margins and alignment
    // Main label is horizontally centered with image-menu
    NAP_LAYOUT_HALIGN(V_Layout1, 0, 0, ekNAP_ALIGN_CENTER)
    // Force the EditBox column to 400 px
    NAP_LAYOUT_HSIZE(V_Layout2, 1, 400)
    // Horizontal space between labels and edits
    NAP_LAYOUT_HMARGIN(V_Layout2, 0, 40)
    // Vertical space between edits
    NAP_LAYOUT_VMARGIN(V_Layout2, 0, 10)
    // Global border margin
    NAP_LAYOUT_MARGIN4(V_Layout1, 0, 120, 250, 50)

    // Adding toolbar
    V_Layout3 := LAYOUT_WITH_TOOLBAR(V_Layout1, {"recortar.bmp", "copia.bmp", "colar.bmp", "desfazer.bmp", "calcula.bmp", "ajuda.bmp", "saida.bmp"}, {"Recortar", "Copiar", "Colar", "Desfazer", "Calculadora", "Ajuda", "Saida"})

    // Configure window and launch
    NAP_PANEL_LAYOUT(V_Panel, V_Layout3)
    NAP_WINDOW_PANEL(V_Janela, V_Panel)
    NAP_WINDOW_TITLE(V_Janela, "Teste de entrada de dados")
    NAP_LAYOUT_FOCUS(V_Layout2, 1, 0)
    NAP_WINDOW_MODAL(V_Janela)
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE CONCLUIR_ENTRADA(V_Layout2)
    LOCAL N_Opt := PERGUN("Dados estao corretos?", {"Sim", "Não"}, 1, .F., "Confirmação", "quest")

    IF N_Opt == 1
        MOSTRAR("M?????", "Entrada de dados finalizada com sucesso", "Informação")
        NAP_LAYOUT_FOCUS(V_Layout2, 1, 0)
    ENDIF

    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_ENTRADA_DADOS_DATA_INVALIDA()
    LOCAL V_Janela, V_Panel, V_Layout1, V_Layout2, V_Layout3
    LOCAL V_Label1
    LOCAL V_ELabel1
    LOCAL V_Edit1
    LOCAL C_String

    C_String := "Teste de entrada de dados" + hb_eol() + "com data invalida"
    V_Janela := NAP_WINDOW_CREATE(ekNAP_WINDOW_STD + ekNAP_WINDOW_ESC)
    V_Panel := NAP_PANEL_CREATE()
    V_Layout1 := NAP_LAYOUT_CREATE(1, 2)
    V_Layout2 := NAP_LAYOUT_CREATE(2, 2)
    V_Label1 := NAP_LABEL_MULTILINE()
    V_ELabel1 := NAP_LABEL_WITH_TEXT("Informe a data 29/02/2001")
    V_Edit1 := NAP_EDIT_CREATE()

    // Widget configuration
    NAP_LABEL_TEXT(V_Label1, C_String)
    NAP_LABEL_ALIGN(V_Label1, ekNAP_ALIGN_CENTER)
    NAP_EDIT_ONCHANGE(V_Edit1, {|| VERIFICAR_DATA(V_Edit1) })

    // Widget disposition in layouts
    NAP_LAYOUT_LABEL(V_Layout1, V_Label1, 0, 0)
    NAP_LAYOUT_LABEL(V_Layout2, V_ELabel1, 0, 0)
    NAP_LAYOUT_EDIT(V_Layout2, V_Edit1, 1, 0)
    NAP_LAYOUT_LAYOUT(V_Layout1, V_Layout2, 0, 1)

    // Margins and alignment
    // Main label is horizontally centered with image-menu
    NAP_LAYOUT_HALIGN(V_Layout1, 0, 0, ekNAP_ALIGN_CENTER)
    // Force the EditBox column to 400 px
    NAP_LAYOUT_HSIZE(V_Layout2, 1, 400)
    // Horizontal space between labels and edits
    NAP_LAYOUT_HMARGIN(V_Layout2, 0, 40)
    // Global border margin
    NAP_LAYOUT_MARGIN4(V_Layout1, 0, 120, 250, 50)

    // Adding toolbar
    V_Layout3 := LAYOUT_WITH_TOOLBAR(V_Layout1, {"recortar.bmp", "copia.bmp", "colar.bmp", "desfazer.bmp", "calcula.bmp", "ajuda.bmp", "saida.bmp"}, {"Recortar", "Copiar", "Colar", "Desfazer", "Calculadora", "Ajuda", "Saida"})

    // Configure window and launch
    NAP_PANEL_LAYOUT(V_Panel, V_Layout3)
    NAP_WINDOW_PANEL(V_Janela, V_Panel)
    NAP_WINDOW_TITLE(V_Janela, "Teste de entrada de dados")
    NAP_LAYOUT_FOCUS(V_Layout2, 1, 0)
    NAP_WINDOW_MODAL(V_Janela)
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE VERIFICAR_DATA(V_Edit1)
    LOCAL C_Text := NAP_EDIT_GET_TEXT(V_Edit1)
    MOSTRAR("M?????", C_Text, "Informação")
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_ENTRADA_DADOS_FILTRO_TECLAS()
    LOCAL V_Janela, V_Panel, V_Layout1, V_Layout2
    LOCAL V_Label1
    LOCAL V_ELabel1
    LOCAL V_Edit1
    LOCAL C_String

    C_String := "Teste de entrada de dados" + hb_eol() + "com filtro de teclas"
    V_Janela := NAP_WINDOW_CREATE(ekNAP_WINDOW_STD + ekNAP_WINDOW_ESC)
    V_Panel := NAP_PANEL_CREATE()
    V_Layout1 := NAP_LAYOUT_CREATE(1, 2)
    V_Layout2 := NAP_LAYOUT_CREATE(2, 2)
    V_Label1 := NAP_LABEL_MULTILINE()
    V_ELabel1 := NAP_LABEL_WITH_TEXT("Só maiúsculo")
    V_Edit1 := NAP_EDIT_CREATE()

    // Widget configuration
    NAP_LABEL_TEXT(V_Label1, C_String)
    NAP_LABEL_ALIGN(V_Label1, ekNAP_ALIGN_CENTER)
    NAP_EDIT_TEXT(V_Edit1, "APENAS ACEITAR MAIÚSCULAS")
    NAP_EDIT_ONFILTER(V_Edit1, {|| VERIFICAR_MAIUSCULAS(V_Edit1) })

    // Widget disposition in layouts
    NAP_LAYOUT_LABEL(V_Layout1, V_Label1, 0, 0)
    NAP_LAYOUT_LABEL(V_Layout2, V_ELabel1, 0, 0)
    NAP_LAYOUT_EDIT(V_Layout2, V_Edit1, 1, 0)
    NAP_LAYOUT_LAYOUT(V_Layout1, V_Layout2, 0, 1)

    // Margins and alignment
    // Main label is horizontally centered with image-menu
    NAP_LAYOUT_HALIGN(V_Layout1, 0, 0, ekNAP_ALIGN_CENTER)
    // Force the EditBox column to 400 px
    NAP_LAYOUT_HSIZE(V_Layout2, 1, 400)
    // Horizontal space between labels and edits
    NAP_LAYOUT_HMARGIN(V_Layout2, 0, 40)
    // Global border margin
    NAP_LAYOUT_MARGIN4(V_Layout1, 0, 120, 250, 50)

    // Configure window and launch
    NAP_PANEL_LAYOUT(V_Panel, V_Layout1)
    NAP_WINDOW_PANEL(V_Janela, V_Panel)
    NAP_WINDOW_TITLE(V_Janela, "Teste de entrada de dados")
    NAP_LAYOUT_FOCUS(V_Layout2, 1, 0)
    NAP_WINDOW_MODAL(V_Janela)
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE VERIFICAR_MAIUSCULAS(V_Edit1)
    // LOCAL C_Text := NAP_EDIT_GET_TEXT(V_Edit1)
    // MOSTRAR("M?????", C_Text, "Informação")
    RETURN

/*---------------------------------------------------------------------------*/

STATIC FUNCTION DADA_PANEL(N_Fields)
    LOCAL V_Panel, V_Layout1
    LOCAL V_Label1, V_Label2, V_Edit1
    LOCAL N_Cont

    V_Layout1 := NAP_LAYOUT_CREATE(2, 2 * N_Fields)
    NAP_LAYOUT_HSIZE(V_Layout1, 1, 300)

    FOR N_Cont := 1 TO N_Fields
        V_Label1 := NAP_LABEL_WITH_TEXT("Campo " + hb_ntos(N_Cont))
        V_Label2 := NAP_LABEL_CREATE()
        V_Edit1 := NAP_EDIT_CREATE()
        NAP_EDIT_TEXT(V_Edit1, "Valor " + hb_ntos(N_Cont))
        NAP_LABEL_TEXT(V_Label2, "Valor " + hb_ntos(N_Cont))
        NAP_LAYOUT_LABEL(V_Layout1, V_Label1, 0, (N_Cont - 1) * 2)
        NAP_LAYOUT_EDIT(V_Layout1, V_Edit1, 1, (N_Cont - 1) * 2)
        NAP_LAYOUT_LABEL(V_Layout1, V_Label2, 1, ((N_Cont - 1) * 2) + 1)

        IF N_Cont < N_Fields
            NAP_LAYOUT_VMARGIN(V_Layout1, ((N_Cont - 1) * 2) + 1, 20)
        ENDIF
    NEXT

    NAP_LAYOUT_HMARGIN(V_Layout1, 0, 20)
    NAP_LAYOUT_MARGIN4(V_Layout1, 0, 20, 0, 0)
    V_Panel := NAP_PANEL_SCROLL(.F., .T.)
    NAP_PANEL_LAYOUT(V_Panel, V_Layout1)
    RETURN V_Panel

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_ENTRADA_DADOS_COM_ROLAMENTO()
    LOCAL V_Janela, V_Panel, V_Layout1, V_Layout2
    LOCAL V_Label1
    LOCAL V_DadaPanel
    LOCAL V_Edit1
    LOCAL C_String

    C_String := "Teste de entrada de dados" + hb_eol() + "com rolamento"
    V_Janela := NAP_WINDOW_CREATE(ekNAP_WINDOW_STD + ekNAP_WINDOW_ESC)
    V_Panel := NAP_PANEL_CREATE()
    V_Layout1 := NAP_LAYOUT_CREATE(1, 2)
    V_Label1 := NAP_LABEL_MULTILINE()
    V_DadaPanel := DADA_PANEL(10)

    // Widget configuration
    NAP_LABEL_TEXT(V_Label1, C_String)
    NAP_LABEL_ALIGN(V_Label1, ekNAP_ALIGN_CENTER)

    // Widget disposition in layouts
    NAP_LAYOUT_LABEL(V_Layout1, V_Label1, 0, 0)
    NAP_LAYOUT_PANEL(V_Layout1, V_DadaPanel, 0, 1)

    // Margins and alignment
    // Main label is horizontally centered with image-menu
    NAP_LAYOUT_HALIGN(V_Layout1, 0, 0, ekNAP_ALIGN_CENTER)
    // Global border margin
    NAP_LAYOUT_MARGIN4(V_Layout1, 0, 50, 100, 50)

    // Adding toolbar
    V_Layout2 := LAYOUT_WITH_TOOLBAR(V_Layout1, {"recortar.bmp", "copia.bmp", "colar.bmp", "desfazer.bmp", "calcula.bmp", "ajuda.bmp", "saida.bmp"}, {"Recortar", "Copiar", "Colar", "Desfazer", "Calculadora", "Ajuda", "Saida"})

    // Configure window and launch
    NAP_PANEL_LAYOUT(V_Panel, V_Layout2)
    NAP_WINDOW_PANEL(V_Janela, V_Panel)
    NAP_WINDOW_TITLE(V_Janela, "Teste de entrada de dados")
    NAP_WINDOW_MODAL(V_Janela)
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_ENTRADA_DADOS_CORES_MENSAGENS()
    MOSTRAR("M?????", "Entrada de dados com cores e mensagens", "Informação")
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_ENTRADA_DADOS_VALID()
    MOSTRAR("M?????", "Entrada de dados com valids e campo memo", "Informação")
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_ENTRADA_DADOS_LISTA_AUTO()
    MOSTRAR("M?????", "Entrada de dados com 'lista' e 'auto'", "Informação")
    RETURN
