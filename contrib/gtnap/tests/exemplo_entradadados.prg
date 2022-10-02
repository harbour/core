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
    NAP_MENUVERT_ADD(V_MenuVert, "Entrada de dados com rolamento vertial", {|| TST_ENTRADA_DADOS_COM_ROLAMENTO() })
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
    MOSTRAR("M?????", "Entrada de dados (read-only)", "Informação")
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_ENTRADA_DADOS_COM_CAMPO_READ_ONLY()
    MOSTRAR("M?????", "Entrada de dados com campo read-only", "Informação")
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_ENTRADA_DADOS_COM_CONFIRMACOES()
    MOSTRAR("M?????", "Entrada de dados com confirmações", "Informação")
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_ENTRADA_DADOS_DATA_INVALIDA()
    MOSTRAR("M?????", "Entrada de dados com data inválida", "Informação")
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_ENTRADA_DADOS_FILTRO_TECLAS()
    MOSTRAR("M?????", "Entrada de dados com filtro de tecla", "Informação")
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_ENTRADA_DADOS_COM_ROLAMENTO()
    MOSTRAR("M?????", "Entrada de dados com rolamento vertial", "Informação")
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
