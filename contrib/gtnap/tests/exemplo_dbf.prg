#require "gtnap"
#include "gtnap.ch"

PROC EXEMPLO_BROWSE_DBF()
    LOCAL V_Janela, V_Panel, V_Layout1
    LOCAL V_MenuVert
    LOCAL V_Label1, V_Label2, V_Label3
    V_Janela := NAP_WINDOW_CREATE(ekNAP_WINDOW_STD + ekNAP_WINDOW_ESC)
    V_Panel := NAP_PANEL_CREATE()
    V_Layout1 := NAP_LAYOUT_CREATE(1, 2)
    V_MenuVert := NAP_MENUVERT_CREATE()
    V_Label1 := NAP_LABEL_WITH_TEXT("Escolha o tipo de janela de browse de DBF")
    NAP_MENUVERT_ADD(V_MenuVert, "Janela seleção simples, com grid, com toolbar", {|| TST_BROWSE_DBF_SIMPLES_COM_GRID_COM_TOOLBAR() })
    NAP_MENUVERT_ADD(V_MenuVert, "Janela seleção múltipla, sem grid, nem toolbar", {|| TST_BROWSE_DBF_MULTIPLA_SEM_GRID_SEM_TOOLBAR() })
    NAP_MENUVERT_ADD(V_MenuVert, "Janela seleção estendida, com coluna congelada", {|| TST_BROWSE_DBF_COLUNA_CONGELADA() })
    NAP_MENUVERT_ADD(V_MenuVert, "Janela seleção, com 'while', sem barras de rolagem", {|| TST_BROWSE_DBF_WHILE() })
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

STATIC PROCEDURE TST_BROWSE_DBF_SIMPLES_COM_GRID_COM_TOOLBAR()
    MOSTRAR("M?????", "Janela seleção simples, com grid, com toolbar", "Informação")
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_BROWSE_DBF_MULTIPLA_SEM_GRID_SEM_TOOLBAR()
    MOSTRAR("M?????", "Janela seleção múltipla, sem grid, nem toolbar", "Informação")
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_BROWSE_DBF_COLUNA_CONGELADA()
    MOSTRAR("M?????", "Janela seleção estendida, com coluna congelada", "Informação")
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_BROWSE_DBF_WHILE()
    MOSTRAR("M?????", "Janela seleção, com 'while', sem barras de rolagem", "Informação")
    RETURN
