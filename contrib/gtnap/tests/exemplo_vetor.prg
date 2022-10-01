#require "gtnap"
#include "gtnap.ch"

/*---------------------------------------------------------------------------*/

PROC EXEMPLO_BROWSE_VETOR()
    LOCAL V_Janela, V_Panel, V_Layout1
    LOCAL V_MenuVert
    LOCAL V_Label1

    V_Janela := NAP_WINDOW_CREATE(ekNAP_WINDOW_STD + ekNAP_WINDOW_ESC)
    V_Panel := NAP_PANEL_CREATE()
    V_Layout1 := NAP_LAYOUT_CREATE(1, 2)
    V_MenuVert := NAP_MENUVERT_CREATE()
    V_Label1 := NAP_LABEL_WITH_TEXT("Escolha o tipo de janela de browse de vetor")
    NAP_MENUVERT_ADD(V_MenuVert, "Janela seleção simples, sem grade, nem toolbar", {|| TST_BROWSE_VETOR_SIMPLES_SEM_GRADE_SEM_TOOLBAR_SEM_ROLAGEM() })
    NAP_MENUVERT_ADD(V_MenuVert, "Janela seleção múltipla, com grade, com toolbar, e rolagem", {|| TST_BROWSE_VETOR_MULTIPLA_COM_GRADE_COM_TOOLBAR_COM_ROLAGEM() })
    NAP_MENUVERT_ADD(V_MenuVert, "Janela seleção estendida, com grade, sem barra de rolagem", {|| TST_BROWSE_VETOR_ESTENDIDA_COM_GRADE_COM_TOOLBAR_SEM_ROLAGEM() })
    NAP_MENUVERT_ADD(V_MenuVert, "Janela seleção simples com AUTOCLOSE", {|| TST_BROWSE_VETOR_SIMPLES_COM_AUTOCLOSE() })
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

STATIC PROCEDURE TST_BROWSE_VETOR_SIMPLES_SEM_GRADE_SEM_TOOLBAR_SEM_ROLAGEM()
    MOSTRAR("M?????", "Janela seleção simples, sem grade, nem toolbar", "Informação")
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_BROWSE_VETOR_MULTIPLA_COM_GRADE_COM_TOOLBAR_COM_ROLAGEM()
    MOSTRAR("M?????", "Janela seleção múltipla, com grade, com toolbar, e rolagem", "Informação")
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_BROWSE_VETOR_ESTENDIDA_COM_GRADE_COM_TOOLBAR_SEM_ROLAGEM()
    MOSTRAR("M?????", "Janela seleção estendida, com grade, sem barra de rolagem", "Informação")
    RETURN

/*---------------------------------------------------------------------------*/

STATIC PROCEDURE TST_BROWSE_VETOR_SIMPLES_COM_AUTOCLOSE()
    MOSTRAR("M?????", "Janela seleção simples com AUTOCLOSE", "Informação")
    RETURN

