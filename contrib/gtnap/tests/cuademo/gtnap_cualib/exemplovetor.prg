/* encoding: cp850 */
#INCLUDE "inkey.ch"
#INCLUDE "cua.ch"

*************************
PROC EXEMPLO_BROWSE_VETOR
*************************
LOCAL V_Janela
*
CUA20 @ 15,20,27,85 JANELA V_Janela ;
     TITULO "Escolha o tipo de janela de browse de vetor" SUBTITULO "%T";
     AJUDA "T?????"
*
ESPECIALIZE V_Janela MENU
ADDOPCAO V_Janela TEXTO "janela seleção simples, sem grade, nem toolbar" ;
   ACAO TST_BROWSE_VETOR_SIMPLES_SEM_GRADE_SEM_TOOLBAR_SEM_ROLAGEM() AJUDA "P06749"
ADDOPCAO V_Janela TEXTO "janela seleção múltipla, com grade, com toolbar, e rolagem" ;
    ACAO TST_BROWSE_VETOR_MULTIPLA_COM_GRADE_COM_TOOLBAR_COM_ROLAGEM() AJUDA "P06751"
ADDOPCAO V_Janela TEXTO "janela seleção estendida, com grade, sem barra de rolagem" ;
   ACAO TST_BROWSE_VETOR_ESTENDIDA_COM_GRADE_COM_TOOLBAR_SEM_ROLAGEM() AJUDA "P06753"
ADDOPCAO V_Janela TEXTO "janela seleção simples com AUTOCLOSE" ;
   ACAO TST_BROWSE_VETOR_SIMPLES_COM_AUTOCLOSE() AJUDA "P06755"
*
ATIVE(V_Janela)
*

// STAT PROC TST_BROWSE_VETOR_SIMPLES_SEM_GRADE_SEM_TOOLBAR_SEM_ROLAGEM
// RETURN

*******************************************************************
STAT PROC TST_BROWSE_VETOR_SIMPLES_SEM_GRADE_SEM_TOOLBAR_SEM_ROLAGEM
*******************************************************************
LOCAL V_Janela, V_Vetor

V_Vetor := {"#Leite condensado","A#rroz tipo 1","Ac#arajé","Doc#e de leite",;
            "Doce#s diversos","Churrasco de #gado","Rapadura #preta",;
            "Amendoim #torrado","Panelada de #bucho"}
*
CUA20 @ 06,64,MAXROW()-4,MAXCOL()-5 JANELA V_Janela ;
    TITU "Browse de vetor" ;
    SUBTITULO "%T;sem grade, sem toolbar;sem barra de rolagem" ;
    AJUDA "T?????"

ADDBOTAO V_Janela TEXTO "F5=exibir selecionados" ;
   ACAO EXIBIR_ITEM_SELECIONADO(V_Janela,V_Vetor) AJUDA "B19277"

CUA20 ADDIMAGEM V_Janela ARQUIVO DIRET_BMPS()+"logaspec.bmp"  ;
  COORDENADAS 04,04,06,08 ACAO MOSTRAR("M?????","Selecionou a imagem") AJUDA "B19133"

ADDACAO V_Janela INKEY K_F6 ACAO MOSTRAR("M?????","Teclou F6") AJUDA "B19121"

CUA20 ESPECIALIZE V_Janela SELECAO SIMPLES VETOR V_Vetor NAOROLAVERTICAL ;
   NAOROLAHORIZONTAL SEMGRADE SEMTOOLBAR

ATIVE(V_Janela)
*
*********************************
STAT PROC EXIBIR_ITEM_SELECIONADO(V_Janela,V_Vetor)
*********************************
LOCAL N_Posicao := ITENSSELECIONADOS(V_Janela)
*
MOSTRAR("M15664","A posição selecionada foi "+LTRIM(STR(N_Posicao)))
MOSTRAR("M15666","A posição selecionada contém '"+V_Vetor[N_Posicao]+"'")
*

// STAT PROC TST_BROWSE_VETOR_MULTIPLA_COM_GRADE_COM_TOOLBAR_COM_ROLAGEM
//     RETURN

********************************************************************
STAT PROC TST_BROWSE_VETOR_MULTIPLA_COM_GRADE_COM_TOOLBAR_COM_ROLAGEM
********************************************************************
LOCAL V_Janela, V_Vetor, V_Posicoes

V_Vetor := {"#Leite condensado","A#rroz tipo 1","Ac#arajé","Doc#e de leite",;
            "Doce#s diversos","Churrasco de #gado","Rapadura #preta",;
            "Amendoim #torrado","Panelada de #bucho"}
*
CUA20 @ 15,64,MAXROW()-5,MAXCOL()-5 JANELA V_Janela ;
    TITU "Browse de vetor" ;
    SUBTITULO "%T;com grade, sem toolbar;com barra de rolagem" ;
    AJUDA "T?????"
ADDBOTAO V_Janela TEXTO "Enter=exibir itens selecionados" ;
    ACAO EXIBIR_ITENS_SELECIONADOS(V_Janela,V_Vetor) AUTOCLOSE AJUDA "B19279"

ADDBOTAO V_Janela TEXTO "V=voltar seleção para o 'default'" ;
   ACAO DEFAULT_SELECIONADOS(V_Janela) AJUDA "B19281"

CUA20 ESPECIALIZE V_Janela SELECAO MULTIPLA VETOR V_Vetor NAOROLAHORIZONTAL

MUDE SELECAO V_Janela PARA {2,4}

V_Posicoes := ATIVE(V_Janela)
*
// IF LEN(V_Posicoes) == 0
//    MOSTRAR("M15668","Não foi selecionada nenhuma posição")
// ENDIF
*
***********************************
STAT PROC EXIBIR_ITENS_SELECIONADOS(V_Janela,V_Vetor)
***********************************
LOCAL V_Posicoes := ITENSSELECIONADOS(V_Janela)
*
MOSTRAR("M15670","Foram selecionadas "+LTRIM(STR(LEN(V_Posicoes)))+" posições")
*
IF LEN(V_Posicoes) > 0
   MOSTRAR("M15672","A primeira posição selecionada contém '"+;
           V_Vetor[V_Posicoes[1]]+"'")
ENDIF
*
*********************************
STAT PROC DEFAULT_SELECIONADOS(V_Janela)
*********************************
MUDE SELECAO V_Janela PARA {2,4}    // registros pré-selecionados
*

STAT PROC TST_BROWSE_VETOR_ESTENDIDA_COM_GRADE_COM_TOOLBAR_SEM_ROLAGEM
    RETURN

// ********************************************************************
// STAT PROC TST_BROWSE_VETOR_ESTENDIDA_COM_GRADE_COM_TOOLBAR_SEM_ROLAGEM
// ********************************************************************
// LOCAL V_Janela, V_Vetor

// V_Vetor := {"Leite condensado","Arroz tipo 1","Acarajé","Doce de leite",;
//             "Doces diversos","Churrasco de gado","Rapadura preta",;
//             "Amendoim torrado","Panelada de bucho"}
// *
// CUA20 @ 06,64,MAXROW()-5,MAXCOL()-5 JANELA V_Janela ;
//     TITU "Browse de vetor" ;
//     SUBTITULO "%T;com grade, com toolbar;sem barra de rolagem" ;
//     AJUDA "T?????"
// *
// ADDBOTAO V_Janela TEXTO "Enter=exibir itens selecionados" ;
//     ACAO EXIBIR_ITENS_SELECIONADOS(V_Janela,V_Vetor) AJUDA "B19283"

// CUA20 ESPECIALIZE V_Janela SELECAO ESTENDIDA VETOR V_Vetor ;
//       NAOROLAVERTICAL NAOROLAHORIZONTAL
// *
// ATIVE(V_Janela)
*

STAT PROC TST_BROWSE_VETOR_SIMPLES_COM_AUTOCLOSE
RETURN

// ************************************************
// STAT PROC TST_BROWSE_VETOR_SIMPLES_COM_AUTOCLOSE
// ************************************************
// LOCAL V_Janela, V_Vetor, N_Posicao

// V_Vetor := {"Leite condensado","Arroz tipo 1","Acarajé","Doce de leite",;
//             "Doces diversos","Churrasco de gado","Rapadura preta",;
//             "Amendoim torrado","Panelada de bucho"}
// *
// CUA20 @ 06,64,MAXROW()-5,MAXCOL()-5 JANELA V_Janela ;
//     TITU "Browse de vetor" ;
//     SUBTITULO "%T;com 'autoclose'" ;
//     AJUDA "T?????"
// *
// ADDBOTAO V_Janela TEXTO "F5=exibir selecionados" ;
//    ACAO (EXIBIR_ITEM_SELECIONADO(V_Janela,V_Vetor),.T.) AUTOCLOSE AJUDA "B19285"

// CUA20 ADDIMAGEM V_Janela ARQUIVO DIRET_BMPS()+"logaspec.bmp"  ;
//   COORDENADAS 04,04,06,08 ;
//   ACAO (MOSTRAR("M?????","Selecionou a imagem"),.T.) AUTOCLOSE AJUDA "B19135"

// ADDACAO V_Janela INKEY K_F6 ;
//   ACAO (MOSTRAR("M?????","Teclou F6"),.T.) AUTOCLOSE AJUDA "B19123"

// CUA20 ESPECIALIZE V_Janela SELECAO SIMPLES VETOR V_Vetor ;
//    NAOROLAVERTICAL NAOROLAHORIZONTAL SEMGRADE SEMTOOLBAR AUTOCLOSE
// *
// N_Posicao := ATIVE(V_Janela)
// *
// MOSTRAR("M15668","Foi selecionada posição "+LTRIM(STR(N_Posicao)))
// *
// *********************


