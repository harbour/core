/* encoding: cp850 */
#INCLUDE "inkey.ch"
#INCLUDE "cua.ch"

***********************
PROC EXEMPLO_AUXILIARES
***********************
LOCAL V_Janela
*
CUA20 @ 15,20,30,70 JANELA V_Janela ;
     TITULO "Escolha o tipo de janela auxiliar" SUBTITULO "%T";
     AJUDA "T?????"
*
ESPECIALIZE V_Janela MENU
ADDOPCAO V_Janela TEXTO "janela de #informação (com parada)" ;
   ACAO TST_INFORMACAO_COM_PARADA() AJUDA "P06685"
ADDOPCAO V_Janela TEXTO "janela de informação (#sem parada)" ;
   ACAO TST_INFORMACAO_SEM_PARADA() AJUDA "P06687"
ADDOPCAO V_Janela TEXTO "janela de #advertência" ;
   ACAO TST_ADVERTENCIA() AJUDA "P06689"
ADDOPCAO V_Janela TEXTO "janela de #erro" ;
   ACAO TST_ERRO() AJUDA "P06691"
ADDOPCAO V_Janela TEXTO "janela de #confirmação" ;
   ACAO TST_CONFIRMACAO() AJUDA "P06693"
ADDOPCAO V_Janela TEXTO "janela de #pergunta" ;
   ACAO TST_PERGUNTA() AJUDA "P06695"
ADDOPCAO V_Janela TEXTO "janela sem especiali#zação" ;
   ACAO TST_SEM_ESPECIALIZACAO() AJUDA "P21067"
*
ATIVE(V_Janela)
*

// STAT PROC TST_INFORMACAO_COM_PARADA()
// RETURN

STAT PROC TST_INFORMACAO_SEM_PARADA()
RETURN

// STAT PROC TST_ADVERTENCIA()
// RETURN

// STAT PROC TST_ERRO()
// RETURN

// STAT PROC TST_CONFIRMACAO()
// RETURN

// STAT PROC TST_PERGUNTA()
// RETURN

// STAT PROC TST_SEM_ESPECIALIZACAO()
// RETURN

***********************************
STAT PROC TST_INFORMACAO_COM_PARADA
***********************************
MOSTRAR("M15566","Teste de janela; de informação; com parada")
*
// ***********************************
// STAT PROC TST_INFORMACAO_SEM_PARADA
// ***********************************
// LOCAL V_Janela
// *
// V_Janela := MSGAGUARDE("M?????",,"Teste de janela; de informação; sem parada...",.T.)
// *
// INKEY(3)
// TONE(200,2)
// MUDE SUBTITULO V_Janela PARA "Teste de ;mudança do ;subtítulo"+";;(M15568)"
// INKEY(2)
// TONE(200,2)
// *
// FECHAR MSGAGUARDE V_Janela
*
*************************
STAT PROC TST_ADVERTENCIA
*************************
ADVERTE("M?????","Teste de janela; de advertência")
*
******************
STAT PROC TST_ERRO
******************
ALARME("M?????","Teste de janela; de erro")
*
*************************
STAT PROC TST_CONFIRMACAO
*************************
IF CONFIRME("Teste de rotina de conformação.;Escolha entre 'sim' e não'",2)
   MOSTRAR("M15570","Foi escolhido 'sim'")
ELSE
   MOSTRAR("M15572","Foi escolhido 'não'")
ENDIF
*
**********************
STAT PROC TST_PERGUNTA
**********************
LOCAL N_Opcao
N_Opcao := PERGUN("Teste de rotina de pergunta; ao usuário",;
               {"OK","Cancelar","Desistir"},3,,"Favor responder à pergunta")
*
MOSTRAR("M15574","A opção escolhida foi '"+STR(N_Opcao,1)+"'")
*
********************************
STAT PROC TST_SEM_ESPECIALIZACAO()
********************************
LOCAL V_Janela
*
CUA20 @ 10,35,MAXROW()-5,MAXCOL()-10 JANELA V_Janela ;
    TITU "Janela sem especialização" ;
    SUBTITULO "%T" ;
    AJUDA "T?????"
ADDBOTAO V_Janela TEXTO "1=exibe o número 1 na tela" ;
   ACAO MOSTRAR("M24826","O usuário digitou '1'") AJUDA "B19265"
ADDBOTAO V_Janela TEXTO "2=exibe o número 2 na tela e fecha janela" ;
   ACAO (MOSTRAR("M24826","O usuário digitou '2'"),.T.) AUTOCLOSE AJUDA "B19265"

CUA20 ADDIMAGEM V_Janela ARQUIVO DIRET_BMPS()+"logaspec.bmp"  ;
     COORDENADAS 01,01,03,05 ;
     ACAO MOSTRAR("M24826","O usuário clicou na imagem") AJUDA "B19129"

ADDACAO V_Janela INKEY K_F6 ;
   ACAO MOSTRAR("M24826","O usuário teclou F6") AJUDA "B19117"

AJUSTA_BOTOES(V_Janela)

ATIVE(V_Janela)
*
************************

