/* encoding: cp850 */
#INCLUDE "inkey.ch"
#INCLUDE "cua.ch"

#INCLUDE "def_dados.ch"

**************************
PROC EXEMPLO_ENTRADA_DADOS
**************************
LOCAL V_Janela

CUA20 @ 08,20,25,80 JANELA V_Janela ;
     TITULO "Escolha o tipo de janela" SUBTITULO "%T;entrada de dados";
     AJUDA "T?????"

ESPECIALIZE V_Janela MENU
ADDOPCAO V_Janela TEXTO "entrada de dados (read-only)" ;
   ACAO TST_ENTRADA_DADOS_TODA_READ_ONLY() AJUDA "P06705"
ADDOPCAO V_Janela TEXTO "entrada de dados com campo read-only" ;
   ACAO TST_ENTRADA_DADOS_COM_CAMPO_READ_ONLY() AJUDA "P06707"
ADDOPCAO V_Janela TEXTO "entrada de dados com confirmações" ;
   ACAO TST_ENTRADA_DADOS_COM_CONFIRMACOES() AJUDA "P06709"
ADDOPCAO V_Janela TEXTO "entrada de dados com data inválida" ;
   ACAO TST_ENTRADA_DADOS_DATA_INVALIDA() AJUDA "P06711"
ADDOPCAO V_Janela TEXTO "entrada de dados com filtro de tecla" ;
   ACAO TST_ENTRADA_DADOS_FILTRO_TECLAS() AJUDA "P06715"
ADDOPCAO V_Janela TEXTO "entrada de dados com rolamento vertical" ;
   ACAO TST_ENTRADA_DADOS_COM_ROLAMENTO() AJUDA "P06717"
ADDOPCAO V_Janela TEXTO "entrada de dados com cores e mensagens" ;
   ACAO TST_ENTRADA_DADOS_CORES_MENSAGENS() AJUDA "P06719"
ADDOPCAO V_Janela TEXTO "entrada de dados com valids e campo memo" ;
   ACAO TST_ENTRADA_DADOS_VALID() AJUDA "P06721"
ADDOPCAO V_Janela TEXTO "entrada de dados com 'lista' e 'auto' " ;
   ACAO TST_ENTRADA_DADOS_LISTA_AUTO() AJUDA "P06713"
ADDOPCAO V_Janela TEXTO "entrada de dados com pedejan" ;
   ACAO TST_ENTRADA_DADOS_PEDEJAN() AJUDA "P35035"
/*
ADDOPCAO V_Janela TEXTO "entrada de dados com 'domínio" ;
   ACAO TST_ENTRADA_DADOS_DOMINIO() AJUDA "P35035"
ADDOPCAO V_Janela TEXTO "entrada de dados com 'campo' " ;
   ACAO TST_ENTRADA_DADOS_CAMPO() AJUDA "P35037"
*/
*
ATIVE(V_Janela)


************************************************
STAT PROC TST_ENTRADA_DADOS_TODA_READ_ONLY
************************************************
LOCAL L_Ok, V_Janela
LOCAL C_Campo1 := "Este campo é read-only."
LOCAL C_Campo2 := "Este campo é read-only."
*
@ 07,43,23,85 JANELA V_Janela  ;
   TITULO "Teste de entrada de dados" ;
   SUBTITULO "%T;toda read-only" ;
   TECLAS {} AJUDA "T?????"

ESPECIALIZE V_Janela ENTRADA EDITA .F.
*
@ V_Janela,00,02 SAY "Campo 1" GET C_Campo1 AJUDA "C?????"
@ V_Janela,01,02 SAY "Campo 2" GET C_Campo2 AJUDA "C?????"

L_Ok := ATIVE(V_Janela)
DO WHILE L_Ok
   MOSTRAR("M15584","Entrada de dados finalizada com sucesso.")
   L_Ok := ATIVE(V_Janela)
ENDDO
DESTRUA V_Janela

*****************************************************
STAT PROC TST_ENTRADA_DADOS_COM_CAMPO_READ_ONLY
*****************************************************
LOCAL L_Ok, V_Janela
LOCAL C_Campo1 := "O campo 2 é read-write       "
LOCAL C_Campo2 := "só se o campo 1 estiver vazio"
*
@ 07,43,23,90 JANELA V_Janela  ;
   TITULO "Teste de entrada de dados" ;
   SUBTITULO "%T;com campo read-only" ;
   TECLAS {} AJUDA "T?????"

ESPECIALIZE V_Janela ENTRADA
*
@ V_Janela,00,02 SAY "Campo 1" GET C_Campo1 AJUDA "C?????"
@ V_Janela,01,02 SAY "Campo 2" GET C_Campo2 EDITA EMPTY(C_Campo1) AJUDA "C?????"

L_Ok := ATIVE(V_Janela)
DO WHILE L_Ok
   MOSTRAR("M15586","Entrada de dados finalizada com sucesso:; C_Campo1: '" + C_Campo1 + "';C_Campo2: '" + C_Campo2 + "'")
   L_Ok := ATIVE(V_Janela)
ENDDO
DESTRUA V_Janela
*
********************************************
STAT PROC TST_ENTRADA_DADOS_COM_CONFIRMACOES
********************************************
LOCAL L_Ok, V_Janela
LOCAL C_Campo1 := "                                       "
LOCAL C_Campo2 := "                                       "
*
@ 07,33,23,90 JANELA V_Janela  ;
   TITULO "Teste de entrada de dados" ;
   SUBTITULO "%T;com confirmações" ;
   TECLAS {} AJUDA "T?????"

ESPECIALIZE V_Janela ENTRADA ;
    CONFIRMAR CONFIRMA_DADOS() ;
    DESISTIR CONFIRMA_CANCELAMENTO()
*
@ V_Janela,00,02 SAY "Campo 1" GET C_Campo1 AJUDA "C?????"
@ V_Janela,01,02 SAY "Campo 2" GET C_Campo2 AJUDA "C?????"

L_Ok := ATIVE(V_Janela)
DO WHILE L_Ok
   MOSTRAR("M15588","Entrada de dados finalizada com sucesso.")
   L_Ok := ATIVE(V_Janela)
ENDDO
DESTRUA V_Janela
*
*****************************************
STAT PROC TST_ENTRADA_DADOS_DATA_INVALIDA
*****************************************
LOCAL L_Ok, V_Janela
LOCAL D_Data := CTOD("")
*
@ 07,43,23,90 JANELA V_Janela  ;
   TITULO "Teste de entrada de dados" ;
   SUBTITULO "%T;com data inválida" ;
   TECLAS {} AJUDA "T?????"
ESPECIALIZE V_Janela ENTRADA DATAERRADA DATAINVA()
*
@ V_Janela,00,02 SAY "Informe a data 29/02/2001" GET D_Data AJUDA "C?????"

L_Ok := ATIVE(V_Janela)
DO WHILE L_Ok
    NAP_LOG("EHHHH ENTRADA FINISH()")
   MOSTRAR("M15590","Entrada de dados finalizada com sucesso.")
   L_Ok := ATIVE(V_Janela)
ENDDO
DESTRUA V_Janela
*
*****************************************
STAT PROC TST_ENTRADA_DADOS_FILTRO_TECLAS
*****************************************
LOCAL L_Ok, V_Janela
LOCAL C_Campo1 := "APENAS ACEITAR MAIÚSCULAS"
*
@ 07,43,23,95 JANELA V_Janela  ;
   TITULO "Teste de entrada de dados" ;
   SUBTITULO "%T;com filtro de teclas" ;
   TECLAS {} AJUDA "T?????"
ESPECIALIZE V_Janela ENTRADA FILTRO CONVERTE_PARA_UPPER() SEMTOOLBAR
*
@ V_Janela,00,02 SAY "Só maiúsculo" GET C_Campo1 AJUDA "C?????"

L_Ok := ATIVE(V_Janela)
DO WHILE L_Ok
   MOSTRAR("M15594","Entrada de dados finalizada com sucesso.")
   L_Ok := ATIVE(V_Janela)
ENDDO
DESTRUA V_Janela
*
*****************************
STAT FUNC CONVERTE_PARA_UPPER()
*****************************
LOCAL N_Lastkey := LASTKEY()
LOCAL C_Retorno
//NAP_LOG("HEY!!!! CONVERTE_PARA_UPPER(): " + hb_ntos(N_Lastkey))
IF N_Lastkey >= 97 .AND. N_Lastkey <= 122   // letras minúsculas
   C_Retorno := UPPER(CHR(N_Lastkey))
ELSE
   C_Retorno := NIL
ENDIF
RETURN C_Retorno
*
*****************************************
STAT PROC TST_ENTRADA_DADOS_COM_ROLAMENTO
*****************************************
LOCAL L_Ok, V_Janela
LOCAL C_Campo1 := "Valor 1....."
LOCAL C_Campo2 := "Valor 2....."
LOCAL C_Campo3 := "Valor 3....."
LOCAL C_Campo4 := "Valor 4....."
LOCAL C_Campo5 := "Valor 5....."
*
@ 07,53,20,80 JANELA V_Janela  ;
   TITULO "Teste de entrada dados" ;
   SUBTITULO "%T;com rolamento" ;
   TECLAS {} AJUDA "T?????"
ESPECIALIZE V_Janela ENTRADA ROLAVERTICAL

@ V_Janela,00,02 SAY "Campo 1" GET C_Campo1 AJUDA "C1????"
@ V_Janela,01,10 SAY LOWER(C_Campo1)
@ V_Janela,02,02 SAY "Campo 2" GET C_Campo2 AJUDA "C2????"
@ V_Janela,03,10 SAY LOWER(C_Campo2)
@ V_Janela,04,02 SAY "Campo 3" GET C_Campo3 AJUDA "C3????"
@ V_Janela,05,10 SAY LOWER(C_Campo3)
@ V_Janela,06,02 SAY "Campo 4" GET C_Campo4 AJUDA "C4????"
@ V_Janela,07,10 SAY LOWER(C_Campo4)
@ V_Janela,08,02 SAY "Campo 5" GET C_Campo5 AJUDA "C5????"
@ V_Janela,09,10 SAY LOWER(C_Campo5)

L_Ok := ATIVE(V_Janela)
DO WHILE L_Ok
   MOSTRAR("M15596","Entrada de dados finalizada com sucesso.")
   L_Ok := ATIVE(V_Janela)
ENDDO
DESTRUA V_Janela
*
*******************************************
STAT PROC TST_ENTRADA_DADOS_CORES_MENSAGENS
*******************************************
LOCAL L_Ok, V_Janela
LOCAL C_Campo1 := "Valor 1....."
LOCAL C_Campo2 := "Valor 2....."
*
@ 07,53,20,80 JANELA V_Janela  ;
   TITULO "Teste de entrada dados" ;
   SUBTITULO "%T;com rolamento" ;
   TECLAS {} AJUDA "T?????"
ESPECIALIZE V_Janela ENTRADA ROLAVERTICAL

@ V_Janela,00,02 SAY "Campo 1" GET C_Campo1 MESSAGE "Estou no campo 1" AJUDA "C?????"
@ V_Janela,01,10 SAY C_Campo1
@ V_Janela,02,02 SAY "Campo 2" GET C_Campo2 COLOR "+" MESSAGE "Estou no campo 2" AJUDA "C?????"
@ V_Janela,03,10 SAY C_Campo2 COLOR "+"

L_Ok := ATIVE(V_Janela)
DO WHILE L_Ok
   MOSTRAR("M15598","Entrada de dados finalizada com sucesso.")
   L_Ok := ATIVE(V_Janela)
ENDDO
DESTRUA V_Janela
*
*********************************
STAT PROC TST_ENTRADA_DADOS_VALID
*********************************
LOCAL L_Ok, V_Janela
LOCAL C_Campo1 := "            "
LOCAL C_Campo2 := "            "
LOCAL C_Campo3 := "            "
LOCAL C_Memo   := "Texto do memo"
*
ALARME("M11111","Tela incompleto, pois ainda não tem o campo memo...")
*
@ 06,53,21,80 JANELA V_Janela  ;
   TITULO "Teste de entrada dados" ;
   SUBTITULO "%T;com valids e memo" ;
   AJUDA "T99999"
ESPECIALIZE V_Janela ENTRADA ROLAVERTICAL

@ V_Janela,00,10 SAY "Texto a"
@ V_Janela,01,02 SAY "Campo 1" GET C_Campo1 ;
     VALID VALIDA_CAMPO("1",C_Campo1) AJUDA "C77777"
@ V_Janela,02,10 SAY "Texto b"
@ V_Janela,03,02 SAY "Campo 2" GET C_Campo2 ;
     WHEN PREENCHE_CAMPO_02(@C_Campo2) VALID VALIDA_CAMPO("2",C_Campo2)  AJUDA "C88888"
@ V_Janela,04,10 SAY "Texto c"
@ V_Janela,05,02 SAY "Campo 3" GET C_Campo3 ;
     VALID VALIDA_CAMPO("3",C_Campo3)  AJUDA "C99999"
@ V_Janela,06,10 SAY "Texto d"
*
ATIVE(V_Janela)
DESTRUA V_Janela

NAP_LOG("VALUE OF CAMPO1: " + C_Campo1)
NAP_LOG("VALUE OF CAMPO2: " + C_Campo2)
NAP_LOG("VALUE OF CAMPO3: " + C_Campo3)
*
***************************
STAT FUNC PREENCHE_CAMPO_02(C_Campo2)
***************************
NAP_LOG("PREENCHE_CAMPO_02 Begin: '" + C_Campo2 + "'")
IF EMPTY(C_Campo2)
   C_Campo2 := REPL("2",LEN(C_Campo2))
ENDIF
NAP_LOG("PREENCHE_CAMPO_02 After: '" + C_Campo2 + "'")
RETURN .T.
*
**********************
STAT FUNC VALIDA_CAMPO (C_Titulo_Campo,C_Conteudo_Campo)
**********************
LOCAL L_OK := .T.
NAP_LOG("VALIDA_CAMPO" + C_Titulo_Campo + ": " + C_Conteudo_Campo)

IF EMPTY(C_Conteudo_Campo)
   L_OK := .F.
   ALARME("M22222","Campo "+C_Titulo_Campo+" vazio")
ENDIF
RETURN L_OK
*
**************************************
STAT PROC TST_ENTRADA_DADOS_LISTA_AUTO
**************************************
LOCAL L_Ok, V_Janela
LOCAL C_Campo1 := PADR("Campo 2 só terá 'drop-down'      ",35)
LOCAL C_Campo2 := PADR("automático se campo 1 ficar vazio",35)
*
@ 07,43,23,95 JANELA V_Janela  ;
   TITULO "Teste de entrada de dados" ;
   SUBTITULO "%T;com 'lista' e 'auto'" ;
   TECLAS {} AJUDA "T?????"
ESPECIALIZE V_Janela ENTRADA
*
@ V_Janela,00,02 SAY "Campo 1" GET C_Campo1 AJUDA "C?????"
@ V_Janela,01,02 SAY "Campo 2" GET C_Campo2 ;
    LISTA LISTA_CAMPO_2() AUTO EMPTY(C_Campo1) AJUDA "C?????"

L_Ok := ATIVE(V_Janela)
DO WHILE L_Ok
   MOSTRAR("M15592","Entrada de dados finalizada com sucesso.")
   L_Ok := ATIVE(V_Janela)
ENDDO
DESTRUA V_Janela
*
***********************
STAT FUNC LISTA_CAMPO_2
***********************
LOCAL V_Janela, N_Opcao, C_Retorno
LOCAL V_Vetor := {"Retornar texto A",;
                  "Retornar texto B"}

CUA20 @ 15,80,25,105 JANELA V_Janela TITULO "'Drop-down'" SUBTITULO "%T" AJUDA "T?????"
*
CUA20 ESPECIALIZE V_Janela SELECAO SIMPLES VETOR V_Vetor ;
   NAOROLAVERTICAL NAOROLAHORIZONTAL SEMGRADE SEMTOOLBAR AUTOCLOSE
*
N_Opcao := ATIVE(V_Janela)
NAP_LOG("LISTA N_Opcao: " + hb_ntos(N_Opcao))
*
IF N_Opcao # 0
   C_Retorno := PADR(V_Vetor[N_Opcao],35)
ELSE
   C_Retorno := NIL
ENDIF
*
RETURN C_Retorno
*

**************************************
STAT PROC TST_ENTRADA_DADOS_PEDEJAN
**************************************
LOCAL L_Ok, V_Janela, V_Janela2
LOCAL C_Texto

L_OK := .F.
V_Janela := {}
V_Janela2:= {}

V_Janela := DEFJAN()

@ MAXROW()-19,MAXCOL()-90,MAXROW()-3,MAXCOL()-9 JANELA V_Janela2 SUBTITULO "" ;
    TECLAS {"F9=Encerra texto"} EMBUTIDA V_Janela AJUDA "T?????"

C_Texto := MEMOREAD("../dados/textotes.txt")

ESPECIALIZE V_Janela2 TEXTO C_Texto LARGURA 55 TABULACAO 1 ;
    EDITA .T. TERMINAR {K_F9} SEMTOOLBAR

KEYBOARD CHR(K_ESC)
ATIVE(V_JANELA)
// KEYBOARD CHR(K_HOME)+CHR(K_ESC)
//ATIVE(V_JANELA2)

//L_Ok := ATIVE(V_Janela)
// IF L_OK
//     L_OK := ATIVE(V_Janela2)
// 	DO WHILE LASTKEY() == K_F9
//        SETPOS(2,40)
// 	   MOSTRAR("M15592","Entrada de dados finalizada com sucesso.")
// 	   L_Ok := ATIVE(V_Janela)
// 	ENDDO
// ENDIF

//DESTRUA V_Janela2
DESTRUA V_Janela

**********************
STAT FUNC DEFJAN
**********************
LOCAl V_JAN:={}
LOCAL C_Campo1 := Space(40)
LOCAL C_Campo2 := Space(40)

@ 03,01,MAXROW()-1,MAXCOL()-1 JANELA V_JAN ;
   TITULO "Teste de entrada de dados" ;
   SUBTITULO "%T;PedeJan" ;
   TECLAS {"Esc=sair"} AJUDA "T?????"

ESPECIALIZE V_Jan ENTRADA EDITA .T.
*
@ V_Jan,00,02 SAY "Campo 1" GET C_Campo1 AJUDA "C?????"
@ V_Jan,01,02 SAY "Campo 2" GET C_Campo2 AJUDA "C?????"
@ V_Jan,07,02 SAY "Observacao"

RETURN V_JAN









// /*
// ***********************************
// STAT PROC TST_ENTRADA_DADOS_DOMINIO
// ***********************************
// LOCAL L_Ok, V_Janela
// LOCAL C_Periodicidade1 := " "
// LOCAL C_Periodicidade2 := " "
// LOCAL C_Periodicidade3 := " "
// LOCAL C_Periodicidade4 := " "
// LOCAL N_MesA := 0
// LOCAL N_MesB := 0
// *
// @ 05,13,32,95 JANELA V_Janela  ;
//    TITULO "Teste de entrada de dados" ;
//    SUBTITULO "%T;com 'dominio'" ;
//    TECLAS {} AJUDA "T?????"
// ESPECIALIZE V_Janela ENTRADA
// *
// @ V_Janela,01,02 SAY "(Campos com 'domínio')"
// *
// @ V_Janela,03,02 SAY "(Campo sem 'auto', sem 'f4comcodigo', com 'semf4', sem 'opcional')"
// @ V_Janela,04,02 SAY "Periodicidade 1..." ;
//     GET C_Periodicidade1 ;
//     WHEN WHEN_PREENCHER_DOMINIO(@C_Periodicidade1) VALID VALID_NAO_ACEITA_SEMESTRE(C_Periodicidade1) ;
//     DOMINIO DOM_CARACTERE() SEMF4 ;
//     AJUDA "C?????"
// *
// @ V_Janela,06,02 SAY "(Campo sem 'auto', sem 'f4comcodigo', sem 'semf4', com 'opcional')"
// @ V_Janela,07,02 SAY "Periodicidade 2..." ;
//     GET C_Periodicidade2  ;
//     VALID VALID_NAO_ACEITA_SEMESTRE(C_Periodicidade2) ;
//     DOMINIO DOM_CARACTERE() OPCIONAL ;
//     AJUDA "C?????"
// *
// @ V_Janela,09,02 SAY "(Campo sem 'auto', sem 'f4comcodigo', com 'semf4', com 'opcional')"
// @ V_Janela,10,02 SAY "Periodicidade 3..." ;
//     GET C_Periodicidade3 ;
//     VALID VALID_NAO_ACEITA_SEMESTRE(C_Periodicidade3) ;
//     DOMINIO DOM_CARACTERE() SEMF4 OPCIONAL ;
//     AJUDA "C?????"
// *
// @ V_Janela,12,02 SAY "(Campo com 'auto', com 'f4comcodigo', sem 'semf4', sem 'opcional')"
// @ V_Janela,13,02 SAY "Periodicidade 4..." ;
//     GET C_Periodicidade4 ;
//     VALID VALID_NAO_ACEITA_SEMESTRE(C_Periodicidade4) ;
//     DOMINIO DOM_CARACTERE() AUTO EMPTY(C_Periodicidade4) F4COMCODIGO ;
//     AJUDA "C?????"
// *
// @ V_Janela,15,02 SAY "(Campo com 'auto', com 'f4comcodigo', sem 'semf4', sem 'opcional')"
// @ V_Janela,16,02 SAY "Mês do trimestre A" ;
//     GET N_MesA ;
//     VALID VALID_NAO_MES_FEVEREIRO(N_MesA) ;
//     DOMINIO DOM_NUMERICO_MES() AUTO EMPTY(N_MesA) F4COMCODIGO ;
//     AJUDA "C?????"

// @ V_Janela,18,02 SAY "(Campo sem 'auto', sem 'f4comcodigo', sem 'semf4', com 'opcional')"
// @ V_Janela,19,02 SAY "Mês do trimestre B" ;
//     GET N_MesB ;
//     VALID VALID_NAO_MES_FEVEREIRO(N_MesB) ;
//     DOMINIO DOM_NUMERICO_MES() AUTO EMPTY(N_MesB) OPCIONAL ;
//     AJUDA "C?????"
// *
// L_Ok := ATIVE(V_Janela)
// DO WHILE L_Ok
//    MOSTRAR("M15592","Entrada de dados finalizada com sucesso.")
//    L_Ok := ATIVE(V_Janela)
// ENDDO
// DESTRUA V_Janela
// *
// **************************************
// STATIC FUNCTION WHEN_PREENCHER_DOMINIO (C_Periodicidade1)  // Por referência
// **************************************
// LOCAL L_ENTRA := .T.
// *
// // L_ENTRA := .F.
// // C_Periodicidade1 := "X" // "S"
// *
// RETURN L_ENTRA
// *
// *************************************
// STATIC FUNC VALID_NAO_ACEITA_SEMESTRE(C_Periodicidade)
// *************************************
// LOCAL L_OK := .T.
// *
// IF C_Periodicidade $ "SX"
//    L_OK := .F.
//    ALARME("M?????","Não pode ser semestral, nem 'X'")
// ENDIF
// *
// RETURN L_OK
// *
// ***********************************
// STATIC FUNC VALID_NAO_MES_FEVEREIRO(N_Mes)
// ***********************************
// LOCAL L_OK := .T.
// *
// IF N_Mes==2
//    L_OK := .F.
//    ALARME("M?????","Não pode ser fevereiro")
// ENDIF
// *
// RETURN L_OK
// *
// *********************************
// STAT PROC TST_ENTRADA_DADOS_CAMPO
// *********************************
// LOCAL L_Ok, V_Janela
// LOCAL D_Data := CTOD("")
// LOCAL N_NumeroInteiro := 0
// LOCAL N_NumeroDecimal := 0
// LOCAL C_EMAIL := SPACE(30)
// LOCAL C_TextoComDominio := SPACE(1)
// *
// @ 05,13,32,95 JANELA V_Janela  ;
//    TITULO "Teste de entrada de dados" ;
//    SUBTITULO "%T;com 'campo'" ;
//    TECLAS {} AJUDA "T?????"
// ESPECIALIZE V_Janela ENTRADA
// *
// @ V_Janela,01,02 SAY "(Campos com 'campo')"
// *
// @ V_Janela,03,02 SAY "(Campo data, sem 'opcional')"
// @ V_Janela,04,02 SAY "Data..." ;
//     GET D_Data PICT "99/99/99" ;
//     VALID VALID_NAO_ACEITA_31_07_2017(D_Data) ;
//     CAMPO CMP_DATA() ;
//     AJUDA "C?????"
// *
// @ V_Janela,06,02 SAY "(Campo inteiro com domínio, sem 'opcional')"
// @ V_Janela,07,02 SAY "Inteiro..." ;
//     GET N_NumeroInteiro ;
//     WHEN WHEN_PREENCHER_CAMPO(@N_NumeroInteiro) CAMPO CMP_NUMERICO_COM_DOMINIO() ;
//     AJUDA "C?????"
// *
// @ V_Janela,09,02 SAY "(Campo decimal sem domínio, com 'opcional')"
// @ V_Janela,10,02 SAY "Decimal..." ;
//     GET N_NumeroDecimal ;
//     CAMPO CMP_NUMERICO_SEM_DOMINIO() OPCIONAL ;
//     AJUDA "C?????"
// *
// @ V_Janela,12,02 SAY "(Campo email sem domínio, com 'opcional')"
// @ V_Janela,13,02 SAY "EMAIL..." ;
//     GET C_EMAIL PICTURE "@K" ;
//     CAMPO CMP_CARACTERE_EMAIL() OPCIONAL ;
//     AJUDA "C?????"
// *
// @ V_Janela,15,02 SAY "(Campo caractere com domínio, sem 'opcional')"
// @ V_Janela,16,02 SAY "Periodicidade..." ;
//     GET C_TextoComDominio ;
//     CAMPO CMP_CARACTERE_COM_DOMINIO() ;
//     AJUDA "C?????"
// *
// L_Ok := ATIVE(V_Janela)
// DO WHILE L_Ok
//    MOSTRAR("M15592","Entrada de dados finalizada com sucesso.")
//    L_Ok := ATIVE(V_Janela)
// ENDDO
// DESTRUA V_Janela
// *
// ***************************************
// STATIC FUNC VALID_NAO_ACEITA_31_07_2017(D_Data)
// ***************************************
// LOCAL L_OK := .T.
// *
// IF D_Data == CTOD("31/07/2017")
//    L_OK := .F.
//    ALARME("M?????","Não pode ser 31/07/2017")
// ENDIF
// *
// RETURN L_OK
// *
// ************************************
// STATIC FUNCTION WHEN_PREENCHER_CAMPO (N_NumeroInteiro)  // Por referência
// ************************************
// LOCAL L_ENTRA := .T.
// *
// //L_ENTRA := .F.
// //N_NumeroInteiro := 201
// *
// RETURN L_ENTRA
// *
// *******************************
// * PADRONIZAÇÃO DE NOMENCLATURA:
// *******************************
// *    DOM_* - Nome de função que retorne a definição de um domínio
// *    CMP_* - Nome de função que retorne a definição de um campo (com domínio relacionado ou não)
// *    REG_* - Nome de função que retorne a definição de um registro
// *
// *****************************
// STATIC FUNCTION DOM_CARACTERE
// *****************************
// LOCAL O_Dominio
// *
// DEF_DOMINIO O_Dominio ;
//     NOME "Periodi-;cidade" ;
//     SE "D" TITULO "#dia",;
//     SE "M" TITULO "#mês",;
//     SE "S" TITULO "#semestre",;
//     SE "A" TITULO "#ano"
// *
// RETURN O_Dominio
// *
// ********************************
// STATIC FUNCTION DOM_NUMERICO_MES
// ********************************
// LOCAL O_Dominio
// *
// DEF_DOMINIO O_Dominio ;
//     NOME "Mês" ;
//     SE  1 TITULO "janeiro",;      // Intencionalmente sem "hot-key"
//     SE  2 TITULO "fevereiro",;
//     SE  3 TITULO "março"
// *
// RETURN O_Dominio
// *
// ************************
// STATIC FUNCTION CMP_DATA
// ************************
// LOCAL O_Campo
// *
// DEF_CAMPO O_Campo TAMANHO 8 ;
//     NOME "Data do pagto"
// *
// ESP_CAMPO O_Campo TIPO_DATA ;
//    MAIOR_OU_IGUAL_A CTOD("01/01/1993") ;
//    MENOR_OU_IGUAL_A CTOD("31/12/2020") ;
//    FINAL_MES  ;
//    SUGERIR CTOD("31/03/2016")
// *
// RETURN O_Campo
// *
// **********************************
// STATIC FUNCTION DOM_NUMERICO_VALOR
// **********************************
// LOCAL O_Dominio
// *
// DEF_DOMINIO O_Dominio ;
//     NOME "Valor" ;
//     SE  100 TITULO "0#100" ,;
//     SE  200 TITULO "0#200" ,;
//     SE 9000 TITULO "#9000"
// *
// RETURN O_Dominio
// *
// ****************************************
// STATIC FUNCTION CMP_NUMERICO_COM_DOMINIO
// ****************************************
// LOCAL O_Campo
// *
// DEF_CAMPO O_Campo TAMANHO 5  ;
//     NOME "Quantidade menor que 9000" ;
//     RESUMIDO "Qtd"
// *
// ESP_CAMPO O_Campo TIPO_INTEIRO MENOR_OU_IGUAL_A 9000 SUGERIR 200
// *
// * forma alternativa
// ADD_AO_CAMPO O_Campo DOMINIO DOM_NUMERICO_VALOR()

// // forma alternativa
// //DEF_NO_CAMPO O_Campo DOMINIO ;
// //    SE  100 TITULO "0#100" ,;
// //    SE  200 TITULO "0#200" ,;
// //    SE 9000 TITULO "#9000"
// *
// RETURN O_Campo
// *
// ****************************************
// STATIC FUNCTION CMP_NUMERICO_SEM_DOMINIO
// ****************************************
// LOCAL O_Campo
// *
// DEF_CAMPO O_Campo TAMANHO 8 ;
//     NOME "Valor decimal" ;
//     RESUMIDO "Vl"
// *
// ESP_CAMPO O_Campo TIPO_DECIMAL 2 MAIOR_QUE 12.34 MENOR_OU_IGUAL_A 12.35 SUGERIR 12.35
// *
// RETURN O_Campo
// *
// ***********************************
// STATIC FUNCTION CMP_CARACTERE_EMAIL
// ***********************************
// LOCAL O_Campo
// *
// DEF_CAMPO O_Campo TAMANHO 30 ;
//     NOME "EMAIL" ;
//     SINGULAR MASCULINO
// *
// ESP_CAMPO O_Campo TIPO_CARACTERE MINIMO 3 ;
//     MINUSCULO SEM_ESPACO_MEIO ;
//     SUGERIR "usuario@exemplo.com.br" ;
//     EXPRESSAO_REGULAR "^[a-zA-Z0-9.!#$%&'*+-\/=?\^_`{|}~-]+@[a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*\s*$"  ;
//     MENSAGEM "Email válido somente no formato 'usuario@dominio'"
// *
// ADD_AO_CAMPO O_Campo VALIDACAO CHECAR_EMAIL_PROIBIDO
// *
// RETURN O_Campo
// *
// *************************************
// STATIC FUNCTION CHECAR_EMAIL_PROIBIDO (C_EMAIL)
// *************************************
// LOCAL V_Lst_Erros := {}
// *
// IF TRIM(LOWER(C_EMAIL))=="proibido@proibido.com.br"
//    AADD(V_Lst_Erros,{_MENS_ERRO_GRAVE,"M39704","não pode ser 'proibido@proibido.com.br'"})
// ENDIF
// *
// RETURN V_Lst_Erros
// *
// *****************************************
// STATIC FUNCTION CMP_CARACTERE_COM_DOMINIO
// *****************************************
// LOCAL O_Campo
// *
// DEF_CAMPO O_Campo TAMANHO 1 ;
//     NOME "Caractere com domínio" ;
//     SINGULAR MASCULINO
// *
// ESP_CAMPO O_Campo TIPO_CARACTERE MINIMO 1 ;
//     MAIUSCULO SEM_ESPACO_MEIO SUGERIR "S"
// *
// * Forma alternativa
// ADD_AO_CAMPO O_Campo DOMINIO DOM_CARACTERE()

// // Forma alternativa
// //DEF_NO_CAMPO O_Campo DOMINIO ;
// //    SE "D" TITULO "#dia",;
// //    SE "M" TITULO "#mês",;
// //    SE "S" TITULO "#semestre",;
// //    SE "A" TITULO "#ano"
// *
// RETURN O_Campo
// *
// */
// *
// *************************
