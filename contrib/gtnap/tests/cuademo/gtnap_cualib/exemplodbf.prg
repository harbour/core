/* encoding: cp850 */
#INCLUDE "inkey.ch"
#INCLUDE "cua.ch"

FIELD cdindx, dtcota, vlcota
***********************
PROC EXEMPLO_BROWSE_DBF
***********************
LOCAL V_Janela
*
// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//
//  Fran: This code works on Windows and Linux
//
IF FILE("../dados/cotacao.dbf") .AND. ;
    .NOT. FILE("../dados/cotacao.cdx")
    USE ../dados/cotacao NEW EXCLUSIVE
    INDEX ON COTACAO->CDINDX+DTOS(COTACAO->DTCOTA) TAG COTACAO1 ;
        TO ../dados/cotacao.cdx
    CLOSE COTACAO
ENDIF
// #elif defined(__PLATFORM__LINUX) || defined(__PLATFORM__Linux)   // ADAPTACAO_LINUX
//    IF FILE("/opt/cuadados/cotacao.dbf") .AND. ;
//       .NOT. FILE("/opt/cuadados/cotacao.cdx")
//       USE /opt/cuadados/cotacao NEW EXCLUSIVE
//       INDEX ON COTACAO->CDINDX+DTOS(COTACAO->DTCOTA) TAG COTACAO1 ;
//             TO /opt/cuadados/cotacao.cdx
//       CLOSE COTACAO
//    ENDIF
// #else
//    #erro "Código não adaptado para esta plataforma"
// #endif
*
CUA20 @ 15,20,26,80 JANELA V_Janela ;
     TITULO "Escolha o tipo de janela de browse de DBF" SUBTITULO "%T";
     AJUDA "T?????"
*
ESPECIALIZE V_Janela MENU
ADDOPCAO V_Janela TEXTO "janela seleção simples, com grid, com toolbar" ;
   ACAO TST_BROWSE_DBF_SIMPLES_COM_GRID_COM_TOOLBAR() AJUDA "P06697"
ADDOPCAO V_Janela TEXTO "janela seleção múltipla, sem grid, nem toolbar" ;
   ACAO TST_BROWSE_DBF_MULTIPLA_SEM_GRID_SEM_TOOLBAR() AJUDA "P06699"
ADDOPCAO V_Janela TEXTO "janela seleção estendida, com coluna congelada" ;
   ACAO TST_BROWSE_DBF_COLUNA_CONGELADA() AJUDA "P06701"
ADDOPCAO V_Janela TEXTO "janela seleção, com 'while', sem barras de rolagem" ;
   ACAO TST_BROWSE_DBF_WHILE() AJUDA "P06703"
/*
ADDOPCAO V_Janela TEXTO "janela seleção simples com BUG no rolamento horizontal" ;
   ACAO TST_BROWSE_ERRO_ROLAMENTO_HORIZONTAL() AJUDA "P06703"
ADDOPCAO V_Janela TEXTO "janela seleção simples com BUG no rolamento vertical" ;
   ACAO TST_BROWSE_ERRO_ROLAMENTO_VERTICAL() AJUDA "P06703"
*/
*
ATIVE(V_Janela)
*

// PROC TST_BROWSE_DBF_SIMPLES_COM_GRID_COM_TOOLBAR
//     @ 22, 0 SAY ""
//     OutStd("TST_BROWSE_DBF_SIMPLES_COM_GRID_COM_TOOLBAR() Option selected")
//     RETURN

// PROC TST_BROWSE_DBF_MULTIPLA_SEM_GRID_SEM_TOOLBAR
//     @ 22, 0 SAY ""
//     OutStd("TST_BROWSE_DBF_MULTIPLA_SEM_GRID_SEM_TOOLBAR() Option selected")
//     RETURN








// PROC TST_BROWSE_DBF_COLUNA_CONGELADA
//     @ 22, 0 SAY ""
//     OutStd("TST_BROWSE_DBF_COLUNA_CONGELADA() Option selected")
//     RETURN

PROC TST_BROWSE_DBF_WHILE
    @ 22, 0 SAY ""
    OutStd("TST_BROWSE_DBF_WHILE() Option selected")
    RETURN

*****************************************************
STAT PROC TST_BROWSE_DBF_SIMPLES_COM_GRID_COM_TOOLBAR
*****************************************************
LOCAL V_Janela
*

//
//  Fran: This code works on Windows and Linux
//
USE ../dados/cotacao NEW SHARED
SET INDEX TO ../dados/cotacao
GOTO TOP

// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//    USE dados\cotacao NEW SHARED
//    SET INDEX TO dados\cotacao
// #elif defined(__PLATFORM__LINUX) || defined(__PLATFORM__Linux)   // ADAPTACAO_LINUX
//    USE /opt/cuadados/cotacao NEW SHARED
//    SET INDEX TO /opt/cuadados/cotacao
// #else
//    #erro "Código não adaptado para esta plataforma"
// #endif
// GOTO TOP
*
CUA20 @ 01,41,MAXROW()-2,MAXCOL()-30 JANELA V_Janela ;
    TITU "Browse de arquivo DBF" ;
    SUBTITULO "%T;seleção simples,;com grid e com toolbar,;"+;
              "com opções adicionais" ;
    AJUDA "T?????"

ADDBOTAO V_Janela TEXTO "V=incrementar valor corrente" ;
   ACAO INCREMENTA_CORRENTE(V_Janela) AJUDA "B19265"
ADDBOTAO V_Janela TEXTO "M=incrementar todos os valores" ;
   ACAO INCREMENTA_TODOS(V_Janela) AJUDA "B19267"
ADDBOTAO V_Janela TEXTO "N=exclui linha atual" ;
   ACAO EXCLUI_LINHA_ATUAL(V_Janela) AJUDA "B?????"
ADDBOTAO V_Janela TEXTO "U=procura linha atual" ;
   ACAO PROCURAR_REGISTRO(V_Janela) RECNOMUDA AJUDA "B?????"

CUA20 ADDIMAGEM V_Janela ARQUIVO DIRET_BMPS()+"logaspec.bmp"  ;
  COORDENADAS 01,01,03,05 ACAO INCREMENTA_CORRENTE(V_Janela) AJUDA "B19129"

// ADDACAO V_Janela INKEY K_F6 ACAO INCREMENTA_CORRENTE(V_Janela) AJUDA "B19117"

CUA20 ESPECIALIZE V_Janela SELECAO SIMPLES AUTOCLOSE

ANEXE V_Janela TITULO "Registro;deletado" COLUNA TESTA_DELECAO(.NOT. EOF())
ANEXE V_Janela TITULO "Cd;moeda"  COLUNA cdindx
ANEXE V_Janela TITULO "Data"      COLUNA dtcota
ANEXE V_Janela TITULO "Data+2"    COLUNA dtcota+1
ANEXE V_Janela TITULO "Data+3"    COLUNA dtcota+2
ANEXE V_Janela TITULO "Cotação"   COLUNA TRANSFORM(vlcota,"@E 999,999,999,999.99999999")
*
ATIVE(V_Janela)
*
CLOSE COTACAO



*****************************************************
STAT PROC TST_BROWSE_DBF_MULTIPLA_SEM_GRID_SEM_TOOLBAR
*****************************************************
LOCAL V_Janela

//
//  Fran: This code works on Windows and Linux
//
USE ../dados/cotacao NEW SHARED
SET INDEX TO ../dados/cotacao
GOTO TOP

// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//    USE dados\cotacao NEW SHARED
//    SET INDEX TO dados\cotacao
// #elif defined(__PLATFORM__LINUX)  || defined(__PLATFORM__Linux)   // ADAPTACAO_LINUX
//    USE /opt/cuadados/cotacao NEW SHARED
//    SET INDEX TO /opt/cuadados/cotacao
// #else
//    #erro "Código não adaptado para esta plataforma"
// #endif
// GOTO TOP
*
CUA20 @ 01,41,MAXROW()-2,MAXCOL()-20 JANELA V_Janela ;
    TITU "Browse de arquivo DBF" ;
    SUBTITULO "%T;seleção múltipla;sem grid e sem toolbar";
    AJUDA "T?????"
ADDBOTAO V_Janela TEXTO "R=incrementar registros selecionados" ;
   ACAO INCREMENTA_SELECIONADOS(V_Janela) AJUDA "B19269"

ADDBOTAO V_Janela TEXTO "V=voltar seleção para o 'default'" ;
   ACAO DEFAULT_SELECIONADOS(V_Janela) AJUDA "B19271"

CUA20 ESPECIALIZE V_Janela SELECAO MULTIPLA CONGELAR 1 ;
   SEMGRADE SEMTOOLBAR

ANEXE V_Janela TITULO "Cd;moeda"  COLUNA cdindx
ANEXE V_Janela TITULO "Data"      COLUNA dtcota
ANEXE V_Janela TITULO "Data+2"    COLUNA dtcota+1
ANEXE V_Janela TITULO "Data+3"    COLUNA dtcota+2
ANEXE V_Janela TITULO "Cotação"   COLUNA TRANSFORM(vlcota,"@E 999,999,999,999.99999999")
*
MUDE SELECAO V_Janela PARA {2,4,6,8}   // registros pré-selecionados
*
ATIVE(V_Janela)
*
CLOSE COTACAO
*



*
***********************
STAT FUNC TESTA_DELECAO(L_NAO_EOF)
***********************
LOCAL C_Str
IF L_NAO_EOF
   IF DELETED()
      C_Str := "Sim"
      // Como o SET DELETED ON está ativo, não era para passar por aqui...
      ALTD()
   ELSE
      C_Str := "Não"
   ENDIF
ELSE
   C_Str := "Eof"
ENDIF
Return C_Str
*

*****************************
STAT PROC INCREMENTA_CORRENTE(V_Janela)
*****************************
LOCAL N_Selecionado := ITENSSELECIONADOS(V_Janela)
*
MOSTRAR("M15634","O registro selecionado foi o RECNO() "+;
   LTRIM(STR(N_Selecionado)))
*
RLOCK()
REPL vlcota WITH vlcota+1
DBCOMMIT()
UNLOCK
RELEIA CORRENTE V_Janela
*
*************************
STAT PROC INCREMENTA_TODOS(V_Janela)
**************************
LOCAL N_Selecionado := ITENSSELECIONADOS(V_Janela)
*
MOSTRAR("M15636","O registro atual é o RECNO() "+;
   LTRIM(STR(N_Selecionado)))
*
FLOCK()
REPL ALL vlcota WITH vlcota+1
DBCOMMIT()
UNLOCK
GOTO N_Selecionado
*
RELEIA TUDO V_Janela
*

STAT PROC EXCLUI_LINHA_ATUAL(V_Janela)
    RETURN
// ****************************
// STAT PROC EXCLUI_LINHA_ATUAL(V_Janela)
// ****************************
// MOSTRAR("M?????","O registro atual (a ser deletado) é o RECNO() "+;
//    LTRIM(STR(RECNO())))
// *
// RLOCK()
// DELETE
// DBCOMMIT()
// UNLOCK
// *
// RELEIA TUDO V_Janela
// *

STAT PROC PROCURAR_REGISTRO (V_Janela)
    RETURN

// ***************************
// STAT PROC PROCURAR_REGISTRO (V_Janela)
// ***************************
// LOCAL C_CDINDX, D_DTCOTA
// LOCAL N_Opcao
// *
// N_Opcao := PERGUN("Qual registro pesquisar:",;
//                {"Primeiro","Terceiro","Antepenultimo","Ultimo"})
// IF N_Opcao==1
//    C_CDINDX := "01" // primeiro registro
//    D_DTCOTA := CTOD("31/01/1993")
// ELSEIF N_Opcao==2
//    C_CDINDX := "01" // terceiro registro
//    D_DTCOTA := CTOD("31/03/1993")
// ELSEIF N_Opcao==3
//    C_CDINDX := "99" // antepenúltimo registro
//    D_DTCOTA := CTOD("28/02/1999")
// ELSEIF N_Opcao==4
//    C_CDINDX := "99" // Último registro
//    D_DTCOTA := CTOD("30/04/1999")
// ELSE
//    ? MEMVAR->ALGO_TEM_DE_SER_SELECIONADO
// ENDIF
// *
// ADVERTE("M?????","Busca por:;"+C_CDINDX+" "+DTOC(D_DTCOTA))
// *
// SEEK C_CDINDX+DTOS(D_DTCOTA)
// IF .NOT. FOUND()
//    ? MEMVAR->REGISTRO_TEM_DE_EXISTIR
// ENDIF
// *
// RELEIA TUDO V_Janela
// *
// *****************************************************
// STAT PROC TST_BROWSE_DBF_MULTIPLA_SEM_GRID_SEM_TOOLBAR
// *****************************************************
// LOCAL V_Janela

// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//    USE dados\cotacao NEW SHARED
//    SET INDEX TO dados\cotacao
// #elif defined(__PLATFORM__LINUX)  || defined(__PLATFORM__Linux)   // ADAPTACAO_LINUX
//    USE /opt/cuadados/cotacao NEW SHARED
//    SET INDEX TO /opt/cuadados/cotacao
// #else
//    #erro "Código não adaptado para esta plataforma"
// #endif
// GOTO TOP
// *
// CUA20 @ 01,41,MAXROW()-2,MAXCOL()-20 JANELA V_Janela ;
//     TITU "Browse de arquivo DBF" ;
//     SUBTITULO "%T;seleção múltipla;sem grid e sem toolbar";
//     AJUDA "T?????"
// ADDBOTAO V_Janela TEXTO "R=incrementar registros selecionados" ;
//    ACAO INCREMENTA_SELECIONADOS(V_Janela) AJUDA "B19269"

// ADDBOTAO V_Janela TEXTO "V=voltar seleção para o 'default'" ;
//    ACAO DEFAULT_SELECIONADOS(V_Janela) AJUDA "B19271"

// CUA20 ESPECIALIZE V_Janela SELECAO MULTIPLA CONGELAR 1 ;
//    SEMGRADE SEMTOOLBAR

// ANEXE V_Janela TITULO "Cd;moeda"  COLUNA cdindx
// ANEXE V_Janela TITULO "Data"      COLUNA dtcota
// ANEXE V_Janela TITULO "Data+2"    COLUNA dtcota+1
// ANEXE V_Janela TITULO "Data+3"    COLUNA dtcota+2
// ANEXE V_Janela TITULO "Cotação"   COLUNA TRANSFORM(vlcota,"@E 999,999,999,999.99999999")
// *
// MUDE SELECAO V_Janela PARA {2,4,6,8}   // registros pré-selecionados
// *
// ATIVE(V_Janela)
// *
// CLOSE COTACAO
// *


*********************************
STAT PROC INCREMENTA_SELECIONADOS(V_Janela)
*********************************
LOCAL V_Selecionados := ITENSSELECIONADOS(V_Janela)
LOCAL N_Ct, N_Recno_Ant
*
MOSTRAR("M15638","Foram selecionados "+;
        LTRIM(STR(LEN(V_Selecionados)))+" registros")
*
N_Recno_Ant := RECNO()
FOR N_Ct := 1 TO LEN(V_Selecionados)
   GOTO V_Selecionados[N_Ct]
   RLOCK()
   REPL vlcota WITH vlcota+1
   UNLOCK
NEXT
DBCOMMIT()
GOTO N_Recno_Ant
*
RELEIA TUDO V_Janela
*

// STAT PROC DEFAULT_SELECIONADOS(V_Janela)
// RETURN
// NAP_TABLEVIEW_SELECT(V_TableView, VN_Selecio)

*********************************
STAT PROC DEFAULT_SELECIONADOS(V_Janela)
*********************************
MUDE SELECAO V_Janela PARA {2,4,6,8}   // registros pré-selecionados

*
*****************************************
STAT PROC TST_BROWSE_DBF_COLUNA_CONGELADA
*****************************************
LOCAL V_Janela

//
//  Fran: This code works on Windows and Linux
//
USE ../dados/cotacao NEW SHARED
SET INDEX TO ../dados/cotacao
GOTO TOP

// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//    USE dados\cotacao NEW SHARED
//    SET INDEX TO dados\cotacao
// #elif defined(__PLATFORM__LINUX)  || defined(__PLATFORM__Linux)   // ADAPTACAO_LINUX
//    USE /opt/cuadados/cotacao NEW SHARED
//    SET INDEX TO /opt/cuadados/cotacao
// #else
//    #erro "Código não adaptado para esta plataforma"
// #endif
// GOTO TOP



*
CUA20 @ 01,41,MAXROW()-2,MAXCOL()-20 JANELA V_Janela ;
    TITU "Browse de arquivo DBF" ;
    SUBTITULO "%T;seleção estendida;com coluna congelada" ;
    AJUDA "T?????"
ADDBOTAO V_Janela TEXTO "Enter=contar selecionados e fechar janela" ;
   ACAO (CONTAR_SELECIONADOS(V_Janela),.T.) AUTOCLOSE AJUDA "B19273"
CUA20 ESPECIALIZE V_Janela SELECAO ESTENDIDA CONGELAR 1

ANEXE V_Janela TITULO "Cd;moeda"  COLUNA cdindx
ANEXE V_Janela TITULO "Data"      COLUNA dtcota
ANEXE V_Janela TITULO "Data+2"    COLUNA dtcota+1
ANEXE V_Janela TITULO "Data+3"    COLUNA dtcota+2
ANEXE V_Janela TITULO "Cotação"   COLUNA TRANSFORM(vlcota,"@E 999,999,999,999.99999999")
*
ATIVE(V_Janela)
*
CLOSE COTACAO
*

*****************************
STAT PROC CONTAR_SELECIONADOS(V_Janela)
*****************************
LOCAL V_Selecionados := ITENSSELECIONADOS(V_Janela)
*
MOSTRAR("M15640","Foram selecionados "+;
        LTRIM(STR(LEN(V_Selecionados)))+" registros")
*
// ******************************
// STAT PROC TST_BROWSE_DBF_WHILE
// ******************************
// LOCAL V_Janela

// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//    USE dados\cotacao NEW SHARED
//    SET INDEX TO dados\cotacao
// #elif defined(__PLATFORM__LINUX)  || defined(__PLATFORM__Linux)   // ADAPTACAO_LINUX
//    USE /opt/cuadados/cotacao NEW SHARED
//    SET INDEX TO /opt/cuadados/cotacao
// #else
//    #erro "Código não adaptado para esta plataforma"
// #endif
// GOTO TOP
// *
// CUA20 @ 01,41,MAXROW()-2,MAXCOL()-20 JANELA V_Janela ;
//     TITU "Browse de arquivo DBF" ;
//     SUBTITULO "%T;cláusula WHILE;sem barras de rolagem" ;
//     AJUDA "T?????"
// ADDBOTAO V_Janela TEXTO "Enter=exibir selecionado" ;
//    ACAO EXIBIR_SELECIONADO(V_Janela) AJUDA "B19275"

// SEEK "99"
// CUA20 ESPECIALIZE V_Janela SELECAO SIMPLES ;
//     WHILE cdindx=="99" NAOROLAVERTICAL NAOROLAHORIZONTAL

// ANEXE V_Janela TITULO "Cd;moeda"  COLUNA cdindx
// ANEXE V_Janela TITULO "Data"      COLUNA dtcota
// ANEXE V_Janela TITULO "Cotação"   COLUNA TRANSFORM(vlcota,"@E 999,999,999,999.99999999")
// *
// ATIVE(V_Janela)
// *
// CLOSE COTACAO
// *
// ****************************
// STAT PROC EXIBIR_SELECIONADO(V_Janela)
// ****************************
// LOCAL N_Selecionado := ITENSSELECIONADOS(V_Janela)
// *
// MOSTRAR("M15642","O registro selecionado foi o RECNO() "+;
//         LTRIM(STR(N_Selecionado)))
// *

// **********************************************
// STAT PROC TST_BROWSE_ERRO_ROLAMENTO_HORIZONTAL()
// **********************************************
// LOCAL V_Janela

// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//    USE dados\cotacao NEW SHARED
//    SET INDEX TO dados\cotacao
// #elif defined(__PLATFORM__LINUX) || defined(__PLATFORM__Linux)   // ADAPTACAO_LINUX
//    USE /opt/cuadados/cotacao NEW SHARED
//    SET INDEX TO /opt/cuadados/cotacao
// #else
//    #erro "Código não adaptado para esta plataforma"
// #endif

// GOTO TOP
// *
// CUA20 @ 07,03,30,59 JANELA V_Janela ;
//     TITU "Browse de arquivo DBF" ;
//     SUBTITULO "%T;com BUG no rolamento horizontal / DISPBOX()" ;
//     AJUDA "T?????"
// CUA20 ESPECIALIZE V_Janela SELECAO SIMPLES
// *
// ANEXE V_Janela TITULO "A2"     COLUNA REPLICATE("A",2)
// ANEXE V_Janela TITULO "B3"     COLUNA REPLICATE("B",3)
// ANEXE V_Janela TITULO "C4"     COLUNA REPLICATE("C",4)
// ANEXE V_Janela TITULO "D5"     COLUNA REPLICATE("D",5)
// ANEXE V_Janela TITULO "E6"     COLUNA REPLICATE("E",6)
// ANEXE V_Janela TITULO "F7"     COLUNA REPLICATE("F",7)
// ANEXE V_Janela TITULO "G8"     COLUNA REPLICATE("G",8)
// ANEXE V_Janela TITULO "H9"     COLUNA REPLICATE("H",9)
// ANEXE V_Janela TITULO "I8"     COLUNA REPLICATE("I",8)
// ANEXE V_Janela TITULO "J7"     COLUNA REPLICATE("J",7)
// ANEXE V_Janela TITULO "K6"     COLUNA REPLICATE("K",6)
// ANEXE V_Janela TITULO "L5"     COLUNA REPLICATE("L",5)
// ANEXE V_Janela TITULO "M4"     COLUNA REPLICATE("M",4)
// ANEXE V_Janela TITULO "N3"     COLUNA REPLICATE("N",3)
// ANEXE V_Janela TITULO "O2"     COLUNA REPLICATE("O",2)
// ANEXE V_Janela TITULO "P3"     COLUNA REPLICATE("P",3)
// ANEXE V_Janela TITULO "Q4"     COLUNA REPLICATE("Q",4)
// ANEXE V_Janela TITULO "R5"     COLUNA REPLICATE("R",5)
// ANEXE V_Janela TITULO "S6"     COLUNA REPLICATE("S",6)
// ANEXE V_Janela TITULO "T7"     COLUNA REPLICATE("T",7)
// ANEXE V_Janela TITULO "U8"     COLUNA REPLICATE("Y",8)
// ANEXE V_Janela TITULO "V9"     COLUNA REPLICATE("V",9)
// *
// ATIVE(V_Janela)
// *
// CLOSE COTACAO
// *
// ********************************************
// STAT PROC TST_BROWSE_ERRO_ROLAMENTO_VERTICAL()
// ********************************************
// LOCAL V_Janela

// #if defined(__PLATFORM__WINDOWS) || defined(__PLATFORM__Windows)
//    USE dados\cotacao NEW SHARED
// #elif defined(__PLATFORM__LINUX) || defined(__PLATFORM__Linux)   // ADAPTACAO_LINUX
//    USE /opt/cuadados/cotacao NEW SHARED
// #else
//    #erro "Código não adaptado para esta plataforma"
// #endif

// //SET INDEX TO dados\cotacao
// *
// * BUG só ocorre quando se chega ao EOF via PgDn (seta pra cima/baixo),
// * por isto está sendo limitada a quantidade de registros visíveis
// SET FILTER TO RECNO() <= 48
// *
// GOTO TOP
// *
// * ATENÇÃO: Imitando a rotina Z:\F\XX\XX1.PRG, onde o erro ocorria...
// *          Ter "coluna congelada", ter "rolamento horizontal", "ter grade"
// *          ou "ter cláusula COLUNA" não efeta o aparecimento ou não do BUG.
// *          Também fazer com que a WVW_PAINT() não tenha efeito não desativou
// *          o BUG, não sendo, portanto causado pelos "code block" chamados
// *          em "backgroud" pela WVW_PAINT().
// *
// *          Para causar o a BUG basta: Teclar "PgDN" em seguida "seta para cima".
// *
// CUA20 @ 01,00,MAXROW(),MAXCOL() JANELA V_Janela ;
//     TITU "Browse de arquivo DBF" ;
//     SUBTITULO "%T;com BUG no rolamento vertical / ?????????()" ;
//     AJUDA "T?????"
// CUA20 ESPECIALIZE V_Janela SELECAO SIMPLES SEMGRADE // CONGELAR 3 NAOROLAHORIZONTAL
// *
// ANEXE  V_Janela TITU "UF"                       COLUNA REPLICATE("U",2)
// ANEXE  V_Janela TITU "Código"                   COLUNA RIGHT(STRZERO(RECNO(),9), 3)
// ANEXE  V_Janela TITU "Nome da Cidade"           COLUNA REPLICATE("C",30)
// ANEXE  V_Janela TITU "Unidade gestora centralizadora"  COLUNA REPLICATE("U",50)
// ANEXE  V_Janela TITU "Tipo"                     COLUNA RIGHT(STRZERO(RECNO(),9), 9) // Ex: "Principal"
// *
// ATIVE(V_Janela)
// *
// CLOSE COTACAO
// *
// ***********************************
