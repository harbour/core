/* encoding: cp850 */
* cua20.ch
* Contém a definição dos comandos da padronização SAA/CUA.
*
* Versäo 2.0   Junho/2009
*/

#ifndef CUA20_CH
#define CUA20_CH

* Janela

 //!! O OITAVO PARAMETRO DA ROTINA ABAIXO DEVE SER POSTERIORMENTE REMOVIDO.

// Não tem a opção TECLAS

#command CUA20 @ <topo>,<esq>,<baixo>,<dir> JANELA <objjanela> ;
         [TITULO <soumalinha>] [SUBTITULO <linhas>] ;
         [DESLOCACAB <posicoes_desloc>];
         [ESPACOPIXELS <espaco_pixels>] [EMBUTIDA <embut>] ;
         AJUDA <help> ;
         => <objjanela> := CriarJanela20(<topo>,<esq>,<baixo>,<dir>,<soumalinha>,;
                                         <linhas>,<help>,NIL,<posicoes_desloc>,;
                                         <espaco_pixels>,<embut>)

// Exclusivo 2.0
#command ADDBOTAO <objjanela> TEXTO <textobotao>  ;
         ACAO <expr_acao> [<com_autoclose: AUTOCLOSE>] ;
         [<com_recnomuda: RECNOMUDA>] ;
         [<com_filtermuda: FILTERMUDA>] ;
         [<com_ordermuda: ORDERMUDA>] ;
         [<com_eofok: EOFOK>] ;
         [<com_handlemuda: HANDLEMUDA>] ;
         [<com_mudadados: MUDADADOS>] AJUDA <help> => ;
         AddBotao(<objjanela>,<textobotao>,;
                  <{expr_acao}>,<.com_autoclose.>,<help>,;
                  .f.,<.com_recnomuda.>,<.com_filtermuda.>,;
                  <.com_ordermuda.>,<.com_eofok.>,<.com_handlemuda.>,<.com_mudadados.>)

// Exclusivo 2.0
#command ADDACAO <objjanela> INKEY <codigoInkey>  ;
         ACAO <expr_acao> [<com_autoclose: AUTOCLOSE>] ;
         [<com_recnomuda: RECNOMUDA>] ;
         [<com_filtermuda: FILTERMUDA>] ;
         [<com_ordermuda: ORDERMUDA>] ;
         [<com_eofok: EOFOK>] ;      
         [<com_handlemuda: HANDLEMUDA>] ;
         [<com_mudadados: MUDADADOS>] AJUDA <help> => ;
        AddAcao(<objjanela>,<codigoInkey>,;
                <{expr_acao}>,<.com_autoclose.>,<help>,;
                .f.,<.com_recnomuda.>,<.com_filtermuda.>,;
                <.com_ordermuda.>,<.com_eofok.>,<.com_handlemuda.>,<.com_mudadados.>)

// Não tem a opção KEYBOARD
// Tem a opção ACAO e AUTOCLOSE
#command CUA20 ADDIMAGEM <objjanela> ARQUIVO <arquivoimagem> ;
         COORDENADAS <topo>,<esq>,<baixo>,<dir> ;
         [ACAO <expr_acao>] [<com_autoclose: AUTOCLOSE>] ;
            [<com_recnomuda: RECNOMUDA>] ;
            [<com_filtermuda: FILTERMUDA>] ;
            [<com_ordermuda: ORDERMUDA>] ;
            [<com_eofok: EOFOK>] ;
            [<com_handlemuda: HANDLEMUDA>] ;
            [<com_mudadados: MUDADADOS>] AJUDA <help> => ;
         AddImagem20(<objjanela>,<arquivoimagem>,<topo>,<esq>,<baixo>,<dir>,;
                     <{expr_acao}>,<.com_autoclose.>,<help>,;
                     .f.,<.com_recnomuda.>,<.com_filtermuda.>,;
                     <.com_ordermuda.>,<.com_eofok.>,<.com_handlemuda.>,<.com_mudadados.>)

#command ADDPROGRESSBAR <objjanela>;
         COORDENADAS <topo>,<esq>,<baixo>,<dir>;
	 AJUDA <help> =>;
	 CRIA_PROGRESSBAR(<objjanela>,<topo>,<esq>,<baixo>,<dir>,<help>)

* Menu
                   
// Exclusivo 2.0
#command ESPECIALIZE <objjanela> MENU ;
         [<rolavert: ROLAVERTICAL>] [<com_autoclose: AUTOCLOSE>] ;
         => EspMenuVert(<objjanela>,<.rolavert.>,;
                        <.com_autoclose.>)        

// Exclusivo 2.0
#command ADDOPCAO <objjanela> TEXTO <textoopcao>  ;
         [ACAO <expr_acao>] ;
         [<com_recnomuda: RECNOMUDA>] ;
         [<com_filtermuda: FILTERMUDA>] ;
         [<com_ordermuda: ORDERMUDA>] ;
         [<com_eofok: EOFOK>] ;
         [<com_handlemuda: HANDLEMUDA>] ;
         [<com_mudadados: MUDADADOS>] AJUDA <help> => ;
         AddOpcao(<objjanela>,<textoopcao>,;
                  <{expr_acao}>,<help>,;
                  .f.,<.com_recnomuda.>,<.com_filtermuda.>,;
                  <.com_ordermuda.>,<.com_eofok.>,<.com_handlemuda.>,<.com_mudadados.>)

* selecao em arquivo (Browse)
// Não tem a opção TERMINAR
// Tem a opção AUTOCLOSE
                  
#command CUA20 ESPECIALIZE <objjanela> SELECAO SIMPLES ;
         [WHILE <cond>] [CONGELAR <num>] ;
         [<naorolavert: NAOROLAVERTICAL>] [<naorolahori: NAOROLAHORIZONTAL>] ;
         [<semgrad: SEMGRADE>] [<sem_toolbar: SEMTOOLBAR>] ;
         [<com_autoclose: AUTOCLOSE>]  => ;
         EspSelArq20(<objjanela>,1,<.semgrad.>,;
         <.naorolavert.>,<.naorolahori.>,<.sem_toolbar.>,;
         <.com_autoclose.>,<num>,<{cond}>)
#command CUA20 ESPECIALIZE <objjanela> SELECAO MULTIPLA  ;
         [WHILE <cond>] [CONGELAR <num>] ;
         [<naorolavert: NAOROLAVERTICAL>] [<naorolahori: NAOROLAHORIZONTAL>] ;
         [<semgrad: SEMGRADE>] [<sem_toolbar: SEMTOOLBAR>] ;
         [<com_autoclose: AUTOCLOSE>] => ;
         EspSelArq20(<objjanela>,2,<.semgrad.>,;
         <.naorolavert.>,<.naorolahori.>,<.sem_toolbar.>,;
         <.com_autoclose.>,<num>,<{cond}>)
#command CUA20 ESPECIALIZE <objjanela> SELECAO ESTENDIDA ;
         [WHILE <cond>] [CONGELAR <num>] ;
         [<naorolavert: NAOROLAVERTICAL>] [<naorolahori: NAOROLAHORIZONTAL>] ;
         [<semgrad: SEMGRADE>] [<sem_toolbar: SEMTOOLBAR>] ;
         [<com_autoclose: AUTOCLOSE>] => ;
         EspSelArq20(<objjanela>,3,<.semgrad.>,;
         <.naorolavert.>,<.naorolahori.>,<.sem_toolbar.>,;
         <.com_autoclose.>,<num>,<{cond}>)

* selecao em vetor (achoice)
// Não tem a opção TECLAS, COMGRADE, ROLAVERTICAL, ROLAHORIZONTAL, COMTOOLBAR
// Tem a opção SEMGRADE, NAOROLAVERTICAL, NAOROLAHORIZONTAL, SEMTOOLBAR
// Tem a opção AUTOCLOSE
         
#command CUA20 ESPECIALIZE <objjanela> SELECAO SIMPLES   VETOR <lista> ;
         [<naorolavert: NAOROLAVERTICAL>] <naorolahori: NAOROLAHORIZONTAL> ;
         [<semgrad: SEMGRADE>] [<sem_toolbar: SEMTOOLBAR>];
         [<com_autoclose: AUTOCLOSE>] => ;
         EspSelVet20(<objjanela>,1,<.semgrad.>,;
                    <.naorolavert.>,<.naorolahori.>,<.sem_toolbar.>,;
                    <.com_autoclose.>,<lista>)
#command CUA20 ESPECIALIZE <objjanela> SELECAO MULTIPLA  VETOR <lista> ;
         [<naorolavert: NAOROLAVERTICAL>] <naorolahori: NAOROLAHORIZONTAL> ;
         [<semgrad: SEMGRADE>] [<sem_toolbar: SEMTOOLBAR>];
         [<com_autoclose: AUTOCLOSE>] => ;
         EspSelVet20(<objjanela>,2,<.semgrad.>,;
                    <.naorolavert.>,<.naorolahori.>,<.sem_toolbar.>,;
                    <.com_autoclose.>,<lista>)
#command CUA20 ESPECIALIZE <objjanela> SELECAO ESTENDIDA VETOR <lista> ;
         [<naorolavert: NAOROLAVERTICAL>] <naorolahori: NAOROLAHORIZONTAL> ;
         [<semgrad: SEMGRADE>] [<sem_toolbar: SEMTOOLBAR>];
         [<com_autoclose: AUTOCLOSE>] => ;
         EspSelVet20(<objjanela>,3,<.semgrad.>,;
                    <.naorolavert.>,<.naorolahori.>,<.sem_toolbar.>,;
                    <.com_autoclose.>,<lista>)

#endif