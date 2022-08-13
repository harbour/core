/* encoding: cp850 */
* cua.ch
* Contém a definição dos comandos da padronização SAA/CUA.
*
* Versão 1.0   Abril/92
*

#ifndef CUA_CH
#define CUA_CH

#command @ <topo>,<esq>,<baixo>,<dir> JANELA <objjanela> ;
         [TITULO <soumalinha>] [SUBTITULO <linhas>] ;
         [TECLAS <vetor>] [EMBUTIDA <embut>] ;
         [ESPACOPIXELS <espaco_pixels>] [DESLOCACAB <posicoes_desloc>];
         AJUDA <help> ;    
         => <objjanela> := CriarJanela(<topo>,<esq>,<baixo>,<dir>,<soumalinha>,;
                                       <vetor>,<help>,<embut>,<linhas>,<espaco_pixels>,;
                                       <posicoes_desloc>)
#command ADDIMAGEM <objjanela> ARQUIVO <arquivoimagem>  ;
         COORDENADAS <topo>,<esq>,<baixo>,<dir> ;
         [KEYBOARD <teclaskeyboard>] [AJUDA <help>] => ;
         AddImagem(<objjanela>,<arquivoimagem>,<topo>,<esq>,<baixo>,<dir>,;
                   <teclaskeyboard>,<help>)

#command MUDE SUBTITULO <objjanela> PARA <novosubtitulo> => MudeSubTitulo(<objjanela>,<novosubtitulo>)

#command DESTRUA <objjanela> => DestruaJan(<objjanela>)

* Janela auxiliar                   

#command FECHAR MSGAGUARDE <objjanela> => FecharMsgAguarde(<objjanela>)

* selecao em arquivo (Browse)
#command ANEXE <objjanela> [TITULO <linhas>] COLUNA <expr,...> => ;
         AnexeCol(<objjanela>,<linhas>,{||<expr>})
#command MUDE SELECAO <objjanela> PARA <novalista> => MudeLista(<objjanela>,<novalista>)
#command MUDE SELECAO <objjanela> PARA NENHUM => MudeLista(<objjanela>)
#command RELEIA CORRENTE <objjanela> => ReleiaCorrente(<objjanela>)
#command RELEIA TUDO     <objjanela> => ReleiaTudo(<objjanela>)

* selecao em vetor (achoice)

#command ESPECIALIZE <objjanela> TEXTO <car> [LARGURA <num>] ;
         [TABULACAO <quant>] [TERMINAR <vetor>] ;
         [VALID <valid>] [EDITA <edita>] [CONFIRMAR <bloc2>] ;
         [DESISTIR <bloc3>] [FILTRO <bloc4>] ;
         [<naorolavert: NAOROLAVERTICAL>] [<rolahori: ROLAHORIZONTAL>] ;
         [<sem_toolbar: SEMTOOLBAR>] => ;
         EspTexto(<objjanela>,{|X|IIF(X==NIL,<car>,<car>:=X)},<vetor>,;
         <num>,<quant>,<{valid}>,<{edita}>,<{bloc2}>,<{bloc3}>,<{bloc4}>,;
         <.naorolavert.>,<.rolahori.>,<.sem_toolbar.>)

#command ESPECIALIZE <objjanela> ARQUIVOTEXTO <handle> [LARGURA <num>] ;
         [TERMINAR <vetor>] [DESTAQUE <palavra>] ;
         [<naorolavert: NAOROLAVERTICAL>] [<naorolahori: NAOROLAHORIZONTAL>] ;
         [<sem_toolbar: SEMTOOLBAR>] => ;
         EspArqTexto(<objjanela>,<handle>,<num>,<vetor>,<palavra>,;
         <.naorolavert.>,<.naorolahori.>,<.sem_toolbar.>)

#command ESPECIALIZE <objjanela> ENTRADA [DATAERRADA <bloc1>] ;
         [CONFIRMAR <bloc2>]  [DESISTIR <bloc3>] [FILTRO <bloc4>] ;
         [EDITA <edita>] [<rolavert: ROLAVERTICAL>] ;
         [<sem_toolbar: SEMTOOLBAR>] => ;
         EspEntrada(<objjanela>,<{bloc1}>,<{bloc2}>,<{bloc3}>,<{bloc4}>,;
                    <{edita}>,<.rolavert.>,<.sem_toolbar.>)

#command @ <ent>,<row>,<col> SAY <xpr,...> [PICTURE <pic>] [COLOR <cor1>] ;
         => AnexeSay(<ent>,<row>,<col>,{||<xpr>},<pic>,<cor1>)

* 1.1) GET original
*      - Com cláusula PICTURE no GET
*      - Com cláusula LISTA...[AUTO...] 
*      - Sem cláusula DOMINIO...[AUTO...] [F4COMCODIGO] [SEMF4]
*      - Sem cláusula CAMPO...[AUTO...] [F4COMCODIGO] [SEMF4]
*      - Sem cláusula IDCAMPO...[AUTO...] [F4COMCODIGO] [SEMF4]
*      - Sem cláusula OPCIONAL
#command @ <ent>,<row>,<col> GET <var> [PICTURE <pic>] [COLOR <cor2>] ;
         [WHEN <when>] [VALID <valid>] [EDITA <edita>] ;
         [LISTA <lista> [AUTO <auto>]] [MESSAGE <mens>] AJUDA <help> ;
         => AnexeGet(<ent>,<row>,<col>,;
            GETNEW(,,{|setVal|IIF(setVal==NIL,<var>,<var>:=setVal) },<(var)>,<pic>),;
            <cor2>,<{when}>,<{valid}>,<{edita}>,<{lista}>,<{auto}>,<{mens}>,<help>,;
            NIL,NIL,NIL,NIL,NIL,NIL)

#command @ <ent>,<row>,<col> SAY <xpr,...> [PICTURE <pic1>] [COLOR <cor1>] ;
         GET <var> [PICTURE <pic2>] [COLOR <cor2>] ;
         [WHEN <when>] [VALID <valid>] [EDITA <edita>] ;
         [LISTA <lista> [AUTO <auto>]] [MESSAGE <mens>] AJUDA <help> ;
         => AnexSayGet(<ent>,<row>,<col>,{||<xpr>},<pic1>,<cor1>,;
            GETNEW(,,{|X|IIF(X==NIL,<var>,<var>:=X) },<(var)>,<pic2>),;
            <cor2>,<{when}>, <{valid}>,<{edita}>,<{lista}>,<{auto}>,<{mens}>,<help>,;
            NIL,NIL,NIL,NIL,NIL,NIL)


* 1.2) GET com cláusula DOMINIO
*      - Sem cláusula PICTURE no GET
*      - Sem cláusula LISTA...[AUTO...] 
*      - Com cláusula DOMINIO...[AUTO...] [F4COMCODIGO] [SEMF4]
*      - Sem cláusula CAMPO...[AUTO...] [F4COMCODIGO] [SEMF4]
*      - Sem cláusula IDCAMPO...[AUTO...] [F4COMCODIGO] [SEMF4]
*      - Com cláusula OPCIONAL
#command @ <ent>,<row>,<col> GET <var> [COLOR <cor2>] ;
         [WHEN <when>] [VALID <valid>] [EDITA <edita>] ;
         DOMINIO <O_Dominio> ;
              [AUTO <auto>] [<F4ComCodigo: F4COMCODIGO>] ;
              [<semF4: SEMF4>] [<opcional: OPCIONAL>];
         [MESSAGE <mens>] AJUDA <help> ;
         => AnexeGet(<ent>,<row>,<col>,;
            GETNEW(,,{|setVal|IIF(setVal==NIL,<var>,<var>:=setVal) },<(var)>,NIL),;
            <cor2>,<{when}>,<{valid}>,<{edita}>,NIL,<{auto}>,<{mens}>,<help>,;
            <O_Dominio>,NIL,NIL,<.F4ComCodigo.>,<.semF4.>,<.opcional.>)

#command @ <ent>,<row>,<col> SAY <xpr,...> [PICTURE <pic1>] [COLOR <cor1>] ;
         GET <var> [COLOR <cor2>] ;
         [WHEN <when>] [VALID <valid>] [EDITA <edita>] ;
         DOMINIO <O_Dominio> ;
              [AUTO <auto>] [<F4ComCodigo: F4COMCODIGO>] ;
              [<semF4: SEMF4>] [<opcional: OPCIONAL>];
         [MESSAGE <mens>] AJUDA <help> ;
         => AnexSayGet(<ent>,<row>,<col>,{||<xpr>},<pic1>,<cor1>,;
            GETNEW(,,{|X|IIF(X==NIL,<var>,<var>:=X) },<(var)>,NIL),;
            <cor2>,<{when}>, <{valid}>,<{edita}>,NIL,<{auto}>,<{mens}>,<help>,;
            <O_Dominio>,NIL,NIL,<.F4ComCodigo.>,<.semF4.>,<.opcional.>)


* 1.3) GET com cláusula CAMPO
*      - Sem cláusula LISTA...[AUTO...] 
*      - Sem cláusula DOMINIO...[AUTO...] [F4COMCODIGO] [SEMF4]
*      - Com cláusula CAMPO...[AUTO...] [F4COMCODIGO] [SEMF4]
*      - Sem cláusula IDCAMPO...[AUTO...] [F4COMCODIGO] [SEMF4]
*      - Com cláusula OPCIONAL
#command @ <ent>,<row>,<col> GET <var> [PICTURE <pic>] [COLOR <cor2>] ;
         [WHEN <when>] [VALID <valid>] [EDITA <edita>] ;
         CAMPO <O_Campo> ;
              [AUTO <auto>] [<F4ComCodigo: F4COMCODIGO>] ;
              [<semF4: SEMF4>] [<opcional: OPCIONAL>];
         [MESSAGE <mens>] AJUDA <help> ;
         => AnexeGet(<ent>,<row>,<col>,;
            GETNEW(,,{|setVal|IIF(setVal==NIL,<var>,<var>:=setVal) },<(var)>,<pic>),;
            <cor2>,<{when}>,<{valid}>,<{edita}>,NIL,<{auto}>,<{mens}>,<help>,;
            NIL,<O_Campo>,NIL,<.F4ComCodigo.>,<.semF4.>,<.opcional.>)

#command @ <ent>,<row>,<col> SAY <xpr,...> [PICTURE <pic1>] [COLOR <cor1>] ;
         GET <var> [PICTURE <pic2>] [COLOR <cor2>] ;
         [WHEN <when>] [VALID <valid>] [EDITA <edita>] ;
         CAMPO <O_Campo> ;
              [AUTO <auto>] [<F4ComCodigo: F4COMCODIGO>] ;
              [<semF4: SEMF4>] [<opcional: OPCIONAL>];
         [MESSAGE <mens>] AJUDA <help> ;
         => AnexSayGet(<ent>,<row>,<col>,{||<xpr>},<pic1>,<cor1>,;
            GETNEW(,,{|X|IIF(X==NIL,<var>,<var>:=X) },<(var)>,<pic2>),;
            <cor2>,<{when}>, <{valid}>,<{edita}>,NIL,<{auto}>,<{mens}>,<help>,;
            NIL,<O_Campo>,NIL,<.F4ComCodigo.>,<.semF4.>,<.opcional.>)

#INCLUDE "cua20.ch"

#endif