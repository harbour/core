/*=======Ver: 7.0.60906========*/
/*DHTMLMenu, (c) 2006, SourceTec Software Co.,LTD  -  www.sothink.com*/
function stfltshB(p){if(!p.eff[0]) return 1;var l=p._shell,fl=l.filters;stfltsp(p);fl[0].apply();return 1;}
function stfltshE(p){if(!p.eff[0]) return 1;var l=p._shell,fl=l.filters;if(fl)fl[0].play();return 1;}
function stflthdB(p){if(!p.eff[1]) return 1;var l=p._shell,fl=l.filters;if(fl){stfltsp(p);fl[p.eff[0]?1:0].apply();}return 1;}
function stflthdE(p){if(!p.eff[1]) return 1;var l=p._shell,fl=l.filters;if(fl)fl[p.eff[0]?1:0].play();return 1;}
function stfltsp(p){var l=p._shell,fl=l.filters,n=0;if(p.eff[0]) n++;if(p.eff[1]) n++;for(var j=0;j<n;j++)if(fl[j].status) fl[j].stop();}
