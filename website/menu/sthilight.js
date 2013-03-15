/*=======Ver: 7.0.60906========*/
/*DHTMLMenu, (c) 2006, SourceTec Software Co.,LTD  -  www.sothink.com*/
function stisL(h,tar,c,w){var t=stgtW(tar,w);var u=t?t.location.href:"";if(!c){u=u.toLowerCase();h=h.toLowerCase();}return 	u&&h&&(u==h||u==h+"/"||u==h+"#"||STM_ILINK&&u==h.substr(0,Math.max(0,h.indexOf("?")))||STM_ILOC&&h==u.substr(0,Math.max(0,u.indexOf("?"))));}
function stshlp(p){var m=st_ms[p.mid];if(m.ckhd) return;	if(m.lits&0x08000000){for(var j=0;j<m.lnks.length;j++){var is=m.lnks[j].dat,pp=[],sn=0,i;		for(var l=0;l<is.length;l++){i=is[l];if(i.ishl){do{pp.push(i.parP);i=i.parP.parI;}while(i)}			if(m.lits&0x10000000)sn=Math.max(pp.length-m.litl,0);for(var q=pp.length-1;q>=sn;q--){				clearTimeout(pp[q].tid);if(!pp[q].isSh){var los=pp[q].lock;pp[q].lock=0;stshP(pp[q]);pp[q].lock=los;}}}}}}
function stgtW(t,w){if(t=="_self")return w;else if(t=="_parent")return w.parent;else if(t=="_top")return w.top;else return parent.frames[t];return 0;}
