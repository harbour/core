function main()
local nver,nmar,ntype,nir,ppp

nmar:=FT_MVERSION(@nver,@ntype,@nir)
ppp:=nmar+nver
? str(nmar,2,0),'.',str(nver,2,0)
? ppp/100
inkey(0)
? 'is mouse on', ft_mreset()
inkey(0)
? FT_MSHOWCRS()
inkey(0)
? ft_mxlimit(0,8*maxcol())
inkey(0)
? ft_mylimit(0,8*maxrow())
inkey(0)

do while lastkey()<>27
? 'mouse row is',ft_mgetx()
? 'mouse col is',ft_mgety()
if lastkey()==27
   exit
endif
enddo
FT_MHIDECRS()
return nil

