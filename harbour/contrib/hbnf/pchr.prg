/*
 * $Id$
 */

/*
 * File......: pchr.prg
 * Author....: Jim Gale
 * CIS ID....: 73670,2561
 *
 * This is an original work by Jim Gale and is placed in the
 * public domain.
 *
 * Modification history:
 * ---------------------
 *
 *    Rev 1.2   17 Aug 1991 15:40:16   GLENN
 * Don Caton fixed some spelling errors in the doc
 *
 *    Rev 1.1   15 Aug 1991 23:06:00   GLENN
 * Forest Belt proofread/edited/cleaned up doc
 *
 *    Rev 1.0   12 Jun 1991 01:45:04   GLENN
 * Initial revision.
 *
 */

Function FT_PCHR(c_nums)
  Local c_ret:='', c_st:=0,c_part,c_st2,c_hex:="0123456789ABCDEF"
  Local c_upper,c_t1,c_t2

   If Substr(c_nums,1,1)==','.or.Trim(c_nums)==''
      Return ""
   Endif

   c_nums := Trim(c_nums) + ",~,"
   c_part := Substr(c_nums,c_st+1,At(",",Substr(c_nums,c_st+2)))

   Do While .not.(c_part=="~".or.c_part=="")

      If Substr(c_part,1,1)==Chr(34)

         c_st2:=At(Chr(34),Substr(c_part,2))+1
         c_ret:=c_ret+Substr(c_part,2,c_st2-2)

      Elseif Substr(c_part,1,1)=="&"

         c_upper:=Upper(c_part)
         c_t1:=At(Substr(c_upper,2,1),c_hex)-1
         If c_t1>-1
            c_t2:=At(Substr(c_upper,3,1),c_hex)-1
            If c_t2>-1
               c_t1:=c_t1*16+c_t2
            Endif
            c_ret:=c_ret+Chr(c_t1)
         Endif

      ElseIf (Val(c_part)>0.and.Val(c_part)<256).or.c_part=="0"

         c_ret:=c_ret+Chr(Val(c_part))

      Else

         If Substr(c_part,1,1)=="/"

            c_upper:=Upper(c_part)

            Do Case
               Case c_upper = '/GRAPHIC'
                  c_ret = c_ret + Chr(27)+Chr(116)+Chr(1)
               Case c_upper = '/ITALIC'
                  c_ret = c_ret + Chr(27)+Chr(116)+Chr(0)
               Case c_upper = '/PICTURE'
                  c_ret = c_ret + Chr(27)+Chr(116)+Chr(1)+;
                  Chr(27)+Chr(120)+Chr(1)+Chr(27)+Chr(107)+Chr(1)+;
                  Chr(27)+Chr(77)+Chr(27)+'U'
               Case c_upper = '/COND' .or. c_upper = '/SI'
                  c_ret = c_ret + Chr(15)
               Case c_upper = '/ROMAN'
                  c_ret = c_ret + Chr(27)+Chr(107)+Chr(0)
               Case c_upper = '/SANS'
                  c_ret = c_ret + Chr(27)+Chr(107)+Chr(1)
               Case c_upper = '/NLQ'
                  c_ret = c_ret + Chr(27)+Chr(120)+Chr(1)
               Case c_upper = '/DRAFT'
                  c_ret = c_ret + Chr(27)+Chr(120)+Chr(0)
               Case c_upper = '/ELITE'
                  c_ret = c_ret + Chr(27)+Chr(77)
               Case c_upper = '/PICA'
                  c_ret = c_ret + Chr(27)+Chr(80)
               Case c_upper = '/EMPHOFF'
                  c_ret = c_ret + Chr(27)+Chr(70)
               Case c_upper = '/EMPH'
                  c_ret = c_ret + Chr(27)+Chr(69)
               Case c_upper = '/1/6'
                  c_ret = c_ret + Chr(27)+Chr(50)
               Case c_upper = '/1/8'
                  c_ret = c_ret + Chr(27)+Chr(48)
               Case c_upper = '/SKIPOFF'
                  c_ret = c_ret + Chr(27)+Chr(79)
               Case c_upper = '/SKIP'
                  c_ret = c_ret + Chr(27)+Chr(78)
               Case c_upper = '/FF'.or.c_upper='/EJECT'
                  c_ret = c_ret + Chr(12)
               Case c_upper = '/INIT'.or.c_upper = '/RESET'
                  c_ret = c_ret + Chr(27)+Chr(64)
               Case c_upper = '/SPANISH'
                  c_ret = c_ret + Chr(27)+Chr(82)+Chr(12)
               Case c_upper = '/USA'
                  c_ret = c_ret + Chr(27)+Chr(82)+Chr(0)
               Case c_upper = '/ONE'
                  c_ret = c_ret + Chr(27)+'U'+Chr(1)
               Case c_upper = '/TWO'
                  c_ret = c_ret + Chr(27)+'U'+Chr(0)
               Case c_upper = '/FAST'
                  c_ret = c_ret + Chr(27)+'s'+Chr(0)
               Case c_upper = '/SLOW'
                  c_ret = c_ret + Chr(27)+'s'+Chr(1)
               Case c_upper = '/OFF'
                  c_ret = c_ret + Chr(19)
               Case c_upper = '/ON'
                  c_ret = c_ret + Chr(17)
               Case c_upper = '/BEEP' .or. c_upper='/BELL'
                  c_ret = c_ret + Chr(7)
               Case c_upper = '/CAN'
                  c_ret = c_ret + Chr(24)
            Endcase

         Endif

      Endif

      c_st = At(",",Substr(c_nums,c_st+1))+c_st
      c_part = Substr(c_nums,c_st+1,At(",",Substr(c_nums,c_st+2)))

   Enddo

Return c_ret
