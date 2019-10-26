pro save_mvn_time_boundaries,orbit=orbit,no_bs=no_bs
         
         env = init_env()
         SAVE_LOC = env.SAVE_LOC
         tplot_saved_mvn_pfp_orbit,orbit
         
         
         if ~keyword_set(no_bs) then begin
           print,'Click FOUR times to determine bow shock (BS) and induced magnetopshere boudary (IMB) positions'
           print,'The first two clicks correspond to the timing when mvn crosses BS.'
           print,'The last two clicks correspond to the timing when mvn crosses IMB.'
           ctime,t_boundary
         
           t_bs = t_boundary[0:1]
           t_imb = t_boundary[2:3]          
           idx_bs = nn('Xmso',t_bs)
           idx_imb = nn('Xmso',t_imb)
           
           print,time_string(t_bs)
           print,time_string(t_imb)
           idx_bs = nn('Xmso',t_bs)
           idx_imb = nn('Xmso',t_imb)
           get_data,'Xmso',data=Xmso
           get_data,'Ymso',data=Ymso
           get_data,'Zmso',data=Zmso
           
           pos_bs = [[Xmso.y[idx_bs[0]],Ymso.y[idx_bs[0]],Zmso.y[idx_bs[0]]],[Xmso.y[idx_bs[1]],Ymso.y[idx_bs[1]],Zmso.y[idx_bs[1]]]]
           pos_imb = [[Xmso.y[idx_imb[0]],Ymso.y[idx_imb[0]],Zmso.y[idx_imb[0]]],[Xmso.y[idx_imb[1]],Ymso.y[idx_imb[1]],Zmso.y[idx_imb[1]]]]

           dat_boundary = {t_bs:t_bs, t_imb:t_imb, pos_bs:pos_bs, pos_imb:pos_imb}
           save,dat_boundary,file=SAVE_LOC + '/maven/sav/time_boundary/tb_'+string(orbit,format='(i05)')+'.sav'
         endif else begin
           print,'Click TWO times to determine the induced magnetopshere boudary (IMB) positions'
           print,'The two clicks correspond to the timing when mvn crosses IMB.'
           ctime,t_boundary

           t_bs = [!values.F_NaN, !values.F_NaN]
           t_imb = t_boundary

           print,time_string(t_imb)
           idx_imb = nn('Xmso',t_imb)
           get_data,'Xmso',data=Xmso
           get_data,'Ymso',data=Ymso
           get_data,'Zmso',data=Zmso

           pos_bs = [!values.F_NaN, !values.F_NaN]
           pos_imb = [[Xmso.y[idx_imb[0]],Ymso.y[idx_imb[0]],Zmso.y[idx_imb[0]]],[Xmso.y[idx_imb[1]],Ymso.y[idx_imb[1]],Zmso.y[idx_imb[1]]]]
           dat_boundary = {t_bs:t_bs, t_imb:t_imb, pos_bs:pos_bs, pos_imb:pos_imb}
           save,dat_boundary,file=SAVE_LOC + '/maven/sav/time_boundary/tb_'+string(orbit,format='(i05)')+'.sav'
          
          
          
          
          
          
          
         endelse

end