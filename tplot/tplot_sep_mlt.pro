pro tplot_sep_mlt,date_arr


    env = init_env()
    SAVE_LOC = env.SAVE_LOC
    
    date_arr = ['20141018','20141019','20141020','20141021','20141022',$
            '20141023','20141024','20141025','20141026','20141027',$
            '20141028','20141029','20141030','20141031']
    
    var_sep1_A = ''
    var_sep1_B = ''
    var_sep2_A = ''
    var_sep2_B = ''
    var_swe = ''
    var_swi = ''
    
    for i=0,n_elements(date_arr)-1 do begin
    
;      filename = SAVE_LOC + '/maven/tplot_save/tplot_pfp_'+date_arr[i]
;      tplot_restore,filename
      tplot_saved_mvn_pfp,date=date_arr[i],/noplot
      ;stop
      store_data,'mvn_sep1_A-F_Eflux_Energy',new='sep1_A-F_'+strcompress(string(fix(i)),/remove)
      store_data,'mvn_sep1_B-O_Eflux_Energy',new='sep1_B-O_'+strcompress(string(fix(i)),/remove)
      store_data,'mvn_sep2_A-F_Eflux_Energy',new='sep2_A-F_'+strcompress(string(fix(i)),/remove)
      store_data,'mvn_sep2_B-O_Eflux_Energy',new='sep2_B-O_'+strcompress(string(fix(i)),/remove)
      store_data,'mvn_swe_et_spec_svy',new='swe_'+strcompress(string(fix(i)),/remove)
      store_data,'mvn_swica_en_eflux',new='swi_'+strcompress(string(fix(i)),/remove)
      
      ;stop
      var_sep1_A = [var_sep1_A,'sep1_A-F_'+strcompress(string(fix(i)),/remove)]
      var_sep1_B = [var_sep1_B,'sep1_B-O_'+strcompress(string(fix(i)),/remove)]
      var_sep2_A = [var_sep2_A,'sep2_A-F_'+strcompress(string(fix(i)),/remove)]
      var_sep2_B = [var_sep2_B,'sep2_B-O_'+strcompress(string(fix(i)),/remove)]
      var_swe = [var_swe,'swe_'+strcompress(string(fix(i)),/remove)]
      var_swi = [var_swi,'swi_'+strcompress(string(fix(i)),/remove)]
      
      ;stop
    endfor
    
     var_sep1_A = var_sep1_A[1:*]
     var_sep1_B = var_sep1_B[1:*]
     var_sep2_A = var_sep2_A[1:*]
     var_sep2_B = var_sep2_B[1:*]
     var_swe = var_swe[1:*]
     var_swi = var_swi[1:*]
      
      store_data,'sep_1_A-F_mlt',data=var_sep1_A
      store_data,'sep_1_B-O_mlt',data=var_sep1_B
      store_data,'sep_2_A-F_mlt',data=var_sep1_A
      store_data,'sep_2_B-O_mlt',data=var_sep1_B
      store_data,'swe_mlt',data=var_swe
      store_data,'swi_mlt',data=var_swi
      
      get_data,'sep1_A-F_10',dl=dl
      ylim,'sep*mlt',dl.yrange
      ylim,'swe_mlt',5,30e3
      ylim,'swi_mlt',5,30e3
      timespan,date_arr[0],n_elements(date_arr),/day
      tplot_options,'tickinterval',3600d*24
      tplot,['sep_1_B-O_mlt','sep_2_B-O_mlt','sep_1_A-F_mlt','sep_2_A-F_mlt','swi_mlt','swe_mlt']
      ;tplot_save,['sep*','swi_mlt','swe_mlt'],file=SAVE_LOC+'/maven/tplot_save/css/sep/sep_css'
end