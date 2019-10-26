pro main_get_vdf_vel,load=load
   
   env = init_env()
   SAVE_LOC = env.SAVE_LOC
   ;trange = time_double(['2015-01-13/11:20:20','2015-01-13/11:22:32'])
   trange = time_double(['2015-01-13/18:59:20','2015-01-13/19:01:24'])
   orbit = 564
   time = (trange[0]+trange[1])/2d
   date_s = strmid(time_string(time),0,4)+strmid(time_string(time),5,2)+strmid(time_string(time),8,2)+ $
     strmid(time_string(time),11,2)+strmid(time_string(time),14,2)+strmid(time_string(time),17,2)
     
   if keyword_set(load) then begin
    tplot_saved_mvn_pfp,orbit=orbit
    mvn_spice_load_kei
    mvn_sta_l2_load,sta_apid='d1'
   endif
   
   dat = mvn_sta_get('d1',tt=trange)
   sta_getdat_bgrm,dat,dat_new,ratio=0.5
   plot_sta_vdf_getvel,dat=dat_new,/local,/show,unit='flux',/backscat,angle=[-45,45],vdf_sav=vdf_sav
   filename = SAVE_LOC + '/maven/ascii/vdfparam/vdfparam_'+date_s+'.dat'
   format = '34(f15.8)'
   stop
   write_ascii,vdf_sav,filename,format


end