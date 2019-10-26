pro main_plot_sta_vdf,trange=trange,remove_proton,ratio=ratio
    
    timespan,trange
    
    if size(trange[0],/type) eq 7 then trange=time_double(trange)  
    sta_apid = ['d1','cf']
    dat = mvn_sta_get(sta_apid,tt=trange)
    if keyword_set(remove_proton) then sta_getdat_bgrm,dat,dat_new,ratio=ratio
    plot_sta_vdf,dat=dat,/local,/show,units='df',mult='4,3',panel_mode=1



end