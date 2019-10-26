pro calc_sw_temp

;    trange1 = ['2015-01-25/00:00','2015-01-26/23:59']
;    mvn_swe_load_l2,trange1,/spec
;    mvn_swe_sc_pot
;    mvn_swe_n1d,mom=mom,pans=more_pans
;
;    mvn_swia_load_l2_data,trange=trange1,/loadfine,/tplot
;    mvn_swia_protonalphamoms
    get_data,'mvn_swe_spec_temp',data=dat_Te
    get_data,'tproton',data=dat_Ti
;    
;    Ti_tot = total(dat_Ti.y^2,2)^.5
;    store_data,'tproton_tot',data={x:dat_Ti.x, y:Ti_tot}
;   
;    options,'tproton',colors=[80,120,230]
;    options,'tproton','labels',['Tx','Ty','Tz'],labflag=1
;    tplot,['mvn_swe_sc_pot','mvn_swe_spec_temp','tproton_tot','alt2']
;    stop
    ctime,t
    tavg = [t-900d, t]
    idx_Te = nn('mvn_swe_spec_temp',tavg)
    idx_Ti = nn('tproton_tot',tavg)
    
    
    Te = avg(dat_Te.y[idx_Te[0]:idx_Te[1]],/NaN)
    Ti = avg(dat_Ti.y[idx_Ti[0]:idx_Ti[1]])
    
    print,Te,Ti, '[eV]'
    print,Te * 11600., Ti*11600., '[K]'
    stop
    
    idx_v = nn('mvn_swica_velocity_mso',tavg)
    get_data,'mvn_swica_velocity_mso',data=v
    vsw = total(v.y^2,2)^.5
    vsw_avg = avg(vsw[idx_v[0]:idx_v[1]])
    
    idx_n = nn('mvn_swica_density',tavg)
    get_data,'mvn_swica_density',data=n
    nsw_avg = avg(n.y[idx_n[0]:idx_n[1]])
    
    idx_b = nn('mvn_B_1sec_MAVEN_MSO',tavg)
    get_data,'mvn_B_1sec_MAVEN_MSO',data=b
    bsw_avg = avg(b.y[idx_b[0]:idx_b[1],*],0)
    
    print,nsw_avg,vsw_avg,bsw_avg
    stop
end