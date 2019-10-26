pro main_contour_efield_agl,orbit


    env = init_env()
    SAVE_LOC = env.SAVE_LOC
    filename = SAVE_LOC + '/maven/sav/solar_wind_info/15min/sw_info_'+string(orbit,format='(i05)')+'.sav'
    restore,filename
    
    
    Vsw = dat_sw.vel
    Bsw = dat_sw.mag_mso
    Esw_in = -crossp(Vsw[*,0],Bsw[*,0])*1e-6 & Esw_out = -crossp(Vsw[*,1],Bsw[*,1])*1e-6
    Esw_avg = (Esw_in + Esw_out)/2.
    time_in = dat_sw.time[0] & time_out = dat_sw.time[1]
    time_avg = (time_in + time_out)/2d
    
    tplot_saved_mvn_pfp_orbit,orbit,/noplot
    get_data,'lon_',dat=lon
    get_data,'lat_',dat=lat
    get_data,'alt_',dat=alt
    idx_peri = where(alt.y eq min(alt.y))
    
    !p.multi=[0,2,1]
    plot_mgs_bfield,mult='2,2',title='#'+string(orbit,format='(i05)')+' Inbound condition'
    contour_vec_angle_bmap,Esw_in,time_in,/over
    oplot,lon.y, lat.y, psym=6,symsize=.2
    plots,lon.y[idx_peri],lat.y[idx_peri], psym=1,symsize=2,thick=3,/data
    plots,lon.y[0],lat.y[0], psym=1,color=230,/data 
    plots,lon.y[n_elements(lon.y)-1],lat.y[n_elements(lon.y)-1], psym=1,color=80,/data 
    
    plot_mgs_bfield,/add,title='#'+string(orbit,format='(i05)')+' Outbound condition'
    contour_vec_angle_bmap,Esw_out,time_out,/over
    oplot,lon.y, lat.y, psym=6,symsize=.2
    plots,lon.y[idx_peri],lat.y[idx_peri], psym=1,symsize=2,thick=3,/data
    plots,lon.y[0],lat.y[0], psym=1,color=230,/data
    plots,lon.y[n_elements(lon.y)-1],lat.y[n_elements(lon.y)-1], psym=1,color=80,/data
       
    plot_mgs_bfield,/add, title='#'+string(orbit,format='(i05)')+' Average condition'
    contour_vec_angle_bmap,Esw_avg,time_avg,/over
    oplot,lon.y, lat.y, psym=6,symsize=.2
    plots,lon.y[idx_peri],lat.y[idx_peri], psym=1,symsize=2,thick=3,/data
    plots,lon.y[0],lat.y[0], psym=1,color=230,/data
    plots,lon.y[n_elements(lon.y)-1],lat.y[n_elements(lon.y)-1], psym=1,color=80,/data
    !p.multi=[0,1,1]



end