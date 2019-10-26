pro tsave_maven_orbit_tplot

     env = init_env()
     SAVE_LOC = env.SAVE_LOC
     
     del_data,'*'
     maven_orbit_tplot, /current;,/loadonl
     tplot_save,filename= SAVE_LOC + '/maven/tplot_save/mvn_ephemeris/mvn_ephemeris'
     
     ;['alt','sza','sheath','pileup','wake','wind','iono','twind','tsheath','tpileup','twake','period','palt','lon','lat']
end