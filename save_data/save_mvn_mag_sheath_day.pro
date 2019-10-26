pro save_mvn_mag_sheath_day,orbit


     env = init_env()
     SAVE_LOC = env.SAVE_LOC
     file_sw_info = SAVE_LOC + '/maven/sav/solar_wind_info/15min/sw_info'+string(orbit,format='(i05)')+'.sav'
     restore,file_sw_info
     time = dat_sw.time
     
     tplot_saved_mvn_pfp,orbit=orbit
     get_data,'Xmso',data=Xmso
     get_data,'Ymso',data=Ymso
     get_data,'Zmso',data=Zmso
     
     
     
     
     idx = nn('mvn_B_1sec_MAVEN_MSO_tot',time_in)  
     

    



end