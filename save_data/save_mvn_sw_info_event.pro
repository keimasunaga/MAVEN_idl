pro save_mvn_sw_info_event,orbit

    tplot_saved_mvn_pfp,orbit=orbit
    get_data,'mvn_swica_density',data=dens
    get_data,'mvn_swica_velocity_mso',data=vel
    get_data,'mvn_B_1sec_MAVEN_MSO',data=mag
    ctime,t
    trange = [t-900d,t]
    idx_dens = nn('mvn_swica_density',trange)
    idx_vel = nn('mvn_swica_velocity_mso',trange)
    idx_mag = nn('mvn_B_1sec_MAVEN_MSO',trange)

    dens_avg = avg(dens.y[idx_dens[0]:idx_dens[1]])
    vel_avg = avg(vel.y[idx_vel[0]:idx_vel[1],*],0)
    mag_avg = avg(mag.y[idx_mag[0]:idx_mag[1],*],0)


   
   print,dens_avg
   print,vel_avg
   print,mag_avg
   
  

end