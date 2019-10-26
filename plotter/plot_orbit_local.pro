pro plot_orbit_local


    env = init_env()
    SAVE_LOC_HEAVY = env.SAVE_LOC_HEAVY

    filename = SAVE_LOC_HEAVY + '/maven/vdf/oxy/00564/vdf_20150113190004.sav'
    restore,filename
    
    v=dat_sav.vmso
    b = dat_sav.bmso
    rot = get_rot_angle(v,b)
    
    plot_mvn_orbit,orbit=564,angle=rot,/show_time,frame='MSE'
    
    
end