pro png_mvn_orbit,orbit

    env = init_env()
    SAVE_LOC = env.SAVE_LOC
    plot_mvn_orbit,orbit=orbit,/all,/show_time,charsize=1.3
    
    makepng,SAVE_LOC+'/maven/png/orbit_plot/orbit_mso_'+string(orbit,format='(i05)')

end