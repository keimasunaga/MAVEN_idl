pro tsave_mvn_euv,trange=trange


    env = init_env()
    SAVE_LOC = env.SAVE_LOC
    
    mvn_euv_load,trange=trange,/all
    get_data,'mvn_euv_data',data=euv
    get_data,'mvn_euv_flag',data=flg
    idx = where(flg.y eq 0)
    euv_new = {x:euv.x[idx], y:euv.y[idx,*]}
    store_data,'EUVM',data=euv_new
    tplot_save,'EUVM',file=SAVE_LOC+'/maven/tplot_save/euv/euv'
    
end