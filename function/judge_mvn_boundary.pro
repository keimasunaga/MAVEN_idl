function judge_mvn_boundary,time,orbit=orbit

    env = init_env()
    SAVE_LOC = env.SAVE_LOC
    restore,SAVE_LOC+'/maven/sav/time_boundary/tb_'+string(orbit,format='(i05)')+'.sav'
    
    t_bs = dat_boundary.t_bs
    t_imb = dat_boundary.t_imb
    
    if time lt t_bs[0] or time gt t_bs[1] then region = 'sw'
    if (time gt t_bs[0] and time lt t_imb[0]) or (time gt t_imb[1] and time lt t_bs[1]) then region = 'ms'
    if time gt t_imb[0] and time lt t_imb[1] then region = 'im'
    
    return,region    
    
end