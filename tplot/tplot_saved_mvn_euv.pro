;       channels A (17-22 nm), B (0-7 nm), and C (121-122 nm)

pro tplot_saved_mvn_euv,noplot=noplot

    env = init_env()
    SAVE_LOC = env.SAVE_LOC
    
    filename = SAVE_LOC + '/maven/tplot_save/euv/euv_v2.tplot'
    tplot_restore,file=filename
    options,'EUVM','labels',['A (17-22nm)', 'B (0-7nm)', 'C (121-122nm)']
    if not keyword_set(noplot) then tplot,'EUVM'

end