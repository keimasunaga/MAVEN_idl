pro tplot_saved_mvn_sw_temp,noplot=noplot

  env = init_env()
  SAVE_LOC = env.SAVE_LOC

  filename = SAVE_LOC + '/maven/tplot_save/mom/sw_temp.tplot'
  tplot_restore,file=filename
  if not keyword_set(noplot) then tplot,['Te','Ti']

end