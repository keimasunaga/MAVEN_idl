pro test_mvn


st = '2014-11-14'
timespan,st
mvn_sta_l2_load,sta_apid=['c0','c6']
mvn_sta_l2_tplot
mvn_swia_load_l2_data,/loadall,/tplot,/eflux
mvn_swe_load_l2,/spec
;mvn_swe_sumplot,/eph,/orb,/loadonly
mvn_swe_etspec,/silent,unit='eflux'
mvn_mag_load,'L1_1SEC'

tplot_vec_tot,'mvn_B_1sec'
tplot_vec_tot,'mvn_swim_velocity_mso'
options,'mvn_B_1sec',colors=[80,120,230]
options,'mvn_swim_velocity*',colors=[80,120,230]
tplot_options,'var_label',['orbnum','lat','lon']

tplot,['swe_a4','mvn_sta_c6_E','mvn_sta_c6_M','mvn_swics_en_eflux','mvn_swim_density','mvn_swim_velocity_mso','mvn_B_1sec','alt2','sza']

end
