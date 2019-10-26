pro tplot_mvn_lpw_test,st
if ~keyword_set(st) then st = '2014-11-13';'2014-10-21'
timespan,st,4,/hours
mvn_lpw_cdf_read,st;,vars=['wspecact','wspeccas']
tplot,['mvn_lpw_w_spec_act_l2','mvn_lpw_w_spec_pas_l2','mvn_lpw_w_e12_burst_lf_l2','mvn_lpw_w_e12_burst_mf_l2','mvn_lpw_w_e12_burst_hf_l2','mvn_lpw_w_n_l2','mvn_lpw_lp_iv_l2','mvn_lpw_lp_n_t_l2','mvn_lpw_mrg_sc_pot_l2']
;get_data,dlimit=dl
;help,dl

end