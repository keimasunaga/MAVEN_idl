;------------------------------------
;Set window properties
device, decomposed=0
loadct,39


;-----------------------------------
;Set time span (Oct. 21st 2014)
timespan, '2014-12-05'


;-----------------------------------
;Load static data
mvn_sta_l2_load, sta_apid=['c0','c6']


;------------------------------------
;Generate tplot structures for STATIC
mvn_sta_l2_tplot


;-------------------------------------
;View names of STATIC tplot structures
;tplot_names,'*sta*'
        

;------------------------------------
;Plot a variety of data
window, 1
tplot,['mvn_sta_c0_E',$
       'mvn_sta_c6_M']





;******************************************
;Add MAGNETOMETER Data to tplot

;----------------------------------------------
;Load Full Resolution
mvn_mag_load,'L1_FULL'

;-----------------------------------------------
;Load 1 second resolution
mvn_mag_load,'L1_1SEC'

;-----------------------------------------------
;Load 30 second resolution in STATIC Corrdinates
mvn_mag_load,'L1_30SEC', spice_frame='MAVEN_STATIC'

;-----------------------------------------------
;Add mag data to tplot
tplot, [17,18,19,20], /add



end
