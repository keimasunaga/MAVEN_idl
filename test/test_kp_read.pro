pro test_kp_read

 maven_init
     env = init_env()
     DATA_LOC = env.DATA_LOC
     
     filename = DATA_LOC + 'maven/data/sci/kp/insitu/2015/01/mvn_kp_insitu_20150126_v02_r06.tab'
     nline = file_lines(filename)
     nhead = 339.
     sline = ''
     date = ''
     data = dblarr(146)
     time_arr = 0d
     Te_arr = 0.
     Ti_arr = 0.
     close,1
     openr,1,filename
     for ii=0,nhead-1 do readf,1,sline
     for ii=nhead,nline-1 do begin
      readf,1,date,data,format= '(a20,146f16.8)'
      time_arr = [time_arr,time_double(date)]
      Te_arr = [Te_arr, data[23]]
      Ti_arr = [Ti_arr, data[47]] 
      
     endfor
     time_arr = time_arr[1:*]
     Te_arr = Te_arr[1:*]
     Ti_arr = Ti_arr[1:*]
     close,1
     
     store_data,'Te',data={x:time_arr,y:Te_arr}
     store_data,'Ti',data={x:time_arr,y:Ti_arr}
     tplot,['Te','Ti']
     tr = ['2015-01-26/06:23:00','2015-01-26/06:38:00']
     idx_ti = nn('Ti',tr)
     Ti_avg = avg(Ti_arr[idx_ti[0]:idx_ti[1]])
     print,Ti_avg
     ;mvn_swia_protonalphamoms
     ;store_data,'Te_cmpr',data=['Te','mvn_swe_spec_temp']
     ;store_data,'Ti_cmpr',data=['Ti','mvn_swica_temperature']
     ;stop
     ;mvn_kp_date_subdir()
     ;maven_init   
     ;mvn_swe_kp

end