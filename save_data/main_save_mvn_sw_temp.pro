pro main_save_mvn_sw_temp


   sd = time_double('2015-09-14')
   ndays = 365
   
   for i=0,ndays-1 do begin
     if i gt 0 then sd = sd + 24d*3600d
     tst = time_struct(sd)
     year = strcompress(string(tst.year),/remove)
     month = string(tst.month,format='(i02)')
     day = string(tst.date,format='(i02)')
     date = year+month+day

     save_mvn_sw_temp,date=date
    
    
   endfor




end