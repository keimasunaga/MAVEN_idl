pro load_mvn_mag_full

    st = time_double('2014-12-27')
    one_day = 3600d * 24d
    for i = 0d,365d do begin
      timespan,st + one_day * i
      mvn_spice_load
      mvn_mag_load,'L2_FULL',spice_frame='MAVEN_MSO'
    endfor
    
end