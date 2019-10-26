PRO tplot_vec_tot,tvar,tplot=tplot,index=index

    get_data,tvar,data=d,index=index
    if index eq 0 then begin
      print,tvar,' is not defined'
      goto, nodata
    endif
    time = d.x
    vec = d.y
    vec_tot = sqrt(vec[*,0]^2 + vec[*,1]^2 + vec[*,2]^2)
    store_data,tvar+'_tot',data={x:time, y:vec_tot}
    if keyword_set(tplot) then tplot,tvar+'_tot'
    nodata:
END