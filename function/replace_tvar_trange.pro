pro replace_tvar_trange,tvar,tspan

  idx = nn(tvar,tspan)
  get_data,tvar,data=data

  if tag_exist(data, 'v') then begin
   
    if (size(data.v))[0] eq 1 then store_data,tvar,data={x:data.x[idx[0]:idx[1]], y:data.y[idx[0]:idx[1],*], v:data.v}
    if (size(data.v))[0] eq 2 then store_data,tvar,data={x:data.x[idx[0]:idx[1]], y:data.y[idx[0]:idx[1],*], v:data.v[idx[0]:idx[1],*]}

  endif else begin
 
    if (size(data.y))[0] eq 1 then store_data,tvar,data={x:data.x[idx[0]:idx[1]], y:data.y[idx[0]:idx[1]]}
    if (size(data.y))[0] eq 2 then store_data,tvar,data={x:data.x[idx[0]:idx[1],*], y:data.y[idx[0]:idx[1],*]}  
 
  endelse


end