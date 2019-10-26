function get_mvn_local_pos,tspan


  get_data,'Xmso',data=Xmso
  get_data,'Ymso',data=Ymso
  get_data,'Zmso',data=Zmso

  if size(Xmso,/type) ne 8 or size(d_v,/type) ne 8 then pos = [0,0,0]

  idx_pos = nn('Xmso',tspan)
  time_idx_pos = Xmso.x[idx_pos]
  if time_idx_pos[1] - time_idx_pos[0] lt 100d then pos = [0,0,0]

  pos = [total(Xmso.y[idx_pos[0]:idx_pos[1],*],1)/(idx_pos[1]-idx_pos[0]+1.),$
         total(Ymso.y[idx_pos[0]:idx_pos[1],*],1)/(idx_pos[1]-idx_pos[0]+1.),$
         total(Zmso.y[idx_pos[0]:idx_pos[1],*],1)/(idx_pos[1]-idx_pos[0]+1.)]

  return,pos

end