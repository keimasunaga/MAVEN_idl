pro intp_sem_disk

   get_data,'merged_SEM_Flux_1st',data=sem
   idx = where(finite(sem.y) eq 1)
   store_data,'merged_SEM_Flux_1st',data={x:sem.x[idx], y:sem.y[idx]}
   tinterpol_mxn,'merged_SEM_Flux_1st','disk_OI'


end