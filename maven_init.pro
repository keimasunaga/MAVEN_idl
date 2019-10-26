PRO MAVEN_INIT
    thm_init

    setenv,'USER=masu-kei'
    setenv,'USERNAME=masu-kei'
    if !version.os eq 'darwin' then setenv,'ROOT_DATA_DIR=/Users/masunaga/work/data/' else setenv,'ROOT_DATA_DIR=/Users/masunaga/work/data/'

    !prompt='MAVEN>'
    dt = - (time_double('2013-11-18/06:28') - systime(1)) / 3600/24
    days = floor(dt)
    dt = (dt - days) * 24
    hours = floor(dt)
    dt = (dt - hours) * 60
    mins = floor(dt)
    dt = (dt - mins)  * 60
    secs = floor(dt)
    ; print,ptrace()
    print,days,hours,mins,secs,format= '(" MAVEN countdown:",i5," Days, ",i02," Hours, ",i02," Minutes, ",i02," Seconds since launch")'

    

    cdf_lib_info, version=v, subincrement=si, release=r, increment=i, copyright=c
    cdf_version = string(format="(i0,'.',i0,'.',i0,a)", v, r, i, si)
    printdat, cdf_version

    cdf_version_readmin = '3.1.0'
    cdf_version_writemin = '3.1.1'

    if cdf_version lt cdf_version_readmin then begin
      print, 'Your version of the CDF library ('+cdf_version+') is unable to read THEMIS data files.'
      print, 'Please go to the following url to learn how to patch your system:'
      print, 'http://cdf.gsfc.nasa.gov/html/idl62_or_earlier_and_cdf3_problems.html'
      message, "You can have your data. You just can't read it! Sorry!"
    endif

    if cdf_version lt cdf_version_writemin then begin
     ; print, ptrace()
      print, 'Your version of the CDF library ('+cdf_version+') is unable to correctly write THEMIS CDF data files.'
      print, 'If you ever need to create CDF files then go to the following URL to learn how to patch your system:'
      print, 'http://cdf.gsfc.nasa.gov/html/idl62_or_earlier_and_cdf3_problems.html'
    endif

;    ; ============ install custom color tables.
;    ; Check for color table with additional tables (download if necessary)
;    ; and set it as default for loadct2
;    ; Defines 3 new tables:
;    ; 41 wind3dp
;    ; 42 B-W reversed
;    ; 43 FAST-Special
;    ;ctable_relpath = 'idl_ctables/colors1.tbl'
;
;    ; ============ color setup
;
;    ; Do not do color setup if taken care for already
;    IF N_ELEMENTS(colartable) EQ 0 THEN colortable = 43 ; default color table
;    ;                        Define POSTSCRIPT color table
;    old_dev = !d.name              ;  save current device name
;    set_plot, 'PS'                 ;  change to PS so we can edit the font mapping
;    loadct2, colortable
;    device, /symbol, font_index=19 ;set font !19 to Symbol
;    set_plot, old_dev              ;  revert to old device
;
;    ;                        Color table for ordinary windows
;    loadct2, colortable
;
;    ;              Make black on white background
;    !p.background = !d.table_size - 1 ; White background   (color table 34)
;    !p.color = 0                      ; Black Pen
;    !p.font = -1                      ; Use default fonts
;
;    IF !d.name EQ 'WIN' THEN device, decompose=0
;    IF !d.name EQ 'X' THEN BEGIN
;      ; device,pseudo_color=8  ;fixes color
;      
;      
;      ; table problem for machines with
;      ; 24-bit color
;      device, decompose=0
;      IF !version.os_name EQ 'linux' THEN device, retain=2
;      ;IF !version.os_name EQ 'darwin' THEN device, retain=2
;      ; Linux does not provide backing store by default
;    ENDIF
;
;    ;===========  debugging options
;   ; IF !prompt EQ 'IDL> ' AND GETENV('USER') NE '' THEN !prompt = GETENV('USER') + '> ' $
;   ; ELSE !prompt = GETENV('USERNAME') + '> '
;
;    ; The following calls set persistent flags in dprint that change
;  ;  dprint, print_trace=1           ; uncomment to display current procedure and line number on each line. (recommended)
;
;    ;============= Useful TPLOT options
;    ; Most standard plotting keywords can be included in the global
;    ; tplot_options routine or individually in each tplot variable using
;    ; the procedure: "options"
;
    IF NOT keyword_set(org) THEN BEGIN
      IF !d.name NE 'WIN' THEN tplot_options, 'charsize', 1.
      time_stamp, /off
    ENDIF
;    ; Some other useful options:
;    tplot_options, window=0         ; Forces tplot to use only window 0 for all time plots
;    tplot_options, 'wshow', 1       ; Raises tplot window when tplot is called
;    tplot_options, 'verbose', 1     ; Displays some extra messages in tplot (e.g. When variables get created/altered)
;    tplot_options, 'ygap', .5       ; Set gap distance between tplot panels.
;    tplot_options, 'lazy_ytitle', 1 ; breaks "_" into carriage returns on ytitles
;    tplot_options, 'no_interp', 1   ; prevents interpolation in spectrograms (recommended)  
;      
;      
;      
      

END