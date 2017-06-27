#C      Splitnose       model_One_Trawl
#_data_and_control_files:       splitnose_simple.dat    //      splitnose_simple.ctl
#_SS-V3.01-O-opt;_12/16/08;_Stock_Synthesis_by_Richard_Methot_(NOAA);_using_Otter_Research_ADMB_7.0.1
1       #_N_Growth_Patterns
1       #_N_Morphs_Within_GrowthPattern
#_Cond  1       #_Morph_between/within_stdev_ratio      (no     read    if      N_morphs=1)
#_Cond  1       #vector_Morphdist_(-1_in_first_val_gives_normal_approx)

#_Cond  0       #       N       recruitment     designs goes    here    if      N_GP*nseas*area>1
#_Cond  0       #       placeholder     for     recruitment     interaction     request
#_Cond  1       1       1       #       example recruitment     design  element for     GP=1,   seas=1, area=1

#_Cond  0       #       N_movement_definitions  goes    here    if      N_areas >       1
#_Cond  1       #       first   age     that    moves   (real   age     at      begin   of      season, not     integer)        also    cond    on      do_migration>0
#_Cond  1       1       1       2       4       10      #       example move    definition      for     seas=1, morph=1,        source=1        dest=2, age1=4, age2=10

1       #_Nblock_Patterns
1       #_Blocks_per_pattern
1992    2008
0.5     #_fracfemale
0       #_natM_type:_0=1Parm;   1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate
1       #       GrowthModel:    1=vonBert       with    L1&L2;  2=Richards      with    L1&L2;  3=not   implemented;    4=not   implemented
1.66    #_Growth_Amin_Age_for_L1
999     #_Growth_Amax_Age_for_L2        (999    to      use     as      Linf)
0       #_SD_add_to_LAA (set    to      0.1     for     SS2     V1.x    compatibility)
0       #_CV_Growth_Pattern:    0       CV=f(LAA);      1       CV=F(A);        2       SD=F(LAA);      3       SD=F(A)
1       #_maturity_option:      1=length        logistic;       2=age   logistic;       3=read  age-maturity    matrix  by      growth_pattern; 4=read  age-fecundity
#_placeholder   for     empirical       age-maturity    by      growth  pattern
4       #_First_Mature_Age
1       #_fecundity     option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b
0       #_hermaphroditism option:  0=none; 1=age-specific fxn
1       #_parameter_offset_approach     (1=none,        2=      M,      G,      CV_G    as      offset  from    female-GP1,     3=like  SS2     V1.x)
1       #_env/block/dev_adjust_method   (1=standard;    2=with  logistic        trans   to      keep    within  base    parm    bounds)

#_growth_parms
#_LO    HI      INIT    PRIOR   PR_type SD      PHASE   env-var use_dev dev_minyr       dev_maxyr       dev_stddev      Block   Block_Fxn
0.01    0.1     0.048   0.048   -1      0.02    -5      0       0       0       0       0.5     0       0       #       NatM_p_1_Fem_GP:1
2       40      14      14      -1      0.02    2       0       0       0       0       0.5     0       0       #       L_at_Amin_Fem_GP_1
2       60      30      30      -1      0.02    2       1       0       0       0       0.5     0       0       #       L_at_Amax_Fem_GP_1
0.05    0.5     0.14    0.14    -1      0.02    2       1       0       0       0       0.5     0       0       #       VonBert_K_Fem_GP_1
0.05    0.3     0.13    0.13    -1      0.02    3       0       0       0       0       0.5     0       0       #       CV_young_Fem_GP_1
0.05    0.3     0.2     0       -1      0.02    3       0       0       0       0       0.5     0       0       #       CV_old_Fem_GP_1

0.01    0.1     0.048   0.048   -1      0.02    -5      0       0       0       0       0.5     0       0       #       NatM_p_1_Mal_GP:1
-3      3       0       0       -1      0.02    -3      0       0       0       0       0.5     0       0       #       L_at_Amin_Mal_GP_1
2       60      27      27      -1      0.02    2       1       0       0       0       0.5     0       0       #       L_at_Amax_Mal_GP_1
0.05    0.5     0.16    0.16    -1      0.02    2       1       0       0       0       0.5     0       0       #       VonBert_K_Mal_GP_1
0.05    0.3     0.13    0.13    -1      0.02    3       0       0       0       0       0.5     0       0       #       CV_young_Mal_GP_1
0.05    0.3     0.2     0.2     -1      0.02    3       0       0       0       0       0.5     0       0       #       CV_old_Mal_GP_1

-3      3       0.00002 0.00002 -1      0.02    -3      0       0       0       0       0.5     0       0       #       Wtlen_1_Fem
2       4       3.0139  3.0139  -1      0.02    -3      0       0       0       0       0.5     0       0       #       Wtlen_2_Fem

15      30      21.84   21.84   -1      0.02    -3      0       0       0       0       0.5     0       0       #       Mat50%_Fem
-5      3       -0.5683 -0.5683 -1      0.02    -3      0       0       0       0       0.5     0       0       #       Mat_slope_Fem
-3      300000  237500  237500  -1      0.02    -3      0       0       0       0       0.5     0       0       #       Eg/gm_inter_Fem
-3      300000  74300   74300   -1      0.02    -3      0       0       0       0       0.5     0       0       #       Eg/gm_slope_wt_Fem

-3      3       0.00002 0.00002 -1      0.02    -3      0       0       0       0       0.5     0       0       #       Wtlen_1_Mal
-3      3       2.9684  2.9684  -1      0.02    -3      0       0       0       0       0.5     0       0       #       Wtlen_2_Mal

-4      4       0       0       -1      0.02    -4      0       0       0       0       0.5     0       0       #       RecrDist_GP_1
-4      4       0       0       -1      0.02    -4      0       0       0       0       0.5     0       0       #       RecrDist_Area_1
-4      4       0       0       -1      0.02    -4      0       0       0       0       0.5     0       0       #       RecrDist_Seas_1
1       1       1       1       -1      0.02    -4      0       0       0       0       0.5     0       0       #       CohortGrowDev

0       #custom_MG-env_setup    (0/1)
-2      2       0       0       -1      0.2     2      #_placeholder   when    no      MG-environ      parameters
#_Cond  0       #custom_MG-block_setup  (0/1)
#_Cond  -2      2       0       0       -1      0.2     -2      #_placeholder   when    no      MG-block        parameters
#_seasonal_effects_on_biology_parms
0       0       0       0       0       0       0       0       0       0       #_femwtlen1,femwtlen2,mat1,mat2,fec1,fec2,Malewtlen1,malewtlen2,L1,K
#_Cond  -2      2       0       0       -1      0.2     -2      #_placeholder   when    no      seasonal        MG      parameters
#_Cond  -4      #_MGparm_Dev_Phase

#_Spawner-Recruitment
3       #_SR_function
#_LO    HI      INIT    PRIOR   PR_type SD      PHASE
3       15      8       8       -1      0.02    1       #       SR_R0
0.2     1       0.58    0.58    0       0.181   -2      #       SR_steep
0.1     1.5     1       1       -1      0.02    -3      #       SR_sigmaR
0       0       0       0       -1      0.02    -3      #       SR_envlink
-2      2       0       0       -1      0.02    -1      #       SR_R1_offset
0       0       0       0       -1      0.02    -99     #       SR_autocorr

0       #_SR_env_link
0       #_SR_env_target_0=none;1=devs;_2=R0;_3=steepness
1       #do_recdev:     0=none; 1=devvector;    2=simple        deviations
1960    #       first   year    of      main    recr_devs;      early   devs    can     preceed this    era
2006    #       last    year    of      main    recr_devs;      forecast        devs    start   in      following       year
5       #_recdev        phase

1       #       (0/1)   to      read    11      advanced        options
0       #_recdev_early_start    (0=none;        neg     value   makes   relative        to      recdev_start)
-4      #_recdev_early_phase
-1       #_forecast_recruitment  phase   (incl.  late    recr)   (0      value   resets  to      maxphase+1)
10000    #_lambda        for     prior_fore_recr occurring       before  endyr+1
1979    #_last_early_yr_nobias_adj_in_MPD
1980    #_first_yr_fullbias_adj_in_MPD
2002    #_last_yr_fullbias_adj_in_MPD
2003    #_first_recent_yr_nobias_adj_in_MPD
 1 #_max_bias_adj_in_MPD (-1 to override ramp and set biasadj=1.0 for all estimated recdevs)
 0 #_period of cycles in recruitment (N parms read below)

-15     #min    rec_dev
15      #max    rec_dev
0       #_read_recdevs
#_end   of      advanced        SR      options

#       read    specified       recr    devs
#_Yr    Input_value

#Fishing        Mortality       info
0.2     #       F       ballpark        for     tuning  early   phases
2001    #       F       ballpark        year    (neg    value   to      disable)
3       #       F_Method:       1=Pope; 2=instan.       F;      3=hybrid        (hybrid is      recommended)
4     #       max     F       or      harvest rate,   depends on      F_Method
#       no      additional      F       input   needed  for     Fmethod 1
#       read    overall start   F       value;  overall phase;  N       detailed        inputs  to      read    for     Fmethod 2
4	#       read    N       iterations      for     tuning  for     Fmethod 3       (recommend      3       to      7)
#Fleet  Year    Seas    F_value se      phase   (for    detailed        setup   of      F_Method=2)

#_initial_F_parms
#_LO    HI      INIT    PRIOR   PR_type SD      PHASE
0       1       0       0.01    0       0.2     -2      #       InitF_FISHERY1_Domestic_Trawl
#0       1       0       0.01    0       0.2     -2      #       InitF_FISHERY2_Foreign_Trawl
#0       1       0       0.01    0       0.2     -2      #       InitF_FISHERY3_Non_Trawl

#_Q_setup
#       A=do    power,  B=env-var,      C=extra SD,     D=devtype(<0=mirror,    0/1=none,       2=cons, 3=rand, 4=randwalk);    E=0=num/1=bio,  F=err_type
#_A     B       C       D        
0       0       0       0               #       1       Fishery_Domestic_Trawl
#0       0       0       0               #       2       Fishery_Foreign_Trawl
#0       0       0       0               #       3       Fishery_Non_Trawl
#0       0       0       1               #       4       Survey_AFSC_triennial
#0       0       0       1               #       5       Survey_AFSC_slope
0       0       0       1               #       6       Survey_NWFSC_shelf_slope
#0       0       0       1               #       7       Survey_NWFSC_slope
#_Cond  0       #_If    q       has     random  component,      then    0=read  one     parm    for     each    fleet   with    random  q;      1=read  a       parm    for     each    year    of      index
#_size_selex_types
#_Pattern       Discard Male    Special
24      1       0       0       #       1       FISHERY_Domestic_Trawl
#5       0       0       1       #       2       FISHERY_Foreign_Trawl
#24      0       0       0       #       3       FISHERY_Non_Trawl
#24      0       0       0       #       4       SURVEY_AFSC_triennial
#24      0       0       0       #       5       SURVEY_AFSC_slope
24      0       0       0       #       6       SURVEY_NWFSC_shelf_slope
#5       0       0       5       #       7       SURVEY_NWFSC_slope
#_age_selex_types
#_Pattern       Retention       Male    Special
10      0       0       0       #       1       FISHERY_Domestic_Trawl
#10      0       0       0       #       2       FISHERY_Foreign_Trawl
#10      0       0       0       #       3       FISHERY_Non_Trawl
#10      0       0       0       #       4       SURVEY_AFSC_triennial
#10      0       0       0       #       5       SURVEY_AFSC_slope
10      0       0       0       #       6       SURVEY_NWFSC_shelf_slope
#10      0       0       0       #       7       SURVEY_NWFSC_slope
#_selex_parms
#_LO    HI      INIT    PRIOR   PR_type SD      PHASE   env-var use_dev dev_minyr       dev_maxyr       dev_stddev      Block   Block_Fxn
#_size_sel:     1_Fishery_Domestic_Trawl
12      35      30      30      1       0.2     1       0       0       0       0       0.5     0       0       #       PEAK
-6      4       -2      -4      1       0.2     -5      0       0       0       0       0.5     0       0       #       TOP:_width      of      plateau
-1      9       3.2     3.2     1       0.2     2       0       0       0       0       0.5     0       0       #       Asc_width
-1      9       4       4       1       0.2     -4      0       0       0       0       0.5     0       0       #       Desc_width
-5      9       -5      -5      -1      0.2     -4      0       0       0       0       0.5     0       0       #       INIT:_selectivity_at_fist_bin
-5      9       8       8       1       0.2     -2      0       0       0       0       0.5     0       0       #       FINAL:_selectivity_at_last_bin
#_Retention_Fishery_1_Domestic_Trawl
12      35      25      25      1       0.2     3       0       0       0       0       0.5     0       0       #_Inflection_for_retention
0.5     7       1       1       1       0.2     3       0       0       0       0       0.5     0       0       #_Slope_for_retention
0       1       1       1       -1      0.2     3       0       0       0       0       0.5     0       0       #_Asymptotic_retention
0       1       0       0       -1      0.2     -4      0       0       0       0       0.5     0       0       #_Male_offset_to_inflection
##_size_sel:     2_Fishery_Foreign_Trawl
#-2      60      0       0       -1      0.2     -4      0       0       0       0       0.5     0       0       #_Min_Bin_Number_in_Fishery_1
#-2      60      0       0       -1      0.2     -4      0       0       0       0       0.5     0       0       #_Max_Bin_Number_in_Fishery_1
##_size_sel:     3_Fishery_Non_Trawl
#12      55      30      30      1       0.2     2       0       0       0       0       0.5     1       2       #       PEAK
#-6      4       -2      -4      1       0.2     -5      0       0       0       0       0.5     1       2       #       TOP:_width      of      plateau
#-1      9       3.2     3.2     1       0.2     3       0       0       0       0       0.5     1       2       #       Asc_width
#-1      9       4       4       1       0.2     -4      0       0       0       0       0.5     1       2       #       Desc_width
#-5      9       -5      -5      -1      0.2     -4      0       0       0       0       0.5     0       0       #       INIT:_selectivity_at_fist_bin
#-5      9       8       1       1       0.2     -3      0       0       0       0       0.5     1       2       #       FINAL:_selectivity_at_last_bin
##_size_sel:     4_AFSC_triennial
#12      35      14      14      1       0.2     2       0       0       0       0       0.5     0       0       #       PEAK
#-6      4       -1.5    -1.5    1       0.2     5       0       0       0       0       0.5     0       0       #       TOP:_width      of      plateau
#-1      9       2.6     2.6     1       0.2     3       0       0       0       0       0.5     0       0       #       Asc_width
#-1      9       4       4       1       0.2     4       0       0       0       0       0.5     0       0       #       Desc_width
#-5      9       -5      -5      -1      0.2     -4      0       0       0       0       0.5     0       0       #       INIT:_selectivity_at_fist_bin
#-5      9       1       1       1       0.2     2       0       0       0       0       0.5     0       0       #       FINAL:_selectivity_at_last_bin
##_size_sel:     5_AFSC_slope
#12      35      14      14      1       0.2     2       0       0       0       0       0.5     0       0       #       PEAK
#-6      4       -1.5    -1.5    1       0.2     -5      0       0       0       0       0.5     0       0       #       TOP:_width      of      plateau
#-1      9       3.5     3.5     1       0.2     3       0       0       0       0       0.5     0       0       #       Asc_width
#-1      9       4       4       1       0.2     -4      0       0       0       0       0.5     0       0       #       Desc_width
#-5      9       -5      -5      -1      0.2     -4      0       0       0       0       0.5     0       0       #       INIT:_selectivity_at_fist_bin
#-5      9       8       8       1       0.2     -2      0       0       0       0       0.5     0       0       #       FINAL:_selectivity_at_last_bin
##_size_sel:     6_NWFSC_shelf_slope
12      35      18      18      1       0.2     1       0       0       0       0       0.5     0       0       #       PEAK
-6      4       -2      -5      1       0.2     -5      0       0       0       0       0.5     0       0       #       TOP:_width      of      plateau
-1      9       4.5     4.5     1       0.2     2       0       0       0       0       0.5     0       0       #       Asc_width
-1      9       4       4       1       0.2     -4      0       0       0       0       0.5     0       0       #       Desc_width
-5      9       -5      -5      -1      0.2     -4      0       0       0       0       0.5     0       0       #       INIT:_selectivity_at_fist_bin
-5      9       8       8       1       0.2     -2      0       0       0       0       0.5     0       0       #       FINAL:_selectivity_at_last_bin
##_size_sel:     7_NWFSC_slope
#-2      60      0       0       -1      0.2     -4      0       0       0       0       0.5     0       0       #_Min_Bin_Number_in_Survey_5
#-2      60      0       0       -1      0.2     -4      0       0       0       0       0.5     0       0       #_Max_Bin_Number_in_Survey_5
#_custom_sel-env_setup  (0/1)
#_placeholder   when    no      enviro  fxns
#0       #_Custom_block_setup
#12      55      33      33      1       0.2     4       #       PEAK
#-6      4       -4      -4      1       0.2     -4      #       TOP:_width      of      plateau
#-1      9       3.2     3.2     1       0.2     4       #       Asc_width
#-1      9       2.6     2.6     1       0.2     -4      #       Desc_width
#-5      9       8       1       1       0.2     -4      #       FINAL:_selectivity_at_last_bin
#2       #separt_adj_method
#       Tag     loss    and     Tag     reporting       parameters      go      next
0       #       TG_custom:      0=no    read;   1=read  if      tags    exist
#_Cond  -6      6       1       1       2       0.01    -4      0       0       0       0       0       0       0       #_placeholder   if      no      parameters
1       #_Variance_adjustments_to_input_values
#1      Domestic_TRAWL
#2      Foreign_TRAWL
#3      Non_TRAWL
#4      AFSC_triennial
#5      AFSC_slope
#6      NWFSC_shelf_slope
#7      NWFSC_slope
#       1       2       3       4       5       6       7
0       0       #_add_to_survey_CV
0       0       #_add_to_discard_stddev
0       0       #_add_to_bodywt_CV
0.23448992     0.566707707       #_mult_by_lencomp_N
1       1.03678358       #_mult_by_agecomp_N
1       1      #_mult_by_size-at-age_N

1       #_maxlambdaphase
1       #_sd_offset

0       #       number  of      changes to      make    to      default Lambdas (default        value   is      1.0)
#       Like_comp       codes:  1=surv; 2=disc; 3=mnwt; 4=length;       5=age;  6=SizeFreq;     7=sizeage;      8=catch;
#       9=init_equ_catch;       10=recrdev;     11=parm_prior;  12=parm_dev;    13=CrashPen;    14=Morphcomp;   15=Tag-comp;    16=Tag-negbin
#like_comp      fleet/survey    phase   value   sizefreq_method
#       lambdas (for    info    only;   columns are     phases)
#       0       0       0       0       #_CPUE/survey:_1
#       1       1       1       1       #_CPUE/survey:_2
#       1       1       1       1       #_CPUE/survey:_3
#       1       1       1       1       #_lencomp:_1
#       1       1       1       1       #_lencomp:_2
#       0       0       0       0       #_lencomp:_3
#       1       1       1       1       #_agecomp:_1
#       1       1       1       1       #_agecomp:_2
#       0       0       0       0       #_agecomp:_3
#       1       1       1       1       #_size-age:_1
#       1       1       1       1       #_size-age:_2
#       0       0       0       0       #_size-age:_3
#       1       1       1       1       #_init_equ_catch
#       1       1       1       1       #_recruitments
#       1       1       1       1       #_parameter-priors
#       1       1       1       1       #_parameter-dev-vectors
#       1       1       1       1       #_crashPenLambda
0       #       (0/1)   read    specs   for     more    stddev  reporting
#       1       1       -1      5       1       5       #       selex   type,   len/age,        year,   N       selex   bins,   Growth  pattern,        N       growth  ages
#       -5      16      27      38      46      #       vector  with    selex   std     bin     picks   (-1     in      first   bin     to      self-generate)
#       1       2       14      26      40      #       vector  with    growth  std     bin     picks   (-1     in      first   bin     to      self-generate)
999

