## This code assigns occupations in ces21 public sector or not
ces21 <- ces21 %>%
  mutate(sector=case_when(
    # 3 codes: are nurses
    # 4 codes: are government jobs: including police officers, fire, CAF etc.
    # 5110: is librarians
    # 9999: is a custom code for government works that do not fit in other 4 digit codes
    NOC21_4 %in% c(3110, 3130, 3210,
                   3211, 4000, 4001,
                   4002, 4003, 4004,
                   4110, 4120, 4121,
                   4122, 4130, 4131,
                   4132, 4140, 4210,
                   4220, 4310, 4320,
                   4410, 4420, 4510,
                   5110,
                   9999) ~ 1,
    TRUE ~ 0,
  ))

# 1 - represents public sector jobs, 0 represents private sector jobs
table(ces21$NOC21_4, ces21$sector)


