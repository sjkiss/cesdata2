## This code assigns occupations in ces21 public sector or not
ces21$no
ces21 %>%
  mutate(sector=case_when(
    #Police officers, firefighters, CAF
    NOC21_4==4210~ 1,
    TRUE==0,
  ))
