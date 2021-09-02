all_lytes_pre_post_repletions_MV_new %>% #Amount of DISTINCT ICU visits 
  ungroup()%>%
  select(icustay_id) %>% 
  distinct() %>%
  count()

all_lytes_pre_post_repletions_MV_new %>% #Amount of DISTINCT Repletion Events 
  ungroup()%>%
  select(linkorderid, Electrolyte) %>%
  distinct() %>%
  group_by(Electrolyte) %>%
  summarize(n = n()) %>% 
  arrange(desc(n)) %>%
  kable()

