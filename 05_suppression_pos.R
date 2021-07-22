# read data from Genedata Expressionist ----------------------------------------
data_pos <- read_xlsx("data/05/05_SuppresPos.xlsx", sheet = "pos")

# read NAPS data and rename ----------------------------------------------------
rti_pos <- read_xlsx("data/05/05_RtiPos.xlsx") %>% 
  rename(rtime = RT,
         rindex = RTI)

# split data -------------------------------------------------------------------
meta_data_pos <- data_pos %>% 
  select(Name, `m/z`, RT)

feature_data_pos <- data_pos %>% 
  select(-`m/z`, -RT)

# calculate the mean of all injections and suppression/enhancing factor ---------
feature_data_sum <- feature_data_pos %>% 
  pivot_longer(cols = -Name) %>% 
  rename(sample = name) %>% 
  mutate(sample_type = str_extract(.$sample, "[^_]+"),
         dilution = str_extract(.$sample, "1dil\\d+|pure")) %>% 
  group_by(sample_type, dilution, Name) %>% 
  summarize(mean = mean(value))%>% 
  pivot_wider(names_from = Name, values_from = mean)%>% 
  unite(col = "sample", sample_type, dilution, sep = "_") %>% 
  column_to_rownames("sample") %>% 
  t() %>% 
  as_tibble() %>% 
  mutate(Name = meta_data_pos$Name)

feature_data_suppres <- feature_data_sum %>% 
  mutate(suppres_Cel_1dil20 = Cel_1dil20 / Cel_pure * 100,
         suppres_Cel_1dil40 = Cel_1dil40 / Cel_pure * 100,
         suppres_Cel_1dil80 = Cel_1dil80 / Cel_pure * 100,
         suppres_plasma_1dil20 = plasma_1dil20 / plasma_pure * 100,
         suppres_plasma_1dil40 = plasma_1dil40 / plasma_pure * 100,
         suppres_plasma_1dil80 = plasma_1dil80 / plasma_pure * 100) %>% 
  full_join(meta_data_pos, by = "Name") %>% 
  select(Name, `m/z`, RT,
         suppres_Cel_1dil20,
         suppres_Cel_1dil40,
         suppres_Cel_1dil80,
         suppres_plasma_1dil20,
         suppres_plasma_1dil40,
         suppres_plasma_1dil80)


# perform retention time indexing ----------------------------------------------
feature_data_suppres <- feature_data_suppres %>% 
  mutate(rti = indexRtime(RT, rti_pos))

# get data around NAPS ---------------------------------------------------------
suppres_data <- tibble()

for(i in 1:nrow(rti_pos)) {
  
  filter_rtime <- rti_pos$rtime[i]
  filter_rindex <- rti_pos$rindex[i]
  
  suppres_filter <- feature_data_suppres %>% 
    filter(abs(RT - filter_rtime) < 0.20) %>% 
    mutate(rt_diff = RT - filter_rtime,
           naps = paste0("C", filter_rindex / 100))
  
  suppres_data <- bind_rows(suppres_data, suppres_filter)
  
}

# plot difference to NAPS center RT vs suppresion effect -----------------------
suppres_data %>% 
  filter(naps == "C15") %>% 
  select(naps, rt_diff, starts_with("suppres")) %>% 
  pivot_longer(-c(naps, rt_diff)) %>% 
  ggplot(aes(x = rt_diff, y = value)) + 
  geom_point() +
  facet_grid(naps~name) +
  scale_x_continuous(limits = c(-0.20, 0.20)) +
  scale_y_continuous(limits = c(0,100)) +
  theme_bw()

