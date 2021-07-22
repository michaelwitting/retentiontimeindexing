# setup plates to read ---------------------------------------------------------
plates <- c("Plate 1",
            "Plate 2",
            "Plate 3",
            "Plate 4",
            "Plate 5",
            "Plate 6",
            "Plate 7")

# read RTI and  RT data from all files -----------------------------------------
rti_ref <- read_data("data/04/04_TotalData_InitialRIDatabase_ver2.xlsx", plates)
rti_hplc <- read_data("data/04/04_AgilentHPLC.xlsx", plates)

rt_ref <- read_rt_data("data/04/04_TotalData_InitialRIDatabase_ver2.xlsx", plates) %>% 
  select("plate", "No.", "Name", "rt")
rt_hplc <- read_rt_data("data/04/04_AgilentHPLC.xlsx", plates) %>% 
  select("plate", "No.", "Name", "rt")

rti_ref <- full_join(rti_ref, rt_ref, by = c("plate", "No.", "Name"))
rti_hplc <- full_join(rti_hplc, rt_hplc, by = c("plate", "No.", "Name"))

# read data from Genedata Expressionist ----------------------------------------
data <- read_xlsx("data/05/05_SuppresNeg.xlsx", sheet = "neg")

# read NAPS data and rename ----------------------------------------------------
rti <- read_xlsx("data/05/05_RtiNeg.xlsx") %>% 
  rename(rtime = RT,
         rindex = RTI)

# split data -------------------------------------------------------------------
meta_data <- data %>% 
  select(Name, `m/z`, RT)

feature_data <- data %>% 
  select(-`m/z`, -RT)

# filter features in C. elegans higher than 500 counts
filter <- feature_data %>% 
  select(Name, starts_with("Cel")) %>% 
  drop_na() %>% 
  filter_all(all_vars(. > 500)) %>% 
  select(Name) %>% 
  unlist() %>% 
  unname()

# calculate RTI for biological data --------------------------------------------
meta_data <- meta_data %>% 
  mutate(rindex = as.integer(indexRtime(RT, rti))) %>% 
  rename(mz = `m/z`,
         rtime = RT) %>% 
  filter(Name %in% filter,
         rindex > 300) %>% 
  as.data.frame()

# prepare database for matching ------------------------------------------------
rti_ref_db <- rti_ref %>% 
  select(Name, `exact mass`, formula, rti, rt) %>% 
  rename(exactmass = `exact mass`,
         rtime_db = rt,
         rindex = rti) %>% 
  filter(rindex > 300) %>% 
  as.data.frame()

rti_hplc_db <- rti_hplc %>% 
  select(Name, `exact mass`, formula, rti, rt) %>% 
  rename(exactmass = `exact mass`,
         rtime_db = rt,
         rindex = rti) %>% 
  filter(rindex > 300) %>% 
  as.data.frame() 

# perform matching only on m/z -------------------------------------------------
match_param <- Mass2MzParam(adducts = c("[M-H]-"))

matches <- matchMz(meta_data,
                   rti_ref_db,
                   match_param)

matches_mz_df <- matchedData(matches, column = c("Name",
                                                 "mz",
                                                 "rtime",
                                                 "rindex",
                                                 "target_Name",
                                                 "adduct",
                                                 "score")) %>% 
  as_tibble() %>% 
  filter(!is.na(score))

# perform matching using matchMz from MetaboAnnotation -------------------------
match_param <- Mass2MzRtParam(adducts = c("[M-H]-"),
                              tolerance = 0.005,
                              toleranceRt = 10)

# matching against UPLC DB
matches <- matchMz(meta_data,
                   rti_ref_db,
                   match_param,
                   massColumn = "exactmass",
                   mzColumn = "mz",
                   rtColumn = "rindex")

matches_uplc_df <- matchedData(matches, columns = c("Name",
                                                    "mz",
                                                    "rtime",
                                                    "rindex",
                                                    "target_Name",
                                                    "target_rindex",
                                                    "target_rtime_db",
                                                    "adduct",
                                                    "score","score_rt")) %>%
  as_tibble() %>% 
  filter(!is.na(score))

# matching against HPLC DB
matches <- matchMz(meta_data,
                   rti_hplc_db,
                   match_param,
                   massColumn = "exactmass",
                   mzColumn = "mz",
                   rtColumn = "rindex")

matches_hplc_df <- matchedData(matches, columns = c("Name",
                                                    "mz",
                                                    "rtime",
                                                    "rindex",
                                                    "target_Name",
                                                    "target_rindex",
                                                    "target_rtime_db",
                                                    "adduct",
                                                    "score","score_rt")) %>%
  as_tibble() %>% 
  filter(!is.na(score))

comparison <- full_join(matches_uplc_df, matches_hplc_df, by = c("Name", "mz", "rtime", "rindex"), suffix = c("_uplc", "_hplc"))
comparison <- full_join(comparison, matches_mz_df, by = c("Name", "mz", "rtime", "rindex"), suffix = c("", "_mz"))

# plot results -----------------------------------------------------------------
p1 <- ggplot(comparison, aes(x = rtime, y = target_rtime_db_uplc)) +
  geom_point() +
  geom_line(data = data.frame(x = c(0,40), y = c(0, 40)), aes(x = x, y = y), colour = "red") +
  theme_bw()

p2 <- ggplot(comparison, aes(x = rindex, y = target_rindex_uplc)) +
  geom_point() +
  geom_line(data = data.frame(x = c(0,2000), y = c(0, 2000)), aes(x = x, y = y), colour = "red") +
  theme_bw()

p3 <- ggplot(comparison, aes(x = rtime, y = target_rtime_db_hplc)) +
  geom_point() +
  geom_line(data = data.frame(x = c(0,40), y = c(0, 40)), aes(x = x, y = y), colour = "red") +
  theme_bw()

p4 <- ggplot(comparison, aes(x = rindex, y = target_rindex_hplc)) +
  geom_point() +
  geom_line(data = data.frame(x = c(0,2000), y = c(0, 2000)), aes(x = x, y = y), colour = "red") +
  theme_bw()

(p1 | p3) / (p2 | p4)
