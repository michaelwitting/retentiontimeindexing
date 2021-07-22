# setup plates to read ---------------------------------------------------------
plates <- c("Plate 1",
            "Plate 2",
            "Plate 3",
            "Plate 4",
            "Plate 5",
            "Plate 6",
            "Plate 7")

# read RT data from all files --------------------------------------------------
rt_ref_data <- read_rt_data("data/04/04_TotalData_InitialRIDatabase_ver2.xlsx", plates)
rt_hplc <- read_rt_data("data/04/04_AgilentHPLC.xlsx", plates)

# combine data from all setups -------------------------------------------------
# combine data from all setups -------------------------------------------------
join_variables <- c("plate", "No.", "Name", "SMILES", "RTI", "exact mass",
                    "formula", "logP", "Mix", "rt")

rt_full_data <- full_join(rt_ref_data %>% select(join_variables),
                           rt_hplc %>% select(join_variables),
                           by = c("plate", "No.", "Name", "SMILES", "RTI", "exact mass",
                                  "formula", "logP", "Mix"),
                           suffix = c("_ref", "_hplc"))

# plot data --------------------------------------------------------------------
p1_1 <- rt_full_data %>% 
  filter(rt_ref > 0.9) %>%
  ggplot(aes(x = rt_ref, y = rt_hplc)) +
  geom_point(colour = viridis(4)[1]) +
  geom_line(data = data.frame(x = c(0,20), y = c(0,20)), aes(x = x, y = y), colour = "red", linetype = "dashed") +
  xlab("Reference RT (0.30 mL/min)") +
  ylab("RT (0.20 mL/min)") +
  theme_bw() +
  scale_fill_viridis()

# read RT data from all files --------------------------------------------------
rti_ref_data <- read_data("data/04/04_TotalData_InitialRIDatabase_ver2.xlsx", plates)
rti_hplc <- read_data("data/04/04_AgilentHPLC.xlsx", plates)

# combine data from all setups -------------------------------------------------
join_variables <- c("plate", "No.", "Name", "SMILES", "RTI", "exact mass",
                    "formula", "logP", "Mix", "rti")

rti_full_data <- full_join(rti_ref_data %>% select(join_variables),
                           rti_hplc %>% select(join_variables),
                           by = c("plate", "No.", "Name", "SMILES", "RTI", "exact mass",
                                  "formula", "logP", "Mix"),
                           suffix = c("_ref", "_hplc"))

# identify outliers ------------------------------------------------------------
outliers <- rti_full_data %>% 
  filter(rti_ref > 300,
         rti_hplc > 300) %>% 
  filter(abs(rti_ref - rti_hplc) / rti_ref * 100 > 5)


# safe outliers to file for further investigations
outliers %>% write_tsv("results/04/04_outliers.tsv")

# plot data --------------------------------------------------------------------
p1_2 <- rti_full_data %>% 
  filter(rti_hplc > 300) %>%
  ggplot(aes(x = rti_ref, y = rti_hplc)) +
  geom_point(colour = viridis(4)[1]) +
  geom_line(data = data.frame(x = c(0,2000), y = c(0,2000)), aes(x = x, y = y), colour = "red", linetype = "dashed") +
  theme_bw()

p1_3 <- rti_full_data %>% 
  filter(rti_hplc > 300) %>% 
  ggplot(aes(x = (rti_hplc - rti_ref) / rti_ref * 100)) +
  geom_histogram(binwidth = 5, fill = viridis(4)[1], colour = "black") +
  geom_vline(xintercept = 0, colour = "red", linetype = "dashed") +
  scale_x_continuous(limits = c(-50,50)) +
  scale_y_continuous(limits = c(0,100)) +
  theme_bw()

p1_1 | p1_2 | p1_3
