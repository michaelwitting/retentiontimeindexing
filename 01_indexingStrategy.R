# setup plates to read ---------------------------------------------------------
plates <- c("Plate 1",
            "Plate 2",
            "Plate 3",
            "Plate 4",
            "Plate 5",
            "Plate 6",
            "Plate 7")

# create empty tibble ----------------------------------------------------------
full_data_pos <- tibble()
full_data_neg <- tibble()

# read reference data ----------------------------------------------------------
for(plate in plates) {
  
  # read positive mode data
  plate_df <- read_xlsx("data/01/01_TotalData_InitialRIDatabase_ver2.xlsx",
                        sheet = plate,
                        range = "A2:K138",
                        na = "n.d.") %>% 
    filter(!is.na(Name))
  
  clipboard <- tibble(plate = plate,
                      calc_rti(plate_df))
  
  
  full_data_pos <- bind_rows(full_data_pos, 
                             clipboard)
  
  # read negative mode data
  plate_df <- read_xlsx("data/01/01_TotalData_InitialRIDatabase_ver2.xlsx",
                        sheet = plate,
                        range = "P2:Z138",
                        na = "n.d.") %>% 
    filter(!is.na(Name))
  
  clipboard <- tibble(plate = plate,
                      calc_rti(plate_df))
  
  
  full_data_neg <- bind_rows(full_data_neg, 
                             clipboard)
  
}

# evaluation of different calculation ways (pre, post vs average) --------------
# pre vs post, positive
plot_data <- tibble(x = full_data_pos$rti_linear_pre_mean,
                    xsd = full_data_pos$rti_linear_pre_sd,
                    y = full_data_pos$rti_linear_post_mean,
                    ysd = full_data_pos$rti_linear_post_sd,
                    diff = full_data_pos$rti_linear_post_mean - full_data_pos$rti_linear_pre_mean)

plot_rti_comparison(plot_data,
                    xlab = "linear interpolation, pre",
                    ylab = "linear interpolation, post",
                    main = "Linear Interpolation, pre vs post, (+)-mode")

plot_data <- tibble(x = full_data_neg$rti_linear_pre_mean,
                    xsd = full_data_neg$rti_linear_pre_sd,
                    y = full_data_neg$rti_linear_post_mean,
                    ysd = full_data_neg$rti_linear_post_sd,
                    diff = full_data_neg$rti_linear_post_mean - full_data_neg$rti_linear_pre_mean)

plot_rti_comparison(plot_data,
                    xlab = "linear interpolation, pre",
                    ylab = "linear interpolation, post",
                    main = "Linear Interpolation, pre vs post, (-)-mode")

# pre vs average
plot_data <- tibble(x = full_data_pos$rti_linear_pre_mean,
                    xsd = full_data_pos$rti_linear_pre_sd,
                    y = full_data_pos$rti_linear_average_mean,
                    ysd = full_data_pos$rti_linear_average_sd,
                    diff = full_data_pos$rti_linear_average_mean - full_data_pos$rti_linear_pre_mean)

plot_rti_comparison(plot_data,
                    xlab = "linear interpolation, pre",
                    ylab = "linear interpolation, average",
                    main = "Linear Interpolation, pre vs average, (+)-mode")

plot_data <- tibble(x = full_data_neg$rti_linear_pre_mean,
                    xsd = full_data_neg$rti_linear_pre_sd,
                    y = full_data_neg$rti_linear_average_mean,
                    ysd = full_data_neg$rti_linear_average_sd,
                    diff = full_data_neg$rti_linear_average_mean - full_data_neg$rti_linear_pre_mean)

plot_rti_comparison(plot_data,
                    xlab = "linear interpolation, pre",
                    ylab = "linear interpolation, average",
                    main = "Linear Interpolation, pre vs average, (-)-mode")

# post vs average
plot_data <- tibble(x = full_data_pos$rti_linear_post_mean,
                    xsd = full_data_pos$rti_linear_post_sd,
                    y = full_data_pos$rti_linear_average_mean,
                    ysd = full_data_pos$rti_linear_average_sd,
                    diff = full_data_pos$rti_linear_average_mean - full_data_pos$rti_linear_post_mean)

plot_rti_comparison(plot_data,
                    xlab = "linear interpolation, post",
                    ylab = "linear interpolation, average",
                    main = "Linear Interpolation, post vs average, (+)-mode")

plot_data <- tibble(x = full_data_neg$rti_linear_post_mean,
                    xsd = full_data_neg$rti_linear_post_sd,
                    y = full_data_neg$rti_linear_average_mean,
                    ysd = full_data_neg$rti_linear_average_sd,
                    diff = full_data_neg$rti_linear_average_mean - full_data_neg$rti_linear_post_mean)

plot_rti_comparison(plot_data,
                    xlab = "linear interpolation, post",
                    ylab = "linear interpolation, average",
                    main = "Linear Interpolation, post vs average, (-)-mode")

# evaluation of different calculation ways (fitting) ---------------------------
# linear vs spline
plot_data <- tibble(x = full_data_pos$rti_linear_average_mean,
                    xsd = full_data_pos$rti_linear_average_sd,
                    y = full_data_pos$rti_spline_average_mean,
                    ysd = full_data_pos$rti_spline_average_sd,
                    diff = full_data_pos$rti_spline_average_mean - full_data_pos$rti_linear_average_mean)

plot_rti_comparison(plot_data,
                    xlab = "linear interpolation, average",
                    ylab = "spline, average",
                    main = "Linear Interpolation vs spline, (+)-mode")

plot_data <- tibble(x = full_data_neg$rti_linear_average_mean,
                    xsd = full_data_neg$rti_linear_average_sd,
                    y = full_data_neg$rti_spline_average_mean,
                    ysd = full_data_neg$rti_spline_average_sd,
                    diff = full_data_neg$rti_spline_average_mean - full_data_neg$rti_linear_average_mean)

plot_rti_comparison(plot_data,
                    xlab = "linear interpolation, average",
                    ylab = "spline, average",
                    main = "Linear Interpolation vs spline, (-)-mode")

# linear vs akima
plot_data <- tibble(x = full_data_pos$rti_linear_average_mean,
                    xsd = full_data_pos$rti_linear_average_sd,
                    y = full_data_pos$rti_akima_average_mean,
                    ysd = full_data_pos$rti_akima_average_sd,
                    diff = full_data_pos$rti_akima_average_mean - full_data_pos$rti_linear_average_mean)

plot_rti_comparison(plot_data,
                    xlab = "linear interpolation, average",
                    ylab = "Akima, average",
                    main = "Linear Interpolation vs Akima, (+)-mode")

plot_data <- tibble(x = full_data_neg$rti_linear_average_mean,
                    xsd = full_data_neg$rti_linear_average_sd,
                    y = full_data_neg$rti_akima_average_mean,
                    ysd = full_data_neg$rti_akima_average_sd,
                    diff = full_data_neg$rti_akima_average_mean - full_data_neg$rti_linear_average_mean)

plot_rti_comparison(plot_data,
                    xlab = "linear interpolation, average",
                    ylab = "Akima, average",
                    main = "Linear Interpolation vs Akima, (-)-mode")

# linear vs akima
plot_data <- tibble(x = full_data_pos$rti_spline_average_mean,
                    xsd = full_data_pos$rti_spline_average_sd,
                    y = full_data_pos$rti_akima_average_mean,
                    ysd = full_data_pos$rti_akima_average_sd,
                    diff = full_data_pos$rti_akima_average_mean - full_data_pos$rti_linear_average_mean)

plot_rti_comparison(plot_data,
                    xlab = "spline, average",
                    ylab = "Akima, average",
                    main = "spline vs Akima, (+)-mode")

plot_data <- tibble(x = full_data_neg$rti_spline_average_mean,
                    xsd = full_data_neg$rti_spline_average_sd,
                    y = full_data_neg$rti_akima_average_mean,
                    ysd = full_data_neg$rti_akima_average_sd,
                    diff = full_data_neg$rti_akima_average_mean - full_data_neg$rti_linear_average_mean)

plot_rti_comparison(plot_data,
                    xlab = "spline, average",
                    ylab = "Akima, average",
                    main = "spline vs Akima, (-)-mode")

# fitting of positive and negative mode data -----------------------------------
full_data_neg_linear <- full_data_neg %>% 
  select(c("plate", "No.", "Name", "SMILES", "RTI", "exact mass", "formula", "logP", "Mix",
           "rti_linear_average_mean", "rti_linear_average_sd", "rti_linear_average_rsd"))

full_data_pos_linear <- full_data_pos %>% 
  select(c("plate", "No.", "Name", "SMILES", "RTI", "exact mass", "formula", "logP", "Mix",
           "rti_linear_average_mean", "rti_linear_average_sd", "rti_linear_average_rsd"))

full_data <- full_join(full_data_neg_linear,
                       full_data_pos_linear,
                       by = c("plate", "No.", "Name", "SMILES", "RTI", 
                              "exact mass", "formula", "logP", "Mix"),
          suffix = c("_neg", "_pos"))


plot_data <- tibble(x = full_data$rti_linear_average_mean_pos,
                    xsd = full_data$rti_linear_average_rsd_pos,
                    y = full_data$rti_linear_average_mean_neg,
                    ysd = full_data$rti_linear_average_mean_neg,
                    diff = full_data$rti_linear_average_mean_neg - full_data$rti_linear_average_mean_pos)

plot_rti_comparison(plot_data,
                    xlab = "linear, average, (+)-mode",
                    ylab = "linear, average, (-)-mode",
                    main = "(+)- vs (-)-mode")

# testing if differences are systematic
full_data %>% mutate(diff = rti_linear_average_mean_neg - rti_linear_average_mean_pos,
                     rti = case_when(is.na(rti_linear_average_mean_neg) & !is.na(rti_linear_average_mean_pos) ~ rti_linear_average_mean_pos,
                                     !is.na(rti_linear_average_mean_neg) & is.na(rti_linear_average_mean_pos) ~ rti_linear_average_mean_neg,
                                     TRUE ~ as.integer((rti_linear_average_mean_neg + rti_linear_average_mean_pos) / 2))) %>% View()

full_data %>% filter(!is.na(rti_linear_average_mean_neg),
                                   !is.na(rti_linear_average_mean_pos)) %>% 
  mutate(diff = rti_linear_average_mean_neg - rti_linear_average_mean_pos) %>% 
  arrange(rti_linear_average_mean_pos) %>% 
  select(diff) %>% 
  unlist() %>% 
  as.numeric() %>% 
  bartels.test()

pearson.test(full_data$rti_linear_average_mean_neg - full_data$rti_linear_average_mean_pos)
shapiro.test(full_data$rti_linear_average_mean_neg - full_data$rti_linear_average_mean_pos)



                     