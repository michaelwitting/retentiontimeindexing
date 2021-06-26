# custom RTI functions for SPLINE and AKIMA ------------------------------------
rtiAkima <- function(x, rti) {
  
  x_na <- x
  
  x[is.na(x)] <- 0
  
  y <- aspline(rti[,1],
               rti[,2],
               x)$y
  
  y[is.na(x_na)] <- NA
  
  y
  
}

rtiSpline <- function(x, rti) {
  
  spline(rti[,1],
         rti[,2],
         xout = x)$y
  
}

# perform calculations, tidyverse style ----------------------------------------
calc_rti <- function(x) {
  
  # get the NAPS data
  x_naps_pre <- x %>% filter(No. == "pre")
  x_naps_post <- x %>% filter(No. == "post")
  x_naps_average <- bind_cols(x_naps_pre,
                                    `Rep 1_post` = x_naps_post$`Rep 1`,
                                    `Rep 2_post` = x_naps_post$`Rep 2`,
                                    `Rep 3_post` = x_naps_post$`Rep 3`) %>% 
    rowwise() %>% 
    mutate(`Rep 1` = mean(c(`Rep 1`, `Rep 1_post`)),
           `Rep 2` = mean(c(`Rep 2`, `Rep 2_post`)),
           `Rep 3` = mean(c(`Rep 3`, `Rep 3_post`))) %>% 
    select(-c(`Rep 1_post`, `Rep 2_post`, `Rep 3_post`))
  
  x <- x %>% 
    filter(!No. %in% c("pre", "post"))
  
  x_test <- x %>% 
    mutate(rti_linear_pre_rep_1 = as.integer(indexRtime(.$`Rep 1`,
                                                           data.frame(rtime = x_naps_pre$`Rep 1`,
                                                                      rindex = x_naps_pre$RTI))),
           rti_linear_post_rep_1 = as.integer(indexRtime(.$`Rep 1`,
                                                            data.frame(rtime = x_naps_post$`Rep 1`,
                                                                       rindex = x_naps_post$RTI))),
           rti_linear_average_rep_1 = as.integer(indexRtime(.$`Rep 1`,
                                                               data.frame(rtime = x_naps_average$`Rep 1`,
                                                                          rindex = x_naps_average$RTI)))) %>% 
    mutate(rti_linear_pre_rep_2 = as.integer(indexRtime(.$`Rep 2`,
                                                           data.frame(rtime = x_naps_pre$`Rep 2`,
                                                                      rindex = x_naps_pre$RTI))),
           rti_linear_post_rep_2 = as.integer(indexRtime(.$`Rep 2`,
                                                            data.frame(rtime = x_naps_post$`Rep 2`,
                                                                       rindex = x_naps_post$RTI))),
           rti_linear_average_rep_2 = as.integer(indexRtime(.$`Rep 2`,
                                                               data.frame(rtime = x_naps_average$`Rep 2`,
                                                                          rindex = x_naps_average$RTI)))) %>% 
    mutate(rti_linear_pre_rep_3 = as.integer(indexRtime(.$`Rep 3`,
                                                           data.frame(rtime = x_naps_pre$`Rep 3`,
                                                                      rindex = x_naps_pre$RTI))),
           rti_linear_post_rep_3 = as.integer(indexRtime(.$`Rep 3`,
                                                            data.frame(rtime = x_naps_post$`Rep 3`,
                                                                       rindex = x_naps_post$RTI))),
           rti_linear_average_rep_3 = as.integer(indexRtime(.$`Rep 3`,
                                                               data.frame(rtime = x_naps_average$`Rep 3`,
                                                                          rindex = x_naps_average$RTI)))) %>% 
    mutate(rti_spline_pre_rep_1 = as.integer(indexRtime(.$`Rep 1`,
                                                           data.frame(rtime = x_naps_pre$`Rep 1`,
                                                                      rindex = x_naps_pre$RTI),
                                                           FUN = rtiSpline)),
           rti_spline_post_rep_1 = as.integer(indexRtime(.$`Rep 1`,
                                                            data.frame(rtime = x_naps_post$`Rep 1`,
                                                                       rindex = x_naps_post$RTI),
                                                            FUN = rtiSpline)),
           rti_spline_average_rep_1 = as.integer(indexRtime(.$`Rep 1`,
                                                               data.frame(rtime = x_naps_average$`Rep 1`,
                                                                          rindex = x_naps_average$RTI),
                                                               FUN = rtiSpline))) %>% 
    mutate(rti_spline_pre_rep_2 = as.integer(indexRtime(.$`Rep 2`,
                                                           data.frame(rtime = x_naps_pre$`Rep 2`,
                                                                      rindex = x_naps_pre$RTI),
                                                           FUN = rtiSpline)),
           rti_spline_post_rep_2 = as.integer(indexRtime(.$`Rep 2`,
                                                            data.frame(rtime = x_naps_post$`Rep 2`,
                                                                       rindex = x_naps_post$RTI),
                                                            FUN = rtiSpline)),
           rti_spline_average_rep_2 = as.integer(indexRtime(.$`Rep 2`,
                                                               data.frame(rtime = x_naps_average$`Rep 2`,
                                                                          rindex = x_naps_average$RTI),
                                                               FUN = rtiSpline))) %>% 
    mutate(rti_spline_pre_rep_3 = as.integer(indexRtime(.$`Rep 3`,
                                                           data.frame(rtime = x_naps_pre$`Rep 3`,
                                                                      rindex = x_naps_pre$RTI),
                                                           FUN = rtiSpline)),
           rti_spline_post_rep_3 = as.integer(indexRtime(.$`Rep 3`,
                                                            data.frame(rtime = x_naps_post$`Rep 3`,
                                                                       rindex = x_naps_post$RTI),
                                                            FUN = rtiSpline)),
           rti_spline_average_rep_3 = as.integer(indexRtime(.$`Rep 3`,
                                                               data.frame(rtime = x_naps_average$`Rep 3`,
                                                                          rindex = x_naps_average$RTI),
                                                               FUN = rtiSpline))) %>% 
    mutate(rti_akima_pre_rep_1 = as.integer(indexRtime(.$`Rep 1`,
                                                          data.frame(rtime = x_naps_pre$`Rep 1`,
                                                                     rindex = x_naps_pre$RTI),
                                                          FUN = rtiAkima)),
           rti_akima_post_rep_1 = as.integer(indexRtime(.$`Rep 1`,
                                                           data.frame(rtime = x_naps_post$`Rep 1`,
                                                                      rindex = x_naps_post$RTI),
                                                           FUN = rtiAkima)),
           rti_akima_average_rep_1 = as.integer(indexRtime(.$`Rep 1`,
                                                              data.frame(rtime = x_naps_average$`Rep 1`,
                                                                         rindex = x_naps_average$RTI),
                                                              FUN = rtiAkima))) %>% 
    mutate(rti_akima_pre_rep_2 = as.integer(indexRtime(.$`Rep 2`,
                                                          data.frame(rtime = x_naps_pre$`Rep 2`,
                                                                     rindex = x_naps_pre$RTI),
                                                          FUN = rtiAkima)),
           rti_akima_post_rep_2 = as.integer(indexRtime(.$`Rep 2`,
                                                           data.frame(rtime = x_naps_post$`Rep 2`,
                                                                      rindex = x_naps_post$RTI),
                                                           FUN = rtiAkima)),
           rti_akima_average_rep_2 = as.integer(indexRtime(.$`Rep 2`,
                                                              data.frame(rtime = x_naps_average$`Rep 2`,
                                                                         rindex = x_naps_average$RTI),
                                                              FUN = rtiAkima))) %>% 
    mutate(rti_akima_pre_rep_3 = as.integer(indexRtime(.$`Rep 3`,
                                                          data.frame(rtime = x_naps_pre$`Rep 3`,
                                                                     rindex = x_naps_pre$RTI),
                                                          FUN = rtiAkima)),
           rti_akima_post_rep_3 = as.integer(indexRtime(.$`Rep 3`,
                                                           data.frame(rtime = x_naps_post$`Rep 3`,
                                                                      rindex = x_naps_post$RTI),
                                                           FUN = rtiAkima)),
           rti_akima_average_rep_3 = as.integer(indexRtime(.$`Rep 3`,
                                                              data.frame(rtime = x_naps_average$`Rep 3`,
                                                                         rindex = x_naps_average$RTI),
                                                              FUN = rtiAkima)))
  
  # summarize
  x_test_summary <- x_test %>% 
    rowwise() %>% 
    mutate(rt_mean = mean(c(`Rep 1`, `Rep 2`, `Rep 3`)),
           rt_sd = sd(c(`Rep 1`, `Rep 2`, `Rep 3`)),
           rt_rsd = rt_sd / rt_mean * 100,
           rti_linear_pre_mean = as.integer(mean(c(rti_linear_pre_rep_1, rti_linear_pre_rep_2, rti_linear_pre_rep_3))),
           rti_linear_pre_sd = as.integer(sd(c(rti_linear_pre_rep_1, rti_linear_pre_rep_2, rti_linear_pre_rep_3))),
           rti_linear_pre_rsd = rti_linear_pre_sd / rti_linear_pre_mean * 100,
           rti_linear_post_mean = as.integer(mean(c(rti_linear_post_rep_1, rti_linear_post_rep_2, rti_linear_post_rep_3))),
           rti_linear_post_sd = as.integer(sd(c(rti_linear_post_rep_1, rti_linear_post_rep_2, rti_linear_post_rep_3))),
           rti_linear_post_rsd = rti_linear_post_sd / rti_linear_post_mean * 100,
           rti_linear_average_mean = as.integer(mean(c(rti_linear_average_rep_1, rti_linear_average_rep_2, rti_linear_average_rep_3))),
           rti_linear_average_sd = as.integer(sd(c(rti_linear_average_rep_1, rti_linear_average_rep_2, rti_linear_average_rep_3))),
           rti_linear_average_rsd = rti_linear_average_sd / rti_linear_average_mean * 100,
           rti_spline_pre_mean = as.integer(mean(c(rti_spline_pre_rep_1, rti_spline_pre_rep_2, rti_spline_pre_rep_3))),
           rti_spline_pre_sd = as.integer(sd(c(rti_spline_pre_rep_1, rti_spline_pre_rep_2, rti_spline_pre_rep_3))),
           rti_spline_pre_rsd = rti_spline_pre_sd / rti_spline_pre_mean * 100,
           rti_spline_post_mean = as.integer(mean(c(rti_spline_post_rep_1, rti_spline_post_rep_2, rti_spline_post_rep_3))),
           rti_spline_post_sd = as.integer(sd(c(rti_spline_post_rep_1, rti_spline_post_rep_2, rti_spline_post_rep_3))),
           rti_spline_post_rsd = rti_spline_post_sd / rti_spline_post_mean * 100,
           rti_spline_average_mean = as.integer(mean(c(rti_spline_average_rep_1, rti_spline_average_rep_2, rti_spline_average_rep_3))),
           rti_spline_average_sd = as.integer(sd(c(rti_spline_average_rep_1, rti_spline_average_rep_2, rti_spline_average_rep_3))),
           rti_spline_average_rsd = rti_spline_average_sd / rti_spline_average_mean * 100,
           rti_akima_pre_mean = as.integer(mean(c(rti_akima_pre_rep_1, rti_akima_pre_rep_2, rti_akima_pre_rep_3))),
           rti_akima_pre_sd = as.integer(sd(c(rti_akima_pre_rep_1, rti_akima_pre_rep_2, rti_akima_pre_rep_3))),
           rti_akima_pre_rsd = rti_akima_pre_sd / rti_akima_pre_mean * 100,
           rti_akima_post_mean = as.integer(mean(c(rti_akima_post_rep_1, rti_akima_post_rep_2, rti_akima_post_rep_3))),
           rti_akima_post_sd = as.integer(sd(c(rti_akima_post_rep_1, rti_akima_post_rep_2, rti_akima_post_rep_3))),
           rti_akima_post_rsd = rti_akima_post_sd / rti_akima_post_mean * 100,
           rti_akima_average_mean = as.integer(mean(c(rti_akima_average_rep_1, rti_akima_average_rep_2, rti_akima_average_rep_3))),
           rti_akima_average_sd = as.integer(sd(c(rti_akima_average_rep_1, rti_akima_average_rep_2, rti_akima_average_rep_3))),
           rti_akima_average_rsd = rti_akima_average_sd / rti_akima_average_mean * 100) %>% 
    select(c("No.", "Name", "SMILES", "RTI", "exact mass", "formula", "logP", "Mix",
             "rt_mean", "rt_sd", "rt_rsd",
             "rti_linear_pre_mean", "rti_linear_pre_sd", "rti_linear_pre_rsd",
             "rti_linear_post_mean", "rti_linear_post_sd", "rti_linear_post_rsd",
             "rti_linear_average_mean", "rti_linear_average_sd", "rti_linear_average_rsd",
             "rti_spline_pre_mean", "rti_spline_pre_sd", "rti_spline_pre_rsd",
             "rti_spline_post_mean", "rti_spline_post_sd", "rti_spline_post_rsd",
             "rti_spline_average_mean", "rti_spline_average_sd", "rti_spline_average_rsd",
             "rti_akima_pre_mean", "rti_akima_pre_sd", "rti_akima_pre_rsd",
             "rti_akima_post_mean", "rti_akima_post_sd", "rti_akima_post_rsd",
             "rti_akima_average_mean", "rti_akima_average_sd", "rti_akima_average_rsd"))

  x_test_summary
  
  
}

# plotting of comparison
plot_rti_comparison <- function(plot_data, xlab = "", ylab = "", main = "") {
  
  p1 <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_point() +
    geom_line(data = tibble(x = c(0, 2000), y = c(0, 2000)), aes(x = x, y = y), colour = "red") +
    scale_x_continuous(limits = c(0,2000)) + 
    scale_y_continuous(limits = c(0,2000)) +
    coord_fixed() +
    xlab(paste0("RTI (", xlab, ")")) +
    ylab(paste0("RTI (", ylab, ")")) +
    ggtitle(main) +
    theme_bw()
  
  print(p1)
  
  p2 <- ggplot(plot_data, aes(x = x, y = diff)) +
    geom_point() +
    geom_errorbar(aes(ymin = 0 - xsd, ymax = 0 + xsd), colour = "darkgray") +
    geom_line(data = tibble(x = c(0, 2000), y = c(0, 2000 / 100 * 1)), aes(x = x, y = y), colour = "green", linetype = "dashed") +
    geom_line(data = tibble(x = c(0, 2000), y = c(0, 2000 / 100 * 3)), aes(x = x, y = y), colour = "orange", linetype = "dashed") +
    geom_line(data = tibble(x = c(0, 2000), y = c(0, 2000 / 100 * 5)), aes(x = x, y = y), colour = "red", linetype = "dashed") +
    geom_text(data = tibble(x = c(2000, 2000, 2000), y = c(2000/100*1, 2000/100*3, 2000/100*5), label = c("1%", "3%", "5%")), aes(x = x, y = y, label = label)) +
    geom_vline(xintercept = 300, colour = "black", linetype = "dashed") +
    scale_x_continuous(limits = c(0,2000)) +
    scale_y_continuous(limits = c(-100, 100)) +
    xlab(paste0("RTI (", xlab, ")")) +
    ylab(paste0("difference (", ylab, " - ", xlab, ")")) +
    ggtitle(main) +
    theme_bw()
  
  print(p2)
  
  p3 <- ggplot(plot_data[which(plot_data$x > 300),], aes(x = x, y = diff)) +
    geom_point() +
    geom_errorbar(aes(ymin = 0 - xsd, ymax = 0 + xsd), colour = "darkgray") +
    geom_line(data = tibble(x = c(0, 2000), y = c(0, 2000 / 100 * 1)), aes(x = x, y = y), colour = "green", linetype = "dashed") +
    geom_line(data = tibble(x = c(0, 2000), y = c(0, 2000 / 100 * 3)), aes(x = x, y = y), colour = "orange", linetype = "dashed") +
    geom_line(data = tibble(x = c(0, 2000), y = c(0, 2000 / 100 * 5)), aes(x = x, y = y), colour = "red", linetype = "dashed") +
    geom_text(data = tibble(x = c(2000, 2000, 2000), y = c(2000/100*1, 2000/100*3, 2000/100*5), label = c("1%", "3%", "5%")), aes(x = x, y = y, label = label)) +
    geom_vline(xintercept = 300, colour = "black", linetype = "dashed") +
    scale_x_continuous(limits = c(0,2000)) +
    scale_y_continuous(limits = c(-100, 100)) +
    xlab(paste0("RTI (", xlab, ")")) +
    ylab(paste0("difference (", ylab, " - ", xlab, ")")) +
    ggtitle(main) +
    theme_bw()
  
  print(p3)
  
}

read_data <- function(xlsx_file, plates) {
  
  # create empty tibble ----------------------------------------------------------
  ref_data_pos <- tibble()
  ref_data_neg <- tibble()
  
  # read reference data ----------------------------------------------------------
  for(plate in plates) {
    
    # read positive mode data
    plate_df <- read_xlsx(xlsx_file,
                          sheet = plate,
                          range = "A2:K138",
                          na = "n.d.") %>% 
      filter(!is.na(Name))
    
    clipboard <- tibble(plate = plate,
                        calc_rti(plate_df))
    
    
    ref_data_pos <- bind_rows(ref_data_pos, 
                              clipboard)
    
    # read negative mode data
    plate_df <- read_xlsx(xlsx_file,
                          sheet = plate,
                          range = "AH2:AR138",
                          na = "n.d.") %>% 
      filter(!is.na(Name))
    
    clipboard <- tibble(plate = plate,
                        calc_rti(plate_df))
    
    
    ref_data_neg <- bind_rows(ref_data_neg, 
                              clipboard)
    
  }
  
  # fitting of positive and negative mode data -----------------------------------
  ref_data_neg_linear <- ref_data_neg %>% 
    select(c("plate", "No.", "Name", "SMILES", "RTI", "exact mass", "formula", "logP", "Mix",
             "rti_linear_average_mean", "rti_linear_average_sd", "rti_linear_average_rsd"))
  
  ref_data_pos_linear <- ref_data_pos %>% 
    select(c("plate", "No.", "Name", "SMILES", "RTI", "exact mass", "formula", "logP", "Mix",
             "rti_linear_average_mean", "rti_linear_average_sd", "rti_linear_average_rsd"))
  
  full_join(ref_data_neg_linear,
            ref_data_pos_linear,
            by = c("plate" ,
                   "No.",
                   "Name",
                   "SMILES",
                   "RTI",                 
                   "exact mass",
                   "formula",
                   "logP",
                   "Mix"),
            suffix = c("_neg", "_pos")) %>% 
    mutate(diff = rti_linear_average_mean_neg - rti_linear_average_mean_pos,
           rti = case_when(is.na(rti_linear_average_mean_neg) & !is.na(rti_linear_average_mean_pos) ~ rti_linear_average_mean_pos,
                           !is.na(rti_linear_average_mean_neg) & is.na(rti_linear_average_mean_pos) ~ rti_linear_average_mean_neg,
                           TRUE ~ as.integer((rti_linear_average_mean_neg + rti_linear_average_mean_pos) / 2)))
  
}

# read RT data only
read_rt_data <- function(xlsx_file, plates) {
  
  # create empty tibble ----------------------------------------------------------
  ref_data_pos <- tibble()
  ref_data_neg <- tibble()
  
  # read reference data ----------------------------------------------------------
  for(plate in plates) {
    
    # read positive mode data
    plate_df <- read_xlsx(xlsx_file,
                          sheet = plate,
                          range = "A2:K138",
                          na = "n.d.") %>% 
      filter(!is.na(Name))
    
    clipboard <- tibble(plate = plate,
                        calc_rti(plate_df))
    
    
    ref_data_pos <- bind_rows(ref_data_pos, 
                              clipboard)
    
    # read negative mode data
    plate_df <- read_xlsx(xlsx_file,
                          sheet = plate,
                          range = "AH2:AR138",
                          na = "n.d.") %>% 
      filter(!is.na(Name))
    
    clipboard <- tibble(plate = plate,
                        calc_rti(plate_df))
    
    
    ref_data_neg <- bind_rows(ref_data_neg, 
                              clipboard)
    
  }
  
  # fitting of positive and negative mode data -----------------------------------
  ref_data_neg_linear <- ref_data_neg %>% 
    select(c("plate", "No.", "Name", "SMILES", "RTI", "exact mass", "formula", "logP", "Mix",
             "rt_mean", "rt_sd", "rt_rsd"))
  
  ref_data_pos_linear <- ref_data_pos %>% 
    select(c("plate", "No.", "Name", "SMILES", "RTI", "exact mass", "formula", "logP", "Mix",
             "rt_mean", "rt_sd", "rt_rsd"))
  
  full_join(ref_data_neg_linear,
            ref_data_pos_linear,
            by = c("plate" ,
                   "No.",
                   "Name",
                   "SMILES",
                   "RTI", 
                   "exact mass",
                   "formula",
                   "logP",
                   "Mix"),
            suffix = c("_neg", "_pos")) %>% 
    mutate(diff = rt_mean_neg - rt_mean_pos,
           rt = case_when(is.na(rt_mean_neg) & !is.na(rt_mean_pos) ~ rt_mean_pos,
                          !is.na(rt_mean_neg) & is.na(rt_mean_pos) ~ rt_mean_neg,
                          TRUE ~ (rt_mean_neg + rt_mean_pos) / 2))
  
}