# setup plates to read ---------------------------------------------------------
plates <- c("Plate 1",
            "Plate 2",
            "Plate 3",
            "Plate 4",
            "Plate 5",
            "Plate 6",
            "Plate 7")

# read data from all files -----------------------------------------------------
rti_ref_data <- read_data("data/03/03_TotalData_InitialRIDatabase_ver2.xlsx", plates)
rti_flow_0.20mL <- read_data("data/03/03_FlowRate_0.20.xlsx", plates)
rti_flow_0.25mL <- read_data("data/03/03_FlowRate_0.25.xlsx", plates)
rti_flow_0.35mL <- read_data("data/03/03_FlowRate_0.35.xlsx", plates)
rti_flow_0.40mL <- read_data("data/03/03_FlowRate_0.40.xlsx", plates)

# combine data from all setups -------------------------------------------------
join_variables <- c("plate", "No.", "Name", "SMILES", "RTI", "exact mass",
                    "formula", "logP", "Mix", "rti")

rti_full_data <- full_join(rti_ref_data %>% select(join_variables),
                           rti_flow_0.20mL %>% select(join_variables),
                           by = c("plate", "No.", "Name", "SMILES", "RTI", "exact mass",
                                  "formula", "logP", "Mix"),
                           suffix = c("_ref", "_flow_0.20"))

rti_full_data <- full_join(rti_full_data,
                           rti_flow_0.25mL %>% select(join_variables),
                           by = c("plate", "No.", "Name", "SMILES", "RTI", "exact mass",
                                  "formula", "logP", "Mix")) %>% 
  rename("rti_flow_0.25" = "rti")

rti_full_data <- full_join(rti_full_data,
                           rti_flow_0.35mL %>% select(join_variables),
                           by = c("plate", "No.", "Name", "SMILES", "RTI", "exact mass",
                                  "formula", "logP", "Mix")) %>% 
  rename("rti_flow_0.35" = "rti")

rti_full_data <- full_join(rti_full_data,
                           rti_flow_0.40mL %>% select(join_variables),
                           by = c("plate", "No.", "Name", "SMILES", "RTI", "exact mass",
                                  "formula", "logP", "Mix")) %>% 
  rename("rti_flow_0.40" = "rti")

# plot data --------------------------------------------------------------------
p1_1 <- rti_full_data %>% 
  filter(rti_ref > 300) %>%
  ggplot(aes(x = rti_ref, y = rti_flow_0.20)) +
  geom_point(colour = viridis(4)[1]) +
  geom_line(data = data.frame(x = c(0,2000), y = c(0,2000)), aes(x = x, y = y), colour = "red", linetype = "dashed") +
  theme_bw()

p1_2 <- rti_full_data %>% 
  filter(rti_ref > 300) %>% 
  ggplot(aes(x = (rti_flow_0.20 - rti_ref) / rti_ref * 100)) +
  geom_histogram(binwidth = 5, fill = viridis(4)[1], colour = "black") +
  geom_vline(xintercept = 0, colour = "red", linetype = "dashed") +
  scale_x_continuous(limits = c(-50,50)) +
  scale_y_continuous(limits = c(0,200)) +
  theme_bw()

p2_1 <- rti_full_data %>% 
  filter(rti_ref > 300) %>%
  ggplot(aes(x = rti_ref, y = rti_flow_0.25)) +
  geom_point(colour = viridis(4)[2]) +
  geom_line(data = data.frame(x = c(0,2000), y = c(0,2000)), aes(x = x, y = y), colour = "red", linetype = "dashed") +
  theme_bw()

p2_2 <- rti_full_data %>% 
  filter(rti_ref > 300) %>% 
  ggplot(aes(x = (rti_flow_0.25 - rti_ref) / rti_ref * 100)) +
  geom_histogram(binwidth = 5, fill = viridis(4)[2], colour = "black") +
  geom_vline(xintercept = 0, colour = "red", linetype = "dashed") +
  scale_x_continuous(limits = c(-50,50)) +
  scale_y_continuous(limits = c(0,200)) +
  theme_bw()

p3_1 <- rti_full_data %>% 
  filter(rti_ref > 300) %>%
  ggplot(aes(x = rti_ref, y = rti_flow_0.35)) +
  geom_point(colour = viridis(4)[3]) +
  geom_line(data = data.frame(x = c(0,2000), y = c(0,2000)), aes(x = x, y = y), colour = "red", linetype = "dashed") +
  theme_bw()

p3_2 <- rti_full_data %>% 
  filter(rti_ref > 300) %>% 
  ggplot(aes(x = (rti_flow_0.35 - rti_ref) / rti_ref * 100)) +
  geom_histogram(binwidth = 5, fill = viridis(4)[3], colour = "black") +
  geom_vline(xintercept = 0, colour = "red", linetype = "dashed") +
  scale_x_continuous(limits = c(-50,50)) +
  scale_y_continuous(limits = c(0,200)) +
  theme_bw()

p4_1 <- rti_full_data %>% 
  filter(rti_ref > 300) %>%
  ggplot(aes(x = rti_ref, y = rti_flow_0.40)) +
  geom_point(colour = viridis(4)[4]) +
  geom_line(data = data.frame(x = c(0,2000), y = c(0,2000)), aes(x = x, y = y), colour = "red", linetype = "dashed") +
  theme_bw()

p4_2 <- rti_full_data %>% 
  filter(rti_ref > 300) %>% 
  ggplot(aes(x = (rti_flow_0.40 - rti_ref) / rti_ref * 100)) +
  geom_histogram(binwidth = 5, fill = viridis(4)[4], colour = "black") +
  geom_vline(xintercept = 0, colour = "red", linetype = "dashed") +
  scale_x_continuous(limits = c(-50,50)) +
  scale_y_continuous(limits = c(0,200)) +
  theme_bw()

(p1_1 | p2_1 | p3_1 | p4_1) / (p1_2 | p2_2 | p3_2 | p4_2)
