
library(pacman)
pacman::p_load(tidyverse, readxl, openxlsx)
source("utilities_tidy.R")

# 
# 1_1) load the data save into data
datos <- 
  read_excel("D:\\OneDrive - CGIAR\\Data Analysis\\experimental_desing\\data\\202219DMF1C1_cere.xlsx", 
             sheet = "Data") # Data for testing

datos <- 
  read_excel("D:\\OneDrive - CGIAR\\Yuca Investigacion\\Mejoramiento\\YG2023 (Siembras 2022)\\CassavaBase\\Design\\202295DVGXE_cere_DISENO.xls")


# 1) Check duplicates in row and columns ----------------------------------

row_col_dup(sel_data_kp = datos)


# 2) function visualize the layout ----------------------------------------


ggplot(datos, aes(
  x = factor(col_number), y = factor(row_number),
  fill = factor(rep_number)
)) +
  geom_tile(color = "black", size = 0.5) + # Black border on tiles
  labs(x = "col_number", y = "row_number", fill = "rep") +
  coord_fixed() + # Square tiles
  theme_xiaofei() +
  geom_point(
    data = datos %>% filter(!is.na(is_a_control)),
    aes(size = is_a_control), alpha = 0.5
  ) + # Minimal theme, no grey background
  theme(
    panel.grid = element_blank(), # No underlying grid lines
    axis.text.x = element_text( # Vertical text on x axis
      angle = 0, vjust = 0.5, hjust = 0
    )
  )


# 3) check reps number ----------------------------------------------------

datos %>%
  count(plot_name, rep_number)  %>%
  arrange(desc(plot_name))


# 4) check duplicate in plot_number ---------------------------------------


plot_number_dup(datos)





