
# Remove all elements from eviroment space --------------------------------

rm(list = ls())

library(pacman)
pacman::p_load(tidyverse, readxl, openxlsx)
source("utilities_tidy.R")

folder_2022 = "D:\\OneDrive - CGIAR\\General\\YG2023 (Siembras 2022)\\"

location = "Costa" # "Costa", "Palmira", "Vietnam" ----- Select one

agronomist = "Jorge Ivan" # "Hernan Camilo", "Jorge Ivan", "Nelson", "Sandra", "Thuy" ---- Select one 

# 1)  Load the data save into datos object --------------------------------

list_file = list.files(paste0(folder_2022, location, "\\", agronomist, "\\"))
list_file

# the data we will use

sel_file_use = list_file[15]
sel_file_use


# Load the data file selected ---------------------------------------------


datos = read_excel(paste0(folder_2022, location, "\\", agronomist, "\\", sel_file_use), 
                   sheet = "Data"
)

# Verify colnames ---------------------------------------------------------

colnames(datos)


# Select colnames required by Cassava Base --------------------------------

CB_colnames <- colnames(all_of(datos))[1:12]
CB_colnames



# Filter datos by CB_colnames ---------------------------------------------

datos_design <- datos %>% select(CB_colnames)



# If agronomist is Jorge Ivan, then: --------------------------------------

if(agronomist == "Jorge Ivan") {
  datos_design <- datos_design[-1, ] %>% 
    mutate_at(CB_colnames[-c(1:2)], as.numeric)
  
}


# 2) Check duplicates in row and columns ----------------------------------

row_col_dup(sel_data_kp = datos_design)


# 3) check reps number ----------------------------------------------------

datos_design %>%
  count(plot_name, rep_number)  %>%
  arrange(desc(plot_name))


# 4) check duplicate in plot_number ---------------------------------------


plot_number_dup(datos_design)

# 2) function visualize the layout ----------------------------------------

ggplot(datos_design, aes(x = factor(col_number), y = factor(row_number),
                         fill= factor(rep_number))) +
  geom_tile(color="black", linewidth=0.5) +           # Black border on tiles
  labs(x="col_number", y="row_number", fill = "rep") +
  coord_fixed() +                                # Square tiles
  theme_xiaofei() +  
  geom_point(data = datos_design %>% filter(!is.na(is_a_control)),
             aes(size = is_a_control), alpha = 0.5) +  # Minimal theme, no grey background
  theme(panel.grid=element_blank(),              # No underlying grid lines
        axis.text.x=element_text(                # Vertical text on x axis
          angle=0,vjust=0.5,hjust=0))



# Save the data into a xlsx file ------------------------------------------

folder_output <- "D:\\OneDrive - CGIAR\\Yuca Investigacion\\Mejoramiento\\YG2023 (Siembras 2022)\\CassavaBase\\Design\\"

master_data = list()

master_data[[("Sheet1")]] <- datos_design

meta_file_name <- paste0(folder_output, paste0("desing_", sel_file_use))

write.xlsx(master_data, file = meta_file_name)

