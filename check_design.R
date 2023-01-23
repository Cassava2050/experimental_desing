
library(pacman)
pacman::p_load(tidyverse, readxl, yarrr, dplyr, knitr, rmarkdown, statgenSTA, statgenGxE, openxlsx, QBMS)
source("utilities_tidy.R")

# 
# 1_1) load the data save into data
library(readxl)
datos <- read_excel("D:\\OneDrive - CGIAR\\Yuca Investigacion\\Mejoramiento\\YG2023 (Siembras 2022)\\CassavaBase\\Design\\202295DVGXE_cere_DISENO.xls")

# 1_2) load data from clipboard

#datos <- read.delim("clipboard", header = T) %>% type.convert(as.is = FALSE)

# ---- function
row_col_dup = function (sel_data_kp= sel_data_kp) {
  
  row_col_ct = datos %>%
    count(col_number, row_number, sort=TRUE) %>%
    filter(n>1) %>%
    arrange(row_number, col_number)
  
  if (nrow(row_col_ct) >0) {
    print("ERROR: The duplicated row and column combination:")
    print(row_col_ct)
    
    row_col_ct_bind = row_col_ct %>%
      mutate(trial_row_col = paste(col_number, row_number, sep = "_"))
    
    duplicated_plot = sel_data_kp %>%
      mutate(trial_row_col = paste(col_number, row_number, sep = "_")) %>%
      filter(trial_row_col %in% row_col_ct_bind$trial_row_col) %>%
      select(plot_name, col_number, row_number, plot_number) %>%
      arrange(plot_number, row_number, col_number )
    
    print("Here are the plot names:")
    print(duplicated_plot)
    print("Please fix the ERROR!")
    return(duplicated_plot)
    
  }
  if (nrow(row_col_ct) == 0) {
    print("Good, there is no duplicated combination of row and column.")
  }
  
}

row_col_dup(sel_data_kp = datos)

# 2) function visualize the layout

# x = col_number
# y = row_number
# fill = factor(rep_number)

ggplot(datos, aes(x=col_number, y= row_number, fill=factor(rep_number))) +
  geom_tile(color="black", size=0.5) +           # Black border on tiles
  labs(x="col_number", y="row_number", fill = "rep") +
  coord_fixed() +                                # Square tiles
  theme_minimal() +                              # Minimal theme, no grey background
  theme(panel.grid=element_blank(),              # No underlying grid lines
        axis.text.x=element_text(                # Vertical text on x axis
          angle=0,vjust=0.5,hjust=0))

ggplot(datos, aes(x = factor(col_number), y = factor(row_number),
                  fill=factor(rep_number))) +
  geom_tile(color="black", size=0.5) +           # Black border on tiles
  labs(x="col_number", y="row_number", fill = "rep") +
  coord_fixed() +                                # Square tiles
  theme_minimal() +  
  geom_point(data = datos %>% filter(!is.na(is_a_control)),
             aes(size = is_a_control), alpha = 0.5) +  # Minimal theme, no grey background
  theme(panel.grid=element_blank(),              # No underlying grid lines
        axis.text.x=element_text(                # Vertical text on x axis
          angle=0,vjust=0.5,hjust=0))

# 3) check rep


#### check and correct rep number
# any wrong rep_number?

# use_trial_name = trial_name
# use_rep_number = rep_number

datos %>%
  count(plot_name, rep_number)  %>%
  arrange(desc(plot_name))



