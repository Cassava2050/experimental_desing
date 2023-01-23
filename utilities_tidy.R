# Functions to make tidy data

# Load the theme used
theme_xiaofei <- function(){ 
  theme_gray() %+replace%
    theme(axis.text.x = element_text(face = "bold", colour = "black", size = 8, angle = 45, hjust = 1),
          axis.text.y = element_text(face = "bold", colour="black", size = 8),
          axis.title.y = element_text(size = 12,face = "bold", angle = 90, vjust = 3) ,
          axis.title.x = element_text(size = 12,face = "bold", vjust = -0.5) ,
          plot.title = element_text(size = 16, face = "bold.italic", hjust = 0.5),
          plot.margin = unit(c(1,1,1,2), "cm") # top, right, bottom, left
          #legend.position = "none"
    )}


# are there duplicated rows and columns?
# if so, change or remove

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


# 4) check duplicate in plot_number ---------------------------------------


plot_number_dup = function (sel_data_kp = sel_data_kp) {
  
  plot_number_ct = sel_data_kp %>%
    count(plot_number, sort=TRUE) %>%
    filter(n>1) %>%
    arrange(plot_number)
  
  if (nrow(plot_number_ct) > 0) {
    print("ERROR: There are duplicate plot number: ")
    #print(row_col_ct)
    
    duplicated_plot = sel_data_kp %>%
      filter(plot_number %in% plot_number_ct$plot_number) %>%
      select(plot_name, col_number, row_number, plot_number) %>%
      arrange(plot_number, row_number, col_number )
    
    print("Please fix the ERROR!, Here are the plot names:")
    print(duplicated_plot, n = Inf)
    
  }
  if (nrow(plot_number_ct) == 0) {
    print("Good, there is no duplicated plot number.")
  }
  
}



