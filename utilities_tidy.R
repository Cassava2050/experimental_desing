# Functions to make tidy data


read_CassavaBase = function(folder = folder,
                            base_data_raw=all_raw,
                            trial_interest = trial_interest,
                            year_interest = year_interest,
                            minimum_share = 20,
                            remove_trial = NA) {
  
  base_data_raw = all_raw
  # there are duplications in 2010, 2011 and 2012 trials in CassavaBase --- 2021 June 5
  # remove the duplicated trials from CassavaBase data
  all_trial = unique(base_data_raw$studyName)
  trial_10_11_12 = c( all_trial[str_detect(all_trial, "2010")], all_trial[str_detect(all_trial, "2011")],
                      all_trial[str_detect(all_trial, "2012")] )
  
  trial_10_11_12_del = trial_10_11_12[!str_detect(trial_10_11_12, "_")]
  base_data = base_data_raw %>%
    filter(!studyName%in%trial_10_11_12_del)
  all_trial = unique(base_data$studyName)
  
  ############################################################################
  #### select trials and historical trials with the clones
  
  # select the trials interested
  sel_trial = base_data %>%
    filter(grepl(trial_interest, studyName)) %>% # look for coincidence in trialName and studyName
    filter(studyYear %in% c(year_interest)) # filter in year interest (2021)
  # unique(sel_trial$studyName)
  
  clones_sel = unique(sel_trial$germplasmName) # clones interested 181 
  # length(clones_sel)
  
  #### historical trials with the clones interested
  clone_trial = base_data %>%
    select(germplasmName, studyName) %>%
    filter(germplasmName %in% clones_sel) %>%
    unique()
  
  # dim:361 x 2
  
  trial_kp = data.frame(table(clone_trial$studyName)) %>%
    filter(Freq >= all_of(minimum_share) )   # because of EPR, so set the high number
  dim(trial_kp)
  
  # remove trials by location
  if(!is.na(remove_trial)){ # if "remove trial is stated,I have to delete from data
    trial_kp = trial_kp[!trial_kp$Var1 %in% remove_trial,]
  }
  print("Here are the trials and the number of their shared clones with")
  print(paste(year_interest, trial_interest, sep=""))
  print(trial_kp)
  
  # DATA of historical trials with the clones interested
  sel_data = base_data %>%
    filter (studyName %in% trial_kp$Var1)
  dim(sel_data)
  
  # save_raw_file = paste("01_", year_interest, trial_interest,  "_raw_data_", Sys.Date(), ".csv", sep = "")
  # write.csv(sel_data, paste(folder, save_raw_file, sep=""), na ="", # na = "empty space"
  #           row.names=FALSE)
  
  return(sel_data)
}


# change column names into standar names

change_colname = function (sel_data = sel_data ,
                           new_names = NA){
  
  # I have the col name of database and the standardized col names
  new_col_names = read_excel("D:\\OneDrive - CGIAR\\Data Analysis\\Documento presentation 13 (Cassava dataset)\\(9) Genetic Gain Waxy\\data\\01_standard_col_names_2021March.xlsx",
                             sheet = "2021June05")
  
  # if there is new column, it will tell you what is the new names, then you need give the "new_names"
  # and the re-run the function
  if (sum(!names(sel_data) %in% new_col_names$database_col_name) != 0){
    unique_col = setdiff(names(sel_data), new_col_names$database_col_name)
    print("In the sel_data, there is/are unique column(s):")
    print(unique_col)
    print("Please add the unique column(s) to the standard col names")
    print("File location,
       D:\\OneDrive - CGIAR\\Data Analysis\\Documento presentation 13 (Cassava dataset)\\(9) Genetic Gain Waxy\\data\\01_standard_col_names_2021March.xlsx,
       sheet = 2021June05" )
    if(is.na(new_names)) {print("ERROR: The unique column(s) not changed yet")}
    if(!is.na(new_names)){
      unique_names = data.frame(database_col_name = unique_col,
                                analysis_col_name = new_names)
      new_col_names = rbind(new_col_names, unique_names)
      
      base_sel_colname = data.frame(names(sel_data))
      names(base_sel_colname) = "database_col_name"
      sel_new_colname = base_sel_colname %>%
        left_join(new_col_names, by = c( "database_col_name"))
      
      names(sel_data) = sel_new_colname$analysis_col_name
      print("Good, new columns were added. The column names are standardized names now!")
      
      ####  remove columns without data
      sel_data_kp = sel_data %>%
        select(   where(  ~!all(is.na(.x)) )    )
      
      return(sel_data_kp)
    }
    
    
  }
  # if there is no new col names, it is easy
  if (sum(!names(sel_data) %in% new_col_names$database_col_name) == 0){
    base_sel_colname = data.frame(names(sel_data))
    names(base_sel_colname) = "database_col_name"
    sel_new_colname = base_sel_colname %>%
      left_join(new_col_names, by = c( "database_col_name"))
    
    names(sel_data) = sel_new_colname$analysis_col_name
    print("Good, the column names are standardized names now!")
    
    ####  remove columns without data
    sel_data_kp = sel_data %>%
      select(   where(  ~!all(is.na(.x)) )   )
    
    return(sel_data_kp)
    
  }
}



# are there duplicated rows and columns?
# if so, change or remove

# ---- function
row_col_dup = function (sel_data_kp= sel_data_kp) {
  
  row_col_ct = sel_data_kp %>%
    count(use_trial_name, use_col_number, use_row_number, sort=TRUE) %>%
    filter(n>1) %>%
    arrange(use_row_number, use_col_number)
  
  if (nrow(row_col_ct) >0) {
    print("ERROR: The duplicated row and column combination:")
    print(row_col_ct)
    
    row_col_ct_bind = row_col_ct %>%
      mutate(trial_row_col = paste(use_trial_name, use_col_number, use_row_number, sep = "_"))
    
    duplicated_plot = sel_data_kp %>%
      mutate(trial_row_col = paste(use_trial_name, use_col_number, use_row_number, sep = "_")) %>%
      filter(trial_row_col %in% row_col_ct_bind$trial_row_col) %>%
      select(use_plot_name, use_col_number, use_row_number, use_trial_name, use_plot_number) %>%
      arrange(use_trial_name, use_plot_number, use_row_number, use_col_number )
    
    print("Here are the plot names:")
    print(duplicated_plot)
    print("Please fix the ERROR!")
    return(duplicated_plot)
    
  }
  if (nrow(row_col_ct) == 0) {
    print("Good, there is no duplicated combination of row and column.")
  }
  
}


#### function visualize the layout -

trial_layout = function(trial = sel_data_kp){
  trial_list = unique(trial$use_trial_name)
  for (i in 1:length(trial_list)){
    trial_i = trial %>%
      filter(use_trial_name %in% trial_list[i])
    myplot <- ggplot(trial_i, aes(x=use_col_number, y= use_row_number, fill=factor(use_rep_number))) +
      geom_tile(color="black", size=0.5) +           # Black border on tiles
      labs(x="col_number", y="row_number", fill = "rep",title = trial_list[i]) +
      coord_fixed() +                                # Square tiles
      theme_minimal() +                              # Minimal theme, no grey background
      theme(panel.grid=element_blank(),              # No underlying grid lines
            axis.text.x=element_text(                # Vertical text on x axis
              angle=0,vjust=0.5,hjust=0))
    print(myplot)
  }
}



### 2.7 convert accession_name to standard names and add the check_test column

# ---- function
check_clone_name = function(clone_list,
                            new_names = NA,
                            add_check = NULL) {
  
  ## 1). list of released varieties
  released = read_excel("D:\\OneDrive - CGIAR\\Data Analysis\\Documento presentation 13 (Cassava dataset)\\(9) Genetic Gain Waxy\\data\\01_Cassava Varieties with multiple names_2021Jan.xlsx",
                        sheet = "varietyName", na="")
  cat("Released varieties:")
  print(sort(released$accession_name)) # the correct name format
  
  ## 2). accessions in genebank
  eGWAS = "D:\\OneDrive - CGIAR\\Data Analysis\\Documento presentation 13 (Cassava dataset)\\(9) Genetic Gain Waxy\\data\\"
  genebank = read_excel(paste(eGWAS, "CIAT_genebank_cassava.xlsx", sep = ""),
                        sheet = "CASSAVA-GRP-CIAT (1)", skip = 6, na = "")
  genebank_clone = unique(genebank$`Accession number`)
  
  ## 3. other know clones
  # we can add more clones   -------- flexibility
  known_clone = c("C19", "C243", "C33", "C39", "C413", "TME3", "HB60" , "KU50" )
  cat("The other known clones:")
  print(sort(known_clone))
  
  # all the clones in the file
  accession_out = data.frame("oldName" = unique(clone_list), "newName" = unique(clone_list))
  accession_out$newName = gsub(" ", "", accession_out$newName) # remove space in names
  accession_all = unique(accession_out$newName)  # list of clones after remove space
  
  variety_wrong = accession_all [!str_detect(accession_all, "-") &   # without "-" in name
                                   !accession_all %in% genebank_clone &    # not in genebank
                                   !accession_all %in% known_clone &       # not in known clones
                                   !accession_all %in% released$accession_name] # not in released list
  
  
  
  if(sum(!is.na(new_names)) ==0) {
    
    if(length(variety_wrong)>0){
      print("The clones below did not have the correct name.")
      print("Need give the new name to -- new_names, then re-run the function")
      print(variety_wrong)
    }
    
    if(length(variety_wrong)==0){
      print("Good, the released names were correctly used")
      
      # trial accessions in released collections
      trial_clone_released = released %>%
        filter(accession_name %in% accession_out$newName) %>%
        data.frame()
      
      # change the trial accession into standard names
      if(nrow(trial_clone_released) >=1) {
        for(j in 1:nrow(trial_clone_released)){
          
          accession_out = accession_out %>%
            mutate(newName =
                     ifelse(newName == trial_clone_released[j,1] ,
                            trial_clone_released[j,2], newName) )
        }
      }
      
      if(nrow(trial_clone_released) ==0) {
        accession_out$newName = accession_out$oldName
      }
      
      accession_all_format = unique(accession_out$newName)
      variety_format = accession_all_format [str_detect(accession_all_format, "_is_")]
      print("Now the standard names are used. Here are the released varieties")
      print(variety_format)
      
      # add check_released column
      
      accession_out$use_check_released = NA
      check_list = c(variety_format, add_check)
      # change the check column
      if(length(check_list) >0){
        accession_out = accession_out %>%
          mutate(use_check_released =
                   ifelse(newName %in% check_list ,
                          "check_released", "test") )
      }
      
      if(length(check_list) == 0){
        accession_out$use_check_released = "test"
      }
      
      check_after = accession_out %>%
        filter(use_check_released == "check_released") %>%
        select(newName) %>%
        unique()
      print("The check or released clones are:")
      print(check_after)
      
      names(accession_out) = c("accession_name_ori", "use_accession_name",
                               "use_check_released")
      
      
      return(accession_out)
      
    }
    
    
  }
  
  
  
  if(sum(!is.na(new_names)) >0) {
    
    old_new_name = data.frame(old_name = variety_wrong,   # with both wrong and correct names
                              new_name = new_names)
    
    for(i in 1:nrow(old_new_name)){
      accession_out = accession_out %>%
        mutate(newName =
                 ifelse(newName == old_new_name[i,1] ,
                        old_new_name[i,2], newName) )   # replace with standard format
    }
    
    accession_all_modi = accession_out$newName
    variety_2wrong =  accession_all_modi [!str_detect( accession_all_modi, "-") &   # without "-" in name
                                            ! accession_all_modi %in% genebank_clone &    # not in genebank
                                            ! accession_all_modi %in% known_clone &       # not in known clones
                                            ! accession_all_modi %in% released$accession_name] # not in released list
    
    if(length(variety_2wrong)>0){
      print("The clones below might not have the correct name.")
      print("Please double-check them!")
      print(variety_2wrong)
    }
    
    if(length(variety_2wrong)==0){
      print("Good, the released names were correctly used")
    }
    
    
    # trial accessions in released collections
    trial_clone_released = released %>%
      filter(accession_name %in% accession_out$newName) %>%
      data.frame()
    
    # change the trial accession into standard names
    if(nrow(trial_clone_released) >0) {
      for(j in 1:nrow(trial_clone_released)){
        
        accession_out = accession_out %>%
          mutate(newName =
                   ifelse(newName == trial_clone_released[j,1] ,
                          trial_clone_released[j,2], newName) )
        
      }
    }
    
    if(nrow(trial_clone_released) ==0) {
      accession_out$newName = accession_out$oldName
    }
    
    accession_all_format = unique(accession_out$newName)
    variety_format = accession_all_format [str_detect(accession_all_format, "_is_")]
    print("Now the standard names are used. Here are the released varieties")
    print(variety_format)
    
    # add check_released column
    
    accession_out$use_check_released = NA
    check_list = c(variety_format, add_check)
    # change the check column
    if(length(check_list) >0){
      accession_out = accession_out %>%
        mutate(use_check_released =
                 ifelse(newName %in% check_list ,
                        "check_released", "test") )
    }
    
    if(length(check_list) == 0){
      accession_out$use_check_released = "test"
    }
    
    check_after = accession_out %>%
      filter(use_check_released == "check_released") %>%
      select(newName) %>%
      unique()
    print("The check or released clones are:")
    print(check_after)
    
    names(accession_out) = c("accession_name_ori", "use_accession_name",
                             "use_check_released")
    return(accession_out)
    
    
  }
  
  
  
}


#### function 6.1. check numeric of obs_ traits   ------------------------------ 6.1 6.1 6.1 6.1 6.1


is_numeric = function (trial_data) {
  
  all_trait = names(trial_data)[str_detect(names(trial_data), "obs_")]
  numeric_trait = names(select_if(trial_data[, all_trait], is.numeric))
  
  if(sum(all_trait%in% numeric_trait) == length(all_trait) ) {
    print("Good, all traits are numeric!")
  }
  if (sum(all_trait%in% numeric_trait) != length(all_trait) ) {
    print("The traits are not numeric. Need fix it!")
    print (all_trait [!all_trait%in% numeric_trait])
    print("After fixing the error, please re-run the function, is_numeric")
  }
  
}



