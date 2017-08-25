
# Data Cleaning
data_cleaning <- function(data) {
  
  # Creation of a Data-Frame
  df_source <- as.data.frame(data);
  
  # Data-Distribution of Failure(1) and    Non-Failure States(0)
  print(table(df_source$failure))
  #     0         1 
  # 1048509      66 
  
  # Creating Clean Data-Frame
  df_clean  <- df_source
  
  # Drop un-necessary Fields
  df_clean$date <- NULL
  df_clean$serial_number <- NULL
  
  # make the model parameter categorical (character)
  df_clean$model <- as.character(df_clean$model)
  
  # Changing the colnames to more readable format
  colnames(df_clean) <- c("model", 
                          "capacity", "failure", 
                          "1_normalized", "1_raw",
                          "2_normalized", "2_raw",
                          "3_normalized", "3_raw",
                          "4_normalized", "4_raw",
                          "5_normalized", "5_raw",
                          "6_normalized", "6_raw",
                          "7_normalized", "7_raw",
                          "8_normalized", "8_raw",
                          "9_normalized", "9_raw",
                          "10_normalized", "10_raw",
                          "11_normalized", "11_raw",
                          "12_normalized", "12_raw",
                          "13_normalized", "13_raw",
                          "14_normalized", "14_raw",
                          "15_normalized", "15_raw",
                          "16_normalized", "16_raw",
                          "17_normalized", "17_raw",
                          "18_normalized", "18_raw",
                          "19_normalized", "19_raw",
                          "20_normalized", "20_raw",
                          "21_normalized", "21_raw",
                          "22_normalized", "22_raw",
                          "23_normalized", "23_raw",
                          "24_normalized", "24_raw",
                          "25_normalized", "25_raw",
                          "26_normalized", "26_raw",
                          "27_normalized", "27_raw",
                          "28_normalized", "28_raw",
                          "29_normalized", "29_raw",
                          "30_normalized", "30_raw",
                          "31_normalized", "31_raw",
                          "32_normalized", "32_raw",
                          "33_normalized", "33_raw",
                          "34_normalized", "34_raw",
                          "35_normalized", "35_raw",
                          "36_normalized", "36_raw",
                          "37_normalized", "37_raw",
                          "38_normalized", "38_raw",
                          "39_normalized", "39_raw",
                          "40_normalized", "40_raw",
                          "41_normalized", "41_raw",
                          "42_normalized", "42_raw",
                          "43_normalized", "43_raw",
                          "44_normalized", "44_raw",
                          "45_normalized", "45_raw");
  
  return(df_clean)
  ##########################
}

########################################################