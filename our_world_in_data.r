

if (exists("destroy_environment")) destroy_environment()

source(file.path("utils.r"))


packages.get(c(
  "tidyverse",
  "janitor",
  "docstring",
  "ggcorrplot",
  "Hmisc",
  "here",
  "GGally"
))


get_OWID_df <- function(url = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv", ...){
  #' get_OWID_df
  #' 
  #' returns a dataframe of the Our world in data data on covid-19 https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv
  #' May used a chache if avaliable
  #' 
  #' @param url is a location of the data default is 
  #' @param ... is passed on to get_df_from_url
  #' 
  #' @details  for more information see https://github.com/owid/covid-19-data as well as https://ourworldindata.org/coronavirus 
  #' 

  df <- get_df_from_url(url) %>% 
    mutate_if(is.character, function(x){gsub(pattern = " ", replacement = "-", tolower(x))}) %>% 
    rename(genc3c := iso_code) %>% 
    rename(location_name := location) %>%
    mutate(
      fraction_positive_log = log_epsilon(total_cases/ total_tests), 
      fraction_tested_log = log_epsilon(total_tests / population),
      fraction_cases_log = log_epsilon(total_cases/ population),
      fraction_dead_log = log_epsilon(total_deaths/ population),
      total_tests_log = log_epsilon(total_tests),
      total_cases_log = log_epsilon(total_cases),
      total_deaths_log = log_epsilon(total_deaths),
      gdp_per_capita_log = log_epsilon(gdp_per_capita),
      population_log = log_epsilon(population)
    )
  
    
  return(df)
}



#owid_Df <- get_OWID_df()






plot_pairs_OWID <-function(owid_Df = get_OWID_df()){
  #' pairs plot of some of the OWID data
  ggp <- 
  owid_Df %>% 
    select(
      continent, location_name, date, population_log,
      fraction_positive_log, fraction_tested_log, fraction_cases_log,  fraction_dead_log, 
      gdp_per_capita_log#, 
      #total_tests, total_cases, total_deaths
    )  %>% 
    filter(continent != "") %>% 
    drop_na() %>% #count(location_name)
    group_by(location_name) %>%
    slice(which.max(date))  %>% 
    ungroup() %>%
    GGally::ggpairs(., columns =  4:ncol(.), ggplot2::aes(colour=continent, label = location_name, size = population_log))
  
  
  return(ggp)
}


plot_pairs_OWID()


##########
# Cases #

#in isolation GDP predicts covid cases
get_OWID_df() %>% 
  filter(continent != "") %>%
  glm(
    formula = fraction_cases_log ~ gdp_per_capita_log, 
    data = . ) %>% summary()    



#Also true for latest date data
get_OWID_df() %>% 
  filter(continent != "") %>%
  group_by(location_name) %>% slice(which.max(date))  %>% ungroup() %>%   
  glm(
    formula = fraction_cases_log ~ gdp_per_capita_log, 
    data = . ) %>% summary()  



#Also true when including Date in regression
get_OWID_df() %>% 
  filter(continent != "") %>%
  glm(
    formula = fraction_cases_log ~ date + gdp_per_capita_log, 
    data = . ) %>% summary()  


#It seems fraction positive does not get rid of the need for GDP
get_OWID_df() %>% 
  filter(continent != "") %>%
  group_by(location_name) %>% slice(which.max(date))  %>% ungroup() %>%     
  glm(
    formula = fraction_cases_log ~ gdp_per_capita_log + fraction_positive_log, 
    data = . ) %>% summary()  




#If we account for the fraction of the population tested this is taken care of it seems
get_OWID_df() %>% 
  filter(continent != "") %>%
  group_by(location_name) %>% slice(which.max(date))  %>% ungroup() %>%     
  glm(
    formula = fraction_cases_log ~ gdp_per_capita_log + fraction_tested_log, 
    data = . ) %>% summary()  







#It seems fraction tested and fraction positive are still good to have, even if GDP is taken care of now
get_OWID_df() %>% 
  filter(continent != "") %>%
  group_by(location_name) %>% slice(which.max(date))  %>% ungroup() %>%     
  glm(
    formula = fraction_cases_log ~ gdp_per_capita_log + fraction_tested_log + fraction_positive_log, 
    data = . ) %>% summary()  




########
#DEATHS#

#in isolation GDP predicts covid Deaths
a <- get_OWID_df() %>% 
  filter(continent != "") %>%
  glm(
    formula = fraction_dead_log ~ gdp_per_capita_log, 
    data = . ) %>% summary()  


#Also true for latest date data
get_OWID_df() %>% 
  filter(continent != "") %>%
  group_by(location_name) %>% slice(which.max(date))  %>% ungroup() %>%   
  glm(
    formula = fraction_dead_log ~ gdp_per_capita_log, 
    data = . ) %>% summary()  



#Also true when including Date in regression
get_OWID_df() %>% 
  filter(continent != "") %>%
  glm(
    formula = fraction_dead_log ~ date + gdp_per_capita_log, 
    data = . ) %>% summary()  



#It seems fraction positive does not get rid of the need for GDP
get_OWID_df() %>% 
  filter(continent != "") %>%
  group_by(location_name) %>% slice(which.max(date))  %>% ungroup() %>%     
  glm(
    formula = fraction_dead_log ~ gdp_per_capita_log + fraction_positive_log, 
    data = . ) %>% summary()  




#If we account for the fraction of the population tested this is taken care of it seems
get_OWID_df() %>% 
  filter(continent != "") %>%
  group_by(location_name) %>% slice(which.max(date))  %>% ungroup() %>%     
  glm(
    formula = fraction_dead_log ~ gdp_per_capita_log + fraction_tested_log, 
    data = . ) %>% summary()  




#It seems fraction tested and fraction positive are still good to have, even if GDP is taken care of now
get_OWID_df() %>% 
  filter(continent != "") %>%
  group_by(location_name) %>% slice(which.max(date))  %>% ungroup() %>%     
  glm(
    formula = fraction_dead_log ~ gdp_per_capita_log + fraction_tested_log + fraction_positive_log, 
    data = . ) %>% summary()  


