#setwd("H:/nhs_ptd_power_bi")

# Import all required NHS R Plot the Dots scripts
source('./r_files/flatten_HTML.r')
source("./R/ptd_spc.R")
source("./R/ptd_rebase.R")
source("./R/ptd_target.R")
source("./R/ptd_spc_options.R")
source("./R/ptd_validate_spc_options.R")
source("./R/ptd_validate_plot_options.R")
source("./R/to_datetime.R")
source("./R/ZZZ.R")
source("./R/ptd_add_rebase_column.R")
source("./R/ptd_calculate_point_type.R")
source("./R/ptd_add_short_group_warnings.R")
source("./R/ptd_add_target_column.R")
source("./R/ptd_calculate_assurance_type.R")
source("./R/ptd_spc_standard.R")
# source("./R/helpers.R")
# source("./R/sparkline.R")
# source("./R/reactable.R")
# source("./R/columns.R")
# source("./R/utils.R")
# source("./R/onLOAD.R")

############### Library Declarations ###############
libraryRequireInstall("plotly")
libraryRequireInstall("dplyr")
libraryRequireInstall("DT")
libraryRequireInstall("lubridate")
libraryRequireInstall("tidyr")
# libraryRequireInstall("xfun")
####################################################

################### Actual code ####################
# 'Values' is the input received from PowerBI
# Note that it seems a bit funny about the format dates are passed through in
# TODO: Make date parsing more flexible - at the moment it will *only* work with yyyy-mm-dd 
# (or possibly a very comprehensive mm-dd-yyyy, and that might depend on locale settings...) 

# If testing, a sample dataset can be loaded from here (changing the path below the 
# repository if necessary)
# Double rebase
# dataset <- read.csv("H:\\nhs_ptd_power_bi\\sample_datasets\\spc_xmr_sample_dataset_double_rebase_increase_good_trending_higher_inconsistent.csv") %>%
#           mutate(date = lubridate::dmy(date))
# More than one KPI
# dataset <- read.csv("H:\\nhs_ptd_power_bi\\sample_datasets\\spc_xmr_sample_dataset_multiple_areas_double_rebase_increase.csv") %>%
#           mutate(date = lubridate::dmy(date))
# Lots of KPIs
# dataset <- read.csv("H:\\nhs_ptd_power_bi\\sample_datasets\\spc_xmr_sample_dataset_10_areas.csv") %>%
#           mutate(date = lubridate::dmy(date))
# More than one KPI - gappy
# dataset <- read.csv("H:\\nhs_ptd_power_bi\\sample_datasets\\spc_xmr_sample_dataset_multiple_areas_double_rebase_increase.csv") %>%
#           mutate(date = lubridate::dmy(date)) %>%
#   filter(date != '2022-03-31') %>%
#   filter(date != '2023-01-01')
# dataset <- read.csv("H:\\nhs_ptd_power_bi\\sample_datasets\\spc_xmr_sample_dataset_multiple_areas_double_rebase_increase.csv") %>%
#           mutate(date = lubridate::dmy(date)) %>%
#   filter(date != '2022-03-31') %>%
#   filter(date != '2023-01-01')
# Dataset with additional dimension
# dataset <- read.csv("H:\\nhs_ptd_power_bi\\sample_datasets\\spc_xmr_sample_dataset_10_areas_kpis_with_second_dimension.csv") %>%
#           mutate(date = lubridate::dmy(date))

# Import the mandatory columns
if(exists("value")) value <- value else value <- NULL
if(exists("date")) date <- date else date <- NULL

dataset <- cbind(value, date) 

# Import the optional columns
if(exists("what") && !is.null(what)) dataset <- bind_cols(dataset, what) else dataset <- dataset %>% mutate(what = NA)
if(exists("improvement_direction") && !is.null(improvement_direction)) dataset <- bind_cols(dataset, improvement_direction) else dataset <- dataset %>% mutate(improvement_direction = NA)
if(exists("target") && !is.null(target)) dataset <- bind_cols(dataset, target) else dataset <- dataset %>% mutate(target = NA)
if(exists("annotations") && !is.null(annotations)) dataset <- bind_cols(dataset, annotations) else dataset <- dataset %>% mutate(annotations = NA)
if(exists("recalc_here") && !is.null(recalc_here)) dataset <- bind_cols(dataset, recalc_here) else dataset <- dataset %>% mutate(recalc_here = NA)
if(exists("is_percentage") && !is.null(is_percentage)) dataset <- bind_cols(dataset, is_percentage) else dataset <- dataset %>% mutate(is_percentage = NA)
if(exists("baseline_duration") && !is.null(baseline_duration)) dataset <- bind_cols(dataset, baseline_duration) else dataset <- dataset %>% mutate(baseline_duration = NA)
if(exists("additional_dimension") && !is.null(additional_dimension)) dataset <- bind_cols(dataset, additional_dimension) else dataset <- dataset %>% mutate(additional_dimension = NA)

colnames(dataset) <- c("value", "date", "what", "improvement_direction", "target", "annotations", "recalc_here", "is_percentage", "baseline_duration", "additional_dimension") 

if(exists("outputtypesettings_OutputType")) outputtypesettings_OutputType <- outputtypesettings_OutputType else outputtypesettings_OutputType <- "graph"

if(exists("legendsettings_LegendPosition")) legendsettings_LegendPosition <- legendsettings_LegendPosition else legendsettings_LegendPosition <- "below"
if(legendsettings_LegendPosition == "off" | outputtypesettings_OutputType == "card") showLegend <- FALSE else showLegend <- TRUE


if(exists("manualrebasesettings_ManualRebasePoints")) manualrebasesettings_ManualRebasePoints <- (manualrebasesettings_ManualRebasePoints %>% strsplit(split=","))[[1]] else manualrebasesettings_ManualRebasePoints <- NULL


if(!is.null(manualrebasesettings_ManualRebasePoints)) manualrebasesettings_ManualRebasePoints <- manualrebasesettings_ManualRebasePoints %>% as.Date(optional=TRUE, format = c("%Y-%m-%d"))
if(!is.null(manualrebasesettings_ManualRebasePoints) && all(is.na(manualrebasesettings_ManualRebasePoints))) manualrebasesettings_ManualRebasePoints <- NULL else manualrebasesettings_ManualRebasePoints <- na.omit(manualrebasesettings_ManualRebasePoints)

dataset <- dataset %>% 
  mutate(date = as.Date(date)) 

if(exists("spcsettings_PadWithZeros")) spcsettings_PadWithZeros <- spcsettings_PadWithZeros else spcsettings_PadWithZeros <- FALSE

if(spcsettings_PadWithZeros == TRUE) {
  

  dataset <- dataset %>%
    group_by(what) %>%
    arrange(what, date) %>%
    mutate(Gap = difftime(lead(date), date, unit="days") %>% as.numeric()) %>% 
    mutate(ApproxInterval = case_when(
      mean(Gap, na.rm=TRUE) > 300 ~ "Years",
      mean(Gap, na.rm=TRUE) > 20 && day(min(date, na.rm=TRUE)) > 28 ~ "Month Ends",
      mean(Gap, na.rm=TRUE) > 20 && day(min(date, na.rm=TRUE)) == 1 ~ "Month Starts",
      mean(Gap, na.rm=TRUE) > 5 ~ "Weeks",
      TRUE ~ "Days"
    )
  ) %>% 
    ungroup()
  
  if (is.na(dataset %>% distinct(what) %>% pull())) {
    
    
    if (unique(dataset$ApproxInterval) == "Years")  {date_seq <- seq.Date(min(dataset$date), max(dataset$date),  by="year")
    
    # If month ends
    } else if(unique(dataset$ApproxInterval) == "Month Ends") { date_seq <- seq.Date(min(dataset$date)+1, max(dataset$date)+1, 
                                                                                         by="month") -1 
    # If months starts
    } else if (unique(dataset$ApproxInterval) == "Month Starts")  { date_seq <- seq.Date(min(dataset$date), max(dataset$date)+1, 
                                                                                             by="month")
    # Weeks
    } else if (unique(dataset$ApproxInterval) == "Weeks" )  {date_seq <- seq.Date(min(dataset$date), max(dataset$date),  
                                                                                      by="week")
    # Days                                                                                                                                                       
    } else {date_seq <- seq.Date(min(dataset$date), max(dataset$date), by="day")
    
    }
    
    dataset <- dataset %>% 
      arrange(date) %>% 
      tidyr::complete(date = date_seq) %>%
      mutate(value = tidyr::replace_na(value, 0)) %>% 
      tidyr::fill_(names(dataset)) %>%
      select(-Gap) 
    
  } else {
  
    sub_datasets <- list()
    
    for (one_what in dataset %>% distinct(what) %>% pull()) {
    
      sub_dataset <- dataset %>% filter(what == one_what)
      
      if (unique(sub_dataset$ApproxInterval) == "Years")  {date_seq <- seq.Date(min(sub_dataset$date), max(sub_dataset$date),  by="year")
      
      # If month ends
      } else if(unique(sub_dataset$ApproxInterval) == "Month Ends") { date_seq <- seq.Date(min(sub_dataset$date)+1, max(sub_dataset$date)+1, 
                                                                                 by="month") -1 
      # If months starts
      } else if (unique(sub_dataset$ApproxInterval) == "Month Starts")  { date_seq <- seq.Date(min(sub_dataset$date), max(sub_dataset$date)+1, 
                                                                                       by="month")
      # Weeks
      } else if (unique(sub_dataset$ApproxInterval) == "Weeks" )  {date_seq <- seq.Date(min(sub_dataset$date), max(sub_dataset$date),  
                                                                            by="week")
      # Days                                                                                                                                                       
      } else {date_seq <- seq.Date(min(sub_dataset$date), max(sub_dataset$date), by="day")
      
      }
      
      sub_datasets[[one_what]] <- sub_dataset %>% 
        arrange(date) %>% 
        tidyr::complete(date = date_seq) %>%
        mutate(value = tidyr::replace_na(value, 0)) %>% 
        tidyr::fill_(names(sub_dataset)) %>%
        select(-Gap) 
        
    
    
    }
    
    dataset <- sub_datasets %>% bind_rows() %>% arrange(what, date)
  
  }
    
}

##########################################################
# Begin calculations for instances where we pass in 
# multiple datasets and don't want them aggregated
##########################################################

if (outputtypesettings_OutputType == "summarytable" | 
    outputtypesettings_OutputType == "summarytable2" |
    outputtypesettings_OutputType == "summarymatrix" | 
    outputtypesettings_OutputType == "facet_graph") {
  
  if(exists("spcsettings_ValueIsPercentage")) spcsettings_ValueIsPercentage <- spcsettings_ValueIsPercentage else spcsettings_ValueIsPercentage <- NULL
  
  ptd_objects <- list()
  ptd_objects_tibble <- list()

  
  dataset <- dataset %>% 
    mutate(combined_what = paste(what, additional_dimension)) 
  
  
  for (combined_what in 1:nrow(dataset %>% distinct(combined_what))) { 
  
    what_item <- (dataset %>% distinct(combined_what) %>% pull())[[combined_what]]
    
    single_what <- dataset %>% 
      filter(combined_what == what_item) 
    
    # Get any target values (if included)
    # If present, pass through to ptd target function
    if(is.na(unique(single_what$target))) target <- NULL else target <- unique(single_what$target) %>% ptd_target()
    #if(exists("spcsettings_Target")) spcsettings_Target <- spcsettings_Target else spcsettings_Target <- NULL
    #if(is.na(target)) target <- spcsettings_Target
    
    # Take improvement direction from where it is specified in original dataframe
    # TO BE DECIDED - is this best provided in the dataframe, or should this be an option in the PBI dataframe?
    # My current thinking is that while dataframe is inefficient for storage, it's far more efficient for creating 
    # a lot of visuals at once 
    improvement_direction <- single_what %>% 
      tail(1) %>% 
      select(improvement_direction) %>% 
      distinct() %>% 
      pull() %>% 
      # Force as character to appease the PBI service
      as.character()
    
    # If no improvement direction passed in the dataset, take the value from the dropdown instead
    # Note default in dropdown is "increase", in line with SPC defaults
    if(exists("spcsettings_ImprovementDirection")) spcsettings_ImprovementDirection <- spcsettings_ImprovementDirection else spcsettings_ImprovementDirection <- "increase"
    if (is.na(improvement_direction)) improvement_direction <- spcsettings_ImprovementDirection
    
    # Look at the dataset to determine whether something has been passed that tells us it's a percentage
    # If not, look at the SPC settings
    if(is.null(unique(single_what$is_percentage) | is.na(unique(single_what$is_percentage)) )) is_percentage <- NULL else is_percentage <- unique(single_what$is_percentage)
    if((is.null(is_percentage) | is.na(is_percentage)) && !is.null(spcsettings_ValueIsPercentage)) is_percentage <- spcsettings_ValueIsPercentage
    
    target <- single_what %>%
      tail(1) %>%
      select(target) %>%
      pull()
    
    if (is.na(target)) target <- NULL
    if(exists("spcsettings_Target")) spcsettings_Target <- spcsettings_Target else spcsettings_Target <- NULL
    if (is.null(target) & !is.null(spcsettings_Target)) target <- spcsettings_Target
    
    # Generate NHS R making data count object
    ptd_df <- ptd_spc(single_what, 
                          value_field = "value",
                          date_field="date", 
                          improvement_direction = improvement_direction,
                          fix_after_n_points = if(is.na(unique(single_what$baseline_duration))) NULL else unique(single_what$baseline_duration),
                          rebase = if(!is.null(manualrebasesettings_ManualRebasePoints)) manualrebasesettings_ManualRebasePoints %>% ptd_rebase() else if ((single_what %>% filter(stringr::str_detect(recalc_here,"y|Y|yes|Yes|YES")) %>% nrow()) < 1) NULL else (single_what %>% filter(stringr::str_detect(recalc_here,"y|Y|yes|Yes|YES")) %>% select(date) %>% distinct() %>% pull()) %>% as.Date() %>% ptd_rebase(),
                          target = if(is.null(target)) target else (target %>% ptd_target()) 
    ) %>% 
      # We want the underlying dataframe rather than the resulting plot
      # so convert to tibble
      as_tibble() %>% 
      # Tweak point type text for nicer display
      mutate(point_type = case_when(
        point_type == "special_cause_concern" ~ "Special Cause - Concern",
        point_type == "special_cause_improvement" ~ "Special Cause - Improvement",
        point_type == "common_cause" ~ "Common Cause",
        TRUE ~ "ERROR - CHECK"
      )) %>% 
      mutate(title = what_item) %>% 
      mutate(is_percentage = if(!is.null(is_percentage) | !is.na(is_percentage)) is_percentage else NA)
    
    
    is_percentage_ptd_object <- ptd_df %>% distinct(is_percentage) %>% pull()
    
    if(is.null(is_percentage_ptd_object) | 
       is.na(is_percentage_ptd_object) | 
       (!is.null(is_percentage_ptd_object) && is_percentage_ptd_object==FALSE)
    ) tickhoverformat <- ',' else tickhoverformat <- ',.0%'
    
    if(outputtypesettings_OutputType == "summarytable2") {
    temp_plot <- (ptd_df %>% 
      ggplot(aes(x=x)) + 
      geom_line(aes(y=y)) + 
      geom_point(aes(y=y, color=point_type)) + 
      theme_void() + 
      scale_colour_manual(values=c("Special Cause - Concern" = "#ED8B00",
                                                             "Special Cause - Improvement" = "#41B6E6",
                                                             "Common Cause" = "#768692")) + 
      theme(legend.position="none", plot.margin = margin(0.1,0.1,0.1,0.1, "mm")#, 
            
            ) + # Rectangle size around label) +
      geom_hline(aes(yintercept=target), linetype="dotted", color="red") + 
        geom_line(aes(x=x, y=lpl), linetype="dotted", color="grey") + 
        geom_line(aes(x=x, y=upl), linetype="dotted", color="grey") + 
      #geom_text(aes(x=quantile(x, 0.1), label=min(x) %>% format("%d %b\n%y"), y=min(ptd_df$y, na.rm=TRUE)+abs(y*0.1)), data=ptd_df %>% arrange(x) %>% head(1), size=1.5) + 
      #geom_text(aes(x=quantile(x, 0.9), label=max(x) %>% format("%d %b\n%y"), y=min(ptd_df$y, na.rm=TRUE)+abs(y*0.1)),  data=ptd_df %>% arrange(x) %>% tail(1), size=1.5) + 
        # Add label showing lowest value 
        geom_label(aes(x=x, y=y, label=if(is_percentage) paste0(round(y * 100, 1), "%") else y), 
                      data=ptd_df %>% arrange(y) %>% head(1), 
                      size=1.5, 
                      label.padding = unit(0.1, "lines"), 
                      nudge_y=abs(ptd_df %>% arrange(y) %>% head(1) %>% pull()) * 0.1
                 )  + 
      # Add label showing highest value
      geom_label(aes(x=x, y=y, label=if(is_percentage) paste0(round(y * 100, 1), "%") else y), 
                 data=ptd_df %>% arrange(y) %>% tail(1), 
                 size=1.5, 
                 label.padding = unit(0.1, "lines"), 
                 nudge_y=abs(ptd_df %>% arrange(y) %>% tail(1) %>% pull()) * -0.1
                 )  +
      ylim(min(min(ptd_df$y, na.rm=TRUE), min(ptd_df$lpl, na.rm=TRUE)) -abs(min(ptd_df$y, na.rm=TRUE)*0.2), 
           
           max(max(ptd_df$y, na.rm=TRUE), max(ptd_df$upl, na.rm=TRUE))+abs(max(ptd_df$y, na.rm=TRUE)*0.2))
      #geom_hline(aes(yintercept=upl), linetype="dotted")  + 
      #geom_hline(aes(yintercept=lpl), linetype="dotted")  
        
        ) %>%
      ggsave("sparkline.png.tmp", ., width=30, height=8, units="mm", device="png", dpi=200)
    }
    
    # Store this for use in the faceted graph
    ptd_objects_tibble[[what_item]] <- ptd_df
  
    # Now do some additional processing to make it more useful for the graph-type plots
    if (!is.na(ptd_df %>% distinct(target) %>% pull())) assurance_type <- ptd_calculate_assurance_type_2(ptd_df, improvement_direction) %>% select(assurance_type) %>% pull() else assurance_type <- ""
    
    final_row <- ptd_df %>% arrange(x) %>% tail(1)
      
    ptd_objects[[combined_what]] <- tibble(
      What = single_what %>% distinct(what) %>% pull() %>% as.character(),
      `Additional Dimension` = single_what %>% distinct(additional_dimension) %>% pull() %>% as.character(),
      `Most Recent Data Point` =  final_row %>% pull(x),
      `Most Recent Value` =  final_row %>% pull(y),
      Mean = final_row %>% pull(mean),
      Max = max(ptd_df$y, na.rm=TRUE),
      Min = min(ptd_df$y, na.rm=TRUE),
      EarliestDate = min(ptd_df$x, na.rm=TRUE),
      LatestDate = max(ptd_df$x, na.rm=TRUE),
      `Lower Process Limit` = final_row %>% pull(lpl),
      `Upper Process Limit` = final_row %>% pull(upl),
      `Target` = final_row %>% pull(target),
      `Variation` = final_row %>% pull(point_type),
      `Assurance` = assurance_type,
      `Improvement Direction` = case_when(improvement_direction == "decrease" ~ "Lower is good", 
                                          improvement_direction == "increase" ~ "Higher is good", 
                                          TRUE ~ "No direction is an improvement"),
      `Combined What` = what_item,
      # sparkline=list(htmltools::tagList(sparkline))
      sparkline=if(outputtypesettings_OutputType == "summarytable2") base64enc::dataURI(file = "sparkline.png.tmp", mime = "image/png") else NA
        
    ) %>%
      mutate(is_percentage = if(!is.null(is_percentage) | !is.na(is_percentage)) is_percentage else NA)%>% 
      mutate(Assurance = case_when(
        
        Assurance == "consistent_pass" ~ "Consistently Meeting Target",
        Assurance == "inconsistent" ~ "Inconsistent - Sometimes Meeting Target, Sometimes Failing to Meet Target",
        Assurance == "consistent_fail" ~ "Consistently Failing to Meet Target",
        Assurance == "" ~ "No Target",
        TRUE ~ "ERROR - Check"
        
      )) 
  
  }
  
  ptd_summary_table <- ptd_objects %>% 
    bind_rows() 
  
 
}

if (outputtypesettings_OutputType == "facet_graph") {

  #if (exists("spcsettings_ValueIsPercentage") && spcsettings_ValueIsPercentage == TRUE) tickhoverformat <- ',.0%' else tickhoverformat <- ""
  
  spc_plots <- list()
  
    for (j in 1:length(ptd_objects_tibble)) {
  
        ptd_object <- ptd_objects_tibble[[j]] 
        
        #if (!is.na(ptd_object %>% distinct(is_percentage) %>% head(1) %>% pull()) && (ptd_object %>% distinct(is_percentage) %>% head(1) %>% pull()) != FALSE) tickhoverformat <- ',.0%' else tickhoverformat <- '' 
  
        is_percentage_ptd_object <- ptd_object %>% distinct(is_percentage) %>% pull()
        
        if(is.null(is_percentage_ptd_object) | 
           is.na(is_percentage_ptd_object) | 
           (!is.null(is_percentage_ptd_object) && is_percentage_ptd_object==FALSE)
           ) tickhoverformat <- ',' else tickhoverformat <- ',.0%'
        
           
        
        fig <- plot_ly(ptd_object,
                        x = ~x,
                        colors = c("Special Cause - Concern" = "#ED8B00",
                                   "Special Cause - Improvement" = "#41B6E6",
                                   "Common Cause" = "#768692"))
          
          fig <- fig %>%
            # Add the main line for the data
            add_trace(y = ~y, 
                      name = 'trace 0',
                      type="scatter",
                      mode = 'lines', 
                      line=list(color='#768692'),
                      showlegend=FALSE) %>%
            # Add in markers for the data, colouring by the point types
            # and using the palette we passed when initialising the figure
            add_trace(y = ~y,
                      type="scatter",
                      mode = 'markers', 
                      color = ~point_type, 
                      showlegend=showLegend,
                      marker=list(size=if(exists("pointsettings_PointSize")) pointsettings_PointSize else 8)
          ) %>%
            # Add in line for lower process limit
            add_trace(y = ~lpl, 
                      name = 'Lower Process Limit',
                      type="scatter",
                      mode = 'lines', 
                      line=list(color='#231f20', dash="dot"),
                      showlegend=FALSE) %>%
            # Add in line for upper process limit
            add_trace(y = ~upl, 
                      name = 'Upper Process Limit',
                      type="scatter",
                      mode = 'lines', 
                      line=list(color='#231f20', dash="dot"),
                      showlegend=FALSE) %>%
            # Add in line for mean
            # TODO: Investigate whether this should be median. Median doesn't appear in plot
            # but I thought that was MDC methodology - I'm probably misremembering.
            add_trace(y = ~mean, name = 'Mean',
                      type="scatter",
                      mode = 'lines', 
                      line=list(color='#231f20'),
                      showlegend=FALSE)
          
  
          target <- ptd_object %>%
            tail(1) %>%
            select(target) %>%
            pull()
  
          # If a target is provided, add in a line for the target
          if (!is.null(target)) {
            fig <- fig %>%
              add_trace(y = ~target, name = 'Target',
                        type="scatter",
                        mode = 'lines', 
                        line=list(color='#DA291C', dash="dot"),
                        showlegend=FALSE)
          }
          
          # Calculate variation type by looking at final point in ptd object
          variation_type <- ptd_object %>%
            tail(1) %>%
            select(point_type) %>%
            pull()
          
          # Get variation image paths
          # Variation image relies on both the value of the most recent point
          # and the direction that is counted as improvement
          # Improvement direction was calculated earlier to pass to ptd arguments
          # TODO: Add in support for 'neutral' improvement direction
          if(variation_type == "Special Cause - Concern" & improvement_direction == "decrease") variation_image <- "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/variation/concern_high.svg"
          if(variation_type == "Special Cause - Concern" & improvement_direction == "increase") variation_image <- "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/variation/concern_low.svg"
          if(variation_type == "Special Cause - Improvement" & improvement_direction == "decrease") variation_image <- "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/variation/improvement_low.svg"
          if(variation_type == "Special Cause - Improvement" & improvement_direction == "increase") variation_image <- "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/variation/improvement_high.svg"
          if(variation_type == "Common Cause") variation_image <- "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/variation/common_cause.svg"
          
          
          # Get assurance image paths
          # NHS R PTD package provides a helper function for calculating this from the PTD object
          if (!is.null(target)) {
          
          assurance_type <- ptd_calculate_assurance_type_2(ptd_object, improvement_direction) %>% select(assurance_type) %>% pull()
          
          if(assurance_type == "inconsistent") assurance_image <- "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/assurance/inconsistent.svg"
          if(assurance_type =="consistent_pass") assurance_image <- "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/assurance/pass.svg"
          if(assurance_type == "consistent_fail") assurance_image <- "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/assurance/fail.svg"
          
          }
          
          if (is.null(target)) assurance_image <- ""
          
          # Get settings from power bi visual formatting options
          #if(exists("titlesettings_TitleJustification")) titlesettings_TitleJustification <- titlesettings_TitleJustification else titlesettings_TitleJustification <- "center"
          #if(exists("titlesettings_TitleOn")) titlesettings_TitleOn <- titlesettings_TitleOn else titlesettings_TitleOn <- TRUE
  
          # If using default title in a card visual, wrap it
          title <- ptd_object %>%
            tail(1) %>%
            select(title) %>%
            pull()
  
            fig <- fig %>%
            layout(
  
            xaxis = list(title = if(exists("xaxissettings_XAxisTitle")) xaxissettings_XAxisTitle else ""),
            yaxis = list(title = if(exists("yaxissettings_YAxisTitle")) yaxissettings_YAxisTitle else "",
                        tickformat=tickhoverformat),
  
  
              annotations=list(
                      text=stringr::str_wrap(title, 25),
                        font=list(size=if(exists("titlesettings_TitleSize")) titlesettings_TitleSize else 10),
                        x = 0.5,  
                        y = 1.0,  
                        xref = "paper",  
                        yref = "paper",  
                        xanchor = "center",  
                        yanchor = "bottom",  
                        showarrow = FALSE 
                        ),
  
              # Add in icons for variation and, if target present, assurance
              # Note that assurance will not always be present, so place variation icon
              # in the far left top hand corner and assurance to the right of it if present
              # Try to get these as far out of the way as possible
              # TODO: add in user options for icon placement
  
              # Useful reference for images:
              # https://plotly.com/r/reference/layout/images/
              # https://plotly.com/r/images/
  
              # TODO: Work out how to add a tooltip explaining the meaning of the icons on hover.
              # From docs, doesn't appear to be something we can add directly to the images
              # Think we will need an invisible point where the images are
              # but this could be tricky to achieve because of the way the image locations
              # and sizes are set.
  
              # TODO: Have not yet verified whether the images work when visual is running on
              # PBI service rather than PBI desktop. Plotly seems to only accept images from web source,
              # but I worry that PBI service will block these requests. Note to self - would base64
              # encoding of the images work if required? Or look into plotly source code at what
              # exactly is happening at this step - what aspect of it being 'on the web' is crucial?
              # Because we can include additional assets in the pbi visual package so I don't think
              # that's an issue.
  
              images = list(
  
                list(
                  source =  assurance_image,
                  xref="paper",
                  yref="paper",
                  x=0.22  ,
                  y=1.05,
                xanchor="right",
                yanchor="top",
                sizex=if(exists("iconsettings_IconSize")) iconsettings_IconSize else 0.1,
                sizey=if(exists("iconsettings_IconSize")) iconsettings_IconSize else 0.1
                ) ,
  
                list(
  
                  source =  variation_image,
                  xref="paper",
                  yref="paper",
                  x=0.1,
                  y=1.05,
                  xanchor="right",
                  yanchor="top",
                  sizex=if(exists("iconsettings_IconSize")) iconsettings_IconSize else 0.1,
                  sizey=if(exists("iconsettings_IconSize")) iconsettings_IconSize else 0.1
  
                )
  
              )
            )
          
          if (legendsettings_LegendPosition == "below") {
          
            fig <- fig %>%
              layout(legend = list(orientation = 'h', 
                          x=0.5, 
                          y=-0.175, 
                          #yref="container", 
                          xanchor="center",
                          itemsizing="constant"
                          )
            )
  
          }
  
      spc_plots[[j]] <- fig
  
  
    }
  
  
    if(exists("facetsettings_NumRows")) facetsettings_NumRows <- facetsettings_NumRows else facetsettings_NumRows <- 1
    if (facetsettings_NumRows == 1) margin_facet <- 0.02 else margin_facet <- c(0.02, 0.02, 0.08, 0.08)  
  
    fig <- subplot(spc_plots, shareX=TRUE, 
                   # Limitation of R plotly - shareY argument works only by row
                   # https://community.plotly.com/t/subplot-sharex-and-sharey-only-work-by-column-and-row/32532/2
                   # https://stackoverflow.com/questions/57672160/plotly-scaley-not-working-across-subplot-rows
                   # Python implementation allows use of additional 'all' argument but this doesn't appear to be 
                   # supported in R: https://plotly.github.io/plotly.py-docs/plotly.subplots.html
                   shareY = if(exists("facetsettings_FixedYAxisScale")) facetsettings_FixedYAxisScale else FALSE, 
                   nrows=facetsettings_NumRows, margin=margin_facet)

}

if (outputtypesettings_OutputType == "summarytable") {
  
  if(all(is.na(ptd_summary_table %>% distinct(`Additional Dimension`) %>% pull()))) {
    ptd_summary_table <- ptd_summary_table %>% 
      select(-c(`Additional Dimension`) ) 
  } 
  
  fig <- ptd_summary_table %>% 
    select(-c(`Combined What`, `is_percentage`
              #, sparkline
              )) %>% 
    mutate_if(is.numeric, round, 1) %>%
    mutate(`Most Recent Data Point` = `Most Recent Data Point` %>% format("%d %b %Y")) %>% 
    mutate(Variation = as.factor(Variation),
           Assurance = as.factor(Assurance)) %>% 
    DT::datatable(filter='top', 
                  rownames = FALSE,
                  fillContainer = FALSE,
                  autoHideNavigation = FALSE,
                  #options=list(scrollY = "100px")
                  )
  
}


if (outputtypesettings_OutputType == "summarytable2") {
  
  fig <- ptd_summary_table %>% 
    mutate(variation_image = case_when(
      (Variation == "Special Cause - Concern" & `Improvement Direction` == "Lower is good") ~ "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/variation/concern_high.svg",
      (Variation == "Special Cause - Concern" & `Improvement Direction` == "Higher is good") ~ "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/variation/concern_low.svg",
      (Variation == "Special Cause - Improvement" & `Improvement Direction` == "Lower is good") ~ "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/variation/improvement_low.svg",
      (Variation == "Special Cause - Improvement" & `Improvement Direction` == "Higher is good") ~ "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/variation/improvement_high.svg",  
      Variation == "Common Cause" ~ "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/variation/common_cause.svg",
      TRUE ~ "")
  ) %>% 
    mutate(assurance_image = case_when(
      Assurance == "Inconsistent - Sometimes Meeting Target, Sometimes Failing to Meet Target" ~ "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/assurance/inconsistent.svg",
      Assurance == "No Target" ~ "",
      Assurance == "Consistently Meeting Target" ~ "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/assurance/pass.svg",
      Assurance == "Consistently Failing to Meet Target" ~ "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/assurance/fail.svg",
      TRUE ~ ""
    )
    ) %>% 
    mutate(variation_image = paste0('<img src="', variation_image,'", alt="', Variation, '" style="width:30px;height:30px;">') ) %>% 
    mutate(assurance_image = case_when(assurance_image == "" ~ "", TRUE ~ paste0('<img src="', assurance_image,'", alt="', Assurance, '" style="width:30px;height:30px;">'))) %>%
  mutate(`Most Recent Value` = case_when(is_percentage == TRUE ~ paste0(round(as.numeric(`Most Recent Value`) * 100, 1), "%"), 
                                         TRUE ~ as.character(`Most Recent Value`))
         ) %>% 
    mutate(`Assurance Variation Combination` = case_when(
      
      Variation == "Special Cause - Concern" | Assurance == "<b style='color:#8A1538;'>Consistently Failing to Meet Target" ~ "Key Area of Concern</b><br/>",
      Assurance == "Inconsistent - Sometimes Meeting Target, Sometimes Failing to Meet Target" ~ "<b style='color:#ED8B00;'>Not Consistently Meeting Target</b><br/>",
      Variation == "Special Cause - Improvement" ~ "<b style='color:#009639;'>Area to Celebrate</b><br/>",
      TRUE ~ ""
      
    )) %>%
    
  #mutate(sparkline = spk_chr(values = 1:3, elementId = paste0("spark", row_number()))) %>%
  mutate(output_string = paste0(`Assurance Variation Combination`, '<small>Most Recent Value:</small> <big>', `Most Recent Value`, 
                                '</big><br/> ', variation_image, assurance_image,
                                '<br/> <img src="', sparkline, '" alt="Sparkline"> <br/>',
                                '<small>', EarliestDate %>% format("%d %b %y"), ' to ', LatestDate %>% format("%d %b %y"), "</small>"
                                )
         ) %>%
    select(What, `Additional Dimension`, output_string) %>% 
    tidyr::spread(key=`Additional Dimension`, value=output_string) %>% 
    DT::datatable(filter='none', 
                  rownames = FALSE,
                  autoHideNavigation = FALSE,
                  escape = FALSE,
                  fillContainer = TRUE,
                  options = list(
                  dom = 'Brt', scrollY = "200px"#,
                      # fnDrawCallback = htmlwidgets::JS('function(){
                      #                                         HTMLWidgets.staticRender();
                      #                                         }')
                  ),
                    class = 'cell-border'
                    #options=list(scrollY = "100px")
      )  #%>%
    #htmltools::attachDependencies(
    #  htmlwidgets::getDependency('sparkline')
    #)
    
  #fig %>% spk_add_deps() 

  #fig
  

  
}


if (outputtypesettings_OutputType == "summarymatrix") {
  
  one_of_each_variation <- c("Special Cause - Concern", "Common Cause", "Special Cause - Improvement")
  one_of_each_assurance <- c("Consistently Failing to Meet Target", 
                              "No Target", 
                              "Inconsistent - Sometimes Meeting Target, Sometimes Failing to Meet Target",
                              "Consistently Meeting Target")
  
  fig <- ptd_summary_table %>% 
    select(What, Variation, Assurance) %>%
    # Wonky temporary workaround to ensure every column exists without having to do lots of 'exists' checks
    union_all(one_of_each_variation %>% as_tibble() %>% rename(Variation = value)) %>% 
    union_all(one_of_each_assurance %>% as_tibble() %>% rename(Assurance = value))  %>% 
    mutate(Variation = factor(Variation, 
                              levels=c("Special Cause - Concern", "Common Cause", "Special Cause - Improvement"))) %>% 
    arrange(Variation) %>% 
    group_by(Variation, Assurance) %>%
    summarise(Result=paste(What, collapse='<br/><br/>')) %>%
    ungroup() %>% 
    tidyr::spread(key=Variation, value=Result)  %>% 
    filter(!is.na(Assurance)) %>% 
    select(-`<NA>`) %>% 
    mutate(Assurance = factor(Assurance, 
                              levels=c("Consistently Failing to Meet Target", 
                                       "No Target", 
                                       "Inconsistent - Sometimes Meeting Target, Sometimes Failing to Meet Target",
                                       "Consistently Meeting Target"))) %>% 
    arrange(Assurance) %>%  
    rename(` ` = `Assurance`) %>%
    DT::datatable(filter='none', 
                  escape=FALSE,
                  rownames = FALSE,
                  autoHideNavigation = FALSE,
                  fillContainer = TRUE,
                  options = list(
                    dom = 'Brt', scrollY = "200px"
                  )
                  )%>% 
    DT::formatStyle(columns = c(" "), fontWeight = 'bold', `text-align` = 'left')
    
  
}

if (outputtypesettings_OutputType == "graph" | outputtypesettings_OutputType == "card") {

  # Get any target values (if included)
  # If present, pass through to ptd target function
  if(is.na(unique(dataset$target))) target <- NULL else target <- unique(dataset$target) %>% ptd_target()
  if(exists("spcsettings_Target")) spcsettings_Target <- spcsettings_Target else spcsettings_Target <- NULL
  if (is.null(target) & !is.null(spcsettings_Target)) target <- spcsettings_Target
  
  # Take improvement direction from where it is specified in original dataframe
  # TO BE DECIDED - is this best provided in the dataframe, or should this be an option in the PBI dataframe?
  # My current thinking is that while dataframe is inefficient for storage, it's far more efficient for creating 
  # a lot of visuals at once 
  improvement_direction <- dataset %>% 
    tail(1) %>% 
    select(improvement_direction) %>% 
    distinct() %>% 
    pull() %>% 
    # Force as character to appease the PBI service
    as.character()

  # If no improvement direction passed in the dataset, take the value from the dropdown instead
  # Note default in dropdown is "increase", in line with SPC defaults
  if(exists("spcsettings_ImprovementDirection")) spcsettings_ImprovementDirection <- spcsettings_ImprovementDirection else spcsettings_ImprovementDirection <- "increase"
  if (is.na(improvement_direction)) improvement_direction <- spcsettings_ImprovementDirection
  
  # Generate NHS R making data count object
  ptd_object <- ptd_spc(dataset, 
          value_field = "value",
          date_field="date", 
          improvement_direction = improvement_direction,
          fix_after_n_points = if(is.na(unique(dataset$baseline_duration))) NULL else unique(dataset$baseline_duration),
          rebase = if(!is.null(manualrebasesettings_ManualRebasePoints)) manualrebasesettings_ManualRebasePoints %>% ptd_rebase() else if((dataset %>% filter(stringr::str_detect(recalc_here,"y|Y|yes|Yes|YES")) %>% nrow()) < 1) NULL else (dataset %>% filter(stringr::str_detect(recalc_here,"y|Y|yes|Yes|YES")) %>% select(date) %>% distinct() %>% pull()) %>% as.Date() %>% ptd_rebase(),
          target = target
          ) %>% 
    # We want the underlying dataframe rather than the resulting plot
    # so convert to tibble
    as_tibble() %>% 
    # Tweak point type text for nicer display
    mutate(point_type = case_when(
      point_type == "special_cause_concern" ~ "Special Cause - Concern",
      point_type == "special_cause_improvement" ~ "Special Cause - Improvement",
      point_type == "common_cause" ~ "Common Cause",
      TRUE ~ "ERROR - CHECK"
    )) 

  # Initialise the plotly figure
  fig <- plot_ly(ptd_object,
                 x = ~x,
                 colors = c("Special Cause - Concern" = "#ED8B00",
                            "Special Cause - Improvement" = "#41B6E6",
                            "Common Cause" = "#768692")) %>%
    # Add the main line for the data
    add_trace(y = ~y, 
              name = 'trace 0',
              type="scatter",
              mode = 'lines', 
              line=list(color='#768692'),
              showlegend=FALSE) %>%
    # Add in markers for the data, colouring by the point types
    # and using the palette we passed when initialising the figure
    add_trace(y = ~y,
              type="scatter",
              mode = 'markers', 
              color = ~point_type, 
              showlegend=showLegend,
              marker=list(size=if(exists("pointsettings_PointSize")) pointsettings_PointSize else 8)
  ) %>%
    # Add in line for lower process limit
    add_trace(y = ~lpl, 
              name = 'Lower Process Limit',
              type="scatter",
              mode = 'lines', 
              line=list(color='#231f20', dash="dot"),
              showlegend=FALSE) %>%
    # Add in line for upper process limit
    add_trace(y = ~upl, 
              name = 'Upper Process Limit',
              type="scatter",
              mode = 'lines', 
              line=list(color='#231f20', dash="dot"),
              showlegend=FALSE) %>%
    # Add in line for mean
    # TODO: Investigate whether this should be median. Median doesn't appear in plot
    # but I thought that was MDC methodology - I'm probably misremembering.
    add_trace(y = ~mean, name = 'Mean',
              type="scatter",
              mode = 'lines', 
              line=list(color='#231f20'),
              showlegend=FALSE
              )
  
  # If a target is provided, add in a line for the target
  if (!is.null(target)) {
    fig <- fig %>%
      add_trace(y = ~target, name = 'Target',
                type="scatter",
                mode = 'lines', 
                line=list(color='#DA291C', dash="dot"),
                showlegend=FALSE)
  }
  
  # Calculate variation type by looking at final point in ptd object
  variation_type <- ptd_object %>%
    tail(1) %>%
    select(point_type) %>%
    pull()
  
  # Get variation image paths
  # Variation image relies on both the value of the most recent point
  # and the direction that is counted as improvement
  # Improvement direction was calculated earlier to pass to ptd arguments
  # TODO: Add in support for 'neutral' improvement direction
  if(variation_type == "Special Cause - Concern" & improvement_direction == "decrease") variation_image <- "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/variation/concern_high.svg"
  if(variation_type == "Special Cause - Concern" & improvement_direction == "increase") variation_image <- "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/variation/concern_low.svg"
  if(variation_type == "Special Cause - Improvement" & improvement_direction == "decrease") variation_image <- "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/variation/improvement_low.svg"
  if(variation_type == "Special Cause - Improvement" & improvement_direction == "increase") variation_image <- "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/variation/improvement_high.svg"
  if(variation_type == "Common Cause") variation_image <- "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/variation/common_cause.svg"
  
  # Get assurance image paths
  # NHS R PTD package provides a helper function for calculating this from the PTD object
  if (!is.null(target)) {
  
    assurance_type <- ptd_calculate_assurance_type_2(ptd_object, improvement_direction) %>% select(assurance_type) %>% pull()
    
    if(assurance_type == "inconsistent") assurance_image <- "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/assurance/inconsistent.svg"
    if(assurance_type =="consistent_pass") assurance_image <- "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/assurance/pass.svg"
    if(assurance_type == "consistent_fail") assurance_image <- "https://raw.githubusercontent.com/Bergam0t/nhs_ptd_power_bi/main/inst/icons/assurance/fail.svg"
    
  } else { assurance_image <- "" }
  
  # Get settings from power bi visual formatting options
  if (exists("titlesettings_TitleOn") && titlesettings_TitleOn == TRUE) {

    what_column <- dataset %>% distinct(what) %>% pull()
    if(!is.na(what_column) & length(what_column) == 1) default_title <- what_column else default_title <- NA
    if(exists("titlesettings_ChartTitle")) titlesettings_ChartTitle <- titlesettings_ChartTitle else titlesettings_ChartTitle <- ""
    if(!is.na(default_title) & (titlesettings_ChartTitle=="") | is.na(titlesettings_ChartTitle)) title <- default_title else title <- titlesettings_ChartTitle

    # If using default title in a card visual, wrap it
    if (outputtypesettings_OutputType == "card" & (!is.na(default_title) & (titlesettings_ChartTitle=="") | is.na(titlesettings_ChartTitle))) {
        title <- stringr::str_wrap(title, 20)
    }

  } else {

    title <- ""

  }

  
  # Look at the dataset to determine whether something has been passed that tells us it's a percentage
  # If not, look at the SPC settings
  if(any( is.null(unique(dataset$is_percentage)) | is.na(unique(dataset$is_percentage)) )) is_percentage <- NULL else is_percentage <- unique(dataset$is_percentage)
  if(exists("spcsettings_ValueIsPercentage")) spcsettings_ValueIsPercentage <- spcsettings_ValueIsPercentage else spcsettings_ValueIsPercentage <- NULL
  if(is.null(is_percentage) & !is.null(spcsettings_ValueIsPercentage)) is_percentage <- spcsettings_ValueIsPercentage
  
  if(is.null(is_percentage) | (!is.null(is_percentage) && is_percentage == FALSE)) tickhoverformat <- ',' else tickhoverformat <- ',.0%'

}

if (outputtypesettings_OutputType == "graph") {
  
  
    # Update fig to include variation icon and, if present, assurance icon
  # Also pass in user parameters from the PBI visual formatting options for titles
  fig <- fig %>%
    layout(

    xaxis = list(title = if(exists("xaxissettings_XAxisTitle")) xaxissettings_XAxisTitle else ""),
    yaxis = list(title = if(exists("yaxissettings_YAxisTitle")) yaxissettings_YAxisTitle else "",
                       tickformat = tickhoverformat),


      title=list(text=title,
                 font=list(size=if(exists("titlesettings_TitleSize")) titlesettings_TitleSize else 10),
                 automargin=TRUE,
                 yref='container',
                 yanchor ='top',
                 xred = if(exists("titlesettings_TitleJustification") && titlesettings_TitleJustification == "central") "center" else "left"
                 
                 ),

      # Add in icons for variation and, if target present, assurance
      # Note that assurance will not always be present, so place variation icon
      # in the far left top hand corner and assurance to the right of it if present
      # Try to get these as far out of the way as possible
      # TODO: add in user options for icon placement

      # Useful reference for images:
      # https://plotly.com/r/reference/layout/images/
      # https://plotly.com/r/images/

      # TODO: Work out how to add a tooltip explaining the meaning of the icons on hover.
      # From docs, doesn't appear to be something we can add directly to the images
      # Think we will need an invisible point where the images are
      # but this could be tricky to achieve because of the way the image locations
      # and sizes are set.

      # TODO: Have not yet verified whether the images work when visual is running on
      # PBI service rather than PBI desktop. Plotly seems to only accept images from web source,
      # but I worry that PBI service will block these requests. Note to self - would base64
      # encoding of the images work if required? Or look into plotly source code at what
      # exactly is happening at this step - what aspect of it being 'on the web' is crucial?
      # Because we can include additional assets in the pbi visual package so I don't think
      # that's an issue.

      images = list(

        list(
          source =  assurance_image,
          xref="paper",
          yref="paper",
          x=0.22  ,
          y=1.05,
         xanchor="right",
         yanchor="top",
         sizex=if(exists("iconsettings_IconSize")) iconsettings_IconSize else 0.1,
         sizey=if(exists("iconsettings_IconSize")) iconsettings_IconSize else 0.1
        ) ,

         list(

          source =  variation_image,
          xref="paper",
          yref="paper",
          x=0.1,
          y=1.05,
          xanchor="right",
          yanchor="top",
          sizex=if(exists("iconsettings_IconSize")) iconsettings_IconSize else 0.1,
          sizey=if(exists("iconsettings_IconSize")) iconsettings_IconSize else 0.1

        )

      )
    )
  
  if (legendsettings_LegendPosition == "below") {
  
    fig <- fig %>%
      layout(legend = list(orientation = 'h', 
                  x=0.5, 
                  y=-0.175, 
                  #yref="container", 
                  xanchor="center",
                  itemsizing="constant"
                  )
    )

  }
  
} else if (outputtypesettings_OutputType == "card") {
  
  if (exists("spcsettings_ValueIsPercentage") && (!is.null(is_percentage) && is_percentage == TRUE)) tickhoverformat <- ',.0%' else tickhoverformat <- ','

  m <- list(
    
    l = 10,
    
    r = 10,
    
    b = 10,
    
    t = 10,
    
    pad = 4
    
  )
  
  fig_plot <- fig %>% 
    layout(
      
   margin=m,
   
   xaxis = list(title = "", showticklabels=FALSE, showgrid=FALSE),
   
   yaxis = list(title = "", showticklabels=FALSE, showgrid=FALSE,
                zerolinecolor = '#ffff',
                hoverformat = tickhoverformat)
   
    ) %>% 
    config(displayModeBar = FALSE)
 
  
  # Create figure that is icons
  m2 <- list(
    
    l = 0,
    
    r = 0,
    
    b = 0,
    
    t = 0,
    
    pad = 0
    
  )
  

  t <- list(
    
    family = "sans serif",
    
    size = 14,
    
    color = toRGB("grey50")
    )


  if (exists("titlesettings_TitleOn") && titlesettings_TitleOn == TRUE) {
  

  card_title <- list(text=title,
               font=list(size=if(exists("titlesettings_TitleSize")) titlesettings_TitleSize*3 else 10*3),
               #automargin=TRUE,
               yref='paper',
               yanchor = 'top',
               y=0.95,
               xref=if(exists("cardsettings_CardTitleJustification") && cardsettings_CardTitleJustification == "central") "center" else "left",
               x=if(exists("cardsettings_CardTitleJustification") && cardsettings_CardTitleJustification == "central") 0.5 else 0.05
               )

  } else {

  card_title <- NULL

  }

  card_text <- dataset %>% 
        arrange(desc(date)) %>% 
        head(1) %>% 
        select(value) %>% 
        pull()

  if (tickhoverformat == ',.0%') card_text <- paste0((card_text * 100) %>% round(1), "%")
  
  
  fig_icons <- plotly_empty() %>% 
    layout(
    images = list(
      
      list(
        source =  assurance_image,
        xref="paper",
        yref="paper",
        x=if(exists("cardsettings_IconPosition") && cardsettings_IconPosition == "central") 0.9 else 0.9,
        y=if(exists("cardsettings_IconPosition") && cardsettings_IconPosition == "central") 0.4 else 0.95,
        xanchor="center",
        yanchor="top",
        sizex=if(exists("iconsettings_IconSize")) iconsettings_IconSize*4 else 0.1*4,
        sizey=if(exists("iconsettings_IconSize")) iconsettings_IconSize*4 else 0.1*4
      ) ,
      
      list(
        
        source =  variation_image,
        xref="paper",
        yref="paper",
        x=if(exists("cardsettings_IconPosition") && cardsettings_IconPosition == "central") 0.1 else 0.9,
        y=if(exists("cardsettings_IconPosition") && cardsettings_IconPosition == "central") 0.4 else 0.45,
        xanchor="center",
        yanchor="top",
        sizex=if(exists("iconsettings_IconSize")) iconsettings_IconSize*4 else 0.1*4,
        sizey=if(exists("iconsettings_IconSize")) iconsettings_IconSize*4 else 0.1*4
        
      )
    ),
    title=card_title,
    
    margin=m2,
    
    annotations = list(
      
      x =  if(exists("cardsettings_IconPosition") && cardsettings_IconPosition == "central") 0.5 else 0.05,
      
      y = 0.7,
      
      text = card_text %>% 
        paste0(
          if(exists("cardsettings_CardPrefix")) cardsettings_CardPrefix else "",
          .,
          if(exists("cardsettings_CardSuffix")) cardsettings_CardSuffix else cardsettings_CardSuffix <- ""
          ),
      
      xref = "paper",
      
      yref = "paper",
      
      xanchor =  if(exists("cardsettings_IconPosition") && cardsettings_IconPosition == "central") 'center' else 'left',
      
      showarrow =FALSE,
      
      font = list(
        
        color = "grey",
        
        size = if(exists("cardsettings_ValueSize")) cardsettings_ValueSize else 48
        
      )
      
      
    )) %>% 
    config(displayModeBar = FALSE) 
  

  # Join as subplots
  
  fig <- plotly::subplot(fig_icons, fig_plot, 
                 nrows = 2, 
                 heights = c(0.6, 0.4)
                 )
     
}

# ####################################################

############# Create and save widget ###############
internalSaveWidget(if(outputtypesettings_OutputType != "summarytable" & outputtypesettings_OutputType != "summarymatrix" & outputtypesettings_OutputType != "summarytable2") fig %>% plotly::partial_bundle(local=FALSE) else fig, 'out.html');
####################################################

################ Reduce paddings ###################
ReadFullFileReplaceString('out.html', 'out.html', ',"padding":[0-9]*,', ',"padding":0,')
####################################################
