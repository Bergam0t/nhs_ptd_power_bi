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

############### Library Declarations ###############
libraryRequireInstall("plotly")
libraryRequireInstall("dplyr")
libraryRequireInstall("DT")
####################################################

################### Actual code ####################
# 'Values' is the input received from PowerBI
# Note that it seems a bit funny about the format dates are passed through in
# TODO: Make date parsing more flexible - at the moment it will *only* work with yyyy-mm-dd 
# (or possibly a very comprehensive mm-dd-yyyy, and that might depend on locale settings...) 

# If testing, a sample dataset can be loaded from here (changing the path below the 
# repository if necessary)
# Double rebase
# Values <- read.csv("H:\\nhs_ptd_power_bi\\sample_datasets\\spc_xmr_sample_dataset_double_rebase_increase_good_trending_higher_inconsistent.csv") %>%
#           mutate(date = lubridate::dmy(date))
# More than one KPI
# Values <- read.csv("H:\\nhs_ptd_power_bi\\sample_datasets\\spc_xmr_sample_dataset_multiple_areas_double_rebase_increase.csv") %>%
#           mutate(date = lubridate::dmy(date))
# Lots of KPIs
# Values <- read.csv("H:\\nhs_ptd_power_bi\\sample_datasets\\spc_xmr_sample_dataset_10_areas.csv") %>%
#           mutate(date = lubridate::dmy(date))

# Import the mandatory columns
if(exists("value")) value <- value else value <- NULL
if(exists("date")) date <- date else date <- NULL

# Import the optional columns
if(exists("what")) what <- what else what <- NULL
if(exists("improvement_direction")) improvement_direction <- improvement_direction else improvement_direction <- NULL
if(exists("target")) target <- target else target <- NULL
if(exists("annotations")) annotations <- annotations else annotations <- NULL
if(exists("recalc_here")) recalc_here <- recalc_here else recalc_here <- NULL
if(exists("baseline_duration")) baseline_duration <- baseline_duration else baseline_duration <- NULL

Values <- cbind(value, date)

if(!is.null(what)) Values <- bind_cols(Values, what) else Values <- Values %>% mutate(what = NA)
if(!is.null(improvement_direction)) Values <- bind_cols(Values, improvement_direction) else Values <- Values %>% mutate(improvement_direction = NA)
if(!is.null(target)) Values <- bind_cols(Values, target) else Values <- Values %>% mutate(target = NA)
if(!is.null(annotations)) Values <- bind_cols(Values, annotations) else Values <- Values %>% mutate(annotations = NA)
if(!is.null(recalc_here)) Values <- bind_cols(Values, recalc_here) else Values <- Values %>% mutate(recalc_here = NA)
if(!is.null(baseline_duration)) Values <- bind_cols(Values, baseline_duration) else Values <- Values %>% mutate(baseline_duration = NA)

colnames(Values) <- c("value", "date", "what", "improvement_direction", "target", "annotations", "recalc_here", "baseline_duration") 

dataset <- Values %>% 
    mutate(date = as.Date(date)) 

if(exists("outputtypesettings_OutputType")) outputtypesettings_OutputType <- outputtypesettings_OutputType else outputtypesettings_OutputType <- "graph"

if(exists("legendsettings_LegendPosition")) legendsettings_LegendPosition <- legendsettings_LegendPosition else legendsettings_LegendPosition <- "below"
if (legendsettings_LegendPosition == "off" | outputtypesettings_OutputType == "card") showLegend <- FALSE else showLegend <- TRUE


##########################################################
# Begin calculations for instances where we pass in 
# multiple datasets and don't want them aggregated
##########################################################

if (outputtypesettings_OutputType == "summarytable" | 
    outputtypesettings_OutputType == "summarymatrix" | 
    outputtypesettings_OutputType == "facet_graph") {
  
  ptd_objects <- list()
  ptd_objects_tibble <- list()
  
  for (what in 1:nrow(dataset %>% distinct(what))) { 
  
  what_item <- (dataset %>% distinct(what) %>% pull())[[what]]
    
  single_what <- dataset %>% 
    filter(what == what_item) 

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
  
  # Generate NHS R making data count object
  ptd_df <- ptd_spc(single_what, 
                        value_field = "value",
                        date_field="date", 
                        improvement_direction = improvement_direction,
                        fix_after_n_points = if(is.na(unique(single_what$baseline_duration))) NULL else unique(single_what$baseline_duration),
                        rebase = if((single_what %>% filter(stringr::str_detect(recalc_here,"y|Y|yes|Yes|YES")) %>% nrow()) < 1) NULL else (single_what %>% filter(stringr::str_detect(recalc_here,"y|Y|yes|Yes|YES")) %>% select(date) %>% distinct() %>% pull()) %>% as.Date() %>% ptd_rebase(),
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
    )) %>% 
    mutate(title = what_item)
  
  # Store this for use in the faceted graph
  ptd_objects_tibble[[what_item]] <- ptd_df

  # Now do some additional processing to make it more useful for the graph-type plots
  if (!is.na(ptd_df %>% distinct(target) %>% pull())) assurance_type <- ptd_calculate_assurance_type_2(ptd_df, improvement_direction) %>% select(assurance_type) %>% pull() else assurance_type <- ""
  
  final_row <- ptd_df %>% arrange(x) %>% tail(1)
    
  ptd_objects[[what]] <- tibble(
    What = what_item,
    `Most Recent Data Point` =  final_row %>% pull(x),
    `Most Recent Value` =  final_row %>% pull(y),
    Mean = final_row %>% pull(mean),
    `Lower Process Limit` = final_row %>% pull(lpl),
    `Upper Process Limit` = final_row %>% pull(upl),
    `Target` = final_row %>% pull(target),
    `Variation` = final_row %>% pull(point_type),
    `Assurance` = assurance_type 
    
    
  ) %>% 
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

  if (exists("spcsettings_ValueIsPercentage") && spcsettings_ValueIsPercentage == TRUE) tickhoverformat <- ',.0%' else tickhoverformat <- ""
  
  spc_plots <- list()
  
    for (j in 1:length(ptd_objects_tibble)) {
  
        ptd_object <- ptd_objects_tibble[[j]]
  
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
  
        if (is.na(target)) target <- NULL
        if(exists("spcsettings_Target")) spcsettings_Target <- spcsettings_Target else spcsettings_Target <- NULL
        if (is.null(target) & !is.null(spcsettings_Target)) target <- spcsettings_Target
  
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
  
    fig <- subplot(spc_plots, shareX=TRUE, shareY=TRUE, nrows=facetsettings_NumRows, margin=margin_facet)

}

if (outputtypesettings_OutputType == "summarytable") {
  
  fig <- ptd_summary_table %>% 
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

if (outputtypesettings_OutputType == "summarymatrix") {
  
  one_of_each_variation <- c("Special Cause - Concern", "Common Cause", "Special Cause - Improvement")
  one_of_each_assurance <- c("Consistently Failing to Meet Target", 
                              "No Target", 
                              "Inconsistent - Sometimes Meeting Target, Sometimes Failing to Meet Target",
                              "Consistently Meeting Target")
  
  summary_matrix <- ptd_summary_table %>% 
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
    rename(` ` = `Assurance`) 
  
  
  # Create any missing columns
  
  fig <- summary_matrix %>%
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
          rebase = if((dataset %>% filter(stringr::str_detect(recalc_here,"y|Y|yes|Yes|YES")) %>% nrow()) < 1) NULL else (dataset %>% filter(stringr::str_detect(recalc_here,"y|Y|yes|Yes|YES")) %>% select(date) %>% distinct() %>% pull()) %>% as.Date() %>% ptd_rebase(),
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
              showlegend=FALSE)
  
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
  if(exists("titlesettings_TitleOn")) titlesettings_TitleOn <- titlesettings_TitleOn else titlesettings_TitleOn <- TRUE
  
  
  
  
  

  if (titlesettings_TitleOn == TRUE) {

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


}

if (outputtypesettings_OutputType == "graph") {
  
  if (exists("spcsettings_ValueIsPercentage") && spcsettings_ValueIsPercentage == TRUE) tickhoverformat <- ',.0%' else tickhoverformat <- ""

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
  
  if (exists("spcsettings_ValueIsPercentage") && spcsettings_ValueIsPercentage == TRUE) tickhoverformat <- ',.0%' else tickhoverformat <- ""

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


  if (titlesettings_TitleOn == TRUE) {
  

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
  
  fig <- subplot(fig_icons, fig_plot, 
                 nrows = 2, 
                 heights = c(0.6, 0.4))
     
}

# ####################################################

############# Create and save widget ###############
internalSaveWidget(if(outputtypesettings_OutputType != "summarytable" & outputtypesettings_OutputType != "summarymatrix") fig %>% plotly::partial_bundle(local=FALSE) else fig, 'out.html');
####################################################

################ Reduce paddings ###################
ReadFullFileReplaceString('out.html', 'out.html', ',"padding":[0-9]*,', ',"padding":0,')
####################################################
