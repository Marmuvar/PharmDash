library(tidyverse)
library(shiny)          #For interaction
library(lubridate)      #For date manipulation
library(knitr)
library(markdown)
library(kableExtra)      

apps_w_pats_stats <- read_rds("apps_w_pats_stats_V_1_1.rds")
brands_w_pats_stats <- read_rds("brands_w_pats_stats_V_1_1.rds")

# Do date calculations as "years" since original calc is in days.
# This is more exact than dividing by 365.24

gen_tbl <- apps_w_pats_stats %>%
  mutate(gen_sponsor = str_to_title(sponsor.x),
         gen_sponsor = as.factor(gen_sponsor),
         admin_route = str_to_title(admin_route),
         admin_route = as.factor(admin_route),
         dosage_form = as.factor(dosage_form),
         brand_sponsor = str_to_title(brand_sponsor),
         brand_sponsor = as.factor(brand_sponsor),
         TTM_mkt = interval(app_date_brand, 
                            app_date_generic) / dyears(1),
         TTM_first = interval(app_date_generic, 
                              first_pat_expiry) / dyears(1),
         TTM_last = interval(app_date_generic, 
                             last_pat_expiry) / dyears(1)) %>%
  filter(is.na(gen_sponsor) == FALSE) 

all_gen_tbl <- gen_tbl %>%
  filter(between(year(app_date_brand), 1982, 2021))

gen_len <- length(gen_tbl$gen_sponsor)

#Determine the order products came to market.
#Multiple products can be "first-to-file", so use "min" for ties.  

ranked_gen_entry <- gen_tbl %>% 
  na.omit() %>% 
  filter(between(year(app_date_brand), 1982, 2021)) %>%
  arrange(app_nums_brand, admin_route, app_date_generic) %>%
  group_by(app_nums_brand, admin_route) %>%
  mutate(gen_rank = rank(app_date_generic, ties.method = "min"),
         ftf = as.integer(gen_rank == 1)) 

gen_tbl <- inner_join(gen_tbl, 
                      ranked_gen_entry, 
                      by = colnames(gen_tbl))

# create and store leveled variable for use later
dosage_form_fact <- levels(as.factor(gen_tbl$dosage_form))
applicant_fact <- levels(as.factor(gen_tbl$gen_sponsor))
route_fact <- levels(gen_tbl$admin_route)

#Update brand table. This is pre-filtered for duplicates.   
brand_tbl <- brands_w_pats_stats %>%
  mutate(brand_sponsor = str_to_title(sponsor),
         brand_sponsor = as.factor(brand_sponsor),
         admin_route = str_to_title(admin_route),
         admin_route = as.factor(admin_route),
         dosage_form = as.factor(dosage_form),
         TF_first = interval(app_date, 
                            first_pat_expiry) / dyears(1),
         TF_last = interval(app_date, 
                              last_pat_expiry) / dyears(1))

brand_len <- length(brand_tbl$brand_sponsor)

#Pre-define limits for constant scal;es in graphs:
gen_file_ranges <- filter(gen_tbl, ftf == 1) %>%
  group_by(year = year(app_date_generic), dosage_form) %>%
  summarize(ftf = mean(TTM_first), ftl = mean(TTM_last), ftm = mean(TTM_mkt)) %>%
  ungroup()

ftf_ranges <- c(min(gen_file_ranges$ftf), max(gen_file_ranges$ftf))
ftl_ranges <- c(min(gen_file_ranges$ftl), max(gen_file_ranges$ftl))
ftm_ranges <- c(min(gen_file_ranges$ftm), max(gen_file_ranges$ftm))

gen_map_ft_ranges <- c(min(gen_file_ranges[c("ftf","ftl", "ftm")]), 
                       max(gen_file_ranges[c("ftf","ftl", "ftm")])
                       )

# Define UI for application that shows multiple views of generic or branded
# pharmaceutical companies on different tabs.  

# Speed up select performance
# https://shiny.rstudio.com/articles/selectize.html

ui <- fluidPage(
  # Application title
  titlePanel(
    "Brand and Generic Pharmaceutical Companies and Products, 1982 - 2021"
  ),
  tabsetPanel(
    id = "iApp",
    selected = "Generics",
    tabPanel(
      "Generics",
      fluidRow(
        column(
          width = 2,
          selectizeInput(
            inputId = "iRoute",
            label = "Route",
            choices = NULL
          )
        ),
        column(
          width = 2,
          selectizeInput(
            inputId = "iSponsor",
            label = "Sponsor 1",
            choices = NULL
          )
        )
      ),
      
      #Show a plot of the generated distribution
      #https://shiny.rstudio.com/articles/tabsets.html.  Accessed 12/17/21
      #https://shiny.rstudio.com/articles/interactive-docs.html  Accessed 12/17/21
      fluidRow(column(
        plotOutput("industry_plot",
                   height = "600px",
                   click = "gen_ind_click"),
        width = 12
        )
      ),
      fluidRow(
        column(
          htmlOutput("gen_spon"),
          htmlOutput("year_prod_rows"),
          htmlOutput("x_value"),
          width = 3
        ),
        column(
          width = 9,
          fluidRow(
            column(
              width = 12,
              style = 'padding:40px;',
              fluidRow(
                column(
                  width = 6,
                  plotOutput("prod_ct_heat_plot",
                             click = "gen_year_click",
                  )
                ),
                column(
                  width = 6,
                  plotOutput("ttf_pat_heat_plot",
                             click = "gen_year_click",
                  )
                )
              ),
              fluidRow(
                column(
                  width = 6,
                  plotOutput("ttm_heat_plot",
                             click = "gen_year_click",
                  ),
                ),
                column(
                  width = 6,
                  plotOutput("ttl_pat_heat_plot",
                             click = "gen_year_click",
                )
              )
            )
          )  
        )
      )
    ),
    fluidRow(
      column(
        plotOutput("ftf_heat_plot",
                   height = "400px",
                   click = "gen_year_click"),
        width = 12
      )
    )
  ),
  tabPanel(
    "Brands",
    sidebarLayout(
      position = "left",
      sidebarPanel(
        width = 12,
        selectizeInput(
          inputId = "iBRoute",
          label = "Route",
          choices = NULL
        ),
        selectizeInput(
          inputId = "iBSponsor",
          label = "Brand Sponsor 1",
          choices = NULL
        )
      ),
      mainPanel(
        width = 12,
        fluidRow(
          plotOutput("brand_col_plot",
                     height = "600px",
                     click = "brand_ind_click"),
        ),
        fluidRow(
          column(br(),
               br(),
               htmlOutput("brand_x_value"),
               br(),
               width = 12)
        ),
        fluidRow(
          column(
            br(),
            htmlOutput("brand_prod"),
            htmlOutput("gen_of_brands"),
            br(),
            width = 3
          ),
          column(
            width = 9,
            fluidRow(
              column(
                plotOutput("brand_TFF_plot",
                           height = "400px",
                           click = "brand_year_click"),
                width = 6
              ),
              column(
                plotOutput("brand_TFL_plot",
                           height = "400px",
                           click = "brand_year_click"),
              width = 6
              )
            ),
            fluidRow(
              column(
                plotOutput("brand_gen_ct_plot",
                           height = "400px",
                           click = "brand_year_click"),
                width = 6
              ),
              column(
                plotOutput("brand_pats_vs_prods_plot", height = "400px"),
                width = 6
              )
            ),
            fluidRow(
              column(
              plotOutput("brand_comp_to_year",
                height = "600px"),
                width = 12
              )
            )
          )
        )
      )
    )
  ),
  tabPanel("Documentation", 
           includeMarkdown("documentation.md")
           )
  )
)
          
# Define server logic required to draw plots 
server <- function(input, output) {
  
  # fct_lump_limit is a wrapper for fct_lump_n
  # Resolves issue with inconsistent treatment of levels when n > levels present
  # X is a vector of characters
  # n is the desired number of factors
  # Returns a list with lesser of n or number of levels present in X
  fct_lump_limit <- function(X, n){
    ct = levels(factor(X)) %>%
      length()
    fct_lump_n(f = X,
               n = min(n, ct),
               ties.method = "min")
  }
  
  gen_react <- reactive({filter(
    gen_tbl, 
    admin_route == input$iRoute,
    !is.na(gen_sponsor)) 
  })
  
  #Adding .$gen_sponsor gave correct reactive filter.  Not sure why...
  #Creating separate context here for 
  # filtered values in graphs vs unfiltered values for pulldown select.
  
  gen_react_trim <- reactive({gen_react() %>%
      mutate(gen_sponsor = fct_lump_limit(.$gen_sponsor, 40)) %>%
      filter(!gen_sponsor %in% c("Other", "NA"))
  })
  
  #Specific filter context for company-specific graphs
  sponsor_react <- reactive({
    filter(gen_tbl,
           gen_sponsor == input$iSponsor,
           admin_route == input$iRoute) %>%
    group_by(year = year(app_date_generic), 
             dosage_form) %>%
    mutate(dosage_form = as.factor(dosage_form)
           )
    })

  #General filter for all brand products based on lookup route
  brand_react <- reactive({
    filter(brand_tbl, 
           admin_route == input$iBRoute, 
           !is.na(brand_sponsor))
    })
  
  #Adding .$brand_sponsor gave correct reactive filter.  Not sure why...
  #Creating separate context here for 
  # filtered values in graphs vs unfiltered values for pulldown select.
  
  brand_react_trim <- reactive({brand_react() %>%
      mutate(brand_sponsor = fct_lump_limit(.$brand_sponsor, 40)) %>%
      filter(!brand_sponsor %in% c("Other", "NA"))
  })

  #Specific filter context for company-specific graphs
  brand_sponsor_react <- reactive({
    filter(brand_tbl,
          brand_sponsor == input$iBSponsor,
          admin_route == input$iBRoute) %>%
    group_by(year = year(app_date),
           dosage_form) %>%
    mutate(dosage_form = as.factor(dosage_form))
    })
  
  gb_sponsor_react <- reactive({filter(all_gen_tbl, 
                                       brand_sponsor == input$iBSponsor, 
                                       admin_route == input$iBRoute,
                                       is.na(gen_sponsor) == FALSE
                                       )
                              })
  #Control selections available for each graph
  #Company list is dynamically filtered for dosage route selection. 
  updateSelectizeInput(session = getDefaultReactiveDomain(), 
                       inputId = "iRoute", 
                       choices = gen_tbl$admin_route,
                       selected = "Oral",
                       server = TRUE)
  observe({
    gen_sponsor_by_dose <- gen_tbl %>%
      filter(admin_route == input$iRoute)
    
  updateSelectizeInput(session = getDefaultReactiveDomain(), 
                       inputId = "iSponsor", 
                       #choices = sort(gen_tbl$gen_sponsor),
                       choices = sort(gen_sponsor_by_dose$gen_sponsor),
                       selected = "Mylan",
                       server = TRUE,
                       options = list(maxOptions = gen_len))
  })
  updateSelectizeInput(session = getDefaultReactiveDomain(), 
                       inputId = "iBRoute", 
                       choices = brand_tbl$admin_route,
                       selected = "Oral",
                       server = TRUE)
  observe({
    sponsor_by_dose <- brand_tbl %>%
      filter(admin_route == input$iBRoute)
    
  updateSelectizeInput(session = getDefaultReactiveDomain(), 
                       inputId = "iBSponsor", 
                       #choices = sort(brand_tbl$brand_sponsor),
                       choices = sort(sponsor_by_dose$brand_sponsor),
                       selected = "Pfizer",
                       server = TRUE,
                       options = list(maxOptions = 5000))
  })
  
  base_theme <- theme(panel.background = element_rect(fill="lightsteelblue3"),
                      axis.text.x = element_text(size = 10, angle = 90),
                      axis.text.y = element_text(size = 10),
                      axis.title.x = element_text(size = 12),
                      axis.title.y = element_text(size = 12),
                      axis.ticks.x = element_blank(),
                      panel.grid.major.x = element_blank(),
                      plot.title = element_text(size = 16, hjust = 0),
                      plot.subtitle = element_text(size = 12, hjust = 0),
                      plot.background = element_rect(color = "black"),
                      legend.position = "bottom",
                      legend.justification = "center"
  )
  
  hm_theme <- base_theme + theme(legend.position="bottom",
                                 legend.key = element_rect(size = "100px" ),
                                 legend.text = element_text(angle = 90),
                                 axis.text.x = element_text(size = 8, angle = 0)
                                  )

  plt_heatmap <- function(df, xCol, yCol, fill_Val,
                          main, subtitle, xLab, yLab, barLab,  n_br = 4, 
                          fill_range = NULL){ 
    ggplot(df, aes(x = !!sym(xCol), y = !!sym(yCol), fill = !!sym(fill_Val))) +
      geom_tile(show.legend = TRUE, color = "white", width = 1) +
      hm_theme +
      labs(title = main,
           subtitle = subtitle,
           x = xLab,
           y = yLab) +
      scale_x_continuous(limits = c(1982, 2022)) + 
      scale_fill_continuous(type = "viridis", direction = 1,
                            begin = 0, 
                            option = "E",
                            limits = fill_range) +
      guides(fill = guide_colorbar(title = barLab))
  }
  
  #Alternate format where x, y, and fill values are literal / not symbolic
  #Needed for generic first-to-file plot
  plt_heatmap_spec <- function(df, xCol, yCol, fill_Val,
                          main, subtitle, xLab, yLab, barLab,  n_br = 4, 
                          fill_range = NULL){
    dosage = sponsor_react() %>%
      summarize(ct = n()) %>%
      ungroup() %>%
      select(dosage_form) %>%
      distinct() %>%
      mutate(dosage_form = as.factor(as.character(dosage_form)))
    dosage_types = levels(dosage$dosage_form)
      
      ggplot(df, aes(x = xCol, y = yCol, fill = fill_Val)) +
      geom_tile(show.legend = TRUE, color = "white", width = 1) +
      hm_theme +
      labs(title = main,
           subtitle = subtitle,
           x = xLab,
           y = yLab) +
      scale_x_continuous(limits = c(1982, 2022)) + 
      scale_y_discrete(limits = dosage_types ) +
      
      scale_fill_continuous(type = "viridis", direction = 1, begin = 0, 
                            option = "E",
                            limits = fill_range
      ) +
      guides(fill = guide_colorbar(title = barLab))
  }
  
  plt_box <- function(df, xCol, yCol,
                          main, subtitle, xLab, yLab){ 
    ggplot(df, aes(x = !!sym(xCol), y = !!sym(yCol))) +
      geom_boxplot(show.legend = FALSE, fill = "white") +
      base_theme +
      labs(title = main, 
           subtitle = subtitle,
           x = xLab,
           y = yLab)
  }
  
  plt_col <- function(tb, xCol, yCol, main, subtitle = "", xLab, yLab,
                      fCol = "deepskyblue3"){

    xCol <- switch(typeof(xCol),
            character = sym(xCol),
            double = xCol)
    yCol <- sym(yCol)
    
    ggplot(tb, aes(x = !!xCol, y = !!yCol)) +
      geom_col(fill = fCol) +
      labs(title = main, subtitle = subtitle, x = xLab, y = yLab) +
      base_theme
  }
  
  plt_col_multi <- function(tb, xCol, yCol, catCol, 
                            main, subtitle = "", xLab, yLab,
                            fCol = "deepskyblue3"){
    xCol = switch(typeof(xCol),
                  character = sym(xCol),
                  double = xCol)
    yCol = sym(yCol)
    catCol = sym(catCol)

    ggplot(tb, aes(x = !!xCol, y = !!yCol)) +
      geom_col(aes(fill = !!catCol), position  = "dodge") +
      scale_fill_discrete(type = c("deepskyblue3", "gold")) +
      labs(title = main, subtitle = subtitle, x = xLab, y = yLab) +
      base_theme
  }
  
#Interactive code  for Generics based on reference to API Manual:
#https://shiny.rstudio.com/articles/plot-interaction-advanced.html
  
# Structure notes:
# Observe() sections execute when a contained value changes, similar to reactive
# Allows a reactive context for contained functions.]
# Compare to reactive(), which provides a dynamic value as function output.

  output$x_value <- renderText({
    if (is.null(input$gen_ind_click$x)) return("")
    else {
      lvls <- levels(gen_react_trim()$gen_sponsor)
      name <- lvls[round(input$gen_ind_click$x)]
      HTML("You've selected <code>", name, "</code>",
      )
    }
  })
  
  observe({
    if (is.null(input$gen_ind_click$x)) return("Select a company from dropdown or click on top 40 graph to display details.")
    else {
      new_choices <- gen_react_trim() %>%
        select(gen_sponsor)
      lvls <- levels(new_choices$gen_sponsor)
      name <- lvls[round(input$gen_ind_click$x)]
      updateSelectizeInput(session = getDefaultReactiveDomain(),
                           inputId = "iSponsor",
                           selected = name,
                           server = FALSE)
    }
  })
  
  output$gen_spon <- renderText({
    if (is.null(input$gen_year_click$x)) return("No year selected")
    else {
      dosage <- sponsor_react() %>%
        summarize(ct = n()) %>%
        ungroup() %>%
        select(dosage_form) %>%
        distinct() %>%
        mutate(dosage_form = as.factor(as.character(dosage_form))) 
      dosage <- levels(dosage$dosage_form)
      year <- round(input$gen_year_click$x)
      HTML("You've selected <code>", year, "</code>",
      )
    }
  })
  
  output$year_prod_rows <- renderPrint({
    if (is.null(input$gen_year_click$x)) return()
    else {
      dosage <- sponsor_react() %>%
        summarize(ct = n()) %>%
        ungroup() %>%
        select(dosage_form) %>%
        distinct() %>%
        mutate(dosage_form = as.factor(as.character(dosage_form))) 
      dosage_levels <- levels(dosage$dosage_form)
      dosage_filter <- dosage_levels[round(input$gen_year_click$y)]

      keeprows <- (round(input$gen_year_click$x) == year(sponsor_react()$app_date_generic) &
        (dosage_filter == sponsor_react()$dosage_form))
      keeprows <- sponsor_react()[keeprows, ] %>%
        select(app_date_brand, ds_name, admin_route, dosage_form, brand_sponsor)
      kable(keeprows, 
            format = "html",
            align = "c",
            caption = paste("Details of reference products for generics",
                            "introduced this year."),
            col.names = c("Year", 
                          "Brand Approval Date", 
                          "Drug Substance", 
                          "Admin. Route", 
                          "Dosage Form,",
                          "Sponsor" )) %>%
        kable_styling(font_size = 8)
    }
  })
  
#Block for interacting with brand tab graphs
  brand_start_msg <- "Select a company from dropdown or click on top 40 graph to display details."
  
  output$brand_x_value <- renderText({
    if (is.null(input$brand_ind_click$x)) return(brand_start_msg)
    else {
      lvls <- levels(brand_react_trim()$brand_sponsor)
      name <- lvls[round(input$brand_ind_click$x)]
      HTML("You've selected <code>", name, "</code>"
      )
    }
  })
  
  brand_lower_msg <- "Click on lower graphs to display product details for the year"
  
  output$brand_prod <- renderText({
    if (is.null(input$brand_year_click$x)) return(brand_lower_msg)
    else {
      year <- round(input$brand_year_click$x)
      HTML("You have selected <code>", year, "</code>",
      )
    }
  })
  
  observe({
    if (is.null(input$brand_ind_click$x)) return(brand_lower_msg)
    else {
      new_choices <- brand_react_trim() %>%
        select(brand_sponsor)
      lvls <- levels(new_choices$brand_sponsor)
      name <- lvls[round(input$brand_ind_click$x)]
      updateSelectizeInput(session = getDefaultReactiveDomain(),
                           inputId = "iBSponsor",
                           selected = name,
                           server = FALSE)
    }
  })
  
  output$gen_of_brands <- renderPrint({
    if (is.null(input$brand_year_click$x)) return(brand_lower_msg)
    else {
      x_year <- round(input$brand_year_click$x)
      keeprows <- filter(all_gen_tbl,
                         brand_sponsor == input$iBSponsor,
                         admin_route == input$iBRoute,
                         year(app_date_brand) == x_year
      ) %>%
        select(ds_name, admin_route, 
               dosage_form, gen_sponsor, app_date_generic) %>%
        arrange(ds_name, dosage_form, app_date_generic)
      kable(keeprows, 
            format = "html",
            align = "c",
            caption = paste("Details of Future Generic Products for",
                            "Brand Products Introduced this Year.")
            ) %>%
        kable_styling(font_size = 8)
    }
  })
  
 #Block for Generic Plots

  output$industry_plot <- renderPlot({
  gen_react_trim() %>%
      plt_box(xCol = "gen_sponsor",
          yCol = "TTM_mkt",
          main = "Time between Generic and \nBranded Drug Approval", 
          subtitle =  "Top 40 companies by Selected Route",
          xLab = "Company",
          yLab = "Time after Branded Approval")
  })

  output$prod_ct_heat_plot <- renderPlot({
    sponsor_react() %>% 
      summarize(val = n()) %>%
      plt_heatmap(xCol = "year", 
                  yCol = "dosage_form", 
                  fill_Val = "val", 
                  main = "Products per Year \nby Dosage Form", 
                  subtitle = paste(input$iSponsor),
                  xLab = "Year",
                  yLab = "Dosage Form",
                  barLab = "Count",
                  fill_range = gen_map_ft_ranges)
  })
  
  #Use yCol = sponsor_react()$dosage form
  #Ensures correct alignment of click$y and selection of correct dosage units
  #In output table
  output$ftf_heat_plot <- renderPlot({
    sponsor_react() %>%
      filter(ftf == 1) %>%
      summarize(val = sum(ftf)) %>%
      plt_heatmap_spec(xCol = .$year, 
                  yCol = .$dosage_form, 
                  fill_Val = .$val, 
                  main = "First to File", 
                  subtitle = paste(input$iSponsor),
                  xLab = "Year",
                  yLab = "Dosage Form",
                  barLab = "Count",
                  fill_range = gen_map_ft_ranges)
  })
  
  output$ttf_pat_heat_plot <- renderPlot({
    sponsor_react() %>%
      summarize(val = mean(TTM_first)) %>%
      plt_heatmap(xCol = "year", 
                  yCol = "dosage_form", 
                  fill_Val = "val", 
                  main = "Mean Generic Approval Time \nafter First Brand Patent Expiry", 
                  subtitle = paste(input$iSponsor),
                  xLab = "Year",
                  yLab = "Dosage Form",
                  barLab = "mean Years",
                  fill_range = gen_map_ft_ranges)
  })
  
  output$ttl_pat_heat_plot <- renderPlot({
    sponsor_react() %>%
      summarize(val = mean(TTM_last)) %>%
      plt_heatmap(xCol = "year", 
                  yCol = "dosage_form", 
                  fill_Val = "val", 
                  main = "Mean Generic Approval Time \nafter Last Brand Patent Expiry", 
                  subtitle = paste(input$iSponsor),
                  xLab = "Year",
                  yLab = "Dosage Form",
                  barLab = "mean Years",
                  fill_range = gen_map_ft_ranges)
  })
  
  output$ttm_heat_plot <- renderPlot({
    sponsor_react() %>%
      summarize(val = mean(TTM_mkt)) %>%
      plt_heatmap(xCol = "year", 
                  yCol = "dosage_form", 
                  fill_Val = "val", 
                  main = "Mean Generic Approval Time \nafter Brand Approval", 
                  subtitle = paste(input$iSponsor),
                  xLab = "Year",
                  yLab = "Dosage Form",
                  barLab = "mean Years")
  })
  
  
#Block for Brand Plots
  
  output$brand_col_plot <- renderPlot({
    brand_react_trim() %>%
      group_by(brand_sponsor, admin_route, app_nums_brand) %>%
      summarize(val = n()) %>%
      plt_col(xCol = "brand_sponsor",
              yCol = "val",
              main = "Brand Products, Top 40 Companies", 
              subtitle = paste("Admin. Route = ", input$iBRoute),
              xLab = "Company",
              yLab = "Number of Products")
  })
  
  output$brand_TFF_plot <- renderPlot({
    brand_sponsor_react() %>%
      group_by(year = year(app_date)) %>%
      summarize(val = mean(TF_first)) %>%
      plt_col(xCol = "year",
              yCol = "val",
              main = "Approval vs. First Patent Expiry", 
              subtitle = paste("Admin. Route = ", input$iBRoute),
              xLab = "Year",
              yLab = "Mean Years before First Expiry")
  })
  
  output$brand_TFL_plot <- renderPlot({
    brand_sponsor_react() %>%
      group_by(year = year(app_date)) %>%
      summarize(val = mean(TF_last)) %>%
      plt_col(xCol = "year",
              yCol = "val",
              main = "Approval vs. Last Patent Expiry", 
              subtitle = paste("Admin. Route = ", input$iBRoute),
              xLab = "Year",
              yLab = "Mean Years before Last Expiry")
  })

  output$brand_gen_ct_plot <- renderPlot({
    brand_sponsor_react() %>%
      group_by(year = year(app_date)) %>%
      summarize("Brand Ct" = n(), "Gen Ct" = sum(gen_ct)) %>%
      pivot_longer(cols = c("Brand Ct", "Gen Ct")) %>%
      plt_col_multi(xCol = "year",
              yCol = "value",
              catCol = "name",
              main = "Brand Product vs. \nFuture Generic Product Applications", 
              subtitle = paste("Admin. Route = ", input$iBRoute),
              xLab = "Year",
              yLab = "Product Count")
  })
  
  output$brand_gen_spon_plot <- renderPlot({
    gb_sponsor_react() %>%
      group_by(year = year(app_date_brand), sponsor.y) %>%
      summarize(gen_ct = length(unique(gen_sponsor)),
                brand_ct = length(unique(app_nums_brand))) %>%
      pivot_longer(cols = c("brand_ct", "gen_ct")) %>%
      plt_col_multi(xCol = "year",
                    yCol = "value",
                    catCol = "name",
                    main = "Brand Applications vs. \nUnique Generic Competitors", 
                    subtitle = paste("Admin. Route = ", input$iBRoute),
                    xLab = "Year",
                    yLab = "Product Count")
  })
  
  output$brand_pats_vs_prods_plot <- renderPlot({
   brand_sponsor_react() %>%
      group_by(brand_sponsor, admin_route) %>%
      summarize(w_pat_ct = sum(pat_ct > 0), unpat_ct = sum(pat_ct == 0)) %>%
      pivot_longer(cols = c("w_pat_ct", "unpat_ct")) %>%
      plt_col_multi(xCol = "brand_sponsor",
                    yCol = "value",
                    catCol = "name",
                    main = "Patented vs. Unpatented Products", 
                    subtitle = paste("Admin. Route = ", input$iBRoute),
                    xLab = "Company",
                    yLab = "Sponsor Count") + 
      theme(axis.text.x = element_text(angle = 0))
  })
  
  output$brand_comp_to_year <- renderPlot({
    gb_sponsor_react() %>%
      mutate(app_date_brand = year(app_date_brand),
             app_date_generic = year(app_date_generic)) %>%
      na.omit() %>%
      group_by(app_date_brand, app_date_generic) %>%
      summarize(g_ct = n()) %>%
      plt_heatmap(xCol = "app_date_brand", 
                  yCol = "app_date_generic", 
                  fill_Val = "g_ct", 
                  main = paste("Total Brand Products and Years of ",
                  "Subsequent Generic Product Introduction"), 
                  subtitle = paste(input$iBSponsor),
                  xLab = "Year of Brand Introduction",
                  yLab = "Year of Sponsor Introduction",
                  barLab = "Count",
                  n_br = min(max(.$g_ct), 6, na.rm = TRUE)) +
      geom_abline(slope = 1, intercept = 0) +
      scale_y_continuous(limits = c(1982, 2022)) +
      theme(legend.position = "right")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)