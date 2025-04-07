# Install required packages if needed
if(!require(shiny)) install.packages("shiny")
if(!require(shinydashboard)) install.packages("shinydashboard")
if(!require(dplyr)) install.packages("dplyr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(tidyr)) install.packages("tidyr")
if(!require(lubridate)) install.packages("lubridate")
if(!require(readxl)) install.packages("readxl")
if(!require(scales)) install.packages("scales")
if(!require(DT)) install.packages("DT")
if(!require(plotly)) install.packages("plotly")
if(!require(leaflet)) install.packages("leaflet")

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(readxl)
library(scales)
library(DT)
library(plotly)
library(leaflet)


setwd("/Users/ebze/Projects/Job Analysis")
job_data <- read_excel("Job opportunities.xlsx")
# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "UK IT Job Market Analysis (2019-2023)"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Job Distribution", tabName = "job_distribution", icon = icon("briefcase")),
      menuItem("Skills Analysis", tabName = "skills", icon = icon("code")),
      menuItem("Salary Analysis", tabName = "salary", icon = icon("pound-sign")),
      menuItem("Temporal Analysis", tabName = "temporal", icon = icon("chart-line")),
      menuItem("Data Explorer", tabName = "data", icon = icon("table"))
    ),
    
    # Filters
    hr(),
    selectInput("year_filter", "Year:", 
                choices = c("All", "2019", "2020", "2021", "2022", "2023"),
                selected = "All"),
    
    selectInput("experience_filter", "Experience Level:", 
                choices = c("All", "Entry Level", "Mid Level", "Senior", "Executive"),
                selected = "All"),
    
    selectInput("location_filter", "Location:", 
                choices = c("All", "London", "Manchester", "Birmingham", "Edinburgh", "Glasgow", "Leeds"),
                selected = "All"),
    
    selectInput("job_type_filter", "Job Type:", 
                choices = c("All", "Full-time", "Part-time", "Contract", "Freelance"),
                selected = "All")
  ),
  
  dashboardBody(
    tabItems(
      # Dashboard tab
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("total_jobs_box", width = 3),
                valueBoxOutput("avg_salary_box", width = 3),
                valueBoxOutput("top_location_box", width = 3),
                valueBoxOutput("top_skill_box", width = 3)
              ),
              fluidRow(
                box(plotlyOutput("job_trend_plot"), title = "Job Posting Trends", status = "primary", solidHeader = TRUE, width = 12)
              ),
              fluidRow(
                box(plotlyOutput("job_title_plot"), title = "Top 10 IT Job Titles", status = "primary", solidHeader = TRUE, width = 12),
                box(leafletOutput("job_map"), title = "Job Distribution by Location", status = "primary", solidHeader = TRUE, width = 12)),
              
      ),
      
      # Job Distribution tab
      tabItem(tabName = "job_distribution",
              fluidRow(
                box(plotlyOutput("title_count_plot"), title = "Top 10 IT Job Titles", status = "info", solidHeader = TRUE, width = 6),
                box(plotlyOutput("experience_plot"), title = "Jobs by Experience Level", status = "info", solidHeader = TRUE, width = 6)
              ),
              fluidRow(
                box(plotlyOutput("job_type_plot"), title = "Distribution by Job Type", status = "info", solidHeader = TRUE, width = 6),
                box(plotlyOutput("industry_plot"), title = "Jobs by Industry", status = "info", solidHeader = TRUE, width = 6)
              ),
              fluidRow(
                box(plotlyOutput("company_plot"), title = "Top Hiring Companies", status = "info", solidHeader = TRUE, width = 12)
              )
      ),
      
      # Skills Analysis tab
      tabItem(tabName = "skills",
              fluidRow(
                box(plotlyOutput("skills_plot"), title = "Top 15 In-Demand IT Skills", status = "success", solidHeader = TRUE, width = 12)
              ),
              fluidRow(
                box(plotlyOutput("skills_title_plot"), title = "Skills by Job Title", status = "success", solidHeader = TRUE, width = 12)
              )
      ),
      
      # Salary Analysis tab
      tabItem(tabName = "salary",
              fluidRow(
                box(plotlyOutput("salary_location_plot"), title = "Salary Distribution by Location", status = "warning", solidHeader = TRUE, width = 6),
                box(plotlyOutput("salary_experience_plot"), title = "Salary by Experience Level", status = "warning", solidHeader = TRUE, width = 6)
              ),
              fluidRow(
                box(plotlyOutput("salary_title_plot"), title = "Average Salary by Job Title", status = "warning", solidHeader = TRUE, width = 6),
                box(plotlyOutput("salary_industry_plot"), title = "Salary by Industry", status = "warning", solidHeader = TRUE, width = 6)
              ),
              fluidRow(
                box(plotlyOutput("salary_trend_plot"), title = "Salary Trends Over Time", status = "warning", solidHeader = TRUE, width = 12)
              )
      ),
      
      # Temporal Analysis tab
      tabItem(tabName = "temporal",
              fluidRow(
                box(plotlyOutput("job_trends_plot"), title = "Job Posting Trends", status = "danger", solidHeader = TRUE, width = 12)
              ),
              fluidRow(
                box(plotlyOutput("seasonal_heatmap"), title = "Seasonal Job Posting Patterns", status = "danger", solidHeader = TRUE, width = 12)
              ),
              fluidRow(
                box(plotlyOutput("job_type_time_plot"), title = "Job Types Over Time", status = "danger", solidHeader = TRUE, width = 6),
                box(plotlyOutput("experience_time_plot"), title = "Experience Levels Over Time", status = "danger", solidHeader = TRUE, width = 6)
              )
      ),
      
      # Data Explorer tab
      tabItem(tabName = "data",
              fluidRow(
                box(DTOutput("job_data_table"), title = "Job Listings Data", status = "primary", solidHeader = TRUE, width = 12)
              )
      )
    )
  )
)
# Define server logic
server <- function(input, output, session) {
  # Load and preprocess data
  job_data_reactive <- reactive({
    # In a real application, load your data here
    # For this example, we'll assume the data is already loaded and preprocessed
    
    # Your original preprocessing logic
    job_data_clean <- job_data %>%
      # Extract min and max salary from salary range
      mutate(
        min_salary = as.numeric(gsub("£([0-9,]+).*", "\\1", `Salary Range`) %>% gsub(",", "", .)),
        max_salary = as.numeric(gsub(".*£([0-9,]+)", "\\1", `Salary Range`) %>% gsub(",", "", .)),
        avg_salary = (min_salary + max_salary) / 2,
        # Extract year and month from date posted
        year = year(`Date Posted`),
        month = month(`Date Posted`),
        month_name = month(month, label = TRUE),
        date = make_date(year, month, 1),
        # Clean up experience level if needed
        `Experience Level` = trimws(`Experience Level`)
      )
    
    # Apply filters
    filtered_data <- job_data_clean
    
    # Apply year filter
    if (input$year_filter != "All") {
      filtered_data <- filtered_data %>% 
        filter(year == as.numeric(input$year_filter))
    }
    
    # Apply experience level filter
    if (input$experience_filter != "All") {
      filtered_data <- filtered_data %>% 
        filter(`Experience Level` == input$experience_filter)
    }
    
    # Apply location filter
    if (input$location_filter != "All") {
      filtered_data <- filtered_data %>% 
        filter(Location == input$location_filter)
    }
    
    # Apply job type filter
    if (input$job_type_filter != "All") {
      filtered_data <- filtered_data %>% 
        filter(`Job Type` == input$job_type_filter)
    }
    
    return(filtered_data)
  })
  
  # Update filter choices based on data
  observe({
    location_choices <- c("All", sort(unique(job_data$Location)))
    updateSelectInput(session, "location_filter", choices = location_choices)
  })
  
  observe({
    experience_levels <- c("All", sort(unique(job_data$`Experience Level`)))
    updateSelectInput(session, "experience_filter", choices = experience_levels)
  })
  
  observe({
    job_types <- c("All", sort(unique(job_data$`Job Type`)))
    updateSelectInput(session, "job_type_filter", choices = job_types)
  })
  
  observe({
    years <- c("All", sort(as.character(unique(year(job_data$`Date Posted`)))))
    updateSelectInput(session, "year_filter", choices = years)
  })
  
  # Value Boxes
  output$total_jobs_box <- renderValueBox({
    req(job_data_reactive())
    validate(need(nrow(job_data_reactive()) > 0, "No data available"))
    
    total_jobs <- nrow(job_data_reactive())
    valueBox(
      total_jobs,
      "Total Job Listings",
      icon = icon("briefcase"),
      color = "blue"
    )
  })
  
  output$avg_salary_box <- renderValueBox({
    req(job_data_reactive())
    validate(need(nrow(job_data_reactive()) > 0, "No data available"))
    
    avg_salary <- mean(job_data_reactive()$avg_salary, na.rm = TRUE) %>% round(0)
    valueBox(
      paste0("£", format(avg_salary, big.mark = ",")),
      "Average Salary",
      icon = icon("pound-sign"),
      color = "green"
    )
  })
  
  output$top_location_box <- renderValueBox({
    req(job_data_reactive())
    validate(need(nrow(job_data_reactive()) > 0, "No data available"))
    
    top_location <- job_data_reactive() %>%
      count(Location) %>%
      arrange(desc(n)) %>%
      slice(1) %>%
      pull(Location)
    
    valueBox(
      top_location,
      "Top Location",
      icon = icon("map-marker"),
      color = "orange"
    )
  })
  
  output$top_skill_box <- renderValueBox({
    req(job_data_reactive())
    validate(need(nrow(job_data_reactive()) > 0, "No data available"))
    
    top_skill <- job_data_reactive() %>%
      select(`Required Skills`) %>%
      separate_rows(`Required Skills`, sep = ",") %>%
      mutate(`Required Skills` = trimws(`Required Skills`)) %>%
      count(`Required Skills`) %>%
      arrange(desc(n)) %>%
      slice(1) %>%
      pull(`Required Skills`)
    
    valueBox(
      top_skill,
      "Top Skill",
      icon = icon("code"),
      color = "purple"
    )
  })
  
  # Dashboard tab plots
  output$job_trend_plot <- renderPlotly({
    req(job_data_reactive())
    validate(need(nrow(job_data_reactive()) > 0, "No data available for the selected filters."))
    
    trend_data <- job_data_reactive() %>%
      count(date) %>%
      arrange(date)
    
    p <- ggplot(trend_data, aes(x = date, y = n)) +
      geom_line(color = "steelblue", size = 1) +
      geom_point(color = "steelblue", size = 2) +
      labs(x = "Date", y = "Number of Job Postings") +
      theme_minimal()
    
    ggplotly(p) %>% layout(showlegend = FALSE)
  })
  
  output$job_title_plot <- renderPlotly({
    req(job_data_reactive())
    validate(need(nrow(job_data_reactive()) > 0, "No data available for the selected filters."))
    
    title_data <- job_data_reactive() %>%
      count(`Job Title`) %>%
      arrange(desc(n)) %>%
      top_n(10)
    
    p <- ggplot(title_data, aes(x = reorder(`Job Title`, n), y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(x = "Job Title", y = "Number of Postings") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # UK Job Map visualization
  # Replace your current job_map function with this:
  output$job_map <- renderLeaflet({
    req(job_data_reactive())
    validate(need(nrow(job_data_reactive()) > 0, "No data available for the selected filters."))
    
    # UK cities coordinates (same as before)
    uk_cities <- data.frame(
      Location = c("London", "Manchester", "Birmingham", "Edinburgh", "Glasgow", "Leeds", 
                   "Liverpool", "Bristol", "Cardiff", "Belfast", "Newcastle", "Sheffield",
                   "Cambridge", "Oxford", "Reading", "Aberdeen", "Brighton", "Nottingham",
                   "Southampton", "Portsmouth", "Coventry", "York", "Leicester", "Swansea",
                   "Sunderland", "Dundee", "Plymouth", "Stoke-on-Trent", "Derby", "Exeter"),
      lat = c(51.5074, 53.4808, 52.4862, 55.9533, 55.8642, 53.8008, 
              53.4084, 51.4545, 51.4816, 54.5973, 54.9783, 53.3811,
              52.2053, 51.7520, 51.4543, 57.1497, 50.8225, 52.9548,
              50.9097, 50.8198, 52.4068, 53.9600, 52.6369, 51.6214,
              54.9060, 56.4620, 50.3755, 53.0027, 52.9225, 50.7236),
      lng = c(-0.1278, -2.2426, -1.8904, -3.1883, -4.2518, -1.5491, 
              -2.9916, -2.5879, -3.1791, -5.9301, -1.6178, -1.4701,
              0.1218, -1.2577, -0.9781, -2.0943, -0.1372, -1.1581,
              -1.4044, -1.0880, -1.5197, -1.0873, -1.1398, -3.9436,
              -1.3817, -2.9707, -4.1427, -2.1794, -1.4746, -3.5275)
    )
    
    # Count jobs by location
    location_counts <- job_data_reactive() %>%
      count(Location) %>%
      arrange(desc(n))
    
    # Join with coordinates
    map_data <- merge(location_counts, uk_cities, by = "Location", all.x = FALSE)
    
    validate(
      need(nrow(map_data) > 0, "No location data available for mapping.")
    )
    
    # Calculate circle size based on count
    map_data$radius <- sqrt(map_data$n) * 3
    map_data$popup_text <- paste0(
      "<strong>", map_data$Location, "</strong><br>",
      "Job Listings: ", map_data$n, "<br>",
      "Percentage: ", round(map_data$n/sum(location_counts$n)*100, 1), "%"
    )
    
    # Create the leaflet map
    leaflet(map_data) %>%
      addTiles() %>%
      addCircleMarkers(
        ~lng, ~lat,
        radius = ~radius,
        color = "#1E88E5",
        fillColor = "#1E88E5",
        fillOpacity = 0.7,
        weight = 1,
        popup = ~popup_text,
        label = ~Location
      ) %>%
      setView(lng = -2.5, lat = 54.0, zoom = 6)
  })
  # Keep the existing location_plot for comparison or as backup
  output$location_plot <- renderPlotly({
    req(job_data_reactive())
    validate(need(nrow(job_data_reactive()) > 0, "No data available for the selected filters."))
    
    location_data <- job_data_reactive() %>%
      count(Location) %>%
      arrange(desc(n)) %>%
      top_n(10)
    
    p <- ggplot(location_data, aes(x = reorder(Location, n), y = n)) +
      geom_bar(stat = "identity", fill = "darkorange") +
      coord_flip() +
      labs(x = "Location", y = "Number of Job Postings") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Job Distribution tab plots
  output$title_count_plot <- renderPlotly({
    req(job_data_reactive())
    validate(need(nrow(job_data_reactive()) > 0, "No data available for the selected filters."))
    
    title_data <- job_data_reactive() %>%
      count(`Job Title`) %>%
      arrange(desc(n)) %>%
      top_n(10)
    
    p <- ggplot(title_data, aes(x = reorder(`Job Title`, n), y = n)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(x = "Job Title", y = "Number of Postings") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$experience_plot <- renderPlotly({
    req(job_data_reactive())
    validate(need(nrow(job_data_reactive()) > 0, "No data available for the selected filters."))
    
    exp_data <- job_data_reactive() %>%
      count(`Experience Level`) %>%
      arrange(desc(n))
    
    p <- ggplot(exp_data, aes(x = reorder(`Experience Level`, n), y = n)) +
      geom_bar(stat = "identity", fill = "darkblue") +
      labs(x = "Experience Level", y = "Number of Job Postings") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$job_type_plot <- renderPlotly({
    req(job_data_reactive())
    validate(need(nrow(job_data_reactive()) > 0, "No data available for the selected filters."))
    
    type_data <- job_data_reactive() %>%
      count(`Job Type`) %>%
      arrange(desc(n))
    
    # Simple bar chart instead of pie
    p <- ggplot(type_data, aes(x = reorder(`Job Type`, n), y = n)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      coord_flip() +
      labs(x = "Job Type", y = "Count") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$industry_plot <- renderPlotly({
    req(job_data_reactive())
    validate(need(nrow(job_data_reactive()) > 0, "No data available for the selected filters."))
    
    industry_data <- job_data_reactive() %>%
      count(Industry) %>%
      arrange(desc(n)) %>%
      top_n(10)
    
    p <- ggplot(industry_data, aes(x = reorder(Industry, n), y = n)) +
      geom_bar(stat = "identity", fill = "purple") +
      coord_flip() +
      labs(x = "Industry", y = "Number of Job Postings") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$company_plot <- renderPlotly({
    req(job_data_reactive())
    validate(need(nrow(job_data_reactive()) > 0, "No data available for the selected filters."))
    
    company_data <- job_data_reactive() %>%
      count(Company) %>%
      arrange(desc(n)) %>%
      top_n(10)
    
    p <- ggplot(company_data, aes(x = reorder(Company, n), y = n)) +
      geom_bar(stat = "identity", fill = "darkgreen") +
      coord_flip() +
      labs(x = "Company", y = "Number of Job Postings") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Skills Analysis tab plots
  output$skills_plot <- renderPlotly({
    req(job_data_reactive())
    validate(need(nrow(job_data_reactive()) > 0, "No data available for the selected filters."))
    
    skill_data <- job_data_reactive() %>%
      select(`Required Skills`) %>%
      separate_rows(`Required Skills`, sep = ",") %>%
      mutate(`Required Skills` = trimws(`Required Skills`)) %>%
      count(`Required Skills`) %>%
      arrange(desc(n)) %>%
      top_n(15)
    
    p <- ggplot(skill_data, aes(x = reorder(`Required Skills`, n), y = n)) +
      geom_bar(stat = "identity", fill = "darkgreen") +
      coord_flip() +
      labs(x = "Skill", y = "Frequency") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$skills_title_plot <- renderPlotly({
    req(job_data_reactive())
    validate(need(nrow(job_data_reactive()) > 0, "No data available for the selected filters."))
    
    # For the heatmap of skills by job title
    skills_by_title <- job_data_reactive() %>%
      select(`Job Title`, `Required Skills`) %>%
      separate_rows(`Required Skills`, sep = ",") %>%
      mutate(`Required Skills` = trimws(`Required Skills`)) %>%
      count(`Job Title`, `Required Skills`) %>%
      group_by(`Job Title`) %>%
      top_n(5, n) %>%
      ungroup()
    
    # Check if we have enough job titles
    validate(
      need(n_distinct(skills_by_title$`Job Title`) > 0, "Not enough job titles available.")
    )
    
    # Get top job titles (up to 8)
    top_titles <- job_data_reactive() %>% 
      count(`Job Title`) %>% 
      arrange(desc(n)) %>% 
      head(8) %>% 
      pull(`Job Title`)
    
    # Filter to top job titles
    skills_by_title <- skills_by_title %>%
      filter(`Job Title` %in% top_titles)
    
    p <- ggplot(skills_by_title, aes(x = `Required Skills`, y = `Job Title`, fill = n)) +
      geom_tile() +
      scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
      labs(x = "Required Skill", y = "Job Title", fill = "Frequency") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    ggplotly(p)
  })
  
  # Salary Analysis tab plots
  output$salary_location_plot <- renderPlotly({
    req(job_data_reactive())
    validate(need(nrow(job_data_reactive()) > 0, "No data available for the selected filters."))
    
    p <- ggplot(job_data_reactive(), aes(x = reorder(Location, avg_salary), y = avg_salary)) +
      geom_boxplot(fill = "lightblue") +
      coord_flip() +
      labs(x = "Location", y = "Average Salary (£)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$salary_experience_plot <- renderPlotly({
    req(job_data_reactive())
    validate(need(nrow(job_data_reactive()) > 0, "No data available for the selected filters."))
    
    p <- ggplot(job_data_reactive(), aes(x = `Experience Level`, y = avg_salary, fill = `Experience Level`)) +
      geom_violin() +
      geom_boxplot(width = 0.1, alpha = 0.5) +
      scale_y_continuous(labels = scales::comma_format(prefix = "£")) +
      labs(x = "Experience Level", y = "Salary Range") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  output$salary_title_plot <- renderPlotly({
    req(job_data_reactive())
    validate(need(nrow(job_data_reactive()) > 0, "No data available for the selected filters."))
    
    salary_by_title <- job_data_reactive() %>%
      group_by(`Job Title`) %>%
      summarise(avg_salary = mean(avg_salary, na.rm = TRUE)) %>%
      arrange(desc(avg_salary)) %>%
      top_n(10)
    
    p <- ggplot(salary_by_title, aes(x = reorder(`Job Title`, avg_salary), y = avg_salary)) +
      geom_bar(stat = "identity", fill = "lightgreen") +
      coord_flip() +
      scale_y_continuous(labels = scales::comma_format(prefix = "£")) +
      labs(x = "Job Title", y = "Average Salary (£)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$salary_industry_plot <- renderPlotly({
    req(job_data_reactive())
    validate(need(nrow(job_data_reactive()) > 0, "No data available for the selected filters."))
    
    salary_by_industry <- job_data_reactive() %>%
      group_by(Industry) %>%
      summarise(avg_salary = mean(avg_salary, na.rm = TRUE)) %>%
      arrange(desc(avg_salary)) %>%
      top_n(10)
    
    p <- ggplot(salary_by_industry, aes(x = reorder(Industry, avg_salary), y = avg_salary)) +
      geom_bar(stat = "identity", fill = "lightgreen") +
      coord_flip() +
      scale_y_continuous(labels = scales::comma_format(prefix = "£")) +
      labs(x = "Industry", y = "Average Salary (£)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$salary_trend_plot <- renderPlotly({
    req(job_data_reactive())
    validate(need(nrow(job_data_reactive()) > 0, "No data available for the selected filters."))
    
    salary_trends <- job_data_reactive() %>%
      group_by(year) %>%
      summarise(avg_salary = mean(avg_salary, na.rm = TRUE))
    
    validate(
      need(nrow(salary_trends) > 1, "Not enough data to show trends. Try removing some filters.")
    )
    
    p <- ggplot(salary_trends, aes(x = factor(year), y = avg_salary, group = 1)) +
      geom_line(color = "darkblue", size = 1) +
      geom_point(color = "darkred", size = 3) +
      scale_y_continuous(labels = scales::comma_format(prefix = "£")) +
      labs(x = "Year", y = "Average Salary") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Temporal Analysis tab plots
  output$job_trends_plot <- renderPlotly({
    req(job_data_reactive())
    validate(need(nrow(job_data_reactive()) > 0, "No data available for the selected filters."))
    
    trend_data <- job_data_reactive() %>%
      count(date) %>%
      arrange(date)
    
    p <- ggplot(trend_data, aes(x = date, y = n)) +
      geom_line(color = "steelblue", size = 1) +
      geom_point(color = "steelblue", size = 2) +
      labs(x = "Date", y = "Number of Job Postings") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$seasonal_heatmap <- renderPlotly({
    req(job_data_reactive())
    validate(need(nrow(job_data_reactive()) > 0, "No data available for the selected filters."))
    
    seasonal_data <- job_data_reactive() %>%
      mutate(
        month_name = month(month, label = TRUE),
        year = year(`Date Posted`)
      ) %>%
      count(year, month, month_name) %>%
      complete(year, month, fill = list(n = 0))
    
    p <- ggplot(seasonal_data, aes(x = month_name, y = factor(year), fill = n)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "darkorange", high = "darkblue") +
      labs(x = "Month", y = "Year", fill = "Number of Postings") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$job_type_time_plot <- renderPlotly({
    req(job_data_reactive())
    validate(need(nrow(job_data_reactive()) > 0, "No data available for the selected filters."))
    
    job_type_time <- job_data_reactive() %>%
      count(year, `Job Type`) %>%
      complete(year, `Job Type`, fill = list(n = 0))
    
    validate(
      need(n_distinct(job_type_time$year) > 1, "Not enough data across years. Try removing some filters.")
    )
    
    p <- ggplot(job_type_time, aes(x = factor(year), y = n, fill = `Job Type`, group = `Job Type`)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Year", y = "Number of Postings", fill = "Job Type") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$experience_time_plot <- renderPlotly({
    req(job_data_reactive())
    validate(need(nrow(job_data_reactive()) > 0, "No data available for the selected filters."))
    
    experience_time <- job_data_reactive() %>%
      count(year, `Experience Level`) %>%
      complete(year, `Experience Level`, fill = list(n = 0))
    
    validate(
      need(n_distinct(experience_time$year) > 1, "Not enough data across years. Try removing some filters.")
    )
    
    p <- ggplot(experience_time, aes(x = factor(year), y = n, fill = `Experience Level`, group = `Experience Level`)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Year", y = "Number of Postings", fill = "Experience Level") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Data Explorer tab
  output$job_data_table <- renderDT({
    req(job_data_reactive())
    validate(need(nrow(job_data_reactive()) > 0, "No data available for the selected filters."))
    
    job_data_reactive() %>%
      select(`Job Title`, `Job Description`, `Required Skills`, `Salary Range`, 
             Location, Company, `Experience Level`, Industry, `Job Type`, `Date Posted`)
  })
}
# Run the application
shinyApp(ui = ui, server = server)
