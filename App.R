library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(readr)
library(dplyr)
library(plotly)
library(formattable)
library(leaflet)


#Load the data
athletes_data <- read.csv("/Users/claireyuen/Desktop/R Shiny Olympic/athletes.csv")
medals_data <- read.csv("/Users/claireyuen/Desktop/R Shiny Olympic/medals.csv")
athlete_data <- na.omit(athlete_data)
medal_data <- na.omit(medal_data)

unique_athletes_data <- athletes_data %>%
  group_by(Name) %>%
  summarize(
    Age = first(Age),
    Gender = first(Gender)
  )


# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Tokyo 2020 Olympics"),
  dashboardSidebar(
    sidebarMenu()
  ),
  dashboardBody(
    fluidRow(
      infoBox("Total Athletes", textOutput("total_athletes"), color = "purple", icon = icon("users"), fill=TRUE),
      infoBox("Average Age of Athletes", textOutput("average_age"), color = "yellow", icon = icon("user"), fill=TRUE),
      infoBox("Total Events Held", textOutput("total_events"), color = "blue", icon = icon("calendar"), fill=TRUE)
    ),
    fluidRow(
      box(title = "Gender Distribution of Athletes", plotlyOutput("gender_pie_chart"), height = 300),
      box(title = "Top 5 Countries with the Most Medals", formattableOutput("top_countries_table"), height = 300)
    ),
    fluidRow(
    box(title = "Top 10 Sports Participation", plotlyOutput("top_sports_bar_chart"), height = 400),
    box(title = "Number of Medals by Country Map", plotlyOutput("countries_map"), height = 400)
    )
  )
)

server <- function(input, output) {
  
  # Calculate total number of athletes
  output$total_athletes <- renderText({
    n_distinct(athletes_data$Name)
  })
  
  # Calculate average age of athletes
  output$average_age <- renderText({
    mean_age <- mean(unique_athletes_data$Age, na.rm = TRUE)
    rounded_mean_age <- round(mean_age, 2)  # Round to two decimal places
    as.character(rounded_mean_age)  # Convert to character to display in renderText
  })
  
  # Calculate total number of events
  output$total_events <- renderText({
    n_distinct(athletes_data$Event)
  })
  
  # Create gender distribution pie chart
  output$gender_pie_chart <- renderPlotly({
    gender_counts <- table(unique_athletes_data$Gender)
    
    # Custom colors for male (lighter navy blue) and female (pink)
    colors <- c("Male" = "#6495ED", "Female" = "pink")
    
    pie_chart <- plot_ly(
      labels = names(gender_counts),
      values = gender_counts,
      type = "pie",
      marker = list(colors = colors),  # Apply custom colors
      textinfo = "label+percent"  # Add labels and percentages
    ) %>%
      layout(
        height = 230
        # width = 300
      )
    
    pie_chart
  })
  
  # Create table for top 5 countries with most medals
  output$top_countries_table <- renderFormattable({
    top_countries <- medals_data %>%
      arrange(desc(Total)) %>%
      head(5)
    
    formattable::formattable(
      top_countries,
      list(
        Country = color_tile("white", "lightblue"),
        Gold = color_bar("gold"),
        Silver = color_bar("silver"),
        Bronze = color_bar("#D2B48C"),
        Total = color_bar("lightblue")
      ),
      searchable = FALSE,  # Disable search bar
      pageLength = 5,  # Set the number of rows per page (adjust as needed)
      table.attr = 'class="table table-bordered table-condensed"'  # Optional: Add Bootstrap styling
    )
  })
  
  # Create bar chart for top 10 sports with most participation
  output$top_sports_bar_chart <- renderPlotly({
    top_sports <- athletes_data %>%
      group_by(Sport) %>%
      summarize(Count = n()) %>%
      arrange(desc(Count)) %>%
      head(10)
    
    bar_chart <- plot_ly(
      x = top_sports$Count,
      y = reorder(top_sports$Sport, top_sports$Count),  # Arrange sports by participant count
      type = "bar",
      orientation = "h"
    )%>%
    layout(
      height = 320  # Adjust the height (in pixels) as needed
    )
    bar_chart
  })
  
  # Create a map showing the number of medals by country
  output$countries_map <- renderPlotly({
    
    # Create the map using plot_ly
    map <- plot_ly(
      data = medals_data,
      type = "choropleth",
      locations = ~Country,  # Country names
      locationmode = "country names",  # Use country names as location mode
      z = ~Total,  # Total medals as the data to plot
      colorscale = "YlOrRd",  # Choose a color scale
      marker = list(line = list(color = "gray")),
      colorbar = list(title = "Total Medals"),
      height = 320
    ) %>%
      layout(
        geo = list(
          showcoastlines = TRUE,
          coastlinecolor = "gray",
          projection = list(type = "mercator")
        )
      )
    
    map
  })
  
}

shinyApp(ui, server)
