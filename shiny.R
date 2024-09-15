# load necessary libraries
library(GGally)
library(plotly)
library(tidyverse)
library(tidyr)

#load data relig_income present in tidyr package 
data(relig_income)
head(relig_income)


# Reshape the dataset from wide to long format
relig_income_long <- relig_income %>%
  pivot_longer(cols = -religion, names_to = "Income_Range", values_to = "Count")

# Check the structure of the data
str(relig_income_long)


#visualization on relig_income dataset of tidyr package

# Bar plot
ggplot(relig_income_long, aes(x = Income_Range, y = Count, fill = religion)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Income Distribution by Religion",
       x = "Income Range", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Heat map
ggplot(relig_income_long, aes(x = Income_Range, y = religion, fill = Count)) +
  geom_tile() +
  labs(title = "Heatmap of Income Distribution by Religion",
       x = "Income Range", y = "Religion") +
  theme_minimal() +
  scale_fill_gradient(low = "white", high = "orange")


#interactive plots with plotly
p <- ggplot(relig_income, aes(x = `<$10k`, y = `$75-100k`, color = religion)) +
  geom_point(size=3) +
  labs(title = "Religion income dataset: <$10k vs $75-100k")
ggplotly(p)


# Bubble Chart
ggplot(relig_income_long, aes(x = Income_Range, y = religion, size = Count, color = religion)) +
  geom_point(alpha = 0.7) +
  scale_size(range = c(3, 20)) +
  labs(title = "Income Distribution by Religious Affiliation",
       x = "Income Level",
       y = "Religion",
       size = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Check for missing values
sum(is.na(relig_income))

# Creating derived features
relig_income_enhanced <- relig_income %>%
  mutate(
    Total = rowSums(select(., -religion)),
    Proportion_Low_Income = (`<$10k` + `$10-20k`) / Total,
    Proportion_High_Income = (`$50-75k` + `$75-100k`) / Total
  )

# Standardized measurements
relig_income_standardized <- relig_income %>%
  mutate(across(where(is.numeric), scale))

# Binning continuous values
relig_income_binned <- relig_income %>%
  mutate(
    Total = rowSums(select(., -religion)),
    Total_Bin = cut(Total, breaks = 5, labels = c("Very Low", "Low", "Medium", "High", "Very High")),
    Proportion_Low_Income = (`<$10k` + `$10-20k`) / Total,
    Proportion_Low_Income_Bin = cut(Proportion_Low_Income, breaks = 5, 
                                    labels = c("Very Low", "Low", "Medium", "High", "Very High"))
  )

# Display the first few rows of each dataset
print("Enhanced Dataset:")
print(head(relig_income_enhanced))

print("Standardized Dataset:")
print(head(relig_income_standardized))

print("Binned Dataset:")
print(head(relig_income_binned))


# Now we make interactive visualization and dashboard using R shiny
library(shinydashboard)
library(shiny)


ui <- dashboardPage(
  dashboardHeader(title = "Religion Income Explorer"),
  dashboardSidebar(
    selectInput("x_var", "X Variable:", 
                choices = unique(relig_income_long$Income_Range)),
    selectInput("y_var", "Y Variable:", 
                choices = unique(relig_income_long$Income_Range),
                selected = "$75-100k"),
    checkboxGroupInput("religions", "Select Religions:",
                       choices = unique(relig_income_long$religion),
                       selected = unique(relig_income_long$religion)[1:5])
  ),
  dashboardBody(
    fluidRow(
      box(plotlyOutput("scatter_plot"), width = 8),
      box(plotOutput("bar_plot"), width = 4)
    ),
    fluidRow(
      box(dataTableOutput("data_table"), width = 12)
    )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    relig_income_long %>%
      filter(religion %in% input$religions)
  })
  
  output$scatter_plot <- renderPlotly({
    plot_data <- filtered_data() %>%
      pivot_wider(names_from = Income_Range, values_from = Count) %>%
      select(religion, !!sym(input$x_var), !!sym(input$y_var))
    
    p <- ggplot(plot_data, aes(x = !!sym(input$x_var), y = !!sym(input$y_var), color = religion, text = religion)) +
      geom_point(size = 3) +
      labs(title = paste(input$y_var, "vs", input$x_var),
           x = input$x_var, y = input$y_var) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$bar_plot <- renderPlot({
    ggplot(filtered_data() %>% filter(Income_Range %in% c(input$x_var, input$y_var)),
           aes(x = religion, y = Count, fill = Income_Range)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Comparison of Selected Income Ranges",
           x = "Religion", y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$data_table <- renderDataTable({
    filtered_data() %>%
      pivot_wider(names_from = Income_Range, values_from = Count) %>%
      arrange(desc(`$75-100k`))
  })
}


shinyApp(ui, server)
 




