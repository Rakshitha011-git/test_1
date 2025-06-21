library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(janitor)

data <- read.csv("superstore_data.csv") -> data

data$Dt_Customer <- as.Date(data$Dt_Customer, "%m/%d/%Y")
data$Year <- year(data$Dt_Customer)
data$Age <- 2025 - data$Year_Birth
data$Total_Spending <- rowSums(data[, c("MntWines", "MntFruits", "MntMeatProducts",
                                        "MntFishProducts", "MntSweetProducts", "MntGoldProds")])

ui <- fluidPage(
  titlePanel("Customer Purchase Analysis"),
  tabsetPanel(tabPanel("Year-wise Analysis",
                       fluidRow(column(6, plotOutput("enrollment_plot")),
                                column(6, plotOutput("year_spend_plot"))
                       ),plotOutput("marital_spend_plot")
  ),
  
  tabPanel("Age-based Purchase Behavior",
           sliderInput("age_range", "Select Age Range:", min = min(data$Age),
                       max = max(data$Age), value = c(25, 60)),
           plotOutput("age_purchase_plot")
  ),
  
  tabPanel("Income vs Spending",
           selectInput("edu_filter", "Select Education Level:", choices = unique(data$Education),
                       selected = unique(data$Education), multiple = T),
           selectInput("marital_filter", "Select Marital Status:", choices = unique(data$Marital_Status),
                       selected = unique(data$Marital_Status), multiple = T),
           plotOutput("income_vs_spend")
  ),
  
  tabPanel("Best_selling Category analysis",
           selectInput("year_input", "Select Year:", choices = c("All", unique(data$Year)), selected = "All"),
           selectInput("marital_input", "Select Marital Status:", choices = c("All", unique(data$Marital_Status)), selected = "All"),
           plotOutput("best_category")
  ),
  
  tabPanel("In-store vs Online purchases",
           plotOutput("purchase_channel_plot"),
           downloadButton("downloadPlot", "Download Plot")
  )
  )
)




server <- function(input, output) {
  
  output$enrollment_plot <- renderPlot({
    enroll_data <- data %>%
      group_by(Year) %>%
      summarise(Count = n())
    
    ggplot(enroll_data, aes(x = Year, y = Count)) +
      geom_col(fill = "gold") +
      labs(title = "Customer Enrollment by Year", x = "Year", y = "Number of Customers")
  })
  
  output$year_spend_plot <- renderPlot({
    spend_data <- data %>%
      group_by(Year) %>%
      summarise(TotalSpending = sum(Total_Spending) / 1000)
    
    ggplot(spend_data, aes(x = Year, y = TotalSpending)) +
      geom_col(fill = "brown") +
      labs(title = "Total Spending by Year", x = "Year", y = "Spending (in Thousands)")
  })
  
  output$marital_spend_plot <- renderPlot({
    marital_spend <- data %>%
      group_by(Marital_Status) %>%
      summarise(TotalSpending = sum(Total_Spending) / 1000)
    
    ggplot(marital_spend, aes(x = Marital_Status, y = TotalSpending, fill = Marital_Status)) +
      geom_col() +
      labs(title = "Total Spending by Marital Status", x = "Marital Status", y = "Spending (in Thousands)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # 2. Age-based Purchase Behavior
  output$age_purchase_plot <- renderPlot({
    filtered <- data %>%
      filter(Age >= input$age_range[1], Age <= input$age_range[2]) %>%
      mutate(TotalPurchases = NumWebPurchases + NumCatalogPurchases + NumStorePurchases)
    
    ggplot(filtered, aes(x = Age, y = TotalPurchases)) +
      geom_point(color = "purple", alpha = 0.6) +
      geom_smooth(method = "loess") +
      labs(title = "Purchases by Age", x = "Age", y = "Total Purchases")
  })
  
  output$income_vs_spend <- renderPlot({
    filtered <- data %>%
      filter(Education %in% input$edu_filter, Marital_Status %in% input$marital_filter)
    
    ggplot(filtered, aes(x = Income, y = Total_Spending / 1000)) +
      geom_point(color = "red", alpha = 0.6) +
      geom_smooth(method = "lm", se = F) +
      labs(title = "Income vs. Total Spending", x = "Income", y = "Spending (in Thousands)")
  })
  
  output$best_category <- renderPlot({
    filtered <- data
    
    if (input$year_input != "All") {
      filtered <- filtered %>% filter(Year == input$year_input)
    }
    if (input$marital_input != "All") {
      filtered <- filtered %>% filter(Marital_Status == input$marital_input)
    }
    
    long_data <- filtered %>%
      select(MntWines, MntFruits, MntMeatProducts, MntFishProducts,
             MntSweetProducts, MntGoldProds) %>%
      summarise_all(~sum(.x, na.rm = T) / 1000) %>%
      pivot_longer(cols = everything(), names_to = "Category", values_to = "Total") %>%
      arrange(desc(Total))
    
    ggplot(long_data, aes(x = reorder(Category, -Total), y = Total, fill = Category)) +
      geom_col() +
      labs(title = "Best-Selling Categories", x = "Category", y = "Sales (in Thousands)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$purchase_channel_plot <- renderPlot({
    channel_data <- data %>%
      summarise(
        InStore_Purchases = sum(NumStorePurchases, na.rm = TRUE),
        Online_Purchases = sum(NumWebPurchases + NumCatalogPurchases, na.rm = TRUE),
        Total_Purchases = sum(NumWebPurchases + NumCatalogPurchases + NumStorePurchases, na.rm = TRUE),
        InStore_Spending = sum(Total_Spending * NumStorePurchases / Total_Purchases, na.rm = TRUE),
        Online_Spending = sum(Total_Spending * (NumWebPurchases + NumCatalogPurchases) / Total_Purchases, na.rm = TRUE)
      )
    
    plot_data <- data.frame(
      Channel = rep(c("In-Store", "Online"), times = 2),
      Metric = c(rep("Purchases", 2), rep("Spending", 2)),
      Value = c(channel_data$InStore_Purchases, channel_data$Online_Purchases,
                channel_data$InStore_Spending, channel_data$Online_Spending)
    )
    
    ggplot(plot_data, aes(x = Channel, y = Value, fill = Metric)) +
      geom_col(position = "dodge") +
      labs(
        title = "In-store vs. Online: Purchases and Spending",
        y = "Count / Amount",
        x = "Channel"
      ) +
      scale_fill_manual(values = c("Purchases" = "blue", "Spending" = "green")) +
      theme_minimal(base_size = 14)
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("channel_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file)
      channel_data <- data %>%
        summarise(
          InStore_Purchases = sum(NumStorePurchases),
          Online_Purchases = sum(NumWebPurchases + NumCatalogPurchases)
        )
      barplot(height = as.numeric(channel_data),
              names.arg = c("In-Store", "Online"),
              col = c("steelblue", "maroon"),
              main = "In-store vs Online Purchases")
      dev.off()
    }
  )
}

shinyApp(ui = ui, server = server)