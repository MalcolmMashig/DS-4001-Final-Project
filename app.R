#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(here)

here::here(
  'future-predictions.R'
) %>% 
  source()

here::here(
  'html.R'
) %>% 
  source()

source("Exploration.R")

ui <- navbarPage("",
        tabPanel("Home",
          fluidPage(
            titlePanel("Capstone Project"),
            mainPanel(
              h3("Question & Background Information"),
              p("For our capstone project, we wanted to analyze Major League Baseball, hoping to use historical data and various statistics in order to predict player value and make conclusions about the sport. We narrowed our focus to be more specific, ultimately choosing to analyze and project future performance of starting pitchers. Our inspiration for picking this particular position came from the fact that starting pitchers have become increasingly important and prominent among playoff contending teams. The Washington Nationals, the winner of the 2019 World Series, for example, were anchored by three elite, highly-paid starting pitchers in Max Scherzer, Stephen Strasburg, and Patrick Corbin. Furthermore, the size and length of the contracts that teams are giving to starting pitchers are tremendously large. This past winter, during baseball’s free agency, over 1 billion dollar was spent on starting pitchers, the most free agency dollars spent on starting pitchers in baseball history. Free agent Gerrit Cole signed a nine-year, 324 million dollar contract with the New York Yankees, which is the largest deal ever signed by a pitcher. Gerrit Cole is currently 29 years old, which is considered to be one of the prime years for a starting pitcher, but he will be 38 years old at the end of his contract, when most pitchers are already in decline and considering retirement. Only the big market baseball teams like New York, Boston, and Philadelphia have the spending capabilities to afford these massive free agent signings, while the smaller market teams like Tampa Bay and Oakland must find cheap alternatives in order to be able to compete. This relatively recent trend in the importance that the sport is placing on finding valuable starting pitching led us to make it the concentration of our capstone project."),
              h3("Exploratory Data Analysis"),
              p("As you can see by the change in fastball velocity and xFIP since 2002, the percentiles have kept a consistent distance. This shows us that standardizing variables like fastball velocity and xFIP will help to mitigate the influence of changes in the game of baseball over time on our analysis"),
              plotOutput("fbplot"),
              plotOutput("xfipplot"),
              p("The plots below show the characteristics of the fastball velocity and xFIP variables. We know from them that fastball velocity generally declines as a pitcher ages, which is expected, and that the change in xFIP for players at a certain age has a generally normal distribution. This was the case for players at the age of 28 and at most other ages"),
              plotOutput("fbvchange"),
              plotOutput("xfipnormal"),
              h3("Introduction"),
              p("This shiny app is a University of Virginia capstone project in statistics, created by second-year 
                students Devan Bose, Jordan Denish, Malcolm Mashig, and Christian Rogers. We created a model to predict 
                the future xFIP value for MLB starting pitchers. Our results are contained in this app."),
              h3("Definition of xFIP"),
              p("Expected Fielding Independent Pitching (xFIP) is an advanced baseball statistic that is an 
                estimate for a pitcher’s earned run average (ERA). A pitcher’s main objective is to prevent runs and 
                xFIP is considered to be a better predictor of future performance than ERA, which can mask the actual 
                skill of a pitcher. xFIP is a common estimate for a pitcher’s ERA that only factors in the batter 
                outcomes that a pitcher can control (strikeouts, walks, and hit by pitches) while also normalizing 
                for home runs allowed by using the league average home run/fly ball rate. xFIP is calculated using 
                the formula xFIP = ((13*(Fly balls * lgHR/FB%))+(3*(BB+HBP))-(2*K))/IP + constant. The constant is 
                generally around 3.10, which helps adjust the xFIP value to an ERA scale."),
              h3("Features of the App"),
              p("- Click on an individual player in the dropdown menu to view their future projections in table and 
              graph form"),
              p("- Click on a team from the dropdown menu to subset the data to players from your favorite team"),
              p("- Click on one of the column headers below to sort the entire dataset by that statistic"),
              p("- Use our xFIP Calculator to input values for an imaginary pitcher and we can calculate his expected xFIP for the next 3 seasons")
            ))),
        tabPanel("Predictions",
          fluidPage(
            titlePanel("Three Year xFIP Predictions"),
            fluidRow(
              column(4,
                  selectInput("sp",
                              "Starting Pitcher:",
                          c("All", unique(as.character(sort(predictions$Name)))))),
              column(4,
                  selectInput("team",
                              "Team:",
                          c("All", unique(as.character(sort(predictions$Team))))))
                      ),
            htmlOutput("picture"),
            htmlOutput("picture2"),
            tableOutput("bio"),
            DT::dataTableOutput("table"),
            tableOutput("stats"),
            plotOutput("plot"))),
        tabPanel("Calculator",
          fluidPage(
            titlePanel("xFIP Prediction Calculator"),
            sliderInput("age", "2019 Age", 18, 45, 25),
            sliderInput("fbv", "2019 Fastball Velocity", 70, 100, 90, step = 0.1),
            sliderInput("fbp", "2019 Fastball Percentage", 0, 100, 55, step = 0.1),
            sliderInput("lagxfip", "2019 xFIP", 2, 6, 4, step = 0.01),
            sliderInput("lagxfip2", "2018 xFIP", 2, 6, 4, step = 0.01),
            tableOutput("calculatedVal"),
            
            setBackgroundColor(
              color = "GhostWhite",
              gradient = c("linear", "radial"),
              direction = c("bottom", "top", "right", "left"),
              shinydashboard = FALSE
            )
            
          )))

server <- function(input, output) {
  output$fbplot <- renderPlot({
    plot1
  })
  output$xfipplot <- renderPlot({
    plot3
  })
  output$fbvchange <- renderPlot({
    fangraphs_clean %>% 
      group_by(Age) %>% 
      summarise(avg_fastball_velocity = mean(FBv, na.rm = TRUE)) %>% 
      filter(between(Age, 20, 38)) %>% 
      ggplot() +
      geom_line(aes(Age, avg_fastball_velocity), color = "blue") +
      labs(title = "Fastball Velocity steadily decreases as players age")
  })
  output$xfipnormal <- renderPlot({
    fangraphs_clean %>% 
      mutate(change_in_xFIP = lead(xFIP) - xFIP) %>% 
      filter(Age == 28) %>% 
      ggplot() +
      geom_density(aes(change_in_xFIP), fill = "blue") +
      labs(title = "Change in xFIP from age 28 to 29 is normal")
  })
  output$calculatedVal <- renderTable({
    calcFuture <- data_frame(lag_xfip = NA, lag_fbv = NA, lag_fbp = NA, lag_age = NA, lag_xfip2 = NA)
    tnt <- fangraphs_clean %>% 
      filter(Season == 2019) %>% 
      summarise(mean_xfip2019 = mean(xFIP),
                sd_xfip2019 = sd(xFIP),
                mean_fbv = mean(FBv),
                sd_fbv = sd(FBv),
                mean_fbp = mean(FBP),
                sd_fbp = sd(FBP))
    
    tet <- fangraphs_clean %>% 
      filter(Season == 2018) %>% 
      summarise(mean_xfip2018 = mean(xFIP),
                sd_xfip2018 = sd(xFIP))
    
    calcFuture <- calcFuture %>%
      mutate(   #### NEED TO STANDARDIZE THE INPUTS FOR THE FUNCTION
        lag_age = input$age,
        lag_xfip2 = (input$lagxfip2 - 
                       tet$mean_xfip2018) / tet$sd_xfip2018,
        lag_xfip = (input$lagxfip - 
                      tnt$mean_xfip2019) / tnt$sd_xfip2019,
        lag_fbv = (input$fbv - 
                     tnt$mean_fbv) / tnt$sd_fbv,
        lag_fbp = (input$fbp - 
                     tnt$mean_fbp) / tnt$sd_fbp,
        age_range = case_when(
          lag_age < 28 ~ "young",
          # between(lag_age, 28, 30) ~ "prime",
          lag_age >= 28 ~ "old"
        )
      )
    calcFuture <- calcFuture %>% 
      mutate(
        predicted_fbv1 = fbv_submodel %>% predict(newdata = calcFuture),
        predicted_fbp1 = fbp_submodel %>% predict(newdata = calcFuture)
      )
    
    calcFuture1 <- calcFuture %>% 
      filter(is.na(lag_xfip2))
    
    calcFuture1 <- calcFuture1 %>% 
      mutate(
        predicted_xfip1 = xfip_model %>% predict(newdata = calcFuture1)
      )
    
    calcFuture2 <- calcFuture %>% 
      filter(!is.na(lag_xfip2))
    
    calcFuture2 <- calcFuture2 %>% 
      mutate(
        predicted_xfip1 = xfip_model2 %>% predict(newdata = calcFuture2)
      )
    
    calcFuture <- bind_rows(calcFuture1, calcFuture2)
    
    # Second Prediction ------------------------
    
    calcFuture <- calcFuture %>% 
      mutate(
        lag_xfip2 = lag_xfip,
        lag_xfip = predicted_xfip1,
        lag_fbv = predicted_fbv1,
        lag_fbp = predicted_fbp1,
        lag_age = lag_age + 1,
        age_range = case_when(
          lag_age < 28 ~ "young",
          # between(lag_age, 28, 30) ~ "prime",
          lag_age >= 28 ~ "old"
        )
      )
    
    calcFuture <- calcFuture %>% 
      mutate(
        predicted_fbv2 = fbv_submodel %>% predict(newdata = calcFuture),
        predicted_fbp2 = fbp_submodel %>% predict(newdata = calcFuture)
      )
    
    calcFuture <- calcFuture %>% 
      mutate(
        predicted_xfip2 = xfip_model2 %>% predict(newdata = calcFuture)
      )
    
    # Third Prediction ------------------------
    
    calcFuture <- calcFuture %>% 
      mutate(
        lag_xfip3 = lag_xfip2,
        lag_xfip2 = lag_xfip,
        lag_xfip = predicted_xfip2,
        lag_fbv = predicted_fbv2,
        lag_fbp = predicted_fbp2,
        lag_age = lag_age + 1,
        age_range = case_when(
          lag_age < 28 ~ "young",
          # between(lag_age, 28, 30) ~ "prime",
          lag_age >= 28 ~ "old"
        )
      )
    
    calcFuture <- calcFuture %>% 
      mutate(
        predicted_fbv3 = fbv_submodel %>% predict(newdata = calcFuture),
        predicted_fbp3 = fbp_submodel %>% predict(newdata = calcFuture)
      )
    
    calcFuture <- calcFuture %>% 
      mutate(
        predicted_xfip3 = xfip_model3 %>% predict(newdata = calcFuture)
      )
    
    calcFuturePredictions <- calcFuture %>% 
      select(
        predicted_xfip1, predicted_xfip2, 
        predicted_xfip3
      )
    
    #####  NOW UNSTANDARDIZE

    stdzFuture <- fangraphs_clean %>%
      group_by(Season) %>%
      summarise(
        sd_xfip = sqrt((var(xFIP, na.rm = TRUE) + var(lag_xfip, na.rm = TRUE) +
                          var(lag_xfip2, na.rm = TRUE)) / 3),
        mean_xfip = (mean(xFIP, na.rm = TRUE) + mean(lag_xfip, na.rm = TRUE) +
                       mean(lag_xfip2, na.rm = TRUE)) / 3
        # sd_xfip = sd(xFIP),
        # mean_xfip = mean(xFIP)
      )
    calcFuturePredictions <- calcFuturePredictions %>%
      mutate(
        # xFIP = xFIP * sd_xfip + mean_xfip, # NOT REAL de-standardized
        "2020 Predicted xFIP" = round(predicted_xfip1 * stdzFuture[36, 2] %>% as.numeric() + stdzFuture[36, 3] %>% as.numeric(), 2),
        "2021 Predicted xFIP" = round(predicted_xfip2 * stdzFuture[36, 2] %>% as.numeric() + stdzFuture[36, 3] %>% as.numeric(), 2),
        "2022 Predicted xFIP" = round(predicted_xfip3 * stdzFuture[36, 2] %>% as.numeric() + stdzFuture[36, 3] %>% as.numeric(), 2)
      )
    calcFuturePredictions %>% 
      select("2020 Predicted xFIP", "2021 Predicted xFIP", "2022 Predicted xFIP")
    
  })
  
  output$picture <- renderText({
    if (input$sp != "All") {
      src <- predictionsURLS %>% 
        filter(Name == input$sp) %>% 
        select(URL) %>% 
        as_vector()
      c('<img src="',src,'" align="middle" width = 106.5 height = 160>')
    }
    })
  
  output$bio <- renderTable({
    if (input$sp == "All" & input$team == "All") {
    } else if (input$sp != "All") {
      get_table(
        fangraphs_urls[which(predictions$Name == input$sp)]
        )
    } else {
    }
  })
  
  output$picture2 <- renderText({
    if (input$sp == "All") {
      src2 <- "https://upload.wikimedia.org/wikipedia/en/thumb/a/a6/Major_League_Baseball_logo.svg/1200px-Major_League_Baseball_logo.svg.png"%>%
        as_vector()
      c('<img src="',src2,'" align="right" width = 180 height = 97.36>')
    }
  })
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    if (input$sp == "All" & input$team == "All") {
      predictions
    } else if (input$sp != "All") {
      predictions[predictions$Name == input$sp,]
    } else {
      predictions[predictions$Team == input$team,]
    }
  }))
  
  output$stats <- renderTable({
    if (input$sp == "All" & input$team == "All") {
    } else if (input$sp != "All") {
      fangraphs_cleanALL %>%    ## should be fangrapsh_cleanALL next step
        filter(Name == input$sp) %>% 
        mutate(Season = as.integer(Season),
               Age = as.integer(Age),
               IP = formatC(IP, format = "f", digits = 1),
               W = as.integer(W),
               L = as.integer(L)
               ) %>% 
        select(
          Season, Team, 
          Age, IP, W, L, ERA, WHIP, 
          FIP, xFIP, `K/BB`, WAR
        )
    } else {
    }
  })
  
  output$plot <- renderPlot({
    if (input$sp != "All") {
      p <- predictions %>% 
        filter(Name == input$sp)
      t <- tibble(year = 2018:2022,
             xFIP = c(p$`2018 xFIP`, p$`2019 xFIP`, 
                      p$`2020 xFIP (Predicted)`, p$`2021 xFIP (Predicted)`,
                      p$`2022 xFIP (Predicted)`))
      t %>% 
        ggplot() +
        geom_line(aes(year, xFIP, linetype = year > 2019), size = 2.5) +
        geom_hline(aes(yintercept=leagueAverageXFIP %>% as.numeric(), color = "red")) +
        geom_text(aes(2020,leagueAverageXFIP %>% as.numeric(), hjust = 2, vjust = -1, label = "League Average XFIP", color = "red")) +
        geom_point(aes(year, xFIP), size = 5, color = "black") +
        theme_linedraw() + 
        theme(legend.position = "none",
              axis.title.x = element_text(size = 30, face = "bold"),
              axis.text.x = element_text(size = 22),
              axis.title.y = element_text(size = 30, face = "bold"),
              axis.text.y = element_text(size = 22)) +
        scale_y_continuous(
          breaks = seq(2, 6, 0.5),
          labels = c("2.0", "2.5", "3.0", "3.5", "4.0", "4.5", "5.0", "5.5", "6.0"),
          limits = c(2, 6)
        )
    } else if (input$sp == "All" & input$team == "All") {} else {
      p <- predictions %>%
        filter(Team == input$team)
      t <- p %>%
        pivot_longer(contains("xFIP"), names_to = "year", values_to = 'xFIP') %>%
        mutate(year = as.numeric(str_sub(year, 1, 4)))
      t %>%
        ggplot() +
        theme_linedraw() +
        geom_line(aes(year, xFIP, color = Name, linetype = year > 2019), size = 2.5,
                  show.legend = FALSE) +
        geom_hline(aes(yintercept=leagueAverageXFIP %>% as.numeric())) +
        geom_text(aes(2020,leagueAverageXFIP %>% as.numeric(), hjust = 2, vjust = -1, label = "League Average XFIP")) +
        ## Maybe change color to red here
        geom_point(aes(year, xFIP, color = Name), size = 5) +
        theme(axis.title.x = element_text(size = 30, face = "bold"),
              axis.text.x = element_text(size = 22),
              axis.title.y = element_text(size = 30, face = "bold"),
              axis.text.y = element_text(size = 22),
              legend.title = element_text(size = 25, face = "bold"),
              legend.text = element_text(size = 20)) +
        scale_y_continuous(
          breaks = seq(2, 6, 0.5),
          labels = c("2.0", "2.5", "3.0", "3.5", "4.0", "4.5", "5.0", "5.5", "6.0"),
          limits = c(2, 6)
        )
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

