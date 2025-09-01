# Load Required Libraries
library(shiny)
library(plotly)
library(dplyr)
library(stringr)
library(tidyr)
library(shinyjs)
library(data.table)
library(syuzhet)

# Define colors for Introvert vs Extrovert
ie_colors <- c(
  "Introvert" = "#3498DB",   # Cool blue
  "Extrovert" = "#E74C3C"    # Warm red
)

# JavaScript: Auto-trigger welcome modal on app load
jsCode <- "
shinyjs.init = function() {
  setTimeout(function() {
    $('#welcome_btn').click();
  }, 1000);
}
"

# Clean up environment on reload (prevent duplicate variables)
if (exists("app_running")) {
  rm(list = setdiff(ls(), c("ie_colors", "jsCode")))
}
app_running <- TRUE

# Load and Prepare Data
user_info <- read.csv("data/user_info.csv")
mbti_labels <- read.csv("data/mbti_labels.csv")
tweets <- fread("data/user_tweets.csv")

# Ensure IDs are treated as character strings
user_info$id <- as.character(user_info$id)
mbti_labels$id <- as.character(mbti_labels$id)
tweets$id <- as.character(tweets$id)

# Join user info with MBTI labels and remove extreme outliers
user_data <- user_info %>%
  left_join(mbti_labels, by = "id") %>%
  filter(
    followers_count < quantile(followers_count, 0.999, na.rm = TRUE),
    friends_count < quantile(friends_count, 0.999, na.rm = TRUE),
    statuses_count < quantile(statuses_count, 0.999, na.rm = TRUE),
    favourites_count < quantile(favourites_count, 0.999, na.rm = TRUE)
  )

# Count distribution of MBTI types
mbti_distribution <- user_data %>%
  count(mbti_personality) %>%
  mutate(percent = n / sum(n))

# Calculate summary stats by MBTI type
mbti_summary <- user_data %>%
  group_by(mbti_personality) %>%
  summarise(
    avg_followers = mean(followers_count, na.rm = TRUE),
    avg_friends = mean(friends_count, na.rm = TRUE),
    avg_statuses = mean(statuses_count, na.rm = TRUE),
    avg_favourites = mean(favourites_count, na.rm = TRUE),
    count = n(),
    .groups = 'drop'
  )

# Clean tweet data: pivot long and drop blanks
tweets_clean <- tweets %>%
  filter(grepl("^[0-9]+$", id)) %>%
  pivot_longer(cols = starts_with("tweet_"),
               names_to = "tweet_number", values_to = "tweet_text") %>%
  filter(!is.na(tweet_text) & tweet_text != "")

# Join tweets with MBTI labels and extract sentiment & interaction type
tweets_joined <- tweets_clean %>%
  left_join(mbti_labels, by = "id") %>%
  filter(!is.na(mbti_personality)) %>%
  mutate(
    sentiment = get_sentiment(tweet_text),
    interaction_type = case_when(
      grepl("^RT", tweet_text) ~ "Amplification",
      grepl("@", tweet_text) ~ "Expression",
      TRUE ~ "Other"
    ),
    ie_group = ifelse(toupper(substr(mbti_personality, 1, 1)) == "I", "Introvert", "Extrovert")
  )

# Average sentiment by MBTI type
mbti_sentiment <- tweets_joined %>%
  group_by(mbti_personality) %>%
  summarise(avg_sentiment = mean(sentiment, na.rm = TRUE), .groups = 'drop')

# Summary of interaction types by I/E group
interaction_summary <- tweets_joined %>%
  filter(interaction_type %in% c("Expression", "Amplification")) %>%
  count(ie_group, interaction_type) %>%
  complete(ie_group = c("Introvert", "Extrovert"),
           interaction_type = c("Expression", "Amplification"), fill = list(n = 0)) %>%
  group_by(ie_group) %>%
  mutate(percentage = n / sum(n) * 100)

# Emoji analysis
emoji_pattern <- "[\U0001F300-\U0001F6FF\U0001F900-\U0001F9FF\U0001FA70-\U0001FAFF\u2600-\u27BF]"

emoji_usage_all <- tweets_joined %>%
  mutate(matched = str_extract_all(tweet_text, emoji_pattern)) %>%
  unnest(matched) %>%
  filter(
    !is.na(matched),
    matched != "",
    nchar(matched) >= 1,
    !matched %in% c("\U0001F3FB", "\U0001F3FC", "\U0001F3FD", "\U0001F3FE", "\U0001F3FF"), # Exclude skin tones
    !str_detect(matched, "[\uFE0F\u200D]")  # Remove variation selectors and joiners
  ) %>%
  mutate(matched = str_trim(matched))

# Identify top emojis
top_emojis <- emoji_usage_all %>%
  count(matched, sort = TRUE) %>%
  filter(n >= 100, nchar(matched) <= 4) %>%
  slice_max(n, n = 10) %>%
  pull(matched)

# Fallback: if too few top emojis found
if(length(top_emojis) < 5) {
  top_emojis <- emoji_usage_all %>%
    count(matched, sort = TRUE) %>%
    filter(n >= 10) %>%
    slice_max(n, n = 8) %>%
    pull(matched)
}

# Emoji usage breakdown by I/E group
emoji_usage <- emoji_usage_all %>%
  filter(matched %in% top_emojis) %>%
  count(ie_group, matched) %>%
  complete(ie_group = c("Introvert", "Extrovert"), 
           matched = top_emojis, 
           fill = list(n = 0))

# Count total usage per emoji
emoji_counts <- emoji_usage %>%
  group_by(matched) %>%
  summarise(total = sum(n), .groups = 'drop') %>%
  arrange(desc(total))

# Generate emoji dropdown choices
emoji_choices <- setNames(
  emoji_counts$matched,
  paste0(emoji_counts$matched, " (", emoji_counts$total, " uses)")
)

# Funnel chart data
# Normalize post/like/retweet metrics and relabel for visualization
funnel_data <- user_data %>%
  mutate(ie_group = ifelse(toupper(substr(mbti_personality, 1, 1)) == "I", "Introvert", "Extrovert")) %>%
  group_by(ie_group) %>%
  summarise(
    avg_posts = mean(statuses_count, na.rm = TRUE),
    avg_favourites = mean(favourites_count, na.rm = TRUE),
    avg_retweets = mean(total_retweet_count, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  pivot_longer(cols = -ie_group, names_to = "metric", values_to = "value") %>%
  group_by(metric) %>%
  mutate(normalized_value = value / max(value) * 100) %>%
  ungroup() %>%
  mutate(metric = recode(metric,
                         "avg_posts" = "Posts Created",
                         "avg_favourites" = "Likes Received", 
                         "avg_retweets" = "Retweets Earned"))



# UI
ui <- navbarPage("MBTI Social Media Dashboard",
                 
                 useShinyjs(),
                 extendShinyjs(text = jsCode, functions = c()),
                 
                 # Custom CSS styles
                 tags$head(
                   tags$style(HTML("
                     .guide-btn { margin: 3px; font-size: 11px; }
                     .chart-header { 
                       background-color: #f8f9fa; 
                       padding: 10px; 
                       margin-bottom: 10px; 
                       border-radius: 5px;
                       border-left: 4px solid #007bff;
                     }
                     .stats-box {
                       background-color: #e9ecef;
                       padding: 10px;
                       border-radius: 5px;
                       margin-bottom: 15px;
                     }
                   "))
                 ),
                 
                 tabPanel("Overview",
                          
                          # Welcome modal
                          div(id = "welcome_content", style = "display:none;",
                              div(
                                h4("What is this dashboard?"),
                                p("Explore how different MBTI personality types behave on social media! 
                                  This dashboard analyzes real Twitter data to uncover fascinating patterns in 
                                  posting habits, emotional expression, and communication styles."),
                                
                                h4("What you'll discover:"),
                                tags$ul(
                                  tags$li("Which personality types are most active on social media"),
                                  tags$li("How different types express emotions in their tweets"),
                                  tags$li("Communication patterns: expression vs amplification"),
                                  tags$li("Popular emoji usage across personality types"),
                                  tags$li("Activity level comparisons and engagement metrics")
                                ),
                                
                                h4("How to explore:"),
                                p("• Use filters to focus on specific MBTI types"),
                                p("• Hover over charts for detailed information"),
                                p("• Click 'Guide' buttons for chart explanations"),
                                
                                div(style = "text-align: center; margin-top: 20px;",
                                    p("Happy exploring!", style = "margin-top: 10px; color: #666;")
                                )
                              )
                          ),
                          
                          # Hidden trigger button for welcome modal
                          actionButton("welcome_btn", "", style = "display:none;"),
                          
                          sidebarLayout(
                            sidebarPanel(
                              # Filter controls
                              h5("Controls"),
                              checkboxGroupInput("selected_mbti", "Select MBTI Type(s):",
                                                 choices = unique(mbti_distribution$mbti_personality),
                                                 selected = unique(mbti_distribution$mbti_personality)),
                              
                              hr(),
                              
                              # Quick statistics box
                              div(class = "stats-box",
                                  h6("Quick Stats"),
                                  verbatimTextOutput("quick_stats", placeholder = TRUE)
                              ),
                              
                              # Reset filters button
                              actionButton("reset_filters", "Reset Filters", 
                                           class = "btn btn-warning btn-sm", style = "width: 100%;"),
                              
                              width = 2
                            ),
                            
                            mainPanel(
                              
                              # MBTI Type Distribution chart
                              div(class = "chart-header",
                                  fluidRow(
                                    column(9, h4("MBTI Type Distribution")),
                                    column(3, actionButton("guide_distribution", "Guide", 
                                                           class = "btn btn-outline-info guide-btn"))
                                  )
                              ),
                              plotlyOutput("plot_distribution", height = "300px"),
                              
                              br(),
                              
                              # Engagement patterns chart
                              div(class = "chart-header",
                                  fluidRow(
                                    column(9, h4("Social Media Engagement Patterns")),
                                    column(3, actionButton("guide_engagement", "Guide", 
                                                           class = "btn btn-outline-info guide-btn"))
                                  )
                              ),
                              plotlyOutput("plot_visibility", height = "500px"),
                              
                              br(),
                              
                              # Sentiment analysis chart
                              div(class = "chart-header",
                                  fluidRow(
                                    column(9, h4("Emotional Expression Analysis")),
                                    column(3, actionButton("guide_sentiment", "Guide", 
                                                           class = "btn btn-outline-info guide-btn"))
                                  )
                              ),
                              plotlyOutput("plot_sentiment"),
                              
                              br(),
                              
                              # Tab panels for additional visuals
                              tabsetPanel(
                                
                                tabPanel("Interaction Style",
                                         br(),
                                         div(class = "chart-header",
                                             fluidRow(
                                               column(9, h4("Communication Patterns")),
                                               column(3, actionButton("guide_interaction", "Guide", 
                                                                      class = "btn btn-outline-info guide-btn"))
                                             )
                                         ),
                                         plotlyOutput("plot_expression_amplification")
                                ),
                                
                                tabPanel("Emoji Usage",
                                         br(),
                                         div(class = "chart-header",
                                             fluidRow(
                                               column(7, h4("Emoji Preferences")),
                                               column(3, actionButton("guide_emoji", "Guide", 
                                                                      class = "btn btn-outline-info guide-btn")),
                                               column(2, actionButton("emoji_random", "Random", 
                                                                      class = "btn btn-outline-secondary guide-btn"))
                                             )
                                         ),
                                         fluidRow(
                                           column(
                                             width = 9,
                                             plotlyOutput("plot_emoji_usage")
                                           ),
                                           column(
                                             width = 3,
                                             div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px;",
                                                 h6("Emoji Controls"),
                                                 selectInput("emoji_select", "Pick Emojis to Display:",
                                                             choices = character(0),
                                                             multiple = TRUE),
                                                 hr(),
                                                 h6("Tips:"),
                                                 p("• Select multiple emojis to compare", style = "font-size: 12px;"),
                                                 p("• Numbers show total usage count", style = "font-size: 12px;"),
                                                 p("• Try the random button!", style = "font-size: 12px;")
                                             )
                                           )
                                         )
                                ),
                                
                                tabPanel("Activity Levels",
                                         br(),
                                         div(class = "chart-header",
                                             fluidRow(
                                               column(9, h4("Activity Level Comparison")),
                                               column(3, actionButton("guide_funnel", "Guide", 
                                                                      class = "btn btn-outline-info guide-btn"))
                                             )
                                         ),
                                         plotlyOutput("plot_funnel")
                                )
                              )
                            )
                          )
                 ),
                 
                 # Footer section
                 tags$footer(
                   style = "background-color: #f8f9fa; border-top: 1px solid #dee2e6; padding: 20px 0; margin-top: 50px;",
                   div(class = "container-fluid",
                       fluidRow(
                         column(12,
                                div(style = "text-align: center;",
                                    h5("Data Source", style = "color: #495057;"),
                                    
                                    # Dataset description
                                    div(style = "font-size: 13px; color: #6c757d; margin-bottom: 15px; text-align: center; max-width: 600px; margin-left: auto; margin-right: auto;",
                                        p(strong("Twitter MBTI Personality Types Dataset"), 
                                          br(),
                                          "Source: ",
                                          tags$a("Kaggle - Twitter MBTI Dataset", 
                                                 href = "https://www.kaggle.com/datasets/sanketrai/twitter-mbti-dataset/data", 
                                                 target = "_blank",
                                                 style = "color: #007bff;"),
                                          br(),
                                          "8,328 Twitter users with self-reported MBTI types and recent tweets"
                                        )
                                    ),
                                    
                                    # Author and course info
                                    p(style = "font-size: 14px; color: #6c757d; margin-bottom: 10px;",
                                      strong("Author: "), 
                                      "Yinghan Ma | ",
                                      strong("Course: "), 
                                      "FIT5147 DVP Assignment 2 | ",
                                      strong("Institution: "), 
                                      "Monash University"
                                    ),
                                    
                                    # Creation date
                                    p(style = "font-size: 12px; color: #6c757d; margin-bottom: 5px;",
                                      strong("Dashboard Created: "), 
                                      "June 11, 2025"
                                    ),
                                    
                                    # Academic disclaimer
                                    p(style = "font-size: 11px; color: #868e96; margin-bottom: 0;",
                                      "This dashboard was created for educational purposes using publicly available datasets. ",
                                      "All analysis follows academic integrity guidelines."
                                    )
                                )
                         )
                       )
                   )
                 )
              )
                 

# Server Logic
server <- function(input, output, session) {
  
  # Trigger welcome modal on app load
  observeEvent(input$welcome_btn, {
    showModal(modalDialog(
      title = "Welcome to MBTI Social Media Dashboard!",
      size = "l",
      div(
        h4("What is this dashboard?"),
        p("Explore how different MBTI personality types behave on social media! 
          This dashboard analyzes real Twitter data to uncover fascinating patterns in 
          posting habits, emotional expression, and communication styles."),
        
        h4("What you'll discover:"),
        tags$ul(
          tags$li("Which personality types are most active on social media"),
          tags$li("How different types express emotions in their tweets"),
          tags$li("Communication patterns: expression vs amplification"),
          tags$li("Popular emoji usage across personality types"),
          tags$li("Activity level comparisons and engagement metrics")
        ),
        
        h4("How to explore:"),
        p("• Use filters to focus on specific MBTI types"),
        p("• Hover over charts for detailed information"),
        p("• Click 'Guide' buttons for chart explanations"),
        
        div(style = "text-align: center; margin-top: 20px;",
            p("Happy exploring!", style = "margin-top: 10px; color: #666;")
        )
      ),
      easyClose = TRUE,
      footer = modalButton("Start Exploring")
    ))
  })
  
  # Reset MBTI selection to default (all selected)
  observeEvent(input$reset_filters, {
    updateCheckboxGroupInput(session, "selected_mbti", 
                             selected = unique(mbti_distribution$mbti_personality))
  })
  
  # Randomly select 3 emojis to display
  observeEvent(input$emoji_random, {
    random_emojis <- sample(emoji_counts$matched, size = min(3, length(emoji_counts$matched)))
    updateSelectInput(session, "emoji_select", selected = random_emojis)
  })
  
  # Display quick summary stats
  output$quick_stats <- renderText({
    filtered_count <- length(input$selected_mbti)
    total_count <- length(unique(mbti_distribution$mbti_personality))
    most_common <- mbti_distribution$mbti_personality[which.max(mbti_distribution$n)]
    total_users <- sum(mbti_distribution$n)
    
    paste0("Selected: ", filtered_count, "/", total_count, " types\n",
           "Most common: ", most_common, "\n",
           "Total users: ", format(total_users, big.mark = ","))
  })
  
  # --- Chart Guides (text only, emoji-free) ---
  
  observeEvent(input$guide_distribution, {
    showModal(modalDialog(
      title = "MBTI Distribution Guide",
      size = "m",
      h4("What does this show?"),
      p("This pie chart displays the percentage of each MBTI personality type in our Twitter dataset."),
      h4("How to read it:"),
      tags$ul(
        tags$li("Each slice = one MBTI type"),
        tags$li("Larger slices = more common types"),
        tags$li("Percentages show relative frequency")
      ),
      h4("What to look for:"),
      p("• Which types are most/least represented?"),
      p("• How balanced is the distribution?"),
      easyClose = TRUE, footer = modalButton("Got it!")
    ))
  })
  
  observeEvent(input$guide_engagement, {
    showModal(modalDialog(
      title = "Social Media Engagement Guide",
      size = "l",
      h4("What does this show?"),
      p("Four charts comparing average social media activity across MBTI types."),
      h4("The four metrics:"),
      tags$ul(
        tags$li("Tweets: Posts created"),
        tags$li("Followers: People following them"),
        tags$li("Likes: Appreciation received"),
        tags$li("Friends: Accounts they follow")
      ),
      h4("Questions to explore:"),
      p("• Which types are most active?"),
      p("• Who attracts more followers?"),
      p("• How do engagement patterns differ?"),
      easyClose = TRUE, footer = modalButton("Got it!")
    ))
  })
  
  observeEvent(input$guide_sentiment, {
    showModal(modalDialog(
      title = "Emotional Expression Guide",
      h4("What does this show?"),
      p("Average positivity score of tweets for each MBTI type."),
      h4("Score meaning:"),
      tags$ul(
        tags$li("0 = Neutral language"),
        tags$li("Higher = More positive/optimistic"),
        tags$li("Lower = More negative/critical")
      ),
      h4("Look for patterns:"),
      p("• Which types are more positive?"),
      p("• Any surprises compared to stereotypes?"),
      easyClose = TRUE, footer = modalButton("Got it!")
    ))
  })
  
  observeEvent(input$guide_interaction, {
    showModal(modalDialog(
      title = "Communication Patterns Guide",
      h4("What does this show?"),
      p("How introverts vs extroverts differ in sharing content."),
      h4("Two interaction types:"),
      tags$ul(
        tags$li("Expression: Original posts, replies"),
        tags$li("Amplification: Retweets, shares")
      ),
      h4("Think about:"),
      p("• Do extroverts create more original content?"),
      p("• Who amplifies others more?"),
      easyClose = TRUE, footer = modalButton("Got it!")
    ))
  })
  
  observeEvent(input$guide_emoji, {
    showModal(modalDialog(
      title = "Emoji Usage Guide",
      h4("What does this show?"),
      p("Popular emoji usage comparison between introverts and extroverts."),
      h4("How to use:"),
      tags$ul(
        tags$li("Select emojis from dropdown"),
        tags$li("Numbers show total usage"),
        tags$li("Compare bar heights between groups")
      ),
      h4("Explore:"),
      p("• Do certain emojis appeal more to one group?"),
      p("• Which emojis are universally popular?"),
      easyClose = TRUE, footer = modalButton("Got it!")
    ))
  })
  
  observeEvent(input$guide_funnel, {
    showModal(modalDialog(
      title = "Activity Level Guide",
      h4("What does this show?"),
      p("Relative activity levels across social media metrics."),
      h4("Three metrics:"),
      tags$ul(
        tags$li("Posts Created: Content generation"),
        tags$li("Likes Received: Content appreciation"),
        tags$li("Retweets Earned: Content amplification")
      ),
      h4("Reading the chart:"),
      p("• Percentages are relative (100% = highest in that category)"),
      p("• Hover for actual numbers"),
      h4("Compare:"),
      p("• Which group generates more content?"),
      p("• Who gets more engagement?"),
      easyClose = TRUE, footer = modalButton("Got it!")
    ))
  })
  
  # Update emoji selector on load
  observe({
    updateSelectInput(session, "emoji_select",
                      choices = emoji_choices,
                      selected = head(emoji_counts$matched, 5))
  })
  
  # --- Chart Outputs ---
  
  # MBTI Type Distribution Pie Chart
  output$plot_distribution <- renderPlotly({
    filtered <- mbti_distribution %>% filter(mbti_personality %in% input$selected_mbti)
    
    plot_ly(filtered, 
            labels = ~mbti_personality, 
            values = ~percent,
            type = 'pie', 
            textinfo = 'label+percent') %>%
      layout(title = "Distribution of MBTI Types")
  })
  
  # Social Media Engagement Metrics
  output$plot_visibility <- renderPlotly({
    data <- mbti_summary %>% filter(mbti_personality %in% input$selected_mbti)
    
    p1 <- plot_ly(data, x = ~mbti_personality, y = ~avg_statuses, type = "bar", 
                  marker = list(color = "#3498DB"),
                  name = "Tweets",
                  showlegend = TRUE,
                  hovertemplate = "<b>%{x}</b><br>Average Tweets: %{y:.0f}<extra></extra>")
    
    p2 <- plot_ly(data, x = ~mbti_personality, y = ~avg_followers, type = "bar", 
                  marker = list(color = "#E74C3C"),
                  name = "Followers",
                  showlegend = TRUE,
                  hovertemplate = "<b>%{x}</b><br>Average Followers: %{y:.0f}<extra></extra>")
    
    p3 <- plot_ly(data, x = ~mbti_personality, y = ~avg_favourites, type = "bar", 
                  marker = list(color = "#27AE60"),
                  name = "Likes",
                  showlegend = TRUE,
                  hovertemplate = "<b>%{x}</b><br>Average Likes: %{y:.0f}<extra></extra>")
    
    p4 <- plot_ly(data, x = ~mbti_personality, y = ~avg_friends, type = "bar", 
                  marker = list(color = "#F39C12"),
                  name = "Friends",
                  showlegend = TRUE,
                  hovertemplate = "<b>%{x}</b><br>Average Friends: %{y:.0f}<extra></extra>")
    
    subplot(p1, p2, p3, p4, nrows = 2) %>%
      layout(
        title = list(text = "Social Media Engagement Across MBTI Types", font = list(size = 16)),
        legend = list(orientation = "v", x = 1.02, y = 0.8,
                      bgcolor = "rgba(255,255,255,0.8)",
                      bordercolor = "rgba(0,0,0,0.2)", borderwidth = 1,
                      font = list(size = 12)),
        margin = list(r = 120, l = 60, t = 80, b = 60)
      )
  })
  
  # Sentiment Score Chart
  output$plot_sentiment <- renderPlotly({
    filtered <- mbti_sentiment %>% filter(mbti_personality %in% input$selected_mbti)
    
    plot_ly(filtered, 
            x = ~mbti_personality, 
            y = ~avg_sentiment, 
            type = "bar",
            marker = list(color = "#3498DB"),
            hovertemplate = "<b>%{x}</b><br>Positivity Score: %{y:.3f}<extra></extra>") %>%
      layout(
        title = "How Positive Are Different MBTI Types in Their Tweets?",
        xaxis = list(title = "MBTI Personality Type"),
        yaxis = list(title = "Positivity Score"),
        margin = list(r = 250),
        annotations = list(
          list(
            x = 1.15, y = 0.5,
            text = "Note:<br>Positivity scores range<br>from 0 (neutral) to<br>1 (most positive)",
            showarrow = FALSE,
            xref = "paper", yref = "paper",
            font = list(size = 10, color = "gray"),
            xanchor = "left",
            align = "left"
          )
        )
      )
  })
  
  # Interaction Type (Expression vs Amplification)
  output$plot_expression_amplification <- renderPlotly({
    plot_ly(interaction_summary, 
            x = ~interaction_type, 
            y = ~percentage, 
            type = "bar",
            color = ~ie_group, 
            colors = ie_colors) %>%
      layout(
        title = "How Do Introverts vs Extroverts Interact on Social Media?",
        yaxis = list(title = "Percentage of Posts"),
        xaxis = list(title = "Type of Interaction"),
        barmode = "group"
      )
  })
  
  # Emoji Usage Comparison
  output$plot_emoji_usage <- renderPlotly({
    selected_emojis <- if(is.null(input$emoji_select) || length(input$emoji_select) == 0) {
      head(top_emojis, 5)
    } else {
      input$emoji_select
    }
    
    filtered <- emoji_usage %>% 
      filter(matched %in% selected_emojis, n > 0)
    
    if(nrow(filtered) == 0) {
      plot_ly() %>%
        layout(title = "No data for selected emojis",
               xaxis = list(title = "Emoji"),
               yaxis = list(title = "Times Used"))
    } else {
      plot_ly(filtered, 
              x = ~matched, 
              y = ~n, 
              type = "bar", 
              color = ~ie_group, 
              colors = ie_colors,
              hovertemplate = "<b>%{fullData.name}</b><br>Emoji: %{x}<br>Used: %{y} times<extra></extra>") %>%
        layout(
          title = "Most Popular Emojis: Introverts vs Extroverts",
          xaxis = list(title = "Emoji", tickfont = list(size = 16)),
          yaxis = list(title = "Times Used"),
          barmode = "group"
        )
    }
  })
  
  # Activity Funnel Chart
  output$plot_funnel <- renderPlotly({
    plot_ly(funnel_data, 
            y = ~metric, 
            x = ~normalized_value, 
            color = ~ie_group,
            colors = ie_colors,
            type = 'bar', 
            orientation = 'h',
            hovertemplate = "<b>%{fullData.name}</b><br>%{y}: %{customdata:.0f} (Relative: %{x:.1f}%)<extra></extra>",
            customdata = ~value) %>%
      layout(
        title = "How Active Are Introverts vs Extroverts on Social Media?",
        xaxis = list(title = "Activity Level (%)"),
        yaxis = list(title = "Activity Type"),
        barmode = 'group'
      )
  })
}

# Run
if (interactive()) {
  shinyApp(ui = ui, server = server)
}