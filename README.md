# MBTI-Twitter-Insights  

This repository explores how different **MBTI personality types** behave on Twitter using real-world social media data.  
It includes both **Data Exploration (DEP)** and **Data Visualisation (DVP)** components, featuring statistical analyses and an interactive R Shiny dashboard.  

---

##  Repository Structure  

```text
MBTI-Twitter-Insights/
│
├── data/ # Dataset storage (not pushed to GitHub)
│ ├── README.md # Instructions for downloading data
│ └── sample_user_tweets.csv # Small sample for demo (optional)
│
├── dep/ # Data Exploration Project
│ ├── DEP_Report.pdf
│ └── DEP.Rmd
│
├── dvp/ # Data Visualisation Project (R Shiny App)
│ ├── app.R
│ └── DVP_Report.pdf
│
├── visuals/ # Exported figures (for reports & README)
│ ├── dep_followers_vs_tweets.png
│ ├── dep_mbti_dimension_comparison.png
│ ├── dep_sentiment_by_mbti.png
│ ├── dep_tweetcount_by_mbti.png
│ ├── dep_user_behavior_by_mbti.png
│ ├── dep_wordcloud_introvert_vs_extrovert.png
│ ├── dvp_activity_funnel.png
│ ├── dvp_dashboard_welcome.png
│ ├── dvp_emoji_usage.png
│ ├── dvp_engagement_patterns.png
│ ├── dvp_interaction_styles.png
│ ├── dvp_mbti_distribution.png
│ └── dvp_sentiment_analysis.png
│
├── README.md # (this file)
└── .gitignore # ignore raw dataset files
```


---

## Dataset  

We use the **Twitter MBTI Personality Types Dataset**:  
- **Source**: [Kaggle – Twitter MBTI Dataset](https://www.kaggle.com/datasets/sanketrai/twitter-mbti-dataset/data)  
- Contains **8,328 Twitter users** with self-reported MBTI types and their tweets.  

⚠️ Due to size limitations, the raw dataset is **not included** in this repository.  
Please download it from Kaggle and place the files into the `data/` folder.  
A small `sample_user_tweets.csv` is provided for demonstration purposes.  

---

## How to Run  

### 1. Data Exploration (DEP)
- Open `dep/DEP.Rmd` in RStudio.  
- Knit or run interactively to reproduce the statistical analysis and plots.  
- Full report available as `dep/DEP_Report.pdf`.  

### 2. Data Visualisation (DVP)
- Navigate to the `dvp/` folder.  
- Launch the interactive dashboard with:  

```R
shiny::runApp("app.R")
```text
UI Layout (MBTI Dashboard)
├── NavbarPage: "MBTI Social Media Dashboard"
│   └── Tab: "Overview"
│       ├── SidebarPanel
│       │   ├── MBTI Type Filter (checkboxGroupInput)
│       │   ├── Quick Stats (textOutput)
│       │   └── Reset Button
│       └── MainPanel
│           ├── Section: MBTI Type Distribution (pie chart)
│           ├── Section: Engagement Patterns (bar charts ×4)
│           ├── Section: Sentiment Analysis (bar chart)
│           ├── TabsetPanel
│           │   ├── Interaction Style (bar chart)
│           │   ├── Emoji Usage (bar chart + selector)
│           │   └── Activity Levels (funnel chart)
│           └── Guides (modal dialogs for each chart)
└── Footer (data source, author, course info)
```
