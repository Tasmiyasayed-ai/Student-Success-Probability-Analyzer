# ============================================================
# STUDENT SUCCESS PROBABILITY ANALYZER
# Statistical Modelling for Machine Learning
# Probability-Based Application
# ============================================================

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(DT)

# ============================================================
# 1. CREATE DATASET
# ============================================================

set.seed(123)

n <- 500

student_data <- data.frame(
  Attendance = round(runif(n, 50, 100), 1),
  Study_Hours = round(runif(n, 0.5, 8), 1),
  Previous_Marks = round(runif(n, 35, 95), 1),
  Practice_Tests = sample(0:10, n, replace = TRUE)
)

# Create probability of passing

logit_value <-
  -7 +
  0.045 * student_data$Attendance +
  0.35 * student_data$Study_Hours +
  0.045 * student_data$Previous_Marks +
  0.18 * student_data$Practice_Tests

pass_probability <- 1 / (1 + exp(-logit_value))

# Create Pass / Fail result

student_data$Result <- ifelse(
  runif(n) < pass_probability,
  "Pass",
  "Fail"
)

student_data$Result <- factor(
  student_data$Result,
  levels = c("Fail", "Pass")
)

# ============================================================
# 2. LOGISTIC REGRESSION MODEL
# ============================================================

model <- glm(
  Result ~ Attendance +
    Study_Hours +
    Previous_Marks +
    Practice_Tests,
  data = student_data,
  family = binomial
)

# ============================================================
# 3. UI
# ============================================================

ui <- page_navbar(
  
  title = "🎓 Student Success Analyzer",
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#4F46E5"
  ),
  
  # ----------------------------------------------------------
  # HOME TAB
  # ----------------------------------------------------------
  
  nav_panel(
    "🏠 Home",
    
    div(
      style = "
        padding: 40px;
        text-align: center;
      ",
      
      h1(
        "Student Success Probability Analyzer",
        style = "
          font-weight: bold;
          margin-bottom: 15px;
        "
      ),
      
      h4(
        "A Probability-Based Application using R",
        style = "margin-bottom: 30px;"
      ),
      
      p(
        "This application uses probability and statistical
        modelling to analyze student performance and estimate
        the probability of passing.",
        style = "
          font-size: 18px;
          max-width: 800px;
          margin: auto;
        "
      ),
      
      br(),
      
      layout_columns(
        
        card(
          card_header("🎲 Probability"),
          p(
            "Analyze the probability of passing using
            student performance data."
          )
        ),
        
        card(
          card_header("📊 Statistics"),
          p(
            "Explore attendance, study hours, marks and
            practice tests using graphs."
          )
        ),
        
        card(
          card_header("🤖 Prediction"),
          p(
            "Estimate the probability of passing using
            logistic regression."
          )
        ),
        
        col_widths = c(4, 4, 4)
      )
    )
  ),
  
  # ----------------------------------------------------------
  # PROBABILITY ANALYSIS TAB
  # ----------------------------------------------------------
  
  nav_panel(
    "📊 Probability Analysis",
    
    layout_sidebar(
      
      sidebar = sidebar(
        
        h4("Probability Analysis"),
        
        p(
          "Select an attendance level to calculate
          the observed probability of passing."
        ),
        
        sliderInput(
          "attendance_probability",
          "Minimum Attendance:",
          min = 50,
          max = 100,
          value = 75,
          step = 5
        )
      ),
      
      layout_columns(
        
        card(
          card_header("Overall Pass Probability"),
          uiOutput("overall_probability")
        ),
        
        card(
          card_header("Conditional Pass Probability"),
          uiOutput("conditional_probability")
        ),
        
        col_widths = c(6, 6)
      ),
      
      br(),
      
      card(
        card_header("📈 Pass / Fail Distribution"),
        plotOutput(
          "result_plot",
          height = "400px"
        )
      ),
      
      br(),
      
      card(
        card_header("📊 Attendance vs Previous Marks"),
        plotOutput(
          "attendance_plot",
          height = "450px"
        )
      )
    )
  ),
  
  # ----------------------------------------------------------
  # PREDICTION TAB
  # ----------------------------------------------------------
  
  nav_panel(
    "🔮 Prediction",
    
    layout_sidebar(
      
      sidebar = sidebar(
        
        h4("Enter Student Information"),
        
        sliderInput(
          "attendance",
          "Attendance (%)",
          min = 50,
          max = 100,
          value = 75,
          step = 1
        ),
        
        sliderInput(
          "study_hours",
          "Study Hours per Day",
          min = 0.5,
          max = 8,
          value = 3,
          step = 0.5
        ),
        
        sliderInput(
          "previous_marks",
          "Previous Marks (%)",
          min = 35,
          max = 95,
          value = 60,
          step = 1
        ),
        
        sliderInput(
          "practice_tests",
          "Practice Tests Attempted",
          min = 0,
          max = 10,
          value = 4,
          step = 1
        ),
        
        actionButton(
          "predict",
          "🔍 Analyze Student",
          class = "btn-primary",
          width = "100%"
        )
      ),
      
      h2(
        "Prediction Result",
        style = "font-weight: bold;"
      ),
      
      br(),
      
      layout_columns(
        
        card(
          card_header("🎯 Probability of Passing"),
          uiOutput("pass_probability")
        ),
        
        card(
          card_header("❌ Probability of Not Passing"),
          uiOutput("fail_probability")
        ),
        
        col_widths = c(6, 6)
      ),
      
      br(),
      
      card(
        card_header("📌 Prediction"),
        uiOutput("prediction_result")
      ),
      
      br(),
      
      card(
        card_header("📊 Probability Visualization"),
        plotOutput(
          "prediction_plot",
          height = "400px"
        )
      )
    )
  ),
  
  # ----------------------------------------------------------
  # DATA TAB
  # ----------------------------------------------------------
  
  nav_panel(
    "📋 Dataset",
    
    div(
      style = "padding: 25px;",
      
      h2(
        "Student Performance Dataset",
        style = "font-weight: bold;"
      ),
      
      p(
        "The dataset contains 500 simulated student
        observations used for probability analysis
        and statistical modelling."
      ),
      
      br(),
      
      DTOutput("student_table")
    )
  ),
  
  # ----------------------------------------------------------
  # MODEL TAB
  # ----------------------------------------------------------
  
  nav_panel(
    "🤖 Statistical Model",
    
    div(
      style = "padding: 30px;",
      
      h2(
        "Logistic Regression Model",
        style = "font-weight: bold;"
      ),
      
      p(
        "Logistic regression is used because the outcome
        has two categories: Pass and Fail."
      ),
      
      br(),
      
      card(
        card_header("Model Formula"),
        
        div(
          style = "
            padding: 20px;
            font-size: 18px;
          ",
          
          code(
            "Result ~ Attendance + Study_Hours +
            Previous_Marks + Practice_Tests"
          )
        )
      ),
      
      br(),
      
      card(
        card_header("Model Summary"),
        
        verbatimTextOutput(
          "model_summary"
        )
      )
    )
  ),
  
  # ----------------------------------------------------------
  # ABOUT TAB
  # ----------------------------------------------------------
  
  nav_panel(
    "ℹ️ About",
    
    div(
      style = "
        padding: 40px;
        max-width: 900px;
        margin: auto;
      ",
      
      h2(
        "About the Project",
        style = "font-weight: bold;"
      ),
      
      br(),
      
      h4("Project Topic"),
      
      p(
        "Probability-Based Applications"
      ),
      
      h4("Subject"),
      
      p(
        "Statistical Modelling for Machine Learning"
      ),
      
      h4("Programming Language"),
      
      p(
        "R"
      ),
      
      h4("Software"),
      
      p(
        "RStudio"
      ),
      
      h4("Concepts Used"),
      
      tags$ul(
        tags$li("Basic Probability"),
        tags$li("Conditional Probability"),
        tags$li("Data Analysis"),
        tags$li("Data Visualization"),
        tags$li("Logistic Regression"),
        tags$li("Statistical Modelling"),
        tags$li("R Shiny")
      ),
      
      br(),
      
      h4("Project Objective"),
      
      p(
        "The objective of this project is to demonstrate
        how probability and statistical modelling can be
        applied to student performance data to estimate
        the probability of passing."
      )
    )
  )
)

# ============================================================
# 4. SERVER
# ============================================================

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # Overall probability
  # ----------------------------------------------------------
  
  output$overall_probability <- renderUI({
    
    probability <- mean(
      student_data$Result == "Pass"
    )
    
    div(
      style = "
        text-align: center;
        padding: 25px;
      ",
      
      h1(
        paste0(
          round(probability * 100, 1),
          "%"
        ),
        style = "
          font-weight: bold;
          color: #4F46E5;
        "
      ),
      
      p(
        "Observed proportion of students who passed."
      )
    )
  })
  
  # ----------------------------------------------------------
  # Conditional probability
  # ----------------------------------------------------------
  
  output$conditional_probability <- renderUI({
    
    selected_students <- student_data[
      student_data$Attendance >=
        input$attendance_probability,
    ]
    
    if (nrow(selected_students) == 0) {
      
      probability <- 0
      
    } else {
      
      probability <- mean(
        selected_students$Result == "Pass"
      )
    }
    
    div(
      style = "
        text-align: center;
        padding: 25px;
      ",
      
      h1(
        paste0(
          round(probability * 100, 1),
          "%"
        ),
        style = "
          font-weight: bold;
          color: #16A34A;
        "
      ),
      
      p(
        paste0(
          "Observed pass proportion among students
          with attendance ≥ ",
          input$attendance_probability,
          "%."
        )
      )
    )
  })
  
  # ----------------------------------------------------------
  # Pass / Fail graph
  # ----------------------------------------------------------
  
  output$result_plot <- renderPlot({
    
    ggplot(
      student_data,
      aes(
        x = Result,
        fill = Result
      )
    ) +
      
      geom_bar(
        width = 0.6
      ) +
      
      labs(
        title = "Student Pass and Fail Distribution",
        x = "Result",
        y = "Number of Students"
      ) +
      
      theme_minimal(
        base_size = 15
      ) +
      
      theme(
        plot.title = element_text(
          face = "bold",
          size = 20
        ),
        legend.position = "none"
      )
  })
  
  # ----------------------------------------------------------
  # Attendance vs Marks graph
  # ----------------------------------------------------------
  
  output$attendance_plot <- renderPlot({
    
    ggplot(
      student_data,
      aes(
        x = Attendance,
        y = Previous_Marks,
        color = Result
      )
    ) +
      
      geom_point(
        size = 3,
        alpha = 0.7
      ) +
      
      labs(
        title = "Attendance vs Previous Marks",
        x = "Attendance (%)",
        y = "Previous Marks (%)",
        color = "Result"
      ) +
      
      theme_minimal(
        base_size = 15
      ) +
      
      theme(
        plot.title = element_text(
          face = "bold",
          size = 20
        )
      )
  })
  
  # ----------------------------------------------------------
  # Prediction
  # ----------------------------------------------------------
  
  prediction <- eventReactive(
    input$predict,
    {
      
      new_student <- data.frame(
        
        Attendance =
          input$attendance,
        
        Study_Hours =
          input$study_hours,
        
        Previous_Marks =
          input$previous_marks,
        
        Practice_Tests =
          input$practice_tests
      )
      
      predict(
        model,
        newdata = new_student,
        type = "response"
      )
    }
  )
  
  # ----------------------------------------------------------
  # Pass probability
  # ----------------------------------------------------------
  
  output$pass_probability <- renderUI({
    
    req(prediction())
    
    probability <- prediction()
    
    div(
      style = "
        text-align: center;
        padding: 30px;
      ",
      
      h1(
        paste0(
          round(probability * 100, 1),
          "%"
        ),
        style = "
          font-weight: bold;
          color: #16A34A;
          font-size: 50px;
        "
      ),
      
      p(
        "Estimated probability of passing"
      )
    )
  })
  
  # ----------------------------------------------------------
  # Fail probability
  # ----------------------------------------------------------
  
  output$fail_probability <- renderUI({
    
    req(prediction())
    
    probability <- 1 - prediction()
    
    div(
      style = "
        text-align: center;
        padding: 30px;
      ",
      
      h1(
        paste0(
          round(probability * 100, 1),
          "%"
        ),
        style = "
          font-weight: bold;
          color: #DC2626;
          font-size: 50px;
        "
      ),
      
      p(
        "Estimated probability of not passing"
      )
    )
  })
  
  # ----------------------------------------------------------
  # Prediction result
  # ----------------------------------------------------------
  
  output$prediction_result <- renderUI({
    
    req(prediction())
    
    probability <- prediction()
    
    if (probability >= 0.5) {
      
      div(
        style = "
          text-align: center;
          padding: 30px;
        ",
        
        h2(
          "🟢 LIKELY TO PASS",
          style = "
            font-weight: bold;
            color: #16A34A;
          "
        ),
        
        p(
          "The model estimates a probability
          of passing above 50%."
        )
      )
      
    } else {
      
      div(
        style = "
          text-align: center;
          padding: 30px;
        ",
        
        h2(
          "🔴 LIKELY NOT TO PASS",
          style = "
            font-weight: bold;
            color: #DC2626;
          "
        ),
        
        p(
          "The model estimates a probability
          of passing below 50%."
        )
      )
    }
  })
  
  # ----------------------------------------------------------
  # Prediction graph
  # ----------------------------------------------------------
  
  output$prediction_plot <- renderPlot({
    
    req(prediction())
    
    probabilities <- data.frame(
      
      Outcome = c(
        "Pass",
        "Not Pass"
      ),
      
      Probability = c(
        prediction(),
        1 - prediction()
      )
    )
    
    ggplot(
      probabilities,
      aes(
        x = Outcome,
        y = Probability,
        fill = Outcome
      )
    ) +
      
      geom_col(
        width = 0.6
      ) +
      
      geom_text(
        aes(
          label = paste0(
            round(
              Probability * 100,
              1
            ),
            "%"
          )
        ),
        vjust = -0.5,
        size = 6
      ) +
      
      scale_y_continuous(
        limits = c(0, 1),
        labels = function(x) {
          paste0(x * 100, "%")
        }
      ) +
      
      labs(
        title = "Predicted Probability",
        x = "",
        y = "Probability"
      ) +
      
      theme_minimal(
        base_size = 15
      ) +
      
      theme(
        plot.title = element_text(
          face = "bold",
          size = 20
        ),
        legend.position = "none"
      )
  })
  
  # ----------------------------------------------------------
  # Dataset table
  # ----------------------------------------------------------
  
  output$student_table <- renderDT({
    
    datatable(
      student_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # ----------------------------------------------------------
  # Model summary
  # ----------------------------------------------------------
  
  output$model_summary <- renderPrint({
    
    summary(model)
    
  })
}

# ============================================================
# 5. RUN APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)