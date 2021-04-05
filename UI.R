# GOJO CRUZ, JAMLECH IRAM N.
# PROJECT - CMSC150, UPLB

#============================= USER INTERFACE ===========================#

#Packages
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(rhandsontable)

#Shiny App UI
#I used a shiny dashboard/dashboard Plus
shinyUI(
  dashboardPagePlus( skin = "black",
    dashboardHeaderPlus(title = "CS 150"),
    dashboardSidebar(
      sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Quadratic Spline Interpolation", tabName = "qsi", icon = icon("external-link-alt")),
      menuItem("Polynomial Regression", tabName = "pr", icon = icon("dot-circle")),
      menuItem("Simplex Method", tabName = "simplex", icon = icon("donate"))
    )),
    dashboardBody(
      tabItems(
        #Tab for the main menu
        tabItem(tabName = "dashboard",
                fluidRow(tags$img(src = "wallpaper05.png", width = "1600px")),
                br(),
                fluidRow(gradientBox(gradientColor = "purple", footer_padding = FALSE, width = 12, closable = FALSE, collapsible = FALSE)),
                fluidRow(
                  gradientBox(title = h3("What is This?"), gradientColor = "blue", width = 4, 
                              h4("CURVE FITTING"),
                              h5("Information is often provided along a spectrum of distinct values. 
                                 You may need comparisons between the distinct values, though. 
                                 This program uses techniques for obtaining intermediate estimates to 
                                 match curves to such details. You may also need a simplified version 
                                 of a complex function. Another approach to do this is to measure feature 
                                 values along the spectrum of interest at a set of distinct values. 
                                 Then, to fit these values, a simpler function can be derived. 
                                 Both of these applications are referred to as curve fitting.[1]"),
                              h5("There are two general curve fitting methods that are distinct from 
                                 each other based on the amount of error associated with these results. 
                                 First, if these data show a significant degree of error or noise, the 
                                 strategy is to derive a single curve representing the general trend of 
                                 these data. Since any single data point may be inaccurate, we do not make 
                                 any effort to converge that value. Alternatively, the curve is structured 
                                 to follow the pattern taken as a group of points. One method of this kind 
                                 is called least-squares regression. [1]"),
                              h5("Second, where it is known that these data are very accurate, the basic approach 
                                 is to fit a curve or a series of curves that pass directly through each point. 
                                 Usually these data come from tables. Interpolation is the estimation of values 
                                 between well known discrete points. [1]"),
                              br(),
                              h4("OPTIMIZATION"),
                              h5("Optimization involves either the minimum or the maximum being searched. 
                                 The optimum is where the curve is flat. The simplex method for solving 
                                 linear programming problems was discovered by Koopman's student Dantzig in 1947. 
                                 A number of researchers, especially Charnes and his colleagues, paved the way 
                                 for other forms of constrained optimization. [1]"),
                              br(),
                              h6("[1] Chapra, Steven C. Numerical methods for engineers / 
                                  Steven C. Chapra, Berger chair in computing and engineering, 
                                  Tufts University, Raymond P. Canale, professor emeritus of civil engineering, 
                                  University of Michigan. - Seventh edition.")
                  ),
                  gradientBox(title = h3("How to Use"), gradientColor = "purple", width = 4,
                              h4("QUADRATIC SPLINE INTERPOLATION"),
                              h5("Often you will have the ability to infer intermediate values between accurate data points. 
                                 Polynomial interpolation is the most common method used for this purpose. 
                                 This technique calculates the appropriate polynomial of the nth-order 
                                 that matches n+1 data points. This method also includes an intermediate quality 
                                 estimation of the derived equation.[1]"),
                              h5("To efficiently use this method, input a .csv (Comma-Separated Values) file
                                 that contains your data points. Then, enter the x value that you want to estimate.
                                 The program will return the estimated value, the functions generated per interval, and the
                                 correct function for the estimate. A plot is also provided to visualize the functions generated."),
                              br(),
                              h4("POLYNOMIAL REGRESSION"),
                              h5("For such situations, a more suitable approach is to extract an estimated feature 
                                 which fits the information form or general trend without actually matching the 
                                 individual points. To achieve this goal, this technique draws a curve that minimizes 
                                 the difference between the data points and the curve. [1]"),
                              h5("To use this program, you must input a .csv (Comma-Separated Values) file of the data points.
                                 Then, supply the degree of the polynomial and the x value that you want to estimate.
                                 The program will return the estimated value and the derived function, as well as its graph."),
                              br(),
                              h4("SIMPLEX METHOD"),
                              h5("The simplex approach was based on the assumption that an unreasonable point will be the optimal 
                                 solution. Therefore, the method must be able to discern whether an unreasonable point happens 
                                 while problem solving. To do this, by adding what are considered slack variables, the constraint 
                                 equations are reformulated as equalities. [1]"),
                              h5("To use this program, a table is provided to modify the constraints and the objective function.
                                 This program will return the minimum cost, since it is made for a specific minimization problem.")
                  ),
                  gradientBox(title = h3("About Me"), gradientColor = "blue", width = 4,
                              h4("JAMLECH IRAM N. GOJO CRUZ"),
                              tags$img(src = "Iram.png", width = "300px"),
                              h5("Bachelor of Science in Computer Science and a student in
                                 CMSC150: Numerical Methods at the University of the Philippines,
                                 Los Banos, Laguna.")
                  )
                )
                
                #fluidPage(
                  #tags$video(src = "welcome.mp4", type = "video/mp4", width = "560px", height = "315px", autoplay = NA, controls = "controls")
                  #tags$iframe(width = "250", height = "150", url_link="https://www.youtube.com/embed/zIHkPpIgjkk")
                #)
                ),
        #Tab for QSI        
        tabItem(tabName = "qsi",
                h1("Quadratic Spline Interpolation"),
                br(),
                fluidRow(gradientBox(gradientColor = "blue", footer_padding = FALSE, width = 12, closable = FALSE, collapsible = FALSE)),
                fluidRow(
                    valueBoxOutput("x_estimate", width = 4),
                    
                    infoBoxOutput("qsi_function", width = 4),
                    
                    infoBoxOutput("x_interval", width = 4)
                ),
                fluidRow(
                    box(title = "File Input", status = "primary", solidHeader = T, width = 4, height = 200,
                        fileInput("file", "Upload CSV File"),
                        h5("Max file size to upload is 5MB"),
                        checkboxInput("header", "Header")),

                    box(title = "Controls for QSI", status = "info", solidHeader = T, width = 4, height = 200,
                        textInput("xValue", 
                                  "Enter x value you want to estimate.
                                  The value should be between the minimum
                                  and maximum value in the data points provided.")),
                    
                    box(title = "File Summary", status = "primary", solidHeader = T, width = 4, height = 200,
                        tableOutput("input_file"))
                ),
                fluidRow(

                    gradientBox(title = "Functions per Interval", gradientColor = "blue", solidHeader = T, width = 6,
                        plotOutput("plot") ,tableOutput("fxn_interval")),
                    
                    gradientBox(title = "Result", gradientColor = "blue", solidHeader = T, width = 6,
                        verbatimTextOutput("qsi_iterations"))
                )
        ),
        
        #Tab for Polynomial Regression
        tabItem(tabName = "pr",
                h1("Polynomial Regression"),
                br(),
                fluidRow(gradientBox(gradientColor = "yellow", footer_padding = FALSE, width = 12, closable = FALSE, collapsible = FALSE)),
                fluidRow(
                  valueBoxOutput("x_estimate2", width = 4),
                  
                  infoBoxOutput("reg_fxn", width = 8)
                ),
                fluidRow(
                  box(title = "File Input", status = "warning", solidHeader = T, width = 4, height = 500,
                      fileInput("file2", "Upload CSV File"),
                      h5("Max file size to upload is 5MB"),
                      checkboxInput("header", "Header")),
                  
                  box(title = "Controls for QSI", status = "warning", solidHeader = T, width = 4, height = 500,
                      textInput("degree", "Enter the degree of polynomial. Accepted degree values are from 1 to 10 only."), 
                      textInput("xValue2", "Enter x value to estimate.")),
                  
                  box(title = "Plot", status = "warning", solidHeader = T, width = 4, height = 500,
                      plotOutput("plot2"))
                ),
                fluidRow(
                  gradientBox(title = "File Summary", gradientColor = "yellow", solidHeader = T, width = 4,
                      tableOutput("input_file2")),
                  
                  gradientBox(title = "Result", gradientColor = "yellow", solidHeader = T, width = 8,
                      verbatimTextOutput("reg_iterations"))
                )
        ),
        
        #Tab for Simplex Implementation
        tabItem(tabName = "simplex",
                h1("Simplex Implementation"),
                br(),
                fluidRow(gradientBox(gradientColor = "green", footer_padding = FALSE, width = 12, closable = FALSE, collapsible = FALSE)),
                fluidRow(
                
                box(width = 12, 
                    h3("ASSESSING THE VALUE OF SUPPLY CHAIN MANAGEMENT OPTIMIZING SHIPMENTS"),
                    br(),
                    h4("Background of the Study:"), 
                    h5("One of the main products of the Fairway Woods Company is custom-made golf clubs. The
                        clubs are manufactured at three plants (Denver, Colorado; Phoenix, Arizona; and Dallas, Texas) and
                        are then shipped by truck to five distribution warehouses in Sacramento, California; Salt Lake City,
                        Utah; Albuquerque, New Mexico; Chicago, Illinois; and New York City, New York. Because shipping
                        costs are a major expense, management is investigating a way to reduce them. For the upcoming golf
                        season, an estimate has been created as to the total output needed from each manufacturing plant and
                        how each warehouse will require satisfying its customers. The CIO from Fairway Woods Company has
                        created a spreadsheet of the shipping costs from each manufacturing plant to each warehouse as a
                        baseline analysis.")
                )
                ),
                fluidRow(
                box(title = "Supply and Costs", status = "success", solidHeader = T, width = 5, height = 200,
                    rHandsontableOutput('table')),
                
                box(title = "Demands", status = "success", solidHeader = T, width = 2, height = 200,
                    rHandsontableOutput('table2')),
                
                valueBoxOutput("simplex_result")
                ),
                
                fluidRow(
                gradientBox(title = "Result per Iteration", gradientColor = "green", solidHeader = T, width = 7,
                    verbatimTextOutput("simplex_iterations"))
                )
        )
      ) #end of tabItems
    ) #end of dashboard body
  ) #end of dashboardPlus
) #end of UI
