# GOJO CRUZ, JAMLECH IRAM N.
# PROJECT - CMSC150, UPLB

#============================= SERVER ===========================#

#Packages Needed
library(shiny)
library(shinydashboard)
source("QSI.R", local = TRUE)
source("Regression.R", local = TRUE)
source("SimplexV2.R", local = TRUE)

#Shiny App Server
shinyServer(function(input, output, session){

  #for Simplex problem; initialized data fram derived from the original problem
  eq1 = c(1,0,0,1,0,0,1,0,0,1,0,0,1,0,0, 1,0,0,0,0,0,0,0,0, 310)
  eq2 = c(0,1,0,0,1,0,0,1,0,0,1,0,0,1,0, 0,1,0,0,0,0,0,0,0, 260)
  eq3 = c(0,0,1,0,0,1,0,0,1,0,0,1,0,0,1, 0,0,1,0,0,0,0,0,0, 280)
  eq4 = c(-1,-1,-1,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,1,0,0,0,0,0, -180)
  eq5 = c(0,0,0,-1,-1,-1,0,0,0,0,0,0,0,0,0, 0,0,0,0,1,0,0,0,0, -80)
  eq6 = c(0,0,0,0,0,0,-1,-1,-1,0,0,0,0,0,0, 0,0,0,0,0,1,0,0,0, -200)
  eq7 = c(0,0,0,0,0,0,0,0,0,-1,-1,-1,0,0,0, 0,0,0,0,0,0,1,0,0, -160)
  eq8 = c(0,0,0,0,0,0,0,0,0,0,0,0,-1,-1,-1, 0,0,0,0,0,0,0,1,0, -220)
  eq9 = c(-10,-6,-3, -8,-5,-4, -6,-4,-5, -5,-3,-5, -4,-6,-9, 0,0,0,0,0,0,0,0,1, 0)
  mat = CreateTableu(eq1, eq2, eq3, eq4, eq5, eq6, eq7, eq8, eq9)
  
  Demands = c(180,80,200,160,220)
  sacramento = c(10,6,3)
  salt_lake = c(8,5,4)
  chicago = c(6,4,5)
  albequerque = c(5,3,5)
  new_york = c(4,6,9)
  supply = c(310,260,280)
  df = data.frame(supply,sacramento,salt_lake,chicago,albequerque,new_york)
  colnames(df) = c("Supply on Plants", "Sacramento", "Salt Lake", "Albuquerque", "Chicago", "New York")
  row.names(df) = c("Denver", "Phoenix", "Dallas")
  df2 = data.frame(Demands)
  row.names(df2) = c("Sacramento", "Salt Lake", "Albuquerque", "Chicago", "New York")

  
  
  # ========================= QUADRATIC SPLINE INTERPOLATION ========================= # 
  
  #read data from csv file
  data <- reactive({
    file_to_read = input$file
    if(is.null(file_to_read)){
      return()
    }
    read.table(file_to_read$datapath, sep = ',', header = input$header, fileEncoding="UTF-8-BOM")
  })
  
  #create matrix from the file
  mat_data <- reactive({
    df = data()
    if(is.null(df)) return(NULL)
    xcol = data()$V1; ycol = data()$V2; len = length(xcol)
    mat = matrix(c(xcol, ycol), nrow = len, ncol=2, byrow = FALSE)
    mat = mat[order(mat[,1]), ]
    mat
  })
  
  #get the x value from user input
  x_value <- reactive({
    if(is.null(input$xValue)) return(NULL)
    inpXVal <- as.numeric(input$xValue)
    mat = mat_data()
    if(inpXVal < mat[1,1] || inpXVal > mat[nrow(mat),1]) return(NULL)
    inpXVal
  })
  
  #output the summary of the file
  output$input_file <- renderTable({
    data()
  })
  
  #output the QSI result
  qsi_result <- reactive({
    tryCatch({
      df = data()
      if(is.null(df)) return(NULL)
      mat = mat_data(); x = x_value()
      if(is.null(mat) || is.null(x)) return(NULL)
      QSI(mat, x, FALSE)
    },
    error = function(err) {
      return(NULL)
    })
  })
  
  #shows the x estimate in a value box
  output$x_estimate <- renderValueBox({
    e = qsi_result()
    
    if(is.null(e)){
      valueBox(0, "X Value Estimate", color = "aqua")
    }
    else{
      e = e$estimate
      e = format(round(e, 4), nsmall = 4)
      valueBox(e, "X Value Estimate", icon = icon("chart-line"), color = "aqua")
    }
  })
  
  #shows the function for correct interval
  output$qsi_function <- renderInfoBox({
    e = qsi_result()
    
    if(is.null(e)){
      infoBox("Function", "", color = "light-blue")
    }
    else{
      e = e$fxn_string
      infoBox("Function", e, color = "light-blue")
    }
  })
  
  #shows the interval where x lies
  output$x_interval <- renderInfoBox({
    e = qsi_result()
    
    if(is.null(e)){
      infoBox("Interval", "", color = "aqua")
    }
    else{
      e = e$x_interval
      infoBox("Interval", e, color = "aqua")
    }
  })
  
  #shows the functions per interval
  output$fxn_interval <- renderTable({
    f = qsi_result()$intervals
    if(is.null(f)) return(NULL)
    f
  })
  
  #prints the matrix for every iterations
  output$qsi_iterations <- renderPrint({
    res_table = qsi_result()
    if(is.null(res_table)) return(NULL)
    res_table$mat_iter
  })
  
  #plots the given data points and the derived functions
  output$plot <- renderPlot({
    mat = mat_data(); e = qsi_result()
    if(is.null(mat) || is.null(e)) return(NULL)
    x = mat[,1]; y = mat[,2]; fxn = e$fxn
    plot(x, y, type="p", pch=19, cex = 2, col="red", main = "Quadratic Spline Interpolation", xlab = "X's", ylab = "Y's")
    legend("bottomleft", c("f(x) estimate","f(x) interval"), fill=c("blue","purple"))
    for(eqs in e$intervals){
      tmpfxn = eval(parse(text=eqs))
      curve(tmpfxn, col="purple", add=TRUE)
    }
    curve(fxn, col="blue", add=TRUE)
  })
  
  
  
  #============================= POLYNOMIAL REGRESSION ===========================#
  
  #read data from csv file
  data2 <- reactive({
    file_to_read = input$file2
    if(is.null(file_to_read)){
      return(NULL)
    }
    read.table(file_to_read$datapath, sep = ',', header = input$header, fileEncoding="UTF-8-BOM")
  })
  
  #accepts the input for degree
  degree_reg <- reactive({
    if(is.null(input$degree)) return(NULL)
    inpDeg <- as.numeric(input$degree)
    if(inpDeg < 1 || inpDeg > 10) return(NULL)
    inpDeg
  })
  
  #accepts the input for the x value to estimate
  x_value2 <- reactive({
    inp = input$xValue2
    if(is.null(inp)) return(NULL)
    inpXVal <- as.numeric(input$xValue2)
    inpXVal
  })
  
  #shows the summary of the csv file
  output$input_file2 <- renderTable({
    d = data2()
    if(is.null(d)) return(NULL)
    d
  })
  
  #result of polynomial regression
  reg_result <- reactive({
    tryCatch({
      df = data2()
      if(is.null(df)) return(NULL)
      xcol = data2()$V1; ycol = data2()$V2; len = length(xcol)
      mat = matrix(c(xcol, ycol), nrow = len, ncol=2, byrow = FALSE)
      mat = mat[order(mat[,1]), ] #sorting the data points
      d = degree_reg(); x = x_value2()
      if(is.null(d) || is.null(x)) return(NULL)
      PolynomialRegression(mat[,1], mat[,2], d, x)
    },
    error = function(err) {
      return(NULL)
    })
  })
  
  #shows the estimated value
  output$x_estimate2 <- renderValueBox({
    e = reg_result()
    
    if(is.null(e)){
      valueBox(0, "X Value Estimate", icon = icon("chart-line"), color = "orange")
    }
    else{
      e = e$estimate
      e = format(round(e, 4), nsmall = 4)
      valueBox(e, "X Value Estimate", icon = icon("chart-line"), color = "orange")
    }
  })
  
  #shows the function generated
  output$reg_fxn <- renderInfoBox({
    e = reg_result()
    
    if(is.null(e)){
      infoBox("Function", "", color = "orange")
    }
    else{
      e = e$fxn_string
      infoBox("Function", e, color = "orange")
    }
  })
  
  #prints the matrix for every iterations
  output$reg_iterations <- renderPrint({
    res_table = reg_result()
    if(is.null(res_table)) return(NULL)
    res_table$mat_iter
  })
  
  #plots the data points and the function
  output$plot2 <- renderPlot({
    df = data2()
    e = reg_result()
    if(is.null(df) || is.null(e)) return(NULL)
    x = data2()$V1; y = data2()$V2; fxn = e$function1
    plot(x, y, type="p", pch=19, cex = 2, col="red", main = "Regression", xlab = "X's", ylab = "Y's")
    legend("topleft", c("f(x) estimate"), fill=c("blue"))
    curve(fxn, col="blue", add=TRUE)
  })
  
  
  
  
  #============================= SIMPLEX METHOD ===========================#
  # ----------------------------- Dual Simplex --------------------------- #
  
  #this will ensure that the changed values from the table is changed globally
  datavalues <- reactiveValues(data=mat)
  
  #outputs the table for the supply and the costs
  output$table <- renderRHandsontable({
    rhandsontable(df)
  })
  
  #outputs the table for the demands
  output$table2 <- renderRHandsontable({
    rhandsontable(df2)
  })
  
  #computation for simplex implementation
  simplex <- reactive({
    tryCatch({
      tab = datavalues$data
      result = SimplexMethod(tab, TRUE)
      result
    },
    error = function(err) {
      return(NULL)
    })
  })
  
  #minimum cost
  min_cost <- reactive({
    min = simplex()
    min$fvalue
  })
  
  #shows the minimum cost computed
  output$simplex_result <- renderValueBox({
    e = min_cost()
    
    if(is.null(e)){
      valueBox(0, "Minimum Cost", color = "olive")
    }
    else{
      valueBox(e, "Minimum Cost", icon = icon("chart-line"), color = "olive")
    }
  })
  
  #prints the matrix for every iteration
  output$simplex_iterations <- renderPrint({
    res_table = simplex()
    if(is.null(res_table)) return("No Feasible Solution")
    res_table
  })
  
  #this function is used to detect the cell where the value is modified and updates the current matrix/data frame
  observeEvent(
    input$table$changes$changes,
    {
      xi = input$table$changes$changes[[1]][[1]] #row
      yi = input$table$changes$changes[[1]][[2]] #column
      old = input$table$changes$changes[[1]][[3]] #old value
      new = input$table$changes$changes[[1]][[4]] #new/modified value
      df = hot_to_r(input$table) #change the values to r objects
      
      #corresponding rows and columns in the auqmented coefficient matrix
      if(yi+1 == 1) datavalues$data[xi+1,25] = new
      if(yi+1 == 2) datavalues$data[9,xi+1] = -new
      if(yi+1 == 3) datavalues$data[9,xi+4] = -new
      if(yi+1 == 4) datavalues$data[9,xi+7] = -new
      if(yi+1 == 5) datavalues$data[9,xi+10] = -new
      if(yi+1 == 6) datavalues$data[9,xi+13] = -new
        
      #just to check the changes that is currently happening
      output$changeinfo <- renderPrint({
        list(paste("Row index of cell which is changed", xi),
             paste("Column index of cell which is changed", yi),
             paste("Old value of the cell", old),
             paste("New value of the cell", new))
      })
      print(datavalues$data)
    }
  )
  
  #same as the one above, but for the demands table
  observeEvent(
    input$table2$changes$changes,
    {
      xi2 = input$table2$changes$changes[[1]][[1]]
      old2 = input$table2$changes$changes[[1]][[3]]
      new2 = input$table2$changes$changes[[1]][[4]]
      
      output$changeinfo <- renderPrint({
        list(
          paste("Column index of cell which is changed 2", xi2),
          paste("Old value of the cell 2", old2),
          paste("New value of the cell 2", new2))
      })
      
      datavalues$data[xi2+4,25] = -new2
      print(datavalues$data)
    }
  )
  
}) #end of server