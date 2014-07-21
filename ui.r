library(shiny)

shinyUI(fluidPage(
  
  title = "Entertainment Expenses Analysis",

 mainPanel(width = 12,
  headerPanel("Entertainment Expenses Analysis")),

  fluidRow(
    # area where to plot the bubble chart
column(12,
       h5("Bubble Chart"),
       plotOutput("gcompl70"),
       h6("Note"),
       p("X axis represent clusters of expenses"),
       p("Y axis represent the total amount of euros paid by the Company to the subject"),
       p("point area is proportional to the percentage of euros paid without passing throug the Account Receivable process")
)),
  fluidRow(
    #sliderinput for selecting year range
    column(6,
           
           sliderInput("Anno", "Analysis period",  2005,  max = 2014, value = c(2009,2014))          
    ),
    #sliderinput for selecting range in euros
    column(6,
           sliderInput("limiti",  "Euros Range:",  min = 0,   max = 400000, value = c(1000,100000))       
  ))
))

