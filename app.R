library(shiny)
library(lpSolve)
library(dplyr)
library(scales)

###########################
ui <- fluidPage(
  sidebarLayout(
  
  sidebarPanel(
numericInput("max.total","Maximum Expenditure $",150000000, max=200000000, min=100000000 ),
sliderInput("labour.fra","Labour Budget as % of Max", value=0.85, min=0, max=1),
sliderInput("disc","Discount Rate",value=0.05, min=0, max=1),
selectInput("max.yrs","Time Horizon, Years", selected = 3, c("Choose one" = "", 1:3)),
numericInput("CET","Cost Effectiveness Threshold",200),
radioButtons("eff","Include Effectiveness Interventions?", choiceNames= c("Yes","No"), choiceValues=c(1,0), inline=TRUE),
radioButtons("cap","Include Capacity Interventions?", choiceNames= c("Yes","No"), choiceValues=c(1,0), inline=TRUE),
radioButtons("new","Include New Platform Interventions?",  choiceNames= c("Yes","No"), choiceValues=c(1,0), inline=TRUE))
,

mainPanel(
  htmlOutput("net.benefit"),
  tableOutput("solution"),
)))

###########################
server <- function(input, output) {

output$net.benefit = renderText({
  
mydata = read.csv("2020 10 07 - LP Input.csv",header = TRUE)
max.labour=input$max.total*input$labour.fra
mydata<-suppressMessages(mydata %>% filter(Eff <= input$eff))
mydata<-suppressMessages(mydata %>% filter(Cap <= input$cap))
mydata<-suppressMessages(mydata %>% filter(New <= input$new))
rows = nrow(mydata)
    
TC.All=TB.All=LC.All=rep(0,rows)
for(i in 1:input$max.yrs) {                   
  assign(paste0("TC.Yr", i),(eval(parse(text=paste0("mydata$VC.Yr", i)))+eval(parse(text=paste0("mydata$FC.Yr", i))))/(1+input$disc)^(i-1))
  assign(paste0("LC.Yr", i),(mydata$Labour.Portion*eval(parse(text=paste0("mydata$VC.Yr", i)))+eval(parse(text=paste0("mydata$FC.Yr", i))))/(1+input$disc)^(i-1))
  assign(paste0("TB.Yr", i),(mydata$Y*eval(parse(text=paste0("mydata$B.Yr", i)))*eval(parse(text=paste0("mydata$N.Yr", i))))/(1+input$disc)^(i-1))
  TC.All = TC.All + (eval(parse(text=paste0("TC.Yr", i))))
  LC.All = LC.All + (eval(parse(text=paste0("LC.Yr", i))))
  TB.All = TB.All + (eval(parse(text=paste0("TB.Yr", i))))}
    
TC.All = TC.All+mydata$D.New+mydata$D.Eff+mydata$D.Cap
NB.All = (TB.All*input$CET)-TC.All
total.cost = tibble(id=1:rows,total.cost=TC.All)
labour.cost = tibble(id=1:rows,total.cost=LC.All)

f.rhs <- c(1,1,1,1,1,1,1,max.labour,input$max.total)
f.dir <- c("<=","<=","<=","<=","<=","<=","<=","<=","<=")
f.obj <- TB.All 
f.cons <- rbind(t(mydata[c(2:8)]),LC.All,TC.All)
    
linearprogram<- lp("max", f.obj, f.cons, f.dir, f.rhs, all.bin=TRUE)
sol<- linearprogram$solution
sol1<-tibble(id=1:rows, x.vector=sol)
names<-tibble(id=1:rows, names=mydata[,1])

solution<-merge(names, sol1)
solution1<-merge(solution,total.cost)
solution2<-merge(solution1,net.b)
solution2<-suppressMessages(solution2 %>% filter(x.vector>0))
net.benefit = sum(solution2$net.b)
final.cost = sum(solution2$total.cost)

paste("<b>","Net Benefit","</b>",dollar(net.benefit),"<br>","<b>","Total Cost","</b>",dollar(final.cost),"<br>")

})

output$solution = renderTable({
  mydata = read.csv("2020 10 07 - LP Input.csv",header = TRUE)
  max.labour=input$max.total*input$labour.fra
  mydata<-suppressMessages(mydata %>% filter(Eff <= input$eff))
  mydata<-suppressMessages(mydata %>% filter(Cap <= input$cap))
  mydata<-suppressMessages(mydata %>% filter(New <= input$new))
  rows = nrow(mydata)
  
  TC.All=TB.All=LC.All=rep(0,rows)
  for(i in 1:input$max.yrs) {                   
    assign(paste0("TC.Yr", i),(eval(parse(text=paste0("mydata$VC.Yr", i)))+eval(parse(text=paste0("mydata$FC.Yr", i))))/(1+input$disc)^(i-1))
    assign(paste0("LC.Yr", i),(mydata$Labour.Portion*eval(parse(text=paste0("mydata$VC.Yr", i)))+eval(parse(text=paste0("mydata$FC.Yr", i))))/(1+input$disc)^(i-1))
    assign(paste0("TB.Yr", i),(mydata$Y*eval(parse(text=paste0("mydata$B.Yr", i)))*eval(parse(text=paste0("mydata$N.Yr", i))))/(1+input$disc)^(i-1))
    TC.All = TC.All + (eval(parse(text=paste0("TC.Yr", i))))
    LC.All = LC.All + (eval(parse(text=paste0("LC.Yr", i))))
    TB.All = TB.All + (eval(parse(text=paste0("TB.Yr", i))))}
  
  TC.All = TC.All+mydata$D.New+mydata$D.Eff+mydata$D.Cap
  NB.All = (TB.All*input$CET)-TC.All
  total.cost = tibble(id=1:rows,total.cost=TC.All)
  labour.cost = tibble(id=1:rows,total.cost=LC.All)
  
  f.rhs <- c(1,1,1,1,1,1,1,max.labour,input$max.total)
  f.dir <- c("<=","<=","<=","<=","<=","<=","<=","<=","<=")
  f.obj <- TB.All 
  f.cons <- rbind(t(mydata[c(2:8)]),LC.All,TC.All)
  
  linearprogram<- lp("max", f.obj, f.cons, f.dir, f.rhs, all.bin=TRUE)
 sol<-linearprogram$solution
  sol1<-tibble(id=1:rows, x.vector=sol)
  names<-tibble(id=1:rows, Option=mydata[,1])
  
  solution<-merge(names, sol1)
  solution$Choice<- ifelse(solution$x.vector ==1, "Fund", " ")
  solution = subset(solution, select = c(2,4))
  solution})
}

shinyApp(ui = ui, server = server)

