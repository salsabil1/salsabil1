## app.R ##
library(shiny)
library(shinydashboard)
require(shinydashboard)
library(lattice)
library(Matrix)
library(ggplot2)
library(plotrix)
library(psych)
library(survival)
library(caret)
library(rpart)
library(Formula)
library(base)
library(Hmisc)
library(stats)
library(randomForest)
library(ROCR)
library(readxl)
library(readxl)
german<- read_excel("german credit dataset (1).xls", skip = 1)
base<-german
attach(base)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Analyse Univariee", tabName = 'au',icon = icon("binoculars")),
      menuItem("Analyse Bivariee",tabName = "ab",icon = icon("th")) ,
      menuItem("Modelisation",tabName = "Mod",icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "au",
              fluidRow(box(title = "Variable",collapsible = T,status = "primary",width = 3,
                           selectInput("var","Choisir la variable",choices = colnames(german),selected = "V01")),
                       box("Representation des variables",collapsible = T,status="primary",width = 9,
                           plotOutput("graph"))
              )
              
      ),
      tabItem(tabName = "ab",
              fluidRow(box(title = "Representation des variables selon la variable Good",collapsible = T,status = "primary",width = 12,
                           verbatimTextOutput("res")))
              
      ),
      tabItem(tabName = "Mod",
              
              
              tabsetPanel(
                tabPanel("Modele",
                         fluidRow(box(title = "",status = "primary",width = 3,
                                      selectInput("mod","Choisir le modele",choices = c("GLM","forward","backward","both","arbre de decision","Random Forest"),selected = "GLM")
                         ),
                         
                         box(title = "Modele",status = "primary",verbatimTextOutput("resM"))
                         
                         
                         )
                ),
                tabPanel("Courbe ROC",box(title = "Courbe ROC",status = "primary", plotOutput("roc"),width=8)
                )
              )
      )
    )))
server <- function(input, output) { 
  output$graph <- renderPlot({
    if(input$var=="V01"){
      table_V1<-table(V01)
      df <- data.frame(
        group = c("1", "2", "3","4"),
        value = c(274, 269, 63,394)
      )
      head(df)
      # Bar plot
      bp<- ggplot(df, aes(x="", y=value, fill=group))+
        geom_bar(width = 1, stat = "identity")
      pie <- bp + coord_polar("y", start=0)
      pie + scale_fill_brewer(palette="Blues")+
        theme_minimal() 
    }else if(input$var=="V02"){
      boxplot(V02, data=german, col = c("yellow"),
              main = paste("Boxplot pour la variable V2"),
              ylab = "Quantiles")
    }else if(input$var=="V03"){
      
      slices <- c(40, 49, 530, 88, 293) 
      lbls <- c("group 0", "group 1", "group 2", "group 3", "group 4")
      pct <- round(slices/sum(slices)*100)
      lbls <- paste(lbls, pct) # add percents to labels 
      lbls <- paste(lbls,"%",sep="") # ad % to labels 
      pie3D(slices,labels=lbls,explode=0.2,
            main="Pie Chart of Countries ")
      #####################V4 but de cr?dit 
      
    }else if(input$var=="V04"){
      #####################V4 but de credit 
      #effectif
      table_V4<-table(V04)
      #Barplot
      barplot(table_V4, main="Repartion des clients par rapport au but de credit ", 
              col="blue")
      
    }else if(input$var=="V05"){
      boxplot(V05, data=german, col = c("yellow"),
              main = paste("Boxplot pour la variable V5"),
              ylab = "le montant de credit")  
      
    }else if(input$var=="V06"){
      slices <- c(603, 103, 63, 48, 183) 
      lbls <- c("1", "2", "3", "4", "5")
      pct <- round(slices/sum(slices)*100)
      lbls <- paste(lbls, pct) # add percents to labels 
      lbls <- paste(lbls,"%",sep="") # ad % to labels 
      pie3D(slices,labels=lbls,explode=0.1,
            main="Pie Chart of Savings account ")  
      
    }else if(input$var=="V07"){
      table_V7<-table(V07)
      barplot(table_V7, main="Repartion des clients selon anciennete au travail ", 
              col="blue")
      
    }else if(input$var=="V08"){
      boxplot(V08, data=german, col = c("yellow"),
              main = paste("Boxplot pour la variable V8"),
              ylab = "Le taux de versement du revenu disponible en %")
      
    }else if(input$var=="V09"){
      slices <- c(50, 310, 548, 92) 
      lbls <- c("A91", "A92", "A93", "A94")
      pct <- round(slices/sum(slices)*100)
      lbls <- paste(lbls, pct) # add percents to labels 
      lbls <- paste(lbls,"%",sep="") # ad % to labels 
      pie3D(slices,labels=lbls,explode=0.1,
            main="Pie Chart of Personal status and sex ")
      
    }else if(input$var=="V10"){
      dp <- data.frame(
        group = c("1", "2", "3","4"),
        value = c(0.05, 0.31,0.54,0.09)
      )
      bp<- ggplot(dp, aes(x="", y=value, fill=group))+
        geom_bar(width = 1, stat = "identity")
      
      pie <- bp + coord_polar("y", start=0)
      
      
      pie + scale_fill_brewer(palette="Blues")+
        theme_minimal()
      
    } else if(input$var=="V11"){
      hist(V11, 
           col = c("orange"),
           main = paste("Histogramme pour la variable V11"),
           ylab = "Effectifs",
           xlab = "Le taux de versement du revenu disponible en %")
      
      
    }else if(input$var=="V12"){
      table_V12<-table(V12)
      barplot(table_V12, main="Repartion des clients par rapport au statut matrimoniale ", 
              col="blue")
      
    }else if(input$var=="V13"){
      boxplot(V13, data=german, col = c("yellow"),
              main = paste("Boxplot pour la variable V13"),
              ylab = "L'age ")  
      
    }else if(input$var=="V14"){
      table_V14<-table(V14)
      barplot(table_V14, main="Repartion des clients par rapport au statut matrimoniale ", 
              col="blue")
      
    }else if(input$var=="V15"){
      table_V15<-table(V15)  
      barplot(table_V15, main="R?partion des clients par rapport au statut matrimoniale ", 
              col="blue")
    }else if(input$var=="V16"){
      boxplot(V16, data=german, col = c("yellow"),
              main = paste("Boxplot pour la variable V16"),
              ylab = "Le taux de versement du revenu disponible en %")
      
    }else if(input$var=="V17"){
      slices <- c(22, 200, 630, 148) 
      lbls <- c("A171", "A172", "A173", "A174")
      pct <- round(slices/sum(slices)*100)
      lbls <- paste(lbls, pct) # add percents to labels 
      lbls <- paste(lbls,"%",sep="") # ad % to labels 
      pie3D(slices,labels=lbls,explode=0.1,
            main="Pie Chart of job")  
      
    }else if(input$var=="V18"){
      slices <- c(845, 155) 
      lbls <- c("1", "2")
      pct <- round(slices/sum(slices)*100)
      lbls <- paste(lbls, pct) # add percents to labels 
      lbls <- paste(lbls,"%",sep="") # ad % to labels 
      
      pie3D(slices,labels=lbls,explode=0.1,
            main="Pie Chart of Number of people being liable to provide maintenance")
    }else if(input$var=="V19"){
      table_V19<-table(V19)
      barplot(table_V19, main="Repartion des clients par rapport au statut matrimoniale ", 
              col="blue")
      
    }else if(input$var=="V20"){
      table_V20<-table(V20)
      barplot(table_V20, main="Repartion des clients par rapport au statut matrimoniale ", 
              col="blue")
      
    }else{
      slices <- c(700, 300) 
      lbls <- c("Good", "bad")
      pct <- round(slices/sum(slices)*100)
      lbls <- paste(lbls, pct) # add percents to labels 
      lbls <- paste(lbls,"%",sep="") # ad % to labels 
      
      pie3D(slices,labels=lbls,explode=0.1)
      
    }
    
    
    
  })
  
  output$res <-renderPrint({
    
    describeBy(german, group=V21)
  })
  output$resM <-renderPrint({
    german$V21 = replace(german$V21,which(german$V21 == 2 ), 0)
    set.seed(333)
    inTraining <- createDataPartition(V21, p = .75, list = FALSE)
    training <- german[ inTraining,]
    testing  <- german[-inTraining,]
    
    if(input$mod=="GLM"){
      kredit.glm = glm(V21 ~ .,data=training, family=binomial)
      
      pred1<- predict(kredit.glm,newdata=testing,type ="response")
      
      
      mc=confusionMatrix(as.factor(pred1>0.5),as.factor(testing$V21==1))
      mc
      
    }else if(input$mod=="forward"){
      modele_simple <- glm(V21 ~ 1,data = training,"binomial")
      pr.f.step<-step(modele_simple, scope = ~V21+factor(V01)+factor(V03)+factor(V04)+V05+factor(V06)+factor(V07)+factor(V09)+factor(V10)+factor(V12)+
                        factor(V14)+factor(V15)+factor(V17)+factor(V19)+factor(V20)+V02+V08+V11+V13+V16+V18,data= training,trace = -1,dir="forward")
      pred2<- predict(pr.f.step,newdata=testing,type ="response")
      mc=confusionMatrix(as.factor(pred2>0.5),as.factor(testing$V21==1)) 
      mc 
      
    }else if(input$mod=="backward"){
      modele_complexe <- glm(V21 ~ . , data= training,"binomial")
      backmodele=step(modele_complexe,data= training, dir = "backward")
      pred3<- predict(backmodele,newdata=testing,type ="response")
      mc=confusionMatrix(as.factor(pred3>0.5),as.factor(testing$V21==1)) 
      mc 
    }else if(input$mod=="both"){
      modele_complexe <- glm(V21 ~ . , data= training,"binomial")
      bothmodele=step(modele_complexe, dir = "both",type="reponse")
      pred4<- predict(bothmodele,newdata=testing,type ="response")
      mc=confusionMatrix(as.factor(pred4>0.5),as.factor(testing$V21==1)) 
      mc 
    }else if(input$mod=="arbre de decision"){
      
      arbre <- rpart(V21 ~ ., data = training, method = "class")   
      pred5 <- predict(arbre, newdata = testing, type = "class") 
      
      mc=confusionMatrix(as.factor(pred5),as.factor(testing$V21)) 
      mc
      
    }else{
      
      rf=randomForest(V21 ~. ,data=training) 
      pred6 <- predict(rf, newdata = testing, type = "class") 
      mc=confusionMatrix(as.factor(pred6>0.5),as.factor(testing$V21==1)) 
      mc
      
    }
    
    
  })  
  output$roc <-renderPlot({
    # courbe ROC pour le modele glm
    
    modele1.posterior<-predict(kredit.glm,testing,type="response" )
    modele1.pred<-prediction(modele1.posterior, testing[,21])
    modele1.roc<-performance(modele1.pred, "tpr","fpr")
    plot(modele1.roc,col="black")
    
    # courbe ROC pour le modele backward
    
    modele1.posterior<-predict(backmodele,testing,type="response" )
    modele1.pred<-prediction(modele1.posterior, testing[,21])
    modele1.roc<-performance(modele1.pred, "tpr","fpr")
    plot(modele1.roc,col="green",add=T)
    
    #courbe  ROC pour le modele both
    modele1.posterior2<-predict(bothmodele,testing,type="response" )
    modele1.pred2<-prediction(modele1.posterior2, testing[,21])
    modele1.roc2<-performance(modele1.pred2, "tpr","fpr")
    plot(modele1.roc2,col="red", add=T)
    
    
    #courbe ROC pour le modele forward
    modele1.posterior1<-predict(pr.f.step,testing,type="response" )
    modele1.pred1<-prediction(modele1.posterior1, testing[,21])
    modele1.roc1<-performance(modele1.pred1, "tpr","fpr")
    plot(modele1.roc1,col="blue",add=T)
    
    #arbre de decision
    ROCdistree=predict(arbre,newdata=testing,type="prob")[,2]
    preddistree=prediction(ROCdistree,testing[,21])
    perfdistree=performance(preddistree,"tpr","fpr")
    plot(perfdistree,col="yellow",add=TRUE)
    #Random Forest
    ROCrf=predict(rf,newdata=testing,type="response")
    predrf=prediction(ROCrf,testing[,21])
    perfdrf=performance(predrf,"tpr","fpr")
    plot(perfdrf,col="pink",add=TRUE)
    legend("right", legend=c("glm","backward", "forward","both","arbre de decision","Random Forest"),
           col=c("black","green","red", "blue","yellow","pink"), lty=1:1, cex=0.8)
    
  })
  
}

shinyApp(ui, server)

