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
                           selectInput("var","Choisir la variable",choices = colnames(base),selected = "Groupes")),
                       box("Representation des variables",collapsible = T,status="primary",width = 9,
                           plotOutput("graph"),
                           plotOutput("graph2"))
              )
              
      ),
      tabItem(tabName = "ab",
              fluidRow(box(title = "Representation",collapsible = T,status = "primary",width = 12,
                           selectInput("var","Choisir les variables",choices = c("coût total / groupes","coût total / antibiothérapie","groupes / age","coût anti infectieux / coût biologie"),selected = "coût total / groupes")),
                       box("Representation des variables",collapsible = T,status="primary",width = 9,  plotOutput("graph3")  ))
              
      ),
      tabItem(tabName = "Mod",
              
              
              tabsetPanel(
                tabPanel("coût/complications",
                         fluidRow(box(title = "",status = "primary",width = 3,
                                      selectInput("mod","Choisir le bras",choices = c("bras Filgrastim","bras Placebo"),selected = "bras Filgrastim")
                         ),
                         
                         box(title = "Modele",status = "primary",verbatimTextOutput("resM"))
                         
                         
                         )
                ),
                tabPanel("Coût/efficacité",box(title = "Courbe ROC",status = "primary", plotOutput("roc"),width=8)
                )
              )
      )
    )))
server <- function(input, output) { 
  
  output$graph <- renderPlot({
    if(input$var=="antibiothérapie"){
      ggplot(base, aes(antibiothérapie) ) +
        geom_histogram(color="grey",fill = "lightblue") + ggtitle("Distribution du nombre de jours de l'antibiothérapie") + 
        xlab("Antibiothérapie") + ylab("Fréquence")
      
      
      
      
      
    }else if(input$var=="Age"){
      ggplot(base, aes( Age) ) +
        geom_histogram(color="grey",fill = "pink") + ggtitle("Distribution d'age") + 
        xlab("Age") + ylab("Fréquence")
      
      
    }else if(input$var=="IMC"){
      ggplot(base, aes( IMC) ) +
        geom_histogram(color="grey",fill = "lightgray") + ggtitle("Distrubtion de l'IMC") + 
        xlab("IMC") + ylab("Fréquence")
      
      
      
      
    }else if(input$var=="caractéristiq_greffon_CMN"){
      ggplot(base, aes( caractéristiq_greffon_CMN) ) +
        geom_histogram(color="grey",fill = "#CC9999")  + 
        xlab("Caractéristique_greffon") + ylab("Fréquence")
      
      
      
    }else if(input$var=="durée_aplasie"){
      ggplot(base, aes(  durée_aplasie) ) +
        geom_histogram(color="grey",fill = "blue") + ggtitle("Distribution de la durée de l'aplasie") + 
        xlab("Durée de l'aplasie") + ylab("Fréquence")
      
    }else if(input$var=="Nombre.de.jours.de.fièvre"){
      ggplot(base, aes( Nombre.de.jours.de.fièvre) ) +
        geom_histogram(color="grey",fill = "purple") + ggtitle("distribution de nombres de jours de fièvre") + 
        xlab("Nombre de jours de fièvre") + ylab("Fréquence")
      
    }else if(input$var=="Durée.de.l.hospitalisation"){
      ggplot(base, aes( Durée.de.l.hospitalisation) ) +
        geom_histogram(color="grey",fill = "red") + ggtitle("distribution de la durée d'hospitalisation") + 
        xlab("Durée de l'hospitalisation") + ylab("Fréquence")
      
    }else if(input$var=="cout_Filgrastim"){
      ggplot(base, aes( cout_Filgrastim) ) +
        geom_histogram(color="grey",fill = "#0099CC") + ggtitle("distribution de coût de filgrastim") + 
        xlab("Coût de Filgrastim") + ylab("Fréquence")
      
    }else if(input$var=="COUT_TOTAL"){
      ggplot(base, aes( COUT_TOTAL) ) +
        geom_histogram(color="grey",fill = "#FFCC66") + ggtitle("distribution du coût total") + 
        xlab("Coût total") + ylab("Fréquence")
      
    }else if(input$var=="durée_mucite"){
      
      ggplot(base, aes( durée_mucite) ) +
        geom_histogram(color="grey",fill = "#CCCCCC") + ggtitle("distribution de la durée de mucite") + 
        xlab("durée de mucite") + ylab("Fréquence")
    } else if(input$var=="nb_jours_filgrastim"){
      ggplot(bd, aes( nb_jours_filgrastim) ) +  
        geom_histogram(color="grey",fill = "#66FF00")  + ggtitle("distribution de nombre de jours de filgrastim") + 
        xlab("nb_jours_filgrastim") + ylab("Fréquence")
      
      
    }else if(input$var=="Groupes"){
      ggplot(base,  aes(x = factor(""), fill = Groupes) ) +
        geom_bar()
      
      
    }else if(input$var=="statut_CMV"){
      ggplot(base,  aes(x = factor(""), fill = statut_CMV) ) +
        geom_bar()
      
      
    }else if(input$var=="diagnostic_binaire"){
      ggplot(base,  aes(x = factor(""), fill = diagnostic_binaire ) ) +
        geom_bar()
      
      
    }else if(input$var=="Diagnostic"){
      ggplot(base,  aes(x = factor(""), fill = Diagnostic) ) +
        geom_bar()
      
    }else if(input$var=="Statut.pré.greffe"){
      ggplot(base,  aes(x = factor(""), fill = Statut.pré.greffe) ) +
        geom_bar()
      
    }else if(input$var=="Mismatch.de.sexe"){
      ggplot(base,  aes(x = factor(""), fill = Mismatch.de.sexe) ) +
        geom_bar()
    }else if(input$var=="IBOMPATIBILITE_ABO_binaire"){
      ggplot(base,  aes(x = factor(""), fill = IBOMPATIBILITE_ABO_binaire) ) +
        geom_bar()
    }else if(input$var=="Incompatibilité.ABO"){
      ggplot(base,  aes(x = factor(""), fill = Incompatibilité.ABO) ) +
        geom_bar()
    }else if(input$var=="ProphylaxieGVH"){
      ggplot(base,  aes(x = factor(""), fill =ProphylaxieGVH) ) +
        geom_bar()
      
      
    }else if(input$var=="status"){
      ggplot(base,  aes(x = factor(""), fill =status ) ) +
        geom_bar()
    }else if(input$var=="statut_survie100j"){
      ggplot(base,  aes(x = factor(""), fill =statut_survie100j) ) +ggtitle("statut de non survie")+
        geom_bar()
      
    }else if(input$var=="reac_ou_acti_CMV "){
      ggplot(base,  aes(x = factor(""), fill =reac_ou_acti_CMV ) ) +
        geom_bar()
      
    }else if(input$var=="mucite"){
      ggplot(base,  aes(x = factor(""), fill = mucite ) ) +
        geom_bar()
      
    }else if(input$var=="mucite_grade3_ou_4"){
      ggplot(base,  aes(x = factor(""), fill =mucite_grade3_ou_4  ) ) +
        geom_bar()
      
    }else if(input$var=="MAT"){
      ggplot(base,  aes(x = factor(""), fill = MAT ) ) +
        geom_bar()
      
    }else if(input$var=="SAM"){
      ggplot(base,  aes(x = factor(""), fill = SAM ) ) +
        geom_bar()
      
    }else if(input$var=="MVO"){
      ggplot(base,  aes(x = factor(""), fill = MVO ) ) +
        geom_bar()
      
    }else if(input$var=="TRM"){
      ggplot(base,  aes(x = factor(""), fill = TRM ) ) +
        geom_bar()
    }else if(input$var=="GVHaigu_supouégalà2"){
      ggplot(base,  aes(x = factor(""), fill = GVHaigu_supouégalà2 ) ) +
        geom_bar()
    }else if(input$var=="infections_cliniquement_documentés"){
      ggplot(base, aes( infections_cliniquement_documentés, fill =  infections_cliniquement_documentés) ) +
        geom_bar()
      
    }else if(input$var=="infections_microbiologiquement_documentées"){
      
      ggplot(base, aes( insuffisance_rénale, fill =  insuffisance_rénale) ) +
        geom_bar()
      
      
      
    }else if(input$var=="rechute_à_J100"){
      ggplot(base, aes( rechute_à_J100, fill =  rechute_à_J100) ) +
        geom_bar()
      
      
    }else {
      ggplot(base, aes( hepatopathie, fill =  hepatopathie) ) +
        geom_bar()
    }
    
    
    
  })
  
  
  output$graph2 <- renderPlot({
    if(input$var=="antibiothérapie"){
      boxplot(antibiothérapie,col="lightblue")
      
      
      
      
      
      
    }else if(input$var=="Age"){
      boxplot(Age,col="pink")
      
    }else if(input$var=="IMC"){
      boxplot(IMC,col="lightgray")
      
    }else if(input$var=="caractéristiq_greffon_CMN"){
      boxplot(caractéristiq_greffon_CMN,col="#CC9999")
      
    }else if(input$var=="durée_aplasie"){
      boxplot(durée_aplasie,col="blue") 
      
    }else if(input$var=="Nombre.de.jours.de.fièvre"){
      boxplot(Nombre.de.jours.de.fièvre,col="purple")
      
    }else if(input$var=="Durée.de.l.hospitalisation"){
      boxplot(Durée.de.l.hospitalisation,col="red")
      
    }else if(input$var=="cout_Filgrastim"){
      boxplot(cout_Filgrastim,col="#0099CC" )
      
    }else if(input$var==" COUT_TOTAL"){
      boxplot(COUT_TOTAL,col="#FFCC66")
      
    }else if(input$var=="durée_mucite"){
      boxplot(durée_mucite,col="#CCCCCC")
      
    } else if(input$var=="nb_jours_filgrastim"){
      boxplot(nb_jours_filgrastim,col="#66FF00")
      
      
    }else if(input$var=="Groupes"){
      ggplot(base, aes(Groupes, fill = Groupes ) ) +
        geom_bar()
      
    }else if(input$var=="statut_CMV"){
      ggplot(base, aes(statut_CMV, fill = statut_CMV) ) +
        geom_bar()
      
    }else if(input$var=="diagnostic_binaire"){
      ggplot(base, aes(diagnostic_binaire, fill = diagnostic_binaire) ) +
        geom_bar()
      
    }else if(input$var=="Diagnostic"){
      ggplot(base, aes(Diagnostic, fill = Diagnostic) ) +
        geom_bar()
      
      
    }else if(input$var=="Statut.pré.greffe"){
      ggplot(base, aes(Statut.pré.greffe, fill = Statut.pré.greffe) ) +
        geom_bar()  
      
    }else if(input$var==" Mismatch.de.sexe"){
      ggplot(base, aes(Mismatch.de.sexe, fill = Mismatch.de.sexe) ) +
        geom_bar()
      
    }else if(input$var=="IBOMPATIBILITE_ABO_binaire"){
      ggplot(base, aes(IBOMPATIBILITE_ABO_binaire, fill = IBOMPATIBILITE_ABO_binaire) ) +
        geom_bar()
      
    }else if(input$var=="Incompatibilité.ABO"){
      ggplot(base, aes(Incompatibilité.ABO, fill = Incompatibilité.ABO) ) +
        geom_bar()
      
    }else if(input$var=="ProphylaxieGVH"){
      ggplot(base, aes(ProphylaxieGVH, fill = ProphylaxieGVH) ) +
        geom_bar()
      
    }else if(input$var=="status"){
      ggplot(base, aes(status, fill = status) ) +
        geom_bar()
      
    }else if(input$var=="statut_survie100j"){
      ggplot(base, aes(statut_survie100j, fill = statut_survie100j) ) +
        geom_bar()
      
    }else if(input$var=="reac_ou_acti_CMV"){
      ggplot(base, aes(reac_ou_acti_CMV, fill = reac_ou_acti_CMV) ) +
        geom_bar()
      
    }else if(input$var=="mucite"){
      ggplot(base, aes(mucite , fill = mucite ) ) +
        geom_bar()
      
    }else if(input$var=="mucite_grade3_ou_4"){
      ggplot(base, aes(mucite_grade3_ou_4, fill = mucite_grade3_ou_4) ) +
        geom_bar()
      
    }else if(input$var=="MAT"){
      ggplot(base, aes( MAT, fill =  MAT) ) +
        geom_bar()
      
    }else if(input$var=="SAM"){
      ggplot(base, aes(SAM, fill =  SAM) ) +
        geom_bar()
      
    }else if(input$var=="MVO"){
      ggplot(base, aes( MVO, fill =  MVO) ) +
        geom_bar()
      
    }else if(input$var=="TRM"){
      ggplot(base, aes( TRM, fill =  TRM) ) +
        geom_bar()
    }else if(input$var=="GVHaigu_supouégalà2"){
      ggplot(base, aes( GVHaigu_supouégalà2, fill =  GVHaigu_supouégalà2) ) +
        geom_bar()
      
      
    }
    
    
    
  })
  
  
  
  
  
  
  
  
  output$graph3 <-renderPlot({if(input$var=="coût total / groupes"){
    ggplot(base, aes(Groupes, COUT_TOTAL) ) +
      geom_point()
    
  }else if(input$var=="coût total / antibiothérapie"){
    ggplot(base, aes(COUT_TOTAL, antibiothérapie)) +
      geom_hex()
  }else if(input$var=="groupes / age"){
    
    ggplot(base, aes(Groupes, Age)) +
      geom_hex()
  }else if(input$var=="coût anti infectieux / coût biologie"){
    ggplot(base, aes(cout_anti_infectieux,cout_biologie) ) +
      geom_point( aes(shape=Groupes ) )}
  })
  
  output$resM <-renderPrint({
    library(readxl)
    bd <- read_excel("~/base données filgrastimvaleurrrr 5.xlsx")
    View(bd)
    attach(bd)
    normalize <- function(x){
      return((x- min(x))/(max(x)-min(x)))
    }
    normalize(COUT_TOTAL)
    
    if(input$mod=="bras Filgrastim"){
      f<-bd[which(bd$Groupes=="1"),]
      View(f)
      modelf<- glm(f$COUT_TOTAL~f$reac_ou_acti_CMV+f$mucite+f$grade_mucite+f$durée_mucite+f$mucite_grade3_ou_4+f$MAT+f$SAM+f$MVO+f$TRM+f$GVHaigu_supouégalà2+f$infections_cliniquement_documentés+f$infections_microbiologiquement_documentées+f$insuffisance_rénale+f$rechute_à_J100+f$hepatopathie)
      summary(modelf)
      anova(modelf,test="F")
      
    }else if(input$mod=="bras Placebo"){
      pl<-bd[which(bd$Groupes=="0"),]
      View(pl)
      modelpl<- glm(pl$COUT_TOTAL~pl$reac_ou_acti_CMV+pl$mucite+pl$grade_mucite+pl$durée_mucite+pl$mucite_grade3_ou_4+pl$MAT+pl$SAM+pl$MVO+pl$TRM+pl$GVHaigu_supouégalà2+pl$infections_cliniquement_documentés+pl$infections_microbiologiquement_documentées+pl$insuffisance_rénale+pl$rechute_à_J100+pl$hepatopathie)
      summary(modelpl)
      anova(modelpl,test="F")
      
      
    }
    
    
  })  
  output$roc <-renderPrint({
    icer<-function(x,y){
      rsl<-( mean(f$COUT_TOTAL)-mean(pl$COUT_TOTAL))/(x-y)
      rsl
    }
    xhosp<-mean(f$Durée.de.l.hospitalisation)
    
    yhosp<-mean(pl$Durée.de.l.hospitalisation)
    
    icer(xhosp,yhosp)
    
    xapl<-mean(f$durée_aplasie)
    xapl
    yapl<-mean(pl$durée_aplasie)
    yapl
    icer(xapl,yapl)
    
    xant<-mean(f$antibiothérapie)
    xant
    yant<-mean(pl$antibiothérapie)
    yant
    icer(xant,yant)
    
    
  })
  
}

shinyApp(ui, server)