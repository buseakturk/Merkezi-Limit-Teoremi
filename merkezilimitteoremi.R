
library(shiny)

ui <- fluidPage(
  
  titlePanel('Ornekleme Dagilimleri ve Merkezi Limit Teoremi'),
  
  sidebarPanel(
    
    helpText('Dagilimi seciniz,parametleri giriniz ve "orneklem buyuklugu sayisini(n) seciniz."Ornekle" 
                 butonuna tikladiginizda sonuclar goruntulenecektir.'),
    
    sliderInput(inputId="n",
                "Orneklem Genisligi(n)",
                value=30,min=5,max=200,step=2),
    
    radioButtons("src.dist", "Dagilim Turleri:",
                 c("Ustel:     Parametre1 = Ortalama,     Parametre2 = Kullanilmaz" = "U",
                   "Normal:    Parametre1 = Ortalama,     Parametre2 = Standart Sapma" = "N",
                   "Duzgun:    Parametre1 = Min,          Parametre2 = Maks" = "D",
                   "Poisson:   Parametre1 = Lamda,        Parametre2 = Kullanilmaz" = "P",
                   "Couchy:    Parametre1 = Lokasyon,     Parametre2 = Olcek" = "C",
                   "Binom:     Parametre1 = Deneme Sayisi,Parametre2 = Basari Olasiligi" = "B",
                   "Gamma:     Parametre1 = Sekil,        Parametre2 = Ölcek" = "G",
                   "Chi square:Parametre1 = Serbestlik Derecesi, Parametre2 = ncp" = "X",
                   "T Dagilimi:Parametre1 = Serbestlik Derecesi, Parametre2 = Kullanilmaz" = "T")),
    
    numericInput("param1","Parametre 1:",10),
    numericInput("param2","Parametre 2:",2),
    actionButton("takeSample","Ornekle ")
  ),
  
  # sidebarPanel bitis.
  
  mainPanel(
    
    tags$head(
      
      tags$style("body {background-color: #faebd7; }")#arka plan rengi
    ),
    
    plotOutput("plotSample")
    
  ) # mainPanel bitis.
  
) # UI bitis.

r <- 10000 # Number of replications (kopya sayisi)
#Merkezi limit teoreminin kuralina gore:
#Ornekleye her bastigimizda secilen dagilimdan 10000 ornek secilecek.

palette(c("#dad7fa", "#e382ac", "#4c7b46", "#4ea8a5",
          "#e3395a", "#e6eb2e", "#cc4320", "#1e62e3"))

server <- function(input, output) {
  set.seed(as.numeric(Sys.time()))
  
  #Simulasyonun uyguladigi veri yapilari icin reaktif bir deger olusturur.
  #Reactive values degiskenleri, user interface (UI) yani kullanici arayuzu icin 
  #cikti hazirlayan bolumleri tarafindan kullanilabilir, ornegin output$plotSample gibi.
  
  rv <- reactiveValues(orneklemler = NULL,
                       toplamlar = NULL,
                       ortalamalar = NULL,
                       varyanslar = NULL)
  
  # ObserveEvent fonksiyonunda kullanici arayuzundeki tum ciktilari veriyoruz.
  # Kullanici arayuzumuzdeki girdi degiskenlerini girdi$değişken adı olarak adlandirabiliriz.
  
  observeEvent(input$takeSample,
               {
                 my.samples <- switch(input$src.dist,
                                      "U" = matrix(rexp   (input$n*r, input$param1             ),r),
                                      "N" = matrix(rnorm  (input$n*r, input$param1,input$param2),r),
                                      "D" = matrix(runif  (input$n*r, input$param1,input$param2),r),
                                      "P" = matrix(rpois  (input$n*r, input$param1)             ,r),
                                      "C" = matrix(rcauchy(input$n*r, input$param1,input$param2),r),
                                      "B" = matrix(rbinom (input$n*r, input$param1,input$param2),r),
                                      "G" = matrix(rgamma (input$n*r, input$param1,input$param2),r),
                                      "X" = matrix(rchisq (input$n*r, input$param1)             ,r),
                                      "T" = matrix(rt     (input$n*r, input$param1)             ,r))
                 
                 #Grafikleri cizmek icin butun degiskenler sayisal olmalidir.
                 rv$orneklemler <- as.numeric(my.samples[1,])
                 rv$ toplamlar<- as.numeric(apply(my.samples,1,sum))
                 rv$ ortalamalar<- as.numeric(apply(my.samples,1,mean))
                 rv$ varyanslar<- as.numeric(apply(my.samples,1,var))
               })
  
  output$plotSample <- renderPlot({
    
    if (input$takeSample) {
      
      #Grafikler icin 2x2 lik bir alan olusturulur ve baslik icin ustte buyuk bir alan (5) olusturur.
      par(mfrow=c(2,2), oma=c(0,0,5,0))
      
      hist(rv$orneklemler, main="Orneklem Dagilimi",                  ylab="Frekans",col=1)
      hist(rv$toplamlar,   main="Orneklemin Toplama Gore Dagilimi",   ylab="Frekans",col=2)
      hist(rv$ortalamalar, main="Orneklemin ortalamaya Gore Dagilimi",ylab="Frekans",col=3)
      hist(rv$varyanslar,  main="Orneklemin Varyansa Gore Dagilimi" , ylab="Frekans",col=4)
      
      mtext("Simulasyon Sonuclari", outer=TRUE, cex=3)
    }
  }, height=660, width=900) 
  
} # server bitis.


shinyApp(ui, server)

