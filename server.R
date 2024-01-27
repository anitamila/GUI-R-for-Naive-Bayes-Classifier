server <- function(input,output,session){
  #Input Data Awal
  data.input <- reactive({
    in.file <- input$data
    if(is.null(in.file)){
      return(NULL)
    }else {
      df <- read.csv(input$data$datapath,
                      header = input$header,
                      sep = input$sep,
                      quote = input$quote)
    }
  })
  
  #Print Data
  output$data = renderDT({
    data = data.input()
  })
  
  #Backward Elimination
  observeEvent(input$run6,{
    withProgress(message = "Backward Elimination in Progress",
                 detail = "Please wait ...", {
                   req(input$data)
                   dataku <- read.csv(input$data$datapath,
                                      header = input$header,
                                      sep = input$sep,
                                      quote = input$quote)
                   
                   #Pembagian index matrix
                   indexmatrix <- createDataPartition(dataku$Klasifikasi, p=.7, 
                                                      list = FALSE, times = 1)
                   
                   #Membuat Data Latih dan Data Uji
                   data.train <- dataku[indexmatrix,]
                   data.test <- dataku[-indexmatrix,]
                   backwa <- multinom(Klasifikasi~., data=data.train)
                   bckw <- stepAIC(backw,direction="backward",trace=F)
                   
                   output$hasil.be<-renderPrint({
                     
                     print(bckw$anova)
                   })
                 }) #kurung withprogress
  }) #kurung observeEvent
  
  #NBC
  observeEvent(input$run,{
    withProgress(message = "Naive Bayes Classifier in Progress",
                 detail = "Please wait ...", {
                   req(input$data)
                   dataku <- read.csv(input$data$datapath,
                                      header = input$header,
                                      sep = input$sep,
                                      quote = input$quote)
                   
                   #Pembagian index matrix
                   indexmatrix <- createDataPartition(dataku$Klasifikasi, p=.7, 
                                                      list = FALSE, times = 1)
                   
                   #Membuat Data Latih dan Data Uji
                   data.train <- dataku[indexmatrix,]
                   data.test <- dataku[-indexmatrix,]
                   
                   #Re-label value of kategoric
                   data.train$Klasifikasi[data.train$Klasifikasi==1] <- "ISPA"
                   data.train$Klasifikasi[data.train$Klasifikasi==2] <- "Pneumonia"
                   data.train$Klasifikasi[data.train$Klasifikasi==3] <- "Bukan.keduanya"
                   data.train$Demam[data.train$Demam==0]<- "Tidak"
                   data.train$Demam[data.train$Demam==1]<- "Ya"
                   data.train$Batuk[data.train$Batuk==0]<- "Tidak"
                   data.train$Batuk[data.train$Batuk==1]<- "Ya"
                   data.train$Pilek[data.train$Pilek==0]<- "Tidak"
                   data.train$Pilek[data.train$Pilek==1]<- "Ya"
                   data.train$Sesak[data.train$Sesak==0]<- "Tidak"
                   data.train$Sesak[data.train$Sesak==1]<- "Ya"
                   data.train$Muntah[data.train$Muntah==0]<- "Tidak"
                   data.train$Muntah[data.train$Muntah==1]<- "Ya"
                   data.test$Klasifikasi[data.test$Klasifikasi==1] <- "ISPA"
                   data.test$Klasifikasi[data.test$Klasifikasi==2] <- "Pneumonia"
                   data.test$Klasifikasi[data.test$Klasifikasi==3] <- "Bukan.keduanya"
                   data.test$Demam[data.test$Demam==0]<- "Tidak"
                   data.test$Demam[data.test$Demam==1]<- "Ya"
                   data.test$Batuk[data.test$Batuk==0]<- "Tidak"
                   data.test$Batuk[data.test$Batuk==1]<- "Ya"
                   data.test$Pilek[data.test$Pilek==0]<- "Tidak"
                   data.test$Pilek[data.test$Pilek==1]<- "Ya"
                   data.test$Sesak[data.test$Sesak==0]<- "Tidak"
                   data.test$Sesak[data.test$Sesak==1]<- "Ya"
                   data.test$Muntah[data.test$Muntah==0]<- "Tidak"
                   data.test$Muntah[data.test$Muntah==1]<- "Ya"
                   
                   #convert klasifikasi ke as.factor
                   data.train$Klasifikasi <- as.factor(data.train$Klasifikasi)
                   data.train$Batuk <- as.factor(data.train$Batuk)
                   data.train$Pilek <- as.factor(data.train$Pilek)
                   data.train$Demam <- as.factor(data.train$Demam)
                   data.train$Sesak <- as.factor(data.train$Sesak)
                   data.train$Muntah <- as.factor(data.train$Muntah)
                   data.test$Klasifikasi <- as.factor(data.test$Klasifikasi)
                   data.test$Batuk <- as.factor(data.test$Batuk)
                   data.test$Pilek <- as.factor(data.test$Pilek)
                   data.test$Demam <- as.factor(data.test$Demam)
                   data.test$Sesak <- as.factor(data.test$Sesak)
                   data.test$Muntah <- as.factor(data.test$Muntah)
                   
                   #K-Fold Cross Validation
                   controlspecs <- trainControl(method = "cv", number = 10,
                                                savePredictions = "all",
                                                classProbs = TRUE)
                   
                   incProgress(1/3) #Penanda progres loading
                   
                   #Input random seed sebagai angka acak
                   set.seed(input$set.seed)
                   
                   #NBC
                   naivebayes <- train(Klasifikasi~., data=data.train, method="nb", 
                                  trControl=controlspecs, tuneLength=0)
                   pred=predict(naivebayes,data.test)
                   hasilnb=cbind(data.test,pred)
                   colnames(hasilnb)= c("Usia", "BB", "Batuk", "Pilek", "Demam", "Sesak", "Muntah", "Klasifikasi aktual", "Prediksi")
                
                   incProgress(2/3) #Penanda progress loading
                   
                   output$klasifikasi.nbc<-renderPrint({
                     hasilnb
                   })
                   incProgress(3/3) #Penanda progress loading
                 }) #kurung withprogress
  }) #kurung observeEvent
  
  #Kinerja
  observeEvent(input$run2,{
    withProgress(message = "Naive Bayes Classifier in Progress",
                 detail = "Please wait ...", {
                   req(input$data)
                   dataku <- read.csv(input$data$datapath,
                                      header = input$header,
                                      sep = input$sep,
                                      quote = input$quote)
                   
                   #Pembagian index matrix
                   indexmatrix <- createDataPartition(dataku$Klasifikasi, p=.7, 
                                                      list = FALSE, times = 1)
                   
                   #Membuat Data Latih dan Data Uji
                   data.train <- dataku[indexmatrix,]
                   data.test <- dataku[-indexmatrix,]
                   
                   #Re-label value of kategoric
                   data.train$Klasifikasi[data.train$Klasifikasi==1] <- "ISPA"
                   data.train$Klasifikasi[data.train$Klasifikasi==2] <- "Pneumonia"
                   data.train$Klasifikasi[data.train$Klasifikasi==3] <- "Bukan.keduanya"
                   data.train$Demam[data.train$Demam==0]<- "Tidak"
                   data.train$Demam[data.train$Demam==1]<- "Ya"
                   data.train$Batuk[data.train$Batuk==0]<- "Tidak"
                   data.train$Batuk[data.train$Batuk==1]<- "Ya"
                   data.train$Pilek[data.train$Pilek==0]<- "Tidak"
                   data.train$Pilek[data.train$Pilek==1]<- "Ya"
                   data.train$Sesak[data.train$Sesak==0]<- "Tidak"
                   data.train$Sesak[data.train$Sesak==1]<- "Ya"
                   data.train$Muntah[data.train$Muntah==0]<- "Tidak"
                   data.train$Muntah[data.train$Muntah==1]<- "Ya"
                   data.test$Klasifikasi[data.test$Klasifikasi==1] <- "ISPA"
                   data.test$Klasifikasi[data.test$Klasifikasi==2] <- "Pneumonia"
                   data.test$Klasifikasi[data.test$Klasifikasi==3] <- "Bukan.keduanya"
                   data.test$Demam[data.test$Demam==0]<- "Tidak"
                   data.test$Demam[data.test$Demam==1]<- "Ya"
                   data.test$Batuk[data.test$Batuk==0]<- "Tidak"
                   data.test$Batuk[data.test$Batuk==1]<- "Ya"
                   data.test$Pilek[data.test$Pilek==0]<- "Tidak"
                   data.test$Pilek[data.test$Pilek==1]<- "Ya"
                   data.test$Sesak[data.test$Sesak==0]<- "Tidak"
                   data.test$Sesak[data.test$Sesak==1]<- "Ya"
                   data.test$Muntah[data.test$Muntah==0]<- "Tidak"
                   data.test$Muntah[data.test$Muntah==1]<- "Ya"
                   
                   #convert klasifikasi ke as.factor
                   data.train$Klasifikasi <- as.factor(data.train$Klasifikasi)
                   data.train$Batuk <- as.factor(data.train$Batuk)
                   data.train$Pilek <- as.factor(data.train$Pilek)
                   data.train$Demam <- as.factor(data.train$Demam)
                   data.train$Sesak <- as.factor(data.train$Sesak)
                   data.train$Muntah <- as.factor(data.train$Muntah)
                   data.test$Klasifikasi <- as.factor(data.test$Klasifikasi)
                   data.test$Batuk <- as.factor(data.test$Batuk)
                   data.test$Pilek <- as.factor(data.test$Pilek)
                   data.test$Demam <- as.factor(data.test$Demam)
                   data.test$Sesak <- as.factor(data.test$Sesak)
                   data.test$Muntah <- as.factor(data.test$Muntah)
                   
                   #K-Fold Cross Validation
                   controlspecs <- trainControl(method = "cv", number = 10,
                                                savePredictions = "all",
                                                classProbs = TRUE)
                   
                   incProgress(1/3) #Penanda progres loading
                   
                   #Input random seed sebagai angka acak
                   set.seed(input$set.seed)
                   
                   #NBC
                   naivebayes <- train(Klasifikasi~., data=data.train, method="nb", 
                                       trControl=controlspecs, tuneLength=0)
                   pred <- predict(naivebayes, data.test)
                   hasilnb=cbind(data.test,pred)
                   colnames(hasilnb)= c("Usia", "BB", "Batuk", "Pilek", "Demam", "Sesak", "Muntah", "Klasifikasi Aktual", "Prediksi")
                   
                   conf.mat<-confusionMatrix(table("Prediksi"=pred,"Aktual"=data.test$"Klasifikasi"))
                   
                   observeEvent(input$run2,{
                     output$conf.nbc<-renderPrint({
                       conf.mat
                     })
                   }) #kurung observeEvent
                 }) #kurung withprogress
  }) #kurung observeEvent

  #NBC Backward
  observeEvent(input$run3,{
    withProgress(message = "Naive Bayes Classifier in Progress",
                 detail = "Please wait ...", {
                   req(input$data)
                   dataku <- read.csv(input$data$datapath,
                                      header = input$header,
                                      sep = input$sep,
                                      quote = input$quote)
                   
                   #Pembagian index matrix
                   indexmatrixbe <- createDataPartition(dataku$Klasifikasi, p=.7, 
                                                      list = FALSE, times = 1)
                   
                   #Membuat Data Latih dan Data Uji
                   data.trainbee <- dataku[indexmatrixbe,]
                   data.testbee <- dataku[-indexmatrixbe,]
                   
                   data.trainbe = data.trainbee
                   data.trainbe <- data.trainbee[,-1]
                   data.testbe = data.testbee
                   data.testbe <- data.testbee[,-1]
                   
                   #Re-label value of kategoric
                   data.trainbe$Klasifikasi[data.trainbe$Klasifikasi==1] <- "ISPA"
                   data.trainbe$Klasifikasi[data.trainbe$Klasifikasi==2] <- "Pneumonia"
                   data.trainbe$Klasifikasi[data.trainbe$Klasifikasi==3] <- "Bukan.keduanya"
                   data.trainbe$Demam[data.trainbe$Demam==0]<- "Tidak"
                   data.trainbe$Demam[data.trainbe$Demam==1]<- "Ya"
                   data.trainbe$Batuk[data.trainbe$Batuk==0]<- "Tidak"
                   data.trainbe$Batuk[data.trainbe$Batuk==1]<- "Ya"
                   data.trainbe$Pilek[data.trainbe$Pilek==0]<- "Tidak"
                   data.trainbe$Pilek[data.trainbe$Pilek==1]<- "Ya"
                   data.trainbe$Sesak[data.trainbe$Sesak==0]<- "Tidak"
                   data.trainbe$Sesak[data.trainbe$Sesak==1]<- "Ya"
                   data.trainbe$Muntah[data.trainbe$Muntah==0]<- "Tidak"
                   data.trainbe$Muntah[data.trainbe$Muntah==1]<- "Ya"
                   data.testbe$Klasifikasi[data.testbe$Klasifikasi==1] <- "ISPA"
                   data.testbe$Klasifikasi[data.testbe$Klasifikasi==2] <- "Pneumonia"
                   data.testbe$Klasifikasi[data.testbe$Klasifikasi==3] <- "Bukan.keduanya"
                   data.testbe$Demam[data.testbe$Demam==0]<- "Tidak"
                   data.testbe$Demam[data.testbe$Demam==1]<- "Ya"
                   data.testbe$Batuk[data.testbe$Batuk==0]<- "Tidak"
                   data.testbe$Batuk[data.testbe$Batuk==1]<- "Ya"
                   data.testbe$Pilek[data.testbe$Pilek==0]<- "Tidak"
                   data.testbe$Pilek[data.testbe$Pilek==1]<- "Ya"
                   data.testbe$Sesak[data.testbe$Sesak==0]<- "Tidak"
                   data.testbe$Sesak[data.testbe$Sesak==1]<- "Ya"
                   data.testbe$Muntah[data.testbe$Muntah==0]<- "Tidak"
                   data.testbe$Muntah[data.testbe$Muntah==1]<- "Ya"
                   
                   #convert klasifikasi ke as.factor
                   data.trainbe$Klasifikasi <- as.factor(data.trainbe$Klasifikasi)
                   data.trainbe$Batuk <- as.factor(data.trainbe$Batuk)
                   data.trainbe$Pilek <- as.factor(data.trainbe$Pilek)
                   data.trainbe$Demam <- as.factor(data.trainbe$Demam)
                   data.trainbe$Sesak <- as.factor(data.trainbe$Sesak)
                   data.trainbe$Muntah <- as.factor(data.trainbe$Muntah)
                   data.testbe$Klasifikasi <- as.factor(data.testbe$Klasifikasi)
                   data.testbe$Batuk <- as.factor(data.testbe$Batuk)
                   data.testbe$Pilek <- as.factor(data.testbe$Pilek)
                   data.testbe$Demam <- as.factor(data.testbe$Demam)
                   data.testbe$Sesak <- as.factor(data.testbe$Sesak)
                   data.testbe$Muntah <- as.factor(data.testbe$Muntah)
                   
                   #K-Fold Cross Validation
                   controlspecs.be <- trainControl(method = "cv", number = 10,
                                                   savePredictions = "all",
                                                   classProbs = TRUE)
                   
                   incProgress(1/3) #Penanda progres loading
                   
                   #Input random seed sebagai angka acak
                   set.seed(input$set.seed)
                   
                   #NBC
                   naivebayes.be <- train(Klasifikasi~., data=data.trainbe, method="nb", 
                                          trControl=controlspecs.be, tuneLength=0)
                   pred.be=predict(naivebayes.be,data.testbe)
                   hasilnb.be=cbind(data.testbe,pred.be)
                   colnames(hasilnb.be)= c("BB", "Batuk", "Pilek", "Demam", "Sesak", "Muntah", "Klasifikasi Aktual", "Prediksi")
                   
                   incProgress(2/3) #Penanda progress loading
                   
                   output$klasifikasi.nbcb<-renderPrint({
                     hasilnb.be
                   })
                   incProgress(3/3) #Penanda progress loading
                 }) #kurung withprogress
  }) #kurung observeEvent
  
  #Kinerja NBC Backward
  observeEvent(input$run4,{
    withProgress(message = "Naive Bayes Classifier in Progress",
                 detail = "Please wait ...", {
                   req(input$data)
                   dataku <- read.csv(input$data$datapath,
                                      header = input$header,
                                      sep = input$sep,
                                      quote = input$quote)
                   
                   #Pembagian index matrix
                   indexmatrixbe <- createDataPartition(dataku$Klasifikasi, p=.7, 
                                                        list = FALSE, times = 1)
                   
                   #Membuat Data Latih dan Data Uji
                   data.trainbee <- dataku[indexmatrixbe,]
                   data.testbee <- dataku[-indexmatrixbe,]
                   
                   backwa <- multinom(Klasifikasi~., data=data.trainbee)
                   bckw <- step(backw,direction="backward",trace=TRUE)
                   
                   data.trainbe = data.trainbee
                   data.trainbe <- data.trainbee[,-1]
                   data.testbe = data.testbee
                   data.testbe <- data.testbee[,-1]
                   
                   #Pembagian index matrix
                   indexmatrixbe <- createDataPartition(dataku$Klasifikasi, p=.7, 
                                                        list = FALSE, times = 1)
                   
                   #Membuat Data Latih dan Data Uji
                   data.trainbee <- dataku[indexmatrixbe,]
                   data.testbee <- dataku[-indexmatrixbe,]
                   
                   backwa <- multinom(Klasifikasi~., data=data.trainbee)
                   bckw <- step(backw,direction="backward",trace=TRUE)
                   
                   data.trainbe = data.trainbee
                   data.trainbe <- data.trainbee[,-1]
                   data.testbe = data.testbee
                   data.testbe <- data.testbee[,-1]
                   
                   #Re-label value of kategoric
                   data.trainbe$Klasifikasi[data.trainbe$Klasifikasi==1] <- "ISPA"
                   data.trainbe$Klasifikasi[data.trainbe$Klasifikasi==2] <- "Pneumonia"
                   data.trainbe$Klasifikasi[data.trainbe$Klasifikasi==3] <- "Bukan.keduanya"
                   data.trainbe$Demam[data.trainbe$Demam==0]<- "Tidak"
                   data.trainbe$Demam[data.trainbe$Demam==1]<- "Ya"
                   data.trainbe$Batuk[data.trainbe$Batuk==0]<- "Tidak"
                   data.trainbe$Batuk[data.trainbe$Batuk==1]<- "Ya"
                   data.trainbe$Pilek[data.trainbe$Pilek==0]<- "Tidak"
                   data.trainbe$Pilek[data.trainbe$Pilek==1]<- "Ya"
                   data.trainbe$Sesak[data.trainbe$Sesak==0]<- "Tidak"
                   data.trainbe$Sesak[data.trainbe$Sesak==1]<- "Ya"
                   data.trainbe$Muntah[data.trainbe$Muntah==0]<- "Tidak"
                   data.trainbe$Muntah[data.trainbe$Muntah==1]<- "Ya"
                   data.testbe$Klasifikasi[data.testbe$Klasifikasi==1] <- "ISPA"
                   data.testbe$Klasifikasi[data.testbe$Klasifikasi==2] <- "Pneumonia"
                   data.testbe$Klasifikasi[data.testbe$Klasifikasi==3] <- "Bukan.keduanya"
                   data.testbe$Demam[data.testbe$Demam==0]<- "Tidak"
                   data.testbe$Demam[data.testbe$Demam==1]<- "Ya"
                   data.testbe$Batuk[data.testbe$Batuk==0]<- "Tidak"
                   data.testbe$Batuk[data.testbe$Batuk==1]<- "Ya"
                   data.testbe$Pilek[data.testbe$Pilek==0]<- "Tidak"
                   data.testbe$Pilek[data.testbe$Pilek==1]<- "Ya"
                   data.testbe$Sesak[data.testbe$Sesak==0]<- "Tidak"
                   data.testbe$Sesak[data.testbe$Sesak==1]<- "Ya"
                   data.testbe$Muntah[data.testbe$Muntah==0]<- "Tidak"
                   data.testbe$Muntah[data.testbe$Muntah==1]<- "Ya"
                   
                   #convert klasifikasi ke as.factor
                   data.trainbe$Klasifikasi <- as.factor(data.trainbe$Klasifikasi)
                   data.trainbe$Batuk <- as.factor(data.trainbe$Batuk)
                   data.trainbe$Pilek <- as.factor(data.trainbe$Pilek)
                   data.trainbe$Demam <- as.factor(data.trainbe$Demam)
                   data.trainbe$Sesak <- as.factor(data.trainbe$Sesak)
                   data.trainbe$Muntah <- as.factor(data.trainbe$Muntah)
                   data.testbe$Klasifikasi <- as.factor(data.testbe$Klasifikasi)
                   data.testbe$Batuk <- as.factor(data.testbe$Batuk)
                   data.testbe$Pilek <- as.factor(data.testbe$Pilek)
                   data.testbe$Demam <- as.factor(data.testbe$Demam)
                   data.testbe$Sesak <- as.factor(data.testbe$Sesak)
                   data.testbe$Muntah <- as.factor(data.testbe$Muntah)
                   
                   #K-Fold Cross Validation
                   controlspecs.be <- trainControl(method = "cv", number = 10,
                                                   savePredictions = "all",
                                                   classProbs = TRUE)
                   
                   incProgress(1/3) #Penanda progres loading
                   
                   #Input random seed sebagai angka acak
                   set.seed(input$set.seed)
                   
                   #NBC
                   naivebayes.be <- train(Klasifikasi~., data=data.trainbe, method="nb", 
                                          trControl=controlspecs.be, tuneLength=0)
                   pred.be=predict(naivebayes.be,data.testbe)
                   hasilnb.be=cbind(data.testbe,pred.be)
                   colnames(hasilnb.be)= c("BB", "Batuk", "Pilek", "Demam", "Sesak", "Muntah", "Klasifikasi aktual", "Prediksi")
                   conf.matbe<-confusionMatrix(table("Prediksi"=pred.be,"Aktual"=data.testbe$"Klasifikasi"))
                   
                   observeEvent(input$run4,{
                     output$conf.nbcb<-renderPrint({
                       conf.matbe
                     })
                   }) #kurung observeEvent
                 }) #kurung withprogress
  }) #kurung observeEvent
  
  #Tab Prediksi
  #Input Data
  output$data2 = renderDT({
    req(input$data2)
    df2 <- read.csv(input$data2$datapath,
                    header = input$header,
                    sep = input$sep,
                    quote = input$quote)
    return(df2)
  })

  
  #Model Prediksi
  observeEvent(input$run5,{
    withProgress(message = "Naive Bayes Classifier in Progress",
                 detail = "Please wait ...",{
                   req(input$data)
                   dataku <- read.csv(input$data$datapath,
                                      header = input$header,
                                      sep = input$sep,
                                      quote = input$quote)
                   
                   
                   req(input$data2)
                   data.pred <- read.csv(input$data2$datapath,
                                         header = input$header,
                                         sep = input$sep,
                                         quote = input$quote)
                   
                   #Pembagian index matrix
                   indexmatrix.be <- createDataPartition(dataku$Klasifikasi, p=.7, list = FALSE, times = 1)
                   
                   #Membuat Data Latih dan Data Uji
                   data.trainbee <- dataku[indexmatrix.be,]
                   data.testbe <- data.pred
                   
                   data.trainbe = data.trainbee
                   data.trainbe <- data.trainbee[,-1]
                   
                   #Re-label value of kategoric
                   data.trainbe$Klasifikasi[data.trainbe$Klasifikasi==1] <- "ISPA"
                   data.trainbe$Klasifikasi[data.trainbe$Klasifikasi==2] <- "Pneumonia"
                   data.trainbe$Klasifikasi[data.trainbe$Klasifikasi==3] <- "Bukan.keduanya"
                   data.trainbe$Demam[data.trainbe$Demam==0]<- "Tidak"
                   data.trainbe$Demam[data.trainbe$Demam==1]<- "Ya"
                   data.trainbe$Batuk[data.trainbe$Batuk==0]<- "Tidak"
                   data.trainbe$Batuk[data.trainbe$Batuk==1]<- "Ya"
                   data.trainbe$Pilek[data.trainbe$Pilek==0]<- "Tidak"
                   data.trainbe$Pilek[data.trainbe$Pilek==1]<- "Ya"
                   data.trainbe$Sesak[data.trainbe$Sesak==0]<- "Tidak"
                   data.trainbe$Sesak[data.trainbe$Sesak==1]<- "Ya"
                   data.trainbe$Muntah[data.trainbe$Muntah==0]<- "Tidak"
                   data.trainbe$Muntah[data.trainbe$Muntah==1]<- "Ya"
                   data.testbe$Demam[data.testbe$Demam==0]<- "Tidak"
                   data.testbe$Demam[data.testbe$Demam==1]<- "Ya"
                   data.testbe$Batuk[data.testbe$Batuk==0]<- "Tidak"
                   data.testbe$Batuk[data.testbe$Batuk==1]<- "Ya"
                   data.testbe$Pilek[data.testbe$Pilek==0]<- "Tidak"
                   data.testbe$Pilek[data.testbe$Pilek==1]<- "Ya"
                   data.testbe$Sesak[data.testbe$Sesak==0]<- "Tidak"
                   data.testbe$Sesak[data.testbe$Sesak==1]<- "Ya"
                   data.testbe$Muntah[data.testbe$Muntah==0]<- "Tidak"
                   data.testbe$Muntah[data.testbe$Muntah==1]<- "Ya"
                   
                   #convert klasifikasi ke as.factor
                   data.trainbe$Klasifikasi <- as.factor(data.trainbe$Klasifikasi)
                   data.trainbe$Batuk <- as.factor(data.trainbe$Batuk)
                   data.trainbe$Pilek <- as.factor(data.trainbe$Pilek)
                   data.trainbe$Demam <- as.factor(data.trainbe$Demam)
                   data.trainbe$Sesak <- as.factor(data.trainbe$Sesak)
                   data.trainbe$Muntah <- as.factor(data.trainbe$Muntah)
                   data.testbe$Batuk <- as.factor(data.testbe$Batuk)
                   data.testbe$Pilek <- as.factor(data.testbe$Pilek)
                   data.testbe$Demam <- as.factor(data.testbe$Demam)
                   data.testbe$Sesak <- as.factor(data.testbe$Sesak)
                   data.testbe$Muntah <- as.factor(data.testbe$Muntah)
                   
                   #K-Fold Cross Validation
                   controlspecs.be <- trainControl(method = "cv", number = 10,
                                                   savePredictions = "all",
                                                   classProbs = TRUE)
                   
                   #Input random seed sebagai angka acak
                   set.seed(input$set.seed)
                   
                   incProgress(1/3) #Penanda progres loading
                   
                   #NBC
                   naivebayes.be <- train(Klasifikasi~., data=data.trainbe, method="nb", 
                                          trControl=controlspecs.be, tuneLength=0)
                   pred.be=predict(naivebayes.be,data.testbe)
                   hasilnb.be=cbind(data.testbe,pred.be)
                   colnames(hasilnb.be)= c("BB", "Batuk", "Pilek", "Demam", "Sesak", "Muntah", "Prediksi")
                   incProgress(2/3) #Penanda progress loading
                   
                   output$prediksi.nbc<-renderPrint({
                     hasilnb.be
                   })
                   incProgress(3/3) #Penanda progress loading
                 }) #kurung WithProgress
}) #kurung observeEvent
} #kurung server