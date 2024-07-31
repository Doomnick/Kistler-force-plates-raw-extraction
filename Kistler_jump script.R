
library(dplyr)

# Načtení souboru a hmotností
folder <- "D:/***/3.skupina/SJ" #path to folder
setwd(folder)
pattern <- "_SJ.txt"



pocet_skoku <- 4


fileList <- list.files(path = folder, pattern = pattern )
Name <- fileList

weights <-readxl::read_excel("weights.xlsx")



for (k in 1:length(Name)) {
  New <- Name[k]
  New <- as.character(New)
  id <- gsub(pattern,'',Name[k])
  
  m <- weights$weights[weights$ID==id]
  
  data <- suppressWarnings(readr::read_csv(New, col_names = FALSE,show_col_types = FALSE))
  radek <- which(data[,1] == "Description")
  data <- data[-c(1:radek),]
  data <- data[-2,]
  write.table(data, file = "my_data.csv", col.names = F)
  data <- suppressWarnings(readr::read_table("my_data.csv", show_col_types = FALSE))
  data <- data[,-1]
  names(data)[1:(ncol(data)-2)] <- names(data)[3:ncol(data)]
  data[, ncol(data)] <- NULL
  colnames(data)[1] ="time"
  data <- data[,-10]
  print(paste("Soubor cislo:", k, "/",length(fileList), sep= " "))
  print(fileList[k])
  

  data$time <- as.numeric(gsub("[^0-9.-]", "", data$time))
  data$`|Ft|"` <- as.numeric(gsub("[^0-9.-]", "", data$`|Ft|"`))
  if ("|Cofxy|" %in% colnames(data)) {
  data$`|Cofxy|"` <- as.numeric(gsub("[^0-9.-]", "", data$`|Cofxy|"`))
  }
  
  for(i in 1:length(data)) {
    data[i] <- zoo::rollmean(data[i], k = 3, fill = NA)
  }
  data <- data[-1,]
  A <- data.frame(data$Fz,data$`|Ft|`) 
  A <- data[, 4] + data[, 8]
  

  # data <- data[5000:32000, ]  # ?prava ?et?zce dat

  FR <- data$Fz
  FL <- data$`|Ft|`
  
  tiha <- mean(A[300:1500,])
  
  t <- data$time
  threshold <- 50
  A$Fz[is.na(A$Fz)] <- 0 
  # Ur?en? odrazu a dopadu
  odraz <- dopad <- numeric()
  for (i in 1:(length(A$Fz) - 1)) {
    if (A$Fz[i] > threshold && A$Fz[i + 1] < threshold) {
      odraz <- c(odraz, i + 1)
    }
    if (A$Fz[i] < threshold && A$Fz[i + 1] > threshold) {
      dopad <- c(dopad, i + 1)
    }
  }
  
  if(odraz[1]>dopad[1]) {
    dopad <- dopad[-1]
  }
  


  min_t <- 300

while (min_t < 401) {
  odraz_vym <-c()
  for (i in 1:length(dopad)) {
    if (dopad[i] - odraz[i] < min_t) {
      odraz_vym <- append(i, odraz_vym)
    }
    }
  if (length(odraz_vym) < length(odraz)-pocet_skoku) {
    break
  } 
    min_t <- min_t - 50
     }
 

if (length(odraz_vym)!=0) {
  odraz_vym <- as.numeric(odraz_vym)
  odraz_vym <- odraz_vym[!is.na(odraz_vym)]
  odraz <- odraz[-c(odraz_vym)]
  dopad <- dopad[-c(odraz_vym)]
  
}
  
  if(tail(odraz,1)>tail(dopad,1)) {
    odraz <- odraz[-length(odraz)]
  }
  
  plot(A$Fz, type = 'l', col = 'blue', main = fileList[k], xlab = 'Sample', ylab = 'Force (N)')
  grid()
  points(dopad, A$Fz[dopad], col = "red", pch = 20)
  abline(v = odraz, col = 'red', pch = 19, cex = 2)
  text(odraz*0.98, max(A$Fz)*0.8, seq(1,length(odraz),1),cex=0.65, pos=3,col="red") 
  legend('topright', legend = c('Force', 'Dopad'), col = c('blue', 'red'), pch = c(NA, 19), lty = 1)
  
  print(paste("Pocet odrazu:", length(odraz), sep = " "))
  print(odraz)
  
  
  odraz_vym <- readline("Vymazat odraz? (cislo,cislo.../n): ")
  
  
  if (odraz_vym != "n") {
   odraz_vym <-  as.numeric(unlist(strsplit(odraz_vym, ",")))
   odraz <- odraz[-c(odraz_vym)]
   dopad <- dopad[-c(odraz_vym)]
  }
  
  
  # Ur?en? za??tku odrazu
  drop <- 0.95
  pzacatek <- c()
  zacatek <- rep(0, min(length(odraz), length(dopad)) - 1)
  for (i in 2:length(A$Fz)) {
    if (A$Fz[i - 1] > drop * m * 9.81 && A$Fz[i] < drop * m * 9.81) {
      pzacatek <- c(pzacatek, i)
    }
  }
  
  for (j in 2:length(pzacatek)) {
    for (i in 1:min(length(odraz), length(dopad))) {
      if (pzacatek[j - 1] < odraz[i] && pzacatek[j] > odraz[i]) {
        if (j-2==0) {
          zacatek[i] <- pzacatek[j - 1]
        } else {
        zacatek[i] <- pzacatek[j - 2]
        }
      }
    }
  }
  
  plot(A$Fz, type = 'l', col = 'blue', main = fileList[k], xlab = 'Sample', ylab = 'Force (N)')
  grid()
  abline(v = odraz, col = 'red', pch = 19, cex = 2)
  text(odraz*0.98, max(A$Fz)*0.8, seq(1,length(odraz),1),cex=0.65, pos=3,col="red") 
  legend('topright', legend = c('Force', 'Odraz'), col = c('blue', 'red'), pch = c(NA, 19), lty = 1)
  points(zacatek, A$Fz[zacatek], col = "green", pch = 20)
  
  # P??prava prom?nn?ch
  imp <- impz <- numeric(length(A$Fz))
  Limp <- Pimp <- Limpz <- Pimpz <- numeric(length(A$Fz))
  v <- P <- hmax <- hmax1 <- maxsila <- numeric(length(odraz))
  Limpulsz <- Pimpulsz <- impulsz <- FRmax <- FLmax <- numeric(length(odraz))
  rmsila <- vmax <- Pmax <- pomerimpLP <- pomermaxsilaLP <- numeric(length(odraz))
  vt <- a <- g50 <- g100 <- g200 <- numeric(length(A$Fz))
  
  # V?po?ty
  for (j in 1:min(length(odraz), length(dopad))) {
    for (i in zacatek[j]:odraz[j]) {
      imp[i] <- (A$Fz[i] - m * 9.81) * (t[i + 1] - t[i])
      Limp[i] <- FL[i] * (t[i + 1] - t[i])
      Pimp[i] <- FR[i] * (t[i + 1] - t[i])
      impz[i] <- impz[i - 1] + imp[i]
      Limpz[i] <- Limpz[i - 1] + Limp[i]
      Pimpz[i] <- Pimpz[i - 1] + Pimp[i]
      v[i] <- impz[i] / m
      P[i] <- A$Fz[i] * v[i]
      g50[i] <- A$Fz[i + 25] - A$Fz[i]
      g100[i] <- A$Fz[i + 50] - A$Fz[i]
      g200[i] <- A$Fz[i + 100] - A$Fz[i]
    }
    hmax[j] <- 50 * v[odraz[j]]^2 / 9.81
    hmax1[j] <- 12.5 * 9.81 * (t[dopad[j]] - t[odraz[j]])^2
    pomerimpLP[j] <- Limpz[i] / (Limpz[i] + Pimpz[i])
    Limpulsz[j] <- Limpz[i]
    Pimpulsz[j] <- Pimpz[i]
    impulsz[j] <- Limpulsz[j] + Pimpulsz[j]
    FRmax[j] <- max(FR[(odraz[j] - 50):odraz[j]])
    FLmax[j] <- max(FL[(odraz[j] - 50):odraz[j]])
    maxsila[j] <- max(A$Fz[(odraz[j] - 50):odraz[j]])
    pomermaxsilaLP[j] <- max(FL[(odraz[j] - 50):odraz[j]]) / maxsila[j]
    rmsila[j] <- (maxsila[j]) / m
    vmax[j] <- max(v[(odraz[j] - 50):odraz[j]])
    Pmax[j] <- max(P[(odraz[j] - 50):odraz[j]])
  }
  
  A$time <- data$time
  v <- numeric(length(A$Fz))

  A$time[is.na(A$time)] <- 0

  start_index <- 1
  
  for (i in 2:length(A$Fz)) {
    if (abs(A$Fz[i] - A$Fz[i - 1]) > threshold) {
      start_index <- i
      v[i] <- 0  # Reset velocity when force changes significantly
    } else {
      # Check for NA or NaN limits
      if (!is.na(A$time[i]) && !is.na(A$time[i - 1])) {
        v[i] <- v[start_index] + integrate(Vectorize(function(t) A$Fz[i]), A$time[start_index], A$time[i])$value
      }
    }
  }
 
  
  
  # V?po?et maxim?ln?ch gradient? s?ly
  # Grad 50; grad 100 a grad 200
  
  # Nalezen? nulov?ch bod? - kdy se excentrick? m?n? na koncentrickou
  
  g50 <- numeric(length(odraz))
  g100 <- g50
  g200 <- g50
  gpeak <- g50
  nulovy_bod <- numeric(length(odraz))
  time_to_peak <- numeric(length(odraz))
  
  for (i in 1:(length(odraz))) {
    j <- odraz[i]
        g50[i] <- (A$Fz[j + 25] - A$Fz[j]) / m
        g100[i] <- (A$Fz[j + 50] - A$Fz[j]) / m
        g200[i] <- (A$Fz[j + 100] - A$Fz[j]) / m
        peak_and_ind <- which.max(A$Fz[j:odraz[i]])
        peak <- A$Fz[j + peak_and_ind - 1]
        ind <- peak_and_ind
        time_to_peak[i] <- t[ind + j] - t[j]
        gpeak[i] <- (peak - A$Fz[j]) / (time_to_peak[i] * m)
    }


  # Maxima relativn? hodnoty n?r?stu s?ly vzta?en? na kg t?lesn? hmotnosti
  grad50 <- max(g50)
  grad100 <- max(g100)
  grad200 <- max(g200)
  gradpeak <- max(gpeak)
  ttpeak <- min(time_to_peak[1:(length(odraz) - 1)])
  
  vysledek <- c(m, max(hmax), max(hmax1), max(maxsila), max(rmsila), max(vmax), max(Pmax), max(Pmax) / m, median(pomerimpLP), median(pomermaxsilaLP), grad50, grad100, grad200, gradpeak, ttpeak)
  vysledek1 <- data.frame(hmax, hmax1, maxsila, rmsila, vmax, Pmax, Pmax / m, pomerimpLP, pomermaxsilaLP, Pimpulsz, Limpulsz, FRmax, FLmax)
  vysledek2 <- data.frame(hmax, hmax1, maxsila, rmsila, vmax, Pmax, Pmax / m, impulsz, Pimpulsz, Limpulsz, FRmax, FLmax)
  
  vysledek1$file_name <- fileList[k]
  vysledek1 <- relocate(vysledek1, file_name, .before= hmax)
  
  if(!exists("vysledek_komplet")) {
    vysledek_komplet <- vysledek1
  } else {
    vysledek_komplet <- rbind(vysledek_komplet, vysledek1)
  }
  
  
}

writexl::write_xlsx(vysledek_komplet, paste(getwd(),"/vysledek",pattern,".xlsx", sep =""))