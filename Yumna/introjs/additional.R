###DIGIT DECIMAL AND SEPARATOR###

test <- c(1209940.2929,2823773.014,47474821.34433)
dtest<- format(round(as.numeric(test), 2), nsmall=2, big.mark=",")
dftest <- as.data.frame(dtest)

###NOTIFICATION (for button)###
observeEvent(input$inputLogin, {
  showModal(modalDialog(
  title = "Pemberitahuan",
  "Anda berhasil masuk ke RED-CLUWE"
  ))
})

###REMOVING NUMBER COLUMN###
provList <- readRDS("data/provList")
datapath <- paste0("data/", "Aceh", "/")
labour <- readRDS(paste0(datapath, "labour"))
labour$ID <- NULL
labour

