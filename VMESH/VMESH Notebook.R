
####################################################################################################
# Notes 
####################################################################################################

# The window sizes and layouts are very customizable
# The I did try out the glayout widget and it was pretty intuitive and could be useful (could be applied to any of the windows) 
# There is a good amount of error checking that will need to be done, such as making sure the variables selected match the model file

################################################################################
# These packages should be all you will need to run the program
################################################################################

# commented out because you will only need to install the packages once
# install.packages("gWidgets")
# install.packages("gWidgetsRGtk2")
library(gWidgets)
options("guiToolkit"="RGtk2")

################################################################################
# The following code are functions needed for the program to run
################################################################################

# function used for printing a file name
fileChoose <- function(action="print", text = "Select a file...", type="open", ...) {
  gfile(text=text, type=type, ..., action = action, handler = function(h,...) {
    do.call(h$action, list(h$file))
  })
}

# Opening window of the program
startDialog <- function(message, handler=NULL) {
  window <- gwindow("Start")
  group <- ggroup(horizontal=FALSE,container = window)
  gimage(filename = "/Users/James/Desktop/vmesh.PNG", container=group)
  
  inner.group <- ggroup(horizontal=FALSE, container = group)
  glabel(message, container=inner.group)  
  button.group <- ggroup(container = inner.group)
  addSpring(button.group)
  gbutton("Start", handler=handler, container=button.group)
  gbutton("cancel", handler = function(h,...) dispose(window),container=button.group)  
  return()
}

# Displays a windown with a file for user to see
tabCheck <- function(message, handler=NULL, file = file) {
  window <- gwindow("Check File", width = 800, height= 700)
  g1 <- ggroup(horizontal=FALSE,container = window)
  gimage(filename = "/Users/James/Desktop/vmesh.PNG", container=g1)
  
  t1 <- gtable(file, expand=TRUE, container = g1)
  gbutton("Continue", handler = function(h,...) dispose(window), container=g1)
  return()
}

# will pop-up a window for selecting the variables
startVars <- function(message, handler=NULL) {
  respVar<<- (colnames(datFile)[1])
  window <- gwindow("VMESH - Select Variables")
  group <- ggroup(horizontal=FALSE,container = window)
  gimage(filename = "/Users/James/Desktop/vmesh.PNG", container=group)
  
  inner.group <- ggroup(horizontal=FALSE, container = group)
  glabel(message, container=inner.group)
  
  button.group <- ggroup(container = inner.group)
  
  addSpring(button.group)
  glabel("Response: ", container=button.group)
  gradio(colnames(datFile), handler = function(h,...) respVar<<-(svalue(h$obj)),  container=button.group)
  glabel("Variables: ", container=button.group)  
  gcheckboxgroup(colnames(datFile), handler = function(h,...) exVars<<-(svalue(h$obj)), container=button.group)
  
  if(analysisType == 3){
    glabel("Censor: ", container=button.group)
    gradio(colnames(datFile),, handler = function(h,...) cens<<-(svalue(h$obj)), container=button.group)
    gbutton("Continue", handler = function(h,...) dispose(window), container=button.group)
  }else{
    gbutton("Continue", handler = function(h,...) dispose(window), container=button.group)
  }
  return()
}


# will pop-up a window for selecting the number of models to display in the output 
# currently you must do this if the model file has less than 20 models 
# The default is set at 20, but will need to be reduced to the number of models in the file if less than 20 
modSlide <- function(message, handler=NULL, file = file) {
  window <- gwindow("Number of Models")
  g1 <- ggroup(horizontal=FALSE,container = window)
  gimage(filename = "/Users/James/Desktop/vmesh.PNG", container=g1)
  glabel("Select the number of models you would like in your output", cont=g1)
  gslider(from=1, to = length(rownames(modFile)), by = 1, value = 20, handler = function(h,...) topn<<-(svalue(h$obj)), cont=g1)
  gbutton("Continue", handler = function(h,...) dispose(window), container=g1)
  return()
}

# Run analysis and display output
# Decided to use the gnotebook option for displaying the graphs 
# This uses the expose handler to display the graphs 
# Had lots of issues with the graph margins being too large with other display set ups 
# Gnotebook should be easy for adding aditional output information 
# Have had some problems with my R session crashing when the display is up for long periods of time
results <- function(message, handler=NULL, file = file) {
  window <- gwindow("Results", visible=FALSE, width = 800, height= 600)
  g1 <- ggroup(container = window, horizontal=FALSE)
  
  #### if statement saying what analysis function to use
  # analysisType == 2 -> aicauto.int.(relresp,xmat,modmat,topn,AICcorr)
  # analysisType == 1 -> aicauto.int.deg
  # analysisType == 3 -> aicauto.int.life(resp,xmat,cens,modmat,topn,AICcorr)
  
  resp <<- unlist(datFile[respVar])
  xmat <<- datFile[exVars]
  modmat <<- modFile
  r1 <<- aicauto.int.rel(resp,xmat,modmat,topn,AICcorr)
  
  nb <- gnotebook(container = g1, tab.pos = 3)
  
  
  gg1 <- ggraphics(container = nb, visible=FALSE, label = "AIC Graph")
  
  ID1 <- addHandlerExpose(gg1, handler=function(h,...) {
    barplot(sortaicval,names.arg=sortmod,xpd=F,main="AIC Ordered Models",xlab="Model",ylab="AIC",ylim=c(min(aicvals,na.rm=T),max(aicvals,na.rm=T)))
    visible(gg1) <- T
    
  })
  
  gg2 <- ggraphics(container = nb, visible=FALSE, label = "Deviance Graph")
  
  ID2 <- addHandlerExpose(gg2, handler=function(h,...) {
    barplot(sortdev,names.arg=sortmod,xpd=F,main="Deviance Ordered Models",xlab="Model",ylab="Deviance",ylim=c(min(devvals),max(devvals)))
    visible(gg2) <- T
  })
  
  gg3 <- ggraphics(container = nb, visible=FALSE, label = "DF vs AIC")
  
  ID3 <- addHandlerExpose(gg3, handler=function(h,...) {
    plot(dfvals,aicvals,main="DF vs AIC",xlab="Model Degrees of Freedom",ylab="AIC")
    visible(gg3) <- T
    
  })
  
  gg4 <- ggraphics(container = nb, visible=FALSE, label = "DF vs Deviance")
  
  ID4 <- addHandlerExpose(gg4, handler=function(h,...) {
    plot(dfvals,devvals,main="DF vs Deviance",xlab="Model Degrees of Freedom",ylab="Deviance")
    visible(gg4) <- T
  })
    
  gtable(r1$Topn, expand=TRUE, cont=g1)

  gbutton("Continue", handler = function(h,...) dispose(window), container=g1)
  visible(window) <- TRUE
  return()
}

aicauto.int.rel <- function(resp,xmat,modmat,topn,AICcorr) {
  xdim <- length(xmat[1,])             # number of columns in Xmatrix
  moddim <- length(modmat[1,])         # number of columns in model matrix
  nummod <- length(modmat[,1])         # number of rows in model matrix
  modlabs <- modmat[,1]
  modmat2 <- modmat[,3:moddim]
  modinteract <- modmat[,2]
  if(xdim == (moddim-2)) {
    aicvals <- rep(9999,nummod)          # vector to store results
    dfvals <<- rep(9999,nummod)
    devvals <<- rep(9999,nummod)
    nobs <- rep(9999,nummod)
    for(i in 1:nummod) {
      # build up x-matrix for individual models
      # basic model
      terms <- modmat2[i,]                  # terms in model
      if(sum(terms=="C")>0) {contfact <- as.matrix(xmat[,terms=="C"])} else {contfact <- NULL}
      dfact <- xmat[,terms=="D"]
      if(sum(terms=="D") == 1) {
        discfact <- factor(dfact)
        levs <- levels(discfact)
        nlev <- length(levs)     
        dcols <- NULL
        for (jj in 2:nlev) {
          dcols <- cbind(dcols,1*(discfact==levs[jj]))
        } # end for jj
        xnew <- cbind(contfact,dcols)
      } # end if
      else if(sum(terms=="D") > 1) {
        dcols <- NULL
        for(k in 1:length(dfact[1,])) {
          discfact <- factor(dfact[,k])
          levs <- levels(discfact)
          nlev <- length(levs)     
          for (jj in 2:nlev) {
            dcols <- cbind(dcols,1*(discfact==levs[jj]))
          } # end for jj
        } # for k
        xnew <- cbind(contfact,dcols)
      } # end  else if
      else {
        xnew <- cbind(contfact)
      } # end else
      if(modinteract[i] == "Y") {
        xbase <- xnew
        dimxbase <- length(xbase[1,])            
        if(dimxbase > 1) {
          for(j in 1:(dimxbase-1)) {
            for (k in (j+1):dimxbase) {
              xnew <- cbind(xnew,xbase[,j]*xbase[,k])
            } # end for k
          } # end for j
        } # end if dimxbase
      } # end if
      
      # run glm reliability for model
      if(is.vector(xnew) | is.matrix(xnew)) {
        temp <- glm(resp ~ .,family=binomial,data=as.data.frame(xnew))
        aicvals[i] <- temp$aic
        dfvals[i] <<- temp$df.null - temp$df.residual
        devvals[i] <<- temp$deviance
        nobs[i] <- temp$df.null + 1
      } # end if
      else {
        temp <- glm(resp ~ 1,family=binomial)
        aicvals[i] <<- NA
        dfvals[i] <<- 0
        devvals[i] <<- temp$deviance
        nobs[i] <- temp$df.null + 1
      } # end else
    }  # for i
  }  # end if
  else {
    print("# of Inputs and Model Columns do not match")
    aicvals <- dfvals <<- NULL
  }  # end else
  # if AICcorrected then adjust value
  if(AICcorr) {
    aicval.unc <- aicvals
    aicvals <<- aicval.unc + (2*dfvals*(dfvals+1))/(nobs-dfvals-1)
  }
  # get ordering of best to worst AIC
  sortaic <- order(aicvals)
  sortmod <<- modlabs[sortaic]
  sortaicval <<- aicvals[sortaic]
  sortaicval2 <- round(aicvals[sortaic],2)
  sortdfval <- dfvals[sortaic]
  sortdev <<- devvals[sortaic]
  sortdev2 <- round(devvals[sortaic],2)
  sortres <- cbind(sortmod,sortaicval,sortdfval,sortdev)
  sortres2 <- cbind(sortmod,sortaicval2,sortdfval,sortdev2)
  if(AICcorr) {
    dimnames(sortres)[[2]] <- c("Model","AICc","DF_model","Deviance")
    dimnames(sortres2)[[2]] <- c("Model","AICc","DF_model","Deviance")
  }
  else {
    dimnames(sortres)[[2]] <- c("Model","AIC","DF_model","Deviance")
    dimnames(sortres2)[[2]] <- c("Model","AIC","DF_model","Deviance")
  }
  # plots
  par(mfcol=c(2,2))
  # range of AIC values from best to worst
  barplot(sortaicval,names.arg=sortmod,xpd=F,xlab="Model",ylab="AIC",ylim=c(min(aicvals,na.rm=T),max(aicvals,na.rm=T)))
  title(main="AIC Ordered Models")
  barplot(sortdev,names.arg=sortmod,xpd=F,xlab="Model",ylab="Deviance",ylim=c(min(devvals),max(devvals)))
  title(main="Deviance Ordered Models")
  
  plot(dfvals,aicvals,xlab="Model Degrees of Freedom",ylab="AIC")
  title(main="DF vs AIC")
  plot(dfvals,devvals,xlab="Model Degrees of Freedom",ylab="Deviance")
  title(main="DF vs Deviance")
  
  if(topn == 0) {
    print(sortres2,quote = FALSE)
    sortressum <- sortres
  } #end if
  else {
    print(sortres2[1:topn,],quote = FALSE)
    sortressum <- sortres[1:topn,]
  } # end else
  return(list(Mod=modlabs,AIC=aicvals,DF=dfvals,Results=sortres,Topn=sortressum))
}

# these two are just proxies for what will be the real functions
# regression
aicauto.int.reg <- function(resp,xmat,modmat,topn,AICcorr) {
  print("This is the regression function")
  return(list(resp,xmat,modmat,topn,AICcorr))
}

# lifetime
aicauto.int.life <- function(resp,xmat,cens,modmat,topn,AICcorr) {
  print("This is the lifetime function")
  return(list(resp,xmat,cens,modmat,topn,AICcorr))
}

##########################################################################################
#The following code should pop up the Starting window of the program
##########################################################################################

startDialog("Variable and Model Evaluation for System Health", handler = function(h,...) {
  win <- gwindow("VMESH - Main", width = 300, height= 500)
  gp <- ggroup(horizontal=FALSE, cont=win) 
  tmp <- gframe("Type of Analysis", container=gp)
  
  analysisType <<- 1
  topn <<- 20
  AICcorr <<- TRUE
  gradio(c("Degradation (Regression)","Reliability (Logistic Regression)","Lifetime (with Censoring)"),
         cont=tmp,
         selected=1, index=FALSE,
         horizontal=FALSE,  
         handler=function(h,...){
           if (svalue(h$obj)=="Degradation (Regression)"){
             assign("analysisType", 1,  envir = globalenv()) 
           } else{ if (svalue(h$obj)=="Reliability (Logistic Regression)"){ 
             assign("analysisType", 2,  envir = globalenv())
           } else {
             assign("analysisType", 3,  envir = globalenv())
           } 
           }
         })
  
  
  tmp0 <- gframe("File Upload", container = gp, horizontal=FALSE)
  btn_upload <- gbutton("Upload Data CSV file", container = tmp0, expand = TRUE, handler = function(h, ...){ 
    data_name <- fileChoose()
    the_data <- read.csv(data_name, header = TRUE)
    datFile <- "datFile"
    assign(datFile, the_data, envir = globalenv())
    ####  will add file name after it is uploaded but need to figure out how to update it if a new file is uploaded to replace the lable before it
    #          glabel("File: ", cont = tmp0)
    #          f <- gtext(data_name, cont = tmp0)
  })
  
  tmp <- gframe("Data File", container=gp)
  gbutton("Check Data", container=tmp, expand = TRUE, handler = function(h,...){
    tabCheck("VMESH - Check Data", handler = function(h,...){
    }, file = datFile)
  })
  
  tmp <- gframe("Select Variables", container = gp)
  gbutton("Select Variables", expand = TRUE, container=tmp, handler = function(h,...){
    startVars("VMESH - Select Variables", handler = function(h,...){
    })
  })
  
  tmp <- gframe("Model Upload", container = gp)
  btn_upload <- gbutton("Upload Model CSV file", expand = TRUE, container = tmp, handler = function(h, ...){         
    models <- read.csv(fileChoose(), header = TRUE)
    modFile <- "modFile"
    assign(modFile, models, envir = globalenv())
    ####  will add file name after it is uploaded but need to figure out how to update it if a new file is uploaded to replace the lable before it
    #   glabel("File: ", cont = tmp)
    #   f <- gtext(mod_name, cont = tmp)
  })
  
  tmp <- gframe("Model File", container=gp)
  gbutton("Check Models", expand = TRUE, container=tmp, handler = function(h,...){
    tabCheck("VMESH - Check Models", handler = function(h,...){
    }, file = modFile)
  })
  
  tmp <- gframe("Output", container = gp, horizontal=FALSE)
  gradio(c("AICc","AIC"), cont=tmp, selected=1, index=FALSE,  
         handler=function(h,...){
           if (svalue(h$obj)=="AICc"){
             assign("AICcorr", TRUE,  envir = globalenv()) 
           } else {
             assign("AICcorr", FALSE,  envir = globalenv())
           }
         }) 
  
  gbutton("Select Number of Models in Output", expand = TRUE, container=tmp, handler = function(h,...){
    modSlide("Number of Models", handler = function(h,...){
    })
  })
  glabel("Top 20 models will be reported by default", cont=tmp)
  
  tmp <- gframe("Run Analysis", container = gp, horizontal=FALSE)
  gbutton("Run", handler = function(h,...){
    results("Results", handler = function(h,...){
    })
  }, cont=tmp)
  gbutton("Cancel", handler = function(h,...) dispose(win), cont=tmp)
  dispose(h$obj)
  
})

####################################################################################################
# Output variables that have been selected in the GUI
# for testing while building the program
# can add "cens" variable
####################################################################################################

# resp <- unlist(datFile[respVar])
# head(resp)
# xmat <- datFile[exVars]
# head(xmat)
# modmat <- modFile
# head(modmat)
# topn
# AICcorr





