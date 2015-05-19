
################################################################################
# These packages should be all you will need to run the program
################################################################################

# commented out because you will only need to install the packages once
# install.packages("gWidgets")   
# install.packages("gWidgetsRGtk2")
library(gWidgets)
#library(gWidgetsRGtk2)
options("guiToolkit"="RGtk2")

## CHANGE THIS FOR YOUR OWN COMPUTER
setwd("~/laptop/Documents/Spring2015/FW595/Final")
##

################################################################################
# The following code (lines 17-73) are functions needed for the program to run
################################################################################

# function used for printing a file name
fileChoose <- function(action="print", text = "Select a file...", type="open", ...) {
  gfile(text=text, type=type, ..., action = action, handler = function(h,...) {
            do.call(h$action, list(h$file))
          })
}

# Opening window of the program
startDialog <- function(message, handler=NULL) {
  window <- gwindow("Start",width=100,height=100)
   group <- ggroup(horizontal=FALSE,container = window)
  
   inner.group <- ggroup(horizontal=FALSE, container = group)
   glabel(message, container=inner.group)  
   button.group <- ggroup(container = inner.group)
   addSpring(button.group)
  
  ## choose between two types of models
  button.frame <- gframe("Choose Model Type",horizontal=F,cont=button.group)
  analysisType <<- 1
  gradio(c("Basic Occupancy Model","Community Occupancy Model"),
         cont=button.frame,
         selected=1, index=FALSE,
         horizontal=FALSE,  
         handler=function(h,...){
           if (svalue(h$obj)=="Basic Occupancy Model"){
             assign("analysisType", 1,  envir = globalenv()) 
           } else if (svalue(h$obj)=="Community Occupancy Model"){ 
             assign("analysisType", 2,  envir = globalenv())
           }
         })
  gbutton("Start", handler=handler, container=button.group)
  gbutton("cancel", handler = function(h,...) dispose(window),container=button.group) 
   return()
}

# Displays a window with a file for user to see
tabCheck <- function(message, handler=NULL, file = file) {
  window <- gwindow("Check File", width = 800, height= 700)
  g1 <- ggroup(horizontal=FALSE,container = window)
  t1 <<- gtable(file, expand=TRUE, container = g1)
  gbutton("Continue", handler = function(h,...) dispose(window), container=g1)
  return()
}

## select variables to input into the model
startVars <- function(message, handler=NULL) {
  respVar<<- (colnames(datFile)[1])
  window <- gwindow("InterOcc - Select Variables")
  group <- ggroup(horizontal=FALSE,container = window)
  
  inner.group <- ggroup(horizontal=FALSE, container = group)
  glabel(message, container=inner.group)
  button.group <- ggroup(container = inner.group)
  addSpring(button.group)
  
  ## checkboxes for response 
  glabel("Response: ", container=button.group)
  gcheckboxgroup(colnames(datFile), handler = function(h,...) respVar<<-(svalue(h$obj)),  container=button.group)
  
  ## checkboxes for input
  glabel("Detection Variables: ", container=button.group)  
  exVars <<- c()
  gcheckboxgroup(colnames(datFile), handler = function(h,...) exVars<<-(svalue(h$obj)), container=button.group)
  
  ## checkboxes for input
  exVars2 <<- c()
  glabel("Occupancy Variables: ", container=button.group)  
  gcheckboxgroup(colnames(datFile), handler = function(h,...) exVars2<<-(svalue(h$obj)), container=button.group)

   gbutton("Continue", handler = function(h,...) dispose(window), container=button.group)

}

# define priors, model specifics
priors <- function(message, handler=NULL, file = file) {
  window <- gwindow("Priors")
  
  ## create matrix layout to fill with widgets
  g <- glayout(container=window)
  g1 <- ggroup(horizontal=TRUE,container = window)
  
  ## first row is just column labels
  g[1,1] <- (glabel("Parameter",container=g))
  g[1,2] <- (glabel("Prior Distribution:",container=g))
  g[1,3] <- (glabel("Values:",container=g))
  
  ## use different options for each model
if (analysisType==1){
  dist.list <<- c("Normal","Uniform","Gamma","Beta","Bernoulli")
  name.list <<- c("Detection Intercept","Occupancy Intercept")
  
  ## add user inputs to label list
  if (!is.null(exVars))
    name.list <<- c(name.list,paste0("Detection: ",exVars))
  
  if (!is.null(exVars2))
    name.list <<- c(name.list,paste0("Occupancy: ",exVars2))
  
  ## loop through and create new row for each input
  for (i in 1:(length(exVars)+length(exVars2)+2)){
    g[i+1,1] <- (glabel(name.list[i], container=g))
    g[i+1,2] <- (gcombobox(dist.list, selected = 1, editable = TRUE, cont=g))
    g[i+1,3] <- (gedit(text="0,0.001", width=25,container=g))
  }
} else{
  name.list <<- c("alpha","beta","sigmaA","sigmaB","rho")
  
  ## start with prior values
  selection <<- c(rep(1,2),rep(2,3))
  txt <<- c(rep("0,0.001",2),rep("0,10",2),"-1,1")
  
  if (mod.type=="unknown species richness"){
    name.list[6] <- "omega";selection[6] <- 2;txt[6] <- "0,1"}
  
  ## loop through for different type of model
  for (j in 1:length(name.list)){
    g[j+1,1] <- glabel(name.list[j],container=g)
  g[j+1,2] <- (gcombobox(dist.list, selected = selection[j], editable = TRUE, cont=g))
  g[j+1,3] <- (gedit(text=txt[j], width=25,container=g))
  }
}

## function for extracting inputs
extracted <<- extract_g(g)

  gbutton("Return", handler = function(h,...) dispose(window), container=g1)
}

## function for designing MCMC Algorithm
mcmc <- function(message, handler=NULL, file = file) {
  window <- gwindow("MCMC Settings")
  g1 <- ggroup(horizontal=TRUE,container = window)
  g2 <- gframe(horizontal=FALSE,cont=g1)
  
  ## pre-made settings
  n.chains <<- 3
  n.iter <<- 1000
  burn_in <<- 200
  
  ## widgets for altering MCMC
  glabel("Choose number of chains:",cont=g2)
  gcombobox(c(1:5),cont=g2,selected=3, editable=TRUE,
         horizontal=FALSE, coerce.with=as.numeric,handler=function(h,...){
           n.chains <<- svalue(h$obj)})
  
  glabel("\n# of MCMC Iterations:",cont=g2)
  gslider(from=0,to=10000,by=1000,value=1000,cont=g2, handler=function(h,...) n.iter <<- svalue(h$obj))
  
  glabel("\nBurn-in Period:",cont=g2)
  gslider(from=0,to=1000,by=100,value=200,cont=g2, handler=function(h,...) burn_in <<- svalue(h$obj))
  
  g3 <- gframe("Parameters to return:",horizontal=T,cont=g1)
 
  if (analysisType==2 && mod.type=="unknown species richness"){
    name.list[6] <- "omega"
    glabel("# of augmented species",cont=g2)
    n.aug <<- 151
    gedit(text="151",width=25,cont=g2,handler=function(h,...) n.aug <<- as.numeric(svalue(h$obj)))
  }
  gcheckboxgroup(name.list,checked=F,horizontal=F,cont=g3,handler=function(h,...) params.return <<- svalue(h$obj))
  
  gbutton("Done", handler = function(h,...) dispose(window), container=g1)
}

# Run analysis only (no display)
results <- function(message, handler=NULL, file = file) {

  if (analysisType==1){
    r2 <<- extend_occ(exVars,exVars2)
    r1 <<- run_base_occ(datFile,respVar,r2)
  } else{
    r1 <<- run_comm_occ()
  }
}

## display results: posteriors or graphs
record <- function(message,handler=NULL,file=file){
  window <- gwindow("Results",expand=T, width = 1200, height= 400)
  g1 <- ggroup(horizontal=FALSE,container = window,expand=T)

  ## posterior table
  if (form=="numerical"){
  
    if (analysisType==1){gtable(unlist(r2[c(3,6)]),expand=TRUE,cont=g1)}
  gtable(r1$final, expand=TRUE, cont=g1)
  
  ## save to folder
  gbutton("Save Results", handler = function(h,...) {
    dir.create(paste0(getwd(),"/output_files"),showWarnings=F)
    if (analysisType==1){
    write.csv(r1$final,"output_files/base_results.csv",row.names=F)
    capture.output(cat(r1$string), file = "output_files/base_string.txt", append = FALSE)
    }else{
      if (mod.type=="known species richness"){
      write.csv(r1$final,"output_files/comm_known_results.csv",row.names=F)
      capture.output(cat(r1$string), file = "output_files/comm_known_string.txt", append = FALSE)
      } else {
        write.csv(r1$final,"output_files/comm_unknown_results.csv",row.names=F)
        capture.output(cat(r1$string), file = "output_files/comm_unknown_string.txt", append = FALSE)
      }
    }
    
  },container=g1)
  } else{
    
    ## matrix for plotting into
    g2 <- glayout(expand=F,container=window)
    
    ## loop through each parameter requested
    for (k in 1:length(params.return)){
      g2[1,k] <- glabel(params.return[k],cont=g2)
      
      ## trace plots
      g2[2,k] <- ggraphics(container=g2,width=150,height=150)
      par(mar=c(0,0,0,0))
      plot(r1$output[,k],density=F,main=paste0("Trace Plot for: ",params.return[k]))
      
      ## autocorrelation plots
      g2[3,k] <- ggraphics(container=g2,width=150,height=150)
      par(mar=c(0,0,0,0))
      autocorr.plot(r1$output[,k],ask=F,main=paste0("Autocorr Plot for: ",params.return[k]))
      
      ## gelman-rubin rhat plots
      g2[4,k] <- ggraphics(container=g2,width=150,height=150)
      par(mar=c(0,0,0,0))
      gelman.plot(r1$output[,k],ask=F,main=paste0("Gelman-Rubin Plot for: ",params.return[k]))
    }
  }
  gbutton("Return", handler = function(h,...) dispose(window), container=g1)

}
##########################################################################################
#The following code should pop up the Starting window of the program
##########################################################################################

startDialog("Interactive Occupancy Modeling", handler = function(h,...) {
  win <- gwindow("InterOcc - Main", width = 300, height= 350)
  gp <- ggroup(horizontal=FALSE, cont=win) 
  #tmp <- gframe("Type of Analysis", container=gp)  
  
  tmp0 <- gframe("File Upload", container = gp, horizontal=FALSE)
     btn_upload <- gbutton("Upload Data", container = tmp0, expand = TRUE, handler = function(h, ...){ 
          data_name <- fileChoose()
          the_data <- read.csv(data_name, header = TRUE)
          datFile <- "datFile"
          assign(datFile, the_data, envir = globalenv())

  
    gbutton("Preview Data", container=tmp0, expand = TRUE, handler = function(h,...){
      tabCheck("InterOcc - Preview Data", handler = function(h,...){
      }, file = datFile)
    })

  tmp <- gframe("Model Details", container = gp, width = 100, height = 50, horizontal = F)
  if (analysisType==1){  
  gbutton("Select Variables", expand = TRUE, container=tmp, handler = function(h,...){
        startVars("InterOcc - Select Variables", handler = function(h,...){
      })
    })} else {
      #glabel("\nSpecify model type:",cont=tmp)
      mod.type <<- "known species richness"
      gradio(c("known species richness","unknown species richness"),selected=1,cont=tmp,
             handler = function(h,...){
               mod.type <<- svalue(h$obj)
             })
    }
    gbutton("Define Priors", handler = function(h,...){
    priors("Priors", handler = function(h,...){
  })
}, cont=tmp)

gbutton("MCMC Settings", expand = TRUE, container = tmp, handler = function(h, ...){         
  mcmc("MCMC")
  })


  tmp <- gframe("Analysis", container = gp, horizontal=T)

  gbutton("Execute Model", handler = function(h,...){
    results("Results", handler = function(h,...){
    })
  }, cont=tmp)

form <<- "numerical"
gradio(c("numerical","graphical"), cont=tmp, selected=1, index=FALSE,  
       handler=function(h,...){
         form <<- svalue(h$obj)
         
         })

gbutton("View Results",handler=function(h,...){
  record(form, handler = function(h,...){
    })
},cont=tmp)

# gbutton("Return to Main Menu", handler = function(h,...) {
#   startDialog("Interactive Occupancy Modeling")
#   dispose(win)
#   }, cont=gp)
  gbutton("Close Program", handler = function(h,...) dispose(win), cont=gp)
  dispose(h$obj)
  
})

####################################################################################################
#Output variables that have been selected in the GUI
####################################################################################################

## extracts things from glayout and stores as data frame
extract_g <- function(g){
  extracted <- matrix("",nrow(g),ncol(g))
  for (i in 1:nrow(g)){
    for (j in 1:ncol(g)){
      extracted[i,j] <- svalue(g[i,j])
    }
  }
  return(extracted)
}

## specific to my analysis
run_base_occ <- function(datFile,respVar,calcs){
  library(rjags)
  y <- datFile[,respVar]
  M <- nrow(y)
  J <- ncol(y)


  piece1 <-  "\n\nmodel { \n                               # prior distributions\n"
  
  piece2 <- " for(i in 1:M){
              z[i] ~ dbin(psi[i],1) # STATE MODEL
              logit(psi[i]) <- "
  
  piece3 <- "for(t in 1:J){
           logit(p[i,t])<- "
  
  piece4 <- " muy[i,t]<-z[i]*p[i,t]     
           y[i,t] ~ dbin(muy[i,t],1) # OBSERVATION MODEL
}
}
      }"
  model_string <- paste0(piece1, calcs$det.priors,calcs$occ.priors,piece2,
                         calcs$occupancy,piece3,calcs$detect,piece4)

  data2 <- eval(parse(text=calcs$data.list))

  inits <- function(){
              eval(parse(text=calcs$init.list))}
  
  params <- eval(parse(text=calcs$param.list))[which(name.list %in% params.return)] #c("b0","a0")
  
  mod <- jags.model(textConnection(model_string), data=data2, inits=inits, n.chains=n.chains, n.adapt=2000)    #run model
  update(mod,burn_in)
  out<- coda.samples(mod,  params, n.iter=n.iter)  #you may increase this if your computer is quick

  final <- cbind(summary(out)[1]$statistics[,1:2],summary(out)[2]$quantiles)
  final <- cbind(c("Detection",rep("Detection",length(exVars)),"Occupancy",rep("Occupancy",length(exVars2))),
                 c('intercept',exVars,'intercept',exVars2),rownames(final),final)
  colnames(final)[c(1:3,6:10)] <- c("process","name","parameter","q2.5","q25","q50","q75","q97.5")
  return(list("output"=out,"final"=final,"string"=model_string))
}

## extends JAGS model for multiple user inputs
extend_occ <- function(exVars, exVars2)
{
  # c("Normal","Uniform","Gamma","Beta","Bernoulli")
  dist.syn <- c("dnorm(","dunif(","dgamma(","dbeta(","dbern(")
  init.list <- "list(z=rbinom(M,1,1), b0=runif(1), a0=runif(1)"
  data.list <- "list(y=y,M=M,J=J"
  
  detect <- "a0"
  det.priors <- paste0("a0 ~ ",dist.syn[which(dist.list==extracted[2,2])],
                        extracted[2,3],")\n")
  occupancy <- "b0"
  occ.priors <- paste0("b0 ~ ",dist.syn[which(dist.list==extracted[3,2])],
                       extracted[3,3],")\n")
  param.list <- "c('a0','b0'"
  if(!is.null(exVars))
  {
    det.string <- paste0("Detection Covariates: ",paste(exVars,collapse=", "))
    for (i in 1:length(exVars)){
      detect <- paste0(detect," + a",i,"*",exVars[i],"[i,1]")
      #det.priors <- paste0(det.priors,"a",i," ~ dnorm(0,0.001)\n")
      det.priors <- paste0(det.priors,"a",i," ~ ",
                    dist.syn[which(dist.list==extracted[3+i,2])],extracted[3+i,3],")\n")
      data.list <- paste0(data.list,",",exVars[i],"=scale(datFile$",exVars[i],")")
      init.list <- paste0(init.list,", a",i,"=runif(1)")
      param.list <- paste0(param.list,",'a",i,"'")
    }
    detect <- paste0(detect,"\n\n")
    
  } else {
    det.string <- "No Covariates on Detection"
    detect <- paste0(detect,"\n\n")
  }
  
  if(!is.null(exVars2))
  {
    occ.string <- paste0("Occupancy Covariates: ",paste(exVars2,collapse=", "))
    for (i in 1:length(exVars2)){
      occupancy <- paste0(occupancy," + b",i,"*",exVars2[i],"[i,1]")
      #occ.priors <- paste0(occ.priors,"b",i," ~ dnorm(0,0.001)\n")
      occ.priors <- paste0(occ.priors,"b",i," ~ ",
             dist.syn[which(dist.list==extracted[3+length(exVars)+i,2])],
             extracted[3+length(exVars)+i,3],")\n")
      data.list <- paste0(data.list,",",exVars2[i],"=scale(datFile$",exVars2[i],")")
      init.list <- paste0(init.list,", b",i,"=runif(1)")
      param.list <- paste0(param.list,",'b",i,"'")
    }
    occupancy <- paste0(occupancy,"\n\n")
  } else {
    occ.string <- "No Covariates on Occupancy"
    occupancy <- paste0(occupancy,"\n\n")
  }
  
  data.list <- paste0(data.list,")")
  init.list <- paste0(init.list,")")
  param.list <- paste0(param.list,")")
  return(list("detect"=detect,"det.priors"=det.priors,"det.string"=det.string,
              "occupancy"=occupancy,"occ.priors"=occ.priors,"occ.string"=occ.string,
              "data.list"=data.list,"init.list"=init.list,"param.list"=param.list))
}


## community model (analysisType==2)
run_comm_occ <- function(){

  Ymat <- as.matrix(read.csv('detectionFreq.NH17.csv'))
  nrepls <- 11
  nsites <- 50
  Ymat2<-t(Ymat)
  if (mod.type=="unknown species richness"){
  Aug<-matrix(0, 50, n.aug)
  Yaug<-cbind(Ymat2, Aug)
  } else{
    Yaug <- Ymat2
  }

piece1 <- "model { \n\n"

dist.syn <- c("dnorm(","dunif(","dgamma(","dbeta(","dbern(")
piece2 <- paste0("alpha ~ ",dist.syn[which(dist.list==extracted[2,2])],extracted[2,3],")\n",
       "mu.b0 ~ ",dist.syn[which(dist.list==extracted[3,2])],extracted[3,3],")\n",
       "sigma.a0 ~ ",dist.syn[which(dist.list==extracted[4,2])],extracted[4,3],")\n",
       "sigma.b0 ~ ",dist.syn[which(dist.list==extracted[5,2])],extracted[5,3],")\n",
       "tau.a0<-1/(sigma.a0*sigma.a0)\ntau.b0<-1/(sigma.b0*sigma.b0)\n",
       "rho ~ ",dist.syn[which(dist.list==extracted[6,2])],extracted[6,3],")\n",
       "var.a0 <- tau.a0/(1.-pow(rho,2))\n")

if (mod.type=="unknown species richness")
  piece2.2 <- paste0("omega ~ ",dist.syn[which(dist.list==extracted[7,2])],extracted[7,3],")\n")

piece3 <- "\n\nfor(k in 1:K){    # k is species\n"

piece3.2 <- "w[k] ~ dbern(omega)\n"

piece4 <- "b0[k] ~ dnorm(mu.b0, tau.b0)   
mu.a0[k] <- alpha + (rho*sigma.a0/sigma.b0)*(b0[k] - mu.b0)
a0[k] ~ dnorm(mu.a0[k], var.a0)


for(i in 1:M){   #i is site\n"
piece5.1 <- "z[i,k] ~ dbin(psi[i,k],1) # STATE MODEL\n"
piece5.2 <- "z[i,k] ~ dbin(psi[i,k]*w[k],1) # STATE MODEL\n"

piece6 <- "logit(psi[i,k]) <- b0[k]

logit(p[i,k])<-a0[k]
muy[i,k]<-z[i,k]*p[i,k]     
y[i,k] ~ dbin(muy[i,k],nrepls) # OBSERVATION MODEL
}
} 
}"

if (mod.type=="known species richness")
{
model_string <- paste0(piece1,piece2,piece3,piece4,piece5.1,piece6)
} else{
model_string <- paste0(piece1,piece2,piece2.2,piece3,piece3.2,piece4,piece5.2,piece6)
}
data <- list(K=dim(Yaug)[2], M=nsites,y=Yaug,nrepls=nrepls)
n <- dim(Yaug)[2]
params <- c('alpha', 'mu.b0', 'rho', 'sigma.b0', 'sigma.a0')
if(mod.type=="unknown species richness")
  params[6] <- 'omega'
z.guess <- Yaug
z.guess[z.guess > 1] <- 1

inits = function() {
  psi.meanGuess = runif(1, .25,1)
  p.meanGuess = runif(1, .25,1)
  rhoGuess = runif(1, 0,1)
  sigma.uGuess =  runif(1,0,1.5)
  sigma.vGuess =  runif(1,0,1.5)
  res.list <- list(sigma.a0=sigma.uGuess, sigma.b0=sigma.vGuess, rho=rhoGuess,
       b0=rnorm(n, log(psi.meanGuess/(1-psi.meanGuess)), sigma.uGuess),
       a0=rnorm(n, log(p.meanGuess/(1-p.meanGuess)), sigma.vGuess),
       z=z.guess )
  if (mod.type=="unknown species richness")
    res.list$omega <- 0.9
  
  return(res.list)
}
params <- params[which(name.list %in% params.return)]

fit <- jags.model(textConnection(model_string),inits=inits,data=data,n.chains=n.chains,  n.adapt=200)
update(fit,burn_in)
out<- coda.samples(fit,  params, n.iter=n.iter)  
final <- data.frame(summary(out)[1]$statistics[,1:2],summary(out)[2]$quantiles)

final <- cbind(rownames(final),final)
colnames(final)[c(1,4:8)] <- c("name","q2.5","q25","q50","q75","q97.5")
return(list("output"=out,"final"=final,"string"=model_string))
}