pres.glmmML<-function(model,data)
  #This function outputs a vector of
  #the Pearson residuals for a glmmML object
  #- "model" is a glmmML object, and "data"
  #is the data frame of the data to which "model"
  #was fitted. This function is based on
  #the glmmML package version 0.65-5 and
  #may not work properly in earlier or later
  #versions. Please report any bugs or
  #errors to Jonathan Rhodes (j.rhodes@uq.edu.au)
{
  Model<-model
  Data<-data
  
  #get the model frame
  ModelFrame<-model.frame(formula(terms(Model)),data=Data)
  
  #get the design matrix
  DesignMatrix<-model.matrix(terms(Model),data=ModelFrame)
  
  #get the linear predictor
  LP<-DesignMatrix %*% as.matrix(Model$coefficients)
  
  #get the cluster
  Cluster<-Data[,toString(Model$call$cluster)]
  Cluster<-as.factor(Cluster)
  
  #add the random-effect posterior modes
  for (i in 1:length(levels(Cluster)))
  {
    LP[Cluster == levels(Cluster)[i]] = LP[Cluster == levels(Cluster)[i]] + Model$posterior.modes[i]
  }
  
  #get the response
  Response<-as.matrix(model.response(ModelFrame))
  
  if (is.null(Model$call$family) || (Model$call$family==as.name(paste("binomial(link = ",dQuote("logit"),")",sep=""))) || (Model$call$family==as.name("binomial")))
    #binomial response with logit link function
  {
    #get the fitted p values
    P<-exp(LP) / (1 + exp(LP))
    
    #get the pearson residuals
    if (ncol(Response) == 1)
      #bernoulli data
    {
      PRes<-(Response - P) / sqrt(P * (1 - P))
    }
    else if (ncol(Response) == 2)
      #binomial data
    {
      PRes<-(Response[,1] - (Response[,1] + Response[,2]) * P) / sqrt((Response[,1] + Response[,2]) * P * (1 - P))
    }
    else
      #response matrix the wrong size
    {
      stop("wrong number of response variables")
    }
    
  }
  else if (Model$call$family==as.name(paste("binomial(link = ",dQuote("cloglog"),")",sep="")))
    #binomial response with complementary log-log link
  {
    
    #get the fitted p values
    P<-1 - exp(-exp(LP))
    
    #get the pearson residuals
    if (ncol(Response) == 1)
      #bernoulli data
    {
      PRes<-(Response - P) / sqrt(P * (1 - P))
    }
    else if (ncol(Response) == 2)
      #binomial data
    {
      PRes<-(Response[,1] - (Response[,1] + Response[,2]) * P) / sqrt((Response[,1] + Response[,2]) * P * (1 - P))
    }
    else
    {
      stop("wrong number of response variables")
    }
  }
  else if ((Model$call$family==as.name(paste("poisson(link = ",dQuote("log"),")",sep=""))) || (Model$call$family==as.name("poisson")))
    #poisson response with log link function
  {
    
    #get the fitted count values
    C<-exp(LP)
    
    #get the pearson residuals
    PRes<-(Response - C) / sqrt(C)
  }
  else
    #response not binomial or poisson
  {
    stop("incompatible family specified")
  }
  
  return(as.vector(PRes))
}