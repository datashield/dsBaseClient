# ds.ranksSecure
#' @title Secure ranking of a vector across all sources
#' @description Securely generate the ranks of a numeric vector and estimate
#' true global quantiles across all data sources simultaneously 
#' @details ds.ranksSecure is a clientside function which calls a series of
#' other clientside and serverside functions to securely generate the global
#' ranks of a numeric vector "V2BR" (vector to be ranked)
#' in order to set up analyses on V2BR based on
#' non-parametric methods, some types of survival analysis and to derive true
#' global quantiles (such as the median, lower (25%) and upper (75%) quartiles,
#' and the 95% and 97.5% quantiles) across all sources simultaneously. These
#' global quantiles are, in general, different to the mean or median of the
#' equivalent quantiles calculated independently in each data source separately.
#' For more details about the cluster of functions that collectively
#' enable secure global ranking and estimation of global quantiles see the 
#' associated document entitled "secure.global.ranking.docx".
#' @param input.var.name a character string in a format that can pass through
#' the DataSHIELD R parser which specifies the name of the vector to be ranked.
#' Needs to have same name in each data source.
#' @param quantiles.for.estimation one of a restricted set of character strings.
#' To mitigate disclosure risk only the following set of quantiles can be
#' generated: c(0.025,0.05,0.10,0.20,0.25,0.30,0.3333,0.40,0.50,0.60,0.6667,
#' 0.70,0.75,0.80,0.90,0.95,0.975). The allowable formats for the argument
#' are of the general form: "0.025-0.975" where the first number is the lowest
#' quantile to be estimated and the second number is the equivalent highest 
#' quantile to estimate. These two quantiles are then estimated along with
#' all allowable quantiles in between. The allowable argument values are then:
#' "0.025-0.975", "0.05-0.95", "0.10-0.90", "0.20-0.80". Two alternative values
#' are "quartiles" i.e. c(0.25,0.50,0.75), and "median" i.e. c(0.50). The
#' default value is "0.05-0.95". If the sample size is so small that an extreme
#' quartile could be disclosive the function will be terminated and an error 
#' message returned telling you that you might try using an argument with a
#' narrower set of quantiles. This disclosure trap will be triggered if the
#' total number of subjects across all studies divided by the total number
#' of quantile values being estimated is less than or equal to nfilter.tab
#' (the minimum cell size in a contingency table).
#' @param generate.quantiles a logical value indicating whether the
#' ds.ranksSecure function should carry on to estimate the key quantile
#' values specified by argument <quantiles.for.estimation> or should stop
#' once the global ranks have been created and written to the serverside.
#' Default is TRUE and as the key quantiles are generally non-disclosive this
#' is usually the setting to use. But, if there is some abnormal configuration
#' of the clusters of values that are being ranked such that some values are
#' treated as being missing and the processing stops, then setting
#' generate.quantiles to FALSE allows the generation of ranks to complete so
#' they can then be used for non-parametric analysis, even if the key values
#' cannot be estimated. A real example of an unusual configuration was in a
#' reasonably large dataset of survival times, where a substantial proportion
#' of survival profiles were censored at precisely 10 years. This meant that
#' the 97.5% percentile could not be separated from the 95% percentile and so
#' the former was allocated the value NA. This stopped processing of the ranks
#' which could then be enabled by setting generate.quantiles to FALSE. However,
#' if this problem is detected an error message is returned which indicates that
#' in some cases (as in this case in fact) the problem can be circumvented
#' by selecting a narrow range of key quantiles to estimate. In this case, in
#' fact, this simply required changing the <quantiles.for.estimation> argument
#' from "0.025-0.975" to "0.05-0.95".
#' @param output.ranks.df a character string in a format that can pass through
#' the DataSHIELD R parser which specifies an optional name for the
#' data.frame written to the serverside on each data source that contains
#' 11 of the key output variables from the ranking procedure pertaining to that
#' particular data source. This includes the global ranks and quantiles of each
#' value of the V2BR (i.e. the values are ranked across all studies
#' simultaneously). If no name is specified, the default name
#' is allocated as "full.ranks.df". This data.frame contains disclosive
#' information and cannot therefore be passed to the clientside.
#' @param summary.output.ranks.df a character string in a format that can pass through
#' the DataSHIELD R parser which specifies an optional name for the summary
#' data.frame written to the serverside on each data source that contains
#' 5 of the key output variables from the ranking procedure pertaining to that
#' particular data source. This again includes the global ranks and quantiles of each
#' value of the V2BR (i.e. the values are ranked across all studies
#' simultaneously). If no name is specified, the default name
#' is allocated as "summary.ranks.df" This data.frame contains disclosive
#' information and cannot therefore be passed to the clientside.
#' @param ranks.sort.by a character string taking two possible values. These
#' are "ID.orig" and "vals.orig". These define the order in which the
#' output.ranks.df and summary.output.ranks.df data frames are presented. If
#' the argument is set as "ID.orig" the order of rows in the output data frames
#' are precisely the same as the order of original input vector that is being
#' ranked (i.e. V2BR). This means the ranks can simply be cbinded to the
#' matrix, data frame or tibble that originally included V2BR so it also 
#' includes the corresponding ranks. If it is set as "vals.orig" the output
#' data frames are in order of increasing magnitude of the original values of
#' V2BR. Default value is "ID.orig".
#' @param shared.seed.value an integer value which is used to set the
#' random seed generator in each study. Initially, the seed is set to be the
#' same in all studies, so the order and parameters of the repeated
#' encryption procedures are precisely the same in each study. Then a 
#' study-specific modification of the seed in each study ensures that the
#' procedures initially generating the masking pseudodata (which are then
#' subject to the same encryption procedures as the real data) are different
#' in each study. For further information about the shared seed and how we
#' intend to transmit it in the future, please see the detailed associated
#' header document.
#' @param synth.real.ratio an integer value specifying the ratio between the
#' number of masking pseudodata values generated in each study compared to
#' the number of real data values in V2BR. 
#' @param NA.manage character string taking three possible values: "NA.delete",
#' "NA.low","NA.hi". This argument determines how missing values are managed
#' before ranking. "NA.delete" results in all missing values being removed
#' prior to ranking. This means that the vector of ranks in each study is 
#' shorter than the original vector of V2BR values by an amount corresponding
#' to the number of missing values in V2BR in that study. Any rows containing
#' missing values in V2BR are simply removed before the ranking procedure is
#' initiated so the order of rows without missing data is unaltered. "NA.low"
#' indicates that all missing values should be converted to a new value that
#' has a meaningful magnitude that is lower (more negative or less positive)
#' than the lowest non-missing value of V2BR in any of the studies. This means,
#' for example, that if there are a total of M values of V2BR that are missing
#' across all studies, there will be a total of M observations that are ranked
#' lowest each with a rank of (M+1)/2. So if 7 are missing the lowest 7 ranks
#' will be 4,4,4,4,4,4,4 and if 4 are missing the first 4 ranks will be 
#' 2.5,2.5,2.5,2.5. "NA.hi" indicates that all missing values should be
#' converted to a new value that has a meaningful magnitude that is higher(less
#' negative or more positive)than the highest non-missing value of V2BR in any
#' of the studies. This means, for example, that if there are a total of M
#' values of V2BR that are missing across all studies and N non-missing
#' values, there will be a total of M observations that are ranked
#' highest each with a rank of (2N-M+1)/2. So if there are a total of 1000
#' V2BR values and 9 are missing the highest 9 ranks will be 996, 996 ... 996.
#' If NA.manage is either "NA.low" or "NA.hi" the final rank vector in each
#' study  will have the same length as the V2BR vector in that same study.
#' 2.5,2.5,2.5,2.5. The default value of the "NA.manage" argument is "NA.delete"
#' @param rm.residual.objects logical value. Default = TRUE: at the beginning
#' and end of each run of ds.ranksSecure delete all extraneous objects that are
#' otherwise left behind. These are not usually needed, but could be of value
#' if one were investigating a problem with the ranking. FALSE: do not delete
#' the residual objects
#' @param monitor.progress logical value. Default = FALSE. If TRUE, function
#' outputs information about its progress.
#' @param datasources specifies the particular opal object(s) to use. If the
#' <datasources> argument is not specified (NULL) the default set of opals
#' will be used. If <datasources> is specified, it should be set without
#' inverted commas: e.g. datasources=opals.em. If you wish to
#' apply the function solely to e.g. the second opal server in a set of three,
#' the argument can be specified as: e.g. datasources=opals.em[2].
#' If you wish to specify the first and third opal servers in a set you specify:
#' e.g. datasources=opals.em[c(1,3)].
#' @return the data frame objects specified by the arguments output.ranks.df
#' and summary.output.ranks.df. These are written to the serverside in each
#' study. Provided the sort order is consistent these data frames can be cbinded
#' to any other data frame, matrix or tibble object containing V2BR or to the
#' V2BR vector itself, allowing the global ranks and quantiles to be
#' analysed rather than the actual values of V2BR. The last call within
#' the ds.ranksSecure function is to another clientside function
#' ds.extractQuantile (for further details see header for that function).
#' This returns an additional data frame "final.quantile.df" of which the first
#' column is the vector of key quantiles to be estimated as specified by the
#' argument <quantiles.for.estimation> and the second column is the list of
#' precise values of V2BR which correspond to these key quantiles. Because
#' the serverside functions associated with ds.ranksSecure and
#' ds.extractQuantile block potentially disclosive output (see information
#' for parameter quantiles.for.estimation) the "final.quantile.df" is returned
#' to the client allowing the direct reporting of V2BR values corresponding to
#' key quantiles such as the quartiles, the median and 95th percentile etc. In
#' addition a copy of the same data frame is also written to the serverside in
#' each study allowing the value of key quantiles such as the median to be
#' incorporated directly in calculations or transformations on the serverside
#' regardless in which study (or studies) those key quantile values have
#' occurred.   
#' @author Paul Burton 4th November, 2021
#' @export
ds.ranksSecure <- function(input.var.name=NULL, quantiles.for.estimation="0.05-0.95",
                           generate.quantiles=TRUE,
                           output.ranks.df=NULL, summary.output.ranks.df = NULL,
                           ranks.sort.by="ID.orig", shared.seed.value=10, 
                           synth.real.ratio=2,NA.manage="NA.delete",
                           rm.residual.objects=TRUE, monitor.progress=FALSE,
                           datasources=NULL){
  
  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  datasources.in.current.function<-datasources
  
  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }
  
  # check if user has provided the name of the column that holds the input variable
  if(is.null(input.var.name)){
    stop("Please provide the name of the variable to be ranked across all sources collectively e.g. 'varname'", call.=FALSE)
  }
  
  # check if user has provided the name of the input variable in a correct character format
  if(!is.character(input.var.name)){
    stop("Please provide the name of the variable that is to be converted to a factor in character format e.g. 'varname'", call.=FALSE)
  }

  # look for output df names and provide defaults if required
  if(is.null(output.ranks.df)){
  output.ranks.df<-"full.ranks.df"
  }

  if(is.null(summary.output.ranks.df)){
    summary.output.ranks.df<-"summary.ranks.df"
  }
  
  if(is.null(synth.real.ratio)){
    synth.real.ratio<-10
  }
 
#CLEAN UP RESIDUAL OBJECTS FROM PREVIOUS RUNS OF THE FUNCTION  
  if(rm.residual.objects)
  {
  #UNLESS THE <rm.residual.objects> IS FALSE,
  #CLEAR UP ANY UNWANTED RESIDUAL OBJECTS FROM THE
  #PREVIOUS RUNNING OF THE ds.ranksSecure FUNCTION IN THE
  #CASE THAT PREVIOUS CALL STOPPED PREMATURELY AND SO THE
  #FINAL CLEARING UP STEP WAS NOT INITIATED.
  
  rm.names<-c("blackbox.output.df", "blackbox.ranks.df",
             "global.bounds.df", "global.ranks.quantiles.df",
             "input.mean.sd.df", "input.ranks.sd.df",
             output.ranks.df, "min.max.df", "numstudies.df",
             "sR4.df", "sR5.df")

  #make transmittable via parser
  rm.names.transmit <- paste(rm.names,collapse=",")
  
  calltext.rm <- call("rmDS", rm.names.transmit)
  
  rm.output <- DSI::datashield.aggregate(datasources, calltext.rm)
  }

  if(monitor.progress){
cat("\n\nStep 1 of 8 complete:
  Cleaned up residual output from
  previous runs of ds.ranksSecure
    
    
    ")
    
    
    }
  
  #CALL AN INITIALISING SERVER SIDE FUNCTION (ASSIGN)
  #TO IDENTIFY QUANTILES OF ORIGINAL VARIABLES IN EACH STUDY
  #TO CREATE A STARTING CONFIGURATION THAT IS ALMOST CERTAINLY >1 WITH A SPAN OF 10
  
  cally0 <- paste0('quantileMeanDS(', input.var.name, ')')
  initialise.input.var <- DSI::datashield.aggregate(datasources, as.symbol(cally0))
  
  
  numstudies<-length(initialise.input.var)
  numvals<-length(initialise.input.var[[1]])
  
  q5.val<-NULL
  q95.val<-NULL
  mean.val<-NULL
  
  for(rr in 1:numstudies){
    q5.val<-c(q5.val,initialise.input.var[[rr]][1])
    q95.val<-c(q95.val,initialise.input.var[[rr]][numvals-1])
    mean.val<-c(mean.val,initialise.input.var[[rr]][numvals])
  }
  
  min.q5<-min(q5.val)
  max.q95<-max(q95.val)
  
  max.sd.input.var<-(max.q95-min.q5)/(2*1.65)
  mean.input.var<-mean(mean.val)
  
  input.mean.sd.df<-data.frame(cbind(mean.input.var,max.sd.input.var))
  
  
  #CALL CLIENTSIDE FUNCTION ds.dmtC2S TO RETURN VALUES TO SERVERSIDE
    dsBaseClient::ds.dmtC2S(dfdata=input.mean.sd.df,newobj="input.mean.sd.df")

if(monitor.progress){
cat("\n\nStep 2 of 8 complete:
  Estimated mean and sd of
  v2br to standardise initial values
    
    
    ")
  }
    
#CALL minMaxRandDS FUNCTION (AGGREGATE) TO CREATE MIN AND MAX VALUES
#FOR INPUT VARIABLE WITH RANDOM NOISE ON TOP. ACTUAL VALUE DOESN'T
#MATTER AS IT IS ONLY TO ALLOCATE LOW AND HIGH VALUES TO NA WHEN
#THEY ARE TO BE INCLUDED IN THE RANKING
    
    calltext0 <- call("minMaxRandDS",input.var.name)
    rand.min.max<-DSI::datashield.aggregate(datasources, calltext0)

    
    numstudies<-length(rand.min.max)

    rand.min.min<-NULL
    rand.max.max<-NULL
    
    for(ss in 1:numstudies){
    rand.min.min<-c(rand.min.min,rand.min.max[[ss]][1])
    rand.max.max<-c(rand.max.max,rand.min.max[[ss]][2])
    }

    min.min.final<-min(rand.min.min)
    max.max.final<-min(rand.max.max)

    min.max.df<-data.frame(cbind(min.min.final,max.max.final))

#CALL CLIENTSIDE FUNCTION ds.dmtC2S TO RETURN VALUES TO SERVERSIDE
dsBaseClient::ds.dmtC2S(dfdata=min.max.df,newobj="min.max.df")
    
if(monitor.progress){
cat("\n\nStep 3 of 8 complete:
  Generated ultra max and ultra min values to allocate to
  missing values if <NA.manage> is NA.hi or NA.low
    
    
    ")
}

print(input.mean.sd.df)


  #CALL THE FIRST SERVER SIDE FUNCTION (ASSIGN)
  #WRITES ENCRYPTED DATA TO SERVERSIDE OBJECT "blackbox.output.df"
  calltext1 <- call("blackBoxDS", input.var.name=input.var.name, 
                    #max.sd.input.var=input.mean.sd.df$max.sd.input.var,
                    #mean.input.var=input.mean.sd.df$mean.input.var,
                    shared.seedval=shared.seed.value,synth.real.ratio,NA.manage)
  DSI::datashield.assign(datasources, "blackbox.output.df", calltext1)

if(monitor.progress){
cat("\n\nStep 4 of 8 complete:
  Pseudo data synthesised,first set of rank-consistent
  transformations complete and blackbox.output.df created
    
    
    ")
  }
  
  #CALL THE SECOND SERVER SIDE FUNCTION (AGGREGATE)
  #RETURN ENCRYPTED DATA IN "blackbox.output.df" TO CLIENTSIDE 
  calltext2 <- call("ranksSecureDS1")
  blackbox.output<-DSI::datashield.aggregate(datasources, calltext2)
  
  numstudies<-length(blackbox.output)
  
  studyid<-rep(1,nrow(blackbox.output[[1]]))
  
  sR3.df<-data.frame(cbind(blackbox.output[[1]],studyid))
  
  
  if(numstudies>=1)
  {
    for(ss in 2:numstudies)
    {
      studyid<-rep(ss,nrow(blackbox.output[[ss]]))
      
      temp.df<-data.frame(cbind(blackbox.output[[ss]],studyid))
      sR3.df<-rbind(sR3.df,temp.df)
    }
  } 
  colnames(sR3.df)<-c(colnames(blackbox.output[[1]]),"studyid")
  
  ord.global.val<-order(sR3.df$encrypted.var)
  sR3.df<-sR3.df[ord.global.val,]
  global.rank<-rank(sR3.df$encrypted.var)
  sR3.sort.global.val.df<-data.frame(cbind(sR3.df,global.rank))
  
  
  #CALL CLIENTSIDE FUNCTION ds.dmtC2S TO RETURN df TO SERVERSIDE
  for(ss in 1:3)
  {
    sR4.df<-sR3.sort.global.val.df[sR3.sort.global.val.df$studyid==ss,]
    dsBaseClient::ds.dmtC2S(dfdata=sR4.df,newobj="sR4.df",
                            datasources = datasources.in.current.function[ss])
  }
  
  numstudies.df<-data.frame(numstudies)
  
  #CALL CLIENTSIDE FUNCTION ds.dmtC2S TO RETURN numstudies TO SERVERSIDE
  dsBaseClient::ds.dmtC2S(dfdata=numstudies.df,newobj="numstudies.df",
                          datasources = datasources.in.current.function)
  
  
  #CALL THE THIRD SERVER SIDE FUNCTION (ASSIGN)
  #SELECTS ENCRYPTED DATA FOR REAL SUBJECTS IN EACH
  #STUDY SPECIFIC sR4.df AND WRITES AS sR5.df ON SERVERSIDE
  calltext3 <- call("ranksSecureDS2")
  DSI::datashield.assign(datasources,"sR5.df",calltext3)

  ds.make("sR5.df$global.rank","testvar.ranks")
  
if(monitor.progress){
cat("\n\nStep 5 of 8 complete:
  Global ranks generated and pseudodata stripped out. Now ready
  to proceed to transformation of global ranks
    
    
    ")
  }
  
  input.ranks.name<-"testvar.ranks"
  
  cally2 <- paste0('quantileMeanDS(', input.ranks.name, ')')
  initialise.input.ranks <- DSI::datashield.aggregate(datasources, as.symbol(cally2))
  
  
  numstudies<-length(initialise.input.ranks)
  numvals<-length(initialise.input.ranks[[1]])
  
  q5.val<-NULL
  q95.val<-NULL
  mean.ranks<-NULL
  
  for(rr in 1:numstudies){
    q5.val<-c(q5.val,initialise.input.ranks[[rr]][1])
    q95.val<-c(q95.val,initialise.input.ranks[[rr]][numvals-1])
    mean.ranks<-c(mean.ranks,initialise.input.ranks[[rr]][numvals])
  }
  
  min.q5<-min(q5.val)
  max.q95<-max(q95.val)
  
  max.sd.input.ranks<-(max.q95-min.q5)/(2*1.65)
  mean.input.ranks<-mean(mean.ranks)
  
  input.ranks.sd.df<-data.frame(cbind(mean.input.ranks,max.sd.input.ranks))
  
  
  #CALL CLIENTSIDE FUNCTION ds.dmtC2S TO RETURN VALUES TO SERVERSIDE
  dsBaseClient::ds.dmtC2S(dfdata=input.ranks.sd.df,newobj="input.ranks.sd.df")
  
  
  
  #CALLS FOURTH SERVER SIDE FUNCTION (ASSIGN)
  #THAT IS A MODIFIED VERSION OF blackBoxDS THAT
  #ENCRYPTS JUST THE RANKS OF THE REAL DATA AND WRITES
  #TO blackbox.ranks.df ON THE SERVERSIDE
  #THIS VERSION (blackBoxDS2) CREATES NO SYNTHETIC DATA TO
  #CONCEAL VALUES 
  

  calltext4 <- call("blackBoxRanksDS","testvar.ranks",
                          shared.seedval=shared.seed.value)

    DSI::datashield.assign(datasources, "blackbox.ranks.df", calltext4)

if(monitor.progress){
cat("\n\nStep 6 of 8 complete:
  Rank-consistent transformations of global ranks complete
  and blackbox.ranks.df created
    
    
    ")
  }

  
  
  #CALL THE FIFTH SERVER SIDE FUNCTION (AGGREGATE)
  #SEND NON-DISCLOSIVE ELEMENTS OF (ENCRYPTED) DATA IN "blackbox.ranks.df"
  #TO CLIENTSIDE 
  
  calltext5 <- call("ranksSecureDS3")
  blackbox.ranks.output<-DSI::datashield.aggregate(datasources, calltext5)
  
  numstudies<-length(blackbox.ranks.output)
  
  sR6.df<-blackbox.ranks.output[[1]]
  
  
  if(numstudies>=1)
  {
    for(ss in 2:numstudies)
    {
      sR6.df<-rbind(sR6.df,blackbox.ranks.output[[ss]])
    }
  } 
  sR6.df<-data.frame(sR6.df)
  colnames(sR6.df)<-c(colnames(blackbox.ranks.output[[1]]))
  
  
  #Rank encrypted ranks across all studies
  real.ranks.global<-rank(sR6.df$encrypted.ranks)
  real.quantiles.global<-real.ranks.global/length(real.ranks.global)
  sR7.df<-cbind(sR6.df,real.ranks.global,real.quantiles.global)
  ord.by.real.ranks.global<-order(sR7.df$real.ranks.global)
  sR7.df.by.real.ranks.global<-sR7.df[ord.by.real.ranks.global,]
  
  
  
  #CALL CLIENTSIDE FUNCTION ds.dmtC2S TO RETURN sR7.df TO SERVERSIDE
  for(ss in 1:3)
  {
    sR7.df.study.specific<-sR7.df.by.real.ranks.global[sR7.df.by.real.ranks.global$studyid==ss,]
    dsBaseClient::ds.dmtC2S(dfdata=sR7.df.study.specific,newobj="global.ranks.quantiles.df",
                             datasources = datasources.in.current.function[ss])
  }
  
  
  
  
  #CALL THE SIXTH SERVER SIDE FUNCTION (ASSIGN)
  #TAKE ALLOCATED GLOBAL RANKS FROM sR7.df APPEND TO blackbox.ranks.df
  #TO CREATE sR9.df
  
  calltext6 <- call("ranksSecureDS4",ranks.sort.by)
  DSI::datashield.assign(datasources,output.ranks.df,calltext6)
  
  
  calltext7 <- call("ranksSecureDS5", output.ranks.df)
  DSI::datashield.assign(datasources,summary.output.ranks.df, calltext7)

  if(monitor.progress){
cat("\n\nStep 7 of 8 complete:
  Final global ranking of values in v2br complete and
  written to each serverside as appropriate
    
    
    ",summary.output.ranks.df)
  }  

  
  
  #CLEAN UP UNWANTED RESIDUAL OBJECTS FROM THE RUNNING OF ds.ranksSecure
  #EXCEPT FOR OBJECTS CREATED BY ds.extractQuantiles

    if(rm.residual.objects)
  {
    #UNLESS THE <rm.residual.objects> IS FALSE,
    #CLEAR UP ANY UNWANTED RESIDUAL OBJECTS
    
    rm.names.rS<-c("blackbox.output.df", "blackbox.ranks.df",
                "global.ranks.quantiles.df","input.mean.sd.df", "input.ranks.sd.df",
                output.ranks.df, "min.max.df", "numstudies.df",
                "sR4.df", "sR5.df")

    #make transmittable via parser
    rm.names.rS.transmit <- paste(rm.names.rS,collapse=",")
    
    calltext.rm.rS <- call("rmDS", rm.names.rS.transmit)
    
#    rm.output.rS <- 
      DSI::datashield.aggregate(datasources, calltext.rm.rS)
    
  }

if(monitor.progress && rm.residual.objects){
cat("\n\nStep 8 of 8 complete:
  Cleaned up residual output from running ds.ranksSecure
    
    
    ")
  }

  if(monitor.progress && !rm.residual.objects){
    cat("\n\nStep 8 of 8 complete:
  Residual output from running ds.ranksSecure NOT deleted
        
        
        ")
  }



if(!generate.quantiles){
  cat("\n\n\n"," FINAL RANKING PROCEDURES COMPLETE:
  PRIMARY RANKING OUTPUT IS IN DATA FRAME",summary.output.ranks.df,
      "
  WHICH IS SORTED BY",ranks.sort.by," AND HAS BEEN
  WRITTEN TO THE SERVERSIDE\n\n\n\n")

    info.message<-"As the argument <generate.quantiles> was set to FALSE no quantiles have been estimated.Please set argument to TRUE if you want to estimate quantiles such as median, quartiles and 90th percentile"
  cat("\n\n",info.message,"\n\n")
    return(info.message)
  }

final.quantile.df<-
  ds.extractQuantiles(       
              quantiles.for.estimation,
              summary.output.ranks.df,
              ranks.sort.by,
              rm.residual.objects,
              extract.datasources=NULL)
  
  return(final.quantile.df)
}

##########################################
#ds.ranksSecure
