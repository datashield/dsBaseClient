#' 
#' @title Return a 'studysideMessage' written by an assign function to serverside
#' @description This function allows for error messages arising from the 
#' running of a server-side assign function to be returned to the client-side
#' @details Errors arising from aggregate server-side functions can be returned
#' directly to the client-side. But this is not possible for server-side assign
#' functions because they are designed specifically to write objects to the
#' server-side and to return no meaningful information to the client-side.
#' Otherwise, users may be able to use assign functions to return disclosive
#' output to the client-side. ds.message.o calls messageDS.o which looks
#' specifically for an object called $serversideMessage in a designated list on
#' the server-side. Server-side functions from which error messages are to be made
#' available, are designed to be able to write the designated error message to
#' the $serversideMessage object into the list that is saved on the server-side
#' as the primary output of that function. So only valid server-side functions of
#' DataSHIELD can write a $studysideMessage, and as additional protection against
#' unexpected ways that someone may try to get round this limitation, a
#' $studysideMessage is a string that cannot exceed a length of nfilter.string
#' a default of 80 characters.  
#' @param message.obj.name is a character string, containing the name of the list containing the
#' message. As an example, the server-side function lexisDS2.o enacts the
#' command:    datashield.assign(datasources, "messageobj", calltext2)
#' As a standard assign function its output is directed to the list object named
#' (in this case) "messageobj". If a studysideMessage is written by DataSHIELD
#' it can be found as messageobj$studysideMessage. To read it, you have to 
#' issue the client-side command: ds.message.o('messageobj'). This tells DataSHIELD
#' to look for the server-side object 'messageobj' and if it finds it, to return any
#' text held in messageobj$studysideMessage. In order to help users to know the name
#' of the server-side list object to ask for in issueing the command:
#' ds.message.o('messageobj') developers are asked to include a message such as:
#' Note3<-"IF FUNCTION FAILED ON ONE OR MORE STUDIES WITHOUT EXPLANATION, TYPE [PRECISELY] THE COMMAND:"
#' Note4<-"ds.message.o('messageobj') FOR MORE ERROR MESSAGES"
#' These represent two of four notes returned by the client-side function ds.lexis.o at the end
#' of each call. In combination, these two notes alert the user to the fact that if there is
#' an error, there may be additional information available in a studysideMessage, and also
#' tells them how to retrieve that message.
#' @param datasources specifies the particular opal object(s) to use, if it is not specified
#' the default set of opals will be used. The default opals are always called default.opals.
#' This parameter is set without inverted commas: e.g. datasources=opals.em or datasources=default.opals
#' If you wish to specify the second opal server in a set of three, the parameter is specified:
#' e.g. datasources=opals.em[2]. If you wish to specify the first and third opal servers in a set specify:
#' e.g. datasources=opals.em[2,3]
#' @return a list object from each study, containing whatever message has been written by
#' DataSHIELD into $studysideMessage.
#' @author Burton PR
#' @export
ds.message.o<-function(message.obj.name=NULL,datasources=NULL){
  
  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
    datasources <- findLoginObjects()
  }

  # Check if user has provided the name of the studyside list object that holds the required message
  # Also check that it is in character format (in inverted commas)
  if(is.null(message.obj.name) | !is.character(message.obj.name)){
    stop("Please provide the name of the studyside list object that holds the message\n in character format ie: 'object.name' in inverted commas", call.=FALSE)
  }

# CALL THE MAIN SERVER SIDE FUNCTION
  calltext <- call("messageDS.o", message.obj.name)
  output.message<-opal::datashield.aggregate(datasources, calltext)
  
#RETURN COMPLETION INFORMATION TO .GlobalEnv
    message("\nMESSAGES FROM STUDYSIDE SERVERS ARE:-\n")
    return(Message=output.message)
}
#ds.message.o


