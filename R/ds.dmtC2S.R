#' @title Copy a clientside data.frame, matrix or tibble to the serverside
#' @description Creates a data.frame, matrix or tibble on the serverside
#' that is equivalent to that same data.frame, matrix or tibble (DMT) on the clientside. 
#' @details ds.dmtC2S calls assign function dmtC2SDS. To keep the
#' function simple (though less flexible), a number of the parameters specifying
#' the DMT to be generated on the serverside are fixed by the
#' characteristics of the DMT to be copied rather than explicitly
#' specifying them as selected arguments. In consequence, 
#' they have been removed from the list of arguments and are instead given invariant
#' values in the first few lines of code. These include: from="clientside.dmt",
#' nrows.scalar=NULL, ncols.scalar=NULL, byrow = FALSE. The specific value
#' "clientside.dmt" for the argument <from> simply
#' means that the required information is generated from
#' the characteristics of a clientside DMT.  The <nrows.scalar>
#' and <ncols.scalar> are fixed empirically by the number of rows and columns of
#' the DMT to be copied. <byrow> specifies writing the serverside DMT by
#' columns or by rows and this is defaulted to byrow=FALSE i.e. "by column".  
#' @param dfdata is a character string that specifies the name of the DMT 
#' to be copied from the clientside to the serverside 
#' @param newobj A character string specifying the name of the DMT on the serverside 
#' to which the output is to be written. If no <newobj> argument is specified or it is NULL
#' the name of the copied DMT defaults to "dmt.copied.C2S".
#' @param datasources specifies the particular 'connection object(s)' to use.
#' e.g. if you have several data sets in the sources you are working with
#' called opals.a, opals.w2, and connection.xyz, you can choose which of
#' these to work with. The call 'datashield.connections_find()' lists all of
#' the different datasets available and if one of these is called 'default.connections'
#' that will be the dataset used by default if no other dataset is specified. If you
#' wish to change the connections you wish to use by default the call
#' datashield.connections_default('opals.a') will set 'default.connections'
#' to be 'opals.a' and so in the absence of specific instructions to the contrary
#' (e.g. by specifying a particular dataset to be used via the <datasources>
#' argument) all subsequent function calls will be to the datasets held in opals.a.
#' If the <datasources> argument is specified, it should be set without
#' inverted commas: e.g. datasources=opals.a or datasources=default.connections.
#' The <datasources> argument also allows you to apply a function solely to a subset
#' of the studies/sources you are working with. For example, the second source
#' in a set of three, can be specified using a call such as datasources=connection.xyz[2].
#' On the other hand, if you wish to specify solely the first and third sources, the
#' appropriate call will be datasources=connections.xyz[c(1,3)]
#' @return the object specified by the <newobj> argument (or default name "dmt.copied.C2S")
#' which is written as a data.frame/matrix/tibble to the serverside.
#' @author Paul Burton for DataSHIELD Development Team - 3rd June, 2021
#' @export
#'
ds.dmtC2S <- function(dfdata=NA, newobj=NULL, datasources=NULL){

  # if no opal login details are provided look for 'opal' objects in the environment
  if(is.null(datasources)){
	  datasources <- datashield.connections_find()
  }
  
  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }
  
  # check if a value has been provided for dfdata
  if(is.null(dfdata)){
    return("Error: dfdata must be a character string, a numeric vector or a scalar")
  }

  # if no value specified for output object, then specify a default
  if(is.null(newobj)){
    newobj <- "dmt.copied.C2S"
  }

#Generate values for arguments reflecting the key characteristics of the
#clientside matrix/data frame to be copied to the serverside
from<-"clientside.dmt"
nrows.scalar<-NULL
ncols.scalar<-NULL
byrow <- FALSE

#Code that appropriately identifies and differentiates
#between matrices and data.frames

  if(is.data.frame(dfdata))
	{
	  inout.object<-"DF"
	}

  if(is.matrix(dfdata))
	{
	  inout.object<-"MAT"
  }

if(dplyr::is.tbl(dfdata))
{
  inout.object<-"TBL"
}

		
  if(!is.matrix(dfdata) && !is.data.frame(dfdata) && !dplyr::is.tbl(dfdata))
	{
	   message("\n            FAILED: <dfdata> must either be a data.frame, matrix or tibble")	
       return('Please respecify')
	}

   inout.object.transmit<-paste0(as.character(inout.object),collapse=",")

#Create definite matrix version called dfdata.mat
   dfdata.mat<-as.matrix(dfdata)

   
   ###########################################################################
#Allow attributes of dmt to be copied to set key arguments
	nrows.scalar<-dim(dfdata.mat)[1]
	ncols.scalar<-dim(dfdata.mat)[2]	 

	nrows.transmit<-paste0(as.character(nrows.scalar),collapse=",")
	ncols.transmit<-paste0(as.character(ncols.scalar),collapse=",")

#Allow different columns of a matrix or different variables in
#a data.frame to have different classes
	colclass.vector<-rep("",ncols.scalar)

	if(!dplyr::is.tbl(dfdata))
	{
    for(k in 1:ncols.scalar)
    {
 #     colclass.vector[k]<-class(dfdata.mat[,k])
	      {
	      colclass.vector[k]<-class(dfdata[,k])
	      }
    }
	}
	
	if(dplyr::is.tbl(dfdata))
	{
	  for(k in 1:ncols.scalar)
	  {
#	    colclass.vector[k]<-class(dfdata.mat[,k])
	    {
	      colclass.vector[k]<-"numeric"
	    }
	  }
	}
	


colclass.transmit<-paste0(as.character(colclass.vector),collapse=",")


#maintain column/row names if any
colnames.vector<-colnames(dfdata.mat)
if(is.null(colnames.vector))
{
colnames.vector<-as.character(1:ncols.scalar)
}

colnames.transmit<-paste0(as.character(colnames.vector),collapse=",")


rownames.vector<-rownames(dfdata.mat)
rownames.transmit<-paste0(as.character(rownames.vector),collapse=",")
		
dfdata.mat.transmit<-paste0(as.character(dfdata.mat),collapse=",")

#convert all of dfdata.mat.transmit into single elements along a character vector
	dfdata.mat.transmit.elements<-unlist(strsplit(dfdata.mat.transmit, split=""))

#identify and strip " " (i.e. spaces in the individual values in
#the elements of a vector which otherwise get blocked by parser). This
#is only needed when the elements (separate vectors) of a matrix have
#effectively been created in a text editor. However, if this code is
#not built in to check and correct for it, the resultant error is very
#difficult to identify and manage.

length.all<-length(dfdata.mat.transmit.elements)
length.no.spaces<-length(dfdata.mat.transmit.elements[dfdata.mat.transmit.elements!=" "])

dfdata.mat.transmit.elements.no.spaces<-rep("",length.no.spaces)

string.pos<-0

for(n in 1:length.all)
{
  if(dfdata.mat.transmit.elements[n]==" ")
  {
  string.pos<-string.pos
  }
  else
  {
  string.pos<-string.pos+1
  dfdata.mat.transmit.elements.no.spaces[string.pos]<-dfdata.mat.transmit.elements[n]
  }

}

#convert to single long string with no gaps
dfdata.mat.transmit.elements.no.spaces.split<-strsplit(dfdata.mat.transmit.elements.no.spaces,split="")

#rename back to dfdata.mat.transmit
dfdata.mat.transmit<-paste0(dfdata.mat.transmit.elements.no.spaces.split,collapse="")


# CALL THE MAIN SERVER SIDE FUNCTION

calltext <- call("dmtC2SDS", dfdata.mat.transmit, inout.object.transmit, from,
                 nrows.transmit, ncols.transmit, colnames.transmit, colclass.transmit, byrow)


datashield.assign(datasources, newobj, calltext)

}

#############################################################################################################

#ds.dmtC2S



