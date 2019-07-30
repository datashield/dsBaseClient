#'
#' @title ds.setDefaultOpals creates a default set of Opal objects called 'default.opals'
#' @description creates a default set of Opal objects called 'default.opals
#' @details By default if there is only one set of opals that is available for
#' analysis, all DataSHIELD client-side functions will
#' use that full set of Opals unless the 'datasources=' argument has been set and specifies that
#' a particular subset of those Opals should be used instead. The correct identification of the full
#' single set of opals is based on the findLoginObjects() function which is an internal DataSHIELD
#' function that is run at the start of nearly every client side function.
#' To illustrate, if the single set of Opals is called 'study.opals' and consists of
#' six opals numbered study.opals[1] to study.opals[6] then all client-side functions will
#' use data from all six of these 'study.opals' unless, say, datasources=study.opals[c(2,5)] is
#' declared and only data from the second and fifth studies will then be used.
#' On the other hand, if there is more than one set of Opals in the analytic environment client-side functions will
#' be unable to determine which set to use. The function findLoginObjects() has therefore been
#' written so that if one of the Opal sets is called 'default.opals' then that set -
#' i.e. 'default.opals' - will be selected by default by all DataSHIELD client-side functions. If there is more
#' than one set of Opals in the analytic environment and NONE of
#' these is called 'default.opals', the function ds.setDefaultOpals() therefore copies
#' one set of opals and to name that copy 'default.opals'. This set will then be selected
#' by default by all client-side functions, unless it is deleted and an alternative set of
#' opals is copied and named 'default.opals'. Regardless how many sets of opals exist and
#' regardless whether any of them may be called 'default.opals', the 'datasources=' argument
#' overrides the defaults and allows the user to base his/her  analysis on any set of opals
#' and any subset of those opals. An earlier version of
#' 'findLoginObjects()' asked the user to specify which Opal to choose
#' if no default could be identified, but that did not work in all versions of R
#' and so has been removed.
#' @keywords internal
#' @return Copies a specified set of Opals (on the client-side server)
#' and calls the copy 'default.opals'
#' @author Burton, PR. 28/9/16
#' @export
#' 

ds.setDefaultOpals.o<-function(opal.name){

    if(is.null(opal.name))stop(" \n\n Please specify a named Opal using the following call syntax:\n ds.setDefaultOpals(opal.name='name of opal in inverted commas')", call.=FALSE)

    eval(parse(text=paste0("default.opals <- ",opal.name)),envir=0)

    findLogin <- getOpals()

    message(paste0("\n\nThere is more than one set of opals available, these are:\n'", paste(findLogin$opals.list,collapse="', '"), "'!!\n\n"))
 
    message(paste0("KEY SUMMARY: THE OBJECT 'default.opals' HAS BEEN CREATED \nAS A COPY OF '", opal.name,"' AND CONSISTS OF:\n"))
#	print(default.opals)
    message(paste0("This opal object 'default.opals' will now be used by default by \nall DataSHIELD client-side functions unless a particular opal object\nis specified via the datasources= argument\n"))	

    message(" \n\nIf you wish to change the set of opals selected as default\nplease run the ds.setDefaultOpals() function again using the call syntax: \nds.setDefaultOpals(opal.name='name of opal in inverted commas')\n\n")

   return("FUNCTION FINISHED")
}
#ds.setDefaultOpals
