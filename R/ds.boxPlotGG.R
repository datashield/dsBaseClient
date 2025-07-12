#' @title Renders boxplot
#' 
#' @description Internal function. Renders a ggplot boxplot by retrieving from the server side a list with the identity stats and other
#' parameters to render the plot without passing any data from the original dataset
#'
#' @param x \code{character} Name on the server side of the data frame to form a boxplot. Structure on the server 
#' of this object must be: \cr
#' 
#'  Column 'x': Names on the X axis of the boxplot, aka variables to plot \cr
#'  Column 'value': Values for that variable (raw data of columns rbinded) \cr
#'  Column 'group': (Optional) Values of the grouping variable \cr
#'  Column 'group2': (Optional) Values of the second grouping variable \cr
#'  
#' @param group \code{character} (default \code{NULL}) Name of the first grouping variable. 
#' @param group2 \code{character} (default \code{NULL}) Name of the second grouping variable. 
#' @param xlabel \code{caracter} (default \code{"x axis"}) Label to put on the x axis of the plot
#' @param ylabel \code{caracter} (default \code{"y axis"}) Label to put on the y axis of the plot
#' @param type \code{character} Return a pooled plot (\code{"pooled"}) or a split plot (one for each study server
#' \code{"split"})
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return \code{ggplot} object

ds.boxPlotGG <- function(x, group = NULL, group2 = NULL, xlabel = "x axis", ylabel = "y axis", type = "pooled", datasources = NULL){
  x_var <- lower <- upper <- ymin <- ymax <- middle <- fill <- NULL
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  cally <- paste0("boxPlotGGDS(", x, ", ", 
                  if(is.null(group)){paste0("NULL")}else{paste0("'",group,"'")}, ", ", 
                  if(is.null(group2)){paste0("NULL")}else{paste0("'",group2,"'")}, ")")
  
  pt <- DSI::datashield.aggregate(datasources, as.symbol(cally))

  if(type == "pooled"){
    num_servers <- length(names(datasources))
    pt_merged <- NULL
    for(i in 1:num_servers){
      pt_merged <- rbind(pt_merged, pt[[i]]$data)
    }
    pt_merged <- data.table::data.table(pt_merged)
    if(!is.null(group) & is.null(group2)){
      pt_merged <- computeWeightedMeans(pt_merged, 
                        variables = c("ymin", "lower", "middle", "upper", "ymax"), 
                        weight = "n", 
                        by = c("group", "x"))
    }
    else if(!is.null(group) & !is.null(group2)){
      pt_merged <- computeWeightedMeans(pt_merged, 
                        variables = c("ymin", "lower", "middle", "upper", "ymax"), 
                        weight = "n", 
                        by = c("group", "group2", "x"))
    }
    else{
      pt_merged <- computeWeightedMeans(pt_merged, 
                        variables = c("ymin", "lower", "middle", "upper", "ymax"), 
                        weight = "n", 
                        by = c("x"))
    }
    if(pt[[1]][[length(pt[[1]])]] == "single_group"){
      plt <- ggplot2::ggplot(pt_merged) +
        ggplot2::geom_boxplot(stat = "identity", ggplot2::aes(x=x, lower=lower,
                                                              upper=upper, ymin=ymin,
                                                              ymax=ymax, middle=middle,
                                                              fill = group)) +
        ggplot2::scale_fill_brewer(name = "Group") +
        ggplot2::xlab(xlabel) +
        ggplot2::ylab(ylabel) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
    }
    else if(pt[[1]][[length(pt[[1]])]] == "double_group"){
      plt <- ggplot2::ggplot(pt_merged) +
        ggplot2::geom_boxplot(stat = "identity", ggplot2::aes(x=x, lower=lower,
                                                              upper=upper, ymin=ymin,
                                                              ymax=ymax, middle=middle,
                                                              fill = group)) +
        ggplot2::facet_wrap(~ group2) +
        ggplot2::scale_fill_brewer(name = "Group") +
        ggplot2::xlab(xlabel) +
        ggplot2::ylab(ylabel) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
    }
    else{
      plt <- ggplot2::ggplot(pt_merged) +
        ggplot2::geom_boxplot(stat = "identity", ggplot2::aes(x=x, lower=lower,
                                                              upper=upper, ymin=ymin,
                                                              ymax=ymax, middle=middle)) +
        ggplot2::scale_fill_brewer(name = "Group") +
        ggplot2::xlab(xlabel) +
        ggplot2::ylab(ylabel) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
    }
    
  }
  else if(type == "split"){ 
    num_servers <- length(names(datasources))
    plt <- NULL
    for(i in 1:num_servers){
      if(pt[[i]][[length(pt[[i]])]] == "single_group"){
        plt[[i]] <- ggplot2::ggplot(pt[[i]][[1]]) +
          ggplot2::geom_boxplot(stat = "identity", ggplot2::aes(x=x, lower=lower,
                                                                upper=upper, ymin=ymin,
                                                                ymax=ymax, middle=middle,
                                                                fill = group)) +
          ggplot2::scale_fill_brewer(name = "Group") +
          ggplot2::xlab(xlabel) +
          ggplot2::ylab(ylabel) +
          ggplot2::ggtitle(paste0("Server: ", names(datasources[i]))) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
      }
      else if(pt[[i]][[length(pt[[i]])]] == "double_group"){
        plt[[i]] <- ggplot2::ggplot(pt[[i]][[1]]) +
          ggplot2::geom_boxplot(stat = "identity", ggplot2::aes(x=x, lower=lower,
                                                                upper=upper, ymin=ymin,
                                                                ymax=ymax, middle=middle,
                                                                fill = group)) +
          ggplot2::facet_wrap(~ group2) +
          ggplot2::scale_fill_brewer(name = "Group") +
          ggplot2::xlab(xlabel) +
          ggplot2::ylab(ylabel) +
          ggplot2::ggtitle(paste0("Server: ", names(datasources[i]))) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
      }
      else{
        plt[[i]] <- ggplot2::ggplot(pt[[i]][[1]]) +
          ggplot2::geom_boxplot(stat = "identity", ggplot2::aes(x=x, lower=lower,
                                                                upper=upper, ymin=ymin,
                                                                ymax=ymax, middle=middle)) +
          ggplot2::xlab(xlabel) +
          ggplot2::ylab(ylabel) +
          ggplot2::ggtitle(paste0("Server: ", names(datasources[i]))) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
      }
    }

    plt <- do.call(gridExtra::grid.arrange, plt)
    
  }
  else{stop("Invalid type")}
  
  return(plt)
  
}
