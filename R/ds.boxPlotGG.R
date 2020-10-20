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
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return \code{ggplot} object



ds.boxPlotGG <- function(x, group = NULL, group2 = NULL, xlabel = "x axis", ylabel = "y axis", type = "pooled", datasources = NULL){
  x_var <- lower <- upper <- ymin <- ymax <- middle <- fill <- NULL
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }

  cally <- paste0("boxPlotGGDS(", x, ", ", 
                  if(is.null(group)){paste0("NULL")}else{paste0("'",group,"'")}, ", ", 
                  if(is.null(group2)){paste0("NULL")}else{paste0("'",group2,"'")}, ")")
  
  pt <- DSI::datashield.aggregate(datasources, as.symbol(cally))
  # return(pt)
  if(type == "pooled"){
    num_servers <- length(names(datasources))
    pt <- lapply(pt, function(x) {x$data$ymin <- x$data$ymin * x$counts$n;
                                  x$data$lower <- x$data$lower * x$counts$n;
                                  x$data$middle <- x$data$middle * x$counts$n;
                                  x$data$upper <- x$data$upper * x$counts$n;
                                  x$data$ymax <- x$data$ymax * x$counts$n;
                                  x$data$PANEL <- as.numeric(x$data$PANEL) * x$counts$n;
                                  x$data$n <- x$counts$n;
                                  x$data$x_var <- rep(x[[2]], each = nrow(x$data)/length(x[[2]]));
                                  x$data$group <- x$data$group * x$counts$n;
                                  return(x)})

    
    # return(pt)
    
    pt_merged <- NULL
    for(i in 1:num_servers){
      pt_merged <- rbind(pt_merged, pt[[i]]$data)
    }
    pt_merged <- data.table::data.table(pt_merged)
    if(!is.null(group)){
      pt_merged <- stats::aggregate(.~fill+x_var, pt_merged, sum)  
    }
    else{pt_merged <- stats::aggregate(.~x_var, pt_merged, sum)  }
    pt_merged$ymin <- pt_merged$ymin / pt_merged$n
    pt_merged$lower <- pt_merged$lower / pt_merged$n
    pt_merged$middle <- pt_merged$middle / pt_merged$n
    pt_merged$upper <- pt_merged$upper / pt_merged$n
    pt_merged$ymax <- pt_merged$ymax / pt_merged$n
    pt_merged$PANEL <- as.numeric(pt_merged$PANEL) / pt_merged$n
    
    # return(pt_merged)
    
    if(pt[[1]][[length(pt[[1]])]] == "single_group"){
      plt <- ggplot2::ggplot(pt_merged) +
        ggplot2::geom_boxplot(stat = "identity", ggplot2::aes(x=x_var, lower=lower,
                                                              upper=upper, ymin=ymin,
                                                              ymax=ymax, middle=middle,
                                                              fill = fill)) +
        ggplot2::scale_fill_brewer(name = "Group") +
        ggplot2::xlab(xlabel) +
        ggplot2::ylab(ylabel) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
    }
    else if(pt[[1]][[length(pt[[1]])]] == "double_group"){
      
      supp.labs <- as.character(levels(pt[[1]][[4]]))
      names(supp.labs) <- seq(from = 1, length.out = length(pt[[1]][[4]]))
      
      plt <- ggplot2::ggplot(pt_merged) +
        ggplot2::geom_boxplot(stat = "identity", ggplot2::aes(x=x_var, lower=lower,
                                                              upper=upper, ymin=ymin,
                                                              ymax=ymax, middle=middle,
                                                              fill = fill, group = group)) +
        ggplot2::facet_wrap(~ PANEL, labeller = ggplot2::labeller(PANEL = supp.labs)) +
        # ggplot2::scale_x_discrete(limits = levels(pt[[1]][[2]])) +
        ggplot2::scale_fill_brewer(name = "Group") +
        ggplot2::xlab(xlabel) +
        ggplot2::ylab(ylabel) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
    }
    else{
      # return(pt_merged)
      plt <- ggplot2::ggplot(pt_merged) +
        ggplot2::geom_boxplot(stat = "identity", ggplot2::aes(x=x_var, lower=lower,
                                                              upper=upper, ymin=ymin,
                                                              ymax=ymax, middle=middle)) +
        # ggplot2::scale_x_discrete(limits = levels(pt[[1]][[2]])) +
        ggplot2::scale_fill_brewer(name = "Group") +
        ggplot2::xlab(xlabel) +
        ggplot2::ylab(ylabel) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
    }
    
  }
  else if(type == "split"){ 
    pt <- lapply(pt, function(x) {
                    x$data$n <- x$counts$n;
                    x$data$x_var <- rep(x[[2]], each = nrow(x$data)/length(x[[2]]));
    return(x)})
    
    num_servers <- length(names(datasources))
    plt <- NULL
    for(i in 1:num_servers){
      if(pt[[i]][[length(pt[[i]])]] == "single_group"){
        plt[[i]] <- ggplot2::ggplot(pt[[i]][[1]]) +
          ggplot2::geom_boxplot(stat = "identity", ggplot2::aes(x=x_var, lower=lower,
                                                                upper=upper, ymin=ymin,
                                                                ymax=ymax, middle=middle,
                                                                fill = fill)) +
          # ggplot2::scale_x_discrete(limits = levels(pt[[i]][[2]])) +
          ggplot2::scale_fill_brewer(name = "Group") +
          ggplot2::xlab(xlabel) +
          ggplot2::ylab(ylabel) +
          ggplot2::ggtitle(paste0("Server: ", names(datasources[i]))) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
      }
      else if(pt[[i]][[length(pt[[i]])]] == "double_group"){
        
        supp.labs <- as.character(levels(pt[[i]][[4]]))
        names(supp.labs) <- seq(from = 1, length.out = length(pt[[i]][[4]]))
        
        plt[[i]] <- ggplot2::ggplot(pt[[i]][[1]]) +
          ggplot2::geom_boxplot(stat = "identity", ggplot2::aes(x=x_var, lower=lower,
                                                                upper=upper, ymin=ymin,
                                                                ymax=ymax, middle=middle,
                                                                fill = fill)) +
          ggplot2::facet_wrap(~ PANEL, labeller = ggplot2::labeller(PANEL = supp.labs)) +
          # ggplot2::scale_x_discrete(limits = levels(pt[[i]][[2]])) +
          ggplot2::scale_fill_brewer(name = "Group") +
          ggplot2::xlab(xlabel) +
          ggplot2::ylab(ylabel) +
          ggplot2::ggtitle(paste0("Server: ", names(datasources[i]))) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
      }
      else{
        plt[[i]] <- ggplot2::ggplot(pt[[i]][[1]]) +
          ggplot2::geom_boxplot(stat = "identity", ggplot2::aes(x=x_var, lower=lower,
                                                                upper=upper, ymin=ymin,
                                                                ymax=ymax, middle=middle)) +
          # ggplot2::scale_x_discrete(limits = levels(pt[[i]][[2]])) +
          # ggplot2::scale_fill_brewer(name = "Group", labels = (pt[[i]][[3]])) +
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