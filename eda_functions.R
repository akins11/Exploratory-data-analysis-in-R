# library(dplyr)
# library(patchwork)
# library(stringr)
# library(purrr)
# library(scales)

# prop_obs
breakz <- function(df, var) {
  stopifnot(inherits(df, "data.frame"))
  if (! is.numeric(df[[ensym(var)]])) {
    stop("argument `var` must be a numeric variable.")
  }
  
  max_value <-  summarise(df, max = max({{ var }})) %>% pull()
  
  if (max_value <= 400) {
    in_by <- 20
    
  } else if (max_value > 400 & max_value <= 800) {
    in_by <- 50
    
  } else if (max_value > 800 & max_value < 1000) {
    in_by <- 100
    
  } else if (max_value >= 1000 & max_value <= 10000) {
    in_by <- 500
    
  } else if (max_value > 10000 & max_value <= 100000) {
    in_by <- 10000
    
  } else {
    in_by <- 100000
  }
  
  output <- list(to = max_value, by = in_by)
  
  return(output)
}



sub_dep <- function(x) {
  
  newName <- substitute(x)
  
  for (i in seq_len(sys.nframe())) {
    oldName <- do.call("substitute", list(as.name(newName), parent.frame(i)))
    newName <- oldName
  }
  
  output <- deparse(newName)
  return(output)
}




#' Plot Text Position
#'
#' @param df data.frame
#' @param var numeric a variable from the data 'df'.
#' @param input_typ string the data structure of the input either a data.frame
#' or a vector
#' @return
#' @export
#'
#' @examples
text_pos <- function(df, var, input_typ = "df") {
  input_typ <- match.arg(input_typ, c("df", "vec"))

  if (input_typ == "df") {
    max_value <- max(df[[ensym(var)]])
    
  } else if (input_typ == "vec") {
    max_value <- max(var)
  }
  
  if (max_value <= 10) {
    pos <- 1
    
  } else if (max_value <= 100) {
    pos <- 10
    
  } else if (max_value <= 1000) {
    pos <- max_value*0.2
    
  }  else if (max_value <= 100000) {
    pos <- max_value*0.1
  }
  
  return(pos)
}


#' @description create plot pre defined axis labels.
#'
#' @param var_lab variable labels to rename.
#'
axis_label <- function(var_lab) { 
  vars_names <- list(location_code = "Location Area",               
                     monthly_premium_auto = "Monthly Premium",      
                     policy_type = "Type of Policy",                
                     total_claim_amount = "Amount claimed In Total") 
  
  if (var_lab %in% names(vars_names)) {
    lab <- vars_names[[var_lab]]
    
  } else {
    lab <- var_lab |> 
      stringr::str_replace_all("_", " ") |>
      stringr::str_to_title()
  }
  
  return(lab)
}



#' @description Return a vector of different lengths. 
#'
#' @param cond the condition which can be coerced to logical type.
#' @param true values when condition is TRUE.
#' @param false values when condition is FALSE.
om_ifelse <- function(cond, true, false) {
  output <- ifelse(cond,
                   list(true),
                   list(false))[[1]]
  
  return(output)
}



#' Detect Outlier
#'
#' @param df data.frame
#' @param variable numeric: a variable from the data 'df'.
#' @param tbl bool: True to return the output as a data.frame.
#' @param typ return the output in form of a named vector.
#'
#' @details typ can be any of slo - strong lower outlier, lo - lower outlier,
#' uo - upper outlier, suo - strong upper outlier.
get_outlier <- function(df, variable, tbl = TRUE, typ) { 
  c_var <- rlang::ensym(variable)

  if (! is.numeric(df[[c_var]])) {
    stop("argument `variable` must be numeric")
  }
  
  f_tbl <- df %>% 
    summarise(
      `strong lower outlier` = quantile({{ variable }}, 0.25) - IQR({{ variable }})*3,
      
      `lower outlier` = quantile({{ variable }}, 0.25) - IQR({{ variable }})*1.5,
      
      median          = median({{ variable }}),
      
      `upper outlier` = IQR({{ variable }})*1.5 + quantile({{ variable }}, 0.75),
      
      `stong upper outlier` = IQR({{ variable }})*3 + quantile({{ variable }}, 0.75)
    ) %>% 
    pivot_longer(cols = everything(), names_to = "stats", values_to = "value")
  
  if (tbl) {
    return(f_tbl)
    
  } else  {
    outliers <- f_tbl[["value"]]
    names(outliers) <- c("slo", "lo", "med", "uo", "suo")
    
    if (! missing(typ)) {
      if (all(typ %in% c("slo", "lo", "med", "uo", "suo"))) {
        
        return(as.vector(outliers[typ]))
        
      } else {
        stop("Invalid typ, `typ` must be any of 'slo' 'lo' 'med' 'uo' 'suo'")
      }
      
    } else {
      return(outliers)
    }
  }
  
}



#' Filter out outliers
#'
#' @param df data.frame
#' @param var numeric: a variable from the data 'df'.
#' @param outlier string: the type of outlier to filter see get_outlier.
#'
filter_outlier <- function(df, var, outlier) {
  if (length(outlier) == 1) {
    if (outlier %in% c("slo", "lo")) {
      f_tbl <- df %>% 
        dplyr::filter({{ var }} > get_outlier(., {{ var }}, F, outlier))
      
    } else if (outlier %in% c("uo", "suo")) {
      f_tbl <- df %>% 
        dplyr::filter({{ var }} < get_outlier(., {{ var }}, F, outlier))
      
    } else stop("`outlier` should be any of  [slo, lo] or [uo, suo]")
    
  } else if (length(outlier) == 2) {
    if (all(outlier %in% c("slo", "suo"))) {
      outlier[[1]] <- "slo"; outlier[[2]] <- "suo"
      
    } else if (all(outlier %in% c("slo", "uo"))) {
      outlier[[1]] <- "slo"; outlier[[2]] <- "uo"
      
    } else if (all(outlier %in% c("lo", "uo"))) {
      outlier[[1]] <- "lo" ; outlier[[2]] <- "uo"
      
    } else if (all(outlier %in% c("lo", "suo"))) {
      outlier[[1]] <- "lo" ; outlier[[2]] <- "suo"
      
    } else stop("`outlier` should be any of c( [slo, lo], [uo, suo] )")
    
    f_tbl <- df %>% 
      dplyr::filter(between({{ var }},
                            get_outlier(., {{ var }}, F, outlier[[1]]),
                            get_outlier(., {{ var }}, F, outlier[[2]])))
    
  } else stop("length of argument `outlier` must not be greater than two")
  
  return(f_tbl)
}



#' Title
#'
#' @param df data.frame
#' @param var_x numeric: a variable from the data 'df'.
#' @param var_y numeric: a variable from the data 'df'.
#' @param outlier string: the type of outlier to filter see get_outlier.
#'
filter_outlier2 <- function(df, var_x, var_y, outlier) {
  error_ex <- 
    paste("example:\n`list(var='both', outlier=list(x=c('slo', 'suo'),",
          "y='uo')`\n","or `list(var = 'x', outlier = c('lo', 'uo')`", sep = "")
  
  if (! is.list(outlier)) {      
    stop("argument `outlier` must be a list with var and outlier names\n",
         error_ex, sep = "")
  }
  if (! all(names(outlier) %in% c("var", "outlier"))) {
    stop("only names such as var and outlier can be used to create the list\n",
         error_ex, sep = "")
  }
  if (!is.list(outlier$outlier)) {    
    if (! all(names(outlier$outlier) %in% c("x", "y"))) {
      stop("only names such as x and y can be used to create the outlier list\n",
           error_ex, sep = "")
    }
  }
  
  # filter --------------------------------------------------------------------|
  if (outlier$var == "x") {
    f_tbl <- filter_outlier(df, {{ var_x }}, outlier$outlier)
    
  } else if (outlier$var == "y") {
    f_tbl <- filter_outlier(df, {{ var_y }}, outlier$outlier)
    
  } else if (outlier$var == "both") {
    if (typeof(outlier$outlier) == "character") {
      f_tbl <- filter_outlier(df, {{ var_x }}, outlier$outlier) %>% 
        filter_outlier({{ var_y }}, outlier$outlier)
      
    } else if (typeof(outlier$outlier) == "list") {
      f_tbl <- filter_outlier(df, {{ var_x }}, outlier$outlier$x) %>% 
        filter_outlier({{ var_y }}, outlier$outlier$y)
    } else {
      stop("a vector of max 2 or a list of max 2 can only be assgined to outlier")
    }
  }
  
  return(f_tbl)
}


#' Numeric Descriptive Summary
#'
#' @param df data.frame.
#' @param var numeric variable from the data 'df'.
#' @param groups group settings for summarise function.
#' @param pivot TRUE to make the output in a long format, FALSE for wide format.
#'
#' @note Used right_arg
numeric_summary <- function(df, var, groups = "drop_last", pivot = FALSE) {
  right_arg(r_df = df)
  if (! is.numeric(df[[ensym(var)]])) {
    stop("argument `var` must be numeric")
  }
  if (! groups %in% c('drop_last', 'drop', 'keep', 'rowwise')) {
    stop("argument `groups` is incorrect see ?summarise argument `.groups`")
  }
  
  # table summary -------------------------------------------------------------|
  f_tbl <- df %>% 
    summarise(count   = n(),
              minimum = min({{ var }}),
              Q25     = quantile({{ var }}, 0.25),
              mean    = mean({{ var }}),
              median  = median({{ var }}),
              Q75     = quantile({{ var }}, 0.75),
              maximum = max({{ var }}),  
              sum     = sum({{ var }}), .groups = groups)
  
  if (pivot) {
    f_tbl <- f_tbl %>% 
      pivot_longer(cols = count:sum, 
                   names_to  = "statistics", 
                   values_to = "value")
    return(f_tbl)
    
  } else {
    return(f_tbl)
  }
}



#' Correlation Analysis
#'
#' @description calculate the correlation of the variables given to num_varx &
#' num_vary within each categorical group of cat_var variable or performs
#' correlation test.
#' 
#' @param df data.frame.
#' @param num_varx character: a variable from the data `df.
#' @param num_vary numeric: a variable from the data `df.
#' @param cat_var string: type of output either cor.test or grouped correlation.
#' @param typ 
#'
corr_lysis <- function(df, num_varx, num_vary, cat_var, typ = "test") {
  
  varx_s <- sub_dep(num_varx)
  vary_s <- sub_dep(num_vary)
  
  if (! is.numeric(df[[varx_s]]) | ! is.numeric(df[[vary_s]])) {
    stop("argument `num_varx` and `num_vary` must be numeric.")
  }
  
  if (typ == "group") {
    varx_nm <- ensym(num_varx)
    vary_nm <- ensym(num_vary)
    col_name <- paste(varx_nm, "&", vary_nm, "corr")
    
    f_tbl <- df %>% 
      group_by({{ cat_var }}) %>% 
      summarise("{col_name}" := cor({{ num_varx }}, {{ num_vary }}))
    
  } else if (typ == "test") {
    
    f_tbl <- cor.test(df[[varx_s]], df[[vary_s]]) %>% 
      broom::tidy() 
    
  } else {
    stop("argument `typ` can only be either 'group' or 'test'")
  }

  return(f_tbl)
}



#' Sort Character variable
#'
#' @param s_df data.frame
#' @param s_vars vector of variables from the data 's_df' to sort.

#' @note Used om_ifelse
sort_vars <- function(s_df, s_vars) {
  sort_list <- map(s_vars, ~length(unique(s_df[[.x]])))
  names(sort_list) <- s_vars
  
  max_no <- max(sort_list[[s_vars[[1]]]], sort_list[[s_vars[[2]]]])
  
  output <- om_ifelse(sort_list[[s_vars[[1]]]] == max_no,
                      c(s_vars[[1]], s_vars[[2]]),
                      c(s_vars[[2]], s_vars[[1]]))
  return(output)
}

#' Check Valid Arguments
#'
#' @param r_df check data.frame
#' @param r_num_var1 numeric: variable in the data 'r_df'.
#' @param r_num_var2 numeric: variable in the data 'r_df'.
#' @param r_cat_var1 character: variable in the data 'r_df'.
#' @param r_cat_var2 character: variable in the data 'r_df'.
#' @param r_typ string: type of function output.
#' @param num_var_name vector: name|s| of the numeric variables.
#' @param cat_var_name vector: name|s| of the character variables.
#' @param others list: other argument to check.
#' 
#' @note Used om_ifelse, sub_dep
right_arg <- function(r_df, 
                      r_num_var1, r_num_var2, 
                      r_cat_var1, r_cat_var2, 
                      r_typ, num_var_name, cat_var_name, others) {
  num_name <- om_ifelse(missing(num_var_name),
                        c("num_var1", "num_var2"),
                        num_var_name)
  cat_name <- om_ifelse(missing(cat_var_name),
                        c("cat_var1", "cat_var2"),
                        cat_var_name)
  
  # data ----------------------------------------------------------------------|
  if (! inherits(r_df, "data.frame")) {
    stop("argument `df` must be a data.frame")
  }
  
  # numeric -------------------------------------------------------------------|
  if (! missing(r_num_var1)) {
    r_num_var1 <- sub_dep(r_num_var1)
    
    if (! missing(r_num_var2)) {
      r_num_var2 <- sub_dep(r_num_var2)
      
      if (! is.numeric(r_df[[r_num_var1]]) | ! is.numeric(r_df[[r_num_var2]])) {
        stop(paste0("argument `",num_name[1],"` and `",num_name[2],"` must be numeric"))
      }
      
    } else {
      if (! is.numeric(r_df[[r_num_var1]])) {
        stop(paste0("argument `",num_name[1],"` must be numeric"))
      }
    }
  }
  
  # character -----------------------------------------------------------------|
  if (! missing(r_cat_var1)) {
    r_cat_var1 <- sub_dep(r_cat_var1)
    
    if (! missing(r_cat_var2)) {
      r_cat_var2 <- sub_dep(r_cat_var2)
      
      if (!is.character(r_df[[r_cat_var1]]) | !is.character(r_df[[r_cat_var2]])) {
        stop(paste0("argument `",cat_name[1],"` and `",cat_name[2],"` must be character"))
      }
      
    } else {
      if (!is.character(r_df[[r_cat_var1]])) {
        stop(paste0("argument `",cat_name[1],"` must be character"))
      }
    }
  }
  
  # other arguments -----------------------------------------------------------|
  if (! missing(others)) {
    if (is.list(others)) {
      if (! others$check %in% others$rvs) {
        stop(paste(others$check, "is incorrect use any of", 
                   paste(others$rvs, collapse = ", ")))
      }
      
    } else {
      stop("value supplied to others must be a list")
    }
  }
  
  # output --------------------------------------------------------------------|
  if (! missing(r_typ)) {
    if (! r_typ %in% c("plt", "tbl")) {
      stop("argument `typ` must either be 'tbl' for table or 'plt' for plot.")
    }
  }
}


#' Unique Category Count
#'
#' @param df data.frame
#' @param var character variable to summarise.
#' @param colour string: color for the bars.
#' @param bk numeric: plot seq breaks.
#' @param typ string: type of output 'plt' for plot, 'tbl' for table.
#'
#' @note Used breakz, om_ifelse.
count_obs <- function(df, var, colour, bk, typ = "plt") {
  right_arg(r_df = df, r_cat_var1 = var, cat_var_name = "var", r_typ = typ)
  
  f_tbl <- df %>% 
    count({{ var }}, sort = TRUE, name = "count") %>% 
    mutate(prop = round(prop.table(count)*100, 2))
  
  f_l <- axis_label(sub_dep(var))
  
  bks <- breakz(f_tbl, count)
  
  p_breaks <- om_ifelse(missing(bk), c(bks$to, bks$by), bk)
  
  f_plt <- f_tbl %>% 
    ggplot(aes(fct_reorder({{ var }}, count, .desc = T), count)) +
    geom_col(fill = ifelse(missing(colour), "#4c9085", colour)) +
    geom_text(aes(label = paste0(prop, "%")), vjust = -0.2) +
    scale_y_continuous(breaks = seq(0, p_breaks[[1]], p_breaks[[2]]),
                       labels = scales::comma_format()) +
    labs(x = "", y = "", title = paste("Count of Customers By", f_l)) +
    theme_minimal()
  
  output <- switch(typ,
                   plt = f_plt,
                   tbl = f_tbl)
  return(output)
}



#' Title
#'
#' @param p_df summarized data frame.
#' @param sub_t string: plot subtitle.
#' @param colors vector: two colors to fill the bars & label.
#'
co_plt_func <- function(p_df, sub_t, colors) {
  ggplot(p_df, aes(count, fct_reorder(y_ticks_l, count))) +
    geom_col(fill = colors[[1]]) +
    geom_label(aes(label = scales::comma(count), x = text_pos), 
               size  = 3.3, 
               color = "white",
               fill  = colors[[2]],
               label.r = unit(0.25, "lines"), 
               label.padding = unit(0.15, "lines") ) +
    labs(x = "", y = "", label = "", subtitle = sub_t) +
    theme_minimal() +
    theme(axis.text.x = element_blank())
}



#' Title
#'
#' @param df data.frame
#' @param var_1 character: first & Second variable to group by.
#' @param var_2 character: first & Second variable to group by.
#' @param colours vector: two colors to fill the bars & label.
#' @param typ character: type of output plt - plot, tbl - table.
#' 
#' @note Used right_arg, sort_vars, om_ifelse, co_plt_func, sub_dep.
count_obs2 <- function(df, var_1, var_2, colours, typ = "plt") {
  vars_s <- c(sub_dep(var_1), sub_dep(var_2))
  
  right_arg(r_df = df, 
            r_cat_var1 = var_1, 
            r_cat_var2 = var_2, 
            cat_var_name = c("var_1", "var_2"),
            r_typ = typ)

  res <- sort_vars(df, vars_s)
  
  var1 <- res[[1]]
  var2 <- res[[2]]
  
  f_tbl <- df %>% 
    group_by(.data[[var1]], .data[[var2]]) %>% 
    count(name = "count") %>% 
    ungroup()
  
  f_tbl_p <- f_tbl %>% 
    mutate(y_ticks_l = str_wrap(.data[[var1]], 10))
    
  
  nm_cat <- length(unique(f_tbl_p[[var2]])) |> as.character()
  var_cat <- unique(f_tbl_p[[var2]])
  var_cat_label <- str_replace_all(var_cat, "-|_", " ") |> str_to_title()
  
  var1_label <- axis_label(var1)
  var2_label <- axis_label(var2)
  
  colours <- om_ifelse(missing(colours), c("#4c9085", "#0f9b8e"), colours)
  
  filter_tbl <- function(v_cat) {
      filter(f_tbl_p, .data[[var2]] == v_cat) %>%
      mutate(text_pos = text_pos(var = count, input_typ = "vec")) 
  }
  
  plt_list <- map(var_cat, filter_tbl) %>% 
    map2(var_cat_label, co_plt_func, colours)
  
  f_plt <- switch(nm_cat,
                  "2" = (plt_list[[1]]+plt_list[[2]]),
                  "3" = (plt_list[[1]]+plt_list[[2]]+plt_list[[3]]),
                  "4" = (plt_list[[1]]+plt_list[[2]]+plt_list[[3]]+plt_list[[4]]),
                  "5" = (plt_list[[1]]+plt_list[[2]]+plt_list[[3]]+plt_list[[4]]+
                        plt_list[[5]]))
  f_plt <- f_plt +
    plot_annotation(paste("Number Of Customers By",var1_label,"And",var2_label))
  
  output <- switch(typ,
                   plt = f_plt,
                   tbl = f_tbl)
  return(output)
}



#' Proportion of Unique Values
#'
#' @param df data.frame
#' @param var_1 character: first & Second variable to group by and summarise.
#' @param var_2 character: first & Second variable to group by and summarise.
#' @param colours vector: two colors to fill the bars & label.
#' @param typ string: type of output plt - plot, tbl - table.
#' 
#' @note Used right_arg, sort_vars, axis_label, om_ifelse.
#'
prop_obs <- function(df, var_1, var_2, colours, typ = "plt") {
  right_arg(r_df = df, 
            r_cat_var1 = var_1,
            r_cat_var2 = var_2,
            cat_var_name = c("var_1", "var_2"),
            r_typ = typ)
  
  vars_s <- c(sub_dep(var_1), sub_dep(var_2))
  res <- sort_vars(df, vars_s)
  
  var1 <- res[[1]]
  var2 <- res[[2]]
  
  f_tbl <- df %>% 
    group_by(.data[[var2]], .data[[var1]]) %>% 
    count(name = "count") %>% 
    group_by(.data[[var1]]) %>% 
    mutate(prop = round(prop.table(count)*100, 1)) %>% 
    arrange(.data[[var2]]) %>% 
    ungroup()
  
  num_uq <- unique(f_tbl[[var2]]) |> length() |> as.character()
  colrs  <- dplyr::lst("2" = c("#0a888a", "#41fdfe"),
                       "3" = append(`2`, "#06b1c4"),
                       "4" = append(`3`, "#03719c"))
  
  colours <- om_ifelse(missing(colours), colrs[[num_uq]], colours)
  
  var1_l <- axis_label(var1)
  var2_l <- axis_label(var2)
  
  f_plt <- ggplot(f_tbl, aes(.data[[var1]], prop, fill = .data[[var2]])) +
    geom_col(position = "dodge", color = "white") +
    geom_text(aes(label = paste0(prop, "%")), position = position_dodge(.9),
              vjust = 1) +
    scale_fill_manual(name = var2_l, values = colours) +
    ggtitle(paste("Percentage Of", var2_l, "For Each", var1_l)) +
    theme_minimal() +
    theme(axis.title = element_blank(), axis.text.y = element_blank())
  
  output <- switch(typ,
                   plt = f_plt,
                   tbl = f_tbl)
  return(output)
}


fd <- function(x) {
  n <- length(x)
  r <- IQR(x, na.rm = TRUE)
  
  2*r / n^(1/3)
}


#' Numeric Distribution
#'
#' @param df data.frame
#' @param num_var numeric: a variable to summarise from the data 'df'.
#' @param colors vector: colors for the plot.
#' @param outlier vector: filter out outliers in the data. any of slo, lo,
#' uo, suo.
#' @param typ string: type of output plt - plot, tbl - table.
#' 
#' @note right_arg, filter_outlier, numeric_summary, om_ifelse, axis_label, sub_dep.
#'
numeric_dis <- function(df, num_var, colors, outlier, typ = "plt") {
  right_arg(r_df = df, r_num_var1 = num_var, r_typ = typ)
  
  if (! missing(outlier)) {
    f_df <- filter_outlier(df, {{ num_var }}, outlier)
    
  } else {
    f_df <- df
  }
  
  f_tbl <- f_df %>% 
    numeric_summary({{ num_var }}, pivot = TRUE)

  colors <- om_ifelse(missing(colors),
                      c("#045c5a", "#cafffc"),
                      colors)

  p_l <- axis_label(sub_dep(num_var))
  
  f_plt_1 <- f_df %>% 
    ggplot(aes(y = {{ num_var }})) +
    geom_boxplot(color = colors[[1]], fill = colors[[2]]) +
    labs(y = "") +
    scale_y_continuous(labels = scales::comma_format()) +
    theme_minimal() +
    theme(axis.text.x = element_blank())
  
  f_plt_2 <- f_df %>% 
    ggplot(aes({{ num_var }})) +
    geom_histogram(binwidth = fd, fill = colors[[1]]) +
    scale_x_continuous(labels = scales::comma_format()) +
    scale_y_continuous(labels = scales::comma_format()) +
    labs(x = "", y = "Count") +
    theme_minimal()
  
  f_plt <- (f_plt_1+f_plt_2) +
    plot_layout(widths = c(1.2, 1.8)) +
    plot_annotation(title = paste("Distribution Of", p_l))
  
  output <- switch(typ,
                   plt = f_plt,
                   tbl = f_tbl)
  return(output)
}



#' Numerical & Categorical Distribution
#'
#' @param df data.frame:
#' @param num_var numeric: a variable from the data df.
#' @param cat_var character: a variable from the data df.
#' @param colors vector: a vector of length 2 containing colors for the plot.
#' @param outlier vector: to filter outliers from the data.
#' @param p_typ string: the type of plot output either 'box' or 'fqp'.
#' 
#' @note Used right_arg, filter_outlier, om_ifelse, sub_dep, axis_label.
#'
num_cat_dis <- function(df, num_var, cat_var, colors, outlier, p_typ = "box") {

  right_arg(r_df = df, 
            r_num_var1 = num_var, 
            r_cat_var1 = cat_var,
            others = list(check = p_typ, rvs = c("box", "fqp", "vio")))
  
  if (! missing(outlier)) {
    f_tbl <- filter_outlier(df, {{ num_var }}, outlier)
    
  } else  f_tbl <- df
  
  colors <- om_ifelse(missing(colors),
                      c("#90e4c1", "#3d9973"),
                      colors)

  cat_l <- axis_label(sub_dep(cat_var))
  num_l <- axis_label(sub_dep(num_var))
  p_title <- paste("Distribution of", num_l, "By", cat_l)
  
  bk <- breakz(f_tbl, {{ num_var }})
  
  no_cat <- f_tbl[[sub_dep(cat_var)]] |> unique() |> length()
  av_color <- c("violetred", "seagreen", "navy", "tan4", "goldenrod3", "cyan3")
  sl_color <- av_color[1:no_cat]
  
  add_layer <- function(fqp = FALSE) {
    if (fqp) {
      add_l <- 
        list(scale_y_continuous(labels = scales::comma_format()),
             scale_x_continuous(labels = scales::comma_format(),
                                breaks = seq(0, bk$to, bk$by)),
             scale_color_manual(values = sl_color)) 
    } else {
      add_l <- 
        list(scale_y_continuous(labels = scales::comma_format(),
                                breaks = seq(0, bk$to, bk$by)),
             labs(x = "", y = "", title = p_title))
    }
    
    add_l[[length(add_l)+1]] <- theme_minimal()
    
    return(add_l)
  }
  
  f_plt_box <- f_tbl %>% 
    ggplot(aes(fct_reorder({{ cat_var }}, {{ num_var }}, .desc = TRUE), 
               {{ num_var }})) +
    geom_boxplot(fill = colors[[1]], color = colors[[2]]) +
    add_layer()
  
  f_plt_vio <- f_tbl %>% 
    ggplot(aes(fct_reorder({{ cat_var }}, {{ num_var }}, .desc = TRUE), 
               {{ num_var }})) +
    geom_violin(fill=colors[[1]], color=colors[[2]], trim=F, draw_quantiles=.5) +
    add_layer()
  
  f_plt_fqp <- f_tbl %>% 
    ggplot(aes({{ num_var }}, color =  {{ cat_var }})) +
    geom_freqpoly(binwidth = fd) +
    add_layer(fqp = TRUE) +
    labs(x = "", y = "Count", title = p_title, color = cat_l)

  output <- switch(p_typ,
                   box = f_plt_box,
                   fqp = f_plt_fqp,
                   vio = f_plt_vio)
  return(output)
}


#' numerical & categorical stat summary plot layer
#'
#' @param sub_typ string: either 'min' or 'max'.
#' @param colr vector: 2 colors for the plot.
#'
#' @return
#' @export
#'
#' @examples
num_cat_sumy_add_layer <- function(sub_typ, colr) {
  add_layer <- list(
    geom_point(size  = ifelse(sub_typ == "min", 3, 6), 
               stroke = 1.5,
               color = colr[[1]], 
               fill  = alpha(colr[[2]], 0.3), 
               alpha = 0.7),
    labs(x = "", 
         y = "", 
         subtitle = ifelse(sub_typ == "min", "Minimum", "Maximum")),
    scale_x_continuous(labels = scales::comma_format()),
    theme_minimal()
  )
  
  add_layer[[length(add_layer)+1]] <- switch(
    sub_typ,
    min = theme(plot.subtitle = element_text(color = colr[[1]], face = "italic")),
    max = theme(plot.subtitle = element_text(color = colr[[1]], face = "italic"),
                axis.text.y   = element_blank() )
  )
  
  return(add_layer)
}



#' Numerical & Categorical stat summary
#'
#' @param df data.frame:
#' @param num_var numeric: a variable from the data df.
#' @param cat_var character: a variable from the data df.
#' @param outlier vector: to filter outliers from the data.
#' @param sumy_fun numeric: type of summary to use either mean, median or sum.
#' @param p_typ string: the type of plot to create either 'avg_tot' or 'min_max'
#' @param min_color vector: 2 colors for the minimum plot when sumy = 'min_max'.
#' @param max_color vector: 2 colors for the maximum plot when sumy = 'min_max'.
#' @param typ string: the type of output either 'plt'-plot or 'tbl'-table.
#'
#' @details outlier can be any of 'slo', 'lo', 'uo', 'suo' see get_outlier
#' @note Used filter_outlier, numeric_summary, om_ifelse, num_cat_sumy_add_layer,
#' sub_dep, axis_label.
num_cat_sumy <- function(df, 
                         num_var, cat_var, 
                         outlier, sumy_fun = median,  p_typ = "avg_tot",   
                         min_color, max_color, typ = "plt") {
  right_arg(r_df = df, 
            r_num_var1 = num_var,
            r_cat_var1 = cat_var,
            r_typ = typ,
            others = list(check = p_typ, rvs = c("avg_tot", "min_max")))
  
  if (! missing(outlier)) {
    f_tbl <- filter_outlier(df, {{ num_var }}, outlier)
    
  } else f_tbl <- df
  
  f_tbl <- f_tbl %>% 
    group_by({{ cat_var }}) %>% 
    numeric_summary({{ num_var }})
  
  min_color <- om_ifelse(missing(min_color), c("#e17701", "#9a0200"), min_color)
  max_color <- om_ifelse(missing(max_color), c("#3ae57f", "#009337"), max_color)
  
  cat_l <- axis_label(sub_dep(cat_var))
  num_l <- axis_label(sub_dep(num_var))
  
  sumy_nm <- list(mean = "Average", median = "Median", sum = "Total")
  s_typ <- deparse(substitute(sumy_fun))
  
  f_plt_avg_sum <- f_tbl %>% 
    ggplot(aes(fct_reorder({{ cat_var }}, {{ sumy_fun }}, .desc = TRUE), 
               {{ sumy_fun }})) +
    geom_col(fill = "#3d9973", color = "#90e4c1") +
    geom_text(aes(label = scales::comma({{ sumy_fun }})), vjust = 1, 
              color = "bisque", fontface = "bold") +
    labs(x = "", y = "", title = paste(sumy_nm[[s_typ]], num_l, "By", cat_l)) +
    theme_minimal() +
    theme(axis.text.y = element_blank())
  
  
  f_plt_min <- f_tbl %>% 
    ggplot(aes(minimum, {{ cat_var }})) +
    num_cat_sumy_add_layer(sub_typ = "min", colr = min_color)
  
  f_plt_max <- f_tbl %>% 
    ggplot(aes(maximum, {{ cat_var }})) +
    num_cat_sumy_add_layer(sub_typ = "max", colr = max_color)
  
  f_plt_min_max <- (f_plt_min + f_plt_max) +
    plot_annotation(title = paste("Summary of", num_l, "By", cat_l))
  
  
  f_plt <- switch(p_typ,
                  avg_tot = f_plt_avg_sum,
                  min_max = f_plt_min_max)
  
  output <- switch(typ,
                   plt = f_plt,
                   tbl = f_tbl)
  return(output)
}




#' Title
#'
#' @param p_df data.frame
#' @param sub_t string: plot subtitle.
#' @param colors vector: two color for the plot
#' @param txt_pos numeric: value for the plot text position. 
#'
num_cat2_plt_func <- function(p_df, sub_t, colors, txt_pos) {
  
  ggplot(p_df, aes(median, fct_reorder(y_ticks_l, median))) +
    geom_col(fill = colors[[1]]) +
    geom_label(aes(label = scales::comma(median), x = txt_pos), 
               size  = 3.3, 
               color = "white", 
               fill  = colors[[2]],
               label.r = unit(0.25, "lines"),
               label.padding = unit(0.15, "lines") ) +
    labs(x = "", y = "", subtitle = sub_t) +
    scale_x_continuous(labels = scales::comma_format()) +
    theme_minimal() +
    theme(axis.text.x = element_blank())
}




#' @description create an Median summary using numeric and character variables.
#'
#' @param df data.frame
#' @param num_var numeric: variable from the data 'df'.
#' @param cat_var1 character: variable from the data 'df'.
#' @param cat_var2 character: variable from the data 'df'.
#' @param outlier vector: to filter outliers from the data. 
#' @param colours vector: two colors for the plot.
#' @param txt_pos numeric: text position.
#' @param typ string: the type of output either 'plt'-plot or 'tbl'-table.
#'
#' @details outlier can be any of 'slo' 'lo' 'med' 'uo' 'suo' see get_outlier.
#' 
#' @note right_arg, sort_vars, filter_outlier, numeric_summary, om_ifelse.
#' num_cat2_plt_func, sub_dep, axis_label.
#' 
num_cat_sumy2 <- function(df, 
                          num_var, cat_var1, cat_var2, 
                          outlier, colours, txt_pos, typ = "plt") {
  right_arg(r_df = df, 
            r_num_var1 = num_var,
            r_cat_var1 = cat_var1,
            r_cat_var2 = cat_var2,
            r_typ = typ)
  
  vars_s <- c(sub_dep(cat_var1), sub_dep(cat_var2))
  
  res <- sort_vars(df, vars_s)
  
  cat1 <- res[[1]]
  cat2 <- res[[2]]

  if (! missing(outlier)) {
    f_tbl <- filter_outlier(df, {{ num_var }}, outlier)
    
  } else f_tbl <- df
  
  f_tbl <- f_tbl %>% 
    group_by(.data[[cat1]], .data[[cat2]]) %>% 
    numeric_summary({{ num_var }}, groups = "drop") %>% 
    mutate(y_ticks_l = str_wrap(.data[[cat1]], 10))
  
  txt_pos <- ifelse(missing(txt_pos), text_pos(f_tbl, "median"), txt_pos)
  
  nmb_cat <- length(unique(f_tbl[[cat2]])) |> as.character()
  var_cat <- unique(f_tbl[[cat2]])
  var_cat_label <- str_replace_all(var_cat, "-|_", " ") |> str_to_title()
  
  num_label <- axis_label(sub_dep(num_var))
  cat1_label <- axis_label(cat1)
  cat2_label <- axis_label(cat2)
  
  colours <- om_ifelse(missing(colours), c("#4c9085", "#0f9b8e"), colours)
  
  filter_tbl <- function(v_cat) {
    filter(f_tbl, .data[[cat2]] == v_cat) 
  }
  
  plt_list <- map(var_cat, filter_tbl) %>% 
    map2(var_cat_label, num_cat2_plt_func, colours, txt_pos)
  
  f_plt <- switch(nmb_cat,
                  "2" = (plt_list[[1]]+plt_list[[2]]),
                  "3" = (plt_list[[1]]+plt_list[[2]]+plt_list[[3]]),
                  "4" = (plt_list[[1]]+plt_list[[2]]+plt_list[[3]]+plt_list[[4]]),
                  "5" = (plt_list[[1]]+plt_list[[2]]+plt_list[[3]]+plt_list[[4]]+
                         plt_list[[5]]))
  
  f_plt <- f_plt +
    plot_annotation(paste("Median",num_label,"By",cat1_label,"And",cat2_label))
  
  output <- switch(typ,
                   plt = f_plt,
                   tbl = f_tbl)
  return(output)
}



#' Numerical Relationship
#'
#' @param df data.frame
#' @param var_x numeric: variable from the data 'df'.
#' @param var_y numeric: variable from the data 'df'.
#' @param outlier vector: to filter outliers from the data. 
#' @param p_typ string: type of geom to use.
#' @param colour vector: color/colors for the plot.
#' @param bin numeric: the amount of bins to use.
#' @param add_lm bool: whether to add a lm line in the plot or not.
#'
#' @details outlier can be any of 'slo' 'lo' 'med' 'uo' 'suo' see get_outlier
#' @note right_arg, filter_outlier2, breakz, om_ifelse axis_label, sub_dep.
#' 
numerical_rel <- function(df, 
                          var_x, var_y, outlier, 
                          p_typ = "point", colour, bin = 30, add_lm = FALSE) {
  right_arg(r_df = df,
            r_num_var1 = var_x, 
            r_num_var2 = var_x,
            num_var_name = c("var_x", "var_y"),
            others = list(check = p_typ, rvs = c("point", "bin2d")))
  
  if (! missing(outlier)) {
    f_tbl <- filter_outlier2(df, {{ var_x }}, {{ var_y }}, outlier)
    
  } else {
    f_tbl <- df
  }
  
  bkx <- breakz(f_tbl, {{ var_x }})
  bky <- breakz(f_tbl, {{ var_y }})
  
  var_xl <- axis_label(sub_dep(var_x))
  var_yl <- axis_label(sub_dep(var_y))
  
  add_layer <- function() {
    list(
      labs(x = var_xl, 
           y = var_yl,
           title = paste("Relationship Between", var_xl, "&", var_yl)),
      scale_x_continuous(breaks = seq(0, bkx$to, bkx$by),
                         labels = scales::comma_format()),
      scale_y_continuous(breaks = seq(0, bky$to, bky$by),
                         labels = scales::comma_format()),
      theme_minimal()
    )
  }
  
  f_plt <- ggplot(f_tbl, aes({{ var_x }}, {{ var_y }}))
  
  if (p_typ == "point") {
    colour <- ifelse(missing(colour), "#1f6357", colour)
    
    f_plt <- f_plt +
      geom_point(color = colour, alpha = 0.5) +
      add_layer()
    
  } else if (p_typ == "bin2d") {
    colour <- om_ifelse(missing(colour), c("#78d1b6", "#1f6357"), colour)
    
    f_plt <- f_plt +
      geom_bin_2d(bins = bin) +
      scale_fill_gradient(low = colour[[1]], high = colour[[2]]) +
      add_layer()
  }
  
  if (add_lm) {
    f_plt <- f_plt +
      geom_smooth(method = "lm", formula = "y ~ x", color = "red") 
  }
   
  return(f_plt) 
}


