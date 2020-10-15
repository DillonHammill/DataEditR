#' @importFrom rstudioapi getActiveDocumentContext
#' @export
data_edit2 <- function(x = NULL,
                       col_bind = NULL,
                       col_edit = TRUE,
                       col_options = NULL,
                       col_stretch = FALSE,
                       col_factor = FALSE,
                       col_names = TRUE,
                       col_readonly = NULL,
                       row_bind = NULL,
                       row_edit = TRUE,
                       save_as = NULL,
                       title = NULL,
                       logo = NULL,
                       logo_size = 100,
                       logo_side = "left",
                       viewer = "dialog",
                       theme = "yeti",
                       quiet = FALSE,
                       read_fun = "read.csv",
                       read_args = NULL,
                       write_fun = "write.csv",
                       write_args = NULL,
                       ...) {
  
  # PREPARE DATA ---------------------------------------------------------------
  
  # RSTUDIO ADD-IN
  context <- getActiveDocumentContext()
  x_addin <- context$selection[[1]]$text
  
  # MISSING DATA
  if(is.null(x)) {
    # RSTUDIO ADD-IN
    if(nzchar(x_addin)) {
      x <- eval(parse(text = x_addin))
      x_text <- x_addin
    # EMPTY TEMPLATE
    } else {
      x <- data.frame("V1" = "")
      x_text <- "x"
    }
  # DATA SUPPLIED
  } else {
    # VECTOR
    if(is.null(dim(x))) {
      # DATA NAME/FILE NAME/COLUMN NAMES
      if(is.character(x)) {
        # DATA OBJECT NAME
        x <- tryCatch(eval(parse(text = x)),
                      error = function(e){NULL})
        # FILENAME/COLUMN NAMES
        if(is.null(x)) {
          # FILENAME - SINGLE + FILE EXTENSION
          if(length(x) == 1 & all(!nzchar(file_ext(x)))) {
            # FUNCTION
            read_fun <- match.fun(read_fun)
            # CHECK READ ARGUMENTS
            if (!is.null(read_args)) {
              if (!class(read_args) == "list") {
                stop("read_args must be a named list of arguments for read_fun.")
              }
            }
            # READ ARGUMENTS
            read_args <- c(list(x), read_args)
            # EXTRA ARGUMENTS
            extra_args <- list(...)
            read_args <- c(
              read_args,
              extra_args[!names(extra_args) %in% names(read_args)]
            )
            # CALL FUNCTION
            x <- do.call(read_fun, read_args)
          # COLUMN NAMES 
          } else {
            x <- data.frame(
              structure(rep(list(""), length(x)),
                        names = x),
              check.names = FALSE,
              stringsAsFactors =  FALSE
            )
          }
        }
      # DIMENSIONS
      } else if(is.numeric(x)) {
        x <- rep(x, length.out = 2)
        x <- data.frame(
          structure(rep(list(rep("", x[1])), x[2]),
                    names = paste0("V", seq_len(x[2]))),
          check.names = FALSE,
          stringsAsFactors =  FALSE
        )
      }
    }
    x_text <- "x"
  }
  
  # BIND ROWS
  if (!is.null(row_bind)) {
    # NEW ROWS
    if (is.null(dim(row_bind))) {
      # ROWS AS LIST
      if (class(row_bind) == "list") {
        # NAMES NOT NECESSARY
        # LENGTHS
        ind <- which(!unlist(lapply(row_bind, length)) == ncol(x))
        if (length(ind) > 0) {
          for (z in ind) {
            row_bind[[z]] <- rep(row_bind[[z]], ncol(x))
          }
        }
        # MATRIX
        row_bind <- do.call("rbind", row_bind)
        # ROW NAMES
      } else {
        row_bind <- matrix(rep("", ncol(x) * length(row_bind)),
                           nrow = length(row_bind),
                           dimnames = list(
                             row_bind,
                             colnames(x)
                           )
        )
      }
    }
    # BIND NEW ROWS
    x <- rbind(x, row_bind[, 1:ncol(x)])
  }
  
  # BIND COLUMNS
  if (!is.null(col_bind)) {
    # NEW COLUMNS
    if (is.null(dim(col_bind))) {
      # COLUMNS AS LIST
      if (class(col_bind) == "list") {
        # NAMES
        if (is.null(names(col_bind))) {
          names(col_bind) <- paste0("V", length(col_bind))
        }
        # LENGTHS
        ind <- which(!unlist(lapply(col_bind, length)) == nrow(x))
        if (length(ind) > 0) {
          for (z in ind) {
            col_bind[[z]] <- rep(col_bind[[z]], nrow(x))
          }
        }
        # MATRIX
        col_bind <- do.call("cbind", col_bind)
        # COLUMN NAMES
      } else {
        col_bind <- matrix(rep("", nrow(x) * length(col_bind)),
                           ncol = length(col_bind),
                           dimnames = list(
                             rownames(x),
                             col_bind
                           )
        )
      }
    }
    # BIND NEW COLUMNS
    x <- cbind(x, col_bind[1:nrow(x), , drop = FALSE])
  }
  
  # COLUMN NAMES
  if (length(unique(colnames(x))) != length(colnames(x))) {
    stop("Column names must be unique!")
  }
  
  # COLUMN OPTIONS - LOGICAL
  if (!is.null(col_options)) {
    for (z in names(col_options)) {
      col_type <- type.convert(col_options[[z]], as.is = TRUE)
      # CHECKBOXES
      if (is.logical(col_type)) {
        if (!is.logical(x[, z])) {
          res <- type.convert(x[, z], as.is = TRUE)
          if (!is.logical(res)) {
            res <- rep(NA, nrow(x))
          }
          x[, z] <- res
        }
        # DROPDOWN MENUS
      } else {
        # NA TO EMPTY CHARACTERS
        if (all(is.na(x[, z]))) {
          x[, z] <- rep("", nrow(x))
        }
      }
    }
  }
  
  # ABSORB ROW NAMES
  if (!is.null(rownames(x))) {
    # EMPTY ROW NAMES - CHARACTER(0)
    if (length(rownames(x)) == 0) {
      rn <- "empty"
      rownames(x) <- 1:nrow(x)
      # ROW INDICES
    } else if (all(rownames(x) == seq(1, nrow(x)))) {
      rn <- "index"
      # ROW NAMES SET
    } else {
      rn <- "set"
      x <- cbind(rownames(x), x)
      colnames(x)[1] <- " "
      rownames(x) <- 1:nrow(x) # display row indices in table
    }
  } else {
    rn <- "empty"
    rownames(x) <- 1:nrow(x)
  }
  
  # SHINY APPLICATION ----------------------------------------------------------
  
  # LOGO
  if (!is.null(logo)) {
    logo <- img(
      src = logo,
      width = logo_size
    )
  }

  # TITLE
  if(is.null(title)) {
    title <- "Interactive Data Editor"
  }
  
  # TITLE/LOGO BAR
  if(!is.null(logo)) {
    # LOGO ON LEFT
    if(logo_side == "left") {
      title <- gadgetTitleBar(
        span(logo,
             title)
      )
    } else {
      title <- gadgetTitleBar(
        span(title,
             logo)
      )
    }
  } else {
    title <- gadgetTitleBar(
      title
    )
  }
  
  # USER INTERFACE -------------------------------------------------------------
  
  ui <- miniPage(
    title,
    theme = shinytheme(theme),
    useShinyjs(),
    dataInputUI("input1"),
    dataEditUI("data1"),
    rHandsontableOutput("x")
  )
  
  # SERVER ---------------------------------------------------------------------
  
  server <- function(input,
                     output,
                     session) {
    
    # DATA
    data_to_edit <- dataInputServer("test1",
                                    read_fun = read_fun,
                                    read_args = read_args)
    
    # VALUES
    data_edits <- reactiveValues(x = data_to_edit())
    
    # DONE
    observeEvent(input$done, {
      stopApp()
    })
    
    # CLOSE
    observeEvent(input$cancel, {
      stopApp()
    })
    
    
  }
  
}