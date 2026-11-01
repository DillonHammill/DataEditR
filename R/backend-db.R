## DATABASE BACKEND ------------------------------------------------------------

#' Edit a table in a database
#'
#' \code{db_table()} describes a database table for \code{data_edit()}. You can
#' usually skip it and pass a \code{dbplyr} lazy table directly, e.g.
#' \code{data_edit(tbl(con, "artists"))}; use \code{db_table()} when the key
#' cannot be detected automatically or the query is not a simple table
#' reference.
#'
#' Rows are matched to the database through the key columns. Edits are staged
#' in the editor and only written when you press \emph{Done} and confirm the
#' commit. All changes are written in a single transaction: if any row has
#' been changed by someone else since it was loaded (or no longer exists), the
#' whole commit is rolled back and nothing is written. Adding, removing and
#' renaming columns is disabled for database tables.
#'
#' Tables do not have to fit in memory: at most \code{limit} rows are loaded
#' at a time, and \emph{Rows} in the toolbar loads other rows (those matching
#' a filter, in a chosen order), with the filter and sorting done by the
#' database. Queries that cannot be written back (SQL, joins, or tables
#' without a primary key, e.g. \code{read_parquet()} with DuckDB) are opened
#' read-only, so large files can still be explored.
#'
#' @param x a \code{dbplyr} lazy table (e.g. \code{dplyr::tbl(con, "artists")},
#'   optionally with \code{filter()}, \code{select()} or \code{arrange()}
#'   applied) or a \code{DBI} connection.
#' @param table name of the table to write to, a character string or a
#'   \code{DBI::Id()}. Required when \code{x} is a connection, otherwise
#'   detected from the lazy table.
#' @param key name(s) of the column(s) that uniquely identify each row. When
#'   \code{NULL} the primary key is detected for SQLite, DuckDB and PostgreSQL
#'   databases.
#' @param limit maximum number of rows to load into the editor at a time, set
#'   to \code{getOption("DataEditR.db_rows", 100000)} by default. Use
#'   \code{dplyr::filter()} on the lazy table, or \emph{Rows} in the editor,
#'   to choose which rows to load.
#' @param check logical indicating whether updates and deletes should verify
#'   that rows have not changed in the database since they were loaded
#'   (optimistic concurrency), set to \code{TRUE} by default.
#' @param readonly \code{TRUE} to open the table read-only, \code{FALSE} to
#'   stop with an error when it cannot be written to. Set to \code{NULL} by
#'   default to open queries that cannot be written to (SQL, joins, tables
#'   without a primary key) read-only.
#'
#' @return an object of class \code{editr_db_table} to pass to
#'   \code{data_edit()} or \code{dataEditServer()}.
#'
#' @author Dillon Hammill, \email{dillon.hammill21@gmail.com}
#'
#' @examples
#' if (interactive() && requireNamespace("RSQLite", quietly = TRUE)) {
#'   con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   DBI::dbExecute(con, "CREATE TABLE cars (id INTEGER PRIMARY KEY,
#'     model TEXT, mpg REAL)")
#'   DBI::dbAppendTable(con, "cars", data.frame(
#'     model = rownames(mtcars), mpg = mtcars$mpg))
#'
#'   # key detected automatically
#'   data_edit(dplyr::tbl(con, "cars"))
#'
#'   # explicit table description
#'   data_edit(db_table(con, table = "cars", key = "id"))
#' }
#'
#' @export
db_table <- function(x,
                     table = NULL,
                     key = NULL,
                     limit = NULL,
                     check = TRUE,
                     readonly = NULL) {
  if (inherits(x, "editr_db_table")) {
    return(x)
  }
  editr_require("DBI", "edit database tables")
  query <- NULL
  why <- NULL # WHY THE TABLE CANNOT BE WRITTEN TO
  if (inherits(x, "DBIConnection")) {
    con <- x
    if (is.null(table)) {
      stop("'table' must be supplied when 'x' is a database connection.",
           call. = FALSE)
    }
  } else if (inherits(x, "tbl_lazy")) {
    editr_require("dbplyr", "edit lazy tables")
    con <- dbplyr::remote_con(x)
    query <- x
    if (is.null(table)) {
      table <- tryCatch(db_base_table(x, con), error = function(e) {
        why <<- conditionMessage(e)
        NULL
      })
    }
  } else {
    stop("'x' must be a dbplyr lazy table or a DBI connection.", call. = FALSE)
  }
  if (isTRUE(readonly) && is.null(why)) why <- "Opened read-only."
  fields <- if (!is.null(table)) DBI::dbListFields(con, table) else colnames(query)
  if (!is.null(key) && !all(key %in% fields)) {
    stop("Key column(s) not found in table: ",
         paste(setdiff(key, fields), collapse = ", "), call. = FALSE)
  }
  if (is.null(why) && is.null(key)) {
    key <- db_primary_key(con, table)
  }
  if (is.null(why) && length(key) == 0) {
    why <- paste0(
      "Could not detect a primary key for table '", db_table_label(table),
      "'. Supply the column(s) that uniquely identify each row through ",
      "`db_table(x, key = ...)`."
    )
  }
  if (!is.null(why) && isFALSE(readonly)) {
    stop(why, call. = FALSE)
  }
  structure(
    list(
      con = con,
      table = table,
      key = if (is.null(why)) key else NULL,
      fields = fields,
      query = query,
      limit = limit %||% getOption("DataEditR.db_rows", 100000),
      check = isTRUE(check),
      readonly = !is.null(why),
      why = why,
      # ROWS ARE LOADED FROM THE QUERY AS GIVEN, FILTERED AND SORTED IN THE
      # EDITOR (SEE db_window())
      filter = NULL,
      sort = NULL,
      desc = FALSE
    ),
    class = "editr_db_table"
  )
}

#' Stop with a helpful message when a suggested package is missing
#' @noRd
editr_require <- function(pkg, what) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Package '", pkg, "' is required to ", what, ".", call. = FALSE)
  }
  invisible(TRUE)
}

#' @export
print.editr_db_table <- function(x, ...) {
  cat("<editr_db_table>\n")
  cat("  table: ", if (is.null(x$table)) "(query)" else db_table_label(x$table),
      if (isTRUE(x$readonly)) " (read-only)", "\n", sep = "")
  if (!isTRUE(x$readonly)) {
    cat("  key:   ", paste(x$key, collapse = ", "), "\n", sep = "")
    cat("  check: ", x$check, "\n", sep = "")
  }
  invisible(x)
}

#' @noRd
is_db_input <- function(x) {
  inherits(x, c("editr_db_table", "tbl_lazy"))
}

#' @noRd
as_db_table <- function(x) {
  db_table(x)
}

#' @noRd
db_table_label <- function(table) {
  if (inherits(table, "Id")) {
    paste(table@name, collapse = ".")
  } else {
    paste(as.character(table), collapse = ".")
  }
}

#' Find the table underneath a dbplyr lazy query
#' @noRd
db_base_table <- function(x, con = NULL) {
  q <- x$lazy_query
  while (!inherits(q, "lazy_base_remote_query")) {
    if (inherits(q, c("lazy_multi_join_query", "lazy_rf_join_query",
                      "lazy_semi_join_query", "lazy_union_query",
                      "lazy_set_op_query")) ||
        is.null(q$x) || !inherits(q$x, "lazy_query")) {
      stop(
        "Cannot edit a query that combines tables (joins or unions). ",
        "Use `db_table(x, table = ..., key = ...)` to say which table to ",
        "write to.",
        call. = FALSE
      )
    }
    q <- q$x
  }
  ident <- q$x
  if (inherits(ident, "sql")) {
    stop(
      "Cannot edit the result of an SQL query. Use ",
      "`db_table(x, table = ..., key = ...)` to say which table to write to.",
      call. = FALSE
    )
  }
  if (inherits(ident, "dbplyr_table_path")) {
    # dbplyr >= 2.5 - QUOTED TABLE PATHS, E.G. `main`.`cars`
    path <- db_split_path(ident, con %||% dbplyr::remote_con(x))
    n <- length(path)
    parts <- list(
      table = if (n >= 1) path[[n]] else NA_character_,
      schema = if (n >= 2) path[[n - 1]] else NULL,
      catalog = if (n >= 3) path[[n - 2]] else NULL
    )
  } else if (is.character(ident)) {
    # dbplyr < 2.4 - TABLE NAMES ARE CHARACTER IDENTIFIERS
    return(as.character(ident))
  } else {
    # dbplyr 2.4 - TABLE IDENTIFIERS
    parts <- unclass(ident)
  }
  table <- parts$table
  schema <- parts$schema
  catalog <- parts$catalog
  if (length(table) != 1 || is.na(table)) {
    stop("Could not determine the table to write to. Supply `table` through ",
         "`db_table()`.", call. = FALSE)
  }
  if (!is.null(schema) && !is.na(schema)) {
    if (!is.null(catalog) && !is.na(catalog)) {
      return(DBI::Id(catalog = catalog, schema = schema, table = table))
    }
    return(DBI::Id(schema = schema, table = table))
  }
  table
}

#' Split a quoted table path (`catalog`.`schema`.`table`) into its parts
#' @noRd
db_split_path <- function(path, con) {
  quote <- substr(as.character(DBI::dbQuoteIdentifier(con, "x")), 1, 1)
  scan(text = as.character(path)[1], what = character(), quote = quote,
       quiet = TRUE, na.strings = character(), sep = ".")
}

#' Detect primary key columns
#' @noRd
db_primary_key <- function(con, table) {
  name <- if (inherits(table, "Id")) {
    unname(table@name[["table"]])
  } else {
    as.character(table)
  }
  schema <- if (inherits(table, "Id") && "schema" %in% names(table@name)) {
    unname(table@name[["schema"]])
  } else {
    NULL
  }
  key <- tryCatch({
    if (inherits(con, "SQLiteConnection")) {
      info <- DBI::dbGetQuery(
        con,
        paste0("PRAGMA table_info(", DBI::dbQuoteIdentifier(con, name), ")")
      )
      info <- info[info$pk > 0, , drop = FALSE]
      info$name[order(info$pk)]
    } else if (inherits(con, "duckdb_connection")) {
      res <- DBI::dbGetQuery(
        con,
        paste0(
          "SELECT constraint_column_names FROM duckdb_constraints() ",
          "WHERE constraint_type = 'PRIMARY KEY' AND table_name = ",
          DBI::dbQuoteString(con, name)
        )
      )
      if (nrow(res) == 0) character(0) else unlist(res[[1]][[1]])
    } else {
      sql <- paste0(
        "SELECT kcu.column_name FROM information_schema.table_constraints tc ",
        "JOIN information_schema.key_column_usage kcu ",
        "ON tc.constraint_name = kcu.constraint_name ",
        "AND tc.table_schema = kcu.table_schema ",
        "WHERE tc.constraint_type = 'PRIMARY KEY' AND tc.table_name = ",
        DBI::dbQuoteString(con, name),
        if (!is.null(schema)) {
          paste0(" AND tc.table_schema = ", DBI::dbQuoteString(con, schema))
        },
        " ORDER BY kcu.ordinal_position"
      )
      DBI::dbGetQuery(con, sql)[[1]]
    }
  }, error = function(e) character(0))
  as.character(key)
}

#' The query rows are loaded from: the table or query given, with the filter
#' and order chosen in the editor (NULL without dbplyr)
#'
#' @param sorted whether to sort (counting does not need it).
#'
#' @noRd
db_window_query <- function(db, sorted = TRUE) {
  q <- db$query
  if (is.null(q)) {
    if (is.null(db$filter) && is.null(db$sort)) return(NULL)
    editr_require("dbplyr", "load rows by a filter")
    q <- dplyr::tbl(db$con, db$table)
  }
  if (!is.null(db$filter)) {
    expr <- formula_parse(db$filter)
    formula_check(expr, colnames(q))
    q <- dplyr::filter(q, !!expr)
  }
  if (sorted && !is.null(db$sort)) {
    col <- rlang::sym(db$sort)
    q <- if (isTRUE(db$desc)) dplyr::arrange(q, dplyr::desc(!!col)) else dplyr::arrange(q, !!col)
  }
  q
}

#' Number of rows of the table or query (with the filter chosen in the editor)
#' @noRd
db_count <- function(db) {
  q <- db_window_query(db, sorted = FALSE)
  n <- if (is.null(q)) {
    DBI::dbGetQuery(db$con, paste0("SELECT COUNT(*) AS n FROM ",
                                   DBI::dbQuoteIdentifier(db$con, db$table)))[[1]]
  } else {
    dplyr::pull(dplyr::collect(dplyr::count(q)), 1)
  }
  as.numeric(n)
}

#' Rows to load from a database table: those matching a filter (an R
#' expression translated to SQL by dbplyr, using columns and the functions
#' formulas may use), in the order of a column, at most limit
#'
#' @return the editr_db_table, set to load these rows.
#'
#' @noRd
db_window <- function(db, filter = NULL, sort = NULL, desc = FALSE,
                      limit = db$limit) {
  filter <- trimws(paste(as.character(filter %||% ""), collapse = "\n"))
  db$filter <- if (nzchar(filter)) filter else NULL
  sort <- as.character(sort %||% "")
  if (nzchar(sort) && !sort %in% db$fields) {
    stop("There is no column called '", sort, "'.", call. = FALSE)
  }
  db$sort <- if (nzchar(sort)) sort else NULL
  db$desc <- isTRUE(desc)
  limit <- suppressWarnings(as.numeric(limit))
  if (length(limit) != 1 || is.na(limit) || limit < 1) {
    stop("Load at least one row.", call. = FALSE)
  }
  db$limit <- floor(limit)
  # CHECK THE FILTER NOW (dbplyr TRANSLATES IT WHEN ROWS ARE LOADED)
  db_window_query(db)
  db
}

#' Load rows from the database
#'
#' @return a data frame, with the number of rows of the table or query in
#'   attribute \code{"db_total"}.
#'
#' @noRd
db_collect <- function(db) {
  q <- db_window_query(db)
  if (!is.null(q)) {
    data <- dplyr::collect(q, n = db$limit)
  } else if (is.finite(db$limit)) {
    sql <- paste0(
      "SELECT * FROM ", DBI::dbQuoteIdentifier(db$con, db$table),
      " LIMIT ", as.integer(db$limit)
    )
    data <- DBI::dbGetQuery(db$con, sql)
  } else {
    data <- DBI::dbReadTable(db$con, db$table)
  }
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  # ONLY COUNTED WHEN THERE MAY BE MORE ROWS THAN WERE LOADED
  total <- if (nrow(data) < db$limit) nrow(data) else db_count(db)
  attr(data, "db_total") <- total
  if (isTRUE(db$readonly)) return(data)
  missing_key <- setdiff(db$key, names(data))
  if (length(missing_key) > 0) {
    stop("The key column(s) ", paste(missing_key, collapse = ", "),
         " must be included in the data to edit a database table.",
         call. = FALSE)
  }
  if (any(duplicated(data[, db$key, drop = FALSE]))) {
    stop("The key column(s) ", paste(db$key, collapse = ", "),
         " do not uniquely identify rows in the loaded data.", call. = FALSE)
  }
  data
}

## COMMIT ----------------------------------------------------------------------

#' Summarise pending database changes
#'
#' @return list with \code{updated}, \code{inserted} and \code{deleted} counts
#'   and the underlying \code{changes}.
#'
#' @noRd
db_changes <- function(store) {
  ch <- net_changes(store$state, store$original)
  list(
    changes = ch,
    updated = length(ch$updated),
    inserted = length(ch$inserted),
    deleted = length(ch$deleted),
    total = length(ch$updated) + length(ch$inserted) + length(ch$deleted)
  )
}

#' Write staged changes to the database in a single transaction
#'
#' @return a list with counts of rows updated, inserted and deleted.
#'
#' @noRd
db_commit <- function(db, store) {
  if (isTRUE(db$readonly)) {
    stop("The table was opened read-only; nothing can be written to it.",
         call. = FALSE)
  }
  con <- db$con
  state <- store$state
  original <- store$original
  ch <- net_changes(state, original)
  key <- db$key
  fields <- db$fields
  tbl_sql <- DBI::dbQuoteIdentifier(con, db$table)
  q <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))
  eq <- db_null_safe_eq(con)
  name_of <- function(s, cid) names(s$data)[match(cid, s$cids)]

  orig_rows <- function(ids) {
    original$data[match(ids, original$ids), , drop = FALSE]
  }
  cur_rows <- function(ids) {
    state$data[match(ids, state$ids), , drop = FALSE]
  }
  where_key <- paste(paste0(q(key), " = ?"), collapse = " AND ")

  DBI::dbWithTransaction(con, {

    # DELETES
    if (length(ch$deleted) > 0) {
      rows <- orig_rows(ch$deleted)
      sql <- paste0("DELETE FROM ", tbl_sql, " WHERE ", where_key)
      params <- unname(as.list(rows[, key, drop = FALSE]))
      if (db$check) {
        check_cols <- setdiff(intersect(names(rows), fields), key)
        if (length(check_cols) > 0) {
          sql <- paste0(sql, " AND ",
                        paste(paste0(q(check_cols), eq, "?"),
                              collapse = " AND "))
          params <- c(params, db_params(rows[, check_cols, drop = FALSE]))
        }
      }
      n <- DBI::dbExecute(con, sql, params = params)
      if (n != nrow(rows)) {
        stop(editr_conflict(db_conflict_message(nrow(rows) - n, "deleted")))
      }
    }

    # UPDATES - GROUPED BY THE SET OF CHANGED COLUMNS
    if (length(ch$updated) > 0) {
      ids <- as.integer(names(ch$updated))
      sets <- vapply(ch$updated, function(cids) {
        paste(sort(cids), collapse = ",")
      }, character(1))
      for (s in unique(sets)) {
        group <- ids[sets == s]
        cids <- strsplit(s, ",", fixed = TRUE)[[1]]
        cols <- vapply(cids, function(cid) name_of(state, cid), character(1))
        cols <- cols[cols %in% fields]
        if (length(cols) == 0) next
        old <- orig_rows(group)
        new <- cur_rows(group)
        # COLUMN NAMES CANNOT CHANGE FOR DATABASE TABLES
        orig_cols <- unname(cols)
        sql <- paste0(
          "UPDATE ", tbl_sql, " SET ",
          paste(paste0(q(cols), " = ?"), collapse = ", "),
          " WHERE ", where_key
        )
        params <- c(
          db_params(new[, cols, drop = FALSE]),
          unname(as.list(old[, key, drop = FALSE]))
        )
        if (db$check) {
          check_cols <- setdiff(unname(orig_cols), key)
          if (length(check_cols) > 0) {
            sql <- paste0(sql, " AND ",
                          paste(paste0(q(check_cols), eq, "?"),
                                collapse = " AND "))
            params <- c(params, db_params(old[, check_cols, drop = FALSE]))
          }
        }
        n <- DBI::dbExecute(con, sql, params = params)
        if (n != length(group)) {
          stop(editr_conflict(db_conflict_message(length(group) - n,
                                                  "updated")))
        }
      }
    }

    # INSERTS - ROWS WITHOUT KEY VALUES LET THE DATABASE GENERATE THEM
    if (length(ch$inserted) > 0) {
      rows <- cur_rows(ch$inserted)
      rows <- rows[, intersect(names(rows), fields), drop = FALSE]
      no_key <- apply(is.na(rows[, key, drop = FALSE]), 1, any)
      if (any(!no_key)) {
        DBI::dbAppendTable(con, db$table, rows[!no_key, , drop = FALSE])
      }
      if (any(no_key)) {
        DBI::dbAppendTable(
          con, db$table,
          rows[no_key, setdiff(names(rows), key), drop = FALSE]
        )
      }
    }
  })

  list(
    updated = length(ch$updated),
    inserted = length(ch$inserted),
    deleted = length(ch$deleted)
  )
}

#' How to refer to a table in generated dbplyr code
#' @noRd
db_table_code <- function(table) {
  q <- function(x) encodeString(x, quote = '"')
  if (inherits(table, "Id")) {
    nm <- table@name
    tb <- q(nm[["table"]])
    if ("schema" %in% names(nm)) {
      if ("catalog" %in% names(nm)) {
        return(paste0("dbplyr::in_catalog(", q(nm[["catalog"]]), ", ",
                      q(nm[["schema"]]), ", ", tb, ")"))
      }
      return(paste0("dbplyr::in_schema(", q(nm[["schema"]]), ", ", tb, ")"))
    }
    return(tb)
  }
  q(as.character(table))
}

#' @noRd
db_params <- function(df) {
  lapply(unname(as.list(df)), function(x) {
    if (inherits(x, "Date") || inherits(x, "POSIXct")) {
      x
    } else if (is.factor(x)) {
      as.character(x)
    } else {
      unclass(x)
    }
  })
}

#' @noRd
db_null_safe_eq <- function(con) {
  if (inherits(con, "SQLiteConnection")) {
    " IS "
  } else if (inherits(con, "MySQLConnection") ||
             inherits(con, "MariaDBConnection")) {
    " <=> "
  } else {
    " IS NOT DISTINCT FROM "
  }
}

#' Error raised when rows changed in the database since they were loaded
#' @noRd
editr_conflict <- function(message) {
  structure(
    class = c("editr_conflict", "error", "condition"),
    list(message = message, call = NULL)
  )
}

#' @noRd
db_conflict_message <- function(n, what) {
  paste0(
    n, " row(s) to be ", what, " were changed or removed in the database ",
    "after they were loaded into the editor. No changes were written. ",
    "Reload the data to see the latest values."
  )
}

## CODE ------------------------------------------------------------------------

#' Generate dplyr code that replicates database edits
#'
#' @param db an \code{editr_db_table}.
#' @param store editor store.
#' @param name name of the lazy table in the generated code.
#'
#' @noRd
data_code_db <- function(db, store, name = "data") {
  state <- store$state
  original <- store$original
  ch <- net_changes(state, original)
  key <- db$key
  if (length(ch$updated) + length(ch$inserted) + length(ch$deleted) == 0) {
    return("# No changes detected\n")
  }
  tibble_code <- function(df) {
    cols <- vapply(names(df), function(col) {
      vals <- paste(vapply(df[[col]], data_code_value, character(1)),
                    collapse = ", ")
      paste0(data_code_quote(col), " = c(", vals, ")")
    }, character(1))
    paste0("tibble::tibble(", paste(cols, collapse = ", "), ")")
  }
  key_str <- if (length(key) == 1) {
    paste0('"', key, '"')
  } else {
    paste0("c(", paste0('"', key, '"', collapse = ", "), ")")
  }
  lines <- c(
    "library(dplyr)",
    "",
    paste0(name, " <- tbl(con, ", db_table_code(db$table), ")"),
    ""
  )
  if (length(ch$deleted) > 0) {
    rows <- original$data[match(ch$deleted, original$ids), key, drop = FALSE]
    lines <- c(lines, paste0(
      "rows_delete(", name, ", ", tibble_code(rows), ", by = ", key_str,
      ", unmatched = \"ignore\", copy = TRUE, in_place = TRUE)"
    ))
  }
  if (length(ch$updated) > 0) {
    ids <- as.integer(names(ch$updated))
    sets <- vapply(ch$updated, function(cids) {
      paste(sort(cids), collapse = ",")
    }, character(1))
    for (s in unique(sets)) {
      group <- ids[sets == s]
      cids <- strsplit(s, ",", fixed = TRUE)[[1]]
      cols <- names(state$data)[match(cids, state$cids)]
      cols <- cols[cols %in% db$fields]
      if (length(cols) == 0) next
      keys <- original$data[match(group, original$ids), key, drop = FALSE]
      vals <- state$data[match(group, state$ids), cols, drop = FALSE]
      df <- cbind(keys, vals[, setdiff(cols, key), drop = FALSE])
      if (any(key %in% cols)) {
        lines <- c(lines, paste0(
          "# NOTE: key value(s) were edited for ", length(group), " row(s); ",
          "rows_update() matches on the original key and cannot change it"
        ))
      }
      lines <- c(lines, paste0(
        "rows_update(", name, ", ", tibble_code(df), ", by = ", key_str,
        ", unmatched = \"ignore\", copy = TRUE, in_place = TRUE)"
      ))
    }
  }
  if (length(ch$inserted) > 0) {
    rows <- state$data[match(ch$inserted, state$ids), , drop = FALSE]
    rows <- rows[, intersect(names(rows), db$fields), drop = FALSE]
    no_key <- apply(is.na(rows[, key, drop = FALSE]), 1, any)
    if (any(!no_key)) {
      lines <- c(lines, paste0(
        "rows_append(", name, ", ", tibble_code(rows[!no_key, , drop = FALSE]),
        ", copy = TRUE, in_place = TRUE)"
      ))
    }
    if (any(no_key)) {
      lines <- c(lines, paste0(
        "rows_append(", name, ", ",
        tibble_code(rows[no_key, setdiff(names(rows), key), drop = FALSE]),
        ", copy = TRUE, in_place = TRUE)"
      ))
    }
  }
  paste0(paste(lines, collapse = "\n"), "\n")
}
