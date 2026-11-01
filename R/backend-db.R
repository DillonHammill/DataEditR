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
#' @param x a \code{dbplyr} lazy table (e.g. \code{dplyr::tbl(con, "artists")},
#'   optionally with \code{filter()}, \code{select()} or \code{arrange()}
#'   applied) or a \code{DBI} connection.
#' @param table name of the table to write to, a character string or a
#'   \code{DBI::Id()}. Required when \code{x} is a connection, otherwise
#'   detected from the lazy table.
#' @param key name(s) of the column(s) that uniquely identify each row. When
#'   \code{NULL} the primary key is detected for SQLite, DuckDB and PostgreSQL
#'   databases.
#' @param limit maximum number of rows to load into the editor, set to
#'   \code{Inf} by default. Use \code{dplyr::filter()} on the lazy table to
#'   choose which rows to edit.
#' @param check logical indicating whether updates and deletes should verify
#'   that rows have not changed in the database since they were loaded
#'   (optimistic concurrency), set to \code{TRUE} by default.
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
                     limit = Inf,
                     check = TRUE) {
  if (inherits(x, "editr_db_table")) {
    return(x)
  }
  editr_require("DBI", "edit database tables")
  query <- NULL
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
      table <- db_base_table(x)
    }
  } else {
    stop("'x' must be a dbplyr lazy table or a DBI connection.", call. = FALSE)
  }
  fields <- DBI::dbListFields(con, table)
  if (is.null(key)) {
    key <- db_primary_key(con, table)
  }
  if (length(key) == 0) {
    stop(
      "Could not detect a primary key for table '", db_table_label(table),
      "'. Supply the column(s) that uniquely identify each row through ",
      "`db_table(x, key = ...)`.",
      call. = FALSE
    )
  }
  if (!all(key %in% fields)) {
    stop("Key column(s) not found in table: ",
         paste(setdiff(key, fields), collapse = ", "), call. = FALSE)
  }
  structure(
    list(
      con = con,
      table = table,
      key = key,
      fields = fields,
      query = query,
      limit = limit,
      check = isTRUE(check)
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
  cat("  table: ", db_table_label(x$table), "\n", sep = "")
  cat("  key:   ", paste(x$key, collapse = ", "), "\n", sep = "")
  cat("  check: ", x$check, "\n", sep = "")
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
db_base_table <- function(x) {
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
  # dbplyr < 2.4 - TABLE NAMES ARE CHARACTER IDENTIFIERS
  if (is.character(ident)) {
    return(as.character(ident))
  }
  parts <- unclass(ident)
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

#' Load rows from the database
#' @noRd
db_collect <- function(db) {
  if (!is.null(db$query)) {
    data <- dplyr::collect(db$query, n = db$limit)
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
