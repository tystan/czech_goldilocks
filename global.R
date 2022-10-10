

# ---- libs ----

### removed this as issues with using github package when using shinyapps.io
# devtools::install_github('tystan/simplexity') 
# library("simplexity")

library("shiny")
library("shinythemes")
library("readr")
library("dplyr")
library("plotly")
library("foreach")

# ---- consts ----

px_h_plot <- "750px"
px_w_plot <- "900px"


plotly_col_pals <-
  c(
    "Blackbody", "Bluered", "Blues",
    "Cividis", "Earth", "Electric",
    "Greens", "Greys", "Hot",
    "Jet", "Picnic", "Portland",
    "Rainbow", "RdBu", "Reds",
    "Viridis", "YlGnBu", "YlOrRd"
  )

# ---- funcs_from_simplexity ----

# can't use github.com/tystan/simplexity package yet as not on CRAN

#' Transform 4-simplex data to a 3-D plotting coordinates
#'
#' @author Ty Stanford <tystan@gmail.com>
#' @description Transform 4-simplex data to a 3-D plotting coordinates
#' @param comp_dat a matrix of \code{n} observations (rows) and \code{4} compositional components (columns)
#' @param warn (default \code{FALSE}) The closure operation to 1 is applied to 
#' the observations in \code{comp_dat}, should a warning about pre-closure observations not being 1 be printed?
#' @export
#' @details
#' Returns a \code{n x 4} \code{data.frame} of \code{(x, y, z)} 3-D plotting coordinates.
#'
#' @examples
#' (grid_4simplex <- mk_simplex_grid(4, 0.2, rm_edges = TRUE, nc = 1))
#' trans_comp_to_tetra(grid_4simplex)
#' 


trans_comp_to_tetra <- function(comp_dat, warn = FALSE) {
  
  comp_dat <- clos_check(comp_dat, warn = warn)
  
  c4 <- 9 / 2
  c5 <- 5
  # n <- nrow(comp_dat)
  # note: if using matrix algebra then use c_mat
  # however, using the constant c5 saves memory
  # c_mat <- matrix(c5, nrow = n, ncol = 3)
  
  trans_mat <-
    c4 *
    matrix(
      c(
        1, -1, -1,  1,
        -1,  1, -1,  1,
        1,  1, -1, -1
      ),
      ncol = 4,
      byrow = TRUE
    )
  
  xyz <- comp_dat %*% t(trans_mat) + c5 # or + c_mat
  xyz <- as.data.frame(xyz)
  colnames(xyz) <- letters[24:26]
  
  return(xyz)
  
}

#' Check that rows sum up to the closure value

#' @author Ty Stanford <tystan@gmail.com>
#' @description Check that rows sum up to the closure value
#' @param x \code{data.frame} or \code{matrix} with of \code{n} observations (rows) and \code{D} compositional components (columns)
#' @param clo_val a positive closure value to check each row sums to this
#' @param warn (default \code{TRUE}) should a warning about pre-closure observations not being \code{clo_val} be printed?
#' @export

#' @examples 
#' (c_dat1 <- matrix(c(rep(0.25, 4), 1:4 / 10, 1:4), byrow = TRUE, ncol = 4))
#' clos_check(c_dat1)
#' (c_dat2 <- matrix(c(rep(0.25, 4), 1:4 / 10)     , byrow = TRUE, ncol = 4))
#' clos_check(c_dat2)
#' (c_dat3 <- matrix(c(rep(0.25, 4), 1:4 / 10, 1:4), byrow = TRUE, ncol = 4))
#' clos_check(c_dat3, clo_val = 10)
#' (c_dat4 <- 3 * matrix(c(rep(0.25, 4), 1:4 / 10), byrow = TRUE, ncol = 4))
#' clos_check(c_dat4, 3)
#' 
#' 
clos_check <- function(x, clo_val = 1, warn = TRUE) {
  rwsms <- rowSums(x)
  rwsms_diffs <- abs(rwsms - clo_val)
  rwsms_not_clos <- rwsms_diffs > 1e-8
  if (any(rwsms_not_clos)) {
    if (warn) {
      cat(
        "NOTE: closure operator being applied to rows as ",
        sum(rwsms_not_clos),
        " row(s) are not closed to ",
        clo_val,
        ".\n",
        sep = ""
      )
    }
    x <- row_wise_closure(x, clo_val = clo_val)
  }
  return(x)
}

#' Apply closure operator over rows

#' @author Ty Stanford <tystan@gmail.com>
#' @description Apply closure operator over rows
#' @param y \code{data.frame} or \code{matrix} with of \code{n} observations (rows) and \code{D} compositional components (columns)
#' @param clo_val a positive value for each row to be closed to (i.e., each rows sums to this)
#' @export
#'
#' @examples
#' (c_dat1 <- matrix(c(rep(0.25, 4), 1:4 / 10, 1:4), byrow = TRUE, ncol = 4))
#' row_wise_closure(c_dat1)

row_wise_closure <- function(y, clo_val = 1) {
  clo_val * t(apply(y, 1, function(x) x / sum(x)))
}


# ---- funcs ----



col_geo_mean <- function(x) {
  if (is.null(dim(x))) {
    return(x)
  } else if (nrow(x) == 1) { # one row
    return(as.numeric(x))
  } else {
    n <- nrow(x)
    return(apply(x ^ (1 / n), 2, prod))
  }
}

clo <- function(x, clo_val = 1) {
  clo_val * x / sum(x)
}


### NOTE: colour choices for plotly palettes
# Blackbody,Bluered,Blues,
# Cividis,Earth,Electric,
# Greens,Greys,Hot,
# Jet,Picnic,Portland,
# Rainbow,RdBu,Reds,
# Viridis,YlGnBu,YlOrRd



plot_4_comp <- function(comp_data, x1, x2, x3, x4, col = NULL, alpha = 0.9, pal = "Bluered") {
  
  obs_labs <- NULL
  if (is.null(col)) {
    col <- "col"
    comp_data[[col]] <- 1
  } else {
    obs_labs <- paste0("<br>", col, " = ", sprintf("%6.2f", comp_data[[col]]))
  }
  
  null_axis <- 
    list(
      title = "", 
      ticktext = "", 
      tickvals = "",
      # showgrid = FALSE,
      showspikes = FALSE
    )
  
  obs_labs <-
    paste0(
      obs_labs,
      "<br>", x1, " = ", sprintf("%6.2f", comp_data[[x1]]),
      "<br>", x2, " = ", sprintf("%6.2f", comp_data[[x2]]),
      "<br>", x3, " = ", sprintf("%6.2f", comp_data[[x3]]),
      "<br>", x4, " = ", sprintf("%6.2f", comp_data[[x4]])
    )
  
  comp_mat <- as.matrix(comp_data[, c(x1, x2, x3, x4)])
  # scale comp_mat to 1
  comp_mat <- row_wise_closure(comp_mat)
  
  tetra_coord <- 
    cbind(
      trans_comp_to_tetra(comp_mat), 
      col = comp_data[[col]],
      obs_labs = obs_labs
    )
  
  
  vert_comp <- diag(4)
  vert_labs_txt <- c(x1, x2, x3, x4) 
  vertex_dat <- trans_comp_to_tetra(vert_comp, warn = TRUE)
  vert_labs_dat <- cbind(vertex_dat, txt = vert_labs_txt)
  colnames(vertex_dat) <- paste0("V", 1:3)
  
  edge_dat <-
    rbind(
      vertex_dat,
      vertex_dat[c(2, 4, 1, 3), ]
    )
  
  
  # add points of 4-simplex in tetra
  plty <- 
    plot_ly() %>%
    add_trace(
      type = "scatter3d",
      mode = "markers",
      data = tetra_coord,
      x = ~x, 
      y = ~y, 
      z = ~z, 
      # color  = ~col,
      opacity = alpha,
      hovertext = ~obs_labs,
      hoverinfo = "text",
      # hoverlabel = list(
      #   align = "right",
      #   bgcolor = map_cts_to_scale(tetra_coord[["col"]])
      # ),
      marker =
        list(
          # coloraxis = "coloraxis",
          color = ~col,
          showscale = TRUE,
          colorscale = pal,
          colorbar = list(
            len = 0.5,
            title = list(
              text = col,
              font = list(
                size = 20
              )
            )
          )
        )
      # hovertemplate = "%{text}"
      # showlegend = FALSE
    ) 
  
  
  # create tetra edges
  plty <- 
    plty %>%
    add_trace(
      x = edge_dat[["V1"]], 
      y = edge_dat[["V2"]], 
      z = edge_dat[["V3"]], 
      type = 'scatter3d', 
      mode = 'lines+markers', 
      opacity = 1,
      line = list(color = 'black', width = 1),
      marker = list(color = 'black'),
      showlegend = FALSE
    )
  
  # label vertices
  plty <- 
    plty %>% 
    add_text(
      x = vert_labs_dat[["x"]], 
      y = vert_labs_dat[["y"]], 
      z = vert_labs_dat[["z"]], 
      text = vert_labs_dat[["txt"]],
      showlegend = FALSE
    )
  
  
  
  # remove x, y, z axes
  plty <- 
    plty %>% 
    layout(
      scene = list(
        dragmode = "orbit",
        xaxis = null_axis,
        yaxis = null_axis,
        zaxis = null_axis,
        camera = list(eye = list(x = 2, y = 1, z = -1))
      )
    )
  
  return(plty)
  
}



add_axis <- function(plot, dat) {
  plot %>%
    add_trace(
      x = dat[["x"]], 
      y = dat[["y"]], 
      z = dat[["z"]], 
      type = 'scatter3d', 
      mode = 'lines+markers', 
      opacity = 1,
      line = list(color = 'black', width = 0.5, opacity  = 0.2),
      marker = list(color = 'black', opacity = 0.2, size = 2, symbol = "x-thin-open"),
      showlegend = FALSE
    ) %>% 
    add_text(
      x = dat[["x"]], 
      y = dat[["y"]], 
      z = dat[["z"]], 
      text = dat[["txt"]], 
      opacity = 0.2,
      showlegend = FALSE
    )
}

add_surf <- function(plot, dat) {
  plot %>%
    add_trace(
      x = dat[["x"]], 
      y = dat[["y"]], 
      z = dat[["z"]], 
      type = 'mesh3d', 
      opacity = 0.2,
      showlegend = FALSE,
      hovertext = NULL
    ) 
}



# ---- read ----

fmp <- read_csv("dat/fmp_pred_adol_newcomp.csv")
fmp <- 
  fmp %>%
  rename(`FM%` = pred)

fmp_mean <- col_geo_mean(fmp[, c("sleep", "sb", "lpa", "mvpa")])
fmp_mean <- clo(fmp_mean)
fmp_mean <- matrix(fmp_mean, nrow = 1)
sum(fmp_mean)
fmp_mean


fmp_mean_tetra_coord <- 
  cbind(
    trans_comp_to_tetra(fmp_mean),
    obs_labs = 
      paste0(
        "Time-use sample mean",
        "<br>sleep = ", sprintf("%6.2f", 24 * fmp_mean[1, 1]),
        "<br>sb = ", sprintf("%6.2f", 24 * fmp_mean[1, 2]),
        "<br>lpa = ", sprintf("%6.2f", 24 * fmp_mean[1, 3]),
        "<br>mvpa = ", sprintf("%6.2f", 24 * fmp_mean[1, 4])
      )
  )



vfa <- read_csv("dat/vfa_pred_child_newcomp.csv")
vfa <- 
  vfa %>%
  rename(VAT = pred)


# ---- fmp_setup ----



fmp_mean <- col_geo_mean(fmp[, c("sleep", "sb", "lpa", "mvpa")])
fmp_mean <- clo(fmp_mean)
sum(fmp_mean)
fmp_mean

s <- seq(0, 1, 0.25)
strt <- c(0, 0, 0, 0)

tetra_axis_lst <- vector(mode = "list", length = 4)
for (j in 1:4) {
  
  strt_j <- strt
  strt_j[j] <- 1
  fnsh_j <- (1 - strt_j) / 3 # c(0, 1/3, 1/3, 1/3)
  
  axis_seq <- 
    foreach(i =  1:length(s), .combine = rbind) %do% {
      s[i] * strt_j + (1 - s[i]) * fnsh_j 
    }
  
  tetra_tmp <- cbind(trans_comp_to_tetra(axis_seq), txt = s)
  
  tetra_axis_lst[[j]] <- tetra_tmp
}

tetra_surf_lst <- vector(mode = "list", length = 4)
m_vec <- fmp_mean
for (j in 1:4) {
  
  offset_tmp <- rep(0, 4)
  offset_tmp[j] <- m_vec[j]
  
  surf_plane <- 
    foreach(i =  1:4, .combine = rbind) %do% {
      if (i == j) {
        NULL
      } else {
        offset_tmp_i <- offset_tmp
        offset_tmp_i[i] <- 1 - m_vec[j]
        clo(offset_tmp_i)
      }
    }
  
  tetra_tmp <- trans_comp_to_tetra(surf_plane)
  
  tetra_surf_lst[[j]] <- tetra_tmp
}


tetra_axis_lst_hrs <- tetra_axis_lst
for (i in 1:4) {
  tetra_axis_lst_hrs[[i]][["txt"]] <-
    24 * tetra_axis_lst_hrs[[i]][["txt"]]
}





# ---- vfa_setup ----



# vfa_mean <- col_geo_mean(vfa[, c("sleep", "sb", "lpa", "mvpa")])
# vfa_mean <- clo(vfa_mean)
# sum(vfa_mean)
# vfa_mean
# 
# s <- seq(0, 1, 0.25)
# strt <- c(0, 0, 0, 0)
# 
# tetra_axis_lst <- vector(mode = "list", length = 4)
# for (j in 1:4) {
#   
#   strt_j <- strt
#   strt_j[j] <- 1
#   fnsh_j <- (1 - strt_j) / 3 # c(0, 1/3, 1/3, 1/3)
#   
#   axis_seq <- 
#     foreach(i =  1:length(s), .combine = rbind) %do% {
#       s[i] * strt_j + (1 - s[i]) * fnsh_j 
#     }
#   
#   tetra_tmp <- cbind(trans_comp_to_tetra(axis_seq), txt = s)
#   
#   tetra_axis_lst[[j]] <- tetra_tmp
# }
# 
# 
# tetra_surf_lst <- vector(mode = "list", length = 4)
# m_vec <- vfa_mean
# for (j in 1:4) {
#   
#   offset_tmp <- rep(0, 4)
#   offset_tmp[j] <- m_vec[j]
#   # offset_tmp <- clo(offset_tmp)
#   
#   surf_plane <- 
#     foreach(i =  1:4, .combine = rbind) %do% {
#       if (i == j) {
#         NULL
#       } else {
#         offset_tmp_i <- offset_tmp
#         offset_tmp_i[i] <- 1 - m_vec[j]
#         clo(offset_tmp_i)
#       }
#     }
#   
#   tetra_tmp <- trans_comp_to_tetra(surf_plane)
#   
#   tetra_surf_lst[[j]] <- tetra_tmp
# }
# 
# 
# tetra_surf_min_lst <- vector(mode = "list", length = 4)
# m_vec <- apply(vfa[, c("sleep", "sb", "lpa", "mvpa")], 2, min)
# for (j in 1:4) {
#   
#   offset_tmp <- rep(0, 4)
#   offset_tmp[j] <- m_vec[j]
#   # offset_tmp <- clo(offset_tmp)
#   
#   surf_plane <- 
#     foreach(i =  1:4, .combine = rbind) %do% {
#       if (i == j) {
#         NULL
#       } else {
#         offset_tmp_i <- offset_tmp
#         offset_tmp_i[i] <- 1 - m_vec[j]
#         clo(offset_tmp_i)
#       }
#     }
#   
#   tetra_tmp <- trans_comp_to_tetra(surf_plane)
#   
#   tetra_surf_min_lst[[j]] <- tetra_tmp
# }
# 
# 
# tetra_surf_max_lst <- vector(mode = "list", length = 4)
# m_vec <- apply(vfa[, c("sleep", "sb", "lpa", "mvpa")], 2, max)
# for (j in 1:4) {
#   
#   offset_tmp <- rep(0, 4)
#   offset_tmp[j] <- m_vec[j]
#   # offset_tmp <- clo(offset_tmp)
#   
#   surf_plane <- 
#     foreach(i =  1:4, .combine = rbind) %do% {
#       if (i == j) {
#         NULL
#       } else {
#         offset_tmp_i <- offset_tmp
#         offset_tmp_i[i] <- 1 - m_vec[j]
#         clo(offset_tmp_i)
#       }
#     }
#   
#   tetra_tmp <- trans_comp_to_tetra(surf_plane)
#   
#   tetra_surf_max_lst[[j]] <- tetra_tmp
# }















