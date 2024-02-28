count_it_up_meg <- function(k, n, debug = FALSE) {
    for (i in 1:k) {
        if (i == 1) {
            tbl <- tibble::tibble(sum_j = 1:k, cnt = 1)
        } else {
            tbl_temp <- tbl |>
                tidyr::crossing(j_i = 1:k)
            tbl_temp$sum_j <- tbl_temp$sum_j + tbl_temp$j_i
            
            tbl_temp2 <- tbl_temp |>
                dplyr::group_by(sum_j) |>
                dplyr::summarise(cnt = sum(cnt))
            tbl <- tbl_temp2[tbl_temp2$sum_j + (k-i)* k >= n & tbl_temp2$sum_j + (k-i) <= n, ]
        }
    }
    if (debug) return(tbl$cnt)
    tbl$cnt / k^k
}

count_it_up_meg_mod <- function(k, n, debug=FALSE) {
    my_prime <- 104395301L
    for (i in 1:k) {
        if (i == 1L) {
            tbl <- tibble::tibble(sum_j = 1:k, cnt_div = 0L, cnt = 1L)
        } else {
            tbl_temp <- tbl |>
                tidyr::crossing(j_i = 1:k)
            tbl_temp$sum_j <- tbl_temp$sum_j + tbl_temp$j_i
            
            tbl_temp2 <- tbl_temp |>
                dplyr::group_by(sum_j) |>
                dplyr::summarise(cnt_div = sum(cnt_div) + sum(cnt) %/% my_prime, cnt = sum(cnt) %% my_prime)
            tbl <- tbl_temp2[tbl_temp2$sum_j + (k-i)* k >= n & tbl_temp2$sum_j + (k-i) <= n, ]
        }
    }
    if (debug) return(tbl)
    in_log_space <- log(tbl$cnt_div + tbl$cnt / my_prime) + log(my_prime) - k *log(k)
    exp(in_log_space)
}
