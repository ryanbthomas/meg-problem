io <- function(x) x + 1L
count_the_ways <- function(k, n = (k * (k + 1L)) %/% 2L, d = k) {
  dim_names <- purrr::map(c(k, n, d), ~ as.character(seq(0L, .x)))
  mem <- array(0L, dim = c(k + 1L, n + 1L, d + 1L), dimnames = dim_names)
  #browser()
  #mem[io(0L), io(0L), io(0L)] <- 1L
  for (num_draws in seq(from = 1L, to = d)) {
    for (max_int in seq(from = 1L, to = k)) {
      for (sum_to in seq(from = 1L, to = min(n, num_draws * max_int))) {
       
        if (num_draws > sum_to) next()
        val <- mem[io(max_int - 1L), io(sum_to), io(num_draws)]
        
        if (max_int >= 1L && max_int <= sum_to) {
          mult <- min(sum_to %/% max_int, num_draws)
          
          for (m in seq(from = 1L, to = mult)) {
              if (m * max_int == sum_to && m == num_draws) {        
                val <- val + 1L
                next()
              }
              
              to_add <- choose(num_draws, m) * mem[io(max_int - 1L), io(sum_to - m * max_int), io(num_draws - m)]
              val <- val + to_add
          
          #  }
          }
        }
        mem[io(max_int),  io(sum_to), io(num_draws)] <- val
      }
    }
  }
  mem[io(k), io(n), io(d)]
  #mem
}
count_the_ways_v <- function(k, n = (k * (k + 1L)) %/%2L, d = k, debug = FALSE) {
  dim_names <- purrr::map(c(k, n, d), ~ as.character(seq(0L, .x)))
  mem <- array(0L, dim = c(k + 1L, n + 1L, d + 1L), dimnames = dim_names)
  
  all_sums <- seq(0L, to = n)
  
  for (num_draws in seq(from = 1L, to = d)) {
    for (max_int in seq(from = 1L, to = k)) {
      
        val <- rep(0L, n + 1L)
        idx <- seq(1L, min(n, num_draws * max_int))
     
        val[io(idx)] <- mem[io(max_int - 1L), io(idx), io(num_draws)]
        idx <- num_draws * max_int == all_sums
        if (any(idx)) {
          val[idx] <- val[idx] + 1L
        }
     
        idx <- all_sums >= max_int & all_sums < num_draws * max_int
       
        if (any(idx)) {
          m <- 1L
      
          use_idx <- all_sums[idx] - m * max_int >= 0
          while(any(use_idx)) {
            val[idx][use_idx] <- val[idx][use_idx] + choose(num_draws, m) * mem[io(max_int - 1L), io(all_sums[idx][use_idx] - m * max_int), io(num_draws - m)]
            m <- m + 1L
            use_idx <- all_sums[idx] - m * max_int >= 0
          }
        }
        mem[io(max_int), , io(num_draws)] <- val
      #}
    }
  }
  if (debug) return(mem)
  mem[io(k), io(n), io(d)]
  #mem
}

count_the_ways_e <- function(k, n = k * (k + 1) %/% 2, d = k) {
  dim_names <- purrr::map(c(k, n, d), ~ as.character(seq(0L, .x)))
  e <- new.env()
  e$mem <- array(0L, dim = c(k + 1, n + 1, d + 1), dimnames = dim_names)
 
  for (max_int in seq(from = 1L, to = k)) {
    for (num_draws in seq(from = 1, to = d)) {
      for (sum_to in seq(from = 1L, to = min(n, num_draws * max_int))) {
       
        if (num_draws > sum_to) next()
        val <- e$mem[io(max_int - 1L), io(sum_to), io(num_draws)]
        
        if (max_int >= 1 && max_int <= sum_to) {
          mult <- min(sum_to %/% max_int, num_draws)
          
          for (m in seq(from = 1L, to = mult)) {
          
            if (m * max_int == sum_to && m == num_draws) {    
              val <- val + 1L
              next()
            }
            
            to_add <- choose(num_draws, m) * e$mem[io(max_int - 1L), io(sum_to - m * max_int), io(num_draws - m)]
            val <- val + to_add
            
            #  }
          }
        }
        e$mem[io(max_int),  io(sum_to), io(num_draws)] <- val
      }
    }
  }
  e$mem[io(k), io(n), io(d)]
  #mem
}
