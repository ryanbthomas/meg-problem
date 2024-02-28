pack_untransformed <- function(v) {
    num_v <- length(v)
    num_pad <- 2^ceiling(log2(num_v)) - num_v
    v <- c(v, rep(0, num_pad))
    idx <- seq(from = 1, to = length(v), by = 2)
    complex(real = v[idx], imaginary = v[idx + 1])
}

unpack_untransformed <- function(v) {
    tmp <- purrr::map(v, ~ c(Re(.x), Im(.x)))
    purrr::list_c(tmp)
}

pack_roots_of_unity <- function(n) {
    prim_root_of_unity <- exp(2*pi*(0+1i)/n)
    sqrt(prim_root_of_unity)^c(0:n)
}
unpack_transformed <- function(ph) {
    n <- length(ph)
    ph <- c(ph, ph[1])
    ph2 <- Conj(rev(ph))
    roots_of_unity <- pack_roots_of_unity(n)
    ph3 <- (0-1i) * (ph - ph2) * roots_of_unity
    0.5 * (ph + ph2 + ph3)
}

pack_transformed <- function(r) {
    n <- length(r) - 1
    r2 <- Conj(rev(r))
    roots_of_unity <- pack_roots_of_unity(n)
    r3 <- (0+1i) * (r - r2) * roots_of_unity
    
    ph <- 0.5 * (r + r2 + r3)
    ph[-(n+1)]
    
}

binary_exponentiation <- function(a, m) {
    z <- a
    rtmp <- rev(intToBits(m))
    first_digit <- which(rtmp == 1)[1]
    bin_of_m <- rtmp[-seq_len(first_digit - 1)]
    b <- bin_of_m[-1]
    while(length(b) > 0) {
        z <- z^2
        if (b[1] == 1) {
            z <- z * a
        }
        b <- b[-1]
    }
    z
}

count_it_up_fft <- function(k, n, d) {
    v <- rep(0, k * d)
    v[1:k] <- 1/k
    
    fv <- fft(v)
    cfv <- binary_exponentiation(fv, d)
    cv <- fft(cfv, inverse = TRUE) / length(cfv)
    
    Re(cv[n - d  + 1]) 
}

count_it_up_fft_wpacking <- function(k, n, d) {
    v <- rep(0, k * d)
    v[1:k] <- 1/k
    
    pv <- pack_untransformed(v)
    
    fpv <- fft(pv)
    
    fv <- unpack_transformed(fpv)
    
    cfv <- binary_exponentiation(fv, d)
    
    cfpv <- pack_transformed(cfv)
    
    cpv <- fft(cfpv, inverse = TRUE) / length(cfpv)
    
    cv <- unpack_untransformed(cpv)
    
    Re(cv[n - d  + 1]) 
}

