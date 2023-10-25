compute_contingency_table <- function(x, y, z) {
  # Compute the initial contingency table
  contingency_table <- table(x, y, z)
  
  # Check the dimensions of the contingency table
  nrow_ct <- nrow(contingency_table)
  ncol_ct <- ncol(contingency_table)
  
  # Check and pad rows if necessary
  if (nrow_ct < 2) {
    contingency_table <- rbind(contingency_table, numeric(2 - nrow_ct, ncol_ct))
  }
  
  # Check and pad columns if necessary
  if (ncol_ct < 2) {
    contingency_table <- cbind(contingency_table, numeric(nrow_ct, 2 - ncol_ct))
  }
  
  return(contingency_table)
}

# Example usage:
a <- data.frame(x = c(1, 2, 1, 2, 1),
                 y = c(1, 1, 2, 2, 1),
                 z = c(1, 1, 1, 0, 0))
table(a$x, a$y, a$z)
compute_contingency_table(a$x, a$y, a$z)

a <- data.frame(x = c(1, 1, 1, 2, 1),
                y = c(1, 1, 2, 2, 1),
                z = c(1, 1, 1, 0, 0))
table(a$x, a$y, a$z)
compute_contingency_table(a$x, a$y, a$z)

a <- data.frame(x = c(1, 1, 1, 1, 1),
                y = c(1, 1, 2, 2, 1),
                z = c(1, 1, 1, 0, 0))
table(a$x, a$y, a$z)
compute_contingency_table(a$x, a$y, a$z)


compute_contingency_table <- function(x, y, z) {
  # Compute the initial contingency table
  contingency_table <- table(x, y, z)
  
  # Extract values from the contingency table
  values <- as.vector(contingency_table)
  
  # Ensure that there are at least 4 values (2 rows x 2 columns)
  if (length(values) < 4) {
    # Pad with zeros if necessary to make it 2x2
    values <- c(values, rep(0, 4 - length(values)))
  }
  
  # Create a new 2x2 contingency table with the padded values
  padded_table <- matrix(values, nrow = 2, ncol = 2)
  
  return(padded_table)
}

