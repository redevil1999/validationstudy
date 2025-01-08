# Subset only STAI columns ----
# To work only on the "corrupted" rows.
# dat <- sliced_MA_data[72:151]
# To work on the entire dataset
dat <- firstdatacheck[72:151]

# New column names ----
# Make new colnames to more easily use regex on them.
nums <- rep(1:4)
nums <- paste0("_", nums)

# I am a basic bitch and will sometimes use loops even though I know R is 
# vectorised and there is probably some way to elegantly apply or map this.
out <- c()
for (elem in stais) {
  out <- c(out, rep(elem, 4))
}
new_colnames <- paste0(out, nums)
names(dat) <- new_colnames
rm(out, new_colnames)

# Check of sliced_MA_data ---
# To do this, work on sliced_MA_data[72:151] above, not firstdatacheck.
# Some participants seem to falsely have been flagged as MA1
# Sum up the non-missing cells. If they have MA, should be > 20.
# rowSums(!is.na(dat), na.rm = FALSE)
# yet some have exactly 20.

# Which of them?
# false_flags <- which(rowSums(!is.na(dat), na.rm = FALSE) == 20)
# false_flags

# Which MA are not actually MA?
# Unique_MA_List[false_flags]
# We can look at these values in the ValidationStudy_Preprocessing.R script, 
# where values are added to MultipleAnswer_List.

# To verify that e.g. row 3 does indeed not have MA
# dat |> 
#   slice(3) |> 
#   glimpse()


# ---

# Turn the cols into numerics to be able to do math on them.
dat <- dat |> 
  mutate(
    across(everything(),
           \(x) x = case_when(
             x == "Nesten aldri" ~ 1,
             x == "Av og til" ~ 2,
             x == "Ofte" ~ 3,
             x == "Nesten alltid" ~ 4,
             TRUE ~ NA
           ))
  )

# Check MA ----
# Counts number of endorsements for each of 20 STAI items. Expected: 1, but some
# endorsed more than one (are MA). 
# Iterates over each set of four columns (the four item scales of a single 
# STAI item). For each, counts non-NA cells. Expected is 1 if only 1 item scale 
# endorsed. Creates a new df of numbers of endorsements for each item.
tmp <- 1
out <- dat[1]
for (column in seq(1, 80, 4)) {
  foo <- dat[, column:(column+3)] 
  out[tmp] <- rowSums(!is.na(foo), na.rm = TRUE)
  
  # Rename the column.
  names(out)[tmp] <- names(dat[column]) |>
    str_extract("STAI_Q[[:digit:]]{1,2}(?=_[[:digit:]]+$)")
  tmp <- tmp + 1
}
rm(tmp, foo)
out

# Out is missing the ID column, but the rows should be in the same order. The
# ID could be added back in easily.
out <- out |> 
  mutate(sessionid = firstdatacheck$session_id,
         .before = 1)
