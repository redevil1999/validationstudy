# To work only on the "corrupted" rows.
dat <- sliced_MA_data[72:151]

# Make new colnames to more easily handle them
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

# ---
# Some participants seem to falsely have been flagged as MA1
# Sum up the non-missing cells. If they have MA, should be > 20.
rowSums(!is.na(dat), na.rm = FALSE)
# yet some have exactly 20.

# Which of them?
false_flags <- which(rowSums(!is.na(dat), na.rm = FALSE) == 20)
false_flags

# Which MA are not actually MA?
Unique_MA_List[false_flags]
# We can look at these values in the ValidationStudy_Preprocessing.R script, 
# where values are added to MultipleAnswer_List.

# To verify that e.g. row 3 does indeed not have MA
dat |> 
  slice(3) |> 
  glimpse()

