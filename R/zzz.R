## Absurd hack to keep R from issuing a bunch of NOTEs about global variables.
## Just list any such variable here.

utils::globalVariables(c(
  "first.name", "last.name", "title", "department", "status", "leave",
  "first.degree", "first.degree.school", "last.degree", "last.degree.school",
  "first.degree.year", "last.degree.year", "latin.honors", "honor", "major",
  "Phi.Beta.Kappa", "Sigma.Xi",
  "name", "year_min", "proportion_male", "proportion_female",
  "gender", "year", "birth.year",
  "p_asi", "p_bla", "p_his", "p_oth", "p_whi",
  "race", "surname", "surname.match"))

