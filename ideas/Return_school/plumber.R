#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

df <- tibble::tribble(
  ~regierungsbezirk,     ~absolut,     ~ms,    ~fz,     ~rs,    ~ws,     ~gym,    ~wald,
  "Oberbayern"  ,  43.253,     27  ,   3.5  ,   30.5  ,   1.9  ,   35.9  ,   1.2  ,
  "Niederbayern"  ,  11.578,   33.8  ,   4.3  ,     34  ,   2.4  ,   25.4  ,     0  ,
  "Oberpfalz"  ,   9.713,   32.7  ,   4.1  ,   31.7  ,   2.7  ,   28.8  ,     0  ,
  "Oberfranken"  ,   8.876,   29.8  ,   4.1  ,   29.7  ,   2.8  ,   31.3  ,   2.3  ,
  "Mittelfranken"  ,  16.242,   31.3  ,     5  ,   27.5  ,   3.5  ,   31.7  ,     1  ,
  "Unterfranken"  ,  11.733,   29.8  ,     5  ,   33.2  ,   1.8  ,   29.7  ,   0.5  ,
  "Schwaben"  ,  17.267,   32.9  ,   4.2  ,   32.9  ,   2.2  ,   27.4  ,   0.4  ,
  "Bayern insgesamt"  , 118.662,   30.1  ,   4.1  ,   31.1  ,   2.3  ,   31.5  ,   0.9  
)


library(plumber)
library(dplyr)

#* @apiTitle School API
#* @apiDescription Get the numbers from the Bavarian schools. The api returns the 
#* regierungsbezirk and 

#* Return schools
#* @param var1
#* @param var2  
#@serializer csv
#* @get /return_schools
function(var1, var2 = NULL, var3 = NULL) {
  varlist <- c("regierungsbezirk", var1, var2)
  
  df_new <- df |> 
    select(all_of(varlist))
  
  return(df_new)
}
# Programmatically alter your API
#* @plumber
function(pr) {
    pr %>%
        # Overwrite the default serializer to return unboxed JSON
        pr_set_serializer(serializer_unboxed_json())
}
