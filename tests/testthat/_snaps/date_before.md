# time_event errors

    Code
      recipe(~., data = examples) %>% step_date_before(numeric, rules = rules) %>%
        prep()
    Condition
      Error in `step_date_before()`:
      Caused by error in `prep()`:
      ! All variables for `step_date` should be either `Date` or`POSIXct` classes.

---

    Code
      recipe(~., data = examples) %>% step_date_before(date1, rules = "wrong") %>%
        prep()
    Condition
      Error in `step_date_before()`:
      Caused by error in `prep()`:
      ! `rules` must be a named list.

---

    Code
      recipe(~., data = examples) %>% step_date_before(date1, rules = list(weekend = on_weekends,
        "Hello")) %>% prep()
    Condition
      Error in `step_date_before()`:
      Caused by error in `prep()`:
      ! All `rules` must be `rschedule`s from {almanac}

---

    Code
      recipe(~., data = examples) %>% step_date_before(date1, rules = list(weekend = on_weekends,
        christmas = "2020-12-25")) %>% prep()
    Condition
      Error in `step_date_before()`:
      Caused by error in `prep()`:
      ! All `rules` must be `rschedule`s from {almanac}

# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_date_before()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `date1_before_weekend`

# empty printing

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Operations 
    Output
      Time events from 

---

    Code
      rec
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
    Output
      Time events from  [trained]

# printing

    Code
      print(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Operations 
    Output
      Time events from date1

---

    Code
      prep(rec)
    Message
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 1
      
      -- Training information 
      Training data contained 20 data points and no incomplete rows.
      
      -- Operations 
    Output
      Time events from date1 [trained]

