# check_name() is used

    Code
      prep(rec, training = dat)
    Condition
      Error in `step_date_after()`:
      Caused by error in `bake()`:
      ! Name collision occurred. The following variable names already exist:
      * `date1_after_weekend`

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

