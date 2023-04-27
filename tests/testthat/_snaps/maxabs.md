# empty printing

    Code
      rec
    Message <cliMessage>
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Operations 
    Output
      Max Abs scaling on <none>

---

    Code
      rec
    Message <cliMessage>
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      outcome:    1
      predictor: 10
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
    Output
      Max Abs scaling on <none> [trained]

# printing

    Code
      print(rec)
    Message <cliMessage>
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 11
      
      -- Operations 
    Output
      Max Abs scaling on all_predictors()

---

    Code
      prep(rec)
    Message <cliMessage>
      
      -- Recipe ----------------------------------------------------------------------
      
      -- Inputs 
      Number of variables by role
      predictor: 11
      
      -- Training information 
      Training data contained 32 data points and no incomplete rows.
      
      -- Operations 
    Output
      Max Abs scaling on mpg, cyl, disp, hp, drat, wt, qsec, vs, am, ge... [trained]

