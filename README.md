This is a Shiny App used to produce slides with the signals tracked by the PHI team as well as the products produced.

### **Inputs (all Excel files):**
- Signal App list
- Signal characterization
- Product tracker
- Tracker reports (optional)

The product tracker and tracker reports are both compiled into one table.


### **Output:**
- Slide deck with the slides - the slides are produced using the R package officer().



### **Notes on infrastructure:**
- This is a Shiny App using the framework with `ui.R` and `server.R`. There is a script called `global.R` where all the scripts are sourced.
- The main code is in `R/funcs`. This includes:
  - Scripts to read and clean the input files
  - `slide_run.R` - script that compiles
  - Note: the slides are added to a slide template that already includes some slides (some of which are hidden). The template is in `data/template/slide_template.pptx`
- Some data is retrieved directly using the EMS API. The script for this is `R/funcs/ems_signals.R`
- The app has two versions, *test* and *main*. The *test* version is usually used first when deploying new changes. Once the team has had the chance to use (usually during one week), the changes are deployed to the *main* app. They are on the internal R Shiny server:
  - test app: https://worldhealthorg.shinyapps.io/phi_morning_slides_test/
  - main app: https://worldhealthorg.shinyapps.io/phi_morning_slides-main/
