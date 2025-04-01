# Get data from EMS and process it for table

# --- Authentication
authn_resource <- "75deca06-ae07-4765-85c0-23e719062833" # Scope / ID for the EMS2 Core API
authn_tenant <- "f610c0b7-bd24-4b39-810b-3dc280afb590" # Tenant id of WHO as registered in WHO AAD
authn_app <- "1b9dce3e-9b1c-4166-aaa7-303cc4407e48" # Client ID / App ID for "WHE GIS dashboard"
authn_password <- "B~V8Q~dE1Gb3I76wMQPo2TEWD26IV8E6Ge.u2cSK" # 'Value' field from the App secret 

authn_auth_type <- "client_credentials"
authn_use_cache <- TRUE

tok0 <- AzureAuth::get_azure_token(resource=authn_resource,
                                   tenant=authn_tenant,
                                   app=authn_app,
                                   auth_type=authn_auth_type,
                                   password=authn_password ,
                                   use_cache=authn_use_cache)

list_azure_tokens()

access_token <- tok0$credentials$access_token

conn <- GraphqlClient$new(
  url = "https://ems2.who.int/csapi/api/graphql",
  headers = list(Authorization = paste0("Bearer ", access_token))
)

# Make query
qry <- Query$new()

qry$query('ems2_evt', '{
                   occurrencehistorys(occurrencetype: 0, iscurrent: 1, eventstatusid: 2) {
                      countryname
                      iso2code
                      regionname
                      occurrencename
                      createddate
                      modifieddate
                      latesthazardid
                      latesthazardname
                      latesthazardimagepath
                      latestdiseasecondid
                      latestdiseasecondname
                      eventid
                      eventstatusname
                      }
          }')


# returns json
x <- conn$exec(qry$queries$ems2_evt)

# Convert into Json, then to dataframe
ems.tab <- jsonlite::fromJSON(x)$data$occurrencehistorys %>% 
  mutate(createddate = as.Date(createddate),
         eventid = str_replace_all(eventid, ' ', ''),
         link = paste0('https://ems.who.int/Event/eventSummary.aspx?eid=', eventid),
         # link = paste0('https://ems.who.int/Event/eventSummary.aspx?eid=', eventid, '/')
         ) %>% 
  filter(createddate == Sys.Date() - 1) %>%
  select(eventid, latestdiseasecondname, countryname, link) %>% 
  rename(`Event ID` = eventid, 
         `Disease/Condition` = latestdiseasecondname, 
         Country = countryname)

