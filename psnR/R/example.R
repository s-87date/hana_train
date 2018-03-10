library(httr)

sso.url <- 'https://auth.api.sonyentertainmentnetwork.com/2.0/ssocookie'
code.url <- 'https://auth.api.sonyentertainmentnetwork.com/2.0/oauth/authorize'
oauth.url <- 'https://auth.api.sonyentertainmentnetwork.com/2.0/oauth/token'

# login request
auth.type <- "password"
client.id <- "71a7beb8-f21a-47d9-a604-2e71bee24fe0"
user.email <- "shu87.scarycat@gmail.com"
user.pwd <- "shu87date"

# oauth request
app.context <- "inapp_ios"
client.id <- "b7cbf451-6bb6-4a5a-8913-71e61f462787"
client.secret <- "zsISsjmCx85zgCJg"
code <- None
duid <- "0000000d000400808F4B3AA3301B4945B2E3636E38C0DDFC"
grant.type <- "authorization_code"
scope <- "capone:report_submission,psn:sceapp,user:account.get,user:account.settings.privacy.get,user:account.settings.privacy.update,user:account.realName.get,user:account.realName.update,kamaji:get_account_hash,kamaji:ugc:distributor,oauth:manage_device_usercodes"

# code reqest
state <- "06d7AuZpOmJAwYYOWmVU63OMY"
duid <- "0000000d000400808F4B3AA3301B4945B2E3636E38C0DDFC"
app_context <- "inapp_ios"
client_id <- "b7cbf451-6bb6-4a5a-8913-71e61f462787"
scope <- "capone:report_submission,psn:sceapp,user:account.get,user:account.settings.privacy.get,user:account.settings.privacy.update,user:account.realName.get,user:account.realName.update,kamaji:get_account_hash,kamaji:ugc:distributor,oauth:manage_device_usercodes"
response_type <- "code"

# refresh oauth request
app_context <- "inapp_ios"
client_id <- "b7cbf451-6bb6-4a5a-8913-71e61f462787"
client_secret <- "zsISsjmCx85zgCJg"
refresh_token <- None
duid <- "0000000d000400808F4B3AA3301B4945B2E3636E38C0DDFC"
grant_type <- "refresh_token"
scope <- "capone:report_submission,psn:sceapp,user:account.get,user:account.settings.privacy.get,user:account.settings.privacy.update,user:account.realName.get,user:account.realName.update,kamaji:get_account_hash,kamaji:ugc:distributor,oauth:manage_device_usercodes"

# two factor auth_request
authentication_type <- "two_step"
ticket_uuid <- None
code <- None
client_id <- "b7cbf451-6bb6-4a5a-8913-71e61f462787"





