#Sidebar----

# tabs
#sb_main <- function(id) {
function(id) {
  dashboardSidebar(
    width = 275
    , HTML("&nbsp;&nbsp;<font size=5><b>Steps</b></font>")
    #Steps, do *not* need to be done sequentially----
    , sidebarMenu(id = id
      , menuItem(text = "Import Files"
                 , tabName = "tab_import"
                 , icon = icon("file-arrow-up")
                 , startExpanded = TRUE)
      , menuItem(text = "Join AUs"
                 , tabName = "tab_AUjoin"
                 , icon = icon("pen-to-square")
                 , startExpanded = TRUE)
    )## sidebarMenu ~ END
  )## dashboardSidebar ~ END
}## FUNCTION ~ END
