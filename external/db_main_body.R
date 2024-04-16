# Main

# tabs
# sourced in global.R
# ref in db_main_body.R
# menu in db_main_sb.R

function(id) {

    tabItems(
      tabItem(tabName = "tab_import"
                , tab_code_import())
      , tabItem(tabName = "tab_AUjoin"
                , tab_code_tab_AUjoin())
    )## tabItems

}## FUNCTION ~ END
