sleep <- function(value) {Sys.sleep(value)} 

setScale <- function(value) {
  webElem <- remDr$findElement(using = 'id', value = "scale")
  webElem$sendKeysToElement(list("\uE003", "\uE003", "\uE003", as.character(value), "\uE004", "\uE007"))
}

printPage <- function() {remDr$screenshot(display = TRUE)}

getPageLink <- function() {
  webElem <- remDr$findElement(using = 'id', value = "thePage")
  webElem$getElementAttribute("src")[[1]]
}

getCurrentNumPage <- function() {
  as.numeric(
    remDr$findElement(
      using = 'id', 
      value = "curPageNum"
    )$getElementAttribute("value")[[1]]
  )
}

authorizationInLibrary <- function() {
  remDr$navigate("https://elib.bstu.ru/Account/OpenID")
  remDr$screenshot(display = TRUE)
  
  webElem <- remDr$findElement(using = 'class', value = "google")
  webElem$clickElement()
  printPage()
  
  sleep(1)
  webElem <- remDr$findElement(using = 'id', value = "0_sb")
  webElem$clickElement()
}

authorizationInGoogle <- function() {
  webElem <- remDr$findElement(using = 'id', value = "identifierId")
  webElem$clickElement()
  webElem$sendKeysToElement(list("ISPritchin@gmail.com", key = "enter"))
  sleep(4)
  webElem <- remDr$findElement(using = 'css', value = "[tabindex='0']")
  webElem$sendKeysToElement(list("ISP - 3/14", key = "enter"))
  printPage()
}

saveImage <- function(i) {remDr$screenshot(file = str_c(c("C:/Users/Данил/Desktop/1/", as.character(i), ".png"), collapse = ""))}

nextPage <- function() {
  el <- remDr$findElements(using = 'class', value = "nextbutton")[[1]]
  el$clickElement()  
}
