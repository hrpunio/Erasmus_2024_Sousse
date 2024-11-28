library("tidyverse")
library("knitr")
library("ggplot2")
library("googlesheets4")

## Attitudes towards statistics survey (Sousse 2024)
## ================================================
question.names <- c("p1", "p2r", 'p3r', 'p4', 'p5r', 'p6r', 'p7',
            'p8', 'p9r', "p10r", "p11r", 'p12r', 'p13', 'p14r', 'p15', 'p16r', 'p17',
            'p18r', 'p19r', "p20r", "p21r", 'p22r', 'p23', 'p24', 'p25r', 'p26r', 'p27r', 'p28r',
            'age', 'sex')
##
q.names <- c('time', question.names)

## badaniu podlega tylko
##
## s01 <- read_sheet('1Njp2kcaLTB6DZuhyRX2yji67rwwAzhDDeZ0hf8jcNe4' )

## Link to survey form
## https://docs.google.com/forms/d/e/1FAIpQLSdX6Or1uujpzDQLy97qYCdhT0SVyXJg-FlMfKh6_Kn6rK6fXA/viewform?usp=sf_link
## Link to sheet
## https://docs.google.com/spreadsheets/d/1mReSaqPCWeLpXypA7ksThBObx92LYMK08Ey4GxajkoU/edit?resourcekey=&gid=902442048#gid=902442048

googlesheets4::gs4_deauth()
## Udostępnij->każda osoba mająca

s0 <- read_sheet('1RPn3sN6jCxmnYjTUF4Z9vkM6GNrGjQI9v6fkbGDei4U', skip = 1, col_names = q.names) |>
  ### remove test entries:
  filter ( as.POSIXct(time) > as.POSIXct('2024/12/01 0:00:00 AM') ) |>
  ###
  select (-one_of('time')) |>
  mutate(across(c("p1", "p2r", 'p3r', 'p4', 'p5r', 'p6r', 'p7',
            'p8', 'p9r', "p10r", "p11r", 'p12r', 'p13', 'p14r', 'p15', 'p16r', 'p17',
            'p18r', 'p19r', "p20r", "p21r", 'p22r', 'p23', 'p24', 'p25r', 'p26r', 'p27r', 'p28r'
         ), ~ case_when(
    . == "Strongly disegree" ~ 1,
    . == "Disegree"          ~ 2,
    . == "Somewhat disegree" ~ 3,
    . == "Neutral"           ~ 4,
    . == "Somewhat agree"    ~ 5,
    . == "Agree"             ~ 6,
    . == "Strongly agree"    ~ 7
  )))  |>
  ##
  mutate ( p2 = 8 - p2r,    p3 = 8 - p3r,   p5 = 8 - p5r,    p6 = 8 - p6r,
           p9 = 8 - p9r,   p10 = 8 - p10r,  p11 = 8 - p11r,  p12 = 8 - p12r,
           p14 = 8 - p14r, p16 = 8 - p16r,  p18 = 8 - p18r,
           p19 = 8 - p19r, p20 = 8 - p20r,  p21 = 8 - p21r,  p22 = 8 - p22r,
           p25 = 8 - p25r, p26 = 8 - p26r,  p27 = 8 - p27r,  p28 = 8 - p28r ) |>
  ##
  mutate (pws = p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 +
            p11 + p12 + p13 + p14 + p15 + p16 + p17 + p18 + p19  + p20 +
            p21 + p22 + p23 + p24 + p25 + p26 + p27 + p28,
   afect = p1 + p2 + p11 + p14 + p15 + p21,
   competence = p3 + p9 + p20 + p23 + p24 + p27,
   value = p5 + p7 + p8 + p10 + p12 + p13 + p16 + p19 + p25,
   difficulty = p4 + p6 + p17 + p18 + p22 + p26 + p28
   ) %>%
  select (pws, sex, age, afect, competence, value, difficulty )
####
### s0 :: ready to analize




