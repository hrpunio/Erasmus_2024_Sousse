# -- Sousse 2024 --
library("tidyverse")
library("knitr")
library("ggplot2")
library("googlesheets4")

## Attitudes towards statistics survey (Sousse 2024)
## r :: reverse questions
## ================================================
question.names <- c("p1", "p2r", 'p3r', 'p4', 'p5r', 'p6r', 'p7',
            'p8', 'p9r', "p10r", "p11r", 'p12r', 'p13', 'p14r', 
            'p15', 'p16r', 'p17',
            'p18r', 'p19r', "p20r", "p21r", 'p22r', 'p23', 'p24', 'p25r', 'p26r', 'p27r', 'p28r',
            'age', 'sex')
##
q.names <- c('time', question.names)

## Starting page: https://tprzechlewski.blogspot.com/p/erasmus-2024.html
## #############
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
  ## Compute dimensions (means for comparison):
   afect = (p1 + p2 + p11 + p14 + p15 + p21) / 6 ,
   competence = (p3 + p9 + p20 + p23 + p24 + p27) / 6,
   value = (p5 + p7 + p8 + p10 + p12 + p13 + p16 + p19 + p25) / 9,
   difficulty = (p4 + p6 + p17 + p18 + p22 + p26 + p28) / 7
   ) %>%
  select (pws, sex, age, afect, competence, value, difficulty )
####
### s0 :: ready to analize

## M/F structure
sex.f <- s0 |> select (sex) |> group_by(sex) |>
  summarize( n=n() ) |>
  mutate(prop=n/sum(n) * 100 )

sex.t <- kable(sex.f, col.names = c('płeć', 'n', '%'))
sex.t

## chart
p.1 <- ggplot(sex.f, aes(x = reorder(sex, n), y = n )) +
  ggtitle("Respondents by gender") +
  xlab("") + ylab("%") +
  ## position=dodge
  geom_bar(position = 'dodge', stat = 'identity', fill = "steelblue") +
  geom_text(data=sex.f, aes(label=sprintf("%.2f", prop), y= prop), hjust=1.5, color="white" ) +
  coord_flip()
p.1

##
## AtS by gender
##
ex.sex.f <- s0 |>
  select (pws, sex) |>
  group_by(sex) |>
  summarize(m=mean(pws))

ex.sex.t <- kable(ex.sex.f, col.names = c('płeć', 'średni pws'))
ex.sex.t

##
## Chart
##
p.2 <- ggplot(ex.sex.f, aes(x = reorder(sex, pws), y = pws )) +
  ggtitle("PwS wg płci") +
  xlab("") + ylab("%") +
  geom_bar(stat = 'identity', fill = "steelblue") +
  geom_text(aes(label=sprintf("%.2f", pws), y= pws), hjust=1.5, color="white" ) +
  coord_flip()

p.2

##
## afect, competence, value, difficulty 
##

p.3 <- s0 |> select (afect, competence, value, difficulty) |>
  pivot_longer(cols = everything(),
    names_to = 'facet', values_to = 'value') |>
  ggplot(aex(y=value, x=facet)) +
  geom_boxplot() +
  ggtitle('Affect, compenence, value and difficulty')

p.3

##
## ats vs age
##
ggplot(s0, aes(x=age, y=pws)) +
  geom_smooth(method='lm', se=FALSE) +
  geom_point()


## linear regression

m1 <- lm(data=s0, pws ~ age )
summary(m1)

## End ##
