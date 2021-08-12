library(haven)
library(tidyverse)
library(car)
library(nortest)
library(grid)
library(gridExtra)
library(ggpubr)
library(lubridate)
library(datapasta)

df <- data.table::data.table(
         barometro = c("jul-18","jul-18","jul-18",
                       "jul-18","jul-18","oct-18","oct-18","oct-18","oct-18",
                       "oct-18","jan-19","jan-19","jan-19","jan-19",
                       "jan-19","mar-20","mar-20","mar-20","mar-20","mar-20",
                       "mar-20","oct-20","oct-20","oct-20","oct-20","oct-20",
                       "oct-20","jan-21","jan-21","jan-21","jan-21",
                       "jan-21","jan-21","apr-21","apr-21","apr-21","apr-21",
                       "apr-21","apr-21"),
             media = c(4.81,3.27,6.45,5.69,4.45,
                       4.05,2.34,5.53,5.13,3.53,3.5,2.3,5.2,3.4,3.2,4.3,
                       2.7,5.9,5.5,3.6,2.1,5,4.1,6.4,5.7,4.7,3.5,5,
                       4.3,6.3,5.2,4.9,3.3,5.3,4.9,6.7,5.4,5,3.7),
              voto = c("Total","PP","PSOE","UP",
                       "CS","Total","PP","PSOE","UP","CS","Total","PP",
                       "PSOE","UP","CS","Total","PP","PSOE","UP","CS","VOX",
                       "Total","PP","PSOE","UP","CS","VOX","Total","PP",
                       "PSOE","UP","CS","VOX","Total","PP","PSOE","UP",
                       "CS","VOX")
      )

df$barometro <- factor(df$barometro, level = c('jul-18', 'oct-18', 'jan-19', "mar-20",
                                               "oct-20", "jan-21", "apr-21"))

evolucion <- ggplot(data = df, mapping = aes(y = media, x = barometro, group = voto, colour=voto)) +
  geom_line(size=1.2) +
  geom_point(size=5) +
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=9),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=9),
        axis.title.x = element_text(color="black", size=9, face="bold"),
        axis.title.y = element_text(color="black", size=9, face="bold"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.caption = element_text(hjust = 1)) +
  scale_x_discrete(name = "")+
  scale_y_continuous(name="Valoración media Nadia Calviño", limits=c(1, 7, breaks = seq(2, 7, by = 0.5))) +
  scale_color_manual(values = c("orange", "blue", "red", "white", "purple", "darkgreen")) +
  scale_color_discrete(name = "Recuerdo de voto")+ 
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background=element_rect(fill = "brown"),
    panel.background = element_rect(fill = 'black'),
    # Change legend 
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.background = element_rect(fill = "black", color = NA),
    legend.key = element_rect(color = "gray", fill = "black"),
    legend.key.size = unit(0.01, 'cm'),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "white")
  )  +
  scale_color_manual(values = c("orange", "blue", "red", "white", "purple", "darkgreen"))
  

evolucion

abril <- read_sav("3318.sav")

abril <- abril %>% select(P28_3, EDAD, P14, P15, ESCIDEOL, RECUVOTOGR)



# EDAD


abril$EDAD[abril$EDAD >= 65] <- 6
abril$EDAD[abril$EDAD >=55 & abril$EDAD <=64] <- 5
abril$EDAD[abril$EDAD >=45 & abril$EDAD <=54] <- 4
abril$EDAD[abril$EDAD >=35 & abril$EDAD <=44] <- 3
abril$EDAD[abril$EDAD >=25 & abril$EDAD <=34] <- 2
abril$EDAD[abril$EDAD >=18 & abril$EDAD <=24] <- 1

abril$RECUVOTOGR[abril$RECUVOTOGR == 6 | abril$RECUVOTOGR == 21 |
                   abril$RECUVOTOGR == 7 | abril$RECUVOTOGR == 6 | 
                   abril$RECUVOTOGR == 67] <- 3

table(abril$EDAD)
table(abril$RECUVOTOGR)


edad_ <- ggplot(data = abril, aes(x = as.factor(EDAD), y= P28_3)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", color="red") +
  labs(title = "",
       subtitle = "",
       caption = "") +
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=9),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=9),
        axis.title.x = element_text(color="blue", size=9, face="bold"),
        axis.title.y = element_text(color="blue", size=9, face="bold"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.caption = element_text(hjust = 1)) +
  scale_x_discrete(name = "Grupo de edad", labels=c("1" = "18-24", "2" = "25-34",
                                                    "3" = "35-44", "4" = "45-54",
                                                    "5" = "55-64", "6" = ">=65"))+
  scale_y_continuous(name="Valoración Nadia Calviño", limits=c(1, 10), breaks = seq(1, 10, by = 1))

edad_



# SIT ECONÓMICA DE ESPAÑA

sit_esp <- ggplot(data = abril, aes(x = as.factor(P14), y= P28_3)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", color="red") +
  labs(title = "",
       subtitle = "",
       caption = "") +
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=9),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=9),
        axis.title.x = element_text(color="blue", size=9, face="bold"),
        axis.title.y = element_text(color="blue", size=9, face="bold"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.caption = element_text(hjust = 1)) +
  scale_x_discrete(name = "Situación económica de España", labels=c("1" = "Muy buena", "2" = "Buena",
                                                                    "3" = "Regular", "4" = "Mala",
                                                                    "5" = "Muy mala", "8" = "NS",
                                                                    "9" = "NC"))+
  scale_y_continuous(name="Valoración Nadia Calviño", limits=c(1, 10), breaks = seq(1, 10, by = 1))

sit_esp + 
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background=element_rect(fill = "green"),
    panel.background = element_rect(fill = 'black'),
    # Change legend 
    legend.position = (legend.position = c(0.1, 0.1)),
    legend.direction = "horizontal",
    legend.background = element_rect(fill = "black", color = NA),
    legend.key = element_rect(color = "gray", fill = "black"),
    legend.key.size = unit(0.01, 'cm'),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "white")
  )




# SIT ECONÓMICA PERSONAL
sit_eco <- ggplot(data = abril, aes(x = as.factor(P15), y= P28_3)) +
  geom_boxplot() +
  stat_summary(fun.y=mean, geom="point", color="red") +
  labs(title = "",
       subtitle = "",
       caption = "") +
  theme(plot.title = element_text(face="bold", color="black", 
                                  size=9),
     axis.text.x = element_text(face="bold", color="black", 
                                   size=9),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=9),
        axis.title.x = element_text(color="black", size=9, face="bold"),
        axis.title.y = element_text(color="black", size=9, face="bold"),
        panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.caption = element_text(hjust = 1)) +
  scale_x_discrete(name = "Situación económica personal barómetro abril 2021", labels=c("1" = "Muy buena", "2" = "Buena",
                                                                   "3" = "Regular", "4" = "Mala",
                                                                   "5" = "Muy mala", "8" = "NS",
                                                                   "9" = "NC"))+
  scale_y_continuous(name="Valoración Nadia Calviño", limits=c(1, 10), breaks = seq(1, 10, by = 1))

sit_eco <- sit_eco + 
  theme(
    # get rid of panel grids
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change plot and panel background
    plot.background=element_rect(fill = "brown"),
    panel.background = element_rect(fill = 'black'),
    # Change legend 
    legend.position = (legend.position = c(0.1, 0.1)),
    legend.direction = "horizontal",
    legend.background = element_rect(fill = "black", color = NA),
    legend.key = element_rect(color = "gray", fill = "black"),
    legend.key.size = unit(0.01, 'cm'),
    legend.title = element_text(color = "white"),
    legend.text = element_text(color = "white")
  )

sit_eco


lillie.test(abril$P28_3) # si >0.05 sigue distribucion normal :)) y podemos seguir
leveneTest(abril$P28_3,abril$P15) #si >0,05 varianzas iguales (homocedasticidad) | <0.05 varianzas no son iguales (heterocedasticidad)
t.test(abril$P28_3~abril$P15, var.equal = F) # HAY DIFERENCIA CUANDO <0.05 

pairwise.t.test(x = abril$P28_3, g = abril$P15, p.adj = "bonf")

# IDEOLOGÍA



# VOTO

voto_abril <- filter(abril, abril$RECUVOTOGR == 1 | abril$RECUVOTOGR == 2 |
                       abril$RECUVOTOGR == 3 | abril$RECUVOTOGR == 4| 
                       abril$RECUVOTOGR == 18) 


voto <-  ggplot(data = voto_abril, aes(x = P28_3, fill = as.factor(RECUVOTOGR))) + 
  geom_histogram(binwidth = 1) +
  facet_wrap(~RECUVOTOGR, scales = "free_y", ncol = 2, nrow = 3,
             labeller = labeller(RECUVOTOGR = 
                                   c("1" = "PP",
                                     "2" = "PSOE",
                                     "3" = "UP",
                                     "4" = "CS",
                                     "18" = "VOX")
             )) + 
  scale_fill_manual(values = c("blue",  "red", "purple", "orange", "darkgreen")) +
  scale_x_continuous(name="Valoración Nadia Calviño", limits=c(1, 10), breaks = seq(1, 10, by = 1)) +
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=9),
        axis.text.y = element_text(face="bold", color="black", 
                                   size=9),
        axis.title.x = element_text(color="black", size=9, face="bold"),
        axis.title.y = element_text(color="black", size=9, face="bold"),
        # get rid of panel grids
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Change plot and panel background
        plot.background=element_rect(fill = "brown"),
        strip.background = element_rect(fill="white"),
        panel.background = element_rect(fill = 'black'),
        # Change legend 
        legend.position = "none",
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "black", color = NA),
        legend.key = element_rect(color = "gray", fill = "black"),
        legend.key.size = unit(0.01, 'cm'),
        legend.title = element_text(color = "white"),
        legend.text = element_text(color = "white")
  )

voto




# REG LOG 

abril <- read_sav("3318.sav")

abril <- abril %>% select(P28_3, EDAD, P14, P15, ESCIDEOL, RECUVOTOGR)

abril$P28_3[abril$P28_3 > 96] <- NA
abril <- na.omit(abril)
abril

abril$P28_3[abril$P28_3 == 5] <- 0
abril$P28_3[abril$P28_3 == 6] <- 0
abril$P28_3[abril$P28_3 == 7] <- 0
abril$P28_3[abril$P28_3 == 8] <- 0
abril$P28_3[abril$P28_3 == 9] <- 0
abril$P28_3[abril$P28_3 == 10] <- 0



abril$P28_3[abril$P28_3 == 1 | abril$P28_3 == 2 |
              abril$P28_3 == 3| abril$P28_3 == 4] <- 1

abril


abril$P15[abril$P15 > 6] <- NA
abril <- na.omit(abril)
abril


abril$EDAD[abril$EDAD >= 65] <- 6
abril$EDAD[abril$EDAD >=55 & abril$EDAD <=64] <- 5
abril$EDAD[abril$EDAD >=45 & abril$EDAD <=54] <- 4
abril$EDAD[abril$EDAD >=35 & abril$EDAD <=44] <- 3
abril$EDAD[abril$EDAD >=25 & abril$EDAD <=34] <- 2
abril$EDAD[abril$EDAD >=18 & abril$EDAD <=24] <- 1


abril <- abril %>%
  mutate(
    P28_3 = factor(P28_3, levels = c(0, 1), labels = c("Aprueba", "Suspende")),
    P15 = factor(P15)
  )

library(ggplot2)
library(ggiraphExtra)
library(moonBook)
library(plyr)
library(dplyr)
library(survival)

logit <- glm(P28_3 ~ P15, 
             data = abril, 
             # tipo de modelo/distribucion
             family = "binomial")

summary(logit)


coef(logit) %>%
  as_tibble() %>%
  mutate(odds = exp(value))

coef(logit) %>%
  as_tibble() %>%
  mutate(
    odds = exp(value),
    prob = plogis(value)
  )


tabla <- tibble::tribble(
                ~Sit_eco_personal, ~Estimate, ~Std_Error, ~z_value, ~`Pr(>|z|)`, ~odds, 
           "(Intercept)",   -1.1952,    0.2380,    -5.022,    5.10e-07,    0.30,    
             "Muy buena",        NA,         NA,       NA,          NA,    NA,   
                 "Buena",    0.2446,     0.2450,    0.999,      0.3180,    1.28,    
               "Regular",        0.5801,     0.2636,   2.201,   0.0278,    1.79,    
                  "Mala",       0.6305,      0.2658,  2.372,    0.0177,    1.88,    
              "Muy mala",        1.3086,     0.3074,   4.257, 2.07e-05,    3.70    
           )


main.title <- ("Odd Ratio")
subtitle <- paste0(
  "0 = Aprueba (valores entre 5 y 10) | 1 = Suspende (valores entre 1 y 4)"
  
) %>% 
  strwrap(width = 80) %>%
  paste(collapse = "\n")


ggtab <- ggtexttable(tabla, rows = NULL, theme = ttheme("light"))
ggtab <- ggtab %>%
  tab_add_title(text = subtitle, face = "bold", size = 10) %>%
  tab_add_title(text = main.title, face = "bold", size = 8, padding = unit(0.1, "line")) %>%
  tab_add_footnote(text = "", size = 10, face = "italic")
ggtab

coef(logit)/4



logit %>%
  ggPredict(terms = c("P15")) %>%
  plot(log.y = TRUE) +
  labs(title = "Valores esperados: probabilidad de ser admitido",
       color = "P15", x = "GPA", y = "Pr(Y=1)")




# GGARRANGE

figure <- ggarrange(evolucion, voto, sit_eco, ggtab,  ncol =2, nrow = 2)

figure <- annotate_figure(figure,
                          top = text_grob("Evolución de la valoración a Calviño por su actuación como ministra \n Distribución de la valoración en el mes de abril por recuerdo de voto \n A peor situación económica personal del entrevistado con menor nota valora a la ministra de Economía", 
                                          color = "black", face = "bold", size = 12),
                          bottom = text_grob("CIS | @dataR_amateur",
                                             hjust = 1, x = 1, face = "italic", size = 8)) +
  theme(plot.background = element_rect(fill = "brown"))

figure



figure + ggsave("calvino.png", width = 13, height = 8.5, dpi = 500)
