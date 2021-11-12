# QUESTÃO 2A #
df_q2$idade*df_q2$idade
reg <- lm(lsalarioh ~ idade + idadeqdd + educ, data = df_q2)

reg1 <- lm(lsalarioh ~ idade + idadeqdd + mulher + preta + 
             parda + servidor + capital + educ + I(educ * mulher) +
             freqescola, data = df_q2)

reg1pp <- lm(lsalarioh ~ idade + idadeqdd + mulher + preta + 
             parda + servidor + capital + educ + I(educ * mulher) +
             freqescola + I(educ * preta) + I(educ * parda), data = df_q2)

reg1ppsub <- lm(lsalarioh ~ idade + idadeqdd + mulher + preta + 
               parda + servidor + capital + educ + I(educ * mulher) +
               freqescola + I(educ * preta) + I(educ * parda), data = df_q2, subset = (salarioh < 100))


reg1a <- lm(lsalarioh ~ idade + idadeqdd + mulher + preta + 
              parda + servidor + capital + freqescola + superiorinc + mediocom +
              medioinc + fundcom + fundinc + semestudo, data = df_q2)


# QUESTÃO 2B #

resettest(reg1, data = df_q2)
resettest(reg1a, data = df_q2)
resettest(reg1pp, data = df_q2)

# QUESTÃO 2D

reg2 <- ivreg(lsalarioh ~ idade + idadeqdd + mulher + preta + 
              parda + servidor + capital + educ + I(educ * mulher) +
              freqescola | idade + idadeqdd + mulher + preta + servidor + parda +
              capital + pessoasdom + filhos + I(filhos * mulher) +
              I(pessoasdom * mulher) + freqescola, data = df_q2)

reg2a <- ivreg(lsalarioh ~ idade + idadeqdd + mulher + preta + 
               parda + servidor + capital + educ + I(educ * mulher) +
               freqescola + I(educ * preta) + I(educ * parda) | 
                 idade + idadeqdd + mulher + preta + servidor + parda +
                 capital + pessoasdom + filhos + I(filhos * mulher) +
                 I(pessoasdom * mulher) + freqescola + I(filhos * preta)
               + I(pessoasdom * preta) + I(filhos * parda) + I(pessoasdom * parda), data = df_q2)

# QUESTÃO 2C e 2E

summary(reg2, diagnostics = TRUE)


# QUESTÃO 3A e 3B #

reg3 <- plm(lsalarioh ~ idade + idadeqdd + mulher + preta + 
              parda + servidor + capital + educ + I(educ * mulher) +
              freqescola, data = df_q2,
            model = 'within', index = c('id','data'))

reg4 <- plm(lsalarioh ~ idade + idadeqdd + mulher + preta + 
              parda + servidor + capital + educ + I(educ * mulher) +
              freqescola, data = df_q2,
            model = 'random', index = c('id','data'))

phtest(reg3, reg4)

# QUESTÃO 3C #

reg4a <- plm(lsalarioh ~ idade + idadeqdd + mulher + preta + 
      parda + servidor + capital + educ + I(educ * mulher) +
      freqescola, data = df_q2, model = 'fd')

bgtest(reg1)

# QUESTÃO 4A #

reg5 <- glm(ocupado ~ casado + nfilhos + ordemdom + idade + idadeqdd + nfilhos7anos +
              educ + preta + parda, data = df_q4, family = binomial(link = 'probit'),
            subset = (mulher == 0))

reg5m <- glm(ocupado ~ casado + nfilhos + nfilhos7anos + ordemdom + idade + idadeqdd + 
            + educ + preta + parda, data = df_q4, family = binomial(link = 'probit'),
            subset = (mulher == 1))


reg5am <- glm(ocupado ~ casado + nfilhos + nfilhos7anos + ordemdom + idade + idadeqdd + 
                superiorinc + mediocom + medioinc + fundcom + fundinc + semestudo +
              preta + parda, data = df_q4, family = binomial(link = 'probit'))


# QUESTÃO 4B #

margins(reg5)
margins(reg5m)
margins(reg5am)

# QUESTÃO 4C #

 # df_q4$servidor[is.na(df_q4$servidor)] <- 0 

reg6 <- censReg(lsalarioh ~ idade + idadeqdd + mulher + preta + 
                parda + servidor + capital + educ + I(educ * mulher) +
                freqescola, data = df_q4, left = 0)

reg6a <- censReg(lsalarioh ~ idade + idadeqdd + mulher + preta + parda +
                servidor + capital + superiorinc + mediocom + 
                 medioinc + fundcom + fundinc + semestudo + freqescola, data = df_q4, left = 0)

summary(reg6)
summary(reg6a)

a <- ggplot(df_q4, aes(x = lsalarioh))
a + geom_density(aes(y = ..count..), fill = "lightgray") +
  geom_vline(aes(xintercept = median(lsalarioh)), 
             linetype = "dashed", size = 0.6,
             color = "#FC4E07") +
  xlim(c(-0.1,2))

summary(margEff(reg6))
summary(margEff(reg6a))

# QUESTÃO 4D #

reg7 <- glm(nfilhos ~ idade + preta + parda +
              servidor + capital + educ + lsalarioh, family = 'poisson',
            data = df_q4, subset = (mulher == 1))

summary(reg7)

# QUESTÃO 4E #

summary(heckit(ocupado ~ casado + ordemdom + idade + idadeqdd + nfilhos7anos +
                  educ + preta, lsalarioh ~ idade + idadeqdd + preta + 
                  parda + servidor + capital + educ + freqescola,
                  data = df_q4), subset = (mulher == 1))

