## Probit-Modelle


## Modell 1 nur mit Temperatur

lvs_model_1 <- glm(lvs ~ temperature, 
                   family = binomial(link = "logit"), 
                   data = lvs_data)
summary(lvs_model_1)

# Temperatur hat sign. Einfluss. Steigerung um 1 Grad bedeutet Verringerung der
# Chance, ein LVS-Gerät gegenüber keines dabei zu haben um den Faktor
# exp(-0.072668) = 0.9299095


## Modell 2 mit metrischen Einflussgrößen

lvs_model_2 <- glm(lvs ~ temperature + snowhight + solar_radiation, 
                   family = binomial(link = "logit"), 
                   data = lvs_data)
summary(lvs_model_2)

# alle drei Variablen haben signifikanten Einfluss und verrringern bei Anstieg
# die Chance, ein LVS-Gerät dabei zu haben ggü keines dabei zu haben

# aus dem package mfx

logitmfx(lvs ~ temperature + snowhight + solar_radiation, data = lvs_data)

# dF/dx interpretierbar, wie bei einem normalen Modell:
# Anstieg der Temperatur um 1 cm verringert die Wahrscheinlichkeit, LVS-Gerät 
# dabeizuhaben um 1.1745e-02 Prozentpunkte

# so ein geringer Effekt, aber trotzdem signifikant?

## Modell 3 mit metrischen und kategorialen Einflussgrößen

lvs_model_3 <- glm(lvs ~ temperature + snowhight + solar_radiation + day + avalanche_report, 
                   family = binomial(link = "logit"), 
                   data = lvs_data)
summary(lvs_model_3)

# Samstag und Sonntag haben keinen sign. Einfluss. Auch solar_radiation hat keinen
# signifkaten Einfluss mehr

logitmfx(lvs ~ temperature + snowhight + solar_radiation + day + avalanche_report, data = lvs_data)

# kuriose Werte bei den avalanche_reports: steigen nicht mit höherer Warnstufe, 
# sondern schwanken hin und her


## Modell 4: Probit-Modell

lvs_model_4 <- glm(lvs ~ temperature + snowhight + solar_radiation + day + avalanche_report, 
                   family = binomial(link = "probit"), 
                   data = lvs_data)
summary(lvs_model_4)

# andere Werte (auch Vorzeichen!) als beim logi-Modell, aber Effekte eh sehr klein

probitmfx(lvs ~ temperature + snowhight + solar_radiation + day + avalanche_report, data = lvs_data)


## Vergleich des Logit- und Probit-Modells

list(
  "Logit-AIC" = AIC(lvs_model_3), "Probit-AIC" = AIC(lvs_model_4)
)

# Probit-Modell etwas besser




#### noch ausprobieren: Effekte zwischen einzelnen Kovariablen. Welche machen Sinn?