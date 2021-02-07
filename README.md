# Propensão a tomar vacina (H1N1 e seasonal flu)
# 
# O atual projeto, com fins puramente acadêmicos, é um desafio do site DrivenData (https://www.drivendata.org/competitions/66/flu-shot-learning/) e a proposta é desenvolver 2 modelos de predição:
# um modelo de predição para identificar pessoas propensas a tomar a vacina do H1N1;
# um modelo de predição para identificar pessoas propensas a tomar a vacina da gripe.
# 
# O ajuste do modelo de regressão logística foi realizado através da distribuição Binomial:
# - cada indivíduo pode assumir 2 valores (tomar ou não a vacina);
# - o experimento é uma tentativa de N tentativas idênticas;
# - cada tentativa é uma Bernoulli;
# - a probabilidade de tomar ou não a vacina não muda de tentativa para tentativa;
# - as tentativas são independentes entre si. 