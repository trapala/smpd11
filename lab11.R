#install.packages("sets")
library("sets")

sets_options("universe", seq(1, 500, 0.5))
variables <- set(
  Matryca = fuzzy_partition(varnames = c(mala = 1, srednia = 15, duza = 28), sd = 0.1),
  JasnoscEkranu = fuzzy_partition(varnames = c(ciemna = 60, srednia = 110, jasna = 210), sd = 1.0),
  CzasReakcjiMatrycy = fuzzy_partition(varnames = c(szybki = 10, wystarczajacy = 17, wolny = 28), sd = 2.0),
  Cena = fuzzy_partition(varnames = c(niska = 0, srednia = 200, wysoka = 500), sd = 10),
  Ocena = fuzzy_partition(varnames = c(slaba = 1, przecietna = 2, niezla = 3, dobra = 4, swietna = 5), FUN = fuzzy_cone, radius = 0.5)
)

# Fuzzy rules
rules <- set(
  fuzzy_rule(Matryca %is% mala && JasnoscEkranu %is% ciemna && CzasReakcjiMatrycy %is% wolny, Ocena %is% slaba),
  fuzzy_rule(Matryca %is% mala && JasnoscEkranu %is% ciemna, Ocena %is% przecietna),
  fuzzy_rule(Matryca %is% srednia && JasnoscEkranu %is% srednia, Ocena %is% dobra),
  fuzzy_rule(Matryca %is% duza && JasnoscEkranu %is% jasna, Ocena %is% swietna),
  fuzzy_rule(Cena %is% niska && Matryca %is% mala && JasnoscEkranu %is% ciemna, Ocena %is% niezla),
  fuzzy_rule(Cena %is% srednia && Matryca %is% srednia && JasnoscEkranu %is% srednia, Ocena %is% dobra),
  fuzzy_rule(Cena %is% duza && Matryca %is% duza && JasnoscEkranu %is% jasna, Ocena %is% swietna),
  fuzzy_rule(Cena %is% wysoka && CzasReakcjiMatrycy %is% szybki, Ocena %is% niezla),
  fuzzy_rule(Cena %is% niska && CzasReakcjiMatrycy %is% wolny, Ocena %is% niezla),
  fuzzy_rule(Cena %is% niska && CzasReakcjiMatrycy %is% szybki, Ocena %is% swietna),
  fuzzy_rule(Cena %is% wysoka && CzasReakcjiMatrycy %is% wolny, Ocena %is% slaba)
)
model <- fuzzy_system(variables, rules)

print(model)
plot(model)
example.1 <- fuzzy_inference(model, list(Matryca = 17, JasnoscEkranu = 75, CzasReakcjiMatrycy = 11, Cena = 150))
gset_defuzzify(example.1, "centroid")
dev.new()
plot(example.1)