# Técnicas multivariadas de la Entrega 3
# -----------------------------------------------------------------------------
# Define las funciones del ACP de los puntajes, del ACM del bloque cualitativo y
# del modelo de dependencia, junto con las medidas de adecuación y supuestos que
# acompañan a cada una. No ejecuta nada al cargarse.
# Lo carga reports/Entrega 3.qmd después de R/preparacion_mv.R.
#
# Depende de FactoMineR y de car.

library(FactoMineR)

# Adecuación muestral ---------------------------------------------------------

# Medida de Kaiser-Meyer-Olkin a partir de la matriz de correlaciones. Compara
# las correlaciones simples con las parciales: cuanto más pesen las parciales,
# menos justificable es resumir las variables en pocos factores.
kmo <- function(R) {
  iR <- solve(R)
  d <- sqrt(diag(iR))
  P <- -iR / outer(d, d)          # correlaciones parciales
  diag(P) <- 0
  diag(R) <- 0
  list(global = sum(R^2) / (sum(R^2) + sum(P^2)),
       por_variable = colSums(R^2) / (colSums(R^2) + colSums(P^2)))
}

# Prueba de esfericidad de Bartlett. Con n del orden de 10^5 rechaza siempre, de
# modo que se reporta por completitud y la lectura se apoya en el KMO.
bartlett_esfericidad <- function(R, n) {
  p <- ncol(R)
  chi <- -(n - 1 - (2 * p + 5) / 6) * log(det(R))
  gl <- p * (p - 1) / 2
  list(chi2 = chi, gl = gl, p = pchisq(chi, gl, lower.tail = FALSE))
}

# Análisis de componentes principales -----------------------------------------

# ACP de los cinco puntajes sobre la matriz de correlaciones. Comparten escala,
# pero se estandariza igual para que las diferencias de dispersión entre pruebas
# no pesen en la solución.
acp_puntajes <- function(df = dataset, vars = VARS_PUNTAJES, ncp = 5) {
  X <- df[stats::complete.cases(df[, vars]), vars]
  R <- stats::cor(X)
  list(
    acp = FactoMineR::PCA(X, scale.unit = TRUE, ncp = ncp, graph = FALSE),
    cor = R,
    kmo = kmo(R),
    bartlett = bartlett_esfericidad(R, nrow(X)),
    n = nrow(X)
  )
}

# Análisis de correspondencias múltiples --------------------------------------

# Las inercias brutas del ACM subestiman mucho la varianza explicada, porque la
# codificación disyuntiva introduce inercia artificial. Se corrigen con Benzécri
# y con Greenacre, menos optimista, que es la que se reporta.
inercias_corregidas <- function(lambda, Q, J) {
  umbral <- 1 / Q
  adj <- ifelse(lambda > umbral, ((Q / (Q - 1)) * (lambda - umbral))^2, 0)
  total_greenacre <- (Q / (Q - 1)) * (sum(lambda^2) - (J - Q) / Q^2)
  data.frame(
    bruta = 100 * lambda / sum(lambda),
    benzecri = 100 * adj / sum(adj),
    greenacre = 100 * adj / total_greenacre
  )
}

# ACM con las nueve variables socioeconómicas e institucionales como activas.
# Las individuales y los puntajes entran como suplementarios: se proyectan sobre
# la estructura sin contribuir a construirla.
acm_contexto <- function(df = dataset, activas = VARS_ACM,
                         sup_cuali = VARS_ACM_SUP, sup_cuanti = VARS_PUNTAJES,
                         ncp = NULL) {
  vars <- c(activas, sup_cuali, sup_cuanti)
  d <- droplevels(df[stats::complete.cases(df[, vars]), vars])
  Q <- length(activas)
  J <- sum(vapply(d[activas], nlevels, integer(1)))
  # FactoMineR recorta `eig` a ncp filas, y las correcciones de inercia necesitan
  # los J - Q autovalores completos, así que se pide la solución entera.
  if (is.null(ncp)) ncp <- J - Q
  res <- FactoMineR::MCA(
    d,
    quali.sup  = match(sup_cuali, names(d)),
    quanti.sup = match(sup_cuanti, names(d)),
    ncp = ncp, graph = FALSE
  )
  # FactoMineR nombra las categorías sin prefijo, así que se guarda a qué
  # variable pertenece cada una para poder colorearlas en el plano.
  categorias <- data.frame(
    categoria = unlist(lapply(d[activas], levels), use.names = FALSE),
    variable  = rep(activas, vapply(d[activas], nlevels, integer(1)))
  )
  list(acm = res, n = nrow(d), Q = Q, J = J, categorias = categorias,
       inercias = inercias_corregidas(res$eig[, 1], Q, J))
}

# Modelo de dependencia -------------------------------------------------------

# Los factores ordenados generan contrastes polinómicos, que complican la
# lectura de los coeficientes sin cambiar el R². Se pasan a contrastes de
# tratamiento para que cada categoría se compare con la de referencia.
sin_orden <- function(df) {
  for (v in names(df)) if (is.ordered(df[[v]])) df[[v]] <- factor(df[[v]], ordered = FALSE)
  df
}

# Regresión lineal multivariada de los cinco puntajes. Devuelve el R² de cada
# componente, que es lo que admite comparación entre modelos.
ajustar_modelo <- function(df, predictores, respuestas = VARS_PUNTAJES) {
  f <- stats::reformulate(
    predictores,
    response = paste0("cbind(", paste(respuestas, collapse = ", "), ")")
  )
  stats::lm(f, data = df)
}

r2_modelo <- function(modelo) {
  vapply(summary(modelo), function(s) s$r.squared, numeric(1))
}

# Descomposición del R² por bloques. El aporte propio de un bloque es lo que el
# modelo completo pierde al quitarlo, y la diferencia entre la suma de los R²
# aislados y el R² conjunto mide cuánto se superponen entre sí.
r2_por_bloques <- function(df, bloques, respuestas = VARS_PUNTAJES) {
  todas <- unlist(bloques, use.names = FALSE)
  d <- sin_orden(droplevels(df[stats::complete.cases(df[, c(todas, respuestas)]),
                               c(todas, respuestas)]))

  completo <- r2_modelo(ajustar_modelo(d, todas, respuestas))

  aislado <- vapply(bloques, function(b) r2_modelo(ajustar_modelo(d, b, respuestas)),
                    numeric(length(respuestas)))
  propio <- vapply(bloques, function(b) {
    completo - r2_modelo(ajustar_modelo(d, setdiff(todas, b), respuestas))
  }, numeric(length(respuestas)))

  list(n = nrow(d), completo = completo,
       aislado = t(aislado), propio = t(propio),
       compartida = colSums(t(aislado)) - completo)
}

# Supuestos -------------------------------------------------------------------

# Factor de inflación de varianza generalizado. Con factores, car devuelve el
# GVIF; la columna comparable con el VIF clásico es GVIF^(1/(2*gl)) al cuadrado.
colinealidad <- function(df, predictores, respuesta = "PUNT_GLOBAL") {
  m <- stats::lm(stats::reformulate(predictores, response = respuesta), data = sin_orden(df))
  v <- car::vif(m)
  if (is.matrix(v)) data.frame(gvif = v[, 1], gl = v[, 2], vif_eq = v[, 3]^2)
  else data.frame(gvif = v, gl = 1, vif_eq = v)
}

# Asimetría y curtosis de los residuos. Con n del orden de 10^5 las pruebas de
# normalidad rechazan cualquier desviación, así que se describen los momentos y
# la lectura se apoya en el teorema central del límite.
forma_residuos <- function(modelo) {
  r <- stats::residuals(modelo)
  if (is.null(dim(r))) r <- matrix(r, ncol = 1, dimnames = list(NULL, "residuo"))
  t(apply(r, 2, function(x) {
    z <- (x - mean(x)) / stats::sd(x)
    c(asimetria = mean(z^3), curtosis = mean(z^4) - 3)
  }))
}
