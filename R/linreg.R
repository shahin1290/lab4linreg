#' Linear regression.
#'
#' Performs linear regression using QR decomposition.
#' See \url{http://staff.www.ltu.se/~jove/courses/c0002m/least squares.pdf}
#' for details.
#' @field formula formula Linear regression formula.
#' @field data data.frame Input data.
#' @field coefficients numeric Coefficients.
#' @field residuals matrix Matrix of residuals.
#' @field fitted.values matrix Matrix of fitted values.
#' @field df numeric Degrees of freedom.
#' @field sigma numeric Estimated residual standard error.
#' @field X matrix Model matrix.
#' @field y numeric Response vector.
#' @field qr ANY Results of the QR decomposition.
#'
#' @export linreg
#' @import ggplot2
#' @import gridExtra
#'
#' @examples
#' linreg_mod <- linreg$new(formula = Petal.Length ~ Species, data=iris)
#' linreg_mod$plot()
#' linreg_mod$print()
#' linreg_mod$resid()
#' linreg_mod$pred()
#' linreg_mod$coef()
#' linreg_mod$summary()
linreg <- setRefClass("linreg",
    fields = list(
      formula = "formula",
      data = "data.frame",
      coefficients = "numeric",
      residuals = "matrix",
      fitted.values = "matrix",
      df = "numeric",
      sigma = "numeric",
      X = "matrix",
      y = "numeric",
      qr = "ANY"
    ),
    methods = list(
      initialize = function(formula, data) {
        .self$formula <- formula
        .self$data <- data

        # Construct the model matrix X and response vector y
        .self$X <- model.matrix(formula, data)
        .self$y <- model.response(model.frame(formula, data = data))

        # Perform QR decomposition
        .self$qr <- qr(.self$X)

        # Check for full rank
        if (.self$qr$rank < ncol(.self$X)) {
          stop("X is not of full rank (likely multicollinearity). Regression coefficients may not be uniquely determined.")
        }

        # Calculate coefficients
        .self$coefficients <- solve.qr(.self$qr, .self$y)

        # Calculate fitted values, residuals, degrees of freedom, and sigma
        .self$fitted.values <- .self$X %*% .self$coefficients
        .self$residuals <- .self$y - .self$fitted.values
        .self$df <- nrow(.self$X) - ncol(.self$X)
        .self$sigma <- sqrt(sum(.self$residuals^2) / .self$df)
      },

      print = function() {
        cat("Call:\n")
        formula_string <- paste0("linreg(formula = ", deparse(.self$formula), ", data = ", deparse(substitute(.self$data)), ")")
        cat(formula_string, "\n")
        cat("\nCoefficients:\n")
        coef_names <- names(.self$coefficients)
        coef_line <- paste(paste0(" ", coef_names, " "), collapse = "")
        cat(coef_line, "\n")
      },

      resid = function() {
        return(.self$residuals)
      },

      pred = function() {
        return(.self$fitted.values)
      },

      coef = function() {
        return(.self$coefficients)
      },

      summary = function() {
        # Calculate standard errors of coefficients
        R <- qr.R(.self$qr)
        V <- chol2inv(R)
        se <- sqrt(diag(V) * .self$sigma^2)

        # Calculate t-values and p-values
        t_values <- .self$coefficients / se
        p_values <- 2 * pt(abs(t_values), df = .self$df, lower.tail = FALSE)

        # Create a table of coefficients, standard errors, t-values, and p-values
        coef_table <- cbind(.self$coefficients, se, t_values, p_values)
        colnames(coef_table) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")

        # Print the summary
        cat("Call:\n")
        cat(paste0("linreg(formula = ", deparse(.self$formula), ", data = ", deparse(substitute(.self$data)), ")\n")) #Corrected data deparse
        cat("\nResiduals:\n")
        quantiles <- quantile(.self$residuals, probs = c(0, 0.25, 0.5, 0.75, 1))
        names(quantiles) <- c("Min", "1Q", "Median", "3Q", "Max")

        # Manually format and print the quantiles using cat
        quantile_string <- paste(names(quantiles), ": ", quantiles, collapse = "\n")
        cat(quantile_string, "\n")
        cat("\nCoefficients:\n")
        printCoefmat(coef_table, digits = 4, signif.stars = TRUE, t.names = FALSE)
        cat("\nResidual standard error:", .self$sigma, "on", .self$df, "degrees of freedom\n")
      },

    plot = function() {
      # Ensure ggplot2 is installed and loaded
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("ggplot2 is required for this function. Please install it using install.packages('ggplot2')")
      }
      if (!requireNamespace("gridExtra", quietly = TRUE)) {
        stop("gridExtra is required for combining plots. Please install it using install.packages('gridExtra')")
      }

      # Define the colors
      color_1 <- rgb(0, 185, 231, maxColorValue = 255)  # R:0 G:185 B:231 (points)
      color_2 <- rgb(23, 199, 210, maxColorValue = 255) # R: 23 G: 199 B: 210 (hline)
      color_3 <- rgb(0, 207, 181, maxColorValue = 255)  # R: 0 G: 207 B: 181 (smoother)

      # Create a custom theme
      custom_theme <- ggplot2::theme_bw() +
        ggplot2::theme(
          panel.border = ggplot2::element_rect(color = "black", fill = NA),  # Add a black border
          plot.title = ggplot2::element_text(hjust = 0.5),                   # Center the title
          panel.grid.major = ggplot2::element_blank(),                 # Remove major gridlines
          panel.grid.minor = ggplot2::element_blank()                  # Remove minor gridlines

        )

      # 1. Residuals vs Fitted Plot
      df_res_fit <- data.frame(fitted = .self$fitted.values, residuals = .self$residuals)

      p1 <- ggplot2::ggplot(df_res_fit, ggplot2::aes(x = fitted, y = residuals)) +
        ggplot2::geom_point(color = color_1) +
        ggplot2::geom_hline(yintercept = 0, color = color_2, linetype = "dashed") +
        ggplot2::geom_smooth(method = "loess", se = FALSE, color = color_3) +
        ggplot2::labs(title = paste("Residuals vs Fitted\nFormula:", deparse(.self$formula)),
                      x = "Fitted values",
                      y = "Residuals") +
        custom_theme  # Apply the custom theme

      # 2. Scale-Location Plot
      sqrt_abs_residuals <- sqrt(abs(.self$residuals))
      df_scale_loc <- data.frame(fitted = .self$fitted.values, sqrt_abs_residuals = sqrt_abs_residuals)

      p2 <- ggplot2::ggplot(df_scale_loc, ggplot2::aes(x = fitted, y = sqrt_abs_residuals)) +
        ggplot2::geom_point(color = color_1) +
        ggplot2::geom_smooth(method = "loess", se = FALSE, color = color_3) +
        ggplot2::labs(title = paste("Scale-Location\nFormula:", deparse(.self$formula)),
                      x = "Fitted values",
                      y = "sqrt(Standardized residuals)") +
        custom_theme  # Apply the custom theme

      # Combine plots vertically using grid.arrange
      gridExtra::grid.arrange(p1, p2, nrow = 2)
    }
  )
)
