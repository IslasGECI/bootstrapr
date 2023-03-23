describe("make_table()", {
  it("check round", {
    data <- read_csv("/workdir/tests/data/densidad_madrigueras_mergulo_morro.csv", show_col_types = FALSE)
    obtained <- make_table(data)
    expected <- read_csv("/workdir/tests/data/intervalo_densidad_mergulo_morro.csv", col_types = c("cnnn"), show_col_types = FALSE)
    expect_equal(obtained, expected, ignore_attr = TRUE)
  })
})

describe("make_table_without_round()", {
  it("does not round", {
    data <- read_csv("/workdir/tests/data/densidad_madrigueras_mergulo_morro.csv", show_col_types = FALSE)
    obtained <- make_table_without_round(data)
    expected <- read_csv("/workdir/tests/data/intervalo_densidad_mergulo_morro_without_round.csv", col_types = c("cnnn"), show_col_types = FALSE)
    expect_equal(obtained, expected, ignore_attr = TRUE, tolerance = 1e-2)
  })
})
