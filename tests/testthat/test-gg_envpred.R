context("Plotting function")

library(envPred)
data(sst)
data(npp)

test_that("Simple corner cases", {
	# sst
    expect_warning(sst_env <- env_stats(sst$time_series, sst$dates, n_states = 11, delta = 1, is_uneven = FALSE, interpolate = FALSE, show_warns = TRUE, noise_method = "spectrum"))
    plot_sst_1 <- gg_envpred(sst_env)
    plot_sst_2 <- gg_envpred(sst_env, type = "spectral")
    plot_sst_3 <- gg_envpred(sst_env, type = "detrended")
    plot_sst_4 <- gg_envpred(sst_env, type = NULL)
    expect_is(plot_sst_1, "gg")
    expect_is(plot_sst_2, "gg")
    expect_is(plot_sst_3, "gg")
    expect_is(plot_sst_4, "gg")
    expect_error(gg_envpred(sst_env, type = NA))
    expect_error(gg_envpred(sst_env, type = "h"))

	# npp
    expect_warning(npp_env <- env_stats(npp$time_series, npp$dates, n_states = 11, delta = 8, is_uneven = TRUE, interpolate = FALSE, show_warns = TRUE, noise_method = "lomb_scargle"))
    plot_npp_1 <- gg_envpred(npp_env)
    plot_npp_2 <- gg_envpred(npp_env, type = "spectral")
    plot_npp_3 <- gg_envpred(npp_env, type = "detrended")
    plot_npp_4 <- gg_envpred(npp_env, type = NULL)
    expect_is(plot_npp_1, "gg")
    expect_is(plot_npp_2, "gg")
    expect_is(plot_npp_3, "gg")
    expect_is(plot_npp_4, "gg")
    expect_error(gg_envpred(npp_env, type = NA))
    expect_error(gg_envpred(npp_env, type = "h"))
})
