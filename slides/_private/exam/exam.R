######################################################################
# Generating pdf and extracting R code from quarto (revealjs) slides #
# ------------------------------------------------------------------ #
# Written by Ladislas Nalborczyk                                     #
# E-mail: ladislas.nalborczyk@cnrs.fr                                #
# Last updated on November 25, 2025                                  #
######################################################################

library(exams)

# defining the working directory?dir
setwd(dir = "../private/exam/")

# checking files in directory
dir()

# generating exam with a simple vector of exercises in RMarkdown (.Rmd) format
myexam <- c(
    "1_cond_prob.Rmd", "2_linear_model.Rmd",
    "3_posterior_parameters.Rmd", "4_brms_to_math.Rmd",
    "ttest.Rmd", "scatterplot.Rmd",
    "function.Rmd", "lm.Rmd",
    "essayreg.Rmd"
    )

# generating PDF version of the exam and corresponding solutions in output directory
# set.seed(666)
exams2pdf(
    file = myexam,
    n = 1,
    nsamp = NULL,
    name = c("exam", "solution"),
    dir = "output",
    edir = "imsb_exercises/",
    template = c("imsb_templates/exam.tex", "imsb_templates/solution.tex"),
    language = "fr",
    texengine = "xelatex",
    header = list(Date = "23-01-2026")
    )

# generating exam with a simple vector of exercises in R/Markdown (.Rmd) format
# myexam_list <- list(
#     "1_cond_prob.Rmd", "2_linear_model.Rmd",
#     "3_posterior_parameters.Rmd", "4_brms_to_math.Rmd"
#     )

# generating exam in PDF format that can be printed, scanned, and evaluated automatically.
# exams2nops(
#     file = myexam_list,
#     # file = myexam,
#     n = 1,
#     # name = c("pdf-exam", "pdf-solution"),
#     name = "pdf-nops",
#     dir = "output_nops",
#     edir = "imsb_exercises",
#     # template = c("templates/exam.tex", "templates/solution.tex"),
#     # date = "2026-01-23",
#     logo = "../../../files/cover.png",
#     # language = "fr",
#     title = "Introduction à la modélisation statistique bayésienne",
#     institution = "Université Grenoble Alpes - TransCoG",
#     showpoints = TRUE,
#     helvet = TRUE
#     )
